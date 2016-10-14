{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Please see the README.md file for examples of using this API.
module System.Process.Typed
    ( -- * Types
      ProcessConfig
    , StreamSpec
    , StreamType (..)
    , Process

      -- * ProcessConfig
      -- ** Smart constructors
    , proc
    , shell

      -- ** Setters
    , setStdin
    , setStdout
    , setStderr
    , setWorkingDir
    , setEnv
    , setCloseFds
    , setCreateGroup
    , setDelegateCtlc
#if MIN_VERSION_process(1, 3, 0)
    , setDetachConsole
    , setCreateNewConsole
    , setNewSession
#endif
#if MIN_VERSION_process(1, 4, 0) && !WINDOWS
    , setChildGroup
    , setChildUser
#endif
    , setCheckExitCode

      -- * Stream specs
    , mkStreamSpec
    , inherit
    , closed
    , byteStringInput
    , byteStringOutput
    , createPipe
    , useHandleOpen
    , useHandleClose
    , sink
    , source

      -- * Launch a process
    , startProcess
    , stopProcess
    , withProcess
    , readProcess
    , runProcess
    , runProcess_

      -- * Interact with a process

      -- ** Process exit code
    , waitExitCode
    , waitExitCodeSTM
    , checkExitCode
    , checkExitCodeSTM

      -- ** Process streams
    , getStdin
    , getStdout
    , getStderr

      -- * Exceptions
    , ExitCodeException (..)
    , ByteStringOutputException (..)
    ) where

import qualified Data.ByteString as S
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Exception (throw, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class
import qualified System.Process as P
import Control.Monad.Catch as C
import Data.Typeable (Typeable)
import System.IO (Handle, hClose)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (newEmptyTMVarIO, atomically, putTMVar, TMVar, readTMVar, tryReadTMVar, STM, tryPutTMVar, throwSTM)
import System.Exit (ExitCode (ExitSuccess))
import qualified Data.ByteString.Lazy as L
import Data.String (IsString (fromString))
import Data.Conduit (ConduitM)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB

#if MIN_VERSION_process(1, 4, 0) && !WINDOWS
import System.Posix.Types (GroupID, UserID)
#endif

-- | An abstract configuration for a process, which can then be
-- launched into an actual running 'Process'. Takes three type
-- parameters, providing the types of standard input, standard output,
-- and standard error, respectively.
--
-- There are three ways to construct a value of this type:
--
-- * With the 'proc' smart constructor, which takes a command name and
-- a list of arguments.
--
-- * With the 'shell' smart constructor, which takes a shell string
--
-- * With the 'IsString' instance via OverloadedStrings. If you
-- provide it a string with no spaces (e.g., @"date"@), it will
-- treat it as a raw command with no arguments (e.g., @proc "date"
-- []@). If it has spaces, it will use @shell@.
--
-- In all cases, the default for all three streams is to inherit the
-- streams from the parent process. For other settings, see the
-- setters below for default values.
--
-- @since 0.1.0.0
data ProcessConfig stdin stdout stderr = ProcessConfig
    { pcCmdSpec :: !P.CmdSpec
    , pcStdin :: !(StreamSpec 'STInput stdin)
    , pcStdout :: !(StreamSpec 'STOutput stdout)
    , pcStderr :: !(StreamSpec 'STOutput stderr)
    , pcWorkingDir :: !(Maybe FilePath)
    , pcEnv :: !(Maybe [(String, String)])
    , pcCloseFds :: !Bool
    , pcCreateGroup :: !Bool
    , pcDelegateCtlc :: !Bool

#if MIN_VERSION_process(1, 3, 0)
    , pcDetachConsole :: !Bool
    , pcCreateNewConsole :: !Bool
    , pcNewSession :: !Bool
#endif

#if MIN_VERSION_process(1, 4, 0) && !WINDOWS
    , pcChildGroup :: !(Maybe GroupID)
    , pcChildUser :: !(Maybe UserID)
#endif

    , pcCheckExitCode :: !Bool
    }
instance (stdin ~ (), stdout ~ (), stderr ~ ())
  => IsString (ProcessConfig stdin stdout stderr) where
    fromString s
        | any (== ' ') s = shell s
        | otherwise = proc s []

-- | Whether a stream is an input stream or output stream. Note that
-- this is from the perspective of the /child process/, so that a
-- child's standard input stream is an @STInput@, even though the
-- parent process will be writing to it.
--
-- @since 0.1.0.0
data StreamType = STInput | STOutput

-- | A specification for how to create one of the three standard child
-- streams. See examples below.
--
-- @since 0.1.0.0
data StreamSpec (streamType :: StreamType) a = StreamSpec
    { ssStream :: !P.StdStream
    , ssCreate :: !(Maybe Handle -> Cleanup a)
    }
    deriving Functor

-- | This instance uses 'byteStringInput' to convert a raw string into
-- a stream of input for a child process.
--
-- @since 0.1.0.0
instance (streamType ~ 'STInput, res ~ ())
  => IsString (StreamSpec streamType res) where
    fromString = byteStringInput . fromString

-- | Internal type, to make for easier composition of cleanup actions.
--
-- @since 0.1.0.0
newtype Cleanup a = Cleanup { runCleanup :: IO (a, IO ()) }
    deriving Functor
instance Applicative Cleanup where
    pure x = Cleanup (return (x, return ()))
    Cleanup f <*> Cleanup x = Cleanup $ do
        (f', c1) <- f
        (`onException` c1) $ do
            (x', c2) <- x
            return (f' x', c1 `finally` c2)

-- | A running process. The three type parameters provide the type of
-- the standard input, standard output, and standard error streams.
--
-- @since 0.1.0.0
data Process stdin stdout stderr = Process
    { pCleanup :: !(IO ())
    , pStdin :: !stdin
    , pStdout :: !stdout
    , pStderr :: !stderr
    , pHandle :: !P.ProcessHandle
    , pExitCode :: !(TMVar ExitCode)
    }

-- | Internal helper
defaultProcessConfig :: ProcessConfig () () ()
defaultProcessConfig = ProcessConfig
    { pcCmdSpec = P.ShellCommand ""
    , pcStdin = inherit
    , pcStdout = inherit
    , pcStderr = inherit
    , pcWorkingDir = Nothing
    , pcEnv = Nothing
    , pcCloseFds = False
    , pcCreateGroup = False
    , pcDelegateCtlc = False

#if MIN_VERSION_process(1, 3, 0)
    , pcDetachConsole = False
    , pcCreateNewConsole = False
    , pcNewSession = False
#endif

#if MIN_VERSION_process(1, 4, 0) && !WINDOWS
    , pcChildGroup = Nothing
    , pcChildUser = Nothing
#endif

    , pcCheckExitCode = False
    }

-- | Create a 'ProcessConfig' from the given command and arguments.
--
-- @since 0.1.0.0
proc :: FilePath -> [String] -> ProcessConfig () () ()
proc cmd args = setProc cmd args defaultProcessConfig

-- | Internal helper
setProc :: FilePath -> [String]
        -> ProcessConfig stdin stdout stderr
        -> ProcessConfig stdin stdout stderr
setProc cmd args p = p { pcCmdSpec = P.RawCommand cmd args }

-- | Create a 'ProcessConfig' from the given shell command.
--
-- @since 0.1.0.0
shell :: String -> ProcessConfig () () ()
shell cmd = setShell cmd defaultProcessConfig

-- | Internal helper
setShell :: String
         -> ProcessConfig stdin stdout stderr
         -> ProcessConfig stdin stdout stderr
setShell cmd p = p { pcCmdSpec = P.ShellCommand cmd }

-- | Set the child's standard input stream to the given 'StreamSpec'.
--
-- Default: 'inherit'
--
-- @since 0.1.0.0
setStdin :: StreamSpec 'STInput stdin
         -> ProcessConfig stdin0 stdout stderr
         -> ProcessConfig stdin stdout stderr
setStdin spec pc = pc { pcStdin = spec }

-- | Set the child's standard output stream to the given 'StreamSpec'.
--
-- Default: 'inherit'
--
-- @since 0.1.0.0
setStdout :: StreamSpec 'STOutput stdout
          -> ProcessConfig stdin stdout0 stderr
          -> ProcessConfig stdin stdout stderr
setStdout spec pc = pc { pcStdout = spec }

-- | Set the child's standard error stream to the given 'StreamSpec'.
--
-- Default: 'inherit'
--
-- @since 0.1.0.0
setStderr :: StreamSpec 'STOutput stderr
          -> ProcessConfig stdin stdout stderr0
          -> ProcessConfig stdin stdout stderr
setStderr spec pc = pc { pcStderr = spec }

-- | Set the working directory of the child process.
--
-- Default: current process's working directory.
--
-- @since 0.1.0.0
setWorkingDir :: FilePath
              -> ProcessConfig stdin stdout stderr
              -> ProcessConfig stdin stdout stderr
setWorkingDir dir pc = pc { pcWorkingDir = Just dir }

-- | Set the environment variables of the child process.
--
-- Default: current process's environment.
--
-- @since 0.1.0.0
setEnv :: [(String, String)]
       -> ProcessConfig stdin stdout stderr
       -> ProcessConfig stdin stdout stderr
setEnv env pc = pc { pcEnv = Just env }

-- | Should we close all file descriptors besides stdin, stdout, and
-- stderr? See 'P.close_fds' for more information.
--
-- Default: False
--
-- @since 0.1.0.0
setCloseFds
    :: Bool
    -> ProcessConfig stdin stdout stderr
    -> ProcessConfig stdin stdout stderr
setCloseFds x pc = pc { pcCloseFds = x }

-- | Should we create a new process group?
--
-- Default: False
--
-- @since 0.1.0.0
setCreateGroup
    :: Bool
    -> ProcessConfig stdin stdout stderr
    -> ProcessConfig stdin stdout stderr
setCreateGroup x pc = pc { pcCreateGroup = x }

-- | Delegate handling of Ctrl-C to the child. For more information,
-- see 'P.delegate_ctlc'.
--
-- Default: False
--
-- @since 0.1.0.0
setDelegateCtlc
    :: Bool
    -> ProcessConfig stdin stdout stderr
    -> ProcessConfig stdin stdout stderr
setDelegateCtlc x pc = pc { pcDelegateCtlc = x }

#if MIN_VERSION_process(1, 3, 0)

-- | Detach console on Windows, see 'P.detach_console'.
--
-- Default: False
--
-- @since 0.1.0.0
setDetachConsole
    :: Bool
    -> ProcessConfig stdin stdout stderr
    -> ProcessConfig stdin stdout stderr
setDetachConsole x pc = pc { pcDetachConsole = x }

-- | Create new console on Windows, see 'P.create_new_console'.
--
-- Default: False
--
-- @since 0.1.0.0
setCreateNewConsole
    :: Bool
    -> ProcessConfig stdin stdout stderr
    -> ProcessConfig stdin stdout stderr
setCreateNewConsole x pc = pc { pcCreateNewConsole = x }

-- | Set a new session with the POSIX @setsid@ syscall, does nothing
-- on non-POSIX. See 'P.new_session'.
--
-- Default: False
--
-- @since 0.1.0.0
setNewSession
    :: Bool
    -> ProcessConfig stdin stdout stderr
    -> ProcessConfig stdin stdout stderr
setNewSession x pc = pc { pcNewSession = x }
#endif

#if MIN_VERSION_process(1, 4, 0) && !WINDOWS
-- | Set the child process's group ID with the POSIX @setgid@ syscall,
-- does nothing on non-POSIX. See 'P.child_group'.
--
-- Default: False
--
-- @since 0.1.0.0
setChildGroup
    :: GroupID
    -> ProcessConfig stdin stdout stderr
    -> ProcessConfig stdin stdout stderr
setChildGroup x pc = pc { pcChildGroup = Just x }

-- | Set the child process's user ID with the POSIX @setuid@ syscall,
-- does nothing on non-POSIX. See 'P.child_user'.
--
-- Default: False
--
-- @since 0.1.0.0
setChildUser
    :: UserID
    -> ProcessConfig stdin stdout stderr
    -> ProcessConfig stdin stdout stderr
setChildUser x pc = pc { pcChildUser = Just x }
#endif

-- | Should we throw an exception when the process exits with a
-- non-success code?
--
-- If set to 'True', then when 'stopProcess' is called - either
-- directly or via 'withProcess' or other wrappers - the processes
-- exit code will be checked. Any exit code besides 'ExitSuccess' will
-- result in an 'ExitCodeException' being thrown.
--
-- Default: 'False'
--
-- @since 0.1.0.0
setCheckExitCode :: Bool
                 -> ProcessConfig stdin stdout stderr
                 -> ProcessConfig stdin stdout stderr
setCheckExitCode x p = p { pcCheckExitCode = x }

-- TODO: Instead of having this setting, we could consider just having
-- alternatives to readProcess, runProcess, etc, that check the exit
-- code. This could actually be a really nice convention: readProcess
-- does not check, readProcess_ or readProcessCheck does.

-- | Create a new 'StreamSpec' from the given 'P.StdStream' and a
-- helper function. This function:
--
-- * Takes as input the raw @Maybe Handle@ returned by the
-- 'P.createProcess' function. This will be determined by the
-- 'P.StdStream' argument.
--
-- * Returns the actual stream value @a@, as well as a cleanup
-- * function to be run when calling 'stopProcess'.
--
-- @since 0.1.0.0
mkStreamSpec :: P.StdStream
             -> (Maybe Handle -> IO (a, IO ()))
             -> StreamSpec streamType a
mkStreamSpec ss f = StreamSpec ss (Cleanup . f)

-- | A stream spec which simply inherits the stream of the parent
-- process.
--
-- @since 0.1.0.0
inherit :: StreamSpec anyStreamType ()
inherit = mkStreamSpec P.Inherit (\Nothing -> pure ((), return ()))

-- | A stream spec which will close the stream for the child process.
--
-- @since 0.1.0.0
closed :: StreamSpec anyStreamType ()
#if MIN_VERSION_process(1, 4, 0)
closed = mkStreamSpec P.NoStream (\Nothing -> pure ((), return ()))
#else
closed = mkStreamSpec P.CreatePipe (\(Just h) -> (((), return ()) <$ hClose h))
#endif

-- | An input stream spec which sets the input to the given
-- 'L.ByteString'. A separate thread will be forked to write the
-- contents to the child process.
--
-- @since 0.1.0.0
byteStringInput :: L.ByteString -> StreamSpec 'STInput ()
byteStringInput lbs = StreamSpec P.CreatePipe $ \(Just h) -> Cleanup $ do
    void $ async $ do
        L.hPut h lbs
        hClose h
    return ((), hClose h)

-- | Capture the output of a process in a 'L.ByteString'.
--
-- This function will fork a separate thread to consume all input from
-- the process, and will only make the results available when the
-- underlying 'Handle' is closed. As this is provided as an 'STM'
-- action, you can either check if the result is available, or block
-- until it's ready.
--
-- In the event of any exception occurring when reading from the
-- 'Handle', the result of this function will be a 'Left' value
-- containing a 'ByteStringOutputException'.
--
-- @since 0.1.0.0
byteStringOutput :: StreamSpec 'STOutput (STM (Either ByteStringOutputException L.ByteString))
byteStringOutput = StreamSpec P.CreatePipe $ \(Just h) -> Cleanup $ do
    mvar <- newEmptyTMVarIO

    void $ async $ do
        let loop front = do
                bs <- S.hGetSome h defaultChunkSize
                if S.null bs
                    then atomically $ putTMVar mvar $ Right $ L.fromChunks $ front []
                    else loop $ front . (bs:)
        loop id `catch` \e -> do
            atomically $ void $ tryPutTMVar mvar $ Left $ ByteStringOutputException e
            throwIO e

    return (readTMVar mvar, hClose h)

-- | Create a new pipe between this process and the child, and return
-- a 'Handle' to communicate with the child.
--
-- @since 0.1.0.0
createPipe :: StreamSpec anyStreamType Handle
createPipe = StreamSpec P.CreatePipe $ \(Just h) -> Cleanup $ return (h, hClose h)

-- | Use the provided 'Handle' for the child process, and when the
-- process exits, do /not/ close it. This is useful if, for example,
-- you want to have multiple processes write to the same log file
-- sequentially.
--
-- @since 0.1.0.0
useHandleOpen :: Handle -> StreamSpec anyStreamType ()
useHandleOpen h = StreamSpec (P.UseHandle h) $ \Nothing -> Cleanup $ return ((), return ())

-- | Use the provided 'Handle' for the child process, and when the
-- process exits, close it. If you have no reason to keep the 'Handle'
-- open, you should use this over 'useHandleOpen'.
--
-- @since 0.1.0.0
useHandleClose :: Handle -> StreamSpec anyStreamType ()
useHandleClose h = StreamSpec (P.UseHandle h) $ \Nothing -> Cleanup $ return ((), hClose h)

-- | Provide input to a process by writing to a conduit.
--
-- @since 0.1.0.0
sink :: MonadIO m => StreamSpec 'STInput (ConduitM S.ByteString o m ())
sink =
    (\h -> C.addCleanup (\_ -> liftIO $ hClose h) (CB.sinkHandle h))
    <$> createPipe

-- | Read output from a process by read from a conduit.
--
-- @since 0.1.0.0
source :: MonadIO m => StreamSpec 'STOutput (ConduitM i S.ByteString m ())
source =
    (\h -> C.addCleanup (\_ -> liftIO $ hClose h) (CB.sourceHandle h))
    <$> createPipe

-- | Launch a process based on the given 'ProcessConfig'. You should
-- ensure that you close 'stopProcess' on the result. It's usually
-- better to use one of the functions in this module which ensures
-- 'stopProcess' is called, such as 'withProcess'.
--
-- @since 0.1.0.0
startProcess :: MonadIO m
             => ProcessConfig stdin stdout stderr
             -> m (Process stdin stdout stderr)
startProcess ProcessConfig {..} = liftIO $ do
    let cp0 =
            case pcCmdSpec of
                P.ShellCommand cmd -> P.shell cmd
                P.RawCommand cmd args -> P.proc cmd args
        cp = cp0
            { P.std_in = ssStream pcStdin
            , P.std_out = ssStream pcStdout
            , P.std_err = ssStream pcStderr
            , P.cwd = pcWorkingDir
            , P.env = pcEnv
            , P.close_fds = pcCloseFds
            , P.create_group = pcCreateGroup
            , P.delegate_ctlc = pcDelegateCtlc

#if MIN_VERSION_process(1, 3, 0)
            , P.detach_console = pcDetachConsole
            , P.create_new_console = pcCreateNewConsole
            , P.new_session = pcNewSession
#endif

#if MIN_VERSION_process(1, 4, 0) && !WINDOWS
            , P.child_group = pcChildGroup
            , P.child_user = pcChildUser
#endif

            }

    (minH, moutH, merrH, pHandle) <- P.createProcess_ "startProcess" cp

    ((pStdin, pStdout, pStderr), pCleanup1) <- runCleanup $ (,,)
        <$> ssCreate pcStdin minH
        <*> ssCreate pcStdout moutH
        <*> ssCreate pcStderr merrH

    pExitCode <- newEmptyTMVarIO
    void $ async $ do
        ec <- P.waitForProcess pHandle
        atomically $ putTMVar pExitCode ec

    let pCleanup2 = pCleanup1 `finally` do
            mec <- atomically $ tryReadTMVar pExitCode
            case mec of
                Nothing -> do
                    P.terminateProcess pHandle
                    -- TODO: should we put in a timeout and then send
                    -- a SIGKILL on Unix?
                    void $ atomically $ readTMVar pExitCode
                Just _ -> return ()
        pCleanup
            | pcCheckExitCode = do
                eres <- try pCleanup2
                ec <- atomically $ readTMVar pExitCode
                case (ec, eres) of
                    (ExitSuccess, Right ()) -> return ()
                    (ExitSuccess, Left e) -> throwIO e
                    _ -> throwIO $ ExitCodeException ec $ either Just (const Nothing) eres
            | otherwise = pCleanup2

    return Process {..}

-- | Close a process and release any resources acquired. This will
-- ensure 'P.terminateProcess' is called, wait for the process to
-- actually exit, and then close out resources allocated for the
-- streams. In the event of any cleanup exceptions being thrown, or if
-- a non-success exit code was received and 'setCheckExitCode' was
-- used, this will throw an exception.
--
-- @since 0.1.0.0
stopProcess :: MonadIO m
            => Process stdin stdout stderr
            -> m ()
stopProcess = liftIO . pCleanup

-- | Use the bracket pattern to call 'startProcess' and ensure
-- 'stopProcess' is called.
--
-- @since 0.1.0.0
withProcess :: (MonadIO m, C.MonadMask m)
            => ProcessConfig stdin stdout stderr
            -> (Process stdin stdout stderr -> m a)
            -> m a
withProcess config = C.bracket (startProcess config) stopProcess

-- | Run a process, capture its standard output and error as a
-- 'L.ByteString', wait for it to complete, and then return its exit
-- code, output, and error.
--
-- Note that any previously used 'setStdout' or 'setStderr' will be
-- overridden.
--
-- @since 0.1.0.0
readProcess :: MonadIO m
            => ProcessConfig stdin stdoutIgnored stderrIgnored
            -> m (ExitCode, L.ByteString, L.ByteString)
readProcess pc =
    liftIO $ withProcess pc' $ \p -> atomically $ (,,)
        <$> waitExitCodeSTM p
        <*> (getStdout p >>= either throwSTM return)
        <*> (getStderr p >>= either throwSTM return)
  where
    pc' = setStdout byteStringOutput
        $ setStderr byteStringOutput pc

-- | Run the given process, wait for it to exit, and returns its
-- 'ExitCode'.
--
-- @since 0.1.0.0
runProcess :: MonadIO m
           => ProcessConfig stdin stdout stderr
           -> m ExitCode
runProcess pc = liftIO $ withProcess pc waitExitCode

-- | Same as 'runProcess', but ignores the 'ExitCode'.
--
-- @since 0.1.0.0
runProcess_ :: MonadIO m
            => ProcessConfig stdin stdout stderr
            -> m ()
runProcess_ = void . runProcess

-- | Wait for the process to exit and then return its 'ExitCode'.
--
-- @since 0.1.0.0
waitExitCode :: MonadIO m => Process stdin stdout stderr -> m ExitCode
waitExitCode = liftIO . atomically . waitExitCodeSTM

-- | Same as 'waitExitCode', but in 'STM'.
--
-- @since 0.1.0.0
waitExitCodeSTM :: Process stdin stdout stderr -> STM ExitCode
waitExitCodeSTM = readTMVar . pExitCode

-- | Check if a process has exited and, if so, return its 'ExitCode'.
--
-- @since 0.1.0.0
checkExitCode :: MonadIO m => Process stdin stdout stderr -> m (Maybe ExitCode)
checkExitCode = liftIO . atomically . checkExitCodeSTM

-- | Same as 'checkExitCode', but in 'STM'.
--
-- @since 0.1.0.0
checkExitCodeSTM :: Process stdin stdout stderr -> STM (Maybe ExitCode)
checkExitCodeSTM = tryReadTMVar . pExitCode

-- | Get the child's standard input stream value.
--
-- @since 0.1.0.0
getStdin :: Process stdin stdout stderr -> stdin
getStdin = pStdin

-- | Get the child's standard output stream value.
--
-- @since 0.1.0.0
getStdout :: Process stdin stdout stderr -> stdout
getStdout = pStdout

-- | Get the child's standard error stream value.
--
-- @since 0.1.0.0
getStderr :: Process stdin stdout stderr -> stderr
getStderr = pStderr

-- | Exit code generated by 'stopProcess' when 'setCheckExitCode' is
-- 'True' and a process exits with a non-success code. Contains the
-- non-success code, and if any other exceptions occur during cleanup,
-- that exception.
--
-- @since 0.1.0.0
data ExitCodeException = ExitCodeException ExitCode (Maybe SomeException)
    deriving (Show, Typeable)
instance Exception ExitCodeException

-- | Wrapper for when an exception is thrown when reading from a child
-- process, used by 'byteStringOutput'.
--
-- @since 0.1.0.0
newtype ByteStringOutputException = ByteStringOutputException SomeException
    deriving (Show, Typeable)
instance Exception ByteStringOutputException
