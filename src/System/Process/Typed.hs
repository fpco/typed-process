{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , setWorkingDirInherit
    , setEnv
    , setEnvInherit
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
    , setChildGroupInherit
    , setChildUser
    , setChildUserInherit
#endif

      -- * Stream specs
    , mkStreamSpec
    , inherit
    , closed
    , byteStringInput
    , byteStringOutput
    , createPipe
    , useHandleOpen
    , useHandleClose

      -- * Launch a process
    , startProcess
    , stopProcess
    , withProcess
    , withProcess_
    , readProcess
    , readProcess_
    , runProcess
    , runProcess_
    , readProcessStdout
    , readProcessStdout_
    , readProcessStderr
    , readProcessStderr_

      -- * Interact with a process

      -- ** Process exit code
    , waitExitCode
    , waitExitCodeSTM
    , getExitCode
    , getExitCodeSTM
    , checkExitCode
    , checkExitCodeSTM

      -- ** Process streams
    , getStdin
    , getStdout
    , getStderr

      -- * Exceptions
    , ExitCodeException (..)
    , ByteStringOutputException (..)
      -- * Unsafe functions
    , unsafeProcessHandle
    ) where

import qualified Data.ByteString as S
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Exception (assert, evaluate, throwIO, Exception, SomeException, finally, bracket, onException, catch)
import Control.Monad (void)
import Control.Monad.IO.Class
import qualified System.Process as P
import Data.Typeable (Typeable)
import System.IO (Handle, hClose)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, waitCatch)
import Control.Concurrent.STM (newEmptyTMVarIO, atomically, putTMVar, TMVar, readTMVar, tryReadTMVar, STM, tryPutTMVar, throwSTM, catchSTM)
import System.Exit (ExitCode (ExitSuccess))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.String (IsString (fromString))

#if MIN_VERSION_process(1, 4, 0) && !WINDOWS
import System.Posix.Types (GroupID, UserID)
#endif

#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative (Applicative (..), (<$>), (<$))
#endif

#if !MIN_VERSION_process(1, 3, 0)
import qualified System.Process.Internals as P (createProcess_)
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
    }
instance Show (ProcessConfig stdin stdout stderr) where
    show pc = concat
        [ case pcCmdSpec pc of
            P.ShellCommand s -> "Shell command: " ++ s
            P.RawCommand x xs -> "Raw command: " ++ unwords (map escape (x:xs))
        , "\n"
        , case pcWorkingDir pc of
            Nothing -> ""
            Just wd -> concat
                [ "Run from: "
                , wd
                , "\n"
                ]
        , case pcEnv pc of
            Nothing -> ""
            Just e -> unlines
                $ "Modified environment:"
                : map (\(k, v) -> concat [k, "=", v]) e
        ]
      where
        escape x
            | any (`elem` " \\\"'") x = show x
            | otherwise = x
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
    , ssCreate :: !(ProcessConfig () () () -> Maybe Handle -> Cleanup a)
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
    { pConfig :: !(ProcessConfig () () ())
    , pCleanup :: !(IO ())
    , pStdin :: !stdin
    , pStdout :: !stdout
    , pStderr :: !stderr
    , pHandle :: !P.ProcessHandle
    , pExitCode :: !(TMVar ExitCode)
    }
instance Show (Process stdin stdout stderr) where
    show p = "Running process: " ++ show (pConfig p)

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

-- | Inherit the working directory from the parent process.
--
-- @since 0.2.2.0
setWorkingDirInherit
  :: ProcessConfig stdin stdout stderr
  -> ProcessConfig stdin stdout stderr
setWorkingDirInherit pc = pc { pcWorkingDir = Nothing }

-- | Set the environment variables of the child process.
--
-- Default: current process's environment.
--
-- @since 0.1.0.0
setEnv :: [(String, String)]
       -> ProcessConfig stdin stdout stderr
       -> ProcessConfig stdin stdout stderr
setEnv env pc = pc { pcEnv = Just env }

-- | Inherit the environment variables from the parent process.
--
-- @since 0.2.2.0
setEnvInherit
  :: ProcessConfig stdin stdout stderr
  -> ProcessConfig stdin stdout stderr
setEnvInherit pc = pc { pcEnv = Nothing }

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

-- | Inherit the group from the parent process.
--
-- @since 0.2.2.0
setChildGroupInherit
  :: ProcessConfig stdin stdout stderr
  -> ProcessConfig stdin stdout stderr
setChildGroupInherit pc = pc { pcChildGroup = Nothing }

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

-- | Inherit the user from the parent process.
--
-- @since 0.2.2.0
setChildUserInherit
  :: ProcessConfig stdin stdout stderr
  -> ProcessConfig stdin stdout stderr
setChildUserInherit pc = pc { pcChildUser = Nothing }
#endif

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
             -> (ProcessConfig () () () -> Maybe Handle -> IO (a, IO ()))
             -> StreamSpec streamType a
mkStreamSpec ss f = StreamSpec ss (\pc mh -> Cleanup (f pc mh))

-- | A stream spec which simply inherits the stream of the parent
-- process.
--
-- @since 0.1.0.0
inherit :: StreamSpec anyStreamType ()
inherit = mkStreamSpec P.Inherit (\_ Nothing -> pure ((), return ()))

-- | A stream spec which will close the stream for the child process.
--
-- @since 0.1.0.0
closed :: StreamSpec anyStreamType ()
#if MIN_VERSION_process(1, 4, 0)
closed = mkStreamSpec P.NoStream (\_ Nothing -> pure ((), return ()))
#else
closed = mkStreamSpec P.CreatePipe (\_ (Just h) -> (((), return ()) <$ hClose h))
#endif

-- | An input stream spec which sets the input to the given
-- 'L.ByteString'. A separate thread will be forked to write the
-- contents to the child process.
--
-- @since 0.1.0.0
byteStringInput :: L.ByteString -> StreamSpec 'STInput ()
byteStringInput lbs = mkStreamSpec P.CreatePipe $ \_ (Just h) -> do
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
-- 'Handle', the 'STM' action will throw a
-- 'ByteStringOutputException'.
--
-- @since 0.1.0.0
byteStringOutput :: StreamSpec 'STOutput (STM L.ByteString)
byteStringOutput = mkStreamSpec P.CreatePipe $ \pc (Just h) -> do
    mvar <- newEmptyTMVarIO

    void $ async $ do
        let loop front = do
                bs <- S.hGetSome h defaultChunkSize
                if S.null bs
                    then atomically $ putTMVar mvar $ Right $ L.fromChunks $ front []
                    else loop $ front . (bs:)
        loop id `catch` \e -> do
            atomically $ void $ tryPutTMVar mvar $ Left $ ByteStringOutputException e pc
            throwIO e

    return (readTMVar mvar >>= either throwSTM return, hClose h)

-- | Create a new pipe between this process and the child, and return
-- a 'Handle' to communicate with the child.
--
-- @since 0.1.0.0
createPipe :: StreamSpec anyStreamType Handle
createPipe = mkStreamSpec P.CreatePipe $ \_ (Just h) -> return (h, hClose h)

-- | Use the provided 'Handle' for the child process, and when the
-- process exits, do /not/ close it. This is useful if, for example,
-- you want to have multiple processes write to the same log file
-- sequentially.
--
-- @since 0.1.0.0
useHandleOpen :: Handle -> StreamSpec anyStreamType ()
useHandleOpen h = mkStreamSpec (P.UseHandle h) $ \_ Nothing -> return ((), return ())

-- | Use the provided 'Handle' for the child process, and when the
-- process exits, close it. If you have no reason to keep the 'Handle'
-- open, you should use this over 'useHandleOpen'.
--
-- @since 0.1.0.0
useHandleClose :: Handle -> StreamSpec anyStreamType ()
useHandleClose h = mkStreamSpec (P.UseHandle h) $ \_ Nothing -> return ((), hClose h)

-- | Launch a process based on the given 'ProcessConfig'. You should
-- ensure that you close 'stopProcess' on the result. It's usually
-- better to use one of the functions in this module which ensures
-- 'stopProcess' is called, such as 'withProcess'.
--
-- @since 0.1.0.0
startProcess :: MonadIO m
             => ProcessConfig stdin stdout stderr
             -> m (Process stdin stdout stderr)
startProcess pConfig'@ProcessConfig {..} = liftIO $ do
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
        <$> ssCreate pcStdin  pConfig minH
        <*> ssCreate pcStdout pConfig moutH
        <*> ssCreate pcStderr pConfig merrH

    pExitCode <- newEmptyTMVarIO
    waitingThread <- async $ do
        ec <-
          if multiThreadedRuntime
            then P.waitForProcess pHandle
            else
              let loop = do
                    threadDelay 20000
                    mec <- P.getProcessExitCode pHandle
                    case mec of
                      Nothing -> loop
                      Just ec -> pure ec
               in loop
        atomically $ putTMVar pExitCode ec
        return ec

    let pCleanup = pCleanup1 `finally` do
            -- First: stop calling waitForProcess, so that we can
            -- avoid race conditions where the process is removed from
            -- the system process table while we're trying to
            -- terminate it.
            cancel waitingThread

            -- Now check if the process had already exited
            eec <- waitCatch waitingThread

            case eec of
                -- Process already exited, nothing to do
                Right _ec -> return ()

                -- Process didn't exit yet, let's terminate it and
                -- then call waitForProcess ourselves
                Left _ -> do
                    P.terminateProcess pHandle
                    ec <- P.waitForProcess pHandle
                    success <- atomically $ tryPutTMVar pExitCode ec
                    evaluate $ assert success ()

    return Process {..}
  where
    pConfig = clearStreams pConfig'

foreign import ccall unsafe "rtsSupportsBoundThreads"
  multiThreadedRuntime :: Bool

-- | Close a process and release any resources acquired. This will
-- ensure 'P.terminateProcess' is called, wait for the process to
-- actually exit, and then close out resources allocated for the
-- streams. In the event of any cleanup exceptions being thrown this
-- will throw an exception.
--
-- @since 0.1.0.0
stopProcess :: MonadIO m
            => Process stdin stdout stderr
            -> m ()
stopProcess = liftIO . pCleanup

-- | Uses the bracket pattern to call 'startProcess' and ensures that
-- 'stopProcess' is called.
--
-- In version 0.2.0.0, this function was monomorphized to @IO@ to
-- avoid a dependency on the exceptions package.
--
-- @since 0.1.0.0
withProcess :: ProcessConfig stdin stdout stderr
            -> (Process stdin stdout stderr -> IO a)
            -> IO a
withProcess config = bracket (startProcess config) stopProcess

-- | Same as 'withProcess', but also calls 'checkExitCode'
--
-- In version 0.2.0.0, this function was monomorphized to @IO@ to
-- avoid a dependency on the exceptions package.
--
-- @since 0.1.0.0
withProcess_ :: ProcessConfig stdin stdout stderr
             -> (Process stdin stdout stderr -> IO a)
             -> IO a
withProcess_ config = bracket
    (startProcess config)
    (\p -> stopProcess p `finally` checkExitCode p)

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
        <*> getStdout p
        <*> getStderr p
  where
    pc' = setStdout byteStringOutput
        $ setStderr byteStringOutput pc

-- | Same as 'readProcess', but instead of returning the 'ExitCode',
-- checks it with 'checkExitCode'.
--
-- @since 0.1.0.0
readProcess_ :: MonadIO m
             => ProcessConfig stdin stdoutIgnored stderrIgnored
             -> m (L.ByteString, L.ByteString)
readProcess_ pc =
    liftIO $ withProcess pc' $ \p -> atomically $ do
        stdout <- getStdout p
        stderr <- getStderr p
        checkExitCodeSTM p `catchSTM` \ece -> throwSTM ece
            { eceStdout = stdout
            , eceStderr = stderr
            }
        return (stdout, stderr)
  where
    pc' = setStdout byteStringOutput
        $ setStderr byteStringOutput pc

-- | Same as 'readProcess', but only read the stdout of the process. Original settings for stderr remain.
--
-- @since 0.2.1.0
readProcessStdout
  :: MonadIO m
  => ProcessConfig stdin stdoutIgnored stderr
  -> m (ExitCode, L.ByteString)
readProcessStdout pc =
    liftIO $ withProcess pc' $ \p -> atomically $ (,)
        <$> waitExitCodeSTM p
        <*> getStdout p
  where
    pc' = setStdout byteStringOutput pc

-- | Same as 'readProcessStdout', but instead of returning the
-- 'ExitCode', checks it with 'checkExitCode'.
--
-- @since 0.2.1.0
readProcessStdout_
  :: MonadIO m
  => ProcessConfig stdin stdoutIgnored stderr
  -> m L.ByteString
readProcessStdout_ pc =
    liftIO $ withProcess pc' $ \p -> atomically $ do
        stdout <- getStdout p
        checkExitCodeSTM p `catchSTM` \ece -> throwSTM ece
            { eceStdout = stdout
            }
        return stdout
  where
    pc' = setStdout byteStringOutput pc

-- | Same as 'readProcess', but only read the stderr of the process. Original settings for stderr remain.
--
-- @since 0.2.1.0
readProcessStderr
  :: MonadIO m
  => ProcessConfig stdin stderrIgnored stderr
  -> m (ExitCode, L.ByteString)
readProcessStderr pc =
    liftIO $ withProcess pc' $ \p -> atomically $ (,)
        <$> waitExitCodeSTM p
        <*> getStderr p
  where
    pc' = setStderr byteStringOutput pc

-- | Same as 'readProcessStderr', but instead of returning the
-- 'ExitCode', checks it with 'checkExitCode'.
--
-- @since 0.2.1.0
readProcessStderr_
  :: MonadIO m
  => ProcessConfig stdin stderrIgnored stderr
  -> m L.ByteString
readProcessStderr_ pc =
    liftIO $ withProcess pc' $ \p -> atomically $ do
        stderr <- getStderr p
        checkExitCodeSTM p `catchSTM` \ece -> throwSTM ece
            { eceStderr = stderr
            }
        return stderr
  where
    pc' = setStderr byteStringOutput pc

-- | Run the given process, wait for it to exit, and returns its
-- 'ExitCode'.
--
-- @since 0.1.0.0
runProcess :: MonadIO m
           => ProcessConfig stdin stdout stderr
           -> m ExitCode
runProcess pc = liftIO $ withProcess pc waitExitCode

-- | Same as 'runProcess', but instead of returning the
-- 'ExitCode', checks it with 'checkExitCode'.
--
-- @since 0.1.0.0
runProcess_ :: MonadIO m
            => ProcessConfig stdin stdout stderr
            -> m ()
runProcess_ pc = liftIO $ withProcess pc checkExitCode

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
getExitCode :: MonadIO m => Process stdin stdout stderr -> m (Maybe ExitCode)
getExitCode = liftIO . atomically . getExitCodeSTM

-- | Same as 'getExitCode', but in 'STM'.
--
-- @since 0.1.0.0
getExitCodeSTM :: Process stdin stdout stderr -> STM (Maybe ExitCode)
getExitCodeSTM = tryReadTMVar . pExitCode

-- | Wait for a process to exit, and ensure that it exited
-- successfully. If not, throws an 'ExitCodeException'.
--
-- @since 0.1.0.0
checkExitCode :: MonadIO m => Process stdin stdout stderr -> m ()
checkExitCode = liftIO . atomically . checkExitCodeSTM

-- | Same as 'checkExitCode', but in 'STM'.
--
-- @since 0.1.0.0
checkExitCodeSTM :: Process stdin stdout stderr -> STM ()
checkExitCodeSTM p = do
    ec <- readTMVar (pExitCode p)
    case ec of
        ExitSuccess -> return ()
        _ -> throwSTM ExitCodeException
            { eceExitCode = ec
            , eceProcessConfig = clearStreams (pConfig p)
            , eceStdout = L.empty
            , eceStderr = L.empty
            }

-- | Internal
clearStreams :: ProcessConfig stdin stdout stderr -> ProcessConfig () () ()
clearStreams pc = pc
    { pcStdin = inherit
    , pcStdout = inherit
    , pcStderr = inherit
    }

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

-- | Exception thrown by 'checkExitCode' in the event of a non-success
-- exit code. Note that 'checkExitCode' is called by other functions
-- as well, like 'runProcess_' or 'readProcess_'.
--
-- @since 0.1.0.0
data ExitCodeException = ExitCodeException
    { eceExitCode :: ExitCode
    , eceProcessConfig :: ProcessConfig () () ()
    , eceStdout :: L.ByteString
    , eceStderr :: L.ByteString
    }
    deriving Typeable
instance Exception ExitCodeException
instance Show ExitCodeException where
    show ece = concat
        [ "Received "
        , show (eceExitCode ece)
        , " when running\n"
        -- Too much output for an exception if we show the modified
        -- environment, so hide it
        , show (eceProcessConfig ece) { pcEnv = Nothing }
        , if L.null (eceStdout ece)
            then ""
            else "Standard output:\n\n" ++ L8.unpack (eceStdout ece)
        , if L.null (eceStderr ece)
            then ""
            else "Standard error:\n\n" ++ L8.unpack (eceStderr ece)
        ]

-- | Wrapper for when an exception is thrown when reading from a child
-- process, used by 'byteStringOutput'.
--
-- @since 0.1.0.0
data ByteStringOutputException = ByteStringOutputException SomeException (ProcessConfig () () ())
    deriving (Show, Typeable)
instance Exception ByteStringOutputException

-- | Take 'System.Process.ProcessHandle' out of the 'Process'.
-- This method is needed in cases one need to use low level functions
-- from the @process@ package. Use cases for this method are:
--
--   1. Send a special signal to the process.
--   2. Terminate the process group instead of terminating single process.
--   3. Use platform specific API on the underlying process.
--
-- This method is considered unsafe because the actions it performs on
-- the underlying process may overlap with the functionality that
-- @typed-process@ provides. For example the user should not call
-- 'System.Process.waitForProcess' on the process handle as eiter
-- 'System.Process.waitForProcess' or 'stopProcess' will lock.
-- Additionally, even if process was terminated by the
-- 'System.Process.terminateProcess' or by sending signal,
-- 'stopProcess' should be called either way in order to cleanup resources
-- allocated by the @typed-process@.
--
-- @since 0.1.1
unsafeProcessHandle :: Process stdin stdout stderr -> P.ProcessHandle
unsafeProcessHandle = pHandle
