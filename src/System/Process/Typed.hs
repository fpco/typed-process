{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The simplest way to get started with this API is to turn on
-- @OverloadedStrings@ and call 'runProcess'.  The following will
-- write the contents of @/home@ to @stdout@ and then print the exit
-- code (on a UNIX system).
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
--
-- 'runProcess' "ls -l /home" >>= print
-- @
--
-- Please see the [README.md](https://github.com/fpco/typed-process#readme)
-- file for more examples of using this API.
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

      -- | #processconfigsetters#

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

      -- | #streamspecs#

      -- * Stream specs
      -- ** Built-in stream specs
    , inherit
    , nullStream
    , closed
    , byteStringInput
    , byteStringOutput
    , createPipe
    , useHandleOpen
    , useHandleClose

    -- ** Create your own stream spec
    , mkStreamSpec
    , mkPipeStreamSpec

      -- | #launchaprocess#

      -- * Launch a process
    , runProcess
    , readProcess
    , readProcessStdout
    , readProcessStderr
    , readProcessInterleaved
    , withProcessWait
    , withProcessTerm
    , startProcess
    , stopProcess
      -- ** Exception-throwing functions
      -- | The functions ending in underbar (@_@) are the same as
      -- their counterparts without underbar but instead of returning
      -- an 'ExitCode' they throw 'ExitCodeException' on failure.
    , runProcess_
    , readProcess_
    , readProcessStdout_
    , readProcessStderr_
    , readProcessInterleaved_
    , withProcessWait_
    , withProcessTerm_

      -- | #interactwithaprocess#

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

      -- * Re-exports
    , ExitCode (..)
    , P.StdStream (..)

      -- * Unsafe functions
    , unsafeProcessHandle
      -- * Deprecated functions
    , withProcess
    , withProcess_
    ) where

import qualified Data.ByteString as S
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Control.Exception as E
import Control.Exception hiding (bracket, finally)
import Control.Monad (void)
import Control.Monad.IO.Class
import qualified System.Process as P
import Data.Typeable (Typeable)
import System.IO (Handle, hClose, IOMode(ReadWriteMode), withBinaryFile)
import System.IO.Error (isPermissionError)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, asyncWithUnmask, cancel, waitCatch)
import Control.Concurrent.STM (newEmptyTMVarIO, atomically, putTMVar, TMVar, readTMVar, tryReadTMVar, STM, tryPutTMVar, throwSTM, catchSTM)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.Process.Typed.Internal
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.String (IsString (fromString))
import GHC.RTS.Flags (getConcFlags, ctxtSwitchTime)
import Control.Monad.IO.Unlift

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
-- [setters below](#processconfigsetters) for default values.
--
-- Once you have a @ProcessConfig@ you can launch a process from it
-- using the functions in the section [Launch a
-- process](#launchaprocess).
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
            | x == "" = "\"\""
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
-- streams, @stdin@, @stdout@ and @stderr@. A 'StreamSpec' can be
-- thought of as containing
--
-- 1. A type safe version of 'P.StdStream' from "System.Process".
-- This determines whether the stream should be inherited from the
-- parent process, piped to or from a 'Handle', etc.
--
-- 2. A means of accessing the stream as a value of type @a@
--
-- 3. A cleanup action which will be run on the stream once the
-- process terminates
--
-- To create a @StreamSpec@ see the section [Stream
-- specs](#streamspecs).
--
-- @since 0.1.0.0
data StreamSpec (streamType :: StreamType) a = StreamSpec
    { ssStream :: !(forall b. (P.StdStream -> IO b) -> IO b)
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
-- To interact with a @Process@ use the functions from the section
-- [Interact with a process](#interactwithaprocess).
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
         -- ^
         -> ProcessConfig stdin0 stdout stderr
         -- ^
         -> ProcessConfig stdin stdout stderr
setStdin spec pc = pc { pcStdin = spec }

-- | Set the child's standard output stream to the given 'StreamSpec'.
--
-- Default: 'inherit'
--
-- @since 0.1.0.0
setStdout :: StreamSpec 'STOutput stdout
          -- ^
          -> ProcessConfig stdin stdout0 stderr
          -- ^
          -> ProcessConfig stdin stdout stderr
setStdout spec pc = pc { pcStdout = spec }

-- | Set the child's standard error stream to the given 'StreamSpec'.
--
-- Default: 'inherit'
--
-- @since 0.1.0.0
setStderr :: StreamSpec 'STOutput stderr
          -- ^
          -> ProcessConfig stdin stdout stderr0
          -- ^
          -> ProcessConfig stdin stdout stderr
setStderr spec pc = pc { pcStderr = spec }

-- | Set the working directory of the child process.
--
-- Default: current process's working directory.
--
-- @since 0.1.0.0
setWorkingDir :: FilePath
              -- ^
              -> ProcessConfig stdin stdout stderr
              -- ^
              -> ProcessConfig stdin stdout stderr
setWorkingDir dir pc = pc { pcWorkingDir = Just dir }

-- | Inherit the working directory from the parent process.
--
-- @since 0.2.2.0
setWorkingDirInherit
  :: ProcessConfig stdin stdout stderr
  -- ^
  -> ProcessConfig stdin stdout stderr
setWorkingDirInherit pc = pc { pcWorkingDir = Nothing }

-- | Set the environment variables of the child process.
--
-- Default: current process's environment.
--
-- @since 0.1.0.0
setEnv :: [(String, String)]
       -- ^
       -> ProcessConfig stdin stdout stderr
       -- ^
       -> ProcessConfig stdin stdout stderr
setEnv env pc = pc { pcEnv = Just env }

-- | Inherit the environment variables from the parent process.
--
-- @since 0.2.2.0
setEnvInherit
  :: ProcessConfig stdin stdout stderr
  -- ^
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
    -- ^
    -> ProcessConfig stdin stdout stderr
    -- ^
    -> ProcessConfig stdin stdout stderr
setCloseFds x pc = pc { pcCloseFds = x }

-- | Should we create a new process group?
--
-- Default: False
--
-- @since 0.1.0.0
setCreateGroup
    :: Bool
    -- ^
    -> ProcessConfig stdin stdout stderr
    -- ^
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
    -- ^
    -> ProcessConfig stdin stdout stderr
    -- ^
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
    -- ^
    -> ProcessConfig stdin stdout stderr
    -- ^
    -> ProcessConfig stdin stdout stderr
setDetachConsole x pc = pc { pcDetachConsole = x }

-- | Create new console on Windows, see 'P.create_new_console'.
--
-- Default: False
--
-- @since 0.1.0.0
setCreateNewConsole
    :: Bool
    -- ^
    -> ProcessConfig stdin stdout stderr
    -- ^
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
    -- ^
    -> ProcessConfig stdin stdout stderr
    -- ^
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
    -- ^
    -> ProcessConfig stdin stdout stderr
    -- ^
    -> ProcessConfig stdin stdout stderr
setChildGroup x pc = pc { pcChildGroup = Just x }

-- | Inherit the group from the parent process.
--
-- @since 0.2.2.0
setChildGroupInherit
  :: ProcessConfig stdin stdout stderr
  -- ^
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
    -- ^
    -> ProcessConfig stdin stdout stderr
    -- ^
    -> ProcessConfig stdin stdout stderr
setChildUser x pc = pc { pcChildUser = Just x }

-- | Inherit the user from the parent process.
--
-- @since 0.2.2.0
setChildUserInherit
  :: ProcessConfig stdin stdout stderr
  -- ^
  -> ProcessConfig stdin stdout stderr
setChildUserInherit pc = pc { pcChildUser = Nothing }
#endif

-- | Create a new 'StreamSpec' from the given 'P.StdStream' and a
-- helper function. This function:
--
-- * Takes as input the raw @Maybe Handle@ returned by the
-- 'P.createProcess' function. The handle will be @Just@ 'Handle' if the
-- 'P.StdStream' argument is 'P.CreatePipe' and @Nothing@ otherwise.
-- See 'P.createProcess' for more details.
--
-- * Returns the actual stream value @a@, as well as a cleanup
-- function to be run when calling 'stopProcess'.
--
-- If making a 'StreamSpec' with 'P.CreatePipe', prefer 'mkPipeStreamSpec',
-- which encodes the invariant that a 'Handle' is created.
--
-- @since 0.1.0.0
mkStreamSpec :: P.StdStream
             -- ^
             -> (ProcessConfig () () () -> Maybe Handle -> IO (a, IO ()))
             -- ^
             -> StreamSpec streamType a
mkStreamSpec ss f = mkManagedStreamSpec ($ ss) f

-- | Create a new 'P.CreatePipe' 'StreamSpec' from the given function.
-- This function:
--
-- * Takes as input the @Handle@ returned by the 'P.createProcess' function.
-- See 'P.createProcess' for more details.
--
-- * Returns the actual stream value @a@, as well as a cleanup
-- function to be run when calling 'stopProcess'.
--
-- @since 0.2.10.0
mkPipeStreamSpec :: (ProcessConfig () () () -> Handle -> IO (a, IO ()))
                 -- ^
                 -> StreamSpec streamType a
                 -- ^
mkPipeStreamSpec f = mkStreamSpec P.CreatePipe $ \pc mh ->
    case mh of
        Just h -> f pc h
        Nothing -> error "Invariant violation: making StreamSpec with CreatePipe unexpectedly did not return a Handle"

-- | Create a new 'StreamSpec' from a function that accepts a
-- 'P.StdStream' and a helper function.  This function is the same as
-- the helper in 'mkStreamSpec'
mkManagedStreamSpec :: (forall b. (P.StdStream -> IO b) -> IO b)
                    -- ^
                    -> (ProcessConfig () () () -> Maybe Handle -> IO (a, IO ()))
                    -- ^
                    -> StreamSpec streamType a
mkManagedStreamSpec ss f = StreamSpec ss (\pc mh -> Cleanup (f pc mh))

-- | A stream spec which simply inherits the stream of the parent
-- process.
--
-- @since 0.1.0.0
inherit :: StreamSpec anyStreamType ()
inherit = mkStreamSpec P.Inherit (\_ _ -> pure ((), return ()))

-- | A stream spec which is empty when used for for input and discards
-- output.  Note this requires your platform's null device to be
-- available when the process is started.
--
-- @since 0.2.5.0
nullStream :: StreamSpec anyStreamType ()
nullStream = mkManagedStreamSpec opener cleanup
  where
    opener f =
      withBinaryFile nullDevice ReadWriteMode $ \handle ->
        f (P.UseHandle handle)
    cleanup _ _ =
      pure ((), return ())

-- | A stream spec which will close the stream for the child process.
-- You usually do not want to use this, as it will leave the
-- corresponding file descriptor unassigned and hence available for
-- re-use in the child process.  Prefer 'nullStream' unless you're
-- certain you want this behavior.
--
-- @since 0.1.0.0
closed :: StreamSpec anyStreamType ()
#if MIN_VERSION_process(1, 4, 0)
closed = mkStreamSpec P.NoStream (\_ _ -> pure ((), return ()))
#else
closed = mkPipeStreamSpec (\_ h -> ((), return ()) <$ hClose h)
#endif

-- | An input stream spec which sets the input to the given
-- 'L.ByteString'. A separate thread will be forked to write the
-- contents to the child process.
--
-- @since 0.1.0.0
byteStringInput :: L.ByteString -> StreamSpec 'STInput ()
byteStringInput lbs = mkPipeStreamSpec $ \_ h -> do
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
byteStringOutput = mkPipeStreamSpec $ \pc h -> byteStringFromHandle pc h

-- | Helper function (not exposed) for both 'byteStringOutput' and
-- 'withProcessInterleave'. This will consume all of the output from
-- the given 'Handle' in a separate thread and provide access to the
-- resulting 'L.ByteString' via STM. Second action will close the
-- reader handle.
byteStringFromHandle
  :: ProcessConfig () () ()
  -> Handle -- ^ reader handle
  -> IO (STM L.ByteString, IO ())
byteStringFromHandle pc h = do
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
createPipe = mkPipeStreamSpec $ \_ h -> return (h, hClose h)

-- | Use the provided 'Handle' for the child process, and when the
-- process exits, do /not/ close it. This is useful if, for example,
-- you want to have multiple processes write to the same log file
-- sequentially.
--
-- @since 0.1.0.0
useHandleOpen :: Handle -> StreamSpec anyStreamType ()
useHandleOpen h = mkStreamSpec (P.UseHandle h) $ \_ _ -> return ((), return ())

-- | Use the provided 'Handle' for the child process, and when the
-- process exits, close it. If you have no reason to keep the 'Handle'
-- open, you should use this over 'useHandleOpen'.
--
-- @since 0.1.0.0
useHandleClose :: Handle -> StreamSpec anyStreamType ()
useHandleClose h = mkStreamSpec (P.UseHandle h) $ \_ _ -> return ((), hClose h)

-- | Launch a process based on the given 'ProcessConfig'. You should
-- ensure that you call 'stopProcess' on the result. It's usually
-- better to use one of the functions in this module which ensures
-- 'stopProcess' is called, such as 'withProcessWait'.
--
-- @since 0.1.0.0
startProcess :: MonadIO m
             => ProcessConfig stdin stdout stderr
             -- ^
             -> m (Process stdin stdout stderr)
startProcess pConfig'@ProcessConfig {..} = liftIO $ do
    ssStream pcStdin $ \realStdin ->
      ssStream pcStdout $ \realStdout ->
        ssStream pcStderr $ \realStderr -> do

          let cp0 =
                  case pcCmdSpec of
                      P.ShellCommand cmd -> P.shell cmd
                      P.RawCommand cmd args -> P.proc cmd args
              cp = cp0
                  { P.std_in = realStdin
                  , P.std_out = realStdout
                  , P.std_err = realStderr
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
          waitingThread <- asyncWithUnmask $ \unmask -> do
              ec <- unmask $ -- make sure the masking state from a bracket isn't inherited
                if multiThreadedRuntime
                  then P.waitForProcess pHandle
                  else do
                    switchTime <- fromIntegral . (`div` 1000) . ctxtSwitchTime
                              <$> getConcFlags
                    let minDelay = 1
                        maxDelay = max minDelay switchTime
                        loop delay = do
                          threadDelay delay
                          mec <- P.getProcessExitCode pHandle
                          case mec of
                            Nothing -> loop $ min maxDelay (delay * 2)
                            Just ec -> pure ec
                    loop minDelay
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
                          eres <- try $ P.terminateProcess pHandle
                          ec <-
                            case eres of
                              Left e
                                -- On Windows, with the single-threaded runtime, it
                                -- seems that if a process has already exited, the
                                -- call to terminateProcess will fail with a
                                -- permission denied error. To work around this, we
                                -- catch this exception and then immediately
                                -- waitForProcess. There's a chance that there may be
                                -- other reasons for this permission error to appear,
                                -- in which case this code may allow us to wait too
                                -- long for a child process instead of erroring out.
                                -- Recommendation: always use the multi-threaded
                                -- runtime!
                                | isPermissionError e && not multiThreadedRuntime && isWindows ->
                                  P.waitForProcess pHandle
                                | otherwise -> throwIO e
                              Right () -> P.waitForProcess pHandle
                          success <- atomically $ tryPutTMVar pExitCode ec
                          evaluate $ assert success ()

          return Process {..}
  where
    pConfig = clearStreams pConfig'

foreign import ccall unsafe "rtsSupportsBoundThreads"
  multiThreadedRuntime :: Bool

isWindows :: Bool
#if WINDOWS
isWindows = True
#else
isWindows = False
#endif

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
-- This function is usually /not/ what you want. You're likely better
-- off using 'withProcessWait'. See
-- <https://github.com/fpco/typed-process/issues/25>.
--
-- @since 0.2.5.0
withProcessTerm :: (MonadUnliftIO m)
  => ProcessConfig stdin stdout stderr
  -- ^
  -> (Process stdin stdout stderr -> m a)
  -- ^
  -> m a
withProcessTerm config = bracket (startProcess config) stopProcess

-- | Uses the bracket pattern to call 'startProcess'. Unlike
-- 'withProcessTerm', this function will wait for the child process to
-- exit, and only kill it with 'stopProcess' in the event that the
-- inner function throws an exception.
--
-- To interact with a @Process@ use the functions from the section
-- [Interact with a process](#interactwithaprocess).
--
-- @since 0.2.5.0
withProcessWait :: (MonadUnliftIO m)
  => ProcessConfig stdin stdout stderr
  -- ^
  -> (Process stdin stdout stderr -> m a)
  -- ^
  -> m a
withProcessWait config f =
  bracket
    (startProcess config)
    stopProcess
    (\p -> f p <* waitExitCode p)

-- | Deprecated synonym for 'withProcessTerm'.
--
-- @since 0.1.0.0
withProcess :: (MonadUnliftIO m)
  => ProcessConfig stdin stdout stderr
  -> (Process stdin stdout stderr -> m a)
  -> m a
withProcess = withProcessTerm
{-# DEPRECATED withProcess "Please consider using withProcessWait, or instead use withProcessTerm" #-}

-- | Same as 'withProcessTerm', but also calls 'checkExitCode'
--
-- To interact with a @Process@ use the functions from the section
-- [Interact with a process](#interactwithaprocess).
--
-- @since 0.2.5.0
withProcessTerm_ :: (MonadUnliftIO m)
  => ProcessConfig stdin stdout stderr
  -- ^
  -> (Process stdin stdout stderr -> m a)
  -- ^
  -> m a
withProcessTerm_ config = bracket
    (startProcess config)
    (\p -> stopProcess p `finally` checkExitCode p)

-- | Same as 'withProcessWait', but also calls 'checkExitCode'
--
-- @since 0.2.5.0
withProcessWait_ :: (MonadUnliftIO m)
  => ProcessConfig stdin stdout stderr
  -- ^
  -> (Process stdin stdout stderr -> m a)
  -- ^
  -> m a
withProcessWait_ config f = bracket
    (startProcess config)
    stopProcess
    (\p -> f p <* checkExitCode p)

-- | Deprecated synonym for 'withProcessTerm_'.
--
-- @since 0.1.0.0
withProcess_ :: (MonadUnliftIO m)
  => ProcessConfig stdin stdout stderr
  -> (Process stdin stdout stderr -> m a)
  -> m a
withProcess_ = withProcessTerm_
{-# DEPRECATED withProcess_ "Please consider using withProcessWait_, or instead use withProcessTerm_" #-}

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
            -- ^
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
-- Exceptions thrown by this function will include stdout and stderr.
--
-- @since 0.1.0.0
readProcess_ :: MonadIO m
             => ProcessConfig stdin stdoutIgnored stderrIgnored
             -- ^
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
  -- ^
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
-- Exceptions thrown by this function will include stdout.
--
-- @since 0.2.1.0
readProcessStdout_
  :: MonadIO m
  => ProcessConfig stdin stdoutIgnored stderr
  -- ^
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

-- | Same as 'readProcess', but only read the stderr of the process.
-- Original settings for stdout remain.
--
-- @since 0.2.1.0
readProcessStderr
  :: MonadIO m
  => ProcessConfig stdin stdout stderrIgnored
  -- ^
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
-- Exceptions thrown by this function will include stderr.
--
-- @since 0.2.1.0
readProcessStderr_
  :: MonadIO m
  => ProcessConfig stdin stdout stderrIgnored
  -- ^
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

withProcessInterleave :: (MonadUnliftIO m)
  => ProcessConfig stdin stdoutIgnored stderrIgnored
  -- ^
  -> (Process stdin (STM L.ByteString) () -> m a)
  -- ^
  -> m a
withProcessInterleave pc inner =
    -- Create a pipe to be shared for both stdout and stderr
    bracket P.createPipe (\(r, w) -> hClose r >> hClose w) $ \(readEnd, writeEnd) -> do
        -- Use the writer end of the pipe for both stdout and stderr. For
        -- the stdout half, use byteStringFromHandle to read the data into
        -- a lazy ByteString in memory.
        let pc' = setStdout (mkStreamSpec (P.UseHandle writeEnd) (\pc'' _ -> byteStringFromHandle pc'' readEnd))
                $ setStderr (useHandleOpen writeEnd)
                  pc
        withProcess pc' $ \p -> do
          -- Now that the process is forked, close the writer end of this
          -- pipe, otherwise the reader end will never give an EOF.
          liftIO $ hClose writeEnd
          inner p

-- | Same as 'readProcess', but interleaves stderr with stdout.
--
-- Motivation: Use this function if you need stdout interleaved with stderr
-- output (e.g. from an HTTP server) in order to debug failures.
--
-- @since 0.2.4.0
readProcessInterleaved
  :: MonadIO m
  => ProcessConfig stdin stdoutIgnored stderrIgnored
  -- ^
  -> m (ExitCode, L.ByteString)
readProcessInterleaved pc =
    liftIO $
    withProcessInterleave pc $ \p ->
    atomically $ (,)
      <$> waitExitCodeSTM p
      <*> getStdout p

-- | Same as 'readProcessInterleaved', but instead of returning the 'ExitCode',
-- checks it with 'checkExitCode'.
--
-- Exceptions thrown by this function will include stdout.
--
-- @since 0.2.4.0
readProcessInterleaved_
  :: MonadIO m
  => ProcessConfig stdin stdoutIgnored stderrIgnored
  -- ^
  -> m L.ByteString
  -- ^
readProcessInterleaved_ pc =
    liftIO $
    withProcessInterleave pc $ \p -> atomically $ do
      stdout' <- getStdout p
      checkExitCodeSTM p `catchSTM` \ece -> throwSTM ece
        { eceStdout = stdout'
        }
      return stdout'

-- | Run the given process, wait for it to exit, and returns its
-- 'ExitCode'.
--
-- @since 0.1.0.0
runProcess :: MonadIO m
           => ProcessConfig stdin stdout stderr
           -- ^
           -> m ExitCode
           -- ^
runProcess pc = liftIO $ withProcess pc waitExitCode

-- | Same as 'runProcess', but instead of returning the
-- 'ExitCode', checks it with 'checkExitCode'.
--
-- @since 0.1.0.0
runProcess_ :: MonadIO m
            => ProcessConfig stdin stdout stderr
            -- ^
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
-- Exceptions thrown by this function will not include stdout or stderr (This prevents unbounded memory usage from reading them into memory).
-- However, some callers such as 'readProcess_' catch the exception, add the stdout and stderr, and rethrow.
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
-- Note that several functions that throw an 'ExitCodeException' intentionally do not populate 'eceStdout' or 'eceStderr'.
-- This prevents unbounded memory usage for large stdout and stderrs.
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

bracket :: MonadUnliftIO m => IO a -> (a -> IO b) -> (a -> m c) -> m c
bracket before after thing = withRunInIO $ \run -> E.bracket before after (run . thing)

finally :: MonadUnliftIO m => m a -> IO () -> m a
finally thing after = withRunInIO $ \run -> E.finally (run thing) after
