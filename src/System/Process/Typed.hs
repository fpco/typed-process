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

import Control.Exception hiding (bracket, finally)
import Control.Monad.IO.Class
import qualified System.Process as P
import System.IO (hClose)
import System.IO.Error (isPermissionError)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (asyncWithUnmask, cancel, waitCatch)
import Control.Concurrent.STM (newEmptyTMVarIO, atomically, putTMVar, TMVar, readTMVar, tryReadTMVar, STM, tryPutTMVar, throwSTM, catchSTM)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.Process.Typed.Internal
import qualified Data.ByteString.Lazy as L
import GHC.RTS.Flags (getConcFlags, ctxtSwitchTime)
import Control.Monad.IO.Unlift

#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative (Applicative (..), (<$>), (<$))
#endif

#if !MIN_VERSION_process(1, 3, 0)
import qualified System.Process.Internals as P (createProcess_)
#endif

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
                          terminateProcess pHandle
                          ec <- P.waitForProcess pHandle
                          success <- atomically $ tryPutTMVar pExitCode ec
                          evaluate $ assert success ()

          return Process {..}
  where
    pConfig = clearStreams pConfig'

    terminateProcess pHandle = do
      eres <- try $ P.terminateProcess pHandle
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
              pure ()
            | otherwise -> throwIO e
          Right () -> pure ()

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
{-# DEPRECATED withProcess "Please consider using `withProcessWait`, or instead use `withProcessTerm`" #-}

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
{-# DEPRECATED withProcess_ "Please consider using `withProcessWait_`, or instead use `withProcessTerm_`" #-}

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
-- 'System.Process.waitForProcess' on the process handle as either
-- 'System.Process.waitForProcess' or 'stopProcess' will lock.
-- Additionally, even if process was terminated by the
-- 'System.Process.terminateProcess' or by sending signal,
-- 'stopProcess' should be called either way in order to cleanup resources
-- allocated by the @typed-process@.
--
-- @since 0.1.1
unsafeProcessHandle :: Process stdin stdout stderr -> P.ProcessHandle
unsafeProcessHandle = pHandle
