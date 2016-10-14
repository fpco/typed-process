{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Please see the README.md file for examples of using this API.
module System.Process.Typed
    ( -- * Process configuration
      ProcessConfig

      -- ** Smart constructors
    , shell
    , proc

      -- ** Setters
    , setShell
    , setRaw
    , setStdin
    , setStdout
    , setStderr
    , setCheckExitCode

      -- ** Stream specs
    , StreamSpec
    , StreamType (..)
    , byteStringInput
    , byteStringOutput
    , createPipe
    , closed
    , useHandleOpen
    , useHandleClose
    , source
    , sink

      -- * Launching a process
    , Process
    , startProcess
    , stopProcess
    , withProcess
    , readProcess
    , runProcess
    , runProcess_

      -- * Interact with a process

      -- ** Process exit code
    , waitForProcess
    , getProcessExitCode
    , waitForProcessSTM
    , getProcessExitCodeSTM

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
import Data.Streaming.Process (Inherited (..), ClosedStream (..))
import Data.Typeable (Typeable)
import System.IO (Handle, hClose)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (newEmptyTMVarIO, atomically, putTMVar, TMVar, readTMVar, tryReadTMVar, STM, tryPutTMVar)
import System.Exit (ExitCode (ExitSuccess))
import qualified Data.ByteString.Lazy as L
import Data.String (IsString (fromString))
import Data.Conduit (ConduitM)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB

data ProcessConfig stdin stdout stderr = ProcessConfig
    { pcCmdSpec :: !P.CmdSpec
    , pcStdin :: !(StreamSpec 'STInput stdin)
    , pcStdout :: !(StreamSpec 'STOutput stdout)
    , pcStderr :: !(StreamSpec 'STOutput stderr)
    , pcCheckExitCode :: !Bool
    }

data StreamType = STInput | STOutput

data StreamSpec (streamType :: StreamType) a = StreamSpec
    { ssStream :: !P.StdStream
    , ssCreate :: !(Maybe Handle -> Cleanup a)
    }
    deriving Functor

newtype Cleanup a = Cleanup { runCleanup :: IO (a, IO ()) }
    deriving Functor
instance Applicative Cleanup where
    pure x = Cleanup (return (x, return ()))
    Cleanup f <*> Cleanup x = Cleanup $ do
        (f', c1) <- f
        (`onException` c1) $ do
            (x', c2) <- x
            return (f' x', c1 `finally` c2)

setStdin :: StreamSpec 'STInput stdin
         -> ProcessConfig stdin0 stdout stderr
         -> ProcessConfig stdin stdout stderr
setStdin spec pc = pc { pcStdin = spec }

setStdout :: StreamSpec 'STOutput stdout
          -> ProcessConfig stdin stdout0 stderr
          -> ProcessConfig stdin stdout stderr
setStdout spec pc = pc { pcStdout = spec }

setStderr :: StreamSpec 'STOutput stderr
          -> ProcessConfig stdin stdout stderr0
          -> ProcessConfig stdin stdout stderr
setStderr spec pc = pc { pcStderr = spec }

data Process stdin stdout stderr = Process
    { pCleanup :: !(IO ())
    , pStdin :: !stdin
    , pStdout :: !stdout
    , pStderr :: !stderr
    , pHandle :: !P.ProcessHandle
    , pExitCode :: !(TMVar ExitCode)
    }

defaultProcessConfig :: ProcessConfig Inherited Inherited Inherited
defaultProcessConfig = ProcessConfig
    { pcCmdSpec = P.ShellCommand ""
    , pcStdin = inherit
    , pcStdout = inherit
    , pcStderr = inherit
    , pcCheckExitCode = False
    }

setCheckExitCode :: Bool
                 -> ProcessConfig stdin stdout stderr
                 -> ProcessConfig stdin stdout stderr
setCheckExitCode x p = p { pcCheckExitCode = x }

shell :: String -> ProcessConfig Inherited Inherited Inherited
shell cmd = setShell cmd defaultProcessConfig

proc :: FilePath -> [String] -> ProcessConfig Inherited Inherited Inherited
proc cmd args = setRaw cmd args defaultProcessConfig

instance (stdin ~ Inherited, stdout ~ Inherited, stderr ~ Inherited)
  => IsString (ProcessConfig stdin stdout stderr) where
    fromString s
        | any (== ' ') s = shell s
        | otherwise = proc s []

setShell :: String
         -> ProcessConfig stdin stdout stderr
         -> ProcessConfig stdin stdout stderr
setShell cmd p = p { pcCmdSpec = P.ShellCommand cmd }

setRaw :: FilePath -> [String]
       -> ProcessConfig stdin stdout stderr
       -> ProcessConfig stdin stdout stderr
setRaw cmd args p = p { pcCmdSpec = P.RawCommand cmd args }

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

    let pCleanup2 = pCleanup1 `finally` P.terminateProcess pHandle
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

data ExitCodeException = ExitCodeException ExitCode (Maybe SomeException)
    deriving (Show, Typeable)
instance Exception ExitCodeException

stopProcess :: MonadIO m
            => Process stdin stdout stderr
            -> m ()
stopProcess = liftIO . pCleanup

withProcess :: (MonadIO m, C.MonadMask m)
            => ProcessConfig stdin stdout stderr
            -> (Process stdin stdout stderr -> m a)
            -> m a
withProcess config = C.bracket (startProcess config) stopProcess

waitForProcess :: MonadIO m => Process stdin stdout stderr -> m ExitCode
waitForProcess = liftIO . atomically . waitForProcessSTM

waitForProcessSTM :: Process stdin stdout stderr -> STM ExitCode
waitForProcessSTM = readTMVar . pExitCode

getProcessExitCode :: MonadIO m => Process stdin stdout stderr -> m (Maybe ExitCode)
getProcessExitCode = liftIO . atomically . getProcessExitCodeSTM

getProcessExitCodeSTM :: Process stdin stdout stderr -> STM (Maybe ExitCode)
getProcessExitCodeSTM = tryReadTMVar . pExitCode

getStdin :: Process stdin stdout stderr -> stdin
getStdin = pStdin

getStdout :: Process stdin stdout stderr -> stdout
getStdout = pStdout

getStderr :: Process stdin stdout stderr -> stderr
getStderr = pStderr

inherit :: StreamSpec anyStreamType Inherited
inherit = StreamSpec P.Inherit (\Nothing -> pure Inherited)

#ifndef MIN_VERSION_process
#define MIN_VERSION_process(a, b, c) 0
#endif

closed :: StreamSpec anyStreamType ClosedStream
#if MIN_VERSION_process(1, 4, 0)
closed = StreamSpec P.NoStream (\Nothing -> pure ClosedStream)
#else
closed = StreamSpec P.CreatePipe (\(Just h) -> Cleanup ((ClosedStream, return ()) <$ hClose h))
#endif

byteStringInput :: L.ByteString -> StreamSpec 'STInput ()
byteStringInput lbs = StreamSpec P.CreatePipe $ \(Just h) -> Cleanup $ do
    void $ async $ do
        L.hPut h lbs
        hClose h
    return ((), hClose h)

instance (streamType ~ 'STInput, res ~ ())
  => IsString (StreamSpec streamType res) where
    fromString = byteStringInput . fromString

byteStringOutput :: StreamSpec 'STOutput (STM L.ByteString)
byteStringOutput = StreamSpec P.CreatePipe $ \(Just h) -> Cleanup $ do
    mvar <- newEmptyTMVarIO

    void $ async $ do
        let loop front = do
                bs <- S.hGetSome h defaultChunkSize
                if S.null bs
                    then atomically $ putTMVar mvar $ L.fromChunks $ front []
                    else loop $ front . (bs:)
        loop id `catch` \e -> do
            atomically $ void $ tryPutTMVar mvar $ throw $ ByteStringOutputException e
            throwIO e

    return (readTMVar mvar, hClose h)

createPipe :: StreamSpec anyStreamType Handle
createPipe = StreamSpec P.CreatePipe $ \(Just h) -> Cleanup $ return (h, hClose h)

useHandleOpen :: Handle -> StreamSpec anyStreamType ()
useHandleOpen h = StreamSpec (P.UseHandle h) $ \Nothing -> Cleanup $ return ((), return ())

useHandleClose :: Handle -> StreamSpec anyStreamType ()
useHandleClose h = StreamSpec (P.UseHandle h) $ \Nothing -> Cleanup $ return ((), hClose h)

data ByteStringOutputException = ByteStringOutputException SomeException
    deriving (Show, Typeable)
instance Exception ByteStringOutputException

readProcess :: MonadIO m
            => ProcessConfig stdin stdoutIgnored stderrIgnored
            -> m (ExitCode, L.ByteString, L.ByteString)
readProcess pc =
    liftIO $ withProcess pc' $ \p -> atomically $ (,,)
        <$> waitForProcessSTM p
        <*> getStdout p
        <*> getStderr p
  where
    pc' = setStdout byteStringOutput
        $ setStderr byteStringOutput pc

runProcess :: MonadIO m
           => ProcessConfig stdin stdout stderr
           -> m ExitCode
runProcess pc = liftIO $ withProcess pc waitForProcess

runProcess_ :: MonadIO m
            => ProcessConfig stdin stdout stderr
            -> m ()
runProcess_ = void . runProcess

source :: MonadIO m => StreamSpec 'STOutput (ConduitM i S.ByteString m ())
source =
    (\h -> C.addCleanup (\_ -> liftIO $ hClose h) (CB.sourceHandle h))
    <$> createPipe

sink :: MonadIO m => StreamSpec 'STInput (ConduitM S.ByteString o m ())
sink =
    (\h -> C.addCleanup (\_ -> liftIO $ hClose h) (CB.sinkHandle h))
    <$> createPipe
