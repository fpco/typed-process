{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Process.TypedSpec (spec) where

import System.Process.Typed
import System.IO
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Network.HTTP.Simple
import Control.Concurrent.Async (Concurrently (..))
import Test.Hspec
import System.Exit
import System.IO.Temp
import qualified Data.ByteString as S
import Data.String (IsString)
import Data.Monoid ((<>))
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Base64 as B64

#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative ((*>))
#endif

spec :: Spec
spec = do
    it "bytestring stdin" $ do
        let bs :: IsString s => s
            bs = "this is a test"
        res <- readProcess (setStdin bs "cat")
        res `shouldBe` (ExitSuccess, bs, "")

    it "useHandleOpen" $ withSystemTempFile "use-handle-open" $ \fp h -> do
        let bs :: IsString s => s
            bs = "this is a test 2"
        S.hPut h bs
        hClose h
        res <- withBinaryFile fp ReadMode $ \h' -> do
            res <- readProcess (setStdin (useHandleOpen h') "cat")
            isOpen <- hIsOpen h'
            isOpen `shouldBe` True
            return res
        res `shouldBe` (ExitSuccess, bs, "")

    it "useHandleClose" $ withSystemTempFile "use-handle-close" $ \fp h -> do
        let bs :: IsString s => s
            bs = "this is a test 3"
        S.hPut h bs
        hClose h
        res <- withBinaryFile fp ReadMode $ \h' -> do
            res <- readProcess (setStdin (useHandleClose h') "cat")
            isOpen <- hIsOpen h'
            isOpen `shouldBe` False
            return res
        res `shouldBe` (ExitSuccess, bs, "")

    it "useHandleOpen+Close" $ withSystemTempFile "use-handle-open-close" $ \fp h -> do
        let bs1, bs2 :: IsString s => s
            bs1 = "this is a test 4\n"
            bs2 = "this is a test 5\n"

        runProcess_
            ( setStdout (useHandleOpen h)
            $ setStdin bs1 "cat")
        runProcess_
            ( setStdout (useHandleClose h)
            $ setStdin bs2 "cat")

        res <- S.readFile fp
        res `shouldBe` bs1 <> bs2

    it "unchecked exit code" $ do
        res <- runProcess "false"
        res `shouldBe` ExitFailure 1

    it "checked exit code" $
        runProcess_ "false" `shouldThrow` \ExitCodeException{} -> True

    it "async" $ withSystemTempFile "httpbin" $ \fp h -> do
        bss <- withProcess (setStdin createSink $ setStdout createSource "base64") $ \p ->
            runConcurrently $
                Concurrently
                    ( httpSink "https://raw.githubusercontent.com/fpco/typed-process/master/README.md" $ \_res ->
                    CB.conduitHandle h .| getStdin p) *>
                Concurrently
                    ( runConduit
                    $ getStdout p
                   .| CL.consume)
        hClose h
        let encoded = S.filter (/= 10) $ S.concat bss
        raw <- S.readFile fp
        encoded `shouldBe` B64.encode raw
