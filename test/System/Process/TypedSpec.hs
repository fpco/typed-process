{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Process.TypedSpec (spec) where

import System.Process.Typed
import System.IO
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (atomically)
import Test.Hspec
import System.Exit
import System.IO.Temp
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.String (IsString)
import Data.Monoid ((<>))
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
        lbs <- withProcess (setStdin createPipe $ setStdout byteStringOutput "base64") $ \p ->
            runConcurrently $
                Concurrently (do
                  bs <- S.readFile "README.md"
                  S.hPut h bs
                  S.hPut (getStdin p) bs
                  hClose (getStdin p)) *>
                Concurrently (atomically $ getStdout p)
        hClose h
        let encoded = S.filter (/= 10) $ L.toStrict lbs
        raw <- S.readFile fp
        encoded `shouldBe` B64.encode raw

    it "interleaved output" $ withSystemTempFile "interleaved-output" $ \fp h -> do
        S.hPut h "\necho 'stdout'\n>&2 echo 'stderr'\necho 'stdout'"
        hClose h

        let config = proc "sh" [fp]
        -- Assert, that our bash script doesn't send output only to stdout and
        -- we assume that we captured from stderr as well
        onlyErr <- readProcessStderr_ (setStdout createPipe config)
        onlyErr `shouldBe` "stderr\n"

        (res, lbs1) <- readProcessInterleaved config
        res `shouldBe` ExitSuccess
        lbs1 `shouldBe` "stdout\nstderr\nstdout\n"

        lbs2 <- readProcessInterleaved_ config
        lbs1 `shouldBe` lbs2

    it "interleaved output handles large data" $ withSystemTempFile "interleaved-output" $ \fp h -> do
        S.hPut h "\nfor i in {1..4064}; do\necho 'stdout';\n>&2 echo 'stderr';\necho 'stdout';\ndone"
        hClose h

        let config = proc "sh" [fp]
        (result, lbs1) <- readProcessInterleaved config
        result `shouldBe` ExitSuccess
        lbs2 <- readProcessInterleaved_ config
        lbs1 `shouldBe` lbs2

        let expected = "stdout\nstderr\nstdout\n"
        L.take (L.length expected) lbs1 `shouldBe` expected

    it "no buffering on handles snoyberg/conduit#402" $ do
        let pc = setStdin createPipe
               $ setStdout createPipe
               $ setStderr createPipe
               $ proc "cat" []
        withProcess pc $ \p -> do
            let test f = hGetBuffering (f p) `shouldReturn` NoBuffering
            test getStdin
            test getStdout
            test getStderr
