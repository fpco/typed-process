{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Process.TypedSpec (spec) where

import System.Process.Typed
import System.Process.Typed.Internal
import System.IO
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (atomically)
import Test.Hspec
import System.Exit
import System.IO.Temp
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.String (IsString(..))
import Data.Monoid ((<>))
import qualified Data.ByteString.Base64 as B64

#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative ((*>))
#endif

spec :: Spec
spec = do
    -- This is mainly to make sure we use the right device filename on Windows
    it "Null device is accessible" $ do
        withBinaryFile nullDevice WriteMode $ \fp -> do
          hPutStrLn fp "Hello world"
        withBinaryFile nullDevice ReadMode $ \fp -> do
          atEnd <- hIsEOF fp
          atEnd `shouldBe` True

    it "bytestring stdin" $ do
        let bs :: IsString s => s
            bs = "this is a test"
        res <- readProcess (setStdin bs "cat")
        res `shouldBe` (ExitSuccess, bs, "")

    it "null stdin" $ do
        res <- readProcess (setStdin nullStream "cat")
        res `shouldBe` (ExitSuccess, "", "")

    it "null stdout" $ do
        -- In particular, writing to that doesn't terminate the process with an error
        bs <- readProcessStderr_ $ setStdout nullStream $ setStdin nullStream $
          proc "sh" ["-c", "echo hello; echo world >&2"]
        bs `shouldBe` "world\n"

    it "null stderr" $ do
        -- In particular, writing to that doesn't terminate the process with an error
        bs <- readProcessStdout_ $ setStderr nullStream $ setStdin nullStream $
          proc "sh" ["-c", "echo hello >&2; echo world"]
        bs `shouldBe` "world\n"

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
        lbs <- withProcessWait (setStdin createPipe $ setStdout byteStringOutput "base64") $ \p ->
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

    describe "withProcessWait" $
        it "succeeds with sleep" $ do
          p <- withProcessWait (proc "sleep" ["1"]) pure
          checkExitCode p :: IO ()

    describe "withProcessWait_" $
        it "succeeds with sleep"
           ((withProcessWait_ (proc "sleep" ["1"]) $ const $ pure ()) :: IO ())

    -- These tests fail on older GHCs/process package versions
    -- because, apparently, waitForProcess isn't interruptible. See
    -- https://github.com/fpco/typed-process/pull/26#issuecomment-505702573.

    {-
    describe "withProcessTerm" $ do
        it "fails with sleep" $ do
          p <- withProcessTerm (proc "sleep" ["1"]) pure
          checkExitCode p `shouldThrow` anyException

    describe "withProcessTerm_" $ do
        it "fails with sleep" $
          withProcessTerm_ (proc "sleep" ["1"]) (const $ pure ())
          `shouldThrow` anyException
    -}

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

    it "empty param are showed" $
      let expected = "Raw command: podman exec --detach-keys \"\" ctx bash"
       in show (proc "podman" ["exec", "--detach-keys", "", "ctx", "bash"]) `shouldBe` expected

    describe "ProcessConfig" $ do
        it "Show shell-escapes arguments" $ do
            let processConfig = proc "echo" ["a", "", "\"b\"", "'c'", "\\d"]
            -- I promise this escaping behavior is correct; paste it into GHCi
            -- `putStrLn` and then paste it into `sh` to verify.
            show processConfig `shouldBe`
                "Raw command: echo a \"\" \"\\\"b\\\"\" \"'c'\" \"\\\\d\""

        it "Show displays working directory" $ do
            let processConfig = setWorkingDir "puppy/doggy" $ proc "true" []
            show processConfig `shouldBe`
                "Raw command: true\n"
                ++ "Run from: puppy/doggy"

        it "Show displays environment (1 variable)" $ do
            let processConfig = setEnv [("PUPPY", "DOGGY")] $ proc "true" []
            show processConfig `shouldBe`
                "Raw command: true\n"
                ++ "Environment:\n"
                ++ "PUPPY=DOGGY"

        it "Show displays environment (multiple variables)" $ do
            let processConfig =
                    setEnv [ ("PUPPY", "DOGGY")
                           , ("SOUND", "AWOO")
                           , ("HOWLING", "RIGHT_NOW")
                           ]
                    $ proc "true" []
            show processConfig `shouldBe`
                "Raw command: true\n"
                ++ "Environment:\n"
                ++ "PUPPY=DOGGY\n"
                ++ "SOUND=AWOO\n"
                ++ "HOWLING=RIGHT_NOW"

        it "Show displays working directory and environment" $ do
            let processConfig =
                    setEnv [ ("PUPPY", "DOGGY")
                           , ("SOUND", "AWOO")
                           ]
                    $ setWorkingDir "puppy/doggy"
                    $ proc "true" []
            show processConfig `shouldBe`
                "Raw command: true\n"
                ++ "Run from: puppy/doggy\n"
                ++ "Environment:\n"
                ++ "PUPPY=DOGGY\n"
                ++ "SOUND=AWOO"


    describe "ExitCodeException" $ do
        it "Show" $ do
            let exitCodeException =
                  ExitCodeException
                      { eceExitCode = ExitFailure 1
                      , eceProcessConfig = proc "cp" ["a", "b"]
                      , eceStdout = fromString "Copied OK\n"
                      , eceStderr = fromString "Uh oh!\n"
                      }
            show exitCodeException `shouldBe`
                "Received ExitFailure 1 when running\n"
                ++ "Raw command: cp a b\n"
                ++ "\n"
                ++ "Standard output:\n"
                ++ "Copied OK\n"
                ++ "\n"
                ++ "Standard error:\n"
                ++ "Uh oh!"

        it "Show only stdout" $ do
            let exitCodeException =
                  ExitCodeException
                      { eceExitCode = ExitFailure 1
                      , eceProcessConfig = proc "show-puppy" []
                      , eceStdout = fromString "No puppies found???\n"
                      , eceStderr = fromString ""
                      }
            show exitCodeException `shouldBe`
                "Received ExitFailure 1 when running\n"
                ++ "Raw command: show-puppy\n"
                ++ "\n"
                ++ "Standard output:\n"
                ++ "No puppies found???"

        it "Show only stderr" $ do
            let exitCodeException =
                  ExitCodeException
                      { eceExitCode = ExitFailure 1
                      , eceProcessConfig = proc "show-puppy" []
                      , eceStdout = fromString ""
                      , eceStderr = fromString "No puppies found???\n"
                      }
            show exitCodeException `shouldBe`
                "Received ExitFailure 1 when running\n"
                ++ "Raw command: show-puppy\n"
                ++ "\n"
                ++ "Standard error:\n"
                ++ "No puppies found???"

        it "Show trims stdout/stderr" $ do
            -- This keeps the `Show` output looking nice regardless of how many
            -- newlines (if any) the command outputs.
            --
            -- This also makes sure that the `Show` output doesn't end with a
            -- spurious trailing newline, making it easier to compose `Show`
            -- instances together.
            let exitCodeException =
                  ExitCodeException
                      { eceExitCode = ExitFailure 1
                      , eceProcessConfig = proc "detect-doggies" []
                      , eceStdout = fromString "\n\npuppy\n\n \n"
                      , eceStderr = fromString "\t \ndoggy\n \t\n"
                      }
            show exitCodeException `shouldBe`
                "Received ExitFailure 1 when running\n"
                ++ "Raw command: detect-doggies\n"
                ++ "\n"
                ++ "Standard output:\n"
                ++ "puppy\n"
                ++ "\n"
                ++ "Standard error:\n"
                ++ "doggy"

        it "Show displays correctly with no newlines in stdout" $ do
            -- Sometimes, commands don't output _any_ newlines!
            let exitCodeException =
                  ExitCodeException
                      { eceExitCode = ExitFailure 1
                      , eceProcessConfig = proc "detect-doggies" []
                      , eceStdout = fromString "puppy"
                      , eceStderr = fromString ""
                      }
            show exitCodeException `shouldBe`
                "Received ExitFailure 1 when running\n"
                ++ "Raw command: detect-doggies\n"
                ++ "\n"
                ++ "Standard output:\n"
                ++ "puppy"
