## typed-process

[![Tests](https://github.com/fpco/typed-process/actions/workflows/tests.yml/badge.svg)](https://github.com/fpco/typed-process/actions/workflows/tests.yml)

API level documentation (Haddocks) may be [found on
Stackage](https://www.stackage.org/package/typed-process).

This library provides the ability to launch and interact with external
processes. It wraps around the
[process library](https://hackage.haskell.org/package/process), and
intends to improve upon it by:

1. Using type variables to represent the standard streams, making them
   easier to manipulate
2. Use proper concurrency (e.g., the async library) in place of the
   weird lazy I/O tricks for such things as consuming output streams
3. Allow for more complex concurrency by providing STM-based functions
4. Using binary I/O correctly
5. Providing a more composable API, designed to be easy to use for
   both simple and complex use cases

__NOTE__ It's highly recommended that you compile any program using this
library with the multi-threaded runtime, usually by adding `ghc-options:
-threaded` to your executable stanza in your cabal or `package.yaml` file. The
single-threaded runtime necessitates some inefficient polling to be used under
the surface.

## Synopsis

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.IO (hPutStr, hClose)
import System.Process.Typed
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Concurrent.STM (atomically)
import Control.Exception (throwIO)

main :: IO ()
main = do
    -- Run a process, print its exit code
    runProcess "true" >>= print
    runProcess "false" >>= print

    -- Check that the exit code is a success
    runProcess_ "true"
    -- This will throw an exception: runProcess_ "false"

    -- Capture output and error
    (dateOut, dateErr) <- readProcess_ "date"
    print (dateOut, dateErr)

    -- Use shell commands
    (dateOut2, dateErr2) <- readProcess_ "date >&2"
    print (dateOut2, dateErr2)

    -- Interact with a process
    let catConfig = setStdin createPipe
                  $ setStdout byteStringOutput
                  $ proc "cat" ["/etc/hosts", "-", "/etc/group"]
    withProcessWait_ catConfig $ \p -> do
        hPutStr (getStdin p) "\n\nHELLO\n"
        hPutStr (getStdin p) "WORLD\n\n\n"
        hClose (getStdin p)

        atomically (getStdout p) >>= L8.putStr
```

## Types

The two primary types in this package are `ProcessConfig` and
`Process`. `ProcessConfig` gives a specification for how to run a
process (e.g., the command to run, working directory, environment
variables) and how to deal with the three standard streams: input,
output, and error. You use one of the functions in this package for
launching a process to turn a `ProcessConfig` into a `Process`, which
represents an actual running system process.

The easiest way to create a `ProcessConfig` is using the `IsString`
instance and `OverloadedStrings`. For example, to run the `date`
command, we can do the following. (NOTE: The type signatures used here
are simply to spell things out, they are not needed.)

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed

main :: IO ()
main = do
    let dateConfig :: ProcessConfig () () ()
        dateConfig = proc "date" []
        -- alternatively: `shell "date"` or just "date"

    process <- startProcess dateConfig
    exitCode <- waitExitCode (process :: Process () () ())
    print exitCode

    stopProcess process
```

This shows the general workflow: use `startProcess` to launch a
`Process` from a `ProcessConfig`, interact with it (such as
`waitExitCode` to wait for the process to exit), and then clean up
resources with `stopProcess`. (We'll get to those `() () ()` type
parameters in the next section.)

Instead of explicitly dealing with `startProcess` and `stopProcess`,
it's recommended to instead use `withProcessWait`, which uses the bracket
pattern and is exception safe:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed

main :: IO ()
main = withProcessWait "date" $ \process -> do
    exitCode <- waitExitCode (process :: Process () () ())
    print exitCode
```

But this pattern of running a process, waiting for it to exit, and
getting its exit code is very common, so it has a helper function of
its own:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed

main :: IO ()
main = do
    exitCode <- runProcess "date"
    print exitCode
```

We'll discuss some functions which automatically check the exit code
below.

## Type parameters

Both `ProcessConfig` and `Process` take three type parameters:
the types of the standard input, output, and error streams for the
process. As you saw above, our default is `()` for each, and our
default behavior is to inherit the streams from the parent
process. This is why, when you run the previous programs, the `date`
program's output goes directly to your console.

We can override these defaults in a number of ways. Perhaps the
easiest is to simply close the stream for the child so it cannot use
it at all.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed

main :: IO ()
main = do
    let dateConfig :: ProcessConfig () () ()
        dateConfig = setStdin closed
                   $ setStdout closed
                   $ setStderr closed
                     "date"
    exitCode <- runProcess dateConfig
    print exitCode
```

A few things to note:

* The type parameter is still `()`, since there's no data to
  return. We'll see some more interesting cases later.
* This process now returns an `ExitFailure 1`, since it tries to write
  to a closed `stdout` file descriptor.

## Using `proc` and `shell`

Using the `OverloadedStrings` approach works nicely for some cases,
but we'll often want more control over things. There are two smart
constructors available: `proc` takes a command and list of arguments,
and `shell` takes a single string which will be passed directly to the
system's shell.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed

main :: IO ()
main = do
    -- Command and arguments
    runProcess (proc "cat" ["/etc/hosts"]) >>= print

    -- Shell
    runProcess (shell "cat /etc/hosts >&2 && false") >>= print
```

The behavior of the `OverloadedStrings` approach we've used until now
is actually based on these two smart constructors. If you provide it a
string without any spaces (like `"date"`), it will use `proc` without
any arguments, e.g. `fromString "date" = proc "date" []`. If there are
any spaces in the string, it will use `shell`.

__EXERCISE__: Rewrite the previous example to not use the `shell`
constructor.

## Checking the exit code

We've done a lot of printing of exit codes. In many cases, we don't
actually want to look at the exit code, but instead just throw an
exception if the process failed. Fortunately, we have such an
exit-code-checking function.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed

main :: IO ()
main = runProcess_ "date"
```

By adding the `_` at the end of `runProcess`, we're now automatically
checking the exit code and throwing an exception if it returns
anything but success. Want to see it in action?

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed

main :: IO ()
main = runProcess_ "false"
```

Under the surface, this function is using the `checkExitCode`
function. We can do this more explicitly if desired:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed

main :: IO ()
main = withProcessWait "false" checkExitCode
```

## Reading from a process

Sending all output to the parent process's handles is sometimes
desired, but often we'd rather just capture that output. The easiest
way to do that is to capture it in memory as a lazy
`ByteString`. Fortunately, we have a helper `readProcess` function for
that:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed
import System.Exit (ExitCode)
import Data.ByteString.Lazy (ByteString)

main :: IO ()
main = do
    (exitCode, out, err) <- readProcess "date"
    print (exitCode :: ExitCode)
    print (out :: ByteString)
    print (err :: ByteString)
```

One thing to point out is that, even though this is a lazy
`ByteString`, it is not using any lazy I/O. When `readProcess` exits,
the output has been fully generated, and is resident in memory. We
only use a lazy `ByteString` instead of a strict one for better memory
configuration (chunking into multiple smaller bits instead of one
massive chunk of data).

Like `runProcess`, there's an exit-code-checking variant of
`readProcess`:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed
import Data.ByteString.Lazy (ByteString)

main :: IO ()
main = do
    (out, err) <- readProcess_ "date"
    print (out :: ByteString)
    print (err :: ByteString)
```

__EXERCISE__: Use shell redirection to move the output from standard
output to standard error.

## Redirecting to a file

Another technique we'll commonly want to employ is to redirect output
from a process to a file. This is superior to the memory approach as
it does not have the risk of using large amounts of memory, though it
is more inconvenient. Together with the
[`UnliftIO.Temporary`](https://www.stackage.org/haddock/lts/unliftio/UnliftIO-Temporary.html), we
can do some nice things:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed
import UnliftIO.Temporary (withSystemTempFile)

main :: IO ()
main = withSystemTempFile "date" $ \fp h -> do
    let dateConfig = setStdin closed
                   $ setStdout (useHandleClose h)
                   $ setStderr closed
                     "date"

    runProcess_ dateConfig

    readFile fp >>= print
```

The `useHandleClose` function lets us provide an already existing
`Handle`, and will close it when done. If you want to write the output
of multiple processes to a single file, you can instead use
`useHandleOpen`:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed
import System.IO (hClose)
import UnliftIO.Temporary (withSystemTempFile)
import Control.Monad (replicateM_)

main :: IO ()
main = withSystemTempFile "date" $ \fp h -> do
    let dateConfig = setStdin closed
                   $ setStdout (useHandleOpen h)
                   $ setStderr closed
                     "date"

    replicateM_ 10 $ runProcess_ dateConfig
    hClose h

    readFile fp >>= putStrLn
```

__EXERCISE__ Create a separate file for error output and capture that
as well.

## Providing input

Using `OverloadedStrings`, it's trivial to provide some input to a
process:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed

main :: IO ()
main = runProcess_ $ setStdin "Hello World!\n" "cat"
```

This is just a shortcut for using the `byteStringInput` function:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed

main :: IO ()
main = runProcess_ $ setStdin (byteStringInput "Hello World!\n") "cat"
```

But like output and error, we can also use a `Handle` or a temporary
file:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed
import System.IO
import UnliftIO.Temporary (withSystemTempFile)

main :: IO ()
main = withSystemTempFile "input" $ \fp h -> do
    hPutStrLn h "Hello World!"
    hClose h

    withBinaryFile fp ReadMode $ \h' ->
        runProcess_ $ setStdin (useHandleClose h') "cat"
```

## Interacting with a process

So far, everything we've done has been _running_ processes: spawning a
child with some settings, then waiting for it to exit. We will often
want to _interact_ with a process: spawn it, and then send it input or
receive output from it while it is still running.

For this, using `createPipe` makes a lot of sense:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed
import System.IO

main :: IO ()
main = do
    let catConfig = setStdin createPipe
                  $ setStdout createPipe
                  $ setStderr closed
                    "cat"

    withProcess_ catConfig $ \p -> do
        hPutStrLn (getStdin p) "Hello!"
        hFlush (getStdin p)
        hGetLine (getStdout p) >>= print

        hClose (getStdin p)
```

__EXERCISE__: What happens if you remove the `hClose` line, and why?
Hint: what happens if you both remove `hClose` _and_ replace
`withProcess_` with `withProcess`?

## Other settings

We've so far only played with modifying streams, but there are a
number of other settings you can tweak. It's best to just
[look at the API docs](https://www.stackage.org/package/typed-process)
for all available functions. We'll give examples of the two most
common settings: the working directory and environment variables.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-16.27 script
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed

main :: IO ()
main = do
    putStrLn "1:"
    runProcess_ "pwd"
    putStrLn "\n2:"
    runProcess_ $ setWorkingDir "/tmp" "pwd"

    putStrLn "\n3:"
    runProcess_ "env"
    putStrLn "\n4:"
    runProcess_ $ setEnv [("HELLO", "WORLD")] "env"
```

## Async and STM

When interacting with a process on multiple streams, you'll often want
to use some kind of concurrency. The strong recommendation is to use
the
[async library](https://haskell-lang.org/library/async). Additionally,
this library provides a number of functions that use STM, which also
plays very nicely with concurrency and the async package. For some
examples, check out:

* `waitExitCodeSTM`
* `getExitCodeSTM`
* `checkExitCodeSTM`
* `byteStringOutput`

__EXERCISE__ Reimplement the `readProcess` function using
`byteStringOutput` and `waitExitCodeSTM`.

__EXERCISE__ Reimplement the `readProcess_` function using
`byteStringOutput` and `checkExitCodeSTM`.
