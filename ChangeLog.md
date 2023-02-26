# ChangeLog for typed-process

## 0.2.11.0

* Expose more from `System.Process.Typed.Internal`

## 0.2.10.0

* Add `mkPipeStreamSpec`

## 0.2.9.0

* Re-export `StdStream`

## 0.2.8.0

* Re-export `ExitCode`, `ExitSuccess` and `ExitFailure`.

## 0.2.7.0

* Include empty argument in the show instance.

## 0.2.6.3

* Doc improvements

## 0.2.6.2

* Doc improvements

## 0.2.6.1

* Doc improvements

## 0.2.6.0

* The cleanup thread applies an `unmask` to the actions which wait for a
  process to exit, allowing the action to be interruptible.

## 0.2.5.0

* Add a `nullStream` [#24](https://github.com/fpco/typed-process/pull/24)
* Add `withProcessWait`, `withProcessWait_`, `withProcessTerm`, and `withProcessTerm_`
  [#25](https://github.com/fpco/typed-process/issues/25)

## 0.2.4.1

* Fix a `Handle` leak in `withProcessInterleave` and its derivatives.

## 0.2.4.0

* Add `readProcessInterleaved` and `readProcessInterleaved_` to support
  capturing output from stdout and stderr in a single ByteString value.

## 0.2.3.0

* Add support for the single-threaded runtime via polling

## 0.2.2.0

* Add inherit versions of setter functions

## 0.2.1.0

* Add `readProcessStdout`, `readProcessStdout_`, `readProcessStderr`, and `readProcessStderr_`
* Do not show modified environment information in exceptions

## 0.2.0.0

* Remove dependency on `conduit` and `conduit-extra`. Relevant code added to
  `Data.Conduit.Process.Typed` in `conduit-extra-1.2.1`.

## 0.1.1

* Introduce 'unsafeProcessHandle' function

## 0.1.0.1

* Fix bug in `waitForProcess` that caused exit code to be lost
* Minor doc improvements

## 0.1.0.0

* Initial commit
