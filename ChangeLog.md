# ChangeLog for typed-process

## 0.2.5.0

* When pipes are created for communications with a child process, they
  are switched to `NoBuffering` mode to avoid inadvertent
  blocking. See
  [snoyberg/conduit#402](https://github.com/snoyberg/conduit/issues/402).

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
