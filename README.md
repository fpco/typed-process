## typed-process

[![Build Status](https://travis-ci.org/fpco/typed-process.svg?branch=master)](https://travis-ci.org/fpco/typed-process) [![Build status](https://ci.appveyor.com/api/projects/status/bhh7aekbgeqp7g5j/branch/master?svg=true)](https://ci.appveyor.com/project/snoyberg/typed-process/branch/master)

Alternative API for processes, featuring more type safety, easier
composability, and better streaming binary data support.

This is a proof of concept approach to a new process API. The guiding principles here are

1. Be explicit in the streams with type variables
2. Use proper concurrency in place of the weird lazy I/O tricks
3. Use STM

This API is still very much open for input, feedback very much
welcome!

Temporarily: you can check out the API docs at:
http://www.snoyman.com/static/typed-process/System-Process-Typed.html
