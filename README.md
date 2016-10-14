## typed-process

Alternative API for processes, featuring more type safety, easier
composability, and better streaming binary data support.

This is a proof of concept approach to a new process API. The guiding principles here are

1. Be explicit in the streams with type variables
2. Use proper concurrency in place of the weird lazy I/O tricks
3. Use STM

This API is still very much open for input, feedback very much
welcome!
