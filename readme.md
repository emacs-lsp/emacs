# Emacs for lsp-mode(POC)

A Emacs fork implementing non-blocking and async `JSONRPC` support

## Motivation

The fork aims to fix the strugle of `lsp-mode` with sync `Emacs` core and json handling

## How it works?

The fork uses separate emacs threads to run the processing and then runs `release_global_lock()` from the C code when the processing does not involve `lisp` objects. There are a lot of benefits from this approach:

* *UI does not block the server.* Imagine that currently you have font lock running. In stock `emacs` that will prevent reading the process intput even if the server has alredy sent the response to the client. Note that this will improve not only emacs performance but it will improve the performance of single threaded server because the server won't be blocked to wait for IO to be read by the client.

* *Server does not block the UI.* Similarly, the processing of `lsp-mode` serialization of requests/deserialization of responses does block UI processing. Only small portion of whole `JSONRPC`

* *Server being slow reading requests does not block the UI* . In stock `emacs` sending requests to the server will force `emacs` to wait for server to read the request.
* *Less garbage*. Since a lot of the processing does not involve lisp objects we generate less garbage and as a result the GC runs less often.

## Current state

The code runs fine with most of the servers I have tested with. Only Linux/Unix is supported for now.

## How to use?

Compile `emacs` just like normal `emacs` and then use the latest version of `lsp-mode`.

# Acknowledgments

Thanks to [606u](https://github.com/606u) for helping me out with low
level process communication code
