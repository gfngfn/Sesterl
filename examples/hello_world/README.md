
```console
# Generate/update `rebar.config`
$ sesterl config ./
  output written on '/path/to/repo/examples/hello_world/rebar.config'.

# Build
$ rebar3 sesterl compile
===> Verifying dependencies...
===> Compiling Sesterl programs (command: "sesterl build ./") ...
  parsing '/path/to/repo/examples/hello_world/src/Main.sest' ...
  type checking '/path/to/repo/examples/hello_world/src/Main.sest' ...
  output written on '/path/to/repo/examples/hello_world/_generated/HelloWorld.Main.erl'.
  output written on '/path/to/repo/examples/hello_world/_generated/sesterl_internal_prim.erl'.
===> Analyzing applications...
===> Compiling hello_world

# Run
$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling hello_world
Erlang/OTP 24 [erts-12.0.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [jit] [dtrace]

Eshell V12.0.1  (abort with ^G)
1> 'HelloWorld.Main':show().
<<"Hello World!">>
ok
2>
```
