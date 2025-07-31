In Pharo, `zoomFactorSettingsOn:` settings: 1 1.12 1.4 1.7 2 2.5 3 3.5 4 4.5 5 

What about Lazy-Smalltalk
- mutable objects need reification queue
https://futureofcoding.org/

Debugging with lldb or gdb:
```sh
zig build
# Assuming your exe name is `zag`
lldb zig-out/bin/zag
# Or to use gdb
gdb zig-out/bin/zag
```
See [this blog](https://ziggit.dev/t/debugging-zig-with-a-debugger/7160) which [references this one](https://ziggit.dev/t/zig-debugging-with-lldb/3931)