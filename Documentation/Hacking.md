Hacking on Zag Smalltalk has 2 components:
- Smalltalk code
- Zig code

For either case you are welcome to clone the GitHub repository and submit pull requests.

## Smalltalk

Download a recent Pharo image (currently using Pharo 10).
Import the Smalltalk code into Pharo using Iceberg

## Zig

### Get Zig
Download a recent Zig directory/folder from [ziglang.org](https://ziglang.org/download/)
Untar or unzip in a convenient place and add the resulting folder to your path.

### Compiling and Running
Here are some common command lines I use:
```
zig build-exe -OReleaseFast -fomit-frame-pointer -freference-trace --main-pkg-path ~/git/Zag-Smalltalk/zig/zag fibonacci.zig
zig test -freference-trace --main-pkg-path ~/git/Zag-Smalltalk/zig/zag fibonacci.zig
zig test -freference-trace primitives.zig
```
Edit the code in an editor of your choice.

#### Debugging
Normally threaded code uses tail-calls to go from word to word. But this means that tracebacks when something goes wrong are of limited value.

The file [`config.zig`](../zig/zag/config.zig) contains debugging parameters. Setting `debugging` to true will turn tail-calls into normal calls which will give reasonable tracebacks, as well as enabling `trace` calls to actually log traces (each of these can also be turned on individually).

You need to disable `debugging` for full valid program execution, because any real program will blow the stack with tail-call turned off.
