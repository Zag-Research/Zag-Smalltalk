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
Edit the code in the 