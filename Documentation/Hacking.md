Hacking on Zag Smalltalk has 3 components:
- Smalltalk code
- Zig code
- documentation

For any case you are welcome to clone the GitHub repository and submit pull requests.

We use [Obsidian](https://obsidian.md/) to edit the documentation files. It lets you see the formatted result as you type, including tables and code blocks.

Note that this is very much a work in progress at the moment. In particular a given commit may not even compile. Generally these will say 'wip' (work in progress) in the commit comment. Further, zig is also in active development, so a given version of zig might have changed something that makes our zig code not compile!

We will shortly be moving development to a separate branch so that (barring zig changes) the main branch should at least run.

## Zig

### Get Zig
Download a recent Zig directory/folder from [ziglang.org](https://ziglang.org/download/)
Untar or unzip in a convenient place and add the resulting folder to your path.

### Compiling and Running
Here are some common command lines I use.
In the `Zag-Smalltalk/zig` directory:
```
zig build-exe -OReleaseFast -fomit-frame-pointer -freference-trace fibonacci.zig
zig test -freference-trace --main-pkg-path ~/git/Zag-Smalltalk/zig fibonacci.zig
```

In the `Zag-Smalltalk/zig/zag` directory:
```
zig test -freference-trace primitives.zig
```
Edit the code in an editor of your choice.

## Smalltalk

Download a recent Pharo image (currently using Pharo 12).
Import the Smalltalk code into Pharo using Iceberg.

(The following doesn't work yet!)
Generate the zig code for a class that has a start method with:
```smalltalk
theClass exportZig: 'filename.zig' startingWith: #startMethod
```
Then copy the exported file into the `Zag-Smalltalk/zig` directory and
```
zig run filename.zig
```

#### Debugging
Normally threaded code uses tail-calls to go from word to word. But this means that tracebacks when something goes wrong are of limited value.

The file [`config.zig`](../zig/zag/config.zig) contains debugging parameters. Setting `debugging` to true will turn tail-calls into normal calls which will give reasonable tracebacks, as well as enabling `trace` calls to actually log traces (each of these can also be turned on individually).

You need to disable `debugging` for full valid program execution, because any real program will blow the stack with tail-call turned off.

Many functions are `inline` for performance reasons. When debugging, this can obscure tracebacks, so it may be useful to remove the `inline`. When you do this, please add the string ` // INLINE` to the end of the line so it can be found easily later. So, for example:
```
inline fn foo(self: *Self) void {
```
would become:
```
fn foo(self: *Self) void { // INLINE
```
