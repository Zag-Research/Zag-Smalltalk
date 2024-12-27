## Reference

### Smalltalk

- [Blue Book](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf)
- [Jecel's list of Smalltalks](https://github.com/jeceljr/SmalltalkSurvey)
- [UTF8 strings for Pharo](https://github.com/svenvc/UTF8String)
- [point-free Smalltalk](https://smalltalkthoughts.blogspot.com/2011/08/point-free-programming-in-smalltalk.html)
- [Marcus Denker blog](https://blog.marcusdenker.de/)
- [Cog blog - build me a JIT as fast as you can](http://www.mirandabanda.org/cogblog/2011/03/01/build-me-a-jit-as-fast-as-you-can/)
- [improving SqueakJS  performance](https://squeak.js.org/docs/jit.md.html)
- [Fibonacci hashing](https://probablydance.com/2018/06/16/fibonacci-hashing-the-optimization-that-the-world-forgot-or-a-better-alternative-to-integer-modulo/)
- [building Pharo VM](https://github.com/pharo-project/pharo-vm) 
```
$ git clone git@github.com:pharo-project/pharo-vm.git
$ cmake -S pharo-vm -B build -DPHARO_DEPENDENCIES_PREFER_DOWNLOAD_BINARIES=TRUE  -DFLAVOUR=StackVM
$ cd build
$ make install
```
- [Clement Bera on Spur's object format](https://clementbera.wordpress.com/2014/01/16/spurs-new-object-format/) and [floating point](https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/)
- [Richard Eng's post on the power of symmetry](https://medium.com/smalltalk-talk/lisp-smalltalk-and-the-power-of-symmetry-8bd96aaa0c0c)
- [Alan Kay's "Early History of Smalltalk"](https://worrydream.com/EarlyHistoryOfSmalltalk/)
- [Graph Neural Networks](https://www.analyticsvidhya.com/blog/2022/03/what-are-graph-neural-networks-and-how-do-they-work/)
- [YC thread on Smalltalk bare-metal on Raspberry PI](https://news.ycombinator.com/item?id=34208753)
- [Hidden Heroes on Alan Kay and the Dynabook](https://hiddenheroes.netguru.com/alan-kay)
- [Dynabook - the laptop manufacturer (name inspired by Kay) - previously Toshiba](https://ca.dynabook.com/company/about-dynabook/) [Wikipedia](https://en.wikipedia.org/wiki/Dynabook_Inc.#The_first_Dynabook)
- [Bee Smalltalk](https://github.com/aucerna/bee-dmr) [video tour 2018](https://www.youtube.com/watch?v=WGalg2CZ_Ps)
### Programming Languages
- [Lenient evaluation is neither strict nor lazy](https://www.sciencedirect.com/science/article/abs/pii/S0096055101000066)
- [Ken Dickey's Crosstalk](https://github.com/KenDickey/Crosstalk/blob/master/DispatchNotes.md)
- [HydraVM](http://www.hydravm.org/hydra)
- [GraalVM](https://www.graalvm.org)

### Memory Management
#### mimalloc
- [on github](https://github.com/microsoft/mimalloc)
- has some interesting ideas similar to those I was thinking about
- [this post](https://github.com/microsoft/mimalloc/issues/215#issuecomment-599711867) provides some hints about their memory structure
#### Mist
- [MistDesign](MistDesign.md) also has some ideas we modify
#### Garbage Collectors
- [Deconstructing the garbage-first collector](https://dl.acm.org/doi/10.1145/3381052.3381320)
- [MMTk](https://github.com/mmtk)

### User interface
- [_Chromium_ Embedded Framework](https://github.com/chromiumembedded/cef)
## Architectures
#### HPC and architectures (cache et al)
- [Algorithmica](https://en.algorithmica.org/hpc/)
- [What Every Programmer Should Know About Memory](http://people.freebsd.org/~lstewart/articles/cpumemory.pdf)
- [Concerns of Self-Modifying-Code on ARM](https://dl.acm.org/doi/10.1145/3546568)
#### x86_64
- [x86-64 instructions](http://linasm.sourceforge.net/docs/instructions/)
- [x86-64](https://devblogs.microsoft.com/oldnewthing/20220831-00/?p=107077) [x86-64 Instructions and ABI (PDF)](https://www.classes.cs.uchicago.edu/archive/2009/spring/22620-1/docs/handout-03.pdf) [Intel syntax: Introduction to x64 Assembly (PDF)](https://www.intel.com/content/dam/develop/external/us/en/documents/introduction-to-x64-assembly-181178.pdf) 
#### ARM
- [Writing AArch64 code for Apple](https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms) and [Code in ARM Assembly (for Apple)](https://eclecticlight.co/2021/06/16/code-in-arm-assembly-registers-explained/)
- [More AArch64 coding advice](https://thinkingeek.com/2016/10/23/exploring-aarch64-assembler-chapter-3/)
- [AArch64 processor (Windows)](https://devblogs.microsoft.com/oldnewthing/20220726-00/?p=106898)
- [Computer Organization and Design (book) ARM editions](https://www.academia.edu/keypass/WEp6S1RhVXFYMjhVYUZBTkNCSzJvMnJLRzgyWFFONENMWWh0cVliVGY0OD0tLTExbEJEMUR4RXRLS3BDVDdIbVRYMXc9PQ==--6d9207bbe2f5e142ab68fe654908b0330e763f5d/t/E9U-QZniS8Z-ij15Y/resource/work/31710742/Computer_organization_and_design_arm_edition?auto=download&email_work_card=download-paper)
- [Application Binary Interface for the Arm® Architecture](https://github.com/ARM-software/abi-aa/) including [Procedure Call Standard for the Arm® 64-bit Architecture (AArch64)](https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst)
- [AArch64 encoding literals](https://dinfuehr.github.io/blog/encoding-of-immediate-values-on-aarch64/)
- [Wikipedia on ARM64 calling conventions](https://en.wikipedia.org/wiki/Calling_convention#ARM_(A64))
#### RISC-V
- [Computer Organization and Design](https://www.academia.edu/38301807/Computer_organization_and_design_RISC_V?auto=download&email_work_card=download-paper)
#### LLVM
- [LLVM on asm syntax](https://releases.llvm.org/10.0.0/docs/LangRef.html#inline-asm-constraint-string) and [GCC on extended asm](https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html)

#### Atomics and Locks
- [Rust Atomics and Locks](https://marabos.nl/atomics/) most of which applies to Zig
- [Understanding Memory Ordering in Rust](https://emschwartz.me/understanding-memory-ordering-in-rust/) ditto (refers to Chapter 3 of the above)

#### IEEE Floating point
- 64 bit floating point ![IEEE 754 Binary-64](Pasted%20image%2020210311212924.png)
- [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754)

### Zig
- [Zig reference](https://ziglang.org/documentation/master/#Assembly) and [Zig Learn](https://ziglearn.org/chapter-2/#random-numbers)
- [Zig 0.10.0 release notes](https://ziglang.org/download/0.10.0/release-notes.html#Function-Pointers)

## Miscellaneous
- [Hidden Heroes](https://hiddenheroes.netguru.com/)

### Rust
- [Learn Rust](https://www.rust-lang.org/learn)
- [Rust docs](https://doc.rust-lang.org/)
	- [Rust Reference](https://doc.rust-lang.org/reference/)
	- [Rustinomicon](https://doc.rust-lang.org/nomicon/)
	- [Rust By Example](https://doc.rust-lang.org/rust-by-example/)
	- [Rust std crate](https://doc.rust-lang.org/std/)
	- [Rust primitive pointers](https://doc.rust-lang.org/std/primitive.pointer.html) and [nightly](https://doc.rust-lang.org/nightly/std/primitive.pointer.html)
	- [Unsafe Rust](https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html)
	- [2018 edition](https://doc.rust-lang.org/edition-guide/rust-2018/index.html)
- [Playground](https://play.rust-lang.org/)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [The Little Book of Rust Macros](https://danielkeep.github.io/tlborm/book/)
- [Rust tips: Box, RC, Arc, Cell, RefCell, Mutex](https://tekshinobi.com/rust-tips-box-rc-arc-cell-refcell-mutex/)
- [mmap(2) man page](https://www.man7.org/linux/man-pages/man2/mmap.2.html)
- [24 Days of Rust - interesting libraries](http://siciarz.net/)
- [Rusty Runtimes: Building Languages In Rust](https://youtu.be/U3upi-y2pCk)
- [Dangerust](http://cliffle.com/p/dangerust/1/)
- [How to properly wrap a C function pointer in Rust?](https://stackoverflow.com/questions/60969071/how-to-properly-wrap-a-c-function-pointer-in-rust)
- [How do I convert a Rust closure to a C-style callback?](https://stackoverflow.com/questions/32270030/how-do-i-convert-a-rust-closure-to-a-c-style-callback)
- [How do Rust closures work and how does it execute a closure?](https://stackoverflow.com/questions/45935100/how-do-rust-closures-work-and-how-does-it-execute-a-closure)
- [How do I store a closure in a struct in Rust?](https://stackoverflow.com/questions/27831944/how-do-i-store-a-closure-in-a-struct-in-rust)
- [Rust Closures concept](https://stackoverflow.com/questions/65682678/rust-closures-concept)
- [Rust Global.dealloc vs ptr::drop\_in\_place vs ManuallyDrop](https://stackoverflow.com/questions/62917242/rust-global-dealloc-vs-ptrdrop-in-place-vs-manuallydrop)
- Function std::ptr::[drop\_in\_place](https://doc.rust-lang.org/std/ptr/fn.drop_in_place.html)
- Union std::mem::[MaybeUninit](https://doc.rust-lang.org/std/mem/union.MaybeUninit.html)
- [Methods for Array Initialization in Rust](https://www.joshmcguigan.com/blog/array-initialization-rust/)
- [tranmute](https://doc.rust-lang.org/nightly/std/mem/fn.transmute.html)
- [std::ptr](https://doc.rust-lang.org/nightly/std/ptr/index.html)
- [libc](https://docs.rs/libc/0.2.68/libc/)
- [labguage doc](https://doc.rust-lang.org/nightly/reference/expressions/if-expr.html)
- [conditional compilation](https://doc.rust-lang.org/reference/conditional-compilation.html)

- Build [pharo-vm](https://github.com/pharo-project/pharo-vm) command: `cmake -DFLAVOUR=StackVM -S pharo-vm -B build`
- **Building C object CMakeFiles/PharoVMCore.dir/generated/64/vm/src/gcc3x-interp.c.o**
```
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/cc -DCOGMTVM=0 -DFEATURE_FFI=1 -DFEATURE_THREADED_FFI=1 -DIMMUTABILITY=1 -DLSB_FIRST=1 -DPharoVM=1 -DPharoVMCore_EXPORTS -DREAD_ONLY_CODE_ZONE=1 -DSOURCE_PATH_SIZE=41 -DUSE_INLINE_MEMORY_ACCESSORS=1 -I/Users/dmason/git/pharo-project/pharo-vm/include -I/Users/dmason/git/pharo-project/pharo-vm/include/pharovm -I/Users/dmason/git/pharo-project/pharo-vm/extracted/vm/include -I/Users/dmason/git/pharo-project/build/build/include/pharovm -I/Users/dmason/git/pharo-project/build/generated/64/vm/include -I/Users/dmason/git/pharo-project/pharo-vm/include/semaphores -I/Users/dmason/git/pharo-project/pharo-vm/extracted/vm/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/vm/include/unix -I/Users/dmason/git/pharo-project/pharo-vm/extracted/vm/include/common -I/Users/dmason/git/pharo-project/pharo-vm/ffi/include -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/FilePlugin/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/FilePlugin/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/FilePlugin/include/unix -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/FileAttributesPlugin/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/FileAttributesPlugin/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/FileAttributesPlugin/include/unix -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/SocketPlugin/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/SocketPlugin/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/SocketPlugin/include/unix -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/SurfacePlugin/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/SurfacePlugin/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/SurfacePlugin/include/unix -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/FloatArrayPlugin/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/FloatArrayPlugin/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/FloatArrayPlugin/include/unix -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/LargeIntegers/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/LargeIntegers/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/LargeIntegers/include/unix -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/JPEGReaderPlugin/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/JPEGReaderPlugin/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/JPEGReaderPlugin/include/unix -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/JPEGReadWriter2Plugin/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/JPEGReadWriter2Plugin/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/JPEGReadWriter2Plugin/include/unix -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/MiscPrimitivePlugin/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/MiscPrimitivePlugin/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/MiscPrimitivePlugin/include/unix -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/BitBltPlugin/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/B2DPlugin/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/B2DPlugin/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/B2DPlugin/include/unix -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/LocalePlugin/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/LocalePlugin/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/SqueakSSL/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/SqueakSSL/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/DSAPrims/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/DSAPrims/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/DSAPrims/include/unix -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/UnixOSProcessPlugin/include/common -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/UnixOSProcessPlugin/include/osx -I/Users/dmason/git/pharo-project/pharo-vm/extracted/plugins/UnixOSProcessPlugin/include/unix -isystem /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk/usr/include/ffi -g -stdlib=libc++ -mmacosx-version-min=10.7 -O2 -Wall -Werror=implicit-function-declaration  -DNDEBUG -DDEBUGVM=0 -g -arch arm64 -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk -fPIC -MD -MT CMakeFiles/PharoVMCore.dir/generated/64/vm/src/gcc3x-interp.c.o -MF CMakeFiles/PharoVMCore.dir/generated/64/vm/src/gcc3x-interp.c.o.d -o CMakeFiles/PharoVMCore.dir/generated/64/vm/src/gcc3x-interp.c.o -c /Users/dmason/git/pharo-project/build/generated/64/vm/src/gcc3x-interp.c
```

- **Linking C executable build/vm/Debug/Pharo.app/Contents/MacOS/Pharo**
```
/opt/homebrew/Cellar/cmake/3.24.2/bin/cmake -E cmake_link_script CMakeFiles/Pharo.dir/link.txt --verbose=1

/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/cc  -g -stdlib=libc++ -mmacosx-version-min=10.7 -O2 -Wall -Werror=implicit-function-declaration  -DNDEBUG -DDEBUGVM=0 -g -arch arm64 -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk -Wl,-search_paths_first -Wl,-headerpad_max_install_names CMakeFiles/Pharo.dir/src/unixMain.c.o -o build/vm/Debug/Pharo.app/Contents/MacOS/Pharo  build/vm/Debug/Pharo.app/Contents/MacOS/Plugins/libPharoVMCore.dylib /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk/usr/lib/libffi.tbd -framework AppKit -framework CoreGraphics
```
