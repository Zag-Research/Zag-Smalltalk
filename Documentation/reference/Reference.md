## Reference

### Smalltalk

- [Rewilding Software Engineering  - currently to Chapter 5](https://medium.com/feenk/rewilding-software-engineering-900ca95ebc8c)
- [Blue Book](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf)
- [Jecel's list of Smalltalks](https://github.com/jeceljr/SmalltalkSurvey)
- [UTF8 strings for Pharo](https://github.com/svenvc/UTF8String)
- [point-free Smalltalk](https://smalltalkthoughts.blogspot.com/2011/08/point-free-programming-in-smalltalk.html)
- [Marcus Denker blog](https://blog.marcusdenker.de/)
- [Cog blog - build me a JIT as fast as you can](http://www.mirandabanda.org/cogblog/2011/03/01/build-me-a-jit-as-fast-as-you-can/)
- [improving SqueakJS  performance](https://squeak.js.org/docs/jit.md.html)
- [Fibonacci hashing](https://probablydance.com/2018/06/16/fibonacci-hashing-the-optimization-that-the-world-forgot-or-a-better-alternative-to-integer-modulo/)
- [building Pharo VM](https://github.com/pharo-project/pharo-vm) 
- [Resilient Smalltalk](https://www.slideshare.net/slideshow/esug2004-rtresilient/5544538) [slide 8](https://www.slideshare.net/slideshow/esug2004-rtresilient/5544538#8)
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
- [Lambda Calculus explained in video](https://www.youtube.com/watch?v=RcVA8Nj6HEo)

### Editors
- [Why Zed IDE Outshines VS Code, Cursor, and Others: A Developer’s Guide](https://medium.com/@vignarajj/why-zed-ide-outshines-vs-code-cursor-and-others-a-developers-guide-7af334fa4392) - includes how to set up [LMStudio](https://lmstudio.ai) to give you local, private AI (also useful outside of Zed)
- [VSCodium](https://vscodium.com/) - VSCode without Microsoft monitoring
### WUFFS
- [wuffs](https://github.com/google/wuffs/tree/main/doc/std) is a buffer-safe library for transcoding arbitrary input, including:
	- [Compression Decoders](https://github.com/google/wuffs/blob/main/doc/std/compression-decoders.md).
	- [Hashers](https://github.com/google/wuffs/blob/main/doc/std/hashers.md).
	- [Image Decoders](https://github.com/google/wuffs/blob/main/doc/std/image-decoders.md).
### Memory Management
#### mimalloc
- [on github](https://github.com/microsoft/mimalloc)
- has some interesting ideas similar to those I was thinking about
- [this post](https://github.com/microsoft/mimalloc/issues/215#issuecomment-599711867) provides some hints about their memory structure
#### Mesh
- [Mesh](https://github.com/plasma-umass/Mesh) is a high performance malloc
#### Mist
- [MistDesign](MistDesign.md) also has some ideas we modify
#### Garbage Collectors
- [Deconstructing the garbage-first collector](https://dl.acm.org/doi/10.1145/3381052.3381320)
- [MMTk](https://github.com/mmtk)

### User interface
- [_Chromium_ Embedded Framework](https://github.com/chromiumembedded/cef)
- [Ultralight](https://ultralig.ht/)
- [DVUI](https://github.com/david-vanderson/dvui)
- [zig-webui](https://github.com/webui-dev/zig-webui)
- [curated list of Electron alternatives](https://github.com/sudhakar3697/awesome-electron-alternatives)
- [Ghostty](https://mitchellh.com/writing/ghostty-is-coming#user-content-fn-3) - particularly libghostty - would be a possible Zag TUI
- [GPUI from Zed](https://www.gpui.rs/) would be a possible Zag GUI (in Rust, so would need interface) [see also](https://zed.dev/blog/videogame) [and](https://github.com/zed-industries/zed/tree/main/crates/gpui) [and](https://github.com/longbridge/gpui-component)
- A better approach might be to expose Package/Class/Method to Zed as a directory structure. This would allow agentic mode.

## Data types
- [CRTDs](https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type)
## Architectures
- [C Is Not a Low-level Language - Your computer is not a fast PDP-11](https://queue.acm.org/detail.cfm?id=3212479)

## Testing
- [Antithesis](https://antithesis.com/product/what_is_antithesis/)

#### Smalltalk machines
- [A discussion where most of these links came from](https://narkive.com/NFS7uGan.4)
- [[The-Design-and-Evaluation-of-a-High-Performance-Smalltalk-System.pdf]]
SOAR (mentioned by Davide Grandi - thanks for the link!) was one of the  
great student project Smalltalk machines. The others were Mushroom and  
J-Machine: 
- [Mushroom](http://www.wolczko.com/mushroom/index.html)
- [J-Machine](http://cva.stanford.edu/j-machine/cva_j_machine.html)
  
Though the Xerox PARC computers such as the Alto and Dorado were never  
released commercially and not all were used as Smalltalk machines I  
think it is fair to compare them to the Symbolics and LMI efforts. The  
Rekursive was an important Smalltalk machine that never reached the  
market: 
http://www.brouhaha.com/~eric/retrocomputing/rekursiv/  
Rekursiv used a Smalltalk 'like' language - not Smalltalk  
See http://www.erg.abdn.ac.uk/research/projects/lingo.html  
Here's some more about Rekursiv  
http://www.brouhaha.com/~eric/retrocomputing/rekursiv/rekursiv.txt  
While in practice the focus was on Lingo, the November 1988 Byte  
magazine article did present the Rekursive as a Smalltalk machine.  
Mario Wolczko's comment (in 1992) about how much (actually how little)  
was developed in this direction is interesting:  
http://www.merlintec.com/old-self-interest/msg00248.html  
 
If we expand our definition to include computers using conventional  
microprocessors but built specifically to run Smalltalk then we have  
some that were actually sold: the Tektronix 4404 (68010) and 4406  
(68020) workstations and the Momenta (386SX) pen based computer (what  
Microsoft has been calling tabletPCs).
http://www.byte.com/art/9611/sec4/art1.htm  
  
Some products of this kind that failed to reach the market were the  
Exobox (ARM?), Interval Research (ARM) and my own Merlin efforts (68000  
and ARM).  
http://www.merlintec.com/lsi/history.html  
The exobox stuff was platform agnostic though mainly expected to run on a linux-  
ish OS.  
And not sold but almost completed before being killed by ATT, the Active Book  
(2Mb rom, 1Mb ram, full Smalltalk-80 system, ARM 8MHz).  
  
I (Jecel) still haven't given up and fortunately FPGAs now allow me to build  
the first class of machines (custom Smalltalk processors) instead of  
the second.  
  
There were other student project and commercial efforts beyond those I  
listed above, but it would take a short book to do them all justice.  

Well, I think the Dorado workstation at Xerox was built just for  
running Smalltalk. Don't really know why it was built. Maybe they  
needed a machine that was fast enough for Smalltalk's bit block  
operations on screen or they needed a machine they could attach a mouse  
to. If Xerox has a museum the Dorado might be displayed there ;-). Then  
there was the Dynabook for which Smalltalk was intended to be operating  
system and development environment. It envisioned what nowadays  
notebooks are (nowadays notebooks even do a little more).  

In '87 some company we can't quite track down demoed at OOPSLA a board  
that ran V286 on a microcoded machine (i.e. Smalltalk/V). From George  
Bosworth:  
The guys that ran it were spin out of aerospace corp in la  
where they had been doing custom stuff for the airforce and  
other DOD type people. The chip was also being positioned  
as a potential Ada chip since all of its microcode was in  
writeable control store.  
The name is escaping me....something like Micro Systems....  
#### HPC and architectures (cache et al)
- [Algorithmica](https://en.algorithmica.org/hpc/)
- [What Every Programmer Should Know About Memory](http://people.freebsd.org/~lstewart/articles/cpumemory.pdf)
- [Concerns of Self-Modifying-Code on ARM](https://dl.acm.org/doi/10.1145/3546568)
- [perf on Linux](https://www.brendangregg.com/perf.html) sampling performance
	- to enable:
		- `sudo apt install linux-perf` (or `linux-tools-common` on older Linux systems)
		- create `/etc/sysctl.d/local-perf.conf` and add the line: `kernel.perf_event_paranoid = -1`
		- then do `sudo sysctl -p`
		- verify by `perf stat -- sleep 1`
- [valgrind on Linux](https://valgrind.org/) detailed simulated performance
#### x86_64
- [x86-64 instructions](http://linasm.sourceforge.net/docs/instructions/)
- [x86-64](https://devblogs.microsoft.com/oldnewthing/20220831-00/?p=107077) [x86-64 Instructions and ABI (PDF)](https://www.classes.cs.uchicago.edu/archive/2009/spring/22620-1/docs/handout-03.pdf) [Intel syntax: Introduction to x64 Assembly (PDF)](https://www.intel.com/content/dam/develop/external/us/en/documents/introduction-to-x64-assembly-181178.pdf) 
#### ARM
- [Writing AArch64 code for Apple](https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms) and [Code in ARM Assembly (for Apple)](https://eclecticlight.co/2021/06/16/code-in-arm-assembly-registers-explained/)
- [More AArch64 coding advice](https://thinkingeek.com/2016/10/23/exploring-aarch64-assembler-chapter-3/)
- [Brief notes on Apple M1 Firestorm microarchitecture](https://github.com/ocxtal/insn_bench_aarch64/blob/master/optimization_notes_apple_m1.md)
- [AArch64 processor (Windows)](https://devblogs.microsoft.com/oldnewthing/20220726-00/?p=106898)
- [Computer Organization and Design (book) ARM editions](https://www.academia.edu/keypass/WEp6S1RhVXFYMjhVYUZBTkNCSzJvMnJLRzgyWFFONENMWWh0cVliVGY0OD0tLTExbEJEMUR4RXRLS3BDVDdIbVRYMXc9PQ==--6d9207bbe2f5e142ab68fe654908b0330e763f5d/t/E9U-QZniS8Z-ij15Y/resource/work/31710742/Computer_organization_and_design_arm_edition?auto=download&email_work_card=download-paper)
- [Application Binary Interface for the Arm® Architecture](https://github.com/ARM-software/abi-aa/) including [Procedure Call Standard for the Arm® 64-bit Architecture (AArch64)](https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst)
- [AArch64 encoding literals](https://dinfuehr.github.io/blog/encoding-of-immediate-values-on-aarch64/)
- [Wikipedia on ARM64 calling conventions](https://en.wikipedia.org/wiki/Calling_convention#ARM_(A64))
#### RISC-V
- [Computer Organization and Design](https://www.academia.edu/38301807/Computer_organization_and_design_RISC_V?auto=download&email_work_card=download-paper)
## LLVM
- [LLVM on asm syntax](https://releases.llvm.org/10.0.0/docs/LangRef.html#inline-asm-constraint-string) and [GCC on extended asm](https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html)

## Compiling
- [Sea of Nodes](https://github.com/SeaOfNodes) [talk](https://www.youtube.com/watch?v=NxiKlnUtyio)

## Atomics and Locks
- [Rust Atomics and Locks](https://marabos.nl/atomics/) most of which applies to Zig
- [Understanding Memory Ordering in Rust](https://emschwartz.me/understanding-memory-ordering-in-rust/) ditto (refers to Chapter 3 of the above)

## SIMD
- [discussion of SSE and Neon](https://blog.yiningkarlli.com/2021/09/neon-vs-sse.html)
## IEEE Floating point
- 64 bit floating point ![IEEE 754 Binary-64](Pasted%20image%2020210311212924.png)
- [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754)

## Zig
- [Zig reference](https://ziglang.org/documentation/master/#Assembly) and [Zig Learn](https://ziglearn.org/chapter-2/#random-numbers)
- [Zig 0.10.0 release notes](https://ziglang.org/download/0.10.0/release-notes.html#Function-Pointers)
- [alternate test runner](https://gist.github.com/karlseguin/c6bea5b35e4e8d26af6f81c22cb5d76b)
- [Custom Test Runner Blog Post: Karl Seguin](https://www.openmymind.net/Using-A-Custom-Test-Runner-In-Zig/)

## Miscellaneous
- [Hidden Heroes](https://hiddenheroes.netguru.com/)
- [Probability Theory: The Logic of Science](https://www.lesswrong.com/posts/KN3BYDkWei9ADXnBy/e-t-jaynes-probability-theory-the-logic-of-science-i)
- [Detailed Primer on Data Statistics](https://lakens.github.io/statistical_inferences/)
- [Why Japanese Developers Write Code Completely Differently (And Why It Works Better)](https://medium.com/@sohail_saifi/why-japanese-developers-write-code-completely-differently-and-why-it-works-better-de84d6244fab)
- [Hash Function Prospector](https://github.com/skeeto/hash-prospector?tab=readme-ov-file)
- [Lab Practices](https://labspractices.com/) Stories from an eXtreme Programming conssultancy

### Rust
- [Learn Rust](https://www.rust-lang.org/learn)
- [Are we Teaching Rust Effectively?](https://blog.kodewerx.org/2025/08/are-we-teaching-rust-effectively.html)
- [Arenas in Rust](https://manishearth.github.io/blog/2021/03/15/arenas-in-rust/)
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

### Pharo
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
