## Image format
An image is composed of a set of files in a directory. 

#### `.image` file

| Name            | Value                | Description                                        |
| --------------- | -------------------- | -------------------------------------------------- |
| `magic`         | `0x6567616D4967615A` | endian flag with 'ZagImage' tag                    |
| `target`        | `Object`             | after image loading, send a message to this object |
| `selector`      | `Symbol`             | this is the unary message to send                  |
| `classTable`    | `Array`              | all the class objects                              |
| `symTable`      | `InternalHash`       | the hashed symbol table                            |
| `dispatchTable` | `Array`              | a copy of the dispatch table                       |
| `codeAddresses` | `DoubleWordArray`    | the addresses encoded in threaded methods          |
The `codeAddresses` array contains the code addresses encoded in the threaded portion of the `CompiledMethod` objects referenced in the dispatch tables. If they don't correspond with the current Zag runtime, then the `CompiledMethod` objects in the `dispatchTable` must be modified. There are 2 parts to this:
1. any JITed code needs to be discarded (freed)
2. any threaded code needs to be scanned and anything equal to one of the values needs to be replaced with the correct value from the current runtime.
A special case for this is when the image has been exported from another Smalltalk (Pharo, Cuis, etc.), in which case the values will be sequential, illegal heap pointers (8,16,24,...).
This mechanism makes changing versions of the runtime easy to handle, and also makes code generation in another Smalltalk easy.
### Other files
The remaining files each represent a part of the heap (a sub-heap). Most begin with an object header (a `ZagHeap` object), the address is encoded in the name of the image file as the longest valid hex number before the extension, e.g., `Zag_1000000000.heap` would be mapped at address 0x1000000000 (64GB). There may be several in the directory, and they will all be `mmap`'ed at the appropriate addresses.
#### `.heap` file

| Name           | Value             | Description                                |
| -------------- | ----------------- | ------------------------------------------ |
| `loadAddress`  | native integer    | address that corresponds with the filename |
| `nextHeap`     | address           | link to next heap header                   |
| `largeObjects` | `DoubleWordArray` | large object addresses for this sub-heap   |
The address/lengths cannot conflict. If any do, Zag won't start.
#### .process file
There may also be files with a `.process` extension that also have an address preceding the extension. They contain a single `Process` object and are linked together. There is one for every mutator process in the running image. These files are created lazily (i.e. only on an image save), but if a process exists and there is a `mmap`ed file, the file will be removed.

| Name          | Value          | Description                                |
| ------------- | -------------- | ------------------------------------------ |
| `loadAddress` | native integer | address that corresponds with the filename |
| `nextProcess` | address        | link to next process header                |
#### .lho file
There may also be files with a `.lho` extension that also have an address preceding the extension. They contain a large heap object, that is mapped independently from the `.heap` memory. The content of them is completely unspecified, as they will be referenced by a normal object in some `.heap` area.
### Loading the image
If `zag` is run with a command like:
```
zag foo
```
`foo` must be a directory. It will look inside the directory for image files with the extension `heap`. The name must have a suffix that is a valid hex address, as described above.

Loading looks like:
1. The `.image`, `.heap`, `.lho`, and `.process` files are mapped into memory at the appropriate addresses (which may fail if the criteria above are not met).
2. The endianness of the image is determined. In theory the image can be corrected to the current endianness, but for now it will simply fail.
3. If the `ZagConstants` array does not match the current one, all the `CompiledMethod`s will be updated to match the current executable, and all JIT'ed methods will be discarded (because there is no obvious way to update the heap that LLVM uses to be consistent).
4. The dispatch table is copied to the base sub-heap.
5. If we found processes, OS threads are allocated to the mutator and I/O processes and start running.
6. Otherwise, a process is created and the selector is sent to the target object (e.g. send `start` to class `System`).

### Saving the image
1. all mutator threads reach a quiescent state and block.
2. then all I/O processes save information to be restartable and block.
3. the dispatch table is copied to the image base.
4. the appropriate target and selector are added to the base sub-heap
5. all already `mmap`ed allocations are synced, all other allocations are written to files with the appropriate name, and then the files are `mmap`ed.

### Save and exiting the image
1. all mutator threads reach a quiescent state and block.
2. then all I/O processes save information to be restartable, close their files/stop-listening, and block.
3. the dispatch table is copied to the base allocation.
4. the appropriate target and selector are added to the base sub-heap
5. all `mmap`ed allocations are synced, all other allocations are written to files with the appropriate name.
6. `zag` exits
