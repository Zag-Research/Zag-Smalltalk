# Reference Documentation

List of commands used for CnP and threaded function experimentation.

## Build Commands

### Generate Fib Build
```bash
# Generate fib build for x86_64-linux target
# Outputs: fib.s (assembly file) and fib.o (object file)
zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux
```

## Analysis Commands

### Disassemble Specific Function
```bash
# Disassemble a specific threaded function with relocations
llvm-objdump -d fib.o --reloc --disassemble-symbols=controlWords.drop.threadedFn
```

### Extract Symbol and Relocation Information
```bash
# Generate JSON output containing sections, symbols, and relocations
llvm-readobj --elf-output-style=JSON --pretty-print --expand-relocs --section-symbols --section-relocations --sections fib.o > fib.json

### Analyze Threaded Functions
```bash
# Parse assembly and relocation information for a specific function
python3 analyze_threaded_fn.py controlWords.drop.threadedFn
```

## Common Threaded Functions to Analyze

- `controlWords.drop.threadedFn`
- `controlWords.dup.threadedFn`
- `controlWords.push.threadedFn`
- `controlWords.pushLiteral.threadedFn`
