# Zag Smalltalk (Zig)

Zag is an experimental Smalltalk VM/runtime written in Zig. It focuses on
multiple object encodings, a compact interpreter, and optional LLVM-backed
JIT experimentation.

## Requirements

- Zig 0.13.0 or newer

## Build

```sh
zig build
```

## Run

```sh
zig build run -- <args>
```

## Tests

```sh
zig build test
```

Useful options:

- `-Dllvm=true` to enable LLVM integration
- `-Dencoding=zag` to select object encoding
- `-DmaxClasses=255` to cap class table size
- `-Dtrace=true` to enable trace logging

## API Documentation

```sh
zig build docs
```

Docs are emitted to `zig-out/docs`. Open `zig-out/docs/index.html` in a
browser.

## Repository Layout

- `zag/` VM core, object encodings, primitives, and runtime
- `experiments/` benchmarks and JIT experiments
- `zag/libs/zig-llvm/` LLVM bindings used when `-Dllvm=true`
