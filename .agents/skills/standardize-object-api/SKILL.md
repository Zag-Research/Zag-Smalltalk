---
name: standardize-object-api
description: This skill enforces a unified function declaration order across all experimental encoding files (e.g., `compact.zig`, `nan.zig`, `zag.zig`, etc.) to ensure maintainability and predictability across different memory layouts.
---
# Standardize-Object-API
---
## Description
This skill enforces a unified function declaration order across all experimental encoding files (e.g., `compact.zig`, `nan.zig`, `zag.zig`, etc.) to ensure maintainability and predictability across different memory layouts.

## Canonical Function Order

When refactoring or auditing encoding files, functions must be ordered according to the following sequence. If a file contains a function not listed here (specialized logic), place it in Group 7.

### Group 1: Constants & Static Initialization
- `False()`
- `True()`
- `Nil()`
- `init(...)`
- `from(ci: ClassIndex) ...` (The variant that returns the encoding type)

### Group 2: Object Creation & Tagging
- `from(value, sp, context) ...` (Main entry point for object creation)
- `fromAddress(...)`
- `fromNativeF(...)`
- `fromNativeI(...)`
- `fromTaggedI(...)`
- `fromUntaggedI(...)`
- `makeImmediate(...)`
- `makeSymbol(...)`
- `makeThunk(...)`
- `makeThunkNoArg(...)`

### Group 3: Type Identification & Checks
- `which_class(...)`
- `classIndex(...)` (The variant that returns a ClassIndex)
- `isImmediate(...)`
- `isImmediateClass(...)`
- `isSymbol(...)`
- `ifHeapObject(...)`
- `hasHeapReference(...)`
- *Any other specialized type checkers (e.g., `isBool`, `isCharacter`)*

### Group 4: Value Extraction & Accessors
- `pointer(...)`
- `encodedPointer(...)`
- `nativeF(...)`
- `nativeI(...)` (including variations like `nativeI_noCheck`)
- `taggedI(...)`
- `untaggedI(...)`
- `extraValue(...)`
- `extraI(...)`
- `extraU(...)`
- `extraImmediateI(...)`
- `extraImmediateU(...)`

### Group 5: Conversions & Casts
- `toWithCheck(...)`
- `toIntNoCheck(...)`
- `toNatNoCheck(...)`

### Group 6: Closure Management
- `immediateClosure(...)`
- `returnLiteralClosure(...)`
- `returnLocalClosure(...)`
- `returnObjectClosure(...)`

### Group 7: Utilities & Internal Helpers
- `hash24(...)`
- `hash32(...)`
- `symbolHash(...)`
- `numArgs(...)`
- `invalidObject(...)`
- `simple(...)` (Scanner helper)
- *Any other internal-only or file-specific functions*

## Strict Implementation Rules

1. **Logic Preservation:** When reordering functions to match this standard, the code logic MUST remain identical. Do not optimize, rewrite, or modify function bodies unless explicitly requested in a separate task.
2. **Signature Consistency:** Maintain existing signatures exactly as they are; only change the physical location of the function block within the file.
3. **Grouping:** Ensure that functions belonging to the same group are kept together.
