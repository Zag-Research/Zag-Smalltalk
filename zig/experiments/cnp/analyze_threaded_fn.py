#!/usr/bin/env python3
"""
Analyze a threaded function from the compiled object file:
- Extract relocations with symbol names
- Resolve section+offset relocations to actual function names
- Match it with assembly code
- Show where CONTINUATION is called and what it points to
"""

import json
import re
import sys


def load_json_data(json_path):
    with open(json_path, "r") as f:
        return json.load(f)


def load_assembly(asm_path, func_name):
    with open(asm_path, "r") as f:
        lines = f.readlines()

    start_idx = None
    end_idx = None
    search_pattern = f"{func_name}:"
    size_pattern = f".size\t{func_name}"

    for i, line in enumerate(lines):
        if search_pattern in line and not line.strip().startswith("#"):
            start_idx = i
        elif start_idx and size_pattern in line:
            end_idx = i
            break

    if start_idx is None or end_idx is None:
        return None

    return lines[start_idx : end_idx + 1]


def find_symbol_at_offset(section, offset):
    best_match = None

    for sym_entry in section.get("Symbols", []):
        if "Symbol" not in sym_entry:
            continue

        sym = sym_entry["Symbol"]
        sym_start = sym["Value"]
        sym_size = sym["Size"]
        sym_end = sym_start + sym_size

        if sym_start <= offset < sym_end:
            if best_match is None or sym_size < best_match["Size"]:
                best_match = sym

    return best_match


def resolve_section_relocation(sections, section_name, offset):
    target_section = None
    for section in sections:
        if section["Section"]["Name"]["Name"] == section_name:
            target_section = section["Section"]
            break

    if not target_section:
        return None

    for delta in [0, 4, -4, 8, -8]:
        adjusted_offset = offset + delta
        symbol = find_symbol_at_offset(target_section, adjusted_offset)
        if symbol:
            sym_name = symbol["Name"]["Name"]
            if delta != 0:
                return f"{sym_name} (offset {offset}, resolved to {adjusted_offset})"
            return sym_name

    return None


def parse_function_info(data, func_name):
    sections = data[0]["Sections"]

    text_section = None
    for section in sections:
        if section["Section"]["Name"]["Name"] == ".text":
            text_section = section["Section"]
            break

    if not text_section:
        return None, sections

    func_symbol = None
    for sym in text_section["Symbols"]:
        if "Symbol" in sym:
            if sym["Symbol"]["Name"]["Name"] == func_name:
                func_symbol = sym["Symbol"]
                break

    if not func_symbol:
        return None, sections

    return {
        "offset": func_symbol["Value"],
        "size": func_symbol["Size"],
        "end": func_symbol["Value"] + func_symbol["Size"],
    }, sections


def extract_relocations(sections, func_info):
    rela_text = None
    for section in sections:
        if section["Section"]["Name"]["Name"] == ".rela.text":
            rela_text = section["Section"]
            break

    if not rela_text:
        return []

    relocations = []
    for reloc in rela_text["Relocations"]:
        reloc_data = reloc["Relocation"]
        offset = reloc_data["Offset"]

        if func_info["offset"] <= offset < func_info["end"]:
            symbol_name = reloc_data["Symbol"]["Name"]
            addend = reloc_data.get("Addend", 0)

            if addend > 2**63:
                addend = addend - 2**64

            resolved_symbol = symbol_name
            if symbol_name.startswith("."):
                target_offset = addend
                resolved = resolve_section_relocation(
                    sections, symbol_name, target_offset
                )
                if resolved:
                    resolved_symbol = f"{symbol_name} -> {resolved}"

            relocations.append(
                {
                    "offset": offset,
                    "func_offset": offset - func_info["offset"],
                    "type": reloc_data["Type"]["Name"],
                    "symbol": symbol_name,
                    "resolved_symbol": resolved_symbol,
                    "addend": addend,
                }
            )

    return sorted(relocations, key=lambda r: r["offset"])


def should_skip_line(line):
    stripped = line.strip()

    if not stripped:
        return True

    if stripped.startswith(".loc"):
        return True
    if stripped.startswith(".cfi_"):
        return True
    if stripped.startswith(".file"):
        return True
    if stripped.startswith(".size"):
        return True

    if re.match(r"^\.L(tmp|func_begin|func_end)", stripped):
        return True

    return False


def print_analysis(func_name, func_info, relocations, asm_lines):
    print("=" * 80)
    print(f"ANALYSIS: {func_name}")
    print("=" * 80)
    print()

    print("Function Information:")
    print(f"  Offset in .text: 0x{func_info['offset']:x}")
    print(f"  Size: {func_info['size']} bytes (0x{func_info['size']:x})")
    print(f"  Range: 0x{func_info['offset']:x} - 0x{func_info['end']:x}")
    print()

    print(f"Relocations ({len(relocations)}):")
    for reloc in relocations:
        print(
            f"  [+0x{reloc['func_offset']:02x}] 0x{reloc['offset']:x}: {reloc['type']:20s}"
        )
        print(f"        Symbol: {reloc['symbol']}")
        if reloc["resolved_symbol"] != reloc["symbol"]:
            print(f"        Resolved: {reloc['resolved_symbol']}")
        if reloc["addend"] != 0:
            print(f"        Addend: {reloc['addend']}")
        print()

    continuation_relocs = [r for r in relocations if r["symbol"] == "CONTINUATION"]
    if continuation_relocs:
        print(f"CONTINUATION calls: {len(continuation_relocs)}")
        for reloc in continuation_relocs:
            print(
                f"  At offset +0x{reloc['func_offset']:x} (absolute 0x{reloc['offset']:x})"
            )
        print()

    if asm_lines:
        print("Assembly Code:")
        print("-" * 80)
        for line in asm_lines:
            if should_skip_line(line):
                continue

            line = line.rstrip()
            if "CONTINUATION" in line:
                print(f">>> {line}")
            elif re.search(r"\b(call|jmp)\b", line) and not line.strip().startswith(
                "."
            ):
                print(f" -> {line}")
            else:
                print(f"    {line}")
        print("-" * 80)
        print()


def main():
    if len(sys.argv) < 2:
        print("analyze_threaded_fn.py <function_name>")
        return 1

    func_name = sys.argv[1]
    json_path = "../../zig-out/fib.json"
    asm_path = "../../zig-out/fib.s"

    print(f"Loading data for {func_name}...")
    data = load_json_data(json_path)

    func_info, sections = parse_function_info(data, func_name)
    if not func_info:
        print(f"Error: Could not find function '{func_name}' in JSON")
        return 1

    relocations = extract_relocations(sections, func_info)
    asm_lines = load_assembly(asm_path, func_name)

    print_analysis(func_name, func_info, relocations, asm_lines)

    return 0


if __name__ == "__main__":
    sys.exit(main())
