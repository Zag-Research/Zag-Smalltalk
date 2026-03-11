#! /usr/bin/env python
import re
import argparse


ENCODING_RE = re.compile(r"objectEncoding\s*=\s*\.(\w+)")
DATA_RE = re.compile(
    r"^\s{0,8}(\w+)\s+(\d+)ms\s+(\d+)ms\s+([\d.]+)ms"
    r"(?:\s+([\d.]+)%)?\s+([\d.]+)ms\s*$",
    re.MULTILINE,
)


def parse(file_content):
    # Split on Config: blocks (leading \n handles blank line before each block)
    blocks = re.split(r"\nConfig:", "\n" + file_content)
    results = {}  # encoding -> benchmark_name -> geomean (float ms)
    column_order = []  # preserve first-seen order across all blocks

    for block in blocks:
        if not block.strip():
            continue
        enc_match = ENCODING_RE.search(block)
        if not enc_match:
            continue
        encoding = enc_match.group(1)
        results[encoding] = {}

        for m in DATA_RE.finditer(block):
            name = m.group(1)
            geomean = float(m.group(6))
            results[encoding][name] = geomean
            if name not in column_order:
                column_order.append(name)

    return results, column_order


def generate_latex_table(results, column_order):
    encodings = list(results.keys())

    col_fmt = "|l|" + "r|" * len(column_order)
    header_cells = " & ".join(
        r"\textbf{" + c + "}" for c in column_order
    )

    lines = [
        r"\begin{tabular}{" + col_fmt + "}",
        r"\hline",
        r"\textbf{Encoding} & " + header_cells + r" \\ \hline",
    ]

    for enc in encodings:
        clean_enc = enc.replace("_", r"\_")
        cells = []
        for col in column_order:
            val = results[enc].get(col)
            if val is None:
                cells.append("N/A")
            else:
                ms = int(round(val))
                cells.append(f"{ms}ms")
        lines.append(clean_enc + " & " + " & ".join(cells) + r" \\ \hline")

    lines.append(r"\end{tabular}")
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(
        description="Parse fib.zig benchmark logs into a LaTeX table."
    )
    parser.add_argument("filename", help="Path to the benchmark log file")
    args = parser.parse_args()

    with open(args.filename, "r") as f:
        content = f.read()

    results, column_order = parse(content)
    print(generate_latex_table(results, column_order))


if __name__ == "__main__":
    main()
