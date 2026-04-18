#! /usr/bin/env python
# to extract the mantissa information from the N. Zurstraßen, “Instruction and FPU Traces,” https://chciken.com/assets/ risc-v floating point/traces.zip files use
# sed -n -e '/"exp_/s/^.*: .\([0-9, ]*\)..$/\1/p' * | grep -v '^\(0, \)*0$'
# then run this to get statistics on the various mantissa values

import sys
from ctypes import pythonapi


def main():
    # List to store the sum of each column
    column_totals = []

    for line in sys.stdin:
        # Clean whitespace and split by comma
        columns = line.strip().split(",")

        # Skip empty lines
        if not columns or columns == [""]:
            continue

        for i, value in enumerate(columns):
            # Expand the totals list if a new column is found
            while len(column_totals) <= i:
                column_totals.append(0)

            column_totals[i] += int(value)

    # Print results
    if not column_totals:
        print("No numeric data processed.")
        return
    print("--- Final Totals ---")
    others = 0
    total = sum(column_totals)
    prev = 0
    column_totals.append(0)
    accum = 0
    spur = 0
    orig = 0
    zag = 0
    newO = 0
    for idx, count in enumerate(column_totals):
        bits = idx >> 7  # & 7
        if bits == prev:
            accum += count
            if idx > 0:
                continue
            else:
                count = 0
        tag = ""
        if prev == 7 or prev == 8 or idx == 0:
            spur += accum
            tag += "s"
        if prev >> 1 < 6:
            orig += accum
            tag += "o"
        if prev >> 1 < 5 or prev >> 1 == 7:
            newO += accum
            tag += "n"
        if prev & 7 == 0 or prev & 7 == 7:
            zag += accum
            tag += "z"
        if idx == 0:
            print(f"value       0: {accum * 100.0 / total:5.2f}% {tag}")
        else:
            print(f"exponent {prev:04b}: {accum * 100.0 / total:5.2f}% {tag}")
        prev = bits
        accum = count
    print(f"zag:     {zag * 100.0 / total:5.2f}%")
    print(f"zagOrig: {orig * 100.0 / total:5.2f}%")
    print(f"newOrig: {newO * 100.0 / total:5.2f}%")
    print(f"spur:    {spur * 100.0 / total:5.2f}%")


if __name__ == "__main__":
    main()
