#! /usr/bin/env python
import re


def generate_complex_latex_table(file_content):
    # Regex patterns
    encoding_regex = re.compile(r"objectEncoding\s*=\s*\.(\w+)")
    # Captures the label (IntegerBr/IntegerCL/Float) and the LAST ms value on that line
    data_regex = re.compile(r"^\s*(Integer\w+|Float)\s+.*?(\d+)ms\s*$", re.MULTILINE)

    blocks = file_content.split("Config:")
    results = {}

    for block in blocks:
        if not block.strip():
            continue

        enc_match = encoding_regex.search(block)
        if enc_match:
            enc_name = enc_match.group(1)
            results[enc_name] = {}

            # Find all relevant data lines in this block
            for match in data_regex.finditer(block):
                label, value = match.groups()
                results[enc_name][label] = int(value)

    # Establish Baselines
    # .onlyInt baseline applies to IntegerBr and IntegerCL
    int_baseline = results.get("onlyInt", {}).get("IntegerBr", 0)
    # .onlyFloat baseline applies to Float
    float_baseline = results.get("onlyFloat", {}).get("Float", 0)

    # LaTeX Table Generation
    latex = [
        r"\begin{tabular}{|l|c|c|c|}",
        r"\hline",
        r"\textbf{Encoding} & \textbf{IntegerBr (adj)} & \textbf{IntegerCL (adj)} & \textbf{Float (adj)} \\ \hline",
    ]

    # Sort encodings for the table rows
    for enc in sorted(results.keys()):
        # Skip the baseline rows themselves if you don't want them in the final table,
        # or leave them in to show they result in 0.

        row_data = results[enc]

        # Helper to get adjusted value
        def get_adj(label, baseline):
            val = row_data.get(label)
            if val is None:
                return "N/A"
            return f"{val - baseline}ms"

        br_adj = get_adj("IntegerBr", int_baseline)
        cl_adj = get_adj("IntegerCL", int_baseline)
        fl_adj = get_adj("Float", float_baseline)

        clean_name = enc.replace("_", r"\_")
        latex.append(f"{clean_name} & {br_adj} & {cl_adj} & {fl_adj} \\\\ \\hline")

    latex.append(r"\end{tabular}")
    return "\n".join(latex)


import argparse


def main():
    parser = argparse.ArgumentParser(
        description="Parse benchmark logs into LaTeX tables."
    )

    # Add the argument for the filename
    parser.add_argument("filename", help="The path to the benchmark log file")

    args = parser.parse_args()

    # Now you can use args.filename
    with open(args.filename, "r") as f:
        content = f.read()
        # Call your function here
        result = generate_complex_latex_table(content)
        print(result)


if __name__ == "__main__":
    main()
