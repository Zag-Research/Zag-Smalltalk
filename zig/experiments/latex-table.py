#! /usr/bin/env python
import argparse
import re

ENCODING_RE = re.compile(r"objectEncoding\s*=\s*\.(\w+)")
CPU_RE = re.compile(r"cpu\s*=.*\.(\w+)")
DATA_RE = re.compile(
    r"^\s{0,8}(\w+)\s+(\d+)ms\s+(\d+)ms\s+([\d.]+)ms"
    r"(?:\s+([\d.]+)%)?\s+([\d.]+)ms\s*$",
    re.MULTILINE,
)
METRICS = ["median", "mean", "stddev", "sdpct", "geomean"]
METRIC_LABELS = ["Median", "Mean", "StdDev", r"SD\%", "GeoM"]
BASELINES = {"onlyInt", "onlyFloat"}


def parse(file_content):
    cpu_match = CPU_RE.search(file_content)
    if cpu_match:
        arch = cpu_match.group(1)
    else:
        arch = "unknown"
    blocks = re.split(r"\nConfig:", "\n" + file_content)
    results, benchmark_order = {}, []
    for block in blocks:
        enc_match = ENCODING_RE.search(block)
        if not enc_match:
            continue
        enc = enc_match.group(1)
        results[enc] = {}
        for m in DATA_RE.finditer(block):
            name = m.group(1)
            results[enc][name] = {
                "median": int(m.group(2)),
                "mean": int(m.group(3)),
                "stddev": float(m.group(4)),
                "sdpct": float(m.group(5)) if m.group(5) else None,
                "geomean": float(m.group(6)),
            }
            if name not in benchmark_order:
                benchmark_order.append(name)
    return results, benchmark_order, arch


def get_base(benchmark, results):
    if benchmark.startswith("Integer"):
        return results.get("onlyInt", {}).get(benchmark)
    if benchmark == "Float":
        return results.get("onlyFloat", {}).get(benchmark)
    return None


def fmt_cell(metric, val, base):
    if val is None:
        return "N/A"
    if metric in ("median", "mean", "geomean") and base:
        return f"{int(round(val - base[metric]))}ms"
    if metric == "stddev":
        return f"{val:.2f}ms"
    if metric == "sdpct":
        return f"{val:.1f}\\%"
    return f"{int(round(val))}ms"


def tabular(row_header, col_headers, rows):
    col_fmt = "|l|" + "r|" * len(col_headers)
    header = (
        r"\textbf{"
        + row_header
        + "} & "
        + " & ".join(r"\textbf{" + c + "}" for c in col_headers)
    )
    lines = [r"\begin{tabular}{" + col_fmt + "}", r"\hline", header + r" \\ \hline"]
    for label, cells in rows:
        lines.append(label + " & " + " & ".join(cells) + r" \\ \hline")
    lines.append(r"\end{tabular}")
    return "\n".join(lines)


def per_encoding_tables(results, benchmark_order):
    tables = []
    for enc, benchmarks in results.items():
        rows = []
        for bm in benchmark_order:
            entry = benchmarks.get(bm)
            base = get_base(bm, results)
            cells = (
                ["N/A"] * len(METRICS)
                if entry is None
                else [fmt_cell(m, entry[m], base) for m in METRICS]
            )
            rows.append((bm, cells))
        tables.append(
            f"\\subsection*{{{enc}}}\n" + tabular("Benchmark", METRIC_LABELS, rows)
        )
    return "\n\n".join(tables)


def median_summary(results, benchmark_order):
    rows = []
    for enc, benchmarks in results.items():
        if enc in BASELINES:
            continue
        cells = []
        for bm in benchmark_order:
            entry = benchmarks.get(bm)
            base = get_base(bm, results)
            cells.append(
                "N/A" if entry is None else fmt_cell("median", entry["median"], base)
            )
        rows.append((enc, cells))
    return tabular("Encoding", benchmark_order, rows)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("filename")
    args = parser.parse_args()
    with open(args.filename) as f:
        results, benchmark_order, arch = parse(f.read())
    print(r"\newcommand{\\" + arch + "Appendix}{")
    print(per_encoding_tables(results, benchmark_order))
    print("}")
    print(r"\newcommand{\\" + arch + "Summary}{")
    print(median_summary(results, benchmark_order))
    print("}")


if __name__ == "__main__":
    main()
