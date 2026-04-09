#! /usr/bin/env python
import argparse
import math
import re

ENCODING_RE = re.compile(r"objectEncoding\s*=\s*\.(\w+)")
CPU_RE = re.compile(r"cpu\s*=.*\.([a-z]+)")
DATA_RE = re.compile(
    r"^\s{0,8}(\w+)\s+(\d+)ms\s+(\d+)ms\s+([\d.]+)ms"
    r"(?:\s+([\d.]+)%)?\s+([\d.]+)ms\s*$",
    re.MULTILINE,
)
METRICS = ["median", "mean", "stddev", "sdpct", "geomean"]
METRIC_LABELS = ["Median", "Mean", "StdDev", r"SD\%", "GeoM"]
NATIVES = {"Native", "NativeF"}


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
        if enc == "zag":
            enc = "max"
        elif enc == "zagSpur":
            enc = "maxSpur"
        elif enc == "zagMixed":
            enc = "ultra"
        elif enc == "taggedPtr":
            enc = "taggedLow"
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
    if False:
        return {"median": 0, "mean": 0, "geomean": 0}
    if benchmark.startswith("Integer"):
        return results.get("onlyInt", {}).get(benchmark)
    if benchmark.startswith("Float"):
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
            cells = (
                ["N/A"] * len(METRICS)
                if entry is None
                else [fmt_cell(m, entry[m], None) for m in METRICS]
            )
            rows.append((bm, cells))
        tables.append(
            f"\\subsection*{{{enc}}}\n" + tabular("Benchmark", METRIC_LABELS, rows)
        )
    return "\n\n".join(tables)


summary_order = [
    "nan",
    "max",
    "maxSpur",
    "ultra",
    "spur",
    "taggedLow",
    "taggedHigh",
    "taggedInt",
    "taggedSMI",
    "cachedPtr",
    "ptr",
]


def median_summary(results, benchmark_order, total=False):
    rows = []
    for enc in results.keys() if total else summary_order:
        if enc not in results:
            continue
        if enc not in results:
            continue
        benchmarks = results[enc]
        cells = []
        for bm in benchmark_order:
            entry = benchmarks.get(bm)
            base = get_base(bm, results)
            cells.append(
                "N/A"
                if entry is None
                else fmt_cell("median", entry["median"], None if total else base)
            )
        rows.append((enc, cells))
    return tabular("Encoding", benchmark_order, rows)


colours = [
    "cyan!50",
    "teal!70",
    "green!60!black",
    "orange!60",
    "red!40",
    "blue!80!black",
    "olive!70",
    "red!60",
    "teal!40",
    "cyan!50!black",
    "green!60",
]


def median_graph(results, benchmark_order):
    result = f"\\benchmarkBarChart{{}}{{{','.join(summary_order)}}}{{%"
    for enc, colour in zip(summary_order, colours):
        if enc not in results:
            continue
        str = ", postaction={pattern=north east lines}" if False else ""
        result = (
            result
            + f"\n\\addplot[fill={colour}, draw=black, line width=0.3pt] coordinates "
            + "{"
        )
        benchmarks = results[enc]
        for bm in benchmark_order:
            entry = benchmarks.get(bm)
            base = get_base(bm, results)
            if entry and base:
                result = (
                    result + f"({bm},{(int(round(entry['median'] - base['median'])))})"
                )
        result = result + "};"
    result = result + "}"
    return result


SCATTER_ENCODINGS = [
    "cachedPtr",
    "nan",
    "ptr",
    "spur",
    "taggedInt",
    #    "taggedSMI",
    #    "taggedLow",
    "taggedHigh",
    "max",
    #    "maxSpur",
    "ultra",
]


def latex_arch_label(arch):
    if arch == "x":
        return "x86\\_64"
    if arch == "aarch":
        return "AArch64"
    if arch == "x86_64":
        return "x86\\_64"
    if arch == "aarch64":
        return "AArch64"
    return arch


def scatter_points(results):
    if not results:
        return {}
    base_int = get_base("IntegerBr", results)
    base_float = get_base("Float", results)
    if not base_int or not base_float:
        raise ValueError("Missing onlyInt/onlyFloat baselines for scatter plot.")
    base_int = {"median": 0}
    base_float = {"median": 0}
    points = {}
    for enc in SCATTER_ENCODINGS:
        entry = results.get(enc, {})
        int_entry = entry.get("IntegerBr")
        float_entry = entry.get("Float")
        if not int_entry or not float_entry:
            continue
        dx = int(round(int_entry["median"] - base_int["median"]))
        dy = int(round(float_entry["median"] - base_float["median"]))
        points[enc] = (dx, dy)
    return points


def pareto_frontier(all_points):
    pts = list(all_points)
    frontier = []
    for i, (x, y) in enumerate(pts):
        dominated = False
        for j, (ox, oy) in enumerate(pts):
            if i == j:
                continue
            if ox <= x and oy <= y and (ox < x or oy < y):
                dominated = True
                break
        if not dominated:
            frontier.append((x, y))
    frontier.sort(key=lambda p: (p[0], p[1]))
    collapsed = []
    for x, y in frontier:
        if collapsed and collapsed[-1][0] == x:
            if y < collapsed[-1][1]:
                collapsed[-1] = (x, y)
        else:
            collapsed.append((x, y))
    return collapsed


def axis_limits(points):
    xs = [p[0] for p in points]
    ys = [p[1] for p in points]
    min_x = min(xs)
    max_x = max(xs)
    pad_x = max(10, int(math.ceil(max_x * 0.06)))
    min_y = min(ys)
    max_y = max(ys)
    pad_y = max(10, int(math.ceil(max_y * 0.06)))
    xmin = int(math.floor((min_x - pad_x) / 10.0) * 10)
    ymin = int(math.floor((min_y - pad_y) / 10.0) * 10)
    xmax = int(math.ceil((max_x + pad_x) / 10.0) * 10)
    ymax = int(math.ceil((max_y + pad_y) / 10.0) * 10)
    return xmin, xmax, ymin, ymax


def label_anchor(y, ymin, ymax):
    if y - ymin < (ymax - ymin) * 0.08:
        return "north west"
    if ymax - y < (ymax - ymin) * 0.08:
        return "south"
    return "south west"


def scatter_plot_raw(results_a, arch_a, results_b=None, arch_b=None):
    points_a = scatter_points(results_a)
    points_b = scatter_points(results_b)
    all_points = list(points_a.values()) + list(points_b.values())
    xmin, xmax, ymin, ymax = axis_limits(all_points)
    frontier_a = pareto_frontier(list(points_a.values()))
    frontier_b = pareto_frontier(list(points_b.values()))

    lines = []
    lines.append("\\begin{tikzpicture}")
    lines.append("\\begin{axis}[")
    lines.append("    width=\\columnwidth,")
    lines.append("    height=\\columnwidth,")
    lines.append("    xlabel={IntegerBr (ms)},")
    lines.append("    ylabel={Float (ms)},")
    lines.append(f"    xmin={xmin}, xmax={xmax},")
    lines.append(f"    ymin={ymin}, ymax={ymax},")
    lines.append("    grid=major,")
    lines.append("    grid style={line width=0.2pt, draw=gray!30},")
    lines.append("    legend style={")
    lines.append("        at={(0.98,0.02)},")
    lines.append("        anchor=south east,")
    lines.append("        font=\\scriptsize,")
    lines.append("        draw=black,")
    lines.append("        legend columns=1,")
    lines.append("    },")
    lines.append("]")
    lines.append("    \\addplot[blue!60, thick, dashed, forget plot] coordinates {")
    lines.append(
        "        " + " ".join(f"({x},{y})" for x, y in frontier_a) + "\n    };"
    )
    lines.append("    \\addplot[red!60, thick, dashed, forget plot] coordinates {")
    lines.append(
        "        " + " ".join(f"({x},{y})" for x, y in frontier_b) + "\n    };"
    )
    lines.append(
        "    \\addplot[only marks, mark=*, mark size=3pt, blue!80!black] coordinates {"
    )
    lines.append(
        "        "
        + " ".join(
            f"({points_a[e][0]},{points_a[e][1]})"
            for e in SCATTER_ENCODINGS
            if e in points_a
        )
        + "\n    };"
    )
    lines.append(f"    \\addlegendentry{{{latex_arch_label(arch_a)}}}")
    if arch_b:
        lines.append(
            "    \\addplot[only marks, mark=*, mark size=3pt, red!70!black] coordinates {"
        )
        lines.append(
            "        "
            + " ".join(
                f"({points_b[e][0]},{points_b[e][1]})"
                for e in SCATTER_ENCODINGS
                if e in points_b
            )
            + "\n    };"
        )
        lines.append(f"    \\addlegendentry{{{latex_arch_label(arch_b)}}}")
    lines.append(
        "    \\addplot[gray!60, thick, dashed] coordinates {(-100,-100) (-100,-100)};"
    )
    lines.append("    \\addlegendentry{Pareto frontier}")
    lines.append(f"    % {latex_arch_label(arch_a)} labels")
    for enc in SCATTER_ENCODINGS:
        if enc not in points_a:
            continue
        x, y = points_a[enc]
        anchor = label_anchor(y, ymin, ymax)
        lines.append(
            f"    \\node[font=\\tiny, blue!80!black, anchor={anchor}] at (axis cs:{x + 2},{y + 3}) {{{enc}}};"
        )
    if arch_b:
        lines.append(f"    % {latex_arch_label(arch_b)} labels")
        for enc in SCATTER_ENCODINGS:
            if enc not in points_b:
                continue
            x, y = points_b[enc]
            anchor = label_anchor(y, ymin, ymax)
            lines.append(
                f"    \\node[font=\\tiny, red!70!black, anchor={anchor}] at (axis cs:{x + 2},{y + 3}) {{{enc}}};"
            )
    lines.append("\\end{axis}")
    lines.append("\\end{tikzpicture}")
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("filename", nargs="+")
    args = parser.parse_args()
    if len(args.filename) == 1:
        with open(args.filename[0]) as f:
            results, benchmark_order, arch = parse(f.read())
        print("\\newcommand{\\" + arch + "Appendix}{")
        print(per_encoding_tables(results, benchmark_order))
        print("}")
        print("\\newcommand{\\" + arch + "Total}{")
        print(median_summary(results, benchmark_order, True))
        print("}")
        benchmark_summary = [item for item in benchmark_order if item not in NATIVES]
        print("\\newcommand{\\" + arch + "Summary}{")
        print(median_summary(results, benchmark_summary))
        print("}")
        print("\\newcommand{\\" + arch + "BarChart}{")
        print(median_graph(results, benchmark_summary))
        print("}")
        print("\\newcommand{\\" + arch + "ScatterPlot}{")
        print(scatter_plot_raw(results, arch))
        print("}")
    elif len(args.filename) == 2:
        with open(args.filename[0]) as f:
            results_a, _, arch_a = parse(f.read())
        with open(args.filename[1]) as f:
            results_b, _, arch_b = parse(f.read())
        print("\\newcommand{\\scatterPlotRaw}{")
        print("% ── Scatter plot (raw ms) ──")
        print(scatter_plot_raw(results_a, arch_a, results_b, arch_b))
        print("}")
    else:
        raise SystemExit("Provide one file (tables) or two files (scatter plot).")


if __name__ == "__main__":
    main()
