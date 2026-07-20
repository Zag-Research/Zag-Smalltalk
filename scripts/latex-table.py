#! /usr/bin/env python
import argparse
import math
import re
import sys

ENCODING_RE = re.compile(r"objectEncoding\s*=\s*\.(\w+)")
CPU_RE = re.compile(r"cpu\s*=.*\.([a-z]+)")
DATA_RE = re.compile(
    r"^\s{0,12}(\w+)\s+(\d+)ms\s+(\d+)ms\s+([\d.]+)ms"
    r"(?:\s+([\d.]+)%)?\s+([\d.]+)ms\s*$",
    re.MULTILINE,
)
METRICS = ["median", "mean", "stddev", "sdpct", "geomean"]
METRIC_LABELS = ["Median", "Mean", "StdDev", r"SD\%", "GeoM"]
NATIVES = {"Native", "NativeF"}
MEASURE = "mean"
REPORT_ENCODINGS = [
    "ptr",
    "nan",
    "nun",
    "zag",
    "zagSpur",
    "spur",
    "spurNZ",
    "taggedInt",
    "onlyInt",
    "onlyFloat",
    "cachedPtr",
    "compact2",
    "compactI1",
    "compactI2",
    "compactI4",
    "compactI6",
    "compactA2",
]


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
        if enc not in REPORT_ENCODINGS:
            continue
        # if enc == "zag":
        #     enc = "ultra"
        # elif enc == "zagSpur":
        #     enc = "maxSpur"
        # elif enc == "zagMixed":
        #     enc = "max"
        results[enc] = {}
        for m in DATA_RE.finditer(block):
            name = m.group(1)
            if name == "IntegerBr":
                name = "Integer"
            elif name == "IntegerClosure":
                name = "IClosure"
            elif name == "FloatClosure":
                name = "FClosure"
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
    if benchmark.startswith("I"):
        return results.get("onlyInt", {}).get(benchmark)
    if benchmark.startswith("F"):
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
    #    "zagMixed",
    "zag",
    "zagSpur",
    "spur",
    #    "spurOpt",
    "spurNZ",
    #    "spurFST",
    #    "compact1",
    #    "compact2",
    #    "compact4",
    #    "compact6",
    #    "compactI1",
    #    "compactI2",
    #    "compactI4",
    #    "compactI6",
    #    "compactY",
    #    "compactZ",
    #    "compactA2",
    #    "taggedLow",
    #    "taggedHigh",
    "taggedInt",
    #    "taggedSMI",
    #    "cachedPtr",
    "ptr",
]
colours = [
    "blue",
    "cyan!35",
    "cyan!50",
    #    "cyan!65",
    #    "cyan!80",
    #    "cyan!95",
    "orange!40",
    "orange!60",
    "green!20",
    #    "green!40",
    #    "green!60",
    #    "green!80",
    #    "green!20!black",
    #    "green!40!black",
    #    "green!60!black",
    #    "green!80!black",
    #    "blue!20",
    #    "blue!50!black",
    "red!50",
    "red!50!black",
    "olive!70",
    "olive!70!black",
    "blue!80!black",
    "red!60",
    "red!60!black",
    "green!30",
    "orange!50!black",
    "teal!40",
    "cyan!50!black",
    "green!60",
    "yellow",
    #    "teal!70",
    #    "green!60!black",
]


def measure_summary(results, benchmark_order, total=False):
    rows = []
    for enc in results.keys() if total else summary_order:
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
                else fmt_cell(MEASURE, entry[MEASURE], None if total else base)
            )
        rows.append((enc, cells))
    return tabular("Encoding", benchmark_order, rows)


def measure_graph(results, benchmark_order):
    result = f"\\benchmarkBarChart{{}}{{{','.join([item for item in summary_order if item in results])}}}{{%"
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
                    result + f"({bm},{(int(round(entry[MEASURE] - base[MEASURE])))})"
                )
        result = result + "};"
    result = result + "}"
    return result


SCATTER_ENCODINGS = [
    "cachedPtr",
    "taggedInt",
    #    "taggedSMI",
    #    "taggedLow",
    #   "taggedHigh",
    # "ptr",
    "nan",
    "spur",
    #    "spurOpt",
    "spurNZ",
    #    "spurFST",
    #    "compact1",
    # "compact2",
    #    "compact4",
    #    "compact6",
    "compactI1",
    "compactI2",
    # "compactI4",
    #    "compactI6",
    #    "compactY",
    #    "compactZ",
    "compactA2",
    #   "zagMixed",
    # "zagSpur",
    "zag",
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


def scatter_points(results, x_tag, y_tag, absolute):
    if not results:
        return {}
    base_x = not absolute and get_base(x_tag, results) or {MEASURE: 0}
    base_y = not absolute and get_base(y_tag, results) or {MEASURE: 0}
    points = {}
    include = absolute
    for enc in SCATTER_ENCODINGS:
        if enc == "nan":
            include = True
        if not include:
            continue
        entry = results.get(enc, {})
        x_entry = entry.get(x_tag)
        y_entry = entry.get(y_tag)
        if not x_entry or not y_entry:
            continue
        dx = int(round(x_entry[MEASURE] - base_x[MEASURE]))
        dy = int(round(y_entry[MEASURE] - base_y[MEASURE]))
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
    min_x = min(xs)  # test
    max_x = max(xs)
    pad_x = max(10, int(math.ceil(max_x * 0.03)))
    min_y = min(ys)
    max_y = max(ys)
    pad_y = max(10, int(math.ceil(max_y * 0.05)))
    xmin = int(math.floor((min_x - pad_x) / 100.0) * 100)
    ymin = int(math.floor((min_y - pad_y) / 100.0) * 100)
    xmax = int(math.ceil((max_x + pad_x) / 100.0) * 100)
    ymax = int(math.ceil((max_y + pad_y) / 100.0) * 100)
    return xmin, xmax, ymin, ymax


def label_anchor(y, ymin, ymax):
    if y - ymin < (ymax - ymin) * 0.08:
        return "north west"
    if ymax - y < (ymax - ymin) * 0.08:
        return "south"
    return "south west"


def scatter_plot_raw(absolute, include_pareto, records):
    lines = []
    all_points = []
    for record in records:
        record["points"] = scatter_points(
            record["results"], record["x"], record["y"], absolute
        )
        all_points = list(all_points) + list(record["points"].values())
    if not all_points:
        return ""
    xmin, xmax, ymin, ymax = axis_limits(all_points)
    if xmin < xmax * 0.35 and ymin < ymax * 0.35:
        xmin = 0
        ymin = 0
    omin = min(xmin, ymin)
    # (omax - omin) * 0.55 + omin
    xmax = max(xmax, (ymax - omin) / 0.55 + omin)
    ymax = max(ymax, (xmax - omin) * 0.55 + omin)
    lines.append("\\begin{tikzpicture}")
    lines.append("\\begin{axis}[")
    lines.append("    width=0.95\\columnwidth,")
    lines.append("    height=0.55\\columnwidth,")
    lines.append("    xlabel={Float (ms)},")
    lines.append("    ylabel={Integer (ms)},")
    lines.append(f"    xmin={omin}, xmax={xmax},")
    lines.append(f"    ymin={omin}, ymax={ymax},")
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
    for record in records:
        points = record["points"]
        if include_pareto:
            frontier = pareto_frontier(list(points.values()))
            lines.append(
                f"    \\addplot[{record['colour']}, thick, dashed, forget plot] coordinates {{"
            )
            lines.append(
                "        " + " ".join(f"({x},{y})" for x, y in frontier) + "\n    };"
            )
        lines.append(
            f"    \\addplot[only marks, mark=*, mark size=3pt, {record['colour']}] coordinates {{"
        )
        lines.append(
            "        "
            + " ".join(
                f"({points[e][0]},{points[e][1]})"
                for e in SCATTER_ENCODINGS
                if e in points
            )
            + "\n    };"
        )
        lines.append(
            f"    \\addlegendentry{{{latex_arch_label(record['arch'])}{' Closure' if record['x'].endswith('Closure') else ''}}}"
        )
        if include_pareto:
            lines.append(
                f"    \\addplot[{record['colour']}, thick, dashed] coordinates {{(-100,-100) (-100,-100)}};"
            )
            lines.append("    \\addlegendentry{Pareto frontier}")
        for enc in SCATTER_ENCODINGS:
            if enc not in points:
                continue
            x, y = points[enc]
            anchor = label_anchor(y, ymin, ymax)
            lines.append(
                f"    \\node[font=\\tiny, {record['colour']}!black, anchor={anchor}] at (axis cs:{x + 2},{y + 3}) {{{enc}}};"
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
        print(measure_summary(results, benchmark_order, True))
        print("}")
        benchmark_summary = [item for item in benchmark_order if item not in NATIVES]
        print("\\newcommand{\\" + arch + "Summary}{")
        print(measure_summary(results, benchmark_summary))
        print("}")
        print("\\newcommand{\\" + arch + "BarChart}{")
        print(measure_graph(results, benchmark_summary))
        print("}")
        colour = "blue" if arch == "aarch" else "red"
        print("\\newcommand{\\" + arch + "ScatterPlot}{")
        print(
            scatter_plot_raw(
                True,
                True,
                [
                    {
                        "y": "Integer",
                        "x": "Float",
                        "arch": arch,
                        "colour": colour + "!90",
                        "results": results,
                    },
                ],
            )
        )
        print("}")
        print("\\newcommand{\\" + arch + "ScatterPlotClosure}{")
        print(
            scatter_plot_raw(
                True,
                True,
                [
                    {
                        "y": "IClosure",
                        "x": "FClosure",
                        "arch": arch,
                        "colour": colour + "!30",
                        "results": results,
                    },
                ],
            )
        )
        print("}")
        print("\\newcommand{\\" + arch + "ScatterPlotBoth}{")
        print(
            scatter_plot_raw(
                True,
                False,
                [
                    {
                        "y": "Integer",
                        "x": "Float",
                        "arch": arch,
                        "colour": colour + "!90",
                        "results": results,
                    },
                    {
                        "y": "IClosure",
                        "x": "FClosure",
                        "arch": arch,
                        "colour": colour + "!30",
                        "results": results,
                    },
                ],
            )
        )
        print("}")
    elif len(args.filename) == 2:
        with open(args.filename[0]) as f:
            results_a, _, arch_a = parse(f.read())
        with open(args.filename[1]) as f:
            results_b, _, arch_b = parse(f.read())
        print("\\newcommand{\\scatterPlotRaw}{")
        print("% ── Scatter plot (raw ms) ──")
        print(
            scatter_plot_raw(
                True,
                False,
                [
                    {
                        "y": "Integer",
                        "x": "Float",
                        "arch": arch_a,
                        "colour": "blue!90",
                        "results": results_a,
                    },
                    {
                        "y": "IClosure",
                        "x": "FClosure",
                        "arch": arch_a,
                        "colour": "blue!30",
                        "results": results_a,
                    },
                    {
                        "y": "Integer",
                        "x": "Float",
                        "arch": arch_b,
                        "colour": "red!90",
                        "results": results_b,
                    },
                    {
                        "y": "IClosure",
                        "x": "FClosure",
                        "arch": arch_b,
                        "colour": "red!30",
                        "results": results_b,
                    },
                ],
            )
        )
        print("}")
    else:
        raise SystemExit("Provide one file (tables) or two files (scatter plot).")


if __name__ == "__main__":
    main()
