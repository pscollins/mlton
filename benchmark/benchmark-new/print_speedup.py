#!/usr/bin/env python3
"""
MLton Benchmark Results and Speedup Tabulator

Reads benchmark JSONL output files and displays results in formatted ASCII tables,
computing relative speedups, percentage differences, and statistical summaries
(Geometric Mean, Average, Median, Min, Max).
"""

import argparse
import csv
from datetime import datetime
import io
import json
import math
import os
from pathlib import Path
import re
import statistics
import sys
from typing import Any, Dict, List, Optional, Tuple


# Canonical MLton benchmark order
BENCHMARK_SUITE_ORDER = [
    "barnes-hut", "boyer", "checksum", "count-graphs", "DLXSimulator",
    "even-odd", "fft", "fib", "flat-array", "hamlet",
    "imp-for", "knuth-bendix", "lexgen", "life", "logic",
    "mandelbrot", "matrix-multiply", "md5", "merge", "mlyacc",
    "model-elimination", "mpuz", "nucleic", "output1", "peek",
    "psdes-random", "ratio-regions", "ray", "raytrace", "simple",
    "smith-normal-form", "string-concat", "tailfib", "tak", "tensor",
    "tsp", "tyan", "vector32-concat", "vector64-concat", "vector-rev",
    "vliw", "wc-input1", "wc-scanStream", "zebra", "zern",
]
BENCHMARK_ORDER_MAP = {name: i for i, name in enumerate(BENCHMARK_SUITE_ORDER)}


# ANSI color codes
class Color:
    RESET = "\033[0m"
    BOLD = "\033[1m"
    DIM = "\033[2m"
    RED = "\033[31m"
    GREEN = "\033[32m"
    YELLOW = "\033[33m"
    BLUE = "\033[34m"
    MAGENTA = "\033[35m"
    CYAN = "\033[36m"
    WHITE = "\033[37m"


ANSI_REGEX = re.compile(r"\033\[[0-9;]*m")


def strip_ansi(text: str) -> str:
    """Strip ANSI escape codes from string for calculating visible width."""
    return ANSI_REGEX.sub("", text)


def visible_len(text: str) -> int:
    """Return visible width of string taking ANSI codes into account."""
    return len(strip_ansi(text))


def colorize(text: str, color: str, enabled: bool = True) -> str:
    """Colorize text if color is enabled."""
    if not enabled or not color:
        return text
    return f"{color}{text}{Color.RESET}"


def get_file_timestamp(path: Path) -> Tuple[float, float]:
    """
    Extract timestamp from filename (pattern :YYYYMMDD_HHMMSS.jsonl)
    or fallback to filesystem modification time.
    """
    m = re.search(r":(\d{8}_\d{6})\.jsonl$", path.name)
    if m:
        try:
            dt = datetime.strptime(m.group(1), "%Y%m%d_%H%M%S")
            return (dt.timestamp(), path.stat().st_mtime)
        except ValueError:
            pass
    mtime = path.stat().st_mtime
    return (mtime, mtime)


def find_default_outputs_dir() -> Path:
    """Find the default outputs directory relative to this script."""
    script_dir = Path(__file__).resolve().parent
    outputs_dir = script_dir / "outputs"
    if outputs_dir.is_dir():
        return outputs_dir
    alt = Path("benchmark/benchmark-new/outputs").resolve()
    if alt.is_dir():
        return alt
    return outputs_dir


def resolve_input_file(target: Optional[str], outputs_dir: Path) -> Path:
    """
    Resolve input file path from target string:
    1. If target is None, find most recent .jsonl in outputs_dir
    2. If target is an existing path, return it
    3. If target is in outputs_dir, return it
    4. If target matches prefix or substring in outputs_dir, return match
    """
    if not target:
        if not outputs_dir.is_dir():
            raise FileNotFoundError(f"Outputs directory not found: {outputs_dir}")
        files = list(outputs_dir.glob("*.jsonl"))
        if not files:
            raise FileNotFoundError(f"No .jsonl output files found in {outputs_dir}")
        return max(files, key=get_file_timestamp)

    p = Path(target)
    if p.is_file():
        return p.resolve()

    # Try relative to outputs_dir
    cand = outputs_dir / target
    if cand.is_file():
        return cand.resolve()
    if cand.with_suffix(".jsonl").is_file():
        return cand.with_suffix(".jsonl").resolve()

    # Search by prefix or substring in outputs_dir
    if outputs_dir.is_dir():
        matches = list(outputs_dir.glob(f"{target}*.jsonl"))
        if not matches:
            matches = list(outputs_dir.glob(f"*{target}*.jsonl"))
        if len(matches) == 1:
            return matches[0].resolve()
        elif len(matches) > 1:
            return max(matches, key=get_file_timestamp).resolve()

    raise FileNotFoundError(f"Could not find benchmark output file matching '{target}' (searched in {outputs_dir})")


def parse_jsonl(file_path: Path) -> List[Dict[str, Any]]:
    """Parse JSONL file into a list of record dictionaries."""
    records = []
    with open(file_path, "r", encoding="utf-8") as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            try:
                data = json.loads(line)
                records.append(data)
            except json.JSONDecodeError as e:
                print(f"Warning: Skipping invalid JSON on line {line_num} in {file_path}: {e}", file=sys.stderr)
    return records


class TableFormatter:
    """Formats and prints tables in ASCII Grid, Markdown, Simple, CSV, or TSV format."""

    def __init__(self, headers: List[str], alignments: List[str], format_type: str = "ascii", use_color: bool = False):
        self.headers = headers
        self.alignments = alignments  # '<' for left, '>' for right, '^' for center
        self.format_type = format_type.lower()
        self.use_color = use_color
        self.rows: List[List[str]] = []
        self.raw_rows: List[List[str]] = []
        self.summary_rows: List[List[str]] = []
        self.raw_summary_rows: List[List[str]] = []

    def add_row(self, row: List[str], raw_row: Optional[List[str]] = None) -> None:
        self.rows.append(row)
        self.raw_rows.append(raw_row if raw_row is not None else [strip_ansi(c) for c in row])

    def add_summary_row(self, row: List[str], raw_row: Optional[List[str]] = None) -> None:
        self.summary_rows.append(row)
        self.raw_summary_rows.append(raw_row if raw_row is not None else [strip_ansi(c) for c in row])

    def render(self) -> str:
        if self.format_type in ("csv", "tsv"):
            return self._render_delimited("," if self.format_type == "csv" else "\t")
        elif self.format_type in ("markdown", "md"):
            return self._render_markdown()
        elif self.format_type in ("plain", "simple"):
            return self._render_simple()
        else:  # 'ascii', 'grid'
            return self._render_ascii_grid()

    def _calculate_widths(self) -> List[int]:
        num_cols = len(self.headers)
        widths = [visible_len(h) for h in self.headers]
        all_raw = self.raw_rows + self.raw_summary_rows
        for row in all_raw:
            for i in range(min(num_cols, len(row))):
                widths[i] = max(widths[i], visible_len(row[i]))
        return widths

    def _align_cell(self, cell: str, raw_cell: str, width: int, align: str) -> str:
        v_len = visible_len(raw_cell)
        pad = max(0, width - v_len)
        if align == "<":
            return cell + (" " * pad)
        elif align == ">":
            return (" " * pad) + cell
        else:  # '^'
            left = pad // 2
            right = pad - left
            return (" " * left) + cell + (" " * right)

    def _render_ascii_grid(self) -> str:
        widths = self._calculate_widths()
        border = "+-" + "-+-".join("-" * w for w in widths) + "-+"
        sep = "+=" + "=+=".join("=" * w for w in widths) + "=+"
        lines = []

        # Top border
        lines.append(border)

        # Header
        header_cells = [
            self._align_cell(h, h, w, a)
            for h, w, a in zip(self.headers, widths, self.alignments)
        ]
        if self.use_color:
            header_str = "| " + " | ".join(colorize(c, Color.BOLD, True) for c in header_cells) + " |"
        else:
            header_str = "| " + " | ".join(header_cells) + " |"
        lines.append(header_str)
        lines.append(border)

        # Data rows
        for row, raw_row in zip(self.rows, self.raw_rows):
            cells = [
                self._align_cell(c, rc, w, a)
                for c, rc, w, a in zip(row, raw_row, widths, self.alignments)
            ]
            lines.append("| " + " | ".join(cells) + " |")

        # Summary rows
        if self.summary_rows:
            lines.append(sep)
            for row, raw_row in zip(self.summary_rows, self.raw_summary_rows):
                cells = [
                    self._align_cell(c, rc, w, a)
                    for c, rc, w, a in zip(row, raw_row, widths, self.alignments)
                ]
                lines.append("| " + " | ".join(cells) + " |")
            lines.append(border)
        else:
            lines.append(border)

        return "\n".join(lines)

    def _render_markdown(self) -> str:
        widths = self._calculate_widths()
        lines = []

        # Header
        header_cells = [
            self._align_cell(h, h, w, a)
            for h, w, a in zip(self.headers, widths, self.alignments)
        ]
        lines.append("| " + " | ".join(header_cells) + " |")

        # Delimiter row
        delims = []
        for w, a in zip(widths, self.alignments):
            if a == "<":
                delims.append(":" + "-" * max(1, w - 1))
            elif a == ">":
                delims.append("-" * max(1, w - 1) + ":")
            else:
                delims.append(":" + "-" * max(1, w - 2) + ":")
        lines.append("| " + " | ".join(delims) + " |")

        # Data rows
        for row, raw_row in zip(self.rows, self.raw_rows):
            cells = [
                self._align_cell(c, rc, w, a)
                for c, rc, w, a in zip(row, raw_row, widths, self.alignments)
            ]
            lines.append("| " + " | ".join(cells) + " |")

        # Summary rows
        if self.summary_rows:
            for row, raw_row in zip(self.summary_rows, self.raw_summary_rows):
                cells = [
                    self._align_cell(c, rc, w, a)
                    for c, rc, w, a in zip(row, raw_row, widths, self.alignments)
                ]
                lines.append("| " + " | ".join(cells) + " |")

        return "\n".join(lines)

    def _render_simple(self) -> str:
        widths = self._calculate_widths()
        lines = []

        header_cells = [
            self._align_cell(h, h, w, a)
            for h, w, a in zip(self.headers, widths, self.alignments)
        ]
        lines.append("  ".join(header_cells))
        lines.append("  ".join("-" * w for w in widths))

        for row, raw_row in zip(self.rows, self.raw_rows):
            cells = [
                self._align_cell(c, rc, w, a)
                for c, rc, w, a in zip(row, raw_row, widths, self.alignments)
            ]
            lines.append("  ".join(cells))

        if self.summary_rows:
            lines.append("  ".join("=" * w for w in widths))
            for row, raw_row in zip(self.summary_rows, self.raw_summary_rows):
                cells = [
                    self._align_cell(c, rc, w, a)
                    for c, rc, w, a in zip(row, raw_row, widths, self.alignments)
                ]
                lines.append("  ".join(cells))

        return "\n".join(lines)

    def _render_delimited(self, delimiter: str) -> str:
        out = io.StringIO()
        writer = csv.writer(out, delimiter=delimiter)
        writer.writerow(self.headers)
        for raw_row in self.raw_rows:
            writer.writerow(raw_row)
        for raw_row in self.raw_summary_rows:
            writer.writerow(raw_row)
        return out.getvalue().strip()


def format_speedup(speedup: Optional[float], use_color: bool = False, precision: int = 2) -> Tuple[str, str]:
    """Format speedup value as string and raw string, with optional ANSI color."""
    if speedup is None:
        return ("*", "*")
    raw = f"{speedup:.{precision}f}x"
    if not use_color:
        return (raw, raw)
    if speedup >= 1.005:
        return (colorize(raw, Color.GREEN, True), raw)
    elif speedup <= 0.995:
        return (colorize(raw, Color.RED, True), raw)
    else:
        return (colorize(raw, Color.DIM, True), raw)


def format_diff(diff: Optional[float], use_color: bool = False, precision: int = 1) -> Tuple[str, str]:
    """Format percentage difference ((Base / Test - 1) * 100)."""
    if diff is None:
        return ("*", "*")
    raw = f"{diff:+.{precision}f}%"
    if not use_color:
        return (raw, raw)
    if diff >= 0.5:
        return (colorize(raw, Color.GREEN, True), raw)
    elif diff <= -0.5:
        return (colorize(raw, Color.RED, True), raw)
    else:
        return (colorize(raw, Color.DIM, True), raw)


def compute_geomean(values: List[float]) -> float:
    """Compute geometric mean of a list of positive floats."""
    if not values:
        return 0.0
    valid = [v for v in values if v > 0]
    if not valid:
        return 0.0
    return math.exp(sum(math.log(v) for v in valid) / len(valid))


def build_comparison_table(
    benchmarks: List[str],
    by_bench: Dict[str, Dict[str, Dict[str, Any]]],
    base_comp: str,
    test_comp: str,
    metric_key: str = "runTime",
    metric_unit: str = "s",
    sort_by: str = "default",
    reverse_sort: bool = False,
    table_format: str = "ascii",
    use_color: bool = False,
    precision: int = 4,
    speedup_precision: int = 2,
    diff_precision: int = 1,
) -> Tuple[str, Dict[str, Any]]:
    """
    Build ASCII/Markdown/CSV comparison table for a given metric between base and test compilers.
    """
    is_size = metric_key == "binarySize"
    headers = ["Benchmark", f"{base_comp} ({metric_unit})", f"{test_comp} ({metric_unit})", "Speedup", "% Diff"]
    alignments = ["<", ">", ">", ">", ">"]

    # Gather rows
    row_data = []
    for bench in benchmarks:
        comps = by_bench.get(bench, {})
        base_rec = comps.get(base_comp, {})
        test_rec = comps.get(test_comp, {})

        base_val = base_rec.get(metric_key)
        test_val = test_rec.get(metric_key)

        speedup: Optional[float] = None
        diff: Optional[float] = None

        if base_val is not None and test_val is not None and test_val > 0 and base_val > 0:
            speedup = base_val / test_val
            diff = (speedup - 1.0) * 100.0

        row_data.append({
            "bench": bench,
            "base_val": base_val,
            "test_val": test_val,
            "speedup": speedup,
            "diff": diff,
        })

    # Sorting
    if sort_by in ("speedup", "ratio", "sp"):
        row_data.sort(key=lambda r: (r["speedup"] is not None, r["speedup"] or 0), reverse=not reverse_sort)
    elif sort_by in ("speedup-asc", "sp-asc"):
        row_data.sort(key=lambda r: (r["speedup"] is None, r["speedup"] or 0), reverse=reverse_sort)
    elif sort_by in ("diff", "delta"):
        row_data.sort(key=lambda r: (r["diff"] is not None, r["diff"] or 0), reverse=not reverse_sort)
    elif sort_by in ("diff-asc", "delta-asc"):
        row_data.sort(key=lambda r: (r["diff"] is None, r["diff"] or 0), reverse=reverse_sort)
    elif sort_by in ("base", "base-val", "base-time"):
        row_data.sort(key=lambda r: (r["base_val"] is not None, r["base_val"] or 0), reverse=not reverse_sort)
    elif sort_by in ("test", "test-val", "test-time"):
        row_data.sort(key=lambda r: (r["test_val"] is not None, r["test_val"] or 0), reverse=not reverse_sort)
    elif sort_by in ("alpha", "name"):
        row_data.sort(key=lambda r: r["bench"], reverse=reverse_sort)
    else:  # 'default', 'suite', 'bench' -> canonical suite order
        row_data.sort(key=lambda r: (BENCHMARK_ORDER_MAP.get(r["bench"], 999), r["bench"]), reverse=reverse_sort)

    table = TableFormatter(headers, alignments, format_type=table_format, use_color=use_color)

    # Valid values for summary stats
    valid_speedups = [r["speedup"] for r in row_data if r["speedup"] is not None]
    valid_diffs = [r["diff"] for r in row_data if r["diff"] is not None]
    valid_base = [r["base_val"] for r in row_data if r["base_val"] is not None]
    valid_test = [r["test_val"] for r in row_data if r["test_val"] is not None]

    faster_count = sum(1 for s in valid_speedups if s >= 1.005)
    slower_count = sum(1 for s in valid_speedups if s <= 0.995)
    same_count = len(valid_speedups) - faster_count - slower_count

    for r in row_data:
        bench = r["bench"]
        base_val = r["base_val"]
        test_val = r["test_val"]

        if is_size:
            base_str = f"{int(base_val):,}" if base_val is not None else "*"
            test_str = f"{int(test_val):,}" if test_val is not None else "*"
        else:
            base_str = f"{base_val:.{precision}f}" if base_val is not None else "*"
            test_str = f"{test_val:.{precision}f}" if test_val is not None else "*"

        sp_fmt, sp_raw = format_speedup(r["speedup"], use_color=use_color, precision=speedup_precision)
        diff_fmt, diff_raw = format_diff(r["diff"], use_color=use_color, precision=diff_precision)

        table.add_row(
            [bench, base_str, test_str, sp_fmt, diff_fmt],
            [bench, base_str, test_str, sp_raw, diff_raw],
        )

    stats = {
        "count": len(valid_speedups),
        "total_benchmarks": len(row_data),
        "faster": faster_count,
        "slower": slower_count,
        "same": same_count,
        "geomean_speedup": None,
        "geomean_diff": None,
        "avg_speedup": None,
        "median_speedup": None,
        "min_speedup": None,
        "max_speedup": None,
    }

    if valid_speedups:
        geomean_sp = compute_geomean(valid_speedups)
        geomean_df = (geomean_sp - 1.0) * 100.0
        avg_sp = statistics.mean(valid_speedups)
        avg_df = (avg_sp - 1.0) * 100.0
        med_sp = statistics.median(valid_speedups)
        med_df = (med_sp - 1.0) * 100.0
        min_sp = min(valid_speedups)
        min_df = (min_sp - 1.0) * 100.0
        max_sp = max(valid_speedups)
        max_df = (max_sp - 1.0) * 100.0

        stats.update({
            "geomean_speedup": geomean_sp,
            "geomean_diff": geomean_df,
            "avg_speedup": avg_sp,
            "median_speedup": med_sp,
            "min_speedup": min_sp,
            "max_speedup": max_sp,
        })

        if is_size:
            avg_base = f"{int(statistics.mean(valid_base)):,}" if valid_base else "-"
            avg_test = f"{int(statistics.mean(valid_test)):,}" if valid_test else "-"
            med_base = f"{int(statistics.median(valid_base)):,}" if valid_base else "-"
            med_test = f"{int(statistics.median(valid_test)):,}" if valid_test else "-"
            min_base = f"{int(min(valid_base)):,}" if valid_base else "-"
            min_test = f"{int(min(valid_test)):,}" if valid_test else "-"
            max_base = f"{int(max(valid_base)):,}" if valid_base else "-"
            max_test = f"{int(max(valid_test)):,}" if valid_test else "-"
        else:
            avg_base = f"{statistics.mean(valid_base):.{precision}f}" if valid_base else "-"
            avg_test = f"{statistics.mean(valid_test):.{precision}f}" if valid_test else "-"
            med_base = f"{statistics.median(valid_base):.{precision}f}" if valid_base else "-"
            med_test = f"{statistics.median(valid_test):.{precision}f}" if valid_test else "-"
            min_base = f"{min(valid_base):.{precision}f}" if valid_base else "-"
            min_test = f"{min(valid_test):.{precision}f}" if valid_test else "-"
            max_base = f"{max(valid_base):.{precision}f}" if valid_base else "-"
            max_test = f"{max(valid_test):.{precision}f}" if valid_test else "-"

        # Summary rows
        geo_sp_fmt, geo_sp_raw = format_speedup(geomean_sp, use_color=use_color, precision=speedup_precision)
        geo_df_fmt, geo_df_raw = format_diff(geomean_df, use_color=use_color, precision=diff_precision)
        avg_sp_fmt, avg_sp_raw = format_speedup(avg_sp, use_color=use_color, precision=speedup_precision)
        avg_df_fmt, avg_df_raw = format_diff(avg_df, use_color=use_color, precision=diff_precision)
        med_sp_fmt, med_sp_raw = format_speedup(med_sp, use_color=use_color, precision=speedup_precision)
        med_df_fmt, med_df_raw = format_diff(med_df, use_color=use_color, precision=diff_precision)
        min_sp_fmt, min_sp_raw = format_speedup(min_sp, use_color=use_color, precision=speedup_precision)
        min_df_fmt, min_df_raw = format_diff(min_df, use_color=use_color, precision=diff_precision)
        max_sp_fmt, max_sp_raw = format_speedup(max_sp, use_color=use_color, precision=speedup_precision)
        max_df_fmt, max_df_raw = format_diff(max_df, use_color=use_color, precision=diff_precision)

        lbl_geo = colorize("GEOMEAN", Color.BOLD, use_color)
        lbl_avg = colorize("AVERAGE", Color.BOLD, use_color)
        lbl_med = colorize("MEDIAN", Color.BOLD, use_color)
        lbl_min = colorize("MIN", Color.BOLD, use_color)
        lbl_max = colorize("MAX", Color.BOLD, use_color)

        table.add_summary_row(
            [lbl_geo, "-", "-", geo_sp_fmt, geo_df_fmt],
            ["GEOMEAN", "-", "-", geo_sp_raw, geo_df_raw],
        )
        table.add_summary_row(
            [lbl_avg, avg_base, avg_test, avg_sp_fmt, avg_df_fmt],
            ["AVERAGE", avg_base, avg_test, avg_sp_raw, avg_df_raw],
        )
        table.add_summary_row(
            [lbl_med, med_base, med_test, med_sp_fmt, med_df_fmt],
            ["MEDIAN", med_base, med_test, med_sp_raw, med_df_raw],
        )
        table.add_summary_row(
            [lbl_min, min_base, min_test, min_sp_fmt, min_df_fmt],
            ["MIN", min_base, min_test, min_sp_raw, min_df_raw],
        )
        table.add_summary_row(
            [lbl_max, max_base, max_test, max_sp_fmt, max_df_fmt],
            ["MAX", max_base, max_test, max_sp_raw, max_df_raw],
        )

    return table.render(), stats


def list_outputs(outputs_dir: Path) -> None:
    """List all available output files in outputs_dir with summary info."""
    if not outputs_dir.is_dir():
        print(f"Outputs directory not found: {outputs_dir}", file=sys.stderr)
        return

    files = list(outputs_dir.glob("*.jsonl"))
    if not files:
        print(f"No .jsonl files found in {outputs_dir}")
        return

    sorted_files = sorted(files, key=get_file_timestamp)
    print(f"Available benchmark output files in {outputs_dir} ({len(sorted_files)} total):\n")
    print(f"{'Filename':<75} {'Records':<8} {'Modified'}")
    print("-" * 110)
    for f in sorted_files:
        records = parse_jsonl(f)
        mtime = datetime.fromtimestamp(f.stat().st_mtime).strftime("%Y-%m-%d %H:%M:%S")
        print(f"{f.name:<75} {len(records):<8} {mtime}")


def main():
    parser = argparse.ArgumentParser(
        description="Print MLton benchmark results in an ASCII table showing relative speedup.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # View speedup table for the most recent benchmark run
  ./print_speedup.py

  # View a specific output file by name or path
  ./print_speedup.py outputs/test_aos_flatten_mlton_mlton_o3:flattening-tests:fd9a06c:20260825_044217.jsonl
  ./print_speedup.py test_conapp_flatten_mlton_mlton_o3

  # Sort by highest speedup first
  ./print_speedup.py --sort speedup

  # Filter benchmarks matching a regex
  ./print_speedup.py --benchmark '(fib|tak|vector|wc)'

  # Compare compile time or binary size
  ./print_speedup.py --metric compile
  ./print_speedup.py --metric size
  ./print_speedup.py --metric all

  # Output in Markdown or CSV format
  ./print_speedup.py --format markdown
  ./print_speedup.py --format csv

  # List all available benchmark runs
  ./print_speedup.py --list
""",
    )

    parser.add_argument(
        "file",
        nargs="?",
        default=None,
        help="Path or name of the JSONL benchmark output file (default: most recent file in outputs/)",
    )
    parser.add_argument(
        "-i",
        "--input",
        "--file",
        "-f",
        dest="input_file",
        default=None,
        help="Alternative way to specify input file path or name",
    )
    parser.add_argument(
        "--outputs-dir",
        default=None,
        help="Directory containing output JSONL files (default: benchmark-new/outputs)",
    )
    parser.add_argument(
        "-m",
        "--metric",
        default="runtime",
        choices=["runtime", "run", "compile", "compile_time", "size", "binary_size", "all"],
        help="Metric to compare: 'runtime' (default), 'compile', 'size', or 'all'",
    )
    parser.add_argument(
        "-b",
        "--benchmark",
        "--filter",
        dest="benchmark_filter",
        default=".*",
        help="Regex pattern to filter benchmarks (default: '.*')",
    )
    parser.add_argument(
        "-s",
        "--sort",
        default="default",
        choices=["default", "suite", "bench", "name", "alpha", "speedup", "ratio", "sp", "speedup-asc", "sp-asc", "diff", "delta", "diff-asc", "delta-asc", "base", "test"],
        help="Sort rows by: 'default' (benchmark suite order), 'name' (alphabetical), 'speedup' (descending), 'speedup-asc', 'diff', 'base', or 'test'",
    )
    parser.add_argument(
        "-r",
        "--reverse",
        action="store_true",
        help="Reverse sort order",
    )
    parser.add_argument(
        "--base",
        default=None,
        help="Compiler abbreviation to use as baseline (default: first compiler, e.g. MLton0)",
    )
    parser.add_argument(
        "--test",
        default=None,
        help="Compiler abbreviation to use as test (default: second compiler, e.g. MLton1)",
    )
    parser.add_argument(
        "--format",
        default="ascii",
        choices=["ascii", "grid", "markdown", "md", "plain", "simple", "csv", "tsv"],
        help="Table output format: 'ascii' (default), 'markdown', 'plain', 'csv', 'tsv'",
    )
    parser.add_argument(
        "--color",
        dest="use_color",
        action="store_true",
        default=None,
        help="Force enable ANSI color highlighting",
    )
    parser.add_argument(
        "--no-color",
        dest="no_color",
        action="store_true",
        help="Disable ANSI color highlighting",
    )
    parser.add_argument(
        "-q",
        "--quiet",
        action="store_true",
        help="Suppress metadata header and print only table/results",
    )
    parser.add_argument(
        "-l",
        "--list",
        action="store_true",
        help="List available output files and exit",
    )
    parser.add_argument(
        "-p",
        "--precision",
        type=int,
        default=4,
        help="Decimal precision for time values (default: 4)",
    )

    args = parser.parse_args()

    # Determine outputs directory
    if args.outputs_dir:
        outputs_dir = Path(args.outputs_dir).resolve()
    else:
        outputs_dir = find_default_outputs_dir()

    if args.list:
        list_outputs(outputs_dir)
        sys.exit(0)

    # Resolve color usage
    if args.no_color or args.format in ("csv", "tsv"):
        use_color = False
    elif args.use_color is not None:
        use_color = args.use_color
    else:
        use_color = sys.stdout.isatty()

    target_file = args.input_file or args.file
    try:
        input_path = resolve_input_file(target_file, outputs_dir)
    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

    records = parse_jsonl(input_path)
    if not records:
        print(f"Error: No valid JSON records found in {input_path}", file=sys.stderr)
        sys.exit(1)

    # Group records by benchmark and compiler
    by_bench: Dict[str, Dict[str, Dict[str, Any]]] = {}
    compilers: Dict[str, str] = {}
    timestamps: List[str] = []
    hostnames: List[str] = []
    commits: List[str] = []

    for r in records:
        b = r.get("bench")
        c = r.get("compilerAbbrev")
        if not b or not c:
            continue
        compilers[c] = r.get("cmd", "")
        by_bench.setdefault(b, {})[c] = r
        if r.get("timestamp"):
            timestamps.append(r["timestamp"])
        if r.get("hostname"):
            hostnames.append(r["hostname"])
        if r.get("commitHash"):
            commits.append(r["commitHash"])

    compiler_list = sorted(list(compilers.keys()))
    if not compiler_list:
        print("Error: No compiler records found in file.", file=sys.stderr)
        sys.exit(1)

    base_comp = args.base or ("MLton0" if "MLton0" in compilers else compiler_list[0])
    if args.test:
        test_comp = args.test
    else:
        test_candidates = [c for c in compiler_list if c != base_comp]
        test_comp = test_candidates[0] if test_candidates else base_comp

    # Benchmark regex filtering
    try:
        bench_re = re.compile(args.benchmark_filter)
    except re.error as e:
        print(f"Error: Invalid benchmark filter regex '{args.benchmark_filter}': {e}", file=sys.stderr)
        sys.exit(1)

    all_benchmarks = [b for b in by_bench.keys() if bench_re.search(b)]
    if not all_benchmarks:
        print(f"Error: No benchmarks matched filter '{args.benchmark_filter}'.", file=sys.stderr)
        sys.exit(1)

    # Print metadata header if not quiet and not CSV/TSV
    if not args.quiet and args.format not in ("csv", "tsv"):
        rel_path = input_path
        try:
            rel_path = input_path.relative_to(Path.cwd())
        except ValueError:
            pass

        title = "Benchmark Speedup Results"
        if use_color:
            print(colorize(title, Color.BOLD + Color.CYAN))
            print(colorize("=" * len(title), Color.CYAN))
        else:
            print(title)
            print("=" * len(title))

        print(f"File:        {rel_path}")
        if timestamps:
            print(f"Timestamp:   {timestamps[0]}")
        if hostnames:
            print(f"Host:        {hostnames[0]}")
        if commits:
            print(f"Git Commit:  {commits[0][:10]}")
        print(f"Base ({base_comp}): {compilers.get(base_comp, '(none)')}")
        print(f"Test ({test_comp}): {compilers.get(test_comp, '(none)')}")
        print()

    # Determine which metrics to display
    metrics_to_run = []
    if args.metric in ("runtime", "run"):
        metrics_to_run.append(("runTime", "s", "Execution Run Time"))
    elif args.metric in ("compile", "compile_time"):
        metrics_to_run.append(("compileTime", "s", "Compilation Time"))
    elif args.metric in ("size", "binary_size"):
        metrics_to_run.append(("binarySize", "B", "Binary Size"))
    elif args.metric == "all":
        metrics_to_run.append(("runTime", "s", "Execution Run Time"))
        metrics_to_run.append(("compileTime", "s", "Compilation Time"))
        metrics_to_run.append(("binarySize", "B", "Binary Size"))

    for idx, (m_key, m_unit, m_title) in enumerate(metrics_to_run):
        if len(metrics_to_run) > 1 and args.format not in ("csv", "tsv"):
            if idx > 0:
                print("\n")
            section_title = f"--- {m_title} ---"
            if use_color:
                print(colorize(section_title, Color.BOLD + Color.YELLOW))
            else:
                print(section_title)

        table_str, stats = build_comparison_table(
            benchmarks=all_benchmarks,
            by_bench=by_bench,
            base_comp=base_comp,
            test_comp=test_comp,
            metric_key=m_key,
            metric_unit=m_unit,
            sort_by=args.sort,
            reverse_sort=args.reverse,
            table_format=args.format,
            use_color=use_color,
            precision=args.precision,
        )

        print(table_str)

        # Print summary line for ASCII/Markdown formats
        if not args.quiet and args.format not in ("csv", "tsv") and stats["count"] > 0:
            geomean_sp = stats["geomean_speedup"]
            geomean_df = stats["geomean_diff"]
            summary_txt = (
                f"\nSummary: {stats['count']} benchmarks | "
                f"Faster: {stats['faster']} | "
                f"Slower: {stats['slower']} | "
                f"Unchanged: {stats['same']} (within ±0.5%) | "
                f"GeoMean Speedup: {geomean_sp:.4f}x ({geomean_df:+.2f}%)"
            )
            if use_color:
                print(colorize(summary_txt, Color.BOLD))
            else:
                print(summary_txt)


if __name__ == "__main__":
    main()
