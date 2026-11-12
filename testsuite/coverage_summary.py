#! /usr/bin/env python

"""Summarize a gnatcov coverage report as a plain-text table.

Reads the Cobertura report that "make coverage-report" produces and prints
one row per source file, then the total: the lines that hold statements,
how many of them the testsuite covers and misses, and the percentage
covered. "make coverage-report" saves this in gnatcov_report/summary.txt.
See "Coverage" in dev_guide.md.
"""

from __future__ import annotations

import argparse
import math
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

#: Root of the GtkAda checkout: file names are shown relative to it.
GTKADA_ROOT = Path(__file__).resolve().parent.parent

#: Where "make coverage-report" puts the Cobertura report.
DEFAULT_REPORT = GTKADA_ROOT / "gnatcov_report" / "cobertura" / "cobertura.xml"


def file_stats(report: Path) -> dict[str, tuple[int, int]]:
    """Map each source file to its (lines with statements, covered lines)"""
    stats = {}
    for cls in ET.parse(report).getroot().iter("class"):
        hits = [int(line.get("hits")) for line in cls.findall("lines/line")]
        name = Path(cls.get("filename"))
        if name.is_relative_to(GTKADA_ROOT):
            name = name.relative_to(GTKADA_ROOT)
        stats[str(name)] = (len(hits), sum(1 for h in hits if h > 0))
    return stats


def percentage(covered: int, lines: int) -> str:
    """Format covered/lines, rounding down so that only 100% shows 100.0%"""
    if lines == 0:
        return "-"
    return f"{math.floor(1000 * covered / lines) / 10:.1f}%"


def summary(stats: dict[str, tuple[int, int]]) -> str:
    """Return the table for stats, sorted by file name, total last"""
    width = max(len("TOTAL"), *map(len, stats))

    def row(name: str, lines: int, covered: int) -> str:
        return (
            f"{name:<{width}}  {lines:>6}  {covered:>7}  {lines - covered:>6}"
            f"  {percentage(covered, lines):>6}"
        )

    header = (
        f"{'File':<{width}}  {'Lines':>6}  {'Covered':>7}  {'Missed':>6}"
        f"  {'Cover':>6}"
    )
    rule = "-" * len(header)
    total_lines = sum(lines for lines, _ in stats.values())
    total_covered = sum(covered for _, covered in stats.values())
    return "\n".join(
        [header, rule]
        + [row(name, *stats[name]) for name in sorted(stats)]
        + [rule, row("TOTAL", total_lines, total_covered)]
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "report",
        nargs="?",
        type=Path,
        default=DEFAULT_REPORT,
        help=f"The Cobertura report to read (default: {DEFAULT_REPORT})",
    )
    args = parser.parse_args()
    if not args.report.is_file():
        print(
            f"{args.report}: no such report; run"
            " 'make coverage-test; make coverage-report' first",
            file=sys.stderr,
        )
        return 1
    print(summary(file_stats(args.report)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
