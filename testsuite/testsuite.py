"""GtkAda testsuite runner"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import ClassVar

import e3.testsuite
from drivers.default_driver import DefaultScriptDriver

REPO_ROOT = Path(__file__).resolve().parent.parent.parent


class GtkAdaTestsuite(e3.testsuite.Testsuite):
    """GtkAda testsuite"""

    tests_subdir = "tests"
    test_driver_map: ClassVar = {"default-script": DefaultScriptDriver}
    default_driver = "default-script"

    def add_options(self, parser: argparse.ArgumentParser) -> None:
        """Add the GtkAda-specific switches"""
        group = parser.add_argument_group(
            title="gtkada", description="GtkAda-specific options"
        )
        group.add_argument(
            "--no-xvfb",
            action="store_true",
            help="Do not start a private Xvfb server for each test: run"
            " against the ambient DISPLAY instead.",
        )
        group.add_argument(
            "--display",
            help="Run the tests against this X display, for instance"
            " ':0' to watch a failing GTK test on your own screen."
            " Implies --no-xvfb.",
        )


if __name__ == "__main__":
    sys.exit(GtkAdaTestsuite().testsuite_main())
