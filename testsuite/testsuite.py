"""GtkAda testsuite runner"""

from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path
from typing import ClassVar

import e3.testsuite
from drivers.default_driver import DefaultScriptDriver
from drivers.library import LibraryBuildError, build_library

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
        group.add_argument(
            "--no-library-build",
            action="store_true",
            help="Do not build the GtkAda library before running the"
            " tests: use the one already in src/lib as it stands.",
        )

    def set_up(self) -> None:
        """Build the GtkAda library once, before any test runs.

        The tests are built with "-XGTKADA_EXTERNALLY_BUILT=yes" and so
        cannot build it for themselves -- deliberately, since they share
        the one library directory and would corrupt it building in
        parallel. See drivers/library.py.
        """
        super().set_up()

        if self.env.options.no_library_build:
            return

        if "GNATCOV_TRACE_FILE" in os.environ:
            # A coverage run has already built an instrumented library
            # ("coverage-build" in Makefile.in); building here would
            # replace it with an uninstrumented one.
            return

        print("====== Building the GtkAda library ======")
        try:
            build_library()
        except LibraryBuildError as exc:
            sys.exit(f"Could not build the GtkAda library:\n{exc}")


if __name__ == "__main__":
    sys.exit(GtkAdaTestsuite().testsuite_main())
