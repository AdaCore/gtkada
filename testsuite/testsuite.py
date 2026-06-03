"""GtkAda testsuite runner"""

from __future__ import annotations

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


if __name__ == "__main__":
    sys.exit(GtkAdaTestsuite().testsuite_main())
