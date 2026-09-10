"""Building the GtkAda library that the tests link against.

Tests are built with "-XGTKADA_EXTERNALLY_BUILT=yes" (see
default_driver.py), so none of them ever rebuilds the library. They have
to be kept from it: every test project withs the one src/gtkada.gpr and
would archive into the one src/lib/gtkada/<kind>/libgtkada.a, so a dozen
tests building in parallel write over each other's archive and the ones
that lose the race fail to link with "libgtkada.a: file format not
recognized".

The library is therefore built exactly once, from here, before any test
runs.
"""

from __future__ import annotations

import subprocess
from pathlib import Path

from drivers.default_driver import BUILD_TIMEOUT, as_text

#: Root of the GtkAda checkout.
GTKADA_ROOT = Path(__file__).resolve().parent.parent.parent

#: The library project that every test project withs.
LIBRARY_GPR = GTKADA_ROOT / "src" / "gtkada.gpr"


class LibraryBuildError(Exception):
    """Raised when the GtkAda library could not be built."""


def build_library() -> str:
    """Build the GtkAda library, and return the build command's output.

    No switch beyond "-p" is passed: the point is to produce exactly the
    library that "make" produces, so that a tree already built by hand is
    left alone rather than recompiled with different switches.

    :raise LibraryBuildError: If the library could not be built.
    """
    command_line = ["gprbuild", "-P", str(LIBRARY_GPR), "-j0", "-p"]
    try:
        p = subprocess.run(
            command_line,
            cwd=GTKADA_ROOT,
            capture_output=True,
            text=True,
            timeout=BUILD_TIMEOUT,
        )
    except subprocess.TimeoutExpired as exc:
        raise LibraryBuildError(
            f"{' '.join(command_line)} timed out after {BUILD_TIMEOUT}s\n"
            f"stdout:\n{as_text(exc.stdout)}\n"
            f"stderr:\n{as_text(exc.stderr)}"
        )

    if p.returncode != 0:
        raise LibraryBuildError(
            f"{' '.join(command_line)} failed with return code "
            f"{p.returncode}\nstdout:\n{p.stdout}\nstderr:\n{p.stderr}"
        )

    return p.stdout
