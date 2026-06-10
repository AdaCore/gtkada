"""
Default test driver for GtkAda. The expectation is that each test will contain
one and only one ".gpr" file, defining a main which is supposed to run
Glib.Test.Init / Glib.Test.Run.

As a minimal example, see tests/main/.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
import json
from pathlib import Path

from e3.testsuite.driver.classic import (
    ClassicTestDriver,
    TestAbortWithFailure,
)


class DefaultScriptDriver(ClassicTestDriver):

    def run(self) -> None:
        env = dict(os.environ)
        gtkada_root = Path(__file__).resolve().parent.parent.parent
        working_dir = self.test_env["working_dir"]

        # Set up a headless-friendly GTK environment, mirroring GTK's own
        # testsuite setup. Without GTK_A11Y=none, tests that create widgets
        # abort when the accessibility bus cannot be reached. The other
        # settings keep tests from touching the user's real configuration.
        env.setdefault("GTK_A11Y", "none")
        env.setdefault("GSETTINGS_BACKEND", "memory")
        env.setdefault("GIO_USE_VFS", "local")
        # Xvfb has no GL stack; without this, realizing a window makes GSK
        # probe for a GL renderer and crash inside libepoxy when no GL
        # library can be loaded.
        env.setdefault("GSK_RENDERER", "cairo")

        env["GPR_PROJECT_PATH"] = (
            str(gtkada_root / "src") + os.pathsep + env.get("GPR_PROJECT_PATH", "")
        )

        # Find the ".gpr" file in the working directory
        gpr_files = list(Path(working_dir).glob("*.gpr"))
        if not gpr_files:
            msg = f"No .gpr file found in {working_dir}"
            self.result.log += msg
            raise TestAbortWithFailure(msg)
        if len(gpr_files) > 1:
            msg = f"Multiple .gpr files found in {working_dir}: {gpr_files}"
            self.result.log += msg
            raise TestAbortWithFailure(msg)
        gpr = gpr_files[0]

        # Launch "gprinspect" to find the mains
        p = subprocess.run(
            ["gprinspect", "-P", gpr.name, "--display=json", "--attributes"],
            # Use the gpr2 gprinspect to get the json dump
            env=env | {"GNAT_GPR_ENGINE": "2"},
            cwd=working_dir,
            capture_output=True,
            text=True,
        )
        j_dict = json.loads(p.stdout)
        try:
            mains = [
                x["values"]
                for x in j_dict["projects"][0]["attributes"]
                if x["name"] == "Main"
            ][0]
            # Strip the suffix if present (e.g. "main.adb" -> "main")
            mains = [Path(main).stem for main in mains]
            # Add ".exe" suffix on Windows if not present
            if sys.platform == "win32":
                mains = [
                    main if main.endswith(".exe") else main + ".exe" for main in mains
                ]
        except KeyError:
            msg = "Failed to find mains in gprinspect output"
            self.result.log += msg
            raise TestAbortWithFailure(msg)

        # Also find where the executables will be placed
        executables_dir = None
        for x in j_dict["projects"][0]["attributes"]:
            if x["name"] == "Exec_Dir":
                executables_dir = Path(working_dir) / x["value"]
                break
        if executables_dir is None:
            for x in j_dict["projects"][0]["attributes"]:
                if x["name"] == "Object_Dir":
                    executables_dir = Path(working_dir) / x["value"]
                    break
        if executables_dir is None:
            executables_dir = Path(working_dir)

        # The gprbuild command line
        gprbuild_cl = ["gprbuild", "-P", str(gpr), "-j0", "-g", "-O0"]

        is_coverage = "GNATCOV_TRACE_FILE" in env
        if is_coverage:
            gnatcov_rts_gpr = gtkada_root / "obj/gnatcov-rts/share/gpr/gnatcov_rts.gpr"

            # Protection against each test wanting to rebuild an instrumented GtkAda
            env["GTKADA_EXTERNALLY_BUILT"] = "yes"

            gprbuild_cl.extend(
                [
                    "--src-subdirs=gnatcov-instr",
                    f"--implicit-with={gnatcov_rts_gpr}",
                    "-XLIBRARY_TYPE=static",
                ]
            )

            gnatcov_instrument_cl = [
                "gnatcov",
                "instrument",
                "-P",
                str(gpr),
                "--level=stmt",
                "--externally-built-projects",
                "--projects=gtkada",
                "-XLIBRARY_TYPE=static",
                "--runtime-project",
                str(gnatcov_rts_gpr),
            ]

            p = subprocess.run(
                gnatcov_instrument_cl,
                env=env,
                cwd=self.test_env["working_dir"],
                capture_output=True,
                text=True,
            )
            if p.returncode != 0:
                print("Running:", " ".join(gnatcov_instrument_cl))
                msg = (
                    "gnatcov instrument failed with return code "
                    f"{p.returncode}\nstdout:\n{p.stdout}\nstderr:\n{p.stderr}"
                )
                self.result.log += msg
                raise TestAbortWithFailure(msg)

        p = subprocess.run(
            gprbuild_cl,
            env=env,
            cwd=self.test_env["working_dir"],
            capture_output=True,
            text=True,
        )
        if p.returncode != 0:
            print("Running:", " ".join(gprbuild_cl))
            msg = (
                "gprbuild failed with return code "
                f"{p.returncode}\nstdout:\n{p.stdout}\nstderr:\n{p.stderr}"
            )
            self.result.log += msg
            raise TestAbortWithFailure(msg)

        # GTK widgets need an X display. Give each test its own private virtual
        # display by wrapping the executable in "xvfb-run -a", which allocates a
        # free server number (so parallel tests don't collide), exports DISPLAY
        # for the child, and tears the server down when the child exits. On
        # Windows GTK needs no X server, so the wrapper is skipped there. If
        # xvfb-run is unavailable, we warn and fall back to the ambient DISPLAY.
        xvfb_log = Path(working_dir) / "xvfb.log"
        xvfb_run = None if sys.platform == "win32" else shutil.which("xvfb-run")
        if xvfb_run:
            run_prefix = [
                xvfb_run,
                "-a",
                "--server-args=-screen 0 1280x1024x24",
                "-e",
                str(xvfb_log),
            ]
        else:
            run_prefix = []
            if sys.platform != "win32":
                self.result.log += (
                    "Warning: xvfb-run not found; running against ambient "
                    f"DISPLAY={env.get('DISPLAY', '<unset>')}\n"
                )

        # The build succeeded: now run the executable(s)
        for main in mains:
            main_path = Path(executables_dir) / main
            if not main_path.exists():
                msg = f"Expected executable {main_path} does not exist after build"
                self.result.log += msg
                raise TestAbortWithFailure(msg)
            p = subprocess.run(
                [*run_prefix, str(main_path), "--keep-going"],
                env=env,
                cwd=self.test_env["working_dir"],
                capture_output=True,
                text=True,
            )
            if p.returncode != 0:
                msg = (
                    f"Executable {main} failed with return code"
                    f" {p.returncode}\nstdout:\n{p.stdout}\n"
                    f"stderr:\n{p.stderr}"
                )
                if xvfb_run and xvfb_log.exists():
                    msg += f"\nXvfb log:\n{xvfb_log.read_text()}"
                self.result.log += msg
                raise TestAbortWithFailure(msg)
