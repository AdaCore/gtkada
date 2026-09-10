"""
Default test driver for GtkAda. The expectation is that each test will contain
one and only one ".gpr" file, defining a main which is supposed to run
Glib.Test.Init / Glib.Test.Run.

As a minimal example, see tests/main/.
"""

from __future__ import annotations

import os
import subprocess
import sys
import json
from contextlib import AbstractContextManager, nullcontext
from pathlib import Path

from e3.testsuite.driver.classic import (
    ClassicTestDriver,
    TestAbortWithFailure,
)

from drivers.xvfb import XvfbError, find_xvfb, private_display

#: Timeout for the build and project inspection tools, in seconds. Kept
#: deliberately generous -- unlike the tests themselves, a cold build of the
#: library takes minutes, and several times that when a dozen of them
#: contend for the machine. The point is only that a wedged tool should fail
#: its own test rather than hang the whole run.
BUILD_TIMEOUT = 30 * 60

#: Default timeout for one test executable, in seconds.
DEFAULT_TEST_TIMEOUT = 60


def as_text(output: bytes | str | None) -> str:
    """Decode the partial output attached to a subprocess.TimeoutExpired.

    Needed because subprocess does not honour text mode on that path: the
    output comes back as bytes, or missing altogether.

    :param output: The exception's "stdout" or "stderr" attribute.
    """
    if output is None:
        return ""
    if isinstance(output, bytes):
        return output.decode("utf-8", errors="replace")
    return output


class DefaultScriptDriver(ClassicTestDriver):

    @property
    def default_process_timeout(self) -> int:
        """Return the default timeout for the processes this driver spawns.

        A GTK test that wedges -- on a display that went away, say, or on a
        dialog that never comes up -- must fail rather than hang the whole
        testsuite. One minute is ample here: no test takes more than a few
        seconds. Individual tests can raise this with "timeout:" in
        test.yaml.
        """
        return self.test_env.get("timeout", DEFAULT_TEST_TIMEOUT)

    def x_display(self, xvfb_log: Path) -> AbstractContextManager[str | None]:
        """Return a context manager yielding the DISPLAY to run a main on.

        Unless the testsuite was passed --no-xvfb or --display, this is a
        private Xvfb server, live for the duration of the context.

        :param xvfb_log: File the server's output is appended to.
        """
        options = self.testsuite_options
        if options.display:
            return nullcontext(options.display)
        if options.no_xvfb:
            return nullcontext(None)
        if find_xvfb() is None and sys.platform != "win32":
            self.result.log += (
                "Warning: Xvfb not found; falling back to the ambient DISPLAY\n"
            )
        return private_display(xvfb_log)

    def build_step(
        self, command_line: list[str], env: dict[str, str]
    ) -> subprocess.CompletedProcess[str]:
        """Run one build or project inspection command, under a timeout.

        :param command_line: The command to run, in the test directory.
        :param env: Environment to run it in.
        :raise TestAbortWithFailure: If the command did not complete in time.
        """
        try:
            return subprocess.run(
                command_line,
                env=env,
                cwd=self.test_env["working_dir"],
                capture_output=True,
                text=True,
                timeout=BUILD_TIMEOUT,
            )
        except subprocess.TimeoutExpired as exc:
            msg = (
                f"{command_line[0]} timed out after {BUILD_TIMEOUT}s\n"
                f"stdout:\n{as_text(exc.stdout)}\n"
                f"stderr:\n{as_text(exc.stderr)}"
            )
            self.result.log += msg
            raise TestAbortWithFailure(msg)

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
        p = self.build_step(
            ["gprinspect", "-P", gpr.name, "--display=json", "--attributes"],
            # Use the gpr2 gprinspect to get the json dump
            env=env | {"GNAT_GPR_ENGINE": "2"},
        )
        try:
            j_dict = json.loads(p.stdout)
        except json.JSONDecodeError:
            msg = (
                "Failed to parse gprinspect output (return code "
                f"{p.returncode})\nstdout:\n{p.stdout}\nstderr:\n{p.stderr}"
            )
            self.result.log += msg
            raise TestAbortWithFailure(msg)
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

            p = self.build_step(gnatcov_instrument_cl, env)
            if p.returncode != 0:
                print("Running:", " ".join(gnatcov_instrument_cl))
                msg = (
                    "gnatcov instrument failed with return code "
                    f"{p.returncode}\nstdout:\n{p.stdout}\nstderr:\n{p.stderr}"
                )
                self.result.log += msg
                raise TestAbortWithFailure(msg)

        p = self.build_step(gprbuild_cl, env)
        if p.returncode != 0:
            print("Running:", " ".join(gprbuild_cl))
            msg = (
                "gprbuild failed with return code "
                f"{p.returncode}\nstdout:\n{p.stdout}\nstderr:\n{p.stderr}"
            )
            self.result.log += msg
            raise TestAbortWithFailure(msg)

        # GTK widgets need an X display, and each main gets its own private
        # one: see drivers/xvfb.py for how, and why not "xvfb-run".
        xvfb_log = Path(working_dir) / "xvfb.log"

        def with_xvfb_log(msg: str) -> str:
            """Append the server's log to a failure message, if there is one"""
            if xvfb_log.exists():
                return f"{msg}\nXvfb log:\n{xvfb_log.read_text()}"
            return msg

        # The build succeeded: now run the executable(s)
        for main in mains:
            main_path = Path(executables_dir) / main
            if not main_path.exists():
                msg = f"Expected executable {main_path} does not exist after build"
                self.result.log += msg
                raise TestAbortWithFailure(msg)

            timeout = self.default_process_timeout
            try:
                # One server per main, matching this loop's granularity: a
                # main that brings its display down cannot then affect the
                # next one.
                with self.x_display(xvfb_log) as display:
                    if display is not None:
                        env["DISPLAY"] = display
                    self.result.log += (
                        f"Running {main} on DISPLAY="
                        f"{env.get('DISPLAY', '<unset>')}\n"
                    )

                    p = subprocess.run(
                        [str(main_path), "--keep-going"],
                        env=env,
                        cwd=self.test_env["working_dir"],
                        capture_output=True,
                        text=True,
                        timeout=timeout,
                    )
            except XvfbError as exc:
                msg = f"Could not start a virtual display for {main}: {exc}"
                self.result.log += with_xvfb_log(msg)
                raise TestAbortWithFailure(msg)
            except subprocess.TimeoutExpired as exc:
                msg = with_xvfb_log(
                    f"Executable {main} timed out after {timeout}s\n"
                    f"stdout:\n{as_text(exc.stdout)}\n"
                    f"stderr:\n{as_text(exc.stderr)}"
                )
                self.result.log += msg
                raise TestAbortWithFailure(msg)

            if p.returncode != 0:
                msg = with_xvfb_log(
                    f"Executable {main} failed with return code"
                    f" {p.returncode}\nstdout:\n{p.stdout}\n"
                    f"stderr:\n{p.stderr}"
                )
                self.result.log += msg
                raise TestAbortWithFailure(msg)
