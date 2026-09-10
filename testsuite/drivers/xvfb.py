"""
Private Xvfb servers for the GtkAda testsuite.

GTK tests need an X display, and parallel tests must not share one: two
tests on the same display interfere through grabs, focus and the clipboard,
and whichever of them finishes first tears the server down under the other.

The obvious tool for the job, "xvfb-run -a", cannot give us that. It picks a
display number by scanning for a free "/tmp/.X<n>-lock" file, but that lock
is created by Xvfb rather than by the wrapper, so between the moment
"xvfb-run" settles on a number and the moment its server claims it, every
other concurrently starting wrapper picks the very same number. One server
wins the race and the losers die with "Server is already active for display
N" -- or, worse, pass the wrapper's flawed liveness check and hand the test
a display it does not own.

We therefore drive Xvfb ourselves and let *it* allocate the number, via
"-displayfd": given no explicit display number, the server walks the
candidates itself, claiming each lock file with O_EXCL, and writes back the
number it ended up with. Check and claim happen in the same process under
one atomic operation, so the race cannot occur; as a bonus the number
arrives only once the server is ready to accept connections, which also
rules out connecting to a server that is not up yet.
"""

from __future__ import annotations

import os
import select
import shutil
import subprocess
import sys
import time
from contextlib import contextmanager
from pathlib import Path
from typing import Iterator

#: Geometry of the single screen offered by our servers.
SCREEN_GEOMETRY = "1280x1024x24"

#: How long to wait for a server to report its display number. Startup takes
#: less than a tenth of a second on an idle machine, but a testsuite running
#: several gprbuilds in parallel can starve it for a good deal longer.
STARTUP_TIMEOUT = 30.0

#: How many times to try spawning a server. Display collisions cannot happen
#: any more, so this covers only genuinely transient failures: keep it small.
MAX_ATTEMPTS = 3

#: How long to give a server to exit, before killing it or giving up on
#: collecting its exit status.
SHUTDOWN_TIMEOUT = 5.0


class XvfbError(Exception):
    """Raised when no private Xvfb server could be started."""


def find_xvfb() -> str | None:
    """Return the path to the Xvfb binary, or None if there is none to use.

    None on Windows, where GTK needs no X server at all, and None when the
    binary is simply not installed.
    """
    if sys.platform == "win32":
        return None
    return shutil.which("Xvfb")


@contextmanager
def private_display(log_file: Path) -> Iterator[str | None]:
    """Run a private Xvfb server and yield its DISPLAY value.

    The server is shut down when the context exits.

    Yield None when Xvfb is unavailable (on Windows, where GTK needs no X
    server, or when the binary is simply not installed), in which case the
    caller should fall back to the ambient DISPLAY.

    :param log_file: File the server's output is appended to.
    :raise XvfbError: If Xvfb is available but no server could be started.
    """
    xvfb = find_xvfb()
    if xvfb is None:
        yield None
        return

    failures: list[str] = []
    for attempt in range(1, MAX_ATTEMPTS + 1):
        try:
            proc, display = _spawn(xvfb, log_file)
        except (XvfbError, OSError) as exc:
            failures.append(f"attempt {attempt}: {exc}")
            continue
        try:
            yield display
        finally:
            _shut_down(proc)
        return

    raise XvfbError("; ".join(failures))


def _spawn(xvfb: str, log_file: Path) -> tuple[subprocess.Popen[bytes], str]:
    """Start one Xvfb server, and return it along with its DISPLAY value.

    :param xvfb: Path to the Xvfb binary.
    :param log_file: File the server's output is appended to.
    :raise XvfbError: If the server did not come up.
    """
    read_fd, write_fd = os.pipe()
    write_fd_open = True
    proc: subprocess.Popen[bytes] | None = None
    try:
        with log_file.open("ab") as log:
            proc = subprocess.Popen(
                [
                    xvfb,
                    # No display number: that is the whole point, see above.
                    "-displayfd",
                    str(write_fd),
                    "-screen",
                    "0",
                    SCREEN_GEOMETRY,
                    "-nolisten",
                    "tcp",
                    # Keep the server alive when the last client goes away,
                    # so that it lasts for the whole context.
                    "-noreset",
                ],
                # This clears CLOEXEC on the write end for us.
                pass_fds=(write_fd,),
                stdin=subprocess.DEVNULL,
                stdout=log,
                stderr=log,
            )

        # The parent must not keep the write end open: a server that dies on
        # startup would then never produce EOF on the read end, and we would
        # wait out the whole timeout instead of failing at once.
        os.close(write_fd)
        write_fd_open = False

        return proc, f":{_read_display_number(read_fd, proc)}"
    except BaseException:
        if proc is not None:
            _shut_down(proc)
        raise
    finally:
        os.close(read_fd)
        if write_fd_open:
            os.close(write_fd)


def _read_display_number(read_fd: int, proc: subprocess.Popen[bytes]) -> str:
    """Read the display number that Xvfb reports on a pipe.

    :param read_fd: Read end of the pipe passed to "-displayfd".
    :param proc: The server we are waiting for.
    :raise XvfbError: If the server died or stayed silent for too long.
    """
    deadline = time.monotonic() + STARTUP_TIMEOUT
    reported = b""
    exiting = False

    while b"\n" not in reported:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise XvfbError(
                f"Xvfb reported no display number within {STARTUP_TIMEOUT:g}s"
            )
        # Wait in slices rather than for the whole deadline, so that a server
        # dying without closing the pipe is noticed too.
        if select.select([read_fd], [], [], min(remaining, 0.5))[0]:
            chunk = os.read(read_fd, 64)
            if not chunk:
                exiting = True
                break
            reported += chunk
        elif proc.poll() is not None:
            exiting = True
            break

    number = reported.strip().decode("ascii", "replace")
    if not number.isdigit():
        details: list[str] = []
        if exiting:
            # The server has closed the pipe, so it is on its way out: the
            # wait is bounded, and its status makes for a far better message.
            try:
                details.append(f"exit status {proc.wait(SHUTDOWN_TIMEOUT)}")
            except subprocess.TimeoutExpired:
                pass
        if number:
            details.append(f"wrote {number!r}")
        raise XvfbError(
            "Xvfb reported no display number"
            + (f" ({', '.join(details)})" if details else "")
        )
    return number


def _shut_down(proc: subprocess.Popen[bytes]) -> None:
    """Stop a server, and wait for it to release its lock file and socket.

    "xvfb-run" merely signals its server and exits, which is why
    "/tmp/.X11-unix" fills up with stale sockets; waiting instead gives Xvfb
    the chance to tidy up after itself.

    :param proc: The server to stop.
    """
    if proc.poll() is not None:
        return

    proc.terminate()
    try:
        proc.wait(timeout=SHUTDOWN_TIMEOUT)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait()
