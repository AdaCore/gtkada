# Hello window

These complete, single-file GTK 4 examples open an empty window and exit when
you close it. Start with `hello_window.adb`: it creates a `Gtk_Application`,
connects `activate`, and calls `App.Run` to initialize GTK and process events.
The window is created inside `activate`, after initialization. Creating the
application object alone does not initialize GTK.

From the repository root, build and run with GNAT and GPRbuild:

```sh
gprbuild -p -P examples/hello-window/hello_window.gpr -aP . -XLIBRARY_TYPE=static
./examples/hello-window/bin/hello_window
```

The same project is installed under `<prefix>/share/examples/gtkada/hello-window/`.
To use the installed library, copy that directory to a writable location and
set up the environment with the supplied script on Unix:

```sh
eval "$("<prefix>/bin/gtkada-env.sh" --print-only)"
gprbuild -p -P hello_window.gpr
./bin/hello_window
```

On Windows, add `<prefix>/lib/gnat` to `GPR_PROJECT_PATH` and `<prefix>/bin` to
`PATH`; the executables have an `.exe` suffix. Both examples require a graphical
display.

For a plain `Gtk_Window` without `GtkApplication`, run `hello_window_manual`
from the same `bin` directory. It calls `Gtk.Main.Init` before creating the
window, then repeatedly calls `Glib.Main.Main_Context_Iteration` on the default
context. `May_Block => True` waits for events without busy polling. The
`close-request` callback clears a flag to end the loop and returns `False` so
GTK destroys the window. The iteration function's result reports whether events
were dispatched; it does not report whether the window is still open.

GTK 4 has no `Gtk.Main.Main` or `Gtk.Main.Main_Quit`, and this binding does not
expose `GMainLoop`. Use `App.Run` for applications, or the manual context loop
when you need to manage event processing yourself.

The examples use GNAT's `'Unrestricted_Access` to keep callbacks in the same
source file as the main procedure. Those callbacks remain in scope until the
event loop returns. In a larger application, put callbacks in a library-level
package and use `'Access`, as in the installed `basic_application` example.
