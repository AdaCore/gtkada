.. _Starting_an_application_with_GtkAda:

***********************************
Starting an application with GtkAda
***********************************

A GTK 4 program is built around an *application object*, of type
`Gtk.Application.Gtk_Application`. Calling `Run` initializes GTK during
application startup, loads resources such as Gtk.Builder UI definitions,
then runs the main loop. Creating the application object alone does not
initialize GTK.
It keeps track of the application's windows, and ends the
program when the last of them is closed (or another method is used to tell it
to hold, see below). It can also make sure that only one instance of the
application runs at a time, if this behavior is desired.

You do not need to call `Gtk.Main.Init` or run a main loop yourself when
creating your windows and widgets in the application's "activate" handler:
`Run` takes care of initialization before invoking that handler.

The complete source code for the larger example below is also installed in
:file:`share/examples/gtkada/basic_application/`, under the GtkAda installation
prefix. Build the application with :file:`basic_application.gpr`; the separate
:file:`test.gpr` project builds the automated checks. The :file:`README` includes
instructions for building and running both programs.

.. _Hello_window:

A minimal hello window
======================

This complete, single-file example opens an empty window. Closing it ends
the application and returns from `Glib.Application.Run`:

.. literalinclude:: ../../examples/hello-window/hello_window.adb
   :language: ada

From the repository root, build and run it with:

.. code-block:: sh

   gprbuild -p -P examples/hello-window/hello_window.gpr -aP . -XLIBRARY_TYPE=static
   ./examples/hello-window/bin/hello_window

The project and sources are also installed in
:file:`share/examples/gtkada/hello-window/`. Copy that directory to a writable
location, set up the installed GtkAda environment, then run
``gprbuild -p -P hello_window.gpr`` and ``./bin/hello_window``.
The example's :file:`README.md` gives environment setup instructions.

The nested callback uses GNAT's ``'Unrestricted_Access`` and remains in scope
until `Run` returns. The larger example below puts callbacks in a library-level
package so that it can use ``'Access``.

The main procedure
==================

See an example of main procedure which creates an application:

.. literalinclude:: ../../testsuite/tests/user-guide/ug_hello.adb
   :language: ada
   :start-after: --  START main
   :end-before: --  END main

The first argument to `Gtk_New` is the *application ID*. It names the
application for the desktop and the session bus, and it must be a
name such as ``com.example.hello`` (see `Glib.Application.Id_Is_Valid`
for the exact rules). If you pass an empty string, the application
cannot be single-instance (see :ref:`Command_line_and_single_instance`).

The second argument is a set of flags of type
`Glib.Application.GApplication_Flags`, combined with ``or``.
Most applications should use `G_Application_Flags_None`.

`Run` initializes GTK, emits "startup" and then "activate", and runs the
main loop. This loop returns when the application ends,
and its result is the exit status of the program. This example reads the
command line from `Ada.Command_Line`.

Creating the main window
========================

.. warning::

   Creating a `Gtk_Application` object does not initialize GTK. Create the
   `Gtk_Application_Window` and its widgets in the "activate" handler,
   after GTK's startup initialization, rather than in the main procedure
   before calling `Run`. Constructing a window before GTK is initialized
   can cause a segmentation fault inside GTK, including in GTK 4.24.1;
   this also occurs in C and is not specific to the Ada binding.

   If you are not using `GtkApplication`, call `Gtk.Main.Init` explicitly
   before creating a plain `Gtk.Window.Gtk_Window` or any other widgets,
   and arrange to run the main loop yourself.

The application shows the user interface in the "activate" signal handler.
This handler receives the application as a `Glib.Application.Gapplication`,
which you can convert to a `Gtk_Application`:

.. literalinclude:: ../../testsuite/tests/user-guide/ug_application.adb
   :language: ada
   :start-after: --  START activate
   :end-before: --  END activate
   :dedent: 3

A few points to note:

* The window is a `Gtk.Application_Window.Gtk_Application_Window`. It
  is owned by the application. Similarly, you can make a
  plain `Gtk.Window.Gtk_Window` become owned by the application, with
  `Gtk.Application.Add_Window`.

* Call `Gtk.Window.Present` to show the window and brings it
  to the front. All widgets are visibly by default in GTK4. To hide a
  widget, call `Gtk.Widget.Set_Visible` with `False`.

* A window is a container that has only one child, set with `Gtk.Window.Set_Child`.
  To arrange several widgets in a window, make its child a container, typically a
  `Gtk.Box.Gtk_Box` (see :doc:`hierarchical_composition`).

* The "activate" signalcan be emitted more than once in the same process,
  for instance when the user launches an application that is already running
  (see :ref:`Command_line_and_single_instance`).

For a larger example, see the GtkAda demo: the code in :file:`gtkada_demo/gtkada_demo.adb`
creates the application, the contents are built in the subprogram `On_Activate` in
:file:`gtkada_demo/main_windows.adb`.

.. _Manual_event_loop:

Running without GtkApplication
==============================

GTK 4 removed `Gtk.Main.Main` and `Gtk.Main.Main_Quit`, and GtkAda does not
currently bind `GMainLoop`. For a plain window, explicitly initialize GTK
and process events with `Glib.Main.Main_Context_Iteration`:

.. literalinclude:: ../../examples/hello-window/hello_window_manual.adb
   :language: ada

The :file:`hello_window.gpr` project above builds this variant too; run
``./examples/hello-window/bin/hello_window_manual`` from the repository root,
or ``./bin/hello_window_manual`` from a copy of the installed example.

With the default ``Context => null``, iteration dispatches GTK's events on
the default main context. ``May_Block => True`` waits for work when no events
are ready, avoiding busy polling. The return value tells whether events were
dispatched, so use a separate flag to decide when to stop. Here the
"close-request" handler clears that flag and returns `False` to let GTK
destroy the window. Do not access the window after it has been destroyed.

Prefer `GtkApplication` for an application that should own its windows and
manage its lifetime. When integrating GTK into an existing event loop, use
``May_Block => False`` to dispatch ready events without waiting, and let the
host loop arrange when to call it again. See :ref:`Processing_external_events`.

Ending the application
======================

`Run` returns when the application has no windows left.

To close a window programmatically, call one of these subprograms:

* `Gtk.Window.Close`: this does what the close button of the title bar does: it
  emits "close-request" on the window, giving the window a chance to intercept
  the request, for instance to display a dialog to the user. See below.

* `Gtk.Window.Destroy` destroys the window without emitting
  "close-request".

The Quit button of the example calls `Close`:

.. literalinclude:: ../../testsuite/tests/user-guide/ug_application.adb
   :language: ada
   :start-after: --  START quit
   :end-before: --  END quit
   :dedent: 3

To stop a window from closing, connect a handler to "close-request". For
instance to display a confirmation dialog. The window is kept open if the
handler for this signal returns `True`:

.. literalinclude:: ../../testsuite/tests/user-guide/ug_application.adb
   :language: ada
   :start-after: --  START close_request
   :end-before: --  END close_request
   :dedent: 3

To hide a window when it is closed, rather than destroying it, call
`Gtk.Window.Set_Hide_On_Close`. A hidden window still belongs to the
application, which therefore keeps running.

`Glib.Application.Quit` ends the application at once, even if it still has
windows. The windows do not get "close-request".

`Glib.Application.Hold` keeps the application running when it has no
windows. Each call must be matched by a call to `Glib.Application.Release`.
This is useful for windowless applications or applications
which need to keep running background tasks.

Startup and shutdown
====================

Besides "activate", the application emits two signals that you can connect
to with `Glib.Application.On_Startup` and `Glib.Application.On_Shutdown`:

* "startup" is emitted once, before the first "activate". GTK is
  initialized by then. It is a good place for one-off setup operations, such as
  loading your CSS files.

* "shutdown" is emitted once, after the main loop has ended and just before
  `Run` returns.

Both are emitted only in the primary instance (see the next section).

.. _Command_line_and_single_instance:

Command line and single instance
================================

When the application has an ID, and the desktop provides a D-Bus session bus,
(as most Linux desktops do), only one instance of the application runs at a
time. The first process to start becomes the *primary instance*. When the
the application is started again, the new process asks the primary instance
to emit "activate" (or "command-line", see below) and then exits. To support
this, the "activate" handler must be able to work with a window that already exists.

The flags passed to `Gtk_New` can change this behaviour. The most interesting ones
are:

`G_Application_Flags_None`
    The default.

`G_Application_Non_Unique`
    Deactivate the "single window" concept: every process is a new instance.

`G_Application_Handles_Command_Line`
    The primary instance emits "command-line" instead of "activate". The
    handler receives the command line arguments,
    including those passed to a second instance.


To handle the command line, use `G_Application_Handles_Command_Line` and
connect a handler to "command-line":

.. literalinclude:: ../../testsuite/tests/user-guide/ug_application.adb
   :language: ada
   :start-after: --  START create_command_line
   :end-before: --  END create_command_line
   :dedent: 6

The handler reads the arguments from its `Command_Line` parameter. It must
emit "activate" itself in order for a window to be shown, since the
application no longer takes care of it. The value that the handler returns
is the exit status of the process that received the command line. That process
might not be the primary instance:

.. literalinclude:: ../../testsuite/tests/user-guide/ug_application.adb
   :language: ada
   :start-after: --  START command_line
   :end-before: --  END command_line
   :dedent: 3

`Glib.Application.Gapplication_Command_Line` also passes the current working
directory of the process that received the command line (`Get_Cwd`), along
with its environment (`Getenv`), if the application was created with the
`G_Application_Send_Environment` flag. Prefer these rather than
`Ada.Directories` and `Ada.Environment_Variables`, which only relate to the
primary instance.
