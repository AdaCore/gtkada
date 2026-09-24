.. _Starting_an_application_with_GtkAda:

***********************************
Starting an application with GtkAda
***********************************

A GTK 4 program is built around an *application object*, of type
`Gtk.Application.Gtk_Application`. The application object initializes GTK,
loads resources such as Gtk.Builder UI definitions, then runs the main loop.
It keeps track of the application's windows, and ends the
program when the last of them is closed (or another method is used to tell it
to hold, see below). It can also make sure that only one instance of the
application runs at a time, if this behavior is desired.

You do not need to call `Gtk.Main.Init` or run a main loop yourself, as the
application object does both.

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
