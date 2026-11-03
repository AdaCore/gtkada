.. _Transitioning_from_GtkAda_3_to_GtkAda_4:

*****************************************
Transitioning from GtkAda 3 to GtkAda 4
*****************************************

GtkAda 4 binds GTK 4. The upstream `migration guide
<https://docs.gtk.org/gtk4/migrating-3to4.html>`_ describes the changes
between GTK 3 and GTK 4; since GtkAda follows the C API closely
(`gtk_window_present` becomes `Gtk.Window.Present`), its advice applies
directly to Ada code.

`Gtk.Main.Main` and `Gtk.Main.Main_Quit` are no longer available. Use
`Glib.Application.Run` with `Gtk.Application.Gtk_Application` to initialize
GTK and run the event loop; create windows in the "activate" handler.
See :ref:`Hello_window` for a complete minimal program. For a plain window
with explicit initialization and event processing, see :ref:`Manual_event_loop`.
