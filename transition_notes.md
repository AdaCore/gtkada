# Notes for the gtk3 to gtk4 transition

## Things done

- Commented / deactivated Gtk and Gdk completely
  - moved corresponding contrib/binding/packages/*.toml to contrib/binding/packages/gtk3/
  - deactivated corresponding entries in contrib/data.py
- Moved GtkAda-specific packages and any gtk3 packages to src/gtk3
- Reintroduced the minimal closure of GtkAda packages to allow GLib to build
- Removed any obsolete code in src/misc.c - translated some code from gtk3 to gtk4
- Upgraded Glib bindings
- Moved some conversion functions from Gtk.Arguments to Glib.Values, to remove any
  dependency from Glib to Gtk.

## To do as we translate

- In case of message reporting missing "Unchecked_To_X", uncomment the
  corresponding functions in src/gtk-arguments.ads and src/gtk-arguments.adb.

## To do (globally)

- Reintroduce Gdk bindings
- Reintroduce Gtk bindings
- Reactivate tests

- Makefile.in:
  - reactivate testgtk
  - reactivate tools

- Consider whether to generate the bindings for Graphene.
  - In which case, remove the Graphene section from GtkAda.Types

- Fix misc.c (look for the "TRANSITION" string)

## To do (package by package)

GBytes.toml:

- fix binding for functions that have gconstarray as parameters

GdkSurface.toml:

- when done, Reactivate bindings in GtkNative.toml

GdkDisplay.toml:

- when done, Reactivate bindings in GtkRoot.toml, GtkWindow.toml

GdkMonitor.toml:

- when done, reactivate bindings in GtkWindow.toml

GtkApplication.toml:

- when done, reactivate bindings in GtkWindow.toml

GtkWidget.toml:

- Review the commented-out code imported from the gtk3 bindings, and reactivate as needed.
