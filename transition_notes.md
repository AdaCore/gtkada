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
  - reactivate the install of gtkada_demo
  - reactivate tools

- Consider whether to generate the bindings for Graphene.
  - In which case, remove the Graphene section from GtkAda.Types

- Fix misc.c (look for the "TRANSITION" string)

## To do (package by package)

GBytes.toml:

- fix binding for functions that have gconstarray as parameters

GInputStream.toml

- when done, reactivate bindings in GdkClipboard.toml, GLoadableIcon.toml

GOutputStream.toml

- when doen, reactivate bindings in GdkContentProvider.toml

GdkAppLaunchContext.toml:

- when done, reactivate bindings in GdkDisplay.toml

GdkEvent.toml:

- when done, reactivate bindings in GtkDisplay.toml

GdkSurface.toml:

- invesigate invalid code for marshaller of "::render" signal

GdkTexture.toml:

- when done, reactivate bindings in GdkClipboard.toml, GdkCursor.toml

GdkKeymapKey:

- when done, reactivate bindings in GdkDisplay.toml

GdkModifierType:

- when done, reactivate bindings in GdkDevice.toml, GtkDisplay.toml,
  GdkSurface.toml

GdkSeat.toml:

- when done, reactivate bindings in GdkDevice.toml, GdkDisplay.toml

GdkModifierType:

- when done, reactivate bindings in GdkDevice.toml

GdkSeat.toml:

- when done, reactivate bindings in GdkDevice.toml

GtkApplication.toml:

- when done, reactivate bindings in GtkWindow.toml

GtkWidget.toml:

- Review the commented-out code imported from the gtk3 bindings, and reactivate as needed.

## Dialog widgets (work item #46)

Bound so far: `GtkNativeDialog`, `GtkAlertDialog`, `GtkFontDialog` +
`GtkFontDialogButton`, `GtkColorDialog` + `GtkColorDialogButton`.
`GdkRGBA` was reactivated to support `GtkColorDialog`. A minimal
`src/gdk.ads` parent unit was reintroduced for the same reason. The
GIO async-result pattern is supported via an opaque
`Glib.G_Async_Result` proxy declared in `glib.ads`; per-dialog
`Gasync_Ready_Callback` access types are emitted by the generator.

Intentionally deferred:

- `GtkAppChooserDialog` — deprecated since 4.10, and depends on the
  yet-to-be-reactivated `GtkDialog`. Dropped from the work item.
- `GtkPageSetupUnixDialog` — depends on `GtkDialog`, `GtkPageSetup`
  and `GtkPrintSettings`. Revisit once those are bound.
- `GtkFileDialog` — every interesting method takes or returns
  `GFile*` / `GListModel<GFile>`. Revisit when `GFile` (Gio
  interface) is wired up.
- `GtkPrintDialog` — needs `GtkPageSetup`, `GtkPrintSettings`,
  `GtkPrintSetup`, `GFile`, `GOutputStream`. Revisit after those.
- `GtkFontDialog.choose_font_and_features` — multiple complex out
  parameters; suppressed for now.
- `GtkFontDialog.{get,set}_filter` — needs `GtkFilter`; suppressed
  with `bind = false`.
- `GdkRGBA.{equal,hash}` — use `gconstpointer`, which the generator
  cannot map yet; suppressed with `bind = false`.
- `Glib.G_Async_Result` (in `src/glib.ads`) is currently a hand-written
  opaque proxy. Revisit and replace it with the generated binding once
  the full `GAsyncResult` interface is reactivated.

## GtkCellRenderer (work item #50)

`GtkCellRenderer` is bound (and marked obsolescent since gtk 4.10).
Three methods are suppressed in
`contrib/binding/packages/GtkCellRenderer.toml` pending bindings for
the following types:

- `gtk_cell_renderer_activate` — needs `GdkEvent`.
- `gtk_cell_renderer_snapshot` — needs `GtkSnapshot`.
- `gtk_cell_renderer_start_editing` — needs `GdkEvent`.

Revisit and re-enable these once `GdkEvent` and `GtkSnapshot` are
bound for gtk4.
