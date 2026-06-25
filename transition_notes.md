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

## gtkada_demo

- Demos for `Create_Tree_Filter` and `Create_Tree_View` rewritten and enabled
- Added new demo: `Create_Custom_Widget`
- Reactivated and ported demos for simple button widgets (`GtkButton`, `GtkCheckButton`, `GtkToggleButton`, others pending)
- Removed demos for widgets removed in GTK4 (See work item #111)
- Reactivated `gtkada_demo`'s `Create_Label` package: migrated from Gtk3 to Gtk4
  (`Gtk_New (Box, Orientation_*, …)` + `Append` instead of `Pack_Start`,
  `Set_Child` instead of `Add`, `Set_Wrap` / `Set_Wrap_Mode` instead of
  `Set_Line_Wrap`, `Set_Markup` with `<u>...</u>` in place of the now-gone
  `Set_Pattern`, dropped `Show_All`) and wired it into
  `Main_Windows.On_Activate`, so the demo window now shows the `Gtk.Label`
  test instead of the placeholder.
- Reintroduced `GtkListStore` for gtk4 (re-enabled in `contrib/data.py`,
  added `contrib/binding/packages/GtkListStore.toml`) and used it in
  `gtkada_demo/main_windows.adb` to build a `Gtk_Paned` + `Gtk_Scrolled_Window` +
  `Gtk_Tree_View` selector with a single "labels" entry that drives
  the `Create_Label` demo on the right-hand side. The C glue
  (`ada_gtk_list_store_set_*`) was already present in `src/misc.c`.
- Reactivated `gtkada_demo`'s `Create_Box`, `Create_Frame`, `Create_Paned`
  and `Create_Scrolled` packages: migrated their bodies from Gtk3 to Gtk4
  and wired them (plus their `Help` text) into the `Main_Windows` selector.
  Notable API substitutions:
  - `Gtk_New (Box, Orientation_*, Spacing)` + `Set_Homogeneous` instead of
    `Gtk_New_Hbox` / `Gtk_New_Vbox`, and `Append` with per-child
    `Set_Hexpand` / `Set_Halign` instead of `Pack_Start`'s
    `Expand` / `Fill` arguments.
  - `Set_Child` instead of `Add`, `Set_Margin_*` instead of
    `Set_Border_Width`, and `Gtk_New (Sep/Paned, Orientation_*)` instead of
    the `H`/`V` constructors.
  - `Gtk_Paned`'s `Pack1` / `Pack2` became `Set_Start_Child` /
    `Set_End_Child` with their `Set_Resize_*_Child` / `Set_Shrink_*_Child`
    companions; the decorative `Set_Shadow_Type` frames are gone (Gtk4
    dropped `Gtk_Shadow_Type`).
  - `Create_Frame` no longer demonstrates shadow types (removed in Gtk4);
    it now showcases label presence/absence and the `Xalign` of the frame
    label (`Set_Label_Align` lost its `Yalign` argument).
  - Dropped the now-redundant `Show_All` calls (Gtk4 widgets are visible by
    default).
  The selector now also shows each demo's `Help` text in a panel below the
  demo frame; the legacy `@b...@B` emphasis markers are converted to Pango
  markup at display time.

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

gtk-handlers.ads:

- when done, fix gtkada_demo/create_tree_view.adb

GBytes.toml:

- fix binding for functions that have gconstarray as parameters

GFile.toml:

- when done, reactivate bindings in GdkTexture.toml

GInputStream.toml

- when done, reactivate bindings in GdkClipboard.toml, GLoadableIcon.toml, GResource.toml, GdkDrop.toml

GOutputStream.toml

- when doen, reactivate bindings in GdkContentProvider.toml

GdkAppLaunchContext.toml:

- when done, reactivate bindings in GdkDisplay.toml

GdkDragContext.toml:

- when done, reactivate bindings in GtkTreeView.toml

GdkEvent.toml:

- when done, reactivate bindings in GtkDisplay.toml, GtkCellArea.toml

GdkPixbuf.toml:

- when done, reactivate bindings in GdkTexture.toml

GdkSurface.toml:

- invesigate invalid code for marshaller of "::render" signal

GdkTexture.toml:

- when done, reactivate bindings in GdkCursor.toml

GdkKeymapKey:

- when done, reactivate bindings in GdkDisplay.toml

GdkModifierType:

- when done, reactivate bindings in GdkDevice.toml, GtkDisplay.toml,
  GdkSurface.toml, GtkTreeView.toml, GdkDevice.toml, GtkCellRendererAccel.toml

GtkApplication.toml:

- when done, reactivate bindings in GtkWindow.toml

GtkSnapshot.toml:

- when done, reactivate bindings in GtkCellArea.toml, GtkCellRenderer.toml
- when done, remove the manual binding in gtkada_demo/create_custom_widget.adb

GtkWidget.toml:

- Review the commented-out code imported from the gtk3 bindings, and reactivate as needed.

GtkFixedLayoutChild.toml:

- `gtk_fixed_layout_child_get_transform` and `gtk_fixed_layout_child_set_transform`
  are currently disabled because they take or return a `GskTransform*` and GSK
  is not bound yet (no `Gsk-4.0.gir` is loaded). Re-enable them when GSK is
  bound.

GtkConstraintLayout.toml:

- All methods that take or return a `GtkConstraint` or `GtkConstraintGuide`
  are currently disabled (add/remove constraint, add/remove guide,
  observe_constraints, observe_guides, add_constraints_from_description*).
  Re-enable them once `GtkConstraint` and `GtkConstraintGuide` are bound.

GtkCustomLayout.toml:

- `gtk_custom_layout_new` is disabled because it takes three callback
  parameters (`GtkCustomRequestModeFunc`, `GtkCustomMeasureFunc`,
  `GtkCustomAllocateFunc`). Re-enable once those callback types are bound.

GtkLayoutManager.toml and GtkLayoutChild.toml:

- `gtk_layout_manager_get_widget`, `gtk_layout_manager_allocate`,
  `gtk_layout_manager_measure`, `gtk_layout_manager_get_layout_child`,
  `gtk_layout_child_get_child_widget`, `gtk_layout_child_get_layout_manager`
  are disabled to break a circular dependency between `Gtk.Widget`,
  `Gtk.Layout_Manager` and `Gtk.Layout_Child`. Once the generator gains
  support for `limited with`, these can be re-enabled.

GtkColumnView - bound but requires the following to be useable:

- `GtkBuilderListItemFactory` to instantiate `GtkColumnViewColumn` widgets
  - needs `GListStore` which has the following considerations:
    - `gconstpointer` binding
    - `GEqualFunc` and `GCompareDateFunc` callback handling
  - needs `Gtk.BuilderScope` and `GtkBuilderCScope` bound

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

- `gtk_cell_renderer_snapshot` — needs `GtkSnapshot`.

Revisit and re-enable these once `GdkEvent` and `GtkSnapshot` are
bound for gtk4.

## Menus (work item #87)

In gtk4 the classic `GtkMenu` / `GtkMenuBar` / `GtkMenuItem` /
`GtkMenuShell` widget family no longer exists, and those packages will
not come back. Menus are now described by a *menu model* (`Glib.Menu` /
`Glib.Menu_Model`, already bound) whose items reference named actions,
and displayed by three widgets, all bound by this work item:

- `Gtk.Popover_Menu_Bar` — the horizontal menu bar, replaces
  `Gtk_Menu_Bar`;
- `Gtk.Menu_Button` — a button that pops up a popover or menu model;
- `Gtk.Popover_Menu` — the popover displaying a menu model (context
  menus, submenus), a subclass of the also-newly-bound `Gtk.Popover`.

Supporting changes:

- `GSimpleActionGroup` was re-enabled (`Glib.Simple_Action_Group`) and
  `gtk_widget_insert_action_group` re-activated in `GtkWidget.toml`
  (its previous blocker, `GAction`, is bound): menu items are wired to
  `Glib.Simple_Action`s through an action group inserted on a widget.
- `gtk_menu_button_set_create_popup_func` is bound by the generator's
  standard closure machinery (including a `Set_Create_Popup_Func_User_Data`
  generic) — no TOML override was needed.
- `Glib.Main.Main_Context_Iteration` was hand-bound in `src/glib-main.ads`
  (the testsuite needs to iterate the default main context).
- The testsuite driver now sets `GSK_RENDERER=cairo`: Xvfb has no GL
  stack, and realizing a window otherwise crashes inside libepoxy while
  GSK probes for a GL renderer.
- `gtkada_demo/create_menu.adb` was rewritten for the gtk4 paradigm
  (menu models + actions instead of menu-item widgets) and re-enabled
  in the selector; `testsuite/c_tests/popover.c` and `popovermenu.c`
  were ported to `testsuite/tests/popover` and
  `testsuite/tests/popovermenu` (the latter builds its models with
  `Glib.Menu` instead of the unbound `GtkBuilder`).

## GtkTextView (work item #89)

`GtkTextView` is bound for gtk4 (re-enabled in `contrib/data.py`, with
`contrib/binding/packages/GtkTextView.toml` quarried from the gtk3
recipe). The bread-and-butter surface is bound cleanly — construction,
the buffer relationship, editability / cursor / wrap-mode / margin
properties, the iter/coordinate geometry getters, the child-anchor and
scroll-to-mark families, and the signals (including `extend-selection`,
whose `GtkTextIter` parameters reuse the marshaller enabled by the
buffer work). The `extend-selection`, `move-viewport` and
`delete-from-cursor` signals also needed `Unchecked_To_*` enum
marshallers (`Gtk_Text_Extend_Selection`, `Gtk_Scroll_Step`,
`Gtk_Delete_Type`), re-enabled in `src/gtk-arguments.ads`.

The gtk3 `get/set_[hv]adjustment` methods are gone in gtk4 (folded into
the unbound `Scrollable` interface) and were dropped from the quarry.

Intentionally deferred, to add later on demand:

- `gtk_text_view_im_context_filter_keypress` — takes a `GdkEvent`, which
  is not bound yet; suppressed with `bind = false`.
- The `snapshot_layer` virtual method (gtk4's replacement for the gtk3
  `draw_layer` vfunc that the quarry overrode) — needs `GtkSnapshot`,
  which is not bound; left unbound (class virtual methods are not bound
  by default).

The `create_text_view` demo was revived (ported to gtk4: `Set_Child`
instead of `Add`, no `Show_All`) and wired into `main_windows.adb`. A
new `testsuite/tests/text-view` test covers view construction, the
buffer get/set relationship, the common properties and the scroll
family; the deferred child-anchor cases from `textbuffer.c`
(`test_iter_with_anchor` / `test_get_text_with_anchor`) were back-filled
into `testsuite/tests/text-buffer`, now attaching a `Gtk_Label` at each
anchor through the view.

## Testsuite

Items to revisit as the GtkAda bindings grow.

## testsuite/tests/label  (/label/markup-parse)

Partial port of GTK's testsuite/gtk/label.c (test_label_markup). The original
C test, via print_attr_list / print_attribute, compares a full stringified
PangoAttrList dump for the label's layout. That comparison was dropped because
Pango.Attributes (src/generated/pango-attributes.ads) currently provides no:

- attribute iterator (pango_attr_list_get_iterator and the
  pango_attr_iterator_* family);
- per-attribute accessors (start_index/end_index, attribute type, and the
  as_int / as_float / as_string / as_color / as_font_desc / as_language /
  as_shape value getters);
- Pango_Color binding or pango_color_to_string.

Revisit and complete /label/markup-parse once the Pango attribute
introspection bindings are available.
