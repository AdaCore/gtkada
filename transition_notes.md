# Notes for the gtk3 to gtk4 transition

## Things done

- Updated GLib, GIO, and GObject GIR file versions from 2.42 to 2.88 (#135)
- TODO: some bindings temporarily deactivated (Noted with 'TODO #135')

- Commented / deactivated Gtk and Gdk completely
  - moved corresponding contrib/binding/packages/*.toml to contrib/binding/packages/gtk3/
  - deactivated corresponding entries in contrib/data.py
- Moved GtkAda-specific packages and any gtk3 packages to src/gtk3
- Reintroduced the minimal closure of GtkAda packages to allow GLib to build
- Removed any obsolete code in src/misc.c - translated some code from gtk3 to gtk4
- Upgraded Glib bindings
- Moved some conversion functions from Gtk.Arguments to Glib.Values, to remove any
  dependency from Glib to Gtk.
- Functions now take Ada 2012 `out` / `in out` parameters where they used to take
  `access` (work item #127). **This breaks downstream sources**: callers must pass
  a variable and drop the `'Access`. Roughly 30 subprograms are concerned, among
  them `Gtk.Text_Buffer.Get_Iter_At_Line{,_Index,_Offset}`,
  `Gtk.Text_View.Get_Iter_At_{Location,Position}` (the `Iter`) and
  `Gtk.Accessible.Get_Bounds` (inherited by every widget). Handler types
  (`Virtual_*`, `Cb_*`) follow suit.
- An output the C side documents as optional keeps `access`, now with a `:= null`
  default (work item #128). These are the outputs C accepts a `NULL` for, and the
  default lets an Ada caller decline them just as easily — passing a dummy variable
  is never needed. Concerned are `Glib.Resource.Get_Info`,
  `Glib.List_Store.Find{,_With_Equal_Func}`, `Glib.Action_Group.Query_Action`,
  `Glib.Variant.{Dup,Get}_{String,Strv,Objv,Bytestring_Array}`,
  `Gdk.Content_Formats.Get_Mime_Types`, `Gtk.Text_View.Get_Iter_At_Position`
  (the `Trailing`) and `Gtk.Tree_View.{Get_Dest_Row_At_Pos,Is_Blank_At_Pos}`.
  Relative to the released API this is a restoration: those parameters were
  `access` before #127. The rule applies to functions only; procedures that
  already exposed such an output as `out` are left alone.
- `Gdk.Content_Provider.Get_Value` takes its `Value` `in out`: the caller
  initialises it with the `GType` the value is to be provided in.
- `Gtk.Builder.Value_From_String` takes its `Pspec` `in`; C never writes to it.

## gtkada_demo

- Demos for `Create_Tree_Filter` and `Create_Tree_View` rewritten and enabled
- Added new demo: `Create_Custom_Widget`
- Added new demo: `Create_Drawing_Area`
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
- Reactivated `gtkada_demo`'s `Create_Tooltips` package: migrated its body
  from Gtk3 to Gtk4 and wired it (plus its `Help` text) into the
  `Main_Windows` selector. Notable API substitutions:
  - `Gtk_Tooltip.Set_Icon_From_Stock (Stock_Delete, …)` (and the
    now-gone `Gtk.Stock` unit) became
    `Gtk_Tooltip.Set_Icon_From_Icon_Name ("edit-delete")`.
  - The custom-tooltip example no longer uses a `Window_Popup` tooltip
    window with `Set_Tooltip_Window` / `Override_Background_Color` (all
    removed in Gtk4); it now embeds a custom widget through the
    `query-tooltip` handler via `Gtk.Tooltip.Set_Custom`.
  - `Gtk_New_With_Label` + `Append` / `Set_Child` instead of `Gtk_New`
    with a label, `Pack_Start` and `Add`; dropped `Show_All` / `Show`.
  - The `On_Query_Tooltip` callback signatures already match the generated
    Gtk4 binding, so they were reused unchanged.
  Ported the corresponding C unit test (`testsuite/c_tests/tooltips.c`)
  to `testsuite/tests/tooltips/`.
- Bound `GtkSearchEntry` and `GtkSearchBar` and reactivated
  `gtkada_demo`'s `Create_Entry` package around them, migrated from Gtk3
  to Gtk4. Notable points:
  - `Gtk_Search_Entry` emits `search-changed` only after `Search_Delay`
    milliseconds of quiet, so the demo filters a small word list from
    `On_Search_Changed` and exposes the delay through a `Gtk_Spin_Button`
    to make that debounce visible. Its `next-match` / `previous-match` /
    `stop-search` keybinding signals are reported into a label.
  - `Gtk_Search_Bar.Connect_Entry` takes a `Gtk_Editable` in Gtk4, not a
    `Gtk_Entry`, so the bar is fed with `+Bar_Entry` through the
    `Implements_Gtk_Editable` conversion.
  - `Set_Key_Capture_Widget` is given the demo frame, so typing anywhere
    reveals the bar. The property is nullable on both widgets and comes
    back as a null `Gtk_Widget` when unset.
  - `Gtk_Combo_Box_Text` and `Gtk_Level_Bar` are not bound yet, so the
    parts of the Gtk3 demo that used them were dropped for now; the rest
    of the Gtk3-isms went the usual way (`Gtk_New (Box, Orientation_*,
    Spacing)` + `Append` instead of `Gtk_New_Vbox` / `Pack_Start`,
    `Set_Child` instead of `Add`, `Set_Margin_*` instead of
    `Set_Border_Width`, no `Show_All`).
  - The Gtk3 demo passed the entry to its callbacks as user data through
    `Gtk.Handlers.User_Callback`; the generated Gtk4 handlers take only
    the emitting widget, so the governed widgets are held at library
    level instead.
  Added `testsuite/tests/search-entry/` (no C test exists to port) and
  `testsuite/tests/search-bar/`, the latter ported from
  `testsuite/c_tests/searchbar.c`.

## gtkada_demo: aligning with gtk4-demo

Goal: bring `gtkada_demo` closer to the demo shipped with gtk4
(`gtk-4.22.2/demos/gtk-demo/`), in presentation and in coverage.

Decisions:

- Selector: `Gtk.List_View` + `Gtk.Tree_List_Model`, upstream's own sidebar
  machinery, rather than `Gtk_Tree_Store` + `Gtk_Tree_View` — the demo's
  chrome should showcase the gtk4 list API, not the one gtk4 deprecated.
- Demo names follow upstream's `Category/Title` strings verbatim
  (`Text View/Markup`, `Lists/Colors`, …) so coverage can be diffed
  mechanically; GtkAda-only demos get `GtkAda/…` categories.
- The shell otherwise stays as it is: demo frame plus help panel.
  Upstream's source-code tab, search entry and "run in its own toplevel"
  are out of scope. (Should we reconsider: the source viewer needs only
  `Gtk.Notebook` + `Gtk.Text_View`, both bound; the search box needs
  `Gtk.Search_Entry`, `--` in `contrib/data.py`.)

### Step 1 — categorised tree selector

- [ ] **Bind `GtkSignalListItemFactory`**: `new` plus the `setup` / `bind` /
  `unbind` / `teardown` signals, each carrying a `Gtk.List_Item` (bound).
  Absent from `contrib/data.py`; a leaf class with no unbound dependencies.
  (The bound `Gtk.Builder_List_Item_Factory` is feasible too — an Ada item
  can expose template-lookup properties via `Glib.Properties.Creation` +
  `Initialize_Class_Record` — but costs, per item type, a property-id enum,
  a `Set_Property`/`Get_Property` pair, a `Param_Spec` per field and a UI
  template. The signal factory costs two handlers. Rejected on cost only.)
- [ ] **Bind `GtkTreeExpander`**: draws the expander arrow and the
  indentation, and ties a row to its `Gtk.Tree_List_Row`. Deferrable — our
  tree is two levels and static, so an indent box plus a toggle calling
  `Gtk.Tree_List_Row.Set_Expanded` would do, at the cost of not looking
  like the rest of gtk4.

  Both are worth doing first regardless: every `Lists/…` demo and the
  awaited `Gtk.Column_View` demo need them. They give row *rendering*
  only — `Lists/Settings`, `Lists/Alternative Settings`,
  `Lists/Application launcher` and `Lists/File browser` stay blocked on
  their data sources (Step 2).
- [ ] **Add a `Demo_Item` GObject**: a `Glib.Object.GObject_Record`
  derivative registered through `Ada_GObject_Class` (the pattern
  `create_custom_widget.adb` uses), carrying title, category, the `Run`
  access-to-procedure and the `Help` function.
- [ ] **Move the `Demos` array** out of `main_windows.adb` into its own
  package (say `Demo_Registry`), each entry naming its full upstream-style
  path; derive the category list from those paths so adding a demo stays a
  one-line change.
- [ ] **Rebuild the selector**: `Glib.List_Store` of categories →
  `Gtk.Tree_List_Model` (create-child-model function returning each
  category's `Glib.List_Store` of demos) → `Gtk.Single_Selection` →
  `Gtk.List_View` with a signal factory building a `GtkTreeExpander` +
  `Gtk.Label` per row. `Gtk.List_Store`, `Gtk.Tree_View`,
  `Gtk.Tree_View_Column` and `Gtk.Cell_Renderer_Text` then leave
  `main_windows.adb`; they stay demonstrated by the `Tree View/…` demos.
- [ ] **Write a new selection callback.** `On_Selection_Changed` cannot be
  carried over: it takes a `Gtk_Tree_Selection`, calls `Get_Selected` for a
  `Gtk_Tree_Iter` and reads an index back through `Demo_Column` — none of
  the three survive. The replacement hangs off `Gtk.Single_Selection`'s
  `notify::selected-item` and must:
  - unwrap the row: `Get_Selected_Item` over a `Gtk.Tree_List_Model`
    returns the `Gtk.Tree_List_Row` wrapper, and
    `Gtk.Tree_List_Row.Get_Item` the item proper. It returns null when
    nothing is selected, which replaces the old `Null_Iter` guard.
  - **reject category rows in the callback itself**, by returning early
    for anything that is not a `Demo_Item`. That type test is *the* guard,
    not a backstop: `Gtk.List_Item.Set_Selectable (False)` does not stop
    the selection model selecting the row. `gtk-list_item.ads` says so
    outright — "making an item non-selectable has no influence on the
    selected state at all. A non-selectable item may still be selected" —
    and GTK resets the property whenever the item is rebound. Set it for
    the click affordance if we like, but nothing may depend on it.
    Upstream guards the same way (`main.c:selection_cb` gates on
    `demo->func != NULL`). Making the invariant structural instead would
    mean a selection model that refuses expandable rows, i.e. subclassing:
    GTK ships nothing off the shelf.
  - read `Run` and `Help` straight off the `Demo_Item`, so the
    `Index in Demos'Range` tests go away with the array.

  Only the tail of the old body carries over: clear the frame, call
  `Run (Demo_Frame)`, set the help label from `To_Markup`.
- [ ] **Remap the existing demos** onto the upstream taxonomy:

  | today | becomes |
  | --- | --- |
  | Boxes | `Layout/Boxes` *(GtkAda-only; upstream has no box demo)* |
  | Buttons | `Buttons/Buttons` *(GtkAda-only)* |
  | Check Buttons | `Buttons/Check Buttons` *(GtkAda-only)* |
  | Color Chooser | `Pickers and Launchers` *(the colour half only)* |
  | Custom Widget | `GtkAda/Custom Widget` |
  | Frames | `Layout/Frames` *(GtkAda-only)* |
  | Labels | `GtkAda/Labels` *(GtkAda-only)* |
  | List Store | `Tree View/List Store` |
  | Menus | `GtkAda/Menus` *(upstream folds menus into other demos)* |
  | Paned | `Paned Widgets` |
  | Reparent | `GtkAda/Reparent` |
  | Scrolled Window | `Layout/Scrolled Window` *(GtkAda-only)* |
  | Spin Buttons | `Spin Buttons` |
  | Text View | `Text View/Multiple Views` |
  | Timeout | `GtkAda/Timeout` |
  | Toggle Buttons | `Buttons/Toggle Buttons` *(GtkAda-only)* |
  | Tooltips | `GtkAda/Tooltips` |
  | Tree Filter | `Tree View/Filter Model` |
  | Tree View | `Tree View/Tree Store` |

### Step 2 — coverage map against upstream

All 110 upstream demos, grouped by what blocks them. "Ready" means every
binding the demo needs is generated. A snapshot to re-check as bindings
land, not a contract.

**Ready now** (no new binding needed)

| upstream demo | notes |
| --- | --- |
| `Tree View/List Store` | exists (`create_list_store`), needs renaming only |
| `Tree View/Filter Model` | exists (`create_tree_filter`) |
| `Tree View/Tree Store` | exists (`create_tree_view`) |
| `Tree View/Editable Cells` | `Gtk.Cell_Renderer_Text` editing + `Gtk.List_Store` |
| `Paned Widgets` | exists (`create_paned`) |
| `Spin Buttons` | exists (`create_spin`) |
| `Expander` | `Gtk.Expander` bound |
| `Entry/Completion` | `Gtk.Entry_Completion` bound |
| `Entry/Undo and Redo` | `Gtk.GEntry` + `Gtk.Editable` undo properties |
| `Text View/Multiple Views` | exists (`create_text_view`) |
| `Text View/Markup` | `Gtk.Text_Buffer` + `Gtk.Text_Tag` |
| `Text View/Tabs` | `Pango.Tabs` bound |
| `Text View/Undo and Redo` | `Gtk.Text_Buffer` undo API |
| `Text View/Automatic Scrolling` | `Gtk.Text_View.Scroll_To_Mark` + `Glib.Main` |
| `Builder` | `Gtk.Builder` bound; the `.ui` must stay within bound widgets |
| `Application Class` | `Gtk.Application` + `Glib.Menu`; drop the header bar |
| `Combo Boxes` | only as a `Gtk.Drop_Down` demo — `Gtk.ComboBox` is gone from gtk4 anyway |

**One binding away** — ranked by how many demos each unlocks. *Upstream*
demos only, with no "revives `create_*`" annotations: nearly every legacy
demo is held back by several missing units, so naming one binding as its
cure misleads. Step 3 has the full blocker sets.

| missing binding | unlocks |
| --- | --- |
| `GtkSignalListItemFactory` (+ `GtkTreeExpander`) | the `Lists/…` demos whose data is plain Ada: `Selections`, `Colors`, `Words`, `Characters`, `Clocks`, `Weather`, plus the awaited `GtkColumnView` demo — and the Step 1 selector itself. Row rendering only; `Lists/Settings`, `Lists/Alternative Settings`, `Lists/Application launcher` and `Lists/File browser` additionally need their data sources (see the list-model row below) |
| `Gtk.DrawingArea` | `Drawing Area`, `Masking`, `Pango/Rotated Text`, `Pango/Text Mask`, and the substrate for most `Path/…` demos |
| `Gtk.Image` / `Gtk.Picture` (+ `GdkPixbuf` or `Gdk.Texture` loading) | `Images`, `Image Scaling`, `Image Filtering`, `Cursors`, the `Paintable/…` family, `Icon View/…` |
| `Gtk.EventController` + the `Gtk.Gesture*` family | `Gestures`, `Paint`, `Text View/Hypertext`, `Constraints/Interactive Constraints` |
| `GtkDragSource` + `GtkDropTarget` | `Drag-and-Drop`; part of `Pickers and Launchers`; and `Clipboard`, whose demo image is a drag source (that one also needs `Gtk.Image`). Both are absent from `contrib/data.py` — and they, not the generic `Gtk.EventController` / gesture family, are what gtk4 DnD is built from. The content side they need is already generated: `Gdk.Content_Provider`, `Gdk.Content_Formats`, `Gdk.Drag`, `Gdk.Drop` |
| `Gtk.ListBox` + `Gtk.ListBoxRow` | `List Box/Complex`, `List Box/Controls` |
| `Gtk.Stack` (+ `StackSwitcher`, `StackSidebar`) | `Stack`, `Stack Sidebar` |
| `Gtk.ShortcutController` | `Shortcuts` (`shortcut_triggers.c`) — the `Gtk.Shortcut` / `Shortcut_Action` / `Shortcut_Trigger` half is already bound |
| `Gtk.SizeGroup` | `Size Groups` |
| `Gtk.Overlay` | `Overlay/Interactive Overlay`, `Overlay/Decorative Overlay`, `Overlay/Transparency` (`Gtk.Overlay_Layout` is bound, the widget is not) |
| `Gtk.Revealer` | `Revealer`, `Read More` |
| `Gtk.FlowBox` (+ `Gtk.FlowBoxChild`) | `Flow Box` |
| `Gtk.SearchEntry` (+ `Gtk.SearchBar`) | `Entry/Search Entry`, and would enable upstream's search box in our own shell |
| `Gtk.HeaderBar` | `Header Bar` |
| `Gtk.InfoBar` | `Info Bars` |
| `Gtk.AspectFrame` | `Aspect Frame` |
| `Gtk.Assistant` | `Assistant` |
| `Gtk.Spinner` | `Spinner` |
| `Gtk.Scale` (+ `Gtk.Range`) | `Scales` |
| `Gtk.LevelBar` / `Gtk.ProgressBar` | no upstream demo of its own; kept here because both are one-class bindings that several legacy demos wait on |
| `Gtk.LinkButton` | `Links` |
| `Gtk.IconView` | `Icon View/Icon View Basics`, `Icon View/Editing and Drag-and-Drop` (the latter drags through `GtkIconView`'s own model-drag API, not the DnD controllers) |
| `Gtk.PasswordEntry` | `Entry/Password Entry` |
| `Gtk.Dialog` / `Gtk.MessageDialog` | `Dialogs`, `Error States` |

**Needs a substantial new area**

| area | demos |
| --- | --- |
| `Gsk.Path` + `Gsk.Stroke` + `Gsk.PathBuilder` | `Path/Fill and Stroke`, `Maze`, `Spinner`, `Sweep`, `Text`, `Walk`, `Path Explorer` (7) |
| `Gsk.Transform` (+ the `Gtk.Fixed` widget) | `Fixed Layout/Cube`, `Fixed Layout/Transformations`. `Gtk.Fixed_Layout` is bound, but both demos position children through `Gsk.Transform`, and there is no `Gsk` binding in the tree at all — no generated sources, no `contrib/binding/packages/` entry, nothing in `contrib/data.py`. A new area, shared with the `Gsk.Path` row. Our own `create_fixed` is *not* in this group: it only calls `Gtk.Fixed.Put` |
| `GtkConstraint` + `GtkConstraintGuide` (see the `GtkConstraintLayout.toml` entry above) | `Constraints/Simple Constraints`, `Interactive`, `VFL`, `Builder` (4) |
| `Gtk.GLArea` + GSK renderers | `OpenGL/Gears`, `OpenGL Area`, `Shadertoy` |
| `GtkMediaStream` / `GtkVideo` | `Video Player`, `Paintable/Media Stream` |
| `Gtk.PrintOperation` + `PageSetup` + `PrintSettings` (+ `Gtk.PrintContext`, `Gtk.PaperSize`) | `Printing/Printing` |
| `Gdk.Paintable` implementable from Ada | `Paintable/Simple`, `Animated`, `Emblems`, `SVG`, `Symbolic` (5) |
| `Gtk.LayoutManager` subclassing from Ada (see the `GtkLayoutManager.toml` entry above) | `Layout Manager/Transition`, `Layout Manager/Transformation` |
| font introspection (`Pango` attribute iterators, `GtkFontChooser`) | `Pango/Font Explorer`, `Pango/Font Rendering` |
| `GAppInfo` / `GSettings` / `GFile` list models | `Lists/Application launcher`, `Lists/Settings`, `Lists/Alternative Settings`, `Lists/File browser` (each *also* needs the list-item factory from "One binding away") |
| `Gtk.FileDialog`, `Gtk.FileLauncher`, `Gtk.UriLauncher`, `Gtk.PrintDialog` (+ `GtkDropTarget`) | `Pickers and Launchers` — a picker/launcher demo, not a list-model one. All four classes are absent from `contrib/data.py` altogether. Its colour and font halves are already bound (`Gtk.ColorDialog` / `Gtk.ColorDialogButton`, `Gtk.FontDialog` / `Gtk.FontDialogButton`), so a colour+font subset can land first and grow |
| benchmark harness | `Benchmark/Fishbowl`, `Frames`, `Scrolling`, `Themes` |

**Deliberately not ported**

- `Lists/Minesweeper`, `Peg Solitaire`, `Sliding Puzzle` — games; low
  binding-coverage value for the effort.
- `Shortcuts Window` — `GtkShortcutsWindow` is deprecated since 4.18.

### Step 3 — GtkAda-only demos still commented out

These `create_*` packages exist in `gtkada_demo/` but are commented out of
`main_windows.adb`. Each belongs under a `GtkAda/…` category once revived.

Four things make one unbuildable, and a package usually hits more than one:

1. a unit disabled in `contrib/data.py` (marked `--`) or absent from it, and
   therefore not generated;
2. a unit that exists only under `src/gtk3`, which `src/gtkada.gpr` does not
   list in `Source_Dirs` (`"generated"` and `"."` only);
3. a `.ui` / `.xml` loaded at run time whose widget classes or signals gtk4
   removed — these do not show up in the `with` clauses;
4. a project-level dependency commented out of `gtkada_demo.gpr`.

Spelling in `contrib/data.py` is a rough signal of "wait for the binding" vs
"rewrite the demo": a disabled entry with a dot (`--Gtk.Revealer`) is usually
a class gtk4 still has, one without (`--GtkContainer`, `--GtkMenu`) one gtk4
removed. A convention, not a guarantee — `# Bound through manual_binding`
entries are exceptions, and `--GtkFileChooserButton` is camel-case although
gtk 4.22 still has the class. Check the gtk4 documentation first.

**Blocked on bindings only.** Each row is the *whole* blocker set for that
package. Markers: *(gtk3)* = the unit exists only under `src/gtk3`;
*(absent)* = no `contrib/data.py` entry at all; unmarked = `--` in
`contrib/data.py`.

  | package | not generated / not visible |
  | --- | --- |
  | `create_cairo` | through `Testcairo_Drawing`: `Gtk.Drawing_Area`, `Gtk.Print_Context`, `Gtk.Print_Operation`, `Gdk.Pixbuf` *(gtk3)*, `Gdk.Cairo` *(gtk3)*, `Gtkada.Printing` *(gtk3)*. `Cairo` itself is bound |
  | `create_calendar` | `Gtk.Calendar` |
  | `create_clipboard` | `Gtk.Clipboard`, `Gtk.Selection_Data` *(absent)*, `Gtk.Hbutton_Box` *(absent)*, `Gtk.Image`, `Gdk.Pixbuf` *(gtk3)*, `Gdk.Property` *(gtk3)*, `Gdk.Types` *(gtk3)*, `Gtkada.Handlers` *(gtk3)*. gtk4 replaced the first three outright — `Gdk.Clipboard` (bound) plus content providers is the target API, so this is a rewrite |
  | `create_css_accordion`, `create_css_editor` | `Gtk.Container`, which gtk4 removed; the CSS provider, style context, and style provider bindings are available |
  | `create_cursors` | `Gtk.Drawing_Area`, `Gdk.Window` (gtk4: `Gdk.Surface`, bound), `Gdk.Device_Manager` *(absent; gone from gtk4)*, `Gtk.Handlers` *(gtk3)* |
  | `create_dnd` | `Gtk.Dnd` *(gtk3)*, `Gdk.Dnd` *(gtk3)*, `Gdk.Drag_Contexts` (`--GdkDragContext`), `Gtk.Target_List` *(absent)*, `Gtk.Selection_Data` *(absent)*, `Gtk.Image`, `Gdk.Window` (gtk4: `Gdk.Surface`), `Gdk.Pixbuf` *(gtk3)*, `Gdk.Types` *(gtk3)*, `Gtk.Handlers` *(gtk3)*. gtk4 replaced the whole DnD API with `GtkDragSource` / `GtkDropTarget`, neither in `contrib/data.py`, over the already-generated `Gdk.Content_Provider`: a rewrite, not a port |
  | `create_entry` | `Gtk.Combo_Box_Text`, `Gtk.Level_Bar`, `Gtk.Search_Entry`, `Gtk.Handlers` *(gtk3)*, plus `Common` (see below) |
  | `create_file_chooser` | `Gtk.File_Chooser` (the interface) and `Gtk.File_Chooser_Button`; both deprecated upstream since 4.10, so the revival should target `GtkFileDialog` — itself unbound and absent from `contrib/data.py`, i.e. a new binding of its own |
  | `create_fixed` | `Gtk.Fixed`. It calls only `Gtk.Fixed.Put`, so this really is its whole blocker — it is *not* part of the `Gsk.Transform` group |
  | `create_flow_box` | `Gtk.Flow_Box`, `Gtk.Flow_Box_Child`, `Gtk.Combo_Box`, `Gtk.Combo_Box_Text`, `Gtk.Handlers` *(gtk3)* |
  | `create_font_chooser` | `Gtk.Font_Chooser_Widget` |
  | `create_gestures` | `Gtk.Gesture`, `Gtk.Gesture_Long_Press`, `Gtk.Gesture_Zoom`, `Gtk.Drawing_Area` |
  | `create_gl` | `Gtk.GLArea`, `Gtk.GRange` (`--Gtk.Range`), `Gtk.Scale`. Its `Epoxy`, `OpenGL` and `Create_GL.GLSL` dependencies are demo-local sources and fine |
  | `create_link_buttons` | `Gtk.Link_Button`, `Gtk.Handlers` *(gtk3)* |
  | `create_notebook` | `Gtk.Combo_Box_Text`, `Gtk.Image`, `Gdk.Pixbuf` *(gtk3)*, `Gtk.Handlers` *(gtk3)*, plus `Common`. `Gtk.Notebook` itself is bound |
  | `create_opacity` | `Gtk.Scale`, plus `Common` |
  | `create_pixbuf` | `Gtk.Drawing_Area`, `Gtk.Image`, `Gdk.Pixbuf` *(gtk3)*, `Gdk.Cairo` *(gtk3)*, `Gtkada.Handlers` *(gtk3)* |
  | `create_print` | `Gtk.Print_Operation`, `Gtk.Print_Context`, `Gtk.Page_Setup`, `Gtk.Paper_Size`, `Gtkada.Printing` *(gtk3)* |
  | `create_progress` | `Gtk.Progress_Bar`, `Gtk.Combo_Box_Text`, `Gtk.Alignment` (removed in gtk4 — use the child's `Halign` / `Valign`), `Gtkada.Handlers` *(gtk3)*, plus `Common` |
  | `create_range` | `Gtk.Scale`, `Gtk.Scale_Button`, `Gtk.Scrollbar`, `Gtk.Volume_Button` |
  | `create_revealer` | `Gtk.Revealer` — genuinely a single binding |
  | `create_size_groups` | `Gtk.Size_Group`, `Gtk.Handlers` *(gtk3)* |
  | `create_spinners` | `Gtk.Spinner`, plus `Common` |
  | `create_stack` | `Gtk.Stack`, `Gtk.Stack_Switcher` — no other blocker |

  Only `create_fixed`, `create_gestures`, `create_gl`, `create_revealer` and
  `create_stack` are blocked purely on bindings Step 2 already tracks. Every
  other package wants at least one unit no upstream demo needs and Step 2
  therefore never schedules (`Gtk.Calendar`, the combo/scale/scrollbar
  family, `Gtk.Container`, `Gtk.Clipboard`, `Gtk.Selection_Data`,
  `Gtk.Target_List`, the `Gdk.*` gtk3 units, `Gtkada.Printing`, the handler
  packages), so these need scheduling as ports in their own right.
  `Gtk.Font_Chooser_Widget` is the near miss — Step 2's font introspection
  row names `GtkFontChooser`, which would carry it.

**`Common` is a shared blocker.** `gtkada_demo/common.ads` withs `Gtk.Dialog`
(`--`) and `Gtk.Handlers` *(gtk3)*, so it does not build. All ten of its
users are commented out today — `create_builder`, `create_entry`,
`create_gtkada_builder`, `create_main_loop`, `create_notebook`,
`create_opacity`, `create_progress`, `create_spinners`,
`create_task_monitor`, `create_test_idle` — which is why nothing notices.
Port `Common` before any of them.

**Blocked on a port of their own** rather than on a Step 2 binding. Each was
checked against its `with` clauses *and*, where it loads one, its `.ui` /
`.xml`.

  | package | blocked on | what it needs |
  | --- | --- | --- |
  | `create_application` | `Gtk.Menu`, `Gtk.Menu_Tool_Button`; `application.ui` uses `GtkToolbar`, `GtkToolButton`, `GtkMenuToolButton`, `GtkSeparatorToolItem`, `GtkInfoBar`, `GtkStatusbar` | rebuild the menu from `Glib.Menu` + `Gtk.Popover_Menu_Bar` (both bound), drop the tool button, and rewrite `application.ui`: the four tool classes are gone from gtk4, so `Gtk.Builder` cannot instantiate them at all. `GtkInfoBar` / `GtkStatusbar` survive in 4.22 (deprecated) and the builder can still create them, but are unbound the moment Ada touches one. The Ada side also calls `Win.Add` and `Gtk_Menu_Tool_Button`, neither of which gtk4 has. `menus.ui` is a plain `GMenu` model and carries over unchanged |
  | `create_builder` | `Gtk.Handlers` *(gtk3)*, `Common`; `gtkbuilder_example.xml` uses `GtkVBox`, `GtkHBox`, `GtkTable` and connects `delete_event` / `destroy` | port the callbacks to the generated `On_*` setters and rewrite the XML: box and table classes went in gtk4 (`GtkBox` with an orientation, `GtkGrid`), `delete_event` becomes `GtkWindow::close-request`, `GtkWidget::destroy` is gone |
  | `create_gtkada_builder` | `Gtkada.Builder` *(gtk3)*, `Common`; the same `gtkbuilder_example.xml` plus `gtkbuilder_custom_widget.xml` (`GtkVBox`, `GtkLinkButton`) | a gtk4 port of `Gtkada.Builder`, or a rewrite onto `Gtk.Builder` + `Gtk.Builder_Cscope` (both bound); the same XML rewrite; and the body casts `Get_Object` results to `Gtk_Hbox`, which gtk4's `Gtk.Box` no longer exports. Worth checking rather than assuming: whether `Gtk.Builder` can still instantiate the unbound `GtkLinkButton` through its type lookup |
  | `create_main_loop` | `Gtk.Main.Main`, `Gtk.Main.Main_Quit`, `Common` | gone from gtk4: the generated `Gtk.Main` keeps only the version accessors and `Init`. Re-think the demo around `Glib.Main` or `Gtk.Application`, or retire it — its subject is the recursive `gtk_main` gtk4 removed |
  | `create_sources` | `Gtkada.Handlers` *(gtk3)* | as `create_builder`; it loads no UI file |
  | `create_task_monitor` | `Gtk.Progress_Bar`, `Gtk.Handlers` *(gtk3)*, `Common`, and `Task_Worker` from `gtkada_demo/task_project/src` | the binding and the handler port, *and* the task project: `gtkada_demo.gpr` has `with "task_project/task_project"` commented out under a `TRANSITION` marker and `Source_Dirs` set to `"./"`, so `Task_Worker` is out of the source closure. Re-enable the project or fold the worker into `gtkada_demo/` |
  | `create_test_idle` | `Gtk.Radio_Button`, `Gtk.Handlers` *(gtk3)*, `Common` | gone from gtk4: `Gtk.Check_Button` with `Set_Group` replaces the radio group, plus the handler port |

  The `Gtk.Handlers` / `Gtkada.Handlers` port runs through four of these
  seven, ten more of the binding-blocked packages, and `Common` itself. It
  is by some distance the single most valuable thing to settle — either
  revive those packages for gtk4, or rule that demos use the generated
  `On_*` setters from now on.

**GtkAda-specific components**, to be scheduled with those components' own
gtk4 ports if we decide to bind them: `create_canvas`, the nine
`create_canvas_view_*`, `create_mdi`, `create_splittable`,
`create_gtkada_dialog`, `libart_demo`, `test_rtree`. `create_gtkada_dialog`
belongs here rather than under "one binding away": beyond `Gtk.Dialog` /
`Gtk.Message_Dialog` it withs `Gtkada.Dialogs` *(gtk3)*, which needs its own
gtk4 port.

**Gone with gtk4**, to be deleted rather than revived:
`create_file_selection`, `create_selection` (gtk3 selection API).

## To do as we translate

- In case of message reporting missing "Unchecked_To_X", uncomment the
  corresponding functions in src/gtk-arguments.ads and src/gtk-arguments.adb.

## To do (globally)

- Reintroduce Gdk bindings
- Reintroduce Gtk bindings
- Reactivate tests

- Makefile.in:
  - reactivate tools
  - reactivate docs (once updated)

- Fix misc.c (look for the "TRANSITION" string)

- Re-enable warnings for obsolescent packages (disabled in Debug mode for now)

- Automatically generate GError params for functions that write to errors (work item #93)

- Host the nine namespace-level `gtk_accelerator_*` functions
  (`accelerator_parse`, `accelerator_parse_with_keycode`,
  `accelerator_name`, `accelerator_name_with_keycode`,
  `accelerator_get_label`, `accelerator_get_label_with_keycode`,
  `accelerator_get_accessible_label`,
  `accelerator_get_default_mod_mask`, `accelerator_valid`).
  No TOML suppresses them: they are simply not hosted by any package, since
  gtk4 has no `GtkAccelGroup` to put them in, so a `[[function]]` host has to
  be chosen for them. Their `GdkModifierType` dependency is bound
  (`Gdk.Enums`), so nothing else blocks them.

- Give the remaining `Generic_Internal_Discrete_Property` instantiations an
  exact-width helper, as the bitfield ones now have. That package reads and
  writes through `ada_g_object_get_ulong` / `ada_g_object_set_ulong`, but
  GObject lcopies a `G_TYPE_ENUM` property through a `gint *` (and a
  `G_TYPE_UINT` one through a `guint *`) and collects it in the matching
  width, so the C types do not line up on LP64. It goes unnoticed on x86-64
  and aarch64 because the Ada type is 32 bits wide and GNAT's unchecked
  conversion from `Gulong` keeps the low-order half, which is the half
  `g_object_get` wrote; a big-endian LP64 target would read the untouched half
  instead. The fix has the same shape as
  `Generic_Internal_Flags_Property`: exact-width helpers in `misc.c`, and
  instantiations (or a generator) that pick the one matching the property's
  GObject representation.

## To do (package by package)

gtk-handlers.ads:

- when done, fix gtkada_demo/create_tree_view.adb

GBytes.toml:

- fix binding for functions that have gconstarray as parameters
- Once Atom_Arrays bound, bind: get_data, unref_to_array, unref_to_data
  - TODO #135: once unref_to_data bound, reactivate GFile.Load_Bytes unit test

GDateTime:

- when done, reactivate bindings in GFileInfo (TODO #135)

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

GtkApplication.toml:

- when done, reactivate bindings in GtkWindow.toml

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

GtkColumnView - bound, awaiting:

- unit tests
- demo

GtkDropDown.toml

- `model` property is not binded because the generator can't generate
  the proper methods. Also it is an open question whether we really nned it.

### Circular dependencies still to break

The generator supports `limited with` (`[[extra.with_spec]]` with
`limited = true`, paired with a plain `[[extra.with_body]]`).
What circular dependencies are left, and why:

- **GtkTextIter.toml** (`gtk_text_iter_get_buffer`,
  `gtk_text_iter_get_child_anchor`) and **GtkTextMark.toml**
  (`gtk_text_mark_get_buffer`). Technically easy, but these operations are
  currently re-exposed by hand from `GtkTextBuffer.toml`, as
  `Get_Buffer (Iter : ...)` / `Get_Buffer (Mark : ...)`. Re-enabling them
  means *removing* those manual re-expositions in the same go, or callers
  meet two versions of the same operation and an ambiguity. That touches
  three TOMLs, two generated packages, the `text-iter` / `text-buffer` /
  `text-view` tests and `gtkada_demo/`, so it wants its own work item.

- **GtkATContext.toml** (`gtk_at_context_get_accessible`,
  `gtk_at_context_get_accessible_role`). This TOML is inert: `contrib/data.py`
  lists `"--Gtk.ATContext"`, so the type is not generated at all and
  `Gtk.Atcontext` is hand-written in `src/gtk-atcontext.ads`. Fixing it means
  re-enabling generation for the type and retiring the hand-written package,
  which is a much larger job than a `limited with`. Do not re-diagnose this
  one as a plain cycle.

- **GApplication.toml** (`g_application_add_main_option`,
  `g_application_add_option_group`). The cycle is real — `Glib.Option` withs
  `Glib.Application` — but a limited view gives incomplete types, which suits
  a tagged return and not the by-copy `GOptionFlags` / `GOptionArg` scalars
  and the `GOption_Group` proxy these take. The better fix is probably to
  remove `Glib.Option`'s dependency on `Glib.Application` instead.

- **GtkCellLayout.toml** (virtual methods, circular with `Gtk.Cell_Area`) and
  **GtkTreeSelection.toml** (`gtk_tree_selection_get_tree_view`, returning a
  `Gtk_Widget`). Both belong to the cell-renderer / tree-view family, which is
  `pragma Obsolescent` in the gtk4 bindings. Not worth churning.

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
- `GtkFontDialog.choose_font_and_features` — multiple complex out
  parameters; suppressed for now.
- `GtkFontDialog.{get,set}_filter` — needs `GtkFilter`; suppressed
  with `bind = false`.
- `GdkRGBA.{equal,hash}` — use `gconstpointer`, which the generator
  cannot map yet; suppressed with `bind = false`.
- `Glib.G_Async_Result` (in `src/glib.ads`) is currently a hand-written
  opaque proxy. Revisit and replace it with the generated binding once
  the full `GAsyncResult` interface is reactivated.

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
  `Glib.Menu` rather than `GtkBuilder`).

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

## GtkColumnView, GtkSignalListItemFactory (work item #155)

`GtkColumnView` was bound earlier but could not be demonstrated or
tested, because the only factory an Ada application can use was missing.
`GtkBuilderListItemFactory` — the other one — builds its cells from a
`.ui` fragment, so without `GtkSignalListItemFactory` the sole way to
populate a column view from Ada was to embed XML in a string literal.

`Gtk.Signal_List_Item_Factory` is now enabled in `contrib/data.py`. It
needed no glue beyond retyping the argument of its four signals
(`setup`, `bind`, `unbind`, `teardown`) in
`contrib/binding/packages/GtkSignalListItemFactory.toml`: the GIR
declares it as a bare `GObject*` although it is always a
`GtkListItem*`, and a `ctype` override on each `[[method.parameter]]`
hands handlers a `Gtk_List_Item` directly. No `Unchecked_To_*`
instantiation was required — `src/gtk-arguments.ads` already carries the
generic `Unchecked_To_Object` that the generated marshaller calls, that
family being needed only for enum and flag signal parameters.

`Gtk.String_List` and `Gtk.String_Object` are enabled too, with no TOML
at all; they are the trivial list model that every upstream column-view
example uses for its data. (The `"StringList"` entry at
`contrib/data.py:931` is unrelated — that map is keyed by
generator-internal type aliases used by `return =` overrides, not by C
type name, and `Gtk.String_List` and `Gtk.Enums.String_List` coexist
happily.)

**Beware `c:type="gpointer"`.** `gtk_list_item_get_item` is declared in
the GIR as `<type name="GObject.Object" c:type="gpointer"/>`, and the
generator honours the `c:type`, so it generated a `System.Address`
return that no caller could use without an `Unchecked_Conversion`. A
`return = "GObject*"` override fixes it. The same affliction touched
`gtk_column_view_cell_get_item`, `gtk_column_view_row_get_item` and
`gtk_single_selection_get_selected_item`; all four now have a TOML and
return a `Glib.Object.GObject`. Note that overriding `return` makes the
generator drop the GIR's `@return` documentation, so each override
restores it with a `[method.doc]` `extend = true` block.

Sorting a column view is a chain rather than a setter, and the shape of
it is easy to get wrong: there is **no** `Gtk.Column_View.Set_Sorter`.
Each column takes its own sorter via
`Gtk.Column_View_Column.Set_Sorter`; the view then exposes through
`Get_Sorter` a read-only sorter reflecting the header the user last
clicked, and *that* one must be installed on a `Gtk_Sort_List_Model`
wrapped around the data. Mind the ownership while you do it:
`gtk_column_view_get_sorter` is transfer-none whereas
`gtk_sort_list_model_new` is transfer-full on both its arguments, so the
sorter needs an explicit `Ref` before being handed over — without it the
sorter is finalised along with the sort model and the program crashes.

`gtkada_demo/create_column_view.adb` shows the lot, and
`testsuite/tests/column-view` covers construction, the model round trip,
the column list, the boolean properties, the sorting chain, and — the
case that earns its keep — a factory that is realized and pumped so that
`setup` and `bind` are proved to fire and to receive the right items.
Note that presenting the window is enough to create the cells
synchronously, so a test that resets its counters *after* `Present` will
see nothing.

Left for later, on demand:

- The demo has two plain label columns. A third, interactive column (a
  recycled `Gtk_Check_Button`, say) would demonstrate why `unbind`
  exists, which is the part of the factory protocol an inattentive
  reader gets wrong.
- `Gtk.String_List.Find` is GTK 4.18+; avoid it if older toolchains must
  build.

## GtkSnapshot

Re-enable the matching `bind = false` entries in
`GtkSnapshot.toml` once the required type is bound:

- gradient nodes (`append_conic/linear/radial_gradient`,
  `append_repeating_linear/radial_gradient`) — need `Gsk.ColorStop`.
- `append_border` / `append_inset_shadow` / `append_outset_shadow` /
  `push_rounded_clip` — need `Gsk.RoundedRect`.
- `append_fill` / `append_stroke` / `push_fill` / `push_stroke` — need
  `Gsk.Path`, `Gsk.FillRule`, `Gsk.Stroke`.
- `append_node` / `to_node` / `transform` — need `Gsk.RenderNode` /
  `Gsk.Transform`.
- `append_scaled_texture` — needs `Gsk.ScalingFilter`.
- `push_blend` / `push_mask` / `push_composite` / `push_isolation` /
  `push_component_transfer` / `push_shadow` / `push_gl_shader` — need the
  corresponding Gsk enums / types.

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
