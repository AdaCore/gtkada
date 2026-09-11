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

Goal: bring `gtkada_demo` closer, in both presentation and coverage, to the
demo that ships with gtk4 (`gtk-4.22.2/demos/gtk-demo/`)

Decisions:

- The selector becomes a **two-level tree built on `Gtk.List_View` +
  `Gtk.Tree_List_Model`**, i.e. the same machinery upstream's own sidebar
  uses, rather than a `Gtk_Tree_Store` fed into the (gtk3-era, cell-renderer
  based) `Gtk_Tree_View`. The demo's own chrome should showcase the gtk4
  list API, not the API gtk4 deprecated.
- Demo names follow **upstream's `Category/Title` strings verbatim**
  (`Text View/Markup`, `Lists/Colors`, `Theming/CSS Basics`, …) so coverage
  against upstream can be diffed mechanically, plus extra top-level
  categories (`GtkAda/…`) for the GtkAda-only demos that have no upstream
  counterpart (Canvas View, MDI, `Gtkada.Dialogs`, …).
- The rest of the shell is **left as it is**: a demo frame plus the help
  panel below it. Upstream's source-code notebook tab, its search entry and
  its "run the demo in its own toplevel" behaviour are explicitly out of
  scope for now — the effort goes into demo content instead. (Noted here in
  case we change our mind: the source viewer would only need
  `Gtk.Notebook` + `Gtk.Text_View`, both bound; the search box needs
  `Gtk.Search_Entry`, which is not.)

### Step 1 — categorised tree selector

1. **Bind the two classes the new selector needs.** Both are small, leaf
   classes with no unbound dependencies; neither appears in
   `contrib/data.py` today.
   - `GtkSignalListItemFactory` — `new` plus the `setup` / `bind` /
     `unbind` / `teardown` signals, each carrying a `GtkListItem` (already
     bound as `Gtk.List_Item`). Without it there is *no* usable way to
     populate a `Gtk_List_View` from Ada: the only bound factory is
     `Gtk.Builder_List_Item_Factory`, which drives the item widgets through
     `Gtk.Expression` property lookups, and `Glib.Object` has no
     `Install_Property`, so an Ada-defined item type cannot expose the
     properties such a factory would bind to.
   - `GtkTreeExpander` — the widget that draws the expander arrow and the
     indentation inside a list item, and that ties the row to its
     `Gtk.Tree_List_Row`. Fallback if we want to defer it: our tree is two
     levels and static, so an indent box plus a toggle calling
     `Gtk.Tree_List_Row.Set_Expanded` would do, at the cost of not looking
     like the rest of gtk4.

   This is the list-widget infrastructure that every `Lists/…` demo and
   `Gtk.Column_View`'s awaited demo (see "GtkColumnView - bound,
   awaiting" above) needs, so it is worth doing first regardless. It
   supplies row *rendering* only, not data: the demos whose model is
   itself unbound — `Lists/Settings`, `Lists/Alternative Settings`,
   `Lists/Application launcher`, `Lists/File browser` — stay blocked on
   the `GAppInfo` / `GSettings` / `GFile` work listed under "Needs a
   substantial new area" in Step 2.

2. **Introduce a `Demo_Item` GObject.** A `Glib.Object.GObject_Record`
   derivative registered through `Ada_GObject_Class` (the pattern
   `create_custom_widget.adb` already uses), carrying the title, the
   category, the `Run` access-to-procedure and the `Help` function. The
   registry currently spelled as the `Demos` array in `main_windows.adb`
   moves into its own package (say `Demo_Registry`), with each entry naming
   its full upstream-style path; the package derives the category list from
   those paths so adding a demo stays a one-line change.

3. **Rebuild the selector.** `Glib.List_Store` of categories →
   `Gtk.Tree_List_Model` with a create-child-model function returning each
   category's `Glib.List_Store` of demos → `Gtk.Single_Selection` →
   `Gtk.List_View` with a signal factory that builds a
   `GtkTreeExpander` + `Gtk.Label` per row. `Gtk.List_Store`,
   `Gtk.Tree_View`, `Gtk.Tree_View_Column` and `Gtk.Cell_Renderer_Text`
   then disappear from `main_windows.adb` — they stay demonstrated by the
   `Tree View/…` demos, which is where they belong.

4. **Write a new selection callback.** The existing
   `On_Selection_Changed` cannot be carried over unchanged: it takes a
   `Gtk_Tree_Selection`, calls `Get_Selected` for a `Gtk_Tree_Iter`, and
   reads an index back out of the model through `Demo_Column`. None of
   those three survive the move. The replacement hangs off
   `Gtk.Single_Selection`'s `notify::selected-item` rather than
   `Gtk_Tree_Selection.On_Changed`, and:

   - unwraps the row. `Get_Selected_Item` on a selection layered over a
     `Gtk.Tree_List_Model` returns the `Gtk.Tree_List_Row` wrapper, not
     the underlying object; `Gtk.Tree_List_Row.Get_Item` yields the
     model item proper. It also returns null when nothing is selected,
     which is the new stand-in for the old `Null_Iter` guard.
   - copes with category rows. The tree exposes categories alongside
     demos, and selecting one must not blank the frame. Cheapest fix is
     to make them unselectable up front: the factory's `bind` handler
     can call `Gtk.List_Item.Set_Selectable (False)` when
     `Gtk.Tree_List_Row.Is_Expandable` holds, so the selection never
     lands on a category. Keep a
     defensive early return for anything that is not a `Demo_Item`
     regardless.
   - reads `Run` and `Help` straight off the `Demo_Item` instead of
     indexing the `Demos` array — the item *is* the registry entry, so
     the `Index in Demos'Range` tests go away with it.

   Only the tail of the old body carries over verbatim: clear the frame,
   call `Run (Demo_Frame)`, and set the help label from `To_Markup`.

5. **Remap the existing demos onto the upstream taxonomy.** Current
   selector → new path:

   | today | becomes |
   | --- | --- |
   | Boxes | `Layout/Boxes` *(GtkAda-only; upstream has no box demo)* |
   | Buttons | `Buttons/Buttons` *(GtkAda-only)* |
   | Check Buttons | `Buttons/Check Buttons` *(GtkAda-only)* |
   | Color Chooser | `Pickers and Launchers` *(partial)* |
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
binding the demo needs is already generated; the annotation names the
missing binding otherwise. To be re-checked as bindings land — this is a
snapshot, not a contract.

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

**One binding away** — ranked by how many demos each unlocks

| missing binding | unlocks |
| --- | --- |
| `GtkSignalListItemFactory` (+ `GtkTreeExpander`) | the `Lists/…` demos whose data is plain Ada: `Selections`, `Colors`, `Words`, `Characters`, `Clocks`, `Weather`, plus the awaited `GtkColumnView` demo — and the Step 1 selector itself. This is row rendering only; `Lists/Settings`, `Lists/Alternative Settings`, `Lists/Application launcher` and `Lists/File browser` additionally need their data sources (see `GAppInfo` / `GSettings` / `GFile` below) |
| `Gtk.CssProvider` (+ `Gtk.StyleContext`) | `Theming/CSS Basics`, `CSS Accordion`, `Multiple Backgrounds`, `Animated Backgrounds`, `Shadows`, `CSS Blend Modes`, `Style Classes` (7), and revives `create_css_accordion` / `create_css_editor` |
| `Gtk.EventController` + the `Gtk.Gesture*` family | `Gestures`, `Drag-and-Drop`, `Paint`, `Peg Solitaire`, `Sliding Puzzle`, `Constraints/Interactive Constraints`, `Text View/Hypertext`, `Icon View/Editing and Drag-and-Drop` |
| `Gtk.DrawingArea` | `Drawing Area`, `Masking`, `Pango/Rotated Text`, `Pango/Text Mask`, and the substrate for most `Path/…` demos |
| `Gtk.Image` / `Gtk.Picture` (+ `GdkPixbuf` or `Gdk.Texture` loading) | `Images`, `Image Scaling`, `Image Filtering`, `Cursors`, the `Paintable/…` family, `Icon View/…` |
| `Gtk.ListBox` + `Gtk.ListBoxRow` | `List Box/Complex`, `List Box/Controls` |
| `Gtk.Stack` (+ `StackSwitcher`, `StackSidebar`) | `Stack`, `Stack Sidebar`, revives `create_stack` |
| `Gtk.ShortcutController` | `Shortcuts` (`shortcut_triggers.c`) — the `Gtk.Shortcut` / `Shortcut_Action` / `Shortcut_Trigger` half is already bound |
| `Gtk.SizeGroup` | `Size Groups`, revives `create_size_groups` |
| `Gtk.Overlay` | `Overlay/Interactive Overlay`, `Overlay/Decorative Overlay`, `Overlay/Transparency` (`Gtk.Overlay_Layout` is bound, the widget is not) |
| `Gtk.Revealer` | `Revealer`, `Read More`, revives `create_revealer` |
| `Gtk.FlowBox` | `Flow Box`, revives `create_flow_box` |
| `Gtk.SearchEntry` (+ `Gtk.SearchBar`) | `Entry/Search Entry`, and would enable upstream's search box in our own shell |
| `Gtk.HeaderBar` | `Header Bar` |
| `Gtk.InfoBar` | `Info Bars` |
| `Gtk.AspectFrame` | `Aspect Frame` |
| `Gtk.Assistant` | `Assistant` |
| `Gtk.Spinner` | `Spinner`, revives `create_spinners` |
| `Gtk.Scale` (+ `Gtk.Range`) | `Scales`, revives `create_range` |
| `Gtk.LevelBar` / `Gtk.ProgressBar` | revives `create_progress` |
| `Gtk.LinkButton` | `Links`, revives `create_link_buttons` |
| `Gtk.IconView` | `Icon View/Icon View Basics` |
| `Gtk.PasswordEntry` | `Entry/Password Entry` |
| `Gtk.Dialog` / `Gtk.MessageDialog` | `Dialogs`, `Error States`, revives `create_gtkada_dialog` |

**Needs a substantial new area**

| area | demos |
| --- | --- |
| `Gsk.Path` + `Gsk.Stroke` + `Gsk.PathBuilder` | `Path/Fill and Stroke`, `Maze`, `Spinner`, `Sweep`, `Text`, `Walk`, `Path Explorer` (7) |
| `Gsk.Transform` (+ the `Gtk.Fixed` widget) | `Fixed Layout/Cube`, `Fixed Layout/Transformations`, and `create_fixed`. `Gtk.Fixed_Layout` is bound, but both demos position their children through `Gsk.Transform`, and there is no `Gsk` binding in the tree at all — no generated sources, no `contrib/binding/packages/` entry, nothing in `contrib/data.py`. So this is a new area, shared with the `Gsk.Path` row above, not a one-widget job |
| `GtkConstraint` + `GtkConstraintGuide` (see the `GtkConstraintLayout.toml` entry above) | `Constraints/Simple Constraints`, `Interactive`, `VFL`, `Builder` (4) |
| `Gtk.GLArea` + GSK renderers | `OpenGL/Gears`, `OpenGL Area`, `Shadertoy`, revives `create_gl` |
| `GtkMediaStream` / `GtkVideo` | `Video Player`, `Paintable/Media Stream` |
| `Gtk.PrintOperation` + `PageSetup` + `PrintSettings` | `Printing/Printing`, revives `create_print` |
| `Gdk.Paintable` implementable from Ada | `Paintable/Simple`, `Animated`, `Emblems`, `SVG`, `Symbolic` (5) |
| `Gtk.LayoutManager` subclassing from Ada (see the `GtkLayoutManager.toml` entry above) | `Layout Manager/Transition`, `Layout Manager/Transformation` |
| font introspection (`Pango` attribute iterators, `GtkFontChooser`) | `Pango/Font Explorer`, `Pango/Font Rendering` |
| `GAppInfo` / `GSettings` / `GFile` list models | `Lists/Application launcher`, `Lists/Settings`, `Lists/Alternative Settings`, `Lists/File browser`, `Pickers and Launchers` (each *also* needs the list-item factory from "One binding away") |
| benchmark harness | `Benchmark/Fishbowl`, `Frames`, `Scrolling`, `Themes` |

**Deliberately not ported**

- `Lists/Minesweeper`, `Peg Solitaire`, `Sliding Puzzle` — games; low
  binding-coverage value for the effort.
- `Shortcuts Window` — `GtkShortcutsWindow` is deprecated since 4.18.

### Step 3 — GtkAda-only demos still commented out

Independently of upstream parity, these `create_*` packages exist in
`gtkada_demo/` but are still commented out of `main_windows.adb`. Each
belongs under a `GtkAda/…` category once revived:

- Blocked on bindings listed above: `create_cairo`, `create_calendar`,
  `create_clipboard`, `create_cursors`, `create_dnd`, `create_entry`,
  `create_fixed`, `create_flow_box`, `create_font_chooser`,
  `create_gestures`, `create_gl`, `create_link_buttons`, `create_notebook`,
  `create_opacity`, `create_pixbuf`, `create_print`, `create_progress`,
  `create_range`, `create_revealer`, `create_size_groups`,
  `create_spinners`, `create_stack`, `create_css_accordion`,
  `create_css_editor`.
- Blocked on a port of their own rather than on a binding from the
  tables above. Each was checked against its `with` clauses, so these
  are not "to be confirmed" — they are known to need work, and the
  table names what. Two things make a package unbuildable: a unit
  disabled in `contrib/data.py` (marked `--`), or one that only exists
  in `src/gtk3`, which `src/gtkada.gpr` does not list among its
  `Source_Dirs` and which is therefore invisible to the demo.

  | package | blocked on | what it needs |
  | --- | --- | --- |
  | `create_application` | `Gtk.Menu`, `Gtk.Menu_Tool_Button` (both `--` in `contrib/data.py`) | gone from gtk4 outright: rebuild the menu from `Glib.Menu` + `Gtk.Popover_Menu_Bar` (both bound) and drop the tool button |
  | `create_builder` | `Gtk.Handlers` (`src/gtk3` only) | port to the generated `On_*` signal setters |
  | `create_gtkada_builder` | `Gtkada.Builder` (`src/gtk3` only) | a gtk4 port of `Gtkada.Builder`, or a rewrite onto `Gtk.Builder` + `Gtk.Builder_Cscope` (both bound) |
  | `create_main_loop` | `Gtk.Main.Main`, `Gtk.Main.Main_Quit` | gone from gtk4: the generated `Gtk.Main` keeps only the version accessors and `Init`. The demo has to be re-thought around `Glib.Main` or `Gtk.Application`, or retired — its whole subject is the recursive `gtk_main` the toolkit removed |
  | `create_sources` | `Gtkada.Handlers` (`src/gtk3` only) | as `create_builder` |
  | `create_task_monitor` | `Gtk.Progress_Bar` (`--`), `Gtk.Handlers` (`src/gtk3` only) | binding `Gtk.ProgressBar` (already tracked above, where it also revives `create_progress`) plus the handler port |
  | `create_test_idle` | `Gtk.Radio_Button` (`--`), `Gtk.Handlers` (`src/gtk3` only) | gone from gtk4: `Gtk.Check_Button` with `Set_Group` replaces the radio group, plus the handler port |

  The `Gtk.Handlers` / `Gtkada.Handlers` port is the common thread
  through four of the seven, so it is worth settling once — either by
  reviving those packages for gtk4 or by ruling that demos use the
  generated `On_*` setters from now on.
- GtkAda-specific components, to be scheduled with those components'
  own gtk4 ports, if we decide to bind them: `create_canvas`,
  the nine `create_canvas_view_*`, `create_mdi`, `create_splittable`,
  `create_gtkada_dialog`, `libart_demo`, `test_rtree`.
- Gone with gtk4, to be deleted rather than revived:
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

GtkLayoutManager.toml and GtkLayoutChild.toml:

- `gtk_layout_manager_get_widget`, `gtk_layout_manager_allocate`,
  `gtk_layout_manager_measure`, `gtk_layout_manager_get_layout_child`,
  `gtk_layout_child_get_child_widget`, `gtk_layout_child_get_layout_manager`
  are disabled to break a circular dependency between `Gtk.Widget`,
  `Gtk.Layout_Manager` and `Gtk.Layout_Child`. Once the generator gains
  support for `limited with`, these can be re-enabled.

GtkColumnView - bound, awaiting:

- unit tests
- demo

GtkDropDown.toml

- `model` property is not binded because the generator can't generate
  the proper methods. Also it is an open question whether we really nned it.

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
