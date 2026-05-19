------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  Switches between children using tabs.
--
--  <picture> <source srcset="notebook-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkNotebook" src="notebook.png"> </picture>
--  There are many configuration options for `GtkNotebook`. Among other
--  things, you can choose on which edge the tabs appear (see
--  [methodGtk.Notebook.set_tab_pos]), whether, if there are too many tabs to
--  fit the notebook should be made bigger or scrolling arrows added (see
--  [methodGtk.Notebook.set_scrollable]), and whether there will be a popup
--  menu allowing the users to switch pages. (see
--  [methodGtk.Notebook.popup_enable]).
--
--  # GtkNotebook as GtkBuildable
--
--  The `GtkNotebook` implementation of the `GtkBuildable` interface supports
--  placing children into tabs by specifying "tab" as the "type" attribute of a
--  `<child>` element. Note that the content of the tab must be created before
--  the tab can be filled. A tab child can be specified without specifying a
--  `<child>` type attribute.
--
--  To add a child widget in the notebooks action area, specify "action-start"
--  or "action-end" as the "type" attribute of the `<child>` element.
--
--  An example of a UI definition fragment with `GtkNotebook`:
--
--  ```xml <object class="GtkNotebook"> <child> <object class="GtkLabel"
--  id="notebook-content"> <property name="label">Content</property> </object>
--  </child> <child type="tab"> <object class="GtkLabel" id="notebook-tab">
--  <property name="label">Tab</property> </object> </child> </object> ```
--
--  # Shortcuts and Gestures
--
--  `GtkNotebook` supports the following keyboard shortcuts:
--
--  - <kbd>Shift</kbd>+<kbd>F10</kbd> or <kbd>Menu</kbd> opens the context
--  menu. - <kbd>Home</kbd> moves the focus to the first tab. - <kbd>End</kbd>
--  moves the focus to the last tab.
--
--  Additionally, the following signals have default keybindings:
--
--  - [signalGtk.Notebook::change-current-page] -
--  [signalGtk.Notebook::focus-tab] - [signalGtk.Notebook::move-focus-out] -
--  [signalGtk.Notebook::reorder-tab] - [signalGtk.Notebook::select-page]
--
--  Tabs support drag-and-drop between notebooks sharing the same
--  `group-name`, or to new windows by handling the `::create-window` signal.
--
--  # Actions
--
--  `GtkNotebook` defines a set of built-in actions:
--
--  - `menu.popup` opens the tabs context menu.
--
--  # CSS nodes
--
--  ``` notebook ├── header.top │ ├── [<action widget>] │ ├── tabs │ │ ├──
--  [arrow] │ │ ├── tab │ │ │ ╰── <tab label> ┊ ┊ ┊ │ │ ├──
--  tab[.reorderable-page] │ │ │ ╰── <tab label> │ │ ╰── [arrow] │ ╰── [<action
--  widget>] │ ╰── stack ├── <child> ┊ ╰── <child> ```
--
--  `GtkNotebook` has a main CSS node with name `notebook`, a subnode with
--  name `header` and below that a subnode with name `tabs` which contains one
--  subnode per tab with name `tab`.
--
--  If action widgets are present, their CSS nodes are placed next to the
--  `tabs` node. If the notebook is scrollable, CSS nodes with name `arrow` are
--  placed as first and last child of the `tabs` node.
--
--  The main node gets the `.frame` style class when the notebook has a border
--  (see [methodGtk.Notebook.set_show_border]).
--
--  The header node gets one of the style class `.top`, `.bottom`, `.left` or
--  `.right`, depending on where the tabs are placed. For reorderable pages,
--  the tab node gets the `.reorderable-page` class.
--
--  A `tab` node gets the `.dnd` style class while it is moved with
--  drag-and-drop.
--
--  The nodes are always arranged from left-to-right, regardless of text
--  direction.
--
--  # Accessibility
--
--  `GtkNotebook` uses the following roles:
--
--  - [enumGtk.AccessibleRole.group] for the notebook widget -
--  [enumGtk.AccessibleRole.tab_list] for the list of tabs -
--  [enumGtk.AccessibleRole.tab] role for each tab -
--  [enumGtk.AccessibleRole.tab_panel] for each page
--
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.List_Model;       use Glib.List_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Notebook is

   type Gtk_Notebook_Record is new Gtk_Widget_Record with null record;
   type Gtk_Notebook is access all Gtk_Notebook_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Notebook : out Gtk_Notebook);
   procedure Initialize
      (Notebook : not null access Gtk_Notebook_Record'Class);
   --  Creates a new `GtkNotebook` widget with no pages.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Notebook_New return Gtk_Notebook;
   --  Creates a new `GtkNotebook` widget with no pages.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_notebook_get_type");

   -------------
   -- Methods --
   -------------

   function Append_Page
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint;
   --  Appends a page to Notebook.
   --  @param Child the `GtkWidget` to use as the contents of the page
   --  @param Tab_Label the `GtkWidget` to be used as the label for the page,
   --  or null to use the default label, "page N"
   --  @return the index (starting from 0) of the appended page in the
   --  notebook, or -1 if function fails

   function Append_Page_Menu
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint;
   --  Appends a page to Notebook, specifying the widget to use as the label
   --  in the popup menu.
   --  @param Child the `GtkWidget` to use as the contents of the page
   --  @param Tab_Label the `GtkWidget` to be used as the label for the page,
   --  or null to use the default label, "page N"
   --  @param Menu_Label the widget to use as a label for the page-switch
   --  menu, if that is enabled. If null, and Tab_Label is a `GtkLabel` or
   --  null, then the menu label will be a newly created label with the same
   --  text as Tab_Label; if Tab_Label is not a `GtkLabel`, Menu_Label must be
   --  specified if the page-switch menu is to be used.
   --  @return the index (starting from 0) of the appended page in the
   --  notebook, or -1 if function fails

   procedure Detach_Tab
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes the child from the notebook.
   --  This function is very similar to [methodGtk.Notebook.remove_page], but
   --  additionally informs the notebook that the removal is happening as part
   --  of a tab DND operation, which should not be cancelled.
   --  @param Child a child

   function Get_Action_Widget
      (Notebook  : not null access Gtk_Notebook_Record;
       Pack_Type : Gtk.Enums.Gtk_Pack_Type) return Gtk.Widget.Gtk_Widget;
   --  Gets one of the action widgets.
   --  See [methodGtk.Notebook.set_action_widget].
   --  @param Pack_Type pack type of the action widget to receive
   --  @return The action widget with the given Pack_Type or null when this
   --  action widget has not been set

   procedure Set_Action_Widget
      (Notebook  : not null access Gtk_Notebook_Record;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Pack_Type : Gtk.Enums.Gtk_Pack_Type);
   --  Sets Widget as one of the action widgets.
   --  Depending on the pack type the widget will be placed before or after
   --  the tabs. You can use a `GtkBox` if you need to pack more than one
   --  widget on the same side.
   --  @param Widget a `GtkWidget`
   --  @param Pack_Type pack type of the action widget

   function Get_Current_Page
      (Notebook : not null access Gtk_Notebook_Record) return Glib.Gint;
   --  Returns the page number of the current page.
   --  @return the index (starting from 0) of the current page in the
   --  notebook. If the notebook has no pages, then -1 will be returned.

   procedure Set_Current_Page
      (Notebook : not null access Gtk_Notebook_Record;
       Page_Num : Glib.Gint);
   --  Switches to the page number Page_Num.
   --  Note that due to historical reasons, GtkNotebook refuses to switch to a
   --  page unless the child widget is visible. Therefore, it is recommended to
   --  show child widgets before adding them to a notebook.
   --  @param Page_Num index of the page to switch to, starting from 0. If
   --  negative, the last page will be used. If greater than the number of
   --  pages in the notebook, nothing will be done.

   function Get_Group_Name
      (Notebook : not null access Gtk_Notebook_Record) return UTF8_String;
   --  Gets the current group name for Notebook.
   --  @return the group name, or null if none is set

   procedure Set_Group_Name
      (Notebook   : not null access Gtk_Notebook_Record;
       Group_Name : UTF8_String := "");
   --  Sets a group name for Notebook.
   --  Notebooks with the same name will be able to exchange tabs via drag and
   --  drop. A notebook with a null group name will not be able to exchange
   --  tabs with any other notebook.
   --  @param Group_Name the name of the notebook group, or null to unset it

   function Get_Menu_Label
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the menu label widget of the page containing Child.
   --  @param Child a widget contained in a page of Notebook
   --  @return the menu label, or null if the notebook page does not have a
   --  menu label other than the default (the tab label).

   procedure Set_Menu_Label
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Changes the menu label for the page containing Child.
   --  @param Child the child widget
   --  @param Menu_Label the menu label, or null for default

   function Get_Menu_Label_Text
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String;
   --  Retrieves the text of the menu label for the page containing Child.
   --  @param Child the child widget of a page of the notebook.
   --  @return the text of the tab label, or null if the widget does not have
   --  a menu label other than the default menu label, or the menu label widget
   --  is not a `GtkLabel`. The string is owned by the widget and must not be
   --  freed.

   procedure Set_Menu_Label_Text
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Text : UTF8_String);
   --  Creates a new label and sets it as the menu label of Child.
   --  @param Child the child widget
   --  @param Menu_Text the label text

   function Get_N_Pages
      (Notebook : not null access Gtk_Notebook_Record) return Glib.Gint;
   --  Gets the number of pages in a notebook.
   --  @return the number of pages in the notebook

   function Get_Nth_Page
      (Notebook : not null access Gtk_Notebook_Record;
       Page_Num : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  Returns the child widget contained in page number Page_Num.
   --  @param Page_Num the index of a page in the notebook, or -1 to get the
   --  last page
   --  @return the child widget, or null if Page_Num is out of bounds

   function Get_Pages
      (Notebook : not null access Gtk_Notebook_Record)
       return Glib.List_Model.Glist_Model;
   --  Returns a `GListModel` that contains the pages of the notebook.
   --  This can be used to keep an up-to-date view. The model also implements
   --  [ifaceGtk.SelectionModel] and can be used to track and modify the
   --  visible page.
   --  @return a `GListModel` for the notebook's children

   function Get_Scrollable
      (Notebook : not null access Gtk_Notebook_Record) return Boolean;
   --  Returns whether the tab label area has arrows for scrolling.
   --  @return True if arrows for scrolling are present

   procedure Set_Scrollable
      (Notebook   : not null access Gtk_Notebook_Record;
       Scrollable : Boolean);
   --  Sets whether the tab label area will have arrows for scrolling if there
   --  are too many tabs to fit in the area.
   --  @param Scrollable True if scroll arrows should be added

   function Get_Show_Border
      (Notebook : not null access Gtk_Notebook_Record) return Boolean;
   --  Returns whether a bevel will be drawn around the notebook pages.
   --  @return True if the bevel is drawn

   procedure Set_Show_Border
      (Notebook    : not null access Gtk_Notebook_Record;
       Show_Border : Boolean);
   --  Sets whether a bevel will be drawn around the notebook pages.
   --  This only has a visual effect when the tabs are not shown.
   --  @param Show_Border True if a bevel should be drawn around the notebook

   function Get_Show_Tabs
      (Notebook : not null access Gtk_Notebook_Record) return Boolean;
   --  Returns whether the tabs of the notebook are shown.
   --  @return True if the tabs are shown

   procedure Set_Show_Tabs
      (Notebook  : not null access Gtk_Notebook_Record;
       Show_Tabs : Boolean);
   --  Sets whether to show the tabs for the notebook or not.
   --  @param Show_Tabs True if the tabs should be shown

   function Get_Tab_Detachable
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Returns whether the tab contents can be detached from Notebook.
   --  @param Child a child `GtkWidget`
   --  @return True if the tab is detachable.

   procedure Set_Tab_Detachable
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Detachable : Boolean);
   --  Sets whether the tab can be detached from Notebook to another notebook
   --  or widget.
   --  Note that two notebooks must share a common group identifier (see
   --  [methodGtk.Notebook.set_group_name]) to allow automatic tabs interchange
   --  between them.
   --  If you want a widget to interact with a notebook through DnD (i.e.:
   --  accept dragged tabs from it) it must be set as a drop destination by
   --  adding to it a [classGtk.DropTarget] controller that accepts the GType
   --  `GTK_TYPE_NOTEBOOK_PAGE`. The `:value` of said drop target will be
   --  preloaded with a [classGtk.NotebookPage] object that corresponds to the
   --  dropped tab, so you can process the value via `::accept` or `::drop`
   --  signals.
   --  Note that you should use [methodGtk.Notebook.detach_tab] instead of
   --  [methodGtk.Notebook.remove_page] if you want to remove the tab from the
   --  source notebook as part of accepting a drop. Otherwise, the source
   --  notebook will think that the dragged tab was removed from underneath the
   --  ongoing drag operation, and will initiate a drag cancel animation.
   --  ```c static void on_drag_data_received (GtkWidget *widget, GdkDrop
   --  *drop, GtkSelectionData *data, guint time, gpointer user_data) { GtkDrag
   --  *drag; GtkWidget *notebook; GtkWidget **child;
   --  drag = gtk_drop_get_drag (drop); notebook = g_object_get_data (drag,
   --  "gtk-notebook-drag-origin"); child = (void*) gtk_selection_data_get_data
   --  (data);
   --  // process_widget (*child);
   --  gtk_notebook_detach_tab (GTK_NOTEBOOK (notebook), *child); } ```
   --  If you want a notebook to accept drags from other widgets, you will
   --  have to set your own DnD code to do it.
   --  @param Child a child `GtkWidget`
   --  @param Detachable whether the tab is detachable or not

   function Get_Tab_Label
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the tab label widget for the page Child.
   --  null is returned if Child is not in Notebook or if no tab label has
   --  specifically been set for Child.
   --  @param Child the page
   --  @return the tab label

   procedure Set_Tab_Label
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Changes the tab label for Child.
   --  If null is specified for Tab_Label, then the page will have the label
   --  "page N".
   --  @param Child the page
   --  @param Tab_Label the tab label widget to use, or null for default tab
   --  label

   function Get_Tab_Label_Text
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String;
   --  Retrieves the text of the tab label for the page containing Child.
   --  @param Child a widget contained in a page of Notebook
   --  @return the text of the tab label, or null if the tab label widget is
   --  not a `GtkLabel`. The string is owned by the widget and must not be
   --  freed.

   procedure Set_Tab_Label_Text
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Text : UTF8_String);
   --  Creates a new label and sets it as the tab label for the page
   --  containing Child.
   --  @param Child the page
   --  @param Tab_Text the label text

   function Get_Tab_Pos
      (Notebook : not null access Gtk_Notebook_Record)
       return Gtk.Enums.Gtk_Position_Type;
   --  Gets the edge at which the tabs are drawn.
   --  @return the edge at which the tabs are drawn

   procedure Set_Tab_Pos
      (Notebook : not null access Gtk_Notebook_Record;
       Pos      : Gtk.Enums.Gtk_Position_Type);
   --  Sets the edge at which the tabs are drawn.
   --  @param Pos the edge to draw the tabs at

   function Get_Tab_Reorderable
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Gets whether the tab can be reordered via drag and drop or not.
   --  @param Child a child `GtkWidget`
   --  @return True if the tab is reorderable.

   procedure Set_Tab_Reorderable
      (Notebook    : not null access Gtk_Notebook_Record;
       Child       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Reorderable : Boolean);
   --  Sets whether the notebook tab can be reordered via drag and drop or
   --  not.
   --  @param Child a child `GtkWidget`
   --  @param Reorderable whether the tab is reorderable or not

   function Insert_Page
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
       Position  : Glib.Gint) return Glib.Gint;
   --  Insert a page into Notebook at the given position.
   --  @param Child the `GtkWidget` to use as the contents of the page
   --  @param Tab_Label the `GtkWidget` to be used as the label for the page,
   --  or null to use the default label, "page N"
   --  @param Position the index (starting at 0) at which to insert the page,
   --  or -1 to append the page after all other pages
   --  @return the index (starting from 0) of the inserted page in the
   --  notebook, or -1 if function fails

   function Insert_Page_Menu
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
       Position   : Glib.Gint) return Glib.Gint;
   --  Insert a page into Notebook at the given position, specifying the
   --  widget to use as the label in the popup menu.
   --  @param Child the `GtkWidget` to use as the contents of the page
   --  @param Tab_Label the `GtkWidget` to be used as the label for the page,
   --  or null to use the default label, "page N"
   --  @param Menu_Label the widget to use as a label for the page-switch
   --  menu, if that is enabled. If null, and Tab_Label is a `GtkLabel` or
   --  null, then the menu label will be a newly created label with the same
   --  text as Tab_Label; if Tab_Label is not a `GtkLabel`, Menu_Label must be
   --  specified if the page-switch menu is to be used.
   --  @param Position the index (starting at 0) at which to insert the page,
   --  or -1 to append the page after all other pages.
   --  @return the index (starting from 0) of the inserted page in the
   --  notebook

   procedure Next_Page (Notebook : not null access Gtk_Notebook_Record);
   --  Switches to the next page.
   --  Nothing happens if the current page is the last page.

   function Page_Num
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint;
   --  Finds the index of the page which contains the given child widget.
   --  @param Child a `GtkWidget`
   --  @return the index of the page containing Child, or -1 if Child is not
   --  in the notebook

   procedure Popup_Disable (Notebook : not null access Gtk_Notebook_Record);
   --  Disables the popup menu.

   procedure Popup_Enable (Notebook : not null access Gtk_Notebook_Record);
   --  Enables the popup menu.
   --  If the user clicks with the right mouse button on the tab labels, a
   --  menu with all the pages will be popped up.

   function Prepend_Page
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint;
   --  Prepends a page to Notebook.
   --  @param Child the `GtkWidget` to use as the contents of the page
   --  @param Tab_Label the `GtkWidget` to be used as the label for the page,
   --  or null to use the default label, "page N"
   --  @return the index (starting from 0) of the prepended page in the
   --  notebook, or -1 if function fails

   function Prepend_Page_Menu
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint;
   --  Prepends a page to Notebook, specifying the widget to use as the label
   --  in the popup menu.
   --  @param Child the `GtkWidget` to use as the contents of the page
   --  @param Tab_Label the `GtkWidget` to be used as the label for the page,
   --  or null to use the default label, "page N"
   --  @param Menu_Label the widget to use as a label for the page-switch
   --  menu, if that is enabled. If null, and Tab_Label is a `GtkLabel` or
   --  null, then the menu label will be a newly created label with the same
   --  text as Tab_Label; if Tab_Label is not a `GtkLabel`, Menu_Label must be
   --  specified if the page-switch menu is to be used.
   --  @return the index (starting from 0) of the prepended page in the
   --  notebook, or -1 if function fails

   procedure Prev_Page (Notebook : not null access Gtk_Notebook_Record);
   --  Switches to the previous page.
   --  Nothing happens if the current page is the first page.

   procedure Remove_Page
      (Notebook : not null access Gtk_Notebook_Record;
       Page_Num : Glib.Gint);
   --  Removes a page from the notebook given its index in the notebook.
   --  @param Page_Num the index of a notebook page, starting from 0. If -1,
   --  the last page will be removed.

   procedure Reorder_Child
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Glib.Gint);
   --  Reorders the page containing Child, so that it appears in position
   --  Position.
   --  If Position is greater than or equal to the number of children in the
   --  list or negative, Child will be moved to the end of the list.
   --  @param Child the child to move
   --  @param Position the new position, or -1 to move to the end

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Announce
      (Self     : not null access Gtk_Notebook_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Notebook_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Notebook_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Notebook_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Notebook_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Notebook_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Notebook_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Notebook_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Notebook_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Notebook_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Notebook_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Notebook_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Notebook_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Notebook_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Notebook_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Enable_Popup_Property : constant Glib.Properties.Property_Boolean;
   --  If True, pressing the right mouse button on the notebook shows a page
   --  switching menu.

   Group_Name_Property : constant Glib.Properties.Property_String;
   --  Group name for tab drag and drop.

   Page_Property : constant Glib.Properties.Property_Int;
   --  The index of the current page.

   Pages_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.List_Model
   --  A selection model with the pages.

   Scrollable_Property : constant Glib.Properties.Property_Boolean;
   --  If True, scroll arrows are added if there are too many pages to fit.

   Show_Border_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the border should be shown.

   Show_Tabs_Property : constant Glib.Properties.Property_Boolean;
   --  Whether tabs should be shown.

   Tab_Pos_Property : constant Gtk.Enums.Property_Gtk_Position_Type;
   --  Which side of the notebook holds the tabs.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Notebook_Gint_Boolean is not null access function
     (Self : access Gtk_Notebook_Record'Class;
      Page : Glib.Gint) return Boolean;

   type Cb_GObject_Gint_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      Page : Glib.Gint) return Boolean;

   Signal_Change_Current_Page : constant Glib.Signal_Name := "change-current-page";
   procedure On_Change_Current_Page
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gint_Boolean;
       After : Boolean := False);
   procedure On_Change_Current_Page
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the current page should be changed.
   --
   --  The default bindings for this signal are
   --  <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>PgUp</kbd>,
   --  <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>PgDn</kbd>,
   --  <kbd>Ctrl</kbd>+<kbd>PgUp</kbd> and <kbd>Ctrl</kbd>+<kbd>PgDn</kbd>.
   -- 
   --  Callback parameters:
   --    --  @param Page the page index

   type Cb_Gtk_Notebook_Gtk_Widget_Gtk_Notebook is not null access function
     (Self : access Gtk_Notebook_Record'Class;
      Page : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   return Gtk_Notebook;

   type Cb_GObject_Gtk_Widget_Gtk_Notebook is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      Page : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   return Gtk_Notebook;

   Signal_Create_Window : constant Glib.Signal_Name := "create-window";
   procedure On_Create_Window
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Widget_Gtk_Notebook;
       After : Boolean := False);
   procedure On_Create_Window
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Widget_Gtk_Notebook;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::create-window signal is emitted when a detachable tab is dropped
   --  on the root window.
   --
   --  A handler for this signal can create a window containing a notebook
   --  where the tab will be attached. It is also responsible for
   --  moving/resizing the window and adding the necessary properties to the
   --  notebook (e.g. the `GtkNotebook`:group-name ).
   -- 
   --  Callback parameters:
   --    --  @param Page the tab of Notebook that is being detached

   type Cb_Gtk_Notebook_Gtk_Notebook_Tab_Boolean is not null access function
     (Self : access Gtk_Notebook_Record'Class;
      Tab  : Gtk.Enums.Gtk_Notebook_Tab) return Boolean;

   type Cb_GObject_Gtk_Notebook_Tab_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      Tab  : Gtk.Enums.Gtk_Notebook_Tab) return Boolean;

   Signal_Focus_Tab : constant Glib.Signal_Name := "focus-tab";
   procedure On_Focus_Tab
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Notebook_Tab_Boolean;
       After : Boolean := False);
   procedure On_Focus_Tab
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Notebook_Tab_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a tab should be focused.
   -- 
   --  Callback parameters:
   --    --  @param Tab the notebook tab

   type Cb_Gtk_Notebook_Gtk_Direction_Type_Void is not null access procedure
     (Self      : access Gtk_Notebook_Record'Class;
      Direction : Gtk.Enums.Gtk_Direction_Type);

   type Cb_GObject_Gtk_Direction_Type_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Direction : Gtk.Enums.Gtk_Direction_Type);

   Signal_Move_Focus_Out : constant Glib.Signal_Name := "move-focus-out";
   procedure On_Move_Focus_Out
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Direction_Type_Void;
       After : Boolean := False);
   procedure On_Move_Focus_Out
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when focus was moved out.
   --
   --  The default bindings for this signal are
   --  <kbd>Ctrl</kbd>+<kbd>Tab</kbd>,
   --  <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Tab</kbd>,
   --  <kbd>Ctrl</kbd>+<kbd>←</kbd>, <kbd>Ctrl</kbd>+<kbd>→</kbd>,
   --  <kbd>Ctrl</kbd>+<kbd>↑</kbd> and <kbd>Ctrl</kbd>+<kbd>↓</kbd>.

   type Cb_Gtk_Notebook_Gtk_Widget_Guint_Void is not null access procedure
     (Self     : access Gtk_Notebook_Record'Class;
      Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Page_Num : Guint);

   type Cb_GObject_Gtk_Widget_Guint_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Page_Num : Guint);

   Signal_Page_Added : constant Glib.Signal_Name := "page-added";
   procedure On_Page_Added
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Widget_Guint_Void;
       After : Boolean := False);
   procedure On_Page_Added
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Widget_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  the ::page-added signal is emitted in the notebook right after a page
   --  is added to the notebook.
   -- 
   --  Callback parameters:
   --    --  @param Child the child `GtkWidget` affected
   --    --  @param Page_Num the new page number for Child

   Signal_Page_Removed : constant Glib.Signal_Name := "page-removed";
   procedure On_Page_Removed
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Widget_Guint_Void;
       After : Boolean := False);
   procedure On_Page_Removed
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Widget_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  the ::page-removed signal is emitted in the notebook right after a page
   --  is removed from the notebook.
   -- 
   --  Callback parameters:
   --    --  @param Child the child `GtkWidget` affected
   --    --  @param Page_Num the Child page number

   Signal_Page_Reordered : constant Glib.Signal_Name := "page-reordered";
   procedure On_Page_Reordered
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Widget_Guint_Void;
       After : Boolean := False);
   procedure On_Page_Reordered
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Widget_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  the ::page-reordered signal is emitted in the notebook right after a
   --  page has been reordered.
   -- 
   --  Callback parameters:
   --    --  @param Child the child `GtkWidget` affected
   --    --  @param Page_Num the new page number for Child

   type Cb_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean is not null access function
     (Self         : access Gtk_Notebook_Record'Class;
      Direction    : Gtk.Enums.Gtk_Direction_Type;
      Move_To_Last : Boolean) return Boolean;

   type Cb_GObject_Gtk_Direction_Type_Boolean_Boolean is not null access function
     (Self         : access Glib.Object.GObject_Record'Class;
      Direction    : Gtk.Enums.Gtk_Direction_Type;
      Move_To_Last : Boolean) return Boolean;

   Signal_Reorder_Tab : constant Glib.Signal_Name := "reorder-tab";
   procedure On_Reorder_Tab
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean;
       After : Boolean := False);
   procedure On_Reorder_Tab
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the tab should be reordered.
   --
   --  The default bindings for this signal are
   --  <kbd>Alt</kbd>+<kbd>Home</kbd>, <kbd>Alt</kbd>+<kbd>End</kbd>,
   --  <kbd>Alt</kbd>+<kbd>PgUp</kbd>, <kbd>Alt</kbd>+<kbd>PgDn</kbd>,
   --  <kbd>Alt</kbd>+<kbd>←</kbd>, <kbd>Alt</kbd>+<kbd>→</kbd>,
   --  <kbd>Alt</kbd>+<kbd>↑</kbd> and <kbd>Alt</kbd>+<kbd>↓</kbd>.
   -- 
   --  Callback parameters:
   --    --  @param Direction the direction to move the tab
   --    --  @param Move_To_Last whether to move to the last position

   type Cb_Gtk_Notebook_Boolean_Boolean is not null access function
     (Self       : access Gtk_Notebook_Record'Class;
      Move_Focus : Boolean) return Boolean;

   type Cb_GObject_Boolean_Boolean is not null access function
     (Self       : access Glib.Object.GObject_Record'Class;
      Move_Focus : Boolean) return Boolean;

   Signal_Select_Page : constant Glib.Signal_Name := "select-page";
   procedure On_Select_Page
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Boolean_Boolean;
       After : Boolean := False);
   procedure On_Select_Page
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a page should be selected.
   --
   --  The default binding for this signal is <kbd>␣</kbd>.
   -- 
   --  Callback parameters:
   --    --  @param Move_Focus whether to move focus

   Signal_Switch_Page : constant Glib.Signal_Name := "switch-page";
   procedure On_Switch_Page
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Widget_Guint_Void;
       After : Boolean := False);
   procedure On_Switch_Page
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Widget_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user or a function changes the current page.
   -- 
   --  Callback parameters:
   --    --  @param Page the new current page
   --    --  @param Page_Num the index of the page

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Accessible"
   --
   --  - "ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Notebook_Record, Gtk_Notebook);
   function "+"
     (Widget : access Gtk_Notebook_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Notebook
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Notebook_Record, Gtk_Notebook);
   function "+"
     (Widget : access Gtk_Notebook_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Notebook
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Tab_Pos_Property : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("tab-pos");
   Show_Tabs_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-tabs");
   Show_Border_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-border");
   Scrollable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("scrollable");
   Pages_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("pages");
   Page_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("page");
   Group_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("group-name");
   Enable_Popup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-popup");
end Gtk.Notebook;
