------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

pragma Ada_05;
--  <description>
--  The Gtk.Notebook.Gtk_Notebook widget is a Gtk.Container.Gtk_Container
--  whose children are pages that can be switched between using tab labels
--  along one edge.
--
--  There are many configuration options for GtkNotebook. Among other things,
--  you can choose on which edge the tabs appear (see
--  Gtk.Notebook.Set_Tab_Pos), whether, if there are too many tabs to fit the
--  notebook should be made bigger or scrolling arrows added (see
--  Gtk.Notebook.Set_Scrollable), and whether there will be a popup menu
--  allowing the users to switch pages. (see Gtk.Notebook.Popup_Enable,
--  Gtk.Notebook.Popup_Disable)
--
--  == GtkNotebook as GtkBuildable ==
--
--  The GtkNotebook implementation of the Gtk.Buildable.Gtk_Buildable
--  interface supports placing children into tabs by specifying "tab" as the
--  "type" attribute of a <child> element. Note that the content of the tab
--  must be created before the tab can be filled. A tab child can be specified
--  without specifying a <child> type attribute.
--
--  To add a child widget in the notebooks action area, specify "action-start"
--  or "action-end" as the "type" attribute of the <child> element.
--
--  == A UI definition fragment with GtkNotebook ==
--
--    <object class="GtkNotebook">
--    <child>
--    <object class="GtkLabel" id="notebook-content">
--    <property name="label">Content</property>
--    </object>
--    </child>
--    <child type="tab">
--    <object class="GtkLabel" id="notebook-tab">
--    <property name="label">Tab</property>
--    </object>
--    </child>
--    </object>
--
--
--  </description>
--  <screenshot>gtk-notebook</screenshot>
--  <group>Layout containers</group>
--  <testgtk>create_notebook.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Notebook is

   type Gtk_Notebook_Record is new Gtk_Container_Record with null record;
   type Gtk_Notebook is access all Gtk_Notebook_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Notebook : out Gtk_Notebook);
   procedure Initialize (Notebook : access Gtk_Notebook_Record'Class);
   --  Creates a new Gtk.Notebook.Gtk_Notebook widget with no pages.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_notebook_get_type");

   -------------
   -- Methods --
   -------------

   function Append_Page
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint;
   --  Appends a page to Notebook.
   --  page in the notebook, or -1 if function fails
   --  "child": the Gtk.Widget.Gtk_Widget to use as the contents of the page
   --  "tab_label": the Gtk.Widget.Gtk_Widget to be used as the label for the
   --  page, or null to use the default label, 'page N'

   procedure Append_Page_Menu
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Appends a page to Notebook, specifying the widget to use as the label
   --  in the popup menu.
   --  page in the notebook, or -1 if function fails
   --  "child": the Gtk.Widget.Gtk_Widget to use as the contents of the page
   --  "tab_label": the Gtk.Widget.Gtk_Widget to be used as the label for the
   --  page, or null to use the default label, 'page N'
   --  "menu_label": the widget to use as a label for the page-switch menu, if
   --  that is enabled. If null, and Tab_Label is a Gtk.Label.Gtk_Label or
   --  null, then the menu label will be a newly created label with the same
   --  text as Tab_Label; if Tab_Label is not a Gtk.Label.Gtk_Label, Menu_Label
   --  must be specified if the page-switch menu is to be used.

   function Get_Action_Widget
      (Notebook  : not null access Gtk_Notebook_Record;
       Pack_Type : Gtk.Enums.Gtk_Pack_Type) return Gtk.Widget.Gtk_Widget;
   procedure Set_Action_Widget
      (Notebook  : not null access Gtk_Notebook_Record;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Pack_Type : Gtk.Enums.Gtk_Pack_Type);
   --  Sets Widget as one of the action widgets. Depending on the pack type
   --  the widget will be placed before or after the tabs. You can use a
   --  Gtk.Box.Gtk_Box if you need to pack more than one widget on the same
   --  side.
   --  Note that action widgets are "internal" children of the notebook and
   --  thus not included in the list returned from Gtk.Container.Foreach.
   --  Since: gtk+ 2.20
   --  "widget": a Gtk.Widget.Gtk_Widget
   --  "pack_type": pack type of the action widget

   function Get_Current_Page
      (Notebook : not null access Gtk_Notebook_Record) return Gint;
   procedure Set_Current_Page
      (Notebook : not null access Gtk_Notebook_Record;
       Page_Num : Gint := -1);
   --  Switches to the page number Page_Num.
   --  Note that due to historical reasons, GtkNotebook refuses to switch to a
   --  page unless the child widget is visible. Therefore, it is recommended to
   --  show child widgets before adding them to a notebook.
   --  "page_num": index of the page to switch to, starting from 0. If
   --  negative, the last page will be used. If greater than the number of
   --  pages in the notebook, nothing will be done.

   function Get_Group_Name
      (Notebook : not null access Gtk_Notebook_Record) return UTF8_String;
   procedure Set_Group_Name
      (Notebook   : not null access Gtk_Notebook_Record;
       Group_Name : UTF8_String);
   --  Sets a group name for Notebook.
   --  Notebooks with the same name will be able to exchange tabs via drag and
   --  drop. A notebook with a null group name will not be able to exchange
   --  tabs with any other notebook.
   --  Since: gtk+ 2.24
   --  "group_name": the name of the notebook group, or null to unset it

   function Get_Menu_Label
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Widget.Gtk_Widget;
   procedure Set_Menu_Label
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Changes the menu label for the page containing Child.
   --  "child": the child widget
   --  "menu_label": the menu label, or null for default

   function Get_Menu_Label_Text
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String;
   procedure Set_Menu_Label_Text
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Text : UTF8_String);
   --  Creates a new label and sets it as the menu label of Child.
   --  "child": the child widget
   --  "menu_text": the label text

   function Get_N_Pages
      (Notebook : not null access Gtk_Notebook_Record) return Gint;
   --  Gets the number of pages in a notebook.
   --  Since: gtk+ 2.2

   function Get_Nth_Page
      (Notebook : not null access Gtk_Notebook_Record;
       Page_Num : Gint) return Gtk.Widget.Gtk_Widget;
   --  Returns the child widget contained in page number Page_Num.
   --  if Page_Num is out of bounds
   --  "page_num": the index of a page in the notebook, or -1 to get the last
   --  page

   function Get_Scrollable
      (Notebook : not null access Gtk_Notebook_Record) return Boolean;
   procedure Set_Scrollable
      (Notebook   : not null access Gtk_Notebook_Record;
       Scrollable : Boolean := True);
   --  Sets whether the tab label area will have arrows for scrolling if there
   --  are too many tabs to fit in the area.
   --  "scrollable": True if scroll arrows should be added

   function Get_Show_Border
      (Notebook : not null access Gtk_Notebook_Record) return Boolean;
   procedure Set_Show_Border
      (Notebook    : not null access Gtk_Notebook_Record;
       Show_Border : Boolean := True);
   --  Sets whether a bevel will be drawn around the notebook pages. This only
   --  has a visual effect when the tabs are not shown. See
   --  Gtk.Notebook.Set_Show_Tabs.
   --  "show_border": True if a bevel should be drawn around the notebook

   function Get_Show_Tabs
      (Notebook : not null access Gtk_Notebook_Record) return Boolean;
   procedure Set_Show_Tabs
      (Notebook  : not null access Gtk_Notebook_Record;
       Show_Tabs : Boolean := True);
   --  Sets whether to show the tabs for the notebook or not.
   --  "show_tabs": True if the tabs should be shown

   function Get_Tab_Detachable
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   procedure Set_Tab_Detachable
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Detachable : Boolean := True);
   --  Sets whether the tab can be detached from Notebook to another notebook
   --  or widget.
   --  Note that 2 notebooks must share a common group identificator (see
   --  Gtk.Notebook.Set_Group_Name) to allow automatic tabs interchange between
   --  them.
   --  If you want a widget to interact with a notebook through DnD (i.e.:
   --  accept dragged tabs from it) it must be set as a drop destination and
   --  accept the target "GTK_NOTEBOOK_TAB". The notebook will fill the
   --  selection with a GtkWidget** pointing to the child widget that
   --  corresponds to the dropped tab. |[ static void
   --  on_drop_zone_drag_data_received (GtkWidget *widget, GdkDragContext
   --  *context, gint x, gint y, GtkSelectionData *selection_data, guint info,
   --  guint time, gpointer user_data) { GtkWidget *notebook; GtkWidget
   --  **child;
   --  notebook = gtk_drag_get_source_widget (context); child = (void*)
   --  gtk_selection_data_get_data (selection_data);
   --  process_widget (*child); gtk_container_remove (GTK_CONTAINER (notebook),
   --  *child); } ]|
   --  If you want a notebook to accept drags from other widgets, you will have
   --  to set your own DnD code to do it.
   --  Since: gtk+ 2.10
   --  "child": a child Gtk.Widget.Gtk_Widget
   --  "detachable": whether the tab is detachable or not

   function Get_Tab_Hborder
      (Notebook : not null access Gtk_Notebook_Record) return Guint16;
   --  Returns the horizontal width of a tab border.
   --  Since: gtk+ 2.22

   function Get_Tab_Label
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Widget.Gtk_Widget;
   procedure Set_Tab_Label
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Changes the tab label for Child. If null is specified for Tab_Label,
   --  then the page will have the label 'page N'.
   --  "child": the page
   --  "tab_label": the tab label widget to use, or null for default tab label

   function Get_Tab_Label_Text
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String;
   procedure Set_Tab_Label_Text
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Text : UTF8_String);
   --  Creates a new label and sets it as the tab label for the page
   --  containing Child.
   --  "child": the page
   --  "tab_text": the label text

   function Get_Tab_Pos
      (Notebook : not null access Gtk_Notebook_Record)
       return Gtk.Enums.Gtk_Position_Type;
   procedure Set_Tab_Pos
      (Notebook : not null access Gtk_Notebook_Record;
       Pos      : Gtk.Enums.Gtk_Position_Type);
   --  Sets the edge at which the tabs for switching pages in the notebook are
   --  drawn.
   --  "pos": the edge to draw the tabs at

   function Get_Tab_Reorderable
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   procedure Set_Tab_Reorderable
      (Notebook    : not null access Gtk_Notebook_Record;
       Child       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Reorderable : Boolean := True);
   --  Sets whether the notebook tab can be reordered via drag and drop or
   --  not.
   --  Since: gtk+ 2.10
   --  "child": a child Gtk.Widget.Gtk_Widget
   --  "reorderable": whether the tab is reorderable or not

   function Get_Tab_Vborder
      (Notebook : not null access Gtk_Notebook_Record) return Guint16;
   --  Returns the vertical width of a tab border.
   --  Since: gtk+ 2.22

   function Insert_Page
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
       Position  : Gint) return Gint;
   --  Insert a page into Notebook at the given position.
   --  page in the notebook, or -1 if function fails
   --  "child": the Gtk.Widget.Gtk_Widget to use as the contents of the page
   --  "tab_label": the Gtk.Widget.Gtk_Widget to be used as the label for the
   --  page, or null to use the default label, 'page N'
   --  "position": the index (starting at 0) at which to insert the page, or
   --  -1 to append the page after all other pages

   function Insert_Page_Menu
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
       Position   : Gint) return Gint;
   --  Insert a page into Notebook at the given position, specifying the
   --  widget to use as the label in the popup menu.
   --  page in the notebook
   --  "child": the Gtk.Widget.Gtk_Widget to use as the contents of the page
   --  "tab_label": the Gtk.Widget.Gtk_Widget to be used as the label for the
   --  page, or null to use the default label, 'page N'
   --  "menu_label": the widget to use as a label for the page-switch menu, if
   --  that is enabled. If null, and Tab_Label is a Gtk.Label.Gtk_Label or
   --  null, then the menu label will be a newly created label with the same
   --  text as Tab_Label; if Tab_Label is not a Gtk.Label.Gtk_Label, Menu_Label
   --  must be specified if the page-switch menu is to be used.
   --  "position": the index (starting at 0) at which to insert the page, or
   --  -1 to append the page after all other pages.

   procedure Next_Page (Notebook : not null access Gtk_Notebook_Record);
   --  Switches to the next page. Nothing happens if the current page is the
   --  last page.

   function Page_Num
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gint;
   --  Finds the index of the page which contains the given child widget.
   --  -1 if Child is not in the notebook
   --  "child": a Gtk.Widget.Gtk_Widget

   procedure Popup_Disable (Notebook : not null access Gtk_Notebook_Record);
   --  Disables the popup menu.

   procedure Popup_Enable (Notebook : not null access Gtk_Notebook_Record);
   --  Enables the popup menu: if the user clicks with the right mouse button
   --  on the tab labels, a menu with all the pages will be popped up.

   function Prepend_Page
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint;
   --  Prepends a page to Notebook.
   --  page in the notebook, or -1 if function fails
   --  "child": the Gtk.Widget.Gtk_Widget to use as the contents of the page
   --  "tab_label": the Gtk.Widget.Gtk_Widget to be used as the label for the
   --  page, or null to use the default label, 'page N'

   function Prepend_Page_Menu
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint;
   --  Prepends a page to Notebook, specifying the widget to use as the label
   --  in the popup menu.
   --  page in the notebook, or -1 if function fails
   --  "child": the Gtk.Widget.Gtk_Widget to use as the contents of the page
   --  "tab_label": the Gtk.Widget.Gtk_Widget to be used as the label for the
   --  page, or null to use the default label, 'page N'
   --  "menu_label": the widget to use as a label for the page-switch menu, if
   --  that is enabled. If null, and Tab_Label is a Gtk.Label.Gtk_Label or
   --  null, then the menu label will be a newly created label with the same
   --  text as Tab_Label; if Tab_Label is not a Gtk.Label.Gtk_Label, Menu_Label
   --  must be specified if the page-switch menu is to be used.

   procedure Prev_Page (Notebook : not null access Gtk_Notebook_Record);
   --  Switches to the previous page. Nothing happens if the current page is
   --  the first page.

   procedure Remove_Page
      (Notebook : not null access Gtk_Notebook_Record;
       Page_Num : Gint);
   --  Removes a page from the notebook given its index in the notebook.
   --  "page_num": the index of a notebook page, starting from 0. If -1, the
   --  last page will be removed.

   procedure Reorder_Child
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Gint);
   --  Reorders the page containing Child, so that it appears in position
   --  Position. If Position is greater than or equal to the number of children
   --  in the list or negative, Child will be moved to the end of the list.
   --  "child": the child to move
   --  "position": the new position, or -1 to move to the end

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Append_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Append_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Prepend_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Convenience functions, same as above but discarding the return value.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Notebook_Record, Gtk_Notebook);
   function "+"
     (Widget : access Gtk_Notebook_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Notebook
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Enable_Popup_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Group_Name_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  Group name for tab drag and drop.
   --
   --  Name: Page_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Scrollable_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Show_Border_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Show_Tabs_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Tab_Pos_Property
   --  Type: Gtk.Enums.Gtk_Position_Type
   --  Flags: read-write

   Enable_Popup_Property : constant Glib.Properties.Property_Boolean;
   Group_Name_Property : constant Glib.Properties.Property_String;
   Page_Property : constant Glib.Properties.Property_Int;
   Scrollable_Property : constant Glib.Properties.Property_Boolean;
   Show_Border_Property : constant Glib.Properties.Property_Boolean;
   Show_Tabs_Property : constant Glib.Properties.Property_Boolean;
   Tab_Pos_Property : constant Gtk.Enums.Property_Gtk_Position_Type;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "change-current-page"
   --     function Handler
   --       (Self   : access Gtk_Notebook_Record'Class;
   --        Object : Gint) return Boolean;
   --
   --  "create-window"
   --     function Handler
   --       (Self : access Gtk_Notebook_Record'Class;
   --        Page : Gtk.Widget.Gtk_Widget;
   --        X    : Gint;
   --        Y    : Gint) return Gtk_Notebook;
   --    --  "page": the tab of Notebook that is being detached
   --    --  "x": the X coordinate where the drop happens
   --    --  "y": the Y coordinate where the drop happens
   --  The ::create-window signal is emitted when a detachable tab is dropped
   --  on the root window.
   --  A handler for this signal can create a window containing a notebook
   --  where the tab will be attached. It is also responsible for
   --  moving/resizing the window and adding the necessary properties to the
   --  notebook (e.g. the Gtk.Notebook.Gtk_Notebook:group ).
   --  added to, or null.
   --  Returns a Gtk.Notebook.Gtk_Notebook that Page should be
   --
   --  "focus-tab"
   --     function Handler
   --       (Self   : access Gtk_Notebook_Record'Class;
   --        Object : Notebook_Tab) return Boolean;
   --
   --  "move-focus-out"
   --     procedure Handler
   --       (Self   : access Gtk_Notebook_Record'Class;
   --        Object : Gtk.Enums.Gtk_Direction_Type);
   --
   --  "page-added"
   --     procedure Handler
   --       (Self     : access Gtk_Notebook_Record'Class;
   --        Child    : Gtk.Widget.Gtk_Widget;
   --        Page_Num : Guint);
   --    --  "child": the child Gtk.Widget.Gtk_Widget affected
   --    --  "page_num": the new page number for Child
   --  the ::page-added signal is emitted in the notebook right after a page
   --  is added to the notebook.
   --
   --  "page-removed"
   --     procedure Handler
   --       (Self     : access Gtk_Notebook_Record'Class;
   --        Child    : Gtk.Widget.Gtk_Widget;
   --        Page_Num : Guint);
   --    --  "child": the child Gtk.Widget.Gtk_Widget affected
   --    --  "page_num": the Child page number
   --  the ::page-removed signal is emitted in the notebook right after a page
   --  is removed from the notebook.
   --
   --  "page-reordered"
   --     procedure Handler
   --       (Self     : access Gtk_Notebook_Record'Class;
   --        Child    : Gtk.Widget.Gtk_Widget;
   --        Page_Num : Guint);
   --    --  "child": the child Gtk.Widget.Gtk_Widget affected
   --    --  "page_num": the new page number for Child
   --  the ::page-reordered signal is emitted in the notebook right after a
   --  page has been reordered.
   --
   --  "reorder-tab"
   --     function Handler
   --       (Self   : access Gtk_Notebook_Record'Class;
   --        Object : Gtk.Enums.Gtk_Direction_Type;
   --        P0     : Boolean) return Boolean;
   --
   --  "select-page"
   --     function Handler
   --       (Self   : access Gtk_Notebook_Record'Class;
   --        Object : Boolean) return Boolean;
   --
   --  "switch-page"
   --     procedure Handler
   --       (Self     : access Gtk_Notebook_Record'Class;
   --        Page     : Gtk.Widget.Gtk_Widget;
   --        Page_Num : Guint);
   --    --  "page": the new current page
   --    --  "page_num": the index of the page
   --  Emitted when the user or a function changes the current page.

   Signal_Change_Current_Page : constant Glib.Signal_Name := "change-current-page";
   Signal_Create_Window : constant Glib.Signal_Name := "create-window";
   Signal_Focus_Tab : constant Glib.Signal_Name := "focus-tab";
   Signal_Move_Focus_Out : constant Glib.Signal_Name := "move-focus-out";
   Signal_Page_Added : constant Glib.Signal_Name := "page-added";
   Signal_Page_Removed : constant Glib.Signal_Name := "page-removed";
   Signal_Page_Reordered : constant Glib.Signal_Name := "page-reordered";
   Signal_Reorder_Tab : constant Glib.Signal_Name := "reorder-tab";
   Signal_Select_Page : constant Glib.Signal_Name := "select-page";
   Signal_Switch_Page : constant Glib.Signal_Name := "switch-page";

private
   Enable_Popup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-popup");
   Group_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("group-name");
   Page_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("page");
   Scrollable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("scrollable");
   Show_Border_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-border");
   Show_Tabs_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-tabs");
   Tab_Pos_Property : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("tab-pos");
end Gtk.Notebook;
