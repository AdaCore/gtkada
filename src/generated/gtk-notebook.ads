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
--  # GtkNotebook as GtkBuildable
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
--  An example of a UI definition fragment with GtkNotebook:
--
--     <object class="GtkNotebook">
--       <child>
--         <object class="GtkLabel" id="notebook-content">
--           <property name="label">Content</property>
--         </object>
--       </child>
--       <child type="tab">
--         <object class="GtkLabel" id="notebook-tab">
--           <property name="label">Tab</property>
--         </object>
--       </child>
--     </object>
--  # CSS nodes
--
--     notebook
--     ├── header.top
--     │   ├── [<action widget>]
--     │   ├── tabs
--     │   │   ├── [arrow]
--     │   │   ├── tab
--     │   │   │   ╰── <tab label>
--     ┊   ┊   ┊
--     │   │   ├── tab[.reorderable-page]
--     │   │   │   ╰── <tab label>
--     │   │   ╰── [arrow]
--     │   ╰── [<action widget>]
--     │
--     ╰── stack
--         ├── <child>
--         ┊
--         ╰── <child>
--
--
--  GtkNotebook has a main CSS node with name notebook, a subnode with name
--  header and below that a subnode with name tabs which contains one subnode
--  per tab with name tab.
--
--  If action widgets are present, their CSS nodes are placed next to the tabs
--  node. If the notebook is scrollable, CSS nodes with name arrow are placed
--  as first and last child of the tabs node.
--
--  The main node gets the .frame style class when the notebook has a border
--  (see Gtk.Notebook.Set_Show_Border).
--
--  The header node gets one of the style class .top, .bottom, .left or
--  .right, depending on where the tabs are placed. For reorderable pages, the
--  tab node gets the .reorderable-page class.
--
--  A tab node gets the .dnd style class while it is moved with drag-and-drop.
--
--  The nodes are always arranged from left-to-right, regarldess of text
--  direction.
--
--  <screenshot>gtk-notebook</screenshot>
--  <group>Layout containers</group>
--  <testgtk>create_notebook.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Container;           use Gtk.Container;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Notebook is

   type Gtk_Notebook_Record is new Gtk_Container_Record with null record;
   type Gtk_Notebook is access all Gtk_Notebook_Record'Class;

   type Gtk_Notebook_Tab is (
      Notebook_Tab_First,
      Notebook_Tab_Last);
   pragma Convention (C, Gtk_Notebook_Tab);

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Notebook_Tab_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Notebook_Tab);
   type Property_Gtk_Notebook_Tab is new Gtk_Notebook_Tab_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Notebook : out Gtk_Notebook);
   procedure Initialize
      (Notebook : not null access Gtk_Notebook_Record'Class);
   --  Creates a new Gtk.Notebook.Gtk_Notebook widget with no pages.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Notebook_New return Gtk_Notebook;
   --  Creates a new Gtk.Notebook.Gtk_Notebook widget with no pages.

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
   --  @param Child the Gtk.Widget.Gtk_Widget to use as the contents of the
   --  page
   --  @param Tab_Label the Gtk.Widget.Gtk_Widget to be used as the label for
   --  the page, or null to use the default label, "page N"
   --  @return the index (starting from 0) of the appended page in the
   --  notebook, or -1 if function fails

   procedure Append_Page_Menu
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Appends a page to Notebook, specifying the widget to use as the label
   --  in the popup menu.
   --  @param Child the Gtk.Widget.Gtk_Widget to use as the contents of the
   --  page
   --  @param Tab_Label the Gtk.Widget.Gtk_Widget to be used as the label for
   --  the page, or null to use the default label, "page N"
   --  @param Menu_Label the widget to use as a label for the page-switch
   --  menu, if that is enabled. If null, and Tab_Label is a
   --  Gtk.Label.Gtk_Label or null, then the menu label will be a newly created
   --  label with the same text as Tab_Label; if Tab_Label is not a
   --  Gtk.Label.Gtk_Label, Menu_Label must be specified if the page-switch
   --  menu is to be used.

   procedure Detach_Tab
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes the child from the notebook.
   --  This function is very similar to Gtk.Container.Remove, but additionally
   --  informs the notebook that the removal is happening as part of a tab DND
   --  operation, which should not be cancelled.
   --  Since: gtk+ 3.16
   --  @param Child a child

   function Get_Action_Widget
      (Notebook  : not null access Gtk_Notebook_Record;
       Pack_Type : Gtk.Enums.Gtk_Pack_Type) return Gtk.Widget.Gtk_Widget;
   --  Gets one of the action widgets. See Gtk.Notebook.Set_Action_Widget.
   --  Since: gtk+ 2.20
   --  @param Pack_Type pack type of the action widget to receive
   --  @return The action widget with the given Pack_Type or null when this
   --  action widget has not been set

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
   --  @param Widget a Gtk.Widget.Gtk_Widget
   --  @param Pack_Type pack type of the action widget

   function Get_Current_Page
      (Notebook : not null access Gtk_Notebook_Record) return Glib.Gint;
   --  Returns the page number of the current page.
   --  @return the index (starting from 0) of the current page in the
   --  notebook. If the notebook has no pages, then -1 will be returned.

   procedure Set_Current_Page
      (Notebook : not null access Gtk_Notebook_Record;
       Page_Num : Glib.Gint := -1);
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
   --  Since: gtk+ 2.24
   --  @return the group name, or null if none is set

   procedure Set_Group_Name
      (Notebook   : not null access Gtk_Notebook_Record;
       Group_Name : UTF8_String := "");
   --  Sets a group name for Notebook.
   --  Notebooks with the same name will be able to exchange tabs via drag and
   --  drop. A notebook with a null group name will not be able to exchange
   --  tabs with any other notebook.
   --  Since: gtk+ 2.24
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
   --  is not a Gtk.Label.Gtk_Label. The string is owned by the widget and must
   --  not be freed.

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
   --  Since: gtk+ 2.2
   --  @return the number of pages in the notebook

   function Get_Nth_Page
      (Notebook : not null access Gtk_Notebook_Record;
       Page_Num : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  Returns the child widget contained in page number Page_Num.
   --  @param Page_Num the index of a page in the notebook, or -1 to get the
   --  last page
   --  @return the child widget, or null if Page_Num is out of bounds

   function Get_Scrollable
      (Notebook : not null access Gtk_Notebook_Record) return Boolean;
   --  Returns whether the tab label area has arrows for scrolling. See
   --  Gtk.Notebook.Set_Scrollable.
   --  @return True if arrows for scrolling are present

   procedure Set_Scrollable
      (Notebook   : not null access Gtk_Notebook_Record;
       Scrollable : Boolean := True);
   --  Sets whether the tab label area will have arrows for scrolling if there
   --  are too many tabs to fit in the area.
   --  @param Scrollable True if scroll arrows should be added

   function Get_Show_Border
      (Notebook : not null access Gtk_Notebook_Record) return Boolean;
   --  Returns whether a bevel will be drawn around the notebook pages. See
   --  Gtk.Notebook.Set_Show_Border.
   --  @return True if the bevel is drawn

   procedure Set_Show_Border
      (Notebook    : not null access Gtk_Notebook_Record;
       Show_Border : Boolean := True);
   --  Sets whether a bevel will be drawn around the notebook pages. This only
   --  has a visual effect when the tabs are not shown. See
   --  Gtk.Notebook.Set_Show_Tabs.
   --  @param Show_Border True if a bevel should be drawn around the notebook

   function Get_Show_Tabs
      (Notebook : not null access Gtk_Notebook_Record) return Boolean;
   --  Returns whether the tabs of the notebook are shown. See
   --  Gtk.Notebook.Set_Show_Tabs.
   --  @return True if the tabs are shown

   procedure Set_Show_Tabs
      (Notebook  : not null access Gtk_Notebook_Record;
       Show_Tabs : Boolean := True);
   --  Sets whether to show the tabs for the notebook or not.
   --  @param Show_Tabs True if the tabs should be shown

   function Get_Tab_Detachable
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Returns whether the tab contents can be detached from Notebook.
   --  Since: gtk+ 2.10
   --  @param Child a child Gtk.Widget.Gtk_Widget
   --  @return True if the tab is detachable.

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
   --  corresponds to the dropped tab.
   --  Note that you should use Gtk.Notebook.Detach_Tab instead of
   --  Gtk.Container.Remove if you want to remove the tab from the source
   --  notebook as part of accepting a drop. Otherwise, the source notebook
   --  will think that the dragged tab was removed from underneath the ongoing
   --  drag operation, and will initiate a drag cancel animation.
   --
   --     static void
   --     on_drag_data_received (GtkWidget        *widget,
   --                            GdkDragContext   *context,
   --                            gint              x,
   --                            gint              y,
   --                            GtkSelectionData *data,
   --                            guint             info,
   --                            guint             time,
   --                            gpointer          user_data)
   --     {
   --       GtkWidget *notebook;
   --       GtkWidget **child;
   --
   --       notebook = gtk_drag_get_source_widget (context);
   --       child = (void*) gtk_selection_data_get_data (data);
   --
   --       // process_widget (*child);
   --
   --       gtk_notebook_detach_tab (GTK_NOTEBOOK (notebook), *child);
   --     }
   --
   --  If you want a notebook to accept drags from other widgets, you will
   --  have to set your own DnD code to do it.
   --  Since: gtk+ 2.10
   --  @param Child a child Gtk.Widget.Gtk_Widget
   --  @param Detachable whether the tab is detachable or not

   function Get_Tab_Hborder
      (Notebook : not null access Gtk_Notebook_Record) return Guint16;
   pragma Obsolescent (Get_Tab_Hborder);
   --  Returns the horizontal width of a tab border.
   --  Since: gtk+ 2.22
   --  Deprecated since 3.4, 1
   --  @return horizontal width of a tab border

   function Get_Tab_Label
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the tab label widget for the page Child. null is returned if
   --  Child is not in Notebook or if no tab label has specifically been set
   --  for Child.
   --  @param Child the page
   --  @return the tab label

   procedure Set_Tab_Label
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Changes the tab label for Child. If null is specified for Tab_Label,
   --  then the page will have the label "page N".
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
   --  not a Gtk.Label.Gtk_Label. The string is owned by the widget and must
   --  not be freed.

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
   --  Gets the edge at which the tabs for switching pages in the notebook are
   --  drawn.
   --  @return the edge at which the tabs are drawn

   procedure Set_Tab_Pos
      (Notebook : not null access Gtk_Notebook_Record;
       Pos      : Gtk.Enums.Gtk_Position_Type);
   --  Sets the edge at which the tabs for switching pages in the notebook are
   --  drawn.
   --  @param Pos the edge to draw the tabs at

   function Get_Tab_Reorderable
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Gets whether the tab can be reordered via drag and drop or not.
   --  Since: gtk+ 2.10
   --  @param Child a child Gtk.Widget.Gtk_Widget
   --  @return True if the tab is reorderable.

   procedure Set_Tab_Reorderable
      (Notebook    : not null access Gtk_Notebook_Record;
       Child       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Reorderable : Boolean := True);
   --  Sets whether the notebook tab can be reordered via drag and drop or
   --  not.
   --  Since: gtk+ 2.10
   --  @param Child a child Gtk.Widget.Gtk_Widget
   --  @param Reorderable whether the tab is reorderable or not

   function Get_Tab_Vborder
      (Notebook : not null access Gtk_Notebook_Record) return Guint16;
   pragma Obsolescent (Get_Tab_Vborder);
   --  Returns the vertical width of a tab border.
   --  Since: gtk+ 2.22
   --  Deprecated since 3.4, 1
   --  @return vertical width of a tab border

   function Insert_Page
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
       Position  : Glib.Gint) return Glib.Gint;
   --  Insert a page into Notebook at the given position.
   --  @param Child the Gtk.Widget.Gtk_Widget to use as the contents of the
   --  page
   --  @param Tab_Label the Gtk.Widget.Gtk_Widget to be used as the label for
   --  the page, or null to use the default label, "page N"
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
   --  @param Child the Gtk.Widget.Gtk_Widget to use as the contents of the
   --  page
   --  @param Tab_Label the Gtk.Widget.Gtk_Widget to be used as the label for
   --  the page, or null to use the default label, "page N"
   --  @param Menu_Label the widget to use as a label for the page-switch
   --  menu, if that is enabled. If null, and Tab_Label is a
   --  Gtk.Label.Gtk_Label or null, then the menu label will be a newly created
   --  label with the same text as Tab_Label; if Tab_Label is not a
   --  Gtk.Label.Gtk_Label, Menu_Label must be specified if the page-switch
   --  menu is to be used.
   --  @param Position the index (starting at 0) at which to insert the page,
   --  or -1 to append the page after all other pages.
   --  @return the index (starting from 0) of the inserted page in the
   --  notebook

   procedure Next_Page (Notebook : not null access Gtk_Notebook_Record);
   --  Switches to the next page. Nothing happens if the current page is the
   --  last page.

   function Page_Num
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint;
   --  Finds the index of the page which contains the given child widget.
   --  @param Child a Gtk.Widget.Gtk_Widget
   --  @return the index of the page containing Child, or -1 if Child is not
   --  in the notebook

   procedure Popup_Disable (Notebook : not null access Gtk_Notebook_Record);
   --  Disables the popup menu.

   procedure Popup_Enable (Notebook : not null access Gtk_Notebook_Record);
   --  Enables the popup menu: if the user clicks with the right mouse button
   --  on the tab labels, a menu with all the pages will be popped up.

   function Prepend_Page
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint;
   --  Prepends a page to Notebook.
   --  @param Child the Gtk.Widget.Gtk_Widget to use as the contents of the
   --  page
   --  @param Tab_Label the Gtk.Widget.Gtk_Widget to be used as the label for
   --  the page, or null to use the default label, "page N"
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
   --  @param Child the Gtk.Widget.Gtk_Widget to use as the contents of the
   --  page
   --  @param Tab_Label the Gtk.Widget.Gtk_Widget to be used as the label for
   --  the page, or null to use the default label, "page N"
   --  @param Menu_Label the widget to use as a label for the page-switch
   --  menu, if that is enabled. If null, and Tab_Label is a
   --  Gtk.Label.Gtk_Label or null, then the menu label will be a newly created
   --  label with the same text as Tab_Label; if Tab_Label is not a
   --  Gtk.Label.Gtk_Label, Menu_Label must be specified if the page-switch
   --  menu is to be used.
   --  @return the index (starting from 0) of the prepended page in the
   --  notebook, or -1 if function fails

   procedure Prev_Page (Notebook : not null access Gtk_Notebook_Record);
   --  Switches to the previous page. Nothing happens if the current page is
   --  the first page.

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
   --  Position. If Position is greater than or equal to the number of children
   --  in the list or negative, Child will be moved to the end of the list.
   --  @param Child the child to move
   --  @param Position the new position, or -1 to move to the end

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

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Enable_Popup_Property : constant Glib.Properties.Property_Boolean;

   Group_Name_Property : constant Glib.Properties.Property_String;
   --  Group name for tab drag and drop.

   Page_Property : constant Glib.Properties.Property_Int;

   Scrollable_Property : constant Glib.Properties.Property_Boolean;

   Show_Border_Property : constant Glib.Properties.Property_Boolean;

   Show_Tabs_Property : constant Glib.Properties.Property_Boolean;

   Tab_Pos_Property : constant Gtk.Enums.Property_Gtk_Position_Type;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Notebook_Gint_Boolean is not null access function
     (Self   : access Gtk_Notebook_Record'Class;
      Object : Glib.Gint) return Boolean;

   type Cb_GObject_Gint_Boolean is not null access function
     (Self   : access Glib.Object.GObject_Record'Class;
      Object : Glib.Gint) return Boolean;

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

   type Cb_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook is not null access function
     (Self : access Gtk_Notebook_Record'Class;
      Page : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X    : Glib.Gint;
      Y    : Glib.Gint) return Gtk_Notebook;

   type Cb_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      Page : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X    : Glib.Gint;
      Y    : Glib.Gint) return Gtk_Notebook;

   Signal_Create_Window : constant Glib.Signal_Name := "create-window";
   procedure On_Create_Window
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook;
       After : Boolean := False);
   procedure On_Create_Window
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::create-window signal is emitted when a detachable tab is dropped
   --  on the root window.
   --
   --  A handler for this signal can create a window containing a notebook
   --  where the tab will be attached. It is also responsible for
   --  moving/resizing the window and adding the necessary properties to the
   --  notebook (e.g. the Gtk.Notebook.Gtk_Notebook:group-name ).
   -- 
   --  Callback parameters:
   --    --  @param Page the tab of Notebook that is being detached
   --    --  @param X the X coordinate where the drop happens
   --    --  @param Y the Y coordinate where the drop happens

   type Cb_Gtk_Notebook_Gtk_Notebook_Tab_Boolean is not null access function
     (Self   : access Gtk_Notebook_Record'Class;
      Object : Gtk_Notebook_Tab) return Boolean;

   type Cb_GObject_Gtk_Notebook_Tab_Boolean is not null access function
     (Self   : access Glib.Object.GObject_Record'Class;
      Object : Gtk_Notebook_Tab) return Boolean;

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

   type Cb_Gtk_Notebook_Gtk_Direction_Type_Void is not null access procedure
     (Self   : access Gtk_Notebook_Record'Class;
      Object : Gtk.Enums.Gtk_Direction_Type);

   type Cb_GObject_Gtk_Direction_Type_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Object : Gtk.Enums.Gtk_Direction_Type);

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
   --    --  @param Child the child Gtk.Widget.Gtk_Widget affected
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
   --    --  @param Child the child Gtk.Widget.Gtk_Widget affected
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
   --    --  @param Child the child Gtk.Widget.Gtk_Widget affected
   --    --  @param Page_Num the new page number for Child

   type Cb_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean is not null access function
     (Self   : access Gtk_Notebook_Record'Class;
      Object : Gtk.Enums.Gtk_Direction_Type;
      P0     : Boolean) return Boolean;

   type Cb_GObject_Gtk_Direction_Type_Boolean_Boolean is not null access function
     (Self   : access Glib.Object.GObject_Record'Class;
      Object : Gtk.Enums.Gtk_Direction_Type;
      P0     : Boolean) return Boolean;

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
   -- 
   --  Callback parameters:

   type Cb_Gtk_Notebook_Boolean_Boolean is not null access function
     (Self   : access Gtk_Notebook_Record'Class;
      Object : Boolean) return Boolean;

   type Cb_GObject_Boolean_Boolean is not null access function
     (Self   : access Glib.Object.GObject_Record'Class;
      Object : Boolean) return Boolean;

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
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Notebook_Record, Gtk_Notebook);
   function "+"
     (Widget : access Gtk_Notebook_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Notebook
   renames Implements_Gtk_Buildable.To_Object;

private
   Tab_Pos_Property : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("tab-pos");
   Show_Tabs_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-tabs");
   Show_Border_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-border");
   Scrollable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("scrollable");
   Page_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("page");
   Group_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("group-name");
   Enable_Popup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-popup");
end Gtk.Notebook;
