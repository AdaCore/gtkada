-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  A Gtk_Notebook is a container that displays all of its children at the
--  same location on the screen. They are organized into pages, that can be
--  selected through tabs (either by clicking on them or by a contextual
--  menu).
--  This is the best way to organize complicated interfaces that have a lot
--  of widgets, by putting the children into groups of coherent widgets.
--
--  You can hide some of the pages of the notebook by simply calling Hide on
--  the widget that is contained in the page (or returned from Get_Nth_Page).
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Layout containers</group>

with Glib.Glist;
pragma Elaborate_All (Glib.Glist);
with Glib.Properties;
with Glib.Values;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Widget;
with Unchecked_Conversion;

package Gtk.Notebook is

   type Gtk_Notebook_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Notebook is access all Gtk_Notebook_Record'Class;

   subtype Gtk_Notebook_Page is Gtk.Gtk_Notebook_Page;

   type Gtk_Notebook_Tab is
     (Notebook_Tab_First,
      Notebook_Tab_Last);
   pragma Convention (C, Gtk_Notebook_Tab);

   ---------------------------------------------
   -- Creating a notebook and inserting pages --
   ---------------------------------------------

   procedure Gtk_New (Widget : out Gtk_Notebook);
   --  Create a new empty notebook.

   procedure Initialize (Widget : access Gtk_Notebook_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Notebook.

   procedure Append_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Insert a new page in Notebook.
   --  The page is put at the end of the list of pages.
   --  The user will select it through a button that contains the
   --  Tab_Label widget, which is generally a Gtk_Label, but could be a box
   --  with a pixmap in it for instance.
   --  No entry is associated with the page in the contextual menu.

   procedure Append_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Same as above, but no label is specified.

   procedure Append_Page_Menu
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Insert a new page in Notebook.
   --  The page is put at the end of the list of pages.
   --  The user will select it through a button that contains the
   --  Tab_Label widget, which is generally a Gtk_Label, but could be a box
   --  with a pixmap in it for instance.
   --  A new entry is inserted into the contextual menu. This new entry is
   --  made with Menu_Label.

   procedure Prepend_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Insert a new page in Notebook.
   --  The page is put at the beginning of the list of pages.
   --  The user will select it through a button that contains the
   --  Tab_Label widget, which is generally a Gtk_Label, but could be a box
   --  with a pixmap in it for instance.
   --  No entry is associated with the page in the contextual menu.

   procedure Prepend_Page_Menu
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Insert a new page in Notebook.
   --  The page is put at the beginning of the list of pages.
   --  The user will select it through a button that contains the
   --  Tab_Label widget, which is generally a Gtk_Label, but could be a box
   --  with a pixmap in it for instance.
   --  A new entry is inserted into the contextual menu. This new entry is
   --  made with Menu_Label.

   procedure Insert_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position  : Gint);
   --  Insert a new page at a specific position in Notebook.
   --  The page is put at the beginning of the list of pages.
   --  The user will select it through a button that contains the
   --  Tab_Label widget, which is generally a Gtk_Label, but could be a box
   --  with a pixmap in it for instance.
   --  No entry is associated with the page in the contextual menu.
   --  The first position in the list of pages is 0.

   procedure Insert_Page_Menu
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position   : Gint);
   --  Insert a new page at a specific position in Notebook.
   --  The page is put at the beginning of the list of pages.
   --  The user will select it through a button that contains the
   --  Tab_Label widget, which is generally a Gtk_Label, but could be a box
   --  with a pixmap in it for instance.
   --  A new entry is inserted into the contextual menu. This new entry is
   --  made with Menu_Label.
   --  The first position in the list of pages is 0.

   procedure Remove_Page
     (Notebook : access Gtk_Notebook_Record; Page_Num : Gint);
   --  Remove a page from the notebook.
   --  The first position in the list of pages is 0.

   --------------------------------------------
   -- Modifying and getting the current page --
   --------------------------------------------

   function Get_Current_Page
     (Notebook : access Gtk_Notebook_Record) return Gint;
   --  Get the number of the current page.
   --  The first page has the number 0.

   function Get_Nth_Page
     (Widget   : access Gtk_Notebook_Record'Class;
      Page_Num : Gint) return Gtk.Widget.Gtk_Widget;
   --  Convert from a page number to the real page.

   function Get_N_Pages
     (Notebook : access Gtk_Notebook_Record) return Gint;
   --  Return the number of pages in the notebook

   function Page_Num
     (Widget : access Gtk_Notebook_Record'Class;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint;
   --  Convert from a child to a page number.
   --  Note that Child is not the notebook page, but the widget you inserted
   --  with Insert_Page, Append_Page,...

   procedure Set_Current_Page
     (Notebook : access Gtk_Notebook_Record;
      Page_Num : in Gint := -1);
   --  Modify the current page.
   --  The current page is the page that is currently visible on the screen.
   --  Nothing happens if there is no such page.
   --  Note also that the page has to be visible on the screen (ie you must
   --  have called Gtk.Widget.Show on it first).
   --  Use -1 to set the current page to the last one.
   --
   --  Note: This call won't succeeded unless you have called Show on the
   --  widget displayed in the page.

   procedure Set_Page
     (Notebook : access Gtk_Notebook_Record;
      Page_Num : in Gint := -1)
     renames Set_Current_Page;
   --  This function is deprecated. Use Set_Current_Page instead.

   procedure Next_Page (Notebook : access Gtk_Notebook_Record);
   --  Display the next page on the screen.

   procedure Prev_Page (Notebook : access Gtk_Notebook_Record);
   --  Display the previous page on the screen.

   -----------------------------
   -- Style and visual aspect --
   -----------------------------

   procedure Set_Show_Border
     (Notebook    : access Gtk_Notebook_Record;
      Show_Border : Boolean := True);
   --  Indicate whether the notebook should display borders.
   --  This border gives a 3D aspect to the notebook.

   function Get_Show_Border
     (Notebook : access Gtk_Notebook_Record) return Boolean;
   --  Return whether the notebook displays borders.

   procedure Set_Show_Tabs
     (Notebook  : access Gtk_Notebook_Record;
      Show_Tabs : Boolean := True);
   --  Indicate whether the tabs should be displayed.
   --  If the tabs are not displayed, the only way for the user to select a
   --  new page is through the contextual menu, and thus you should make sure
   --  that the pages were created with the Insert_Page_Menu, ... subprograms.

   function Get_Show_Tabs
     (Notebook : access Gtk_Notebook_Record) return Boolean;
   --  Return whether the tabs are displayed.

   procedure Set_Tab_Pos
     (Notebook : access Gtk_Notebook_Record;
      Pos      : Gtk.Enums.Gtk_Position_Type);
   --  Change the position of the tabs.
   --  The tabs can be displayed on any of the four sides of the notebook.

   function Get_Tab_Pos
     (Widget : access Gtk_Notebook_Record) return Gtk.Enums.Gtk_Position_Type;
   --  Return the current position of the tabs.

   procedure Set_Scrollable
     (Notebook   : access Gtk_Notebook_Record;
      Scrollable : in Boolean := True);
   --  Indicate whether Notebook display scrolling arrows when there are
   --  too many tabs.
   --  The default is not to display such scrolling arrows. Note also that
   --  a notebook with too many pages, even if the scrolling is activated,
   --  is sometimes hard to use for the user.

   function Get_Scrollable
     (Notebook : access Gtk_Notebook_Record) return Boolean;
   --  Return whether Notebook is scrollable.
   --  See Set_Scrollable for more details.

   ----------------
   -- Popup Menu --
   ----------------
   --  The pages of a notebook can be selected both via tabs and a contextual
   --  menu (right mouse button). Note however that the menu is available only
   --  if the pages were inserted with Insert_Page_Menu, Append_Page_Menu or
   --  Prepend_Page_Menu.

   procedure Popup_Enable (Notebook : access Gtk_Notebook_Record);
   --  Enable the popup menu.
   --  When the user pressed the right mouse button, a menu is selected that
   --  allows him to select a new page.

   procedure Popup_Disable (Notebook : access Gtk_Notebook_Record);
   --  Disable the popup menu.
   --  This menu won't be display any more when the user pressed the right
   --  mouse button.

   ---------------------
   -- Page properties --
   ---------------------

   function Get_Tab_Label
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   --  Return the widget displayed in the tab used to select Page.
   --  This widget is in fact the one given in argument to Insert_Page,etc.
   --  when the page was created.

   procedure Set_Tab_Label
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Modify the widget displayed in the tab for the page that contains Child.
   --  Tab_Label is generally a Gtk_Label, although it can also be a Gtk_Box
   --  that contains a Gtk_Pixmap and a Gtk_Label if you want to show pixmaps.

   procedure Set_Tab_Label_Text
     (Notebook : access Gtk_Notebook_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Text : UTF8_String);
   --  Modify the text displayed in the tab for the page that contains Child.
   --  This is a less general form of Set_Tab_Label above.

   function Get_Tab_Label_Text
     (Notebook : access Gtk_Notebook_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class) return UTF8_String;
   --  Return the text displayed in the tab for the page that contains Child.

   procedure Set_Tab
     (Notebook  : access Gtk_Notebook_Record;
      Page_Num  : Gint;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Set Notebook tab widget for a given page number.
   --  This function is mainly intended for use by Gate.

   function Get_Menu_Label
     (Notebook : access Gtk_Notebook_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   --  Return the widget displayed in the contextual menu for the Child.
   --  This is the widget given in argument to Insert_Page_Menu,
   --  Append_Page_Menu and Prepend_Page_Menu.

   procedure Set_Menu_Label
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Modify the widget displayed in the contextual menu for the page
   --  that contains Child.

   procedure Set_Menu_Label_Text
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Text : UTF8_String);
   --  Modify the text displayed in the contextual menu for the page that
   --  contains Child.
   --  This is a less general form of Set_Menu_Label above.

   function Get_Menu_Label_Text
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class)
      return UTF8_String;
   --  Return the text displayed in the contextual menu for the page that
   --  contains Child.

   procedure Query_Tab_Label_Packing
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand     : out Boolean;
      Fill       : out Boolean;
      Pack_Type  : out Gtk.Enums.Gtk_Pack_Type);
   --  Return the packing used for the tab associated with the page
   --  that contains Child.
   --  See the Gtk.Box package for more information on the parameters.

   procedure Set_Tab_Label_Packing
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand    : Boolean;
      Fill      : Boolean;
      Pack_Type : Gtk.Enums.Gtk_Pack_Type);
   --  Modify the packing used for the tab associated with the page that
   --  contains Child.

   procedure Reorder_Child
     (Notebook : access Gtk_Notebook_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : Gint);
   --  Change the position of the page that contains Child.

   --------------------
   -- GValue support --
   --------------------

   function Get_Notebook_Page
     (Value : Glib.Values.GValue) return Gtk_Notebook_Page;
   --  Convert a Value into a notebook page.

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   function Convert is new Unchecked_Conversion
     (Gtk_Notebook_Page, System.Address);
   function Convert is new Unchecked_Conversion
     (System.Address, Gtk_Notebook_Page);
   package Page_List is new Glib.Glist.Generic_List (Gtk_Notebook_Page);

   function Get_Children
     (Widget : access Gtk_Notebook_Record) return Page_List.Glist;
   pragma Obsolescent;
   --  Return the list of all pages in the notebook.

   procedure Set_Homogeneous_Tabs
     (Notebook    : access Gtk_Notebook_Record;
      Homogeneous : in Boolean := True);
   pragma Obsolescent;  --  Set_Homogeneous_Tabs
   --  Indicate whether all the tabs should have the same size (width or
   --  height, depending on which side they are displayed on).

   procedure Set_Tab_Border
     (Notebook     : access Gtk_Notebook_Record;
      Border_Width : in Gint);
   pragma Obsolescent;  --  Set_Tab_Border
   --  Change the width of the tabs' borders.
   --  This modifies both the horizontal border and the vertical border.

   procedure Set_Tab_Hborder
     (Notebook     : access Gtk_Notebook_Record;
      Border_Width : in Gint);
   pragma Obsolescent;  --  Set_Tab_Hborder
   --  Modify the width of the horizontal borders of the tabs.

   procedure Set_Tab_Vborder
     (Notebook     : access Gtk_Notebook_Record;
      Border_Width : in Gint);
   pragma Obsolescent;  --  Set_Tab_Vborder
   --  Modify the height of the vertical borders of the tabs.

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name: Page_Property
   --  Type: Gint
   --  See:  Set_Current_Page / Get_Current_Page
   --
   --  Name: Tab_Pos_Property
   --  Type: Gtk_Position_Type
   --  See:  Set_Tab_Pos / Get_Tab_Pos
   --
   --  Name: Tab_Border_Property
   --  Type: Guint
   --  Descr: Width of the border around the tab labels
   --
   --  Name: Tab_HBorder_Property
   --  Type: Guint
   --  Descr: Width of the horizontal border around the tab labels
   --
   --  Name: Tab_VBorder_Property
   --  Type: Guint
   --  Descr: Width of the vertical border around the tab labels
   --
   --  Name: Show_Tabs_Property
   --  Type: Boolean
   --  See:  Set_Show_Tabs / Get_Show_Tabs
   --
   --  Name: Show_Border_Property
   --  Type: Boolean
   --  See:  Set_Show_Border / Get_Show_Border
   --
   --  Name: Scrollable_Property
   --  Type: Boolean
   --  See:  Set_Scrollable / Get_Scrollable
   --
   --  Name: Enable_Popup_Property
   --  Type: Boolean
   --  See:  Popup_Enable / Popup_Disable
   --
   --  Name: Homogeneous_Property
   --  Type: Boolean
   --  See:  Set_Homogeneous_Tabs / -
   --
   --  </properties>

   Page_Property         : constant Glib.Properties.Property_Int;
   Tab_Pos_Property      : constant Gtk.Enums.Property_Gtk_Position_Type;
   Tab_Border_Property   : constant Glib.Properties.Property_Uint;
   Tab_Hborder_Property  : constant Glib.Properties.Property_Uint;
   Tab_Vborder_Property  : constant Glib.Properties.Property_Uint;
   Show_Tabs_Property    : constant Glib.Properties.Property_Boolean;
   Show_Border_Property  : constant Glib.Properties.Property_Boolean;
   Scrollable_Property   : constant Glib.Properties.Property_Boolean;
   Enable_Popup_Property : constant Glib.Properties.Property_Boolean;
   Homogeneous_Property  : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "switch_page"
   --    procedure Handler
   --      (Notebook : access Gtk_Notebook_Record'Class;
   --       Page     : Gtk_Notebook_Page;
   --       Page_Num : Guint);
   --   Notify when the current page is modified in the notebook.
   --   This is called every time the user selected a new page, or the
   --   program selected a new page with Next_Page, Prev_Page, ...
   --
   --  - "select_page"
   --    function Handler
   --      (Notebook   : access Gtk_Notebook_Record'Class;
   --       Move_Focus : Boolean) return Boolean;
   --    You should emit this signal to request that the notebook selects the
   --    page corresponding to the focus tab. If Move_Focus is true, the page
   --    acquires the keyboard focus.
   --    Seldom used.
   --
   --  - "focus_tab"
   --    function Handler
   --       (Notebook : access Gtk_Notebook_Record'Class;
   --        Tab      : Gtk_Notebook_Tab) return Boolean;
   --    Gives the focus to one of the tabs in the notebook. This signal is
   --    mostly used as a keybinding for Home, End,... so that the proper
   --    behavior can be implemented
   --
   --  - "move_focus_out"
   --    procedure Handler
   --      (Notebook  : access Gtk_Notebook_Record'Class;
   --       Direction : Gtk_Direction_Type);
   --    You should emit this signal to request that the focus be transfered
   --    from the current page to the parent widget.
   --    Seldom used.
   --
   --  - "change_current_page"
   --    procedure Handler
   --      (Notebook : access Gtk_Notebook_Record'Class;
   --       Offset   : Gint);
   --    You should emit this signal to request that the notebook selects
   --    another page as the current page. The offset is relative to the
   --    currently selected page.
   --
   --  </signals>

   Signal_Switch_Page         : constant String := "switch_page";
   Signal_Select_Page         : constant String := "select_page";
   Signal_Focus_Tab           : constant String := "focus_tab";
   Signal_Move_Focus_Out      : constant String := "move_focus_out";
   Signal_Change_Current_Page : constant String := "change_current_page";

private
   type Gtk_Notebook_Record is new Gtk.Container.Gtk_Container_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_notebook_get_type");

   Page_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("page");
   Tab_Pos_Property : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("tab-pos");
   Tab_Border_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("tab-border");
   Tab_Hborder_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("tab-hborder");
   Tab_Vborder_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("tab-vborder");
   Show_Tabs_Property   : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-tabs");
   Show_Border_Property   : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-border");
   Scrollable_Property   : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("scrollable");
   Enable_Popup_Property   : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-popup");
   Homogeneous_Property  : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("homgeneous");
end Gtk.Notebook;
