-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--  </description>
--  <c_version>1.2.6</c_version>
with Gdk; use Gdk;
with Glib.Glist;
with Gtk.Object;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Widget;
with Unchecked_Conversion;

package Gtk.Notebook is

   type Gtk_Notebook_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Notebook is access all Gtk_Notebook_Record'Class;

   type Gtk_Notebook_Page is new Gdk.C_Proxy;
   --  A page of the notebook.
   --  It can contain a single child, and is also associated with a tab
   --  label used to select that page in the notebook.

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
      Position  : in Gint);
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
      Position   : in Gint);
   --  Insert a new page at a specific position in Notebook.
   --  The page is put at the beginning of the list of pages.
   --  The user will select it through a button that contains the
   --  Tab_Label widget, which is generally a Gtk_Label, but could be a box
   --  with a pixmap in it for instance.
   --  A new entry is inserted into the contextual menu. This new entry is
   --  made with Menu_Label.
   --  The first position in the list of pages is 0.

   procedure Remove_Page (Notebook : access Gtk_Notebook_Record;
                          Page_Num : in Gint);
   --  Remove a page from the notebook.
   --  The first position in the list of pages is 0.

   function Get_Child
     (Page : in Gtk_Notebook_Page) return Gtk.Widget.Gtk_Widget;
   --  Return the child from a page.
   --  A page is automatically created by GtkAda when you inserted a
   --  widget in the notebook. The child is the widget you created itself.

   --------------------------------------------
   -- Modifying and getting the current page --
   --------------------------------------------

   function Get_Current_Page (Notebook : access Gtk_Notebook_Record)
                             return Gint;
   --  Get the number of the current page.
   --  The first page has the number 0.

   function Get_Cur_Page
     (Widget : access Gtk_Notebook_Record'Class) return Gtk_Notebook_Page;
   --  Return the current page.

   --  Note: This function returns a record type instead of an access type
   --  because there is no easy way to automatically free the memory for a
   --  Gtk_Notebook_Page..

   function Get_Nth_Page
     (Widget   : access Gtk_Notebook_Record'Class;
      Page_Num : Gint) return Gtk_Widget;
   --  Convert from a page number to the real page.

   function Page_Num (Widget : access Gtk_Notebook_Record'Class;
                      Child  : access Gtk.Widget.Gtk_Widget_Record'Class)
                     return Gint;
   --  Convert from a child to a page number.
   --  Note that Child is not the notebook page, but the widget you inserted
   --  with Insert_Page, Append_Page,...

   procedure Set_Page (Notebook : access Gtk_Notebook_Record;
                       Page_Num : in Gint);
   --  Modify the current page.
   --  The current page is the page that is currently visible on the screen.
   --  Nothing happens if there is no such page.

   procedure Next_Page (Notebook : access Gtk_Notebook_Record);
   --  Display the next page on the screen.

   procedure Prev_Page (Notebook : access Gtk_Notebook_Record);
   --  Display the previous page on the screen.

   -----------------------------
   -- Style and visual aspect --
   -----------------------------

   procedure Set_Show_Border (Notebook    : access Gtk_Notebook_Record;
                              Show_Border : in Boolean);
   --  Indicate whether the notebook should display borders.
   --  This border gives a 3D aspect to the notebook.

   procedure Set_Show_Tabs (Notebook  : access Gtk_Notebook_Record;
                            Show_Tabs : in Boolean);
   --  Indicate whether the tabs should be displayed.
   --  If the tabs are not displayed, the only way for the user to select a
   --  new page is through the contextual menu, and thus you should make sure
   --  that the pages were created with the Insert_Page_Menu, ... subprograms.

   procedure Set_Tab_Pos (Notebook : access Gtk_Notebook_Record;
                          Pos      : in Gtk.Enums.Gtk_Position_Type);
   --  Change the position of the tabs.
   --  The tabs can be display on any of the four sides of the notebook.

   function Get_Tab_Pos (Widget : access Gtk_Notebook_Record)
                        return Gtk.Enums.Gtk_Position_Type;
   --  Return the current position of the tabs.

   procedure Set_Homogeneous_Tabs (Notebook    : access Gtk_Notebook_Record;
                                   Homogeneous : in Boolean);
   --  Indicate whether all the tabs should have the same size (width or
   --  height, depending on which side they are displayed on).

   procedure Set_Tab_Border (Notebook     : access Gtk_Notebook_Record;
                             Border_Width : in Gint);
   --  Change the width of the tabs' borders.
   --  This modifies both the horizontal border and the vertical border.

   procedure Set_Tab_Hborder (Notebook     : access Gtk_Notebook_Record;
                              Border_Width : in Gint);
   --  Modify the width of the horizontal borders of the tabs.

   procedure Set_Tab_Vborder (Notebook     : access Gtk_Notebook_Record;
                              Border_Width : in Gint);
   --  Modify the height of the vertical borders of the tabs.

   procedure Set_Scrollable (Notebook   : access Gtk_Notebook_Record;
                             Scrollable : in Boolean);
   --  Indicate whether Notebook display scrolling arrows when there are
   --  too many tabs.
   --  The default is not to display such scrolling arrows. Note also that
   --  a notebook with too many pages, even if the scrolling is activated,
   --  is sometimes hard to use for the user.

   ----------------
   -- Popup Menu --
   ----------------
   --  The pages of a notebook can be selected both via tabs and a contextual
   --  menu (right mouse button). Note however that the menu is available only
   --  if the pages were inserted with Insert_Page_Menu, Append_Page_Menu or
   --  Prepend_Page_Menu.

   procedure Popup_Disable (Notebook : access Gtk_Notebook_Record);
   --  Disable the popup menu.
   --  This menu won't be display any more when the user pressed the right
   --  mouse button.

   procedure Popup_Enable (Notebook : access Gtk_Notebook_Record);
   --  Enable the popup menu.
   --  When the user pressed the right mouse button, a menu is selected that
   --  allows him to select a new page.

   ---------------------
   -- Page properties --
   ---------------------

   function Get_Tab_Label (Page : in Gtk_Notebook_Page)
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
      Tab_Text : in String);
   --  Modify the text displayed in the tab for the page that contains Child.
   --  This is a less general form of Set_Tab_Label above.

   --  <doc_ignore>
   --  This function is now obsolete, please do not use it.
   procedure Set_Tab (Notebook  : access Gtk_Notebook_Record;
                      Page_Num  : in Gint;
                      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Set Notebook tab widget
   --  </doc_ignore>

   function Get_Menu_Label (Page : in Gtk_Notebook_Page)
                           return Gtk.Widget.Gtk_Widget;
   --  Return the widget displayed in the contextual menu for the page.
   --  This is the widget given in argument to Insert_Page_Menu,
   --  Append_Page_Menu and Prepend_Page_Menu

   procedure Set_Menu_Label
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Modify the widget displayed in the contextual menu for the page
   --  that contains Child.

   procedure Set_Menu_Label_Text
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Text : in String);
   --  Modify the text displayed in the contextual menu for the page that
   --  contains Child.
   --  This is a less general form of Set_Menu_Label above.

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
      Expand    : in Boolean;
      Fill      : in Boolean;
      Pack_Type : in Gtk.Enums.Gtk_Pack_Type);
   --  Modify the packing used for the tab associated with the page that
   --  contains Child.

   procedure Reorder_Child
     (Notebook : access Gtk_Notebook_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : Gint);
   --  Change the position of the page that contains Child.

   -------------------
   -- List of pages --
   -------------------

   --  <doc_ignore>
   function Convert is new Unchecked_Conversion
     (Gtk_Notebook_Page, System.Address);
   function Convert is new Unchecked_Conversion
     (System.Address, Gtk_Notebook_Page);
   package Page_List is new Glib.Glist.Generic_List (Gtk_Notebook_Page);
   --  </doc_ignore>

   function Get_Children
     (Widget : access Gtk_Notebook_Record) return Page_List.Glist;
   --  Return the list of all pages in the notebook.

   ----------------------------
   -- Support for GATE/DGATE --
   ----------------------------

   procedure Generate (N        : in Node_Ptr;
                       File     : in File_Type);
   --  Gate internal function

   procedure Generate (Notebook : in out Gtk.Object.Gtk_Object;
                       N        : in Node_Ptr);
   --  Dgate internal function

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "switch_page"
   --    procedure Handler (Notebook : access Gtk_Notebook_Record'Class;
   --                       Page     : in Gtk_Notebook_Page;
   --                       Page_Num : Guint);
   --
   --   Modify when the current page is modified in the notebook.
   --   This is called every time the user selected a new page, or the
   --   program selected a new page with Next_Page, Prev_Page, ...
   --  </signals>
private
   type Gtk_Notebook_Record is new Gtk.Container.Gtk_Container_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_notebook_get_type");
end Gtk.Notebook;
