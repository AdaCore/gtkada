-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
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

with Gdk; use Gdk;
with Gtk.Box;
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Notebook is

   type Gtk_Notebook is new Gtk.Container.Gtk_Container with private;
   type Gtk_Notebook_Page is new Root_Type with private;

   procedure Append_Page
      (Notebook  : in Gtk_Notebook'Class;
       Child     : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label : in Gtk.Box.Gtk_Box'Class);
   procedure Append_Page_Menu
      (Notebook   : in Gtk_Notebook'Class;
       Child      : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label  : in Gtk.Box.Gtk_Box'Class;
       Menu_Label : in Gtk.Box.Gtk_Box'Class);
   function Current_Page (Notebook : in Gtk_Notebook'Class)
                          return        Gint;
   function Get_Children (Widget : in Gtk_Notebook'Class)
                          return      Widget_List.Glist;
   function Get_Cur_Page (Widget : in Gtk_Notebook'Class)
                          return      Gtk_Notebook_Page;
   function Get_Menu_Label (Page : in Gtk_Notebook_Page)
                            return Gtk.Box.Gtk_Box;
   function Get_Tab_Label (Page : in Gtk_Notebook_Page)
                           return Gtk.Box.Gtk_Box;
   function Get_Tab_Pos (Widget : in Gtk_Notebook'Class)
                         return      Gtk_Position_Type;
   procedure Gtk_New (Widget : out Gtk_Notebook);
   procedure Insert_Page
      (Notebook  : in Gtk_Notebook'Class;
       Child     : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label : in Gtk.Box.Gtk_Box'Class;
       Position  : in Gint);
   procedure Insert_Page_Menu
      (Notebook   : in Gtk_Notebook'Class;
       Child      : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label  : in Gtk.Box.Gtk_Box'Class;
       Menu_Label : in Gtk.Box.Gtk_Box'Class;
       Position   : in Gint);
   procedure Next_Page (Notebook : in out Gtk_Notebook'Class);
   procedure Popup_Disable (Notebook : in Gtk_Notebook'Class);
   procedure Popup_Enable (Notebook : in Gtk_Notebook'Class);
   procedure Prepend_Page
      (Notebook  : in Gtk_Notebook'Class;
       Child     : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label : in Gtk.Box.Gtk_Box'Class);
   procedure Prepend_Page_Menu
      (Notebook   : in Gtk_Notebook'Class;
       Child      : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label  : in Gtk.Box.Gtk_Box'Class;
       Menu_Label : in Gtk.Box.Gtk_Box'Class);
   procedure Prev_Page (Notebook : in out Gtk_Notebook'Class);
   procedure Remove_Page
      (Notebook : in Gtk_Notebook'Class;
       Page_Num : in Gint);
   procedure Set_Page
      (Notebook : in Gtk_Notebook'Class;
       Page_Num : in Gint);
   procedure Set_Scrollable
      (Notebook   : in Gtk_Notebook'Class;
       Scrollable : in Boolean);
   procedure Set_Show_Border
      (Notebook    : in Gtk_Notebook'Class;
       Show_Border : in Boolean);
   procedure Set_Show_Tabs
      (Notebook  : in Gtk_Notebook'Class;
       Show_Tabs : in Boolean);
   procedure Set_Tab_Border
      (Notebook     : in Gtk_Notebook'Class;
       Border_Width : in Gint);
   procedure Set_Tab_Pos
      (Notebook : in Gtk_Notebook'Class;
       Pos      : in Gtk_Position_Type);

private
   type Gtk_Notebook is new Gtk.Container.Gtk_Container with null record;
   type Gtk_Notebook_Page is new Root_Type with null record;

   --  mapping: Append_Page gtknotebook.h gtk_notebook_append_page
   --  mapping: Append_Page_Menu gtknotebook.h gtk_notebook_append_page_menu
   --  mapping: Current_Page gtknotebook.h gtk_notebook_current_page
   --  mapping: NOT_IMPLEMENTED gtknotebook.h gtk_notebook_get_type
   --  mapping: Gtk_New gtknotebook.h gtk_notebook_new
   --  mapping: Insert_Page gtknotebook.h gtk_notebook_insert_page
   --  mapping: Insert_Page_Menu gtknotebook.h gtk_notebook_insert_page_menu
   --  mapping: Next_Page gtknotebook.h gtk_notebook_next_page
   --  mapping: Popup_Disable gtknotebook.h gtk_notebook_popup_disable
   --  mapping: Popup_Enable gtknotebook.h gtk_notebook_popup_enable
   --  mapping: Prepend_Page gtknotebook.h gtk_notebook_prepend_page
   --  mapping: Prepend_Page_Menu gtknotebook.h gtk_notebook_prepend_page_menu
   --  mapping: Prev_Page gtknotebook.h gtk_notebook_prev_page
   --  mapping: Remove_Page gtknotebook.h gtk_notebook_remove_page
   --  mapping: Set_Page gtknotebook.h gtk_notebook_set_page
   --  mapping: Set_Scrollable gtknotebook.h gtk_notebook_set_scrollable
   --  mapping: Set_Show_Border gtknotebook.h gtk_notebook_set_show_border
   --  mapping: Set_Show_Tabs gtknotebook.h gtk_notebook_set_show_tabs
   --  mapping: Set_Tab_Border gtknotebook.h gtk_notebook_set_tab_border
   --  mapping: Set_Tab_Pos gtknotebook.h gtk_notebook_set_tab_pos
end Gtk.Notebook;
