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

with Gdk; use Gdk;
with Gtk.Box;    use Gtk.Box;
with Gtk.Container;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Notebook is

   type Gtk_Notebook is new Gtk.Container.Gtk_Container with private;
   type Gtk_Notebook_Page is new Root_Type with private;

   procedure Append_Page
      (Notebook  : in Gtk_Notebook;
       Child     : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label : in Gtk.Box.Gtk_Box'Class);
   procedure Append_Page_Menu
      (Notebook   : in Gtk_Notebook;
       Child      : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label  : in Gtk.Box.Gtk_Box'Class;
       Menu_Label : in Gtk.Box.Gtk_Box'Class);
   function Get_Current_Page (Notebook : in Gtk_Notebook) return Gint;
   function Get_Children (Widget : in Gtk_Notebook) return Widget_List.Glist;
   function Get_Cur_Page (Widget : in Gtk_Notebook'Class)
                          return Gtk_Notebook_Page;
   function Get_Menu_Label (Page : in Gtk_Notebook_Page) return Gtk_Box;
   function Get_Tab_Label (Page : in Gtk_Notebook_Page) return Gtk_Box;
   function Get_Tab_Pos (Widget : in Gtk_Notebook) return Gtk_Position_Type;
   procedure Gtk_New (Widget : out Gtk_Notebook);
   procedure Insert_Page
      (Notebook  : in Gtk_Notebook;
       Child     : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label : in Gtk.Box.Gtk_Box'Class;
       Position  : in Gint);
   procedure Insert_Page_Menu
      (Notebook   : in Gtk_Notebook;
       Child      : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label  : in Gtk.Box.Gtk_Box'Class;
       Menu_Label : in Gtk.Box.Gtk_Box'Class;
       Position   : in Gint);
   procedure Next_Page (Notebook : in out Gtk_Notebook);
   procedure Popup_Disable (Notebook : in Gtk_Notebook);
   procedure Popup_Enable (Notebook : in Gtk_Notebook);
   procedure Prepend_Page
      (Notebook  : in Gtk_Notebook;
       Child     : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label : in Gtk.Box.Gtk_Box'Class);
   procedure Prepend_Page_Menu
      (Notebook   : in Gtk_Notebook;
       Child      : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label  : in Gtk.Box.Gtk_Box'Class;
       Menu_Label : in Gtk.Box.Gtk_Box'Class);
   procedure Prev_Page (Notebook : in out Gtk_Notebook);
   procedure Remove_Page
      (Notebook : in Gtk_Notebook;
       Page_Num : in Gint);
   procedure Set_Page
      (Notebook : in Gtk_Notebook;
       Page_Num : in Gint);
   procedure Set_Scrollable
      (Notebook   : in Gtk_Notebook;
       Scrollable : in Boolean);
   procedure Set_Show_Border
      (Notebook    : in Gtk_Notebook;
       Show_Border : in Boolean);
   procedure Set_Show_Tabs
      (Notebook  : in Gtk_Notebook;
       Show_Tabs : in Boolean);
   procedure Set_Tab_Border
      (Notebook     : in Gtk_Notebook;
       Border_Width : in Gint);
   procedure Set_Tab_Pos
      (Notebook : in Gtk_Notebook;
       Pos      : in Gtk_Position_Type);

private
   type Gtk_Notebook is new Gtk.Container.Gtk_Container with null record;
   type Gtk_Notebook_Page is new Root_Type with null record;

end Gtk.Notebook;
