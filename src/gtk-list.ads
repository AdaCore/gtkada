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

with Gtk.Container;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;

package Gtk.List is

   type Gtk_List is new Gtk.Container.Gtk_Container with private;

   procedure Append_Items
      (List  : in Gtk_List;
       Items : in Widget_List.Glist);
   function Child_Position
      (List   : in Gtk_List;
       Child  : in Gtk.Widget.Gtk_Widget'Class)
       return      Gint;
   procedure Clear_Items
      (List    : in Gtk_List;
       Start   : in Gint;
       The_End : in Gint);
   function Get_Children (Widget : in Gtk.List.Gtk_List)
                          return      Widget_List.Glist;
   function Get_Selection (Widget : in Gtk.List.Gtk_List)
                           return      Widget_List.Glist;
   procedure Gtk_New (Widget : out Gtk_List);
   procedure Insert_Items
      (List     : in Gtk_List;
       Items    : in Widget_List.Glist;
       Position : in Gint);
   procedure Prepend_Items
      (List  : in Gtk_List;
       Items : in Widget_List.Glist);
   procedure Remove_Items
      (List  : in Gtk_List;
       Items : in Widget_List.Glist);
   procedure Remove_Items_No_Unref
      (List  : in Gtk_List;
       Items : in Widget_List.Glist);
   procedure Select_Child
      (List  : in Gtk_List;
       Child : in Gtk.Widget.Gtk_Widget'Class);
   procedure Select_Item
      (List : in Gtk_List;
       Item : in Gint);
   procedure Set_Selection_Mode
      (List : in Gtk_List;
       Mode : in Gtk_Selection_Mode);
   procedure Unselect_Child
      (List  : in Gtk_List;
       Child : in Gtk.Widget.Gtk_Widget'Class);
   procedure Unselect_Item
      (List : in Gtk_List;
       Item : in Gint);

private
   type Gtk_List is new Gtk.Container.Gtk_Container with null record;

end Gtk.List;
