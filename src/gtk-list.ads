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

   type Gtk_List_Record is new Gtk.Container.Gtk_Container_Record with private;
   type Gtk_List is access all Gtk_List_Record'Class;

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_List.

   procedure Append_Items
     (List  : access Gtk_List_Record;
      Items : in Widget_List.Glist);

   function Child_Position
     (List   : access Gtk_List_Record;
      Child  : in Gtk.Widget.Gtk_Widget) return Gint;

   procedure Clear_Items
     (List    : access Gtk_List_Record;
      Start   : in Gint;
      The_End : in Gint);
   --  Remove some items from the list.
   --  if The_End is negative, it is assigned the position of the last item
   --  in the list.
   --  The first item in the list has an index of 0.

   function Get_Children
     (Widget : access Gtk.List.Gtk_List_Record) return Widget_List.Glist;

   function Get_Selection
     (Widget : access Gtk.List.Gtk_List_Record) return Widget_List.Glist;

   procedure Gtk_New (Widget : out Gtk_List);

   procedure Initialize (Widget : access Gtk_List_Record'Class);

   procedure Insert_Items
     (List     : access Gtk_List_Record;
      Items    : in Widget_List.Glist;
      Position : in Gint);

   procedure Prepend_Items
     (List  : access Gtk_List_Record;
      Items : in Widget_List.Glist);

   procedure Remove_Items
     (List  : access Gtk_List_Record;
      Items : in Widget_List.Glist);

   procedure Remove_Items_No_Unref
     (List  : access Gtk_List_Record;
      Items : in Widget_List.Glist);

   procedure Select_Child
     (List  : access Gtk_List_Record;
      Child : in Gtk.Widget.Gtk_Widget);

   procedure Select_Item
     (List : access Gtk_List_Record;
      Item : in Gint);

   procedure Set_Selection_Mode
     (List : access Gtk_List_Record;
      Mode : in Gtk_Selection_Mode);

   procedure Unselect_Child
     (List  : access Gtk_List_Record;
      Child : in Gtk.Widget.Gtk_Widget);

   procedure Unselect_Item
     (List : access Gtk_List_Record;
      Item : in Gint);

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private
   type Gtk_List_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_list_get_type");
end Gtk.List;
