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

with Gtk.Object; use Gtk.Object;
with Gtk.Container;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Tree is

   type Gtk_Tree_Record is new Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Tree is access all Gtk_Tree_Record'Class;

   procedure Append
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class);
   function Child_Position
     (Tree   : access Gtk_Tree_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class)
      return      Gint;
   procedure Clear_Items
     (Tree    : access Gtk_Tree_Record;
      Start   : in Gint;
      The_End : in Gint);
   function Get_Children (Widget : access Gtk.Tree.Gtk_Tree_Record)
                          return      Widget_List.Glist;
   function Get_Selection (Widget : access Gtk.Tree.Gtk_Tree_Record)
                           return      Widget_List.Glist;
   procedure Gtk_New (Widget : out Gtk_Tree);
   procedure Initialize (Widget : access Gtk_Tree_Record);
   procedure Insert
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position  : in Gint);
   procedure Prepend
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Remove_Items
     (Tree  : access Gtk_Tree_Record;
      Items : in Widget_List.Glist);
   procedure Select_Child
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Select_Item
     (Tree : access Gtk_Tree_Record;
      Item : in Gint);
   procedure Set_Selection_Mode
     (Tree : access Gtk_Tree_Record;
      Mode : in Gtk_Selection_Mode);
   procedure Set_View_Lines
     (Tree : access Gtk_Tree_Record;
      Flag : in Boolean);
   procedure Set_View_Mode
     (Tree : access Gtk_Tree_Record;
      Mode : in Gtk_Tree_View_Mode);
   procedure Unselect_Child
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Unselect_Item
     (Tree : access Gtk_Tree_Record;
      Item : in Gint);

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N : in Node_Ptr; File : in File_Type);

   procedure Generate (Tree : in out Gtk_Object; N : in Node_Ptr);

private
   type Gtk_Tree_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

end Gtk.Tree;
