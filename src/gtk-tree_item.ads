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
with Gtk.Item;
with Gtk.Tree;
with Gtk.Widget;

package Gtk.Tree_Item is

   type Gtk_Tree_Item_Record is new Gtk.Item.Gtk_Item_Record with private;
   type Gtk_Tree_Item is access all Gtk_Tree_Item_Record'Class;

   procedure Collapse (Tree_Item : access Gtk_Tree_Item_Record);
   procedure Deselect (Tree_Item : access Gtk_Tree_Item_Record);
   procedure Expand (Tree_Item : access Gtk_Tree_Item_Record);
   function Get_Subtree (Tree_Item : access Gtk_Tree_Item_Record)
     return Gtk.Tree.Gtk_Tree;
   procedure Gtk_New (Tree_Item : out Gtk_Tree_Item;
                      Label     : in String := "");
   procedure Initialize (Tree_Item : access Gtk_Tree_Item_Record;
                         Label     : in String := "");
   procedure Gtk_Select (Tree_Item : access Gtk_Tree_Item_Record);
   procedure Remove_Subtree (Tree_Item : access Gtk_Tree_Item_Record);
   procedure Set_Subtree
     (Tree_Item : access Gtk_Tree_Item_Record;
      Subtree   : in Gtk.Widget.Gtk_Widget);

   --  GtkAda0.6 : From_Tree and To_Tree have been removed. Please use
   --  Gdk.Unchecked_Cast insted


   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N         : in Node_Ptr;
                       File      : in File_Type);

   procedure Generate (Tree_Item : in out Gtk_Object;
                       N         : in Node_Ptr);

private
   type Gtk_Tree_Item_Record is new Gtk.Item.Gtk_Item_Record with null record;

end Gtk.Tree_Item;
