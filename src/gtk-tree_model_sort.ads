-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2002 ACT-Europe                 --
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

--  <c_version>1.3.11</c_version>

with Gtk;
with Gtk.Tree_Model;

package Gtk.Tree_Model_Sort is

   type Gtk_Tree_Model_Sort_Record is
     new Gtk.Tree_Model.Gtk_Tree_Model_Record with private;
   type Gtk_Tree_Model_Sort is access all Gtk_Tree_Model_Sort_Record'Class;

   function Get_Type return Glib.GType;
   --  Return the internal type associated with a Gtk_Tree_Model_Sort.

   function Get_Model
     (Tree_Model : access Gtk_Tree_Model_Sort_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Return the model the Gtk_Tree_Model_Sort is sorting.

   function Convert_Child_Path_To_Path
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Child_Path      : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Convert Child_Path to a path relative to Tree_Model_Sort.
   --  That is, Child_Path points to a path in the child model.
   --  The returned path will point to the same row in the sorted model.
   --  If Child_Path isn't a valid path on the child model, then Null
   --  is returned.

   procedure Convert_Child_Iter_To_Iter
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Sort_Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Child_Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Set Sort_Iter to point to the row in Tree_Model_Sort that
   --  corresponds to the row pointed at by Child_Iter.

   function Convert_Path_To_Child_Path
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Sorted_Path     : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Convert Sort_Path to a path on the child model of Tree_Model_Sort.
   --  That is, Sort_Path points ot a location in Tree_Model_Sort.
   --  The returned path will point to the same location in the model
   --  not being sorted.

   procedure Convert_Iter_To_Child_Iter
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Child_Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sorted_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Set Child_Iter to point to the row pointed to by Sorted_Iter.

   procedure Reset_Default_Sort_Func
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record);

   procedure Clear_Cache (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gtk_Tree_Model_Sort_Record is
     new Gtk.Tree_Model.Gtk_Tree_Model_Record with null record;

   pragma Import (C, Get_Type, "gtk_tree_model_sort_get_type");

end Gtk.Tree_Model_Sort;

--  missing:
--  gtk_tree_model_sort_new_with_model
