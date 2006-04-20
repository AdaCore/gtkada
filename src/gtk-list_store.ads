-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                      Copyright (C) 2001-2006                      --
--                               AdaCore                             --
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

with Glib.Values;
with Gtk;
with Gtk.Tree_Model;

package Gtk.List_Store is

   type Gtk_List_Store_Record is
     new Gtk.Tree_Model.Gtk_Tree_Model_Record with private;
   type Gtk_List_Store is access all Gtk_List_Store_Record'Class;

   procedure Gtk_New
     (List_Store : out Gtk_List_Store;
      Types      : GType_Array);
   --  Creates a new list store using Types to fill the columns.

   procedure Initialize
     (List_Store : access Gtk_List_Store_Record'Class;
      Types      : GType_Array);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Set_Column_Types
     (List_Store : access Gtk_List_Store_Record;
      Types      : GType_Array);

   procedure Set_Value
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.Values.GValue);
   --  Set the data in the cell specified by Iter and Column.
   --  The type of Value must be convertible to the type of the column.

   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : UTF8_String);
   --  Same as above, for an UTF8 string.

   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gint);
   --  Same as above, for a Gint.

   procedure Remove
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Remove the given row from the list store.
   --  After being removed, Iter is set to be the next valid row, or
   --  invalidated if it pointed to the last row in List_Store.

   procedure Insert
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Position   : Gint);
   --  Create a new row at Position.
   --  Iter will be changed to point to this new row.
   --  If Position is larger than the number of rows on the list, then the new
   --  row will be appended to the list. The row will be empty before this
   --  function is called. To fill in values, you need to call Set_Value.

   procedure Insert_Before
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Insert a new row before Sibling.
   --  If Sibling is Null_Iter, then the row will be appended to the end of the
   --  list. Iter will be changed to point to this new row. The row will be
   --  empty before this function is called. To fill in values, you need to
   --  call Set_Value.

   procedure Insert_After
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Insert a new row after Sibling.
   --  If Sibling is Null_Iter, then the row will be prepended to the beginning
   --  of the list. Iter will be changed to point to this new row. The row will
   --  be empty after this function is called. To fill in values, you need to
   --  call Set_Value.

   procedure Prepend
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Prepend a new row to List_Store.
   --  Iter will be changed to point to this new row. The row will be empty
   --  after this function is called. To fill in values, you need to call
   --  Set_Value.

   procedure Append
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Append a new row to List_Store.
   --  Iter will be changed to point to this new row. The row will be empty
   --  after this function is called. To fill in values, you need to call
   --  Set_Value.

   procedure Clear (List_Store : access Gtk_List_Store_Record);
   --  Remove all the rows in List_Store.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gtk_List_Store_Record is
     new Gtk.Tree_Model.Gtk_Tree_Model_Record with null record;

   pragma Import (C, Get_Type, "gtk_list_store_get_type");
end Gtk.List_Store;
