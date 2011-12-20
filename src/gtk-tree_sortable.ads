------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--  Gtk_Tree_Sortable is an interface to be implemented by tree models which
--  support sorting. The Gtk_Tree_View uses the methods provided by this
--  interface to sort the model.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>

with Glib.Types;
with Gtk.Enums;
with Gtk.Tree_Model;

package Gtk.Tree_Sortable is
   type Gtk_Tree_Sortable is new Glib.Types.GType_Interface;

   Default_Sort_Column_Id  : constant Gint := -1;
   Unsorted_Sort_Column_Id : constant Gint := -2;
   --  Two special values for the sort column

   function Get_Type return Glib.GType;
   --  Returns the internal type used for a Gtk_Tree_Sortable

   procedure Set_Sort_Column_Id
     (Sortable       : Gtk_Tree_Sortable;
      Sort_Column_Id : Gint;
      Order          : Gtk.Enums.Gtk_Sort_Type);
   procedure Get_Sort_Column_Id
     (Sortable       : Gtk_Tree_Sortable;
      Sort_Column_Id : out Gint;
      Order          : out Gtk.Enums.Gtk_Sort_Type);
   --  Sets the current sort column to be Sort_Column_Id. The Sortable will
   --  resort itself to reflect this change, after emitting sort_column_changed
   --  signal. If Sort_Column_Id is Default_Sort_Column_Id, then the default
   --  sort function will be used, if it is set.

   type Gtk_Tree_Iter_Compare_Func is access function
     (Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
   --  A Gtk_Tree_Iter_Compare_Func should return a negative integer, zero, or
   --  a positive integer if a sorts before b, a sorts with b, or a sorts after
   --  b respectively. If two iters compare as equal, their order in the sorted
   --  model is undefined. In order to ensure that the Gtk_Tree_Sortable
   --  behaves as expected, the Gtk_Tree_Iter_Compare_Func must define a
   --  partial order on the model, i.e. it must be reflexive, antisymmetric and
   --  transitive.
   --  For example, if model is a product catalogue, then a compare function
   --  for the "price" column could be one which returns price_of(a) -
   --  price_of(b).

   procedure Set_Default_Sort_Func
     (Sortable  : Gtk_Tree_Sortable;
      Sort_Func : Gtk_Tree_Iter_Compare_Func);
   function Has_Default_Sort_Func
     (Sortable : Gtk_Tree_Sortable) return Boolean;
   --  Sets the default comparison function used when sorting to be Sort_Func.
   --  If the current sort column id of Sortable is Default_Sort_Column_Id,
   --  then the model will sort using this function.
   --  If Sort_Func is null, then there will be no default comparison function.
   --  This means that once the model has been sorted, it can't go back to the
   --  default state. In this case, when the current sort column id of Sortable
   --  is Default_Sort_Column_Id, the model will be unsorted.

   procedure Set_Sort_Func
     (Sortable       : Gtk_Tree_Sortable;
      Sort_Column_Id : Gint;
      Sort_Func      : Gtk_Tree_Iter_Compare_Func);
   --  Sets the comparison function used when sorting to be Sort_Func. If the
   --  current sort column id of Sortable is the same as Sort_Column_Id, then
   --  the model will sort using this function.

   generic
      type Data_Type (<>) is private;
   package Compare_Funcs is
      type Gtk_Tree_Iter_Compare_Func is access function
        (Model     : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
         A         : Gtk.Tree_Model.Gtk_Tree_Iter;
         B         : Gtk.Tree_Model.Gtk_Tree_Iter;
         User_Data : Data_Type) return Gint;

      type Destroy_Notify is access procedure (Data : in out Data_Type);
      --  Free the memory used by Data

      procedure Set_Default_Sort_Func
        (Sortable  : Gtk_Tree_Sortable;
         Sort_Func : Gtk_Tree_Iter_Compare_Func;
         User_Data : Data_Type;
         Destroy   : Destroy_Notify := null);
      procedure Set_Sort_Func
        (Sortable       : Gtk_Tree_Sortable;
         Sort_Column_Id : Gint;
         Sort_Func      : Gtk_Tree_Iter_Compare_Func;
         User_Data      : Data_Type;
         Destroy        : Destroy_Notify := null);
      --  Same as above, but an additional user data can be passed to the sort
      --  function.
   private
      --  <doc_ignore>
      type Data_Type_Access is access Data_Type;
      type Data_Type_Record is record
         Func    : Gtk_Tree_Iter_Compare_Func;
         Destroy : Destroy_Notify;
         Data    : Data_Type_Access;
      end record;
      type Data_Type_Record_Access is access Data_Type_Record;
      pragma Convention (C, Data_Type_Record_Access);

      procedure Internal_Destroy_Notify (Data : Data_Type_Record_Access);
      pragma Convention (C, Internal_Destroy_Notify);

      function Internal_Compare_Func
        (Model : System.Address;
         A, B  : System.Address;
         Data  : Data_Type_Record_Access) return Gint;
      pragma Convention (C, Internal_Compare_Func);
      --  </doc_ignore>
   end Compare_Funcs;
   --  </doc_ignore>

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:

   --  <signals>
   --  - "sort_column_changed"
   --    procedure Handler (Sortable : Gtk_Tree_Sortable);
   --    Emitted when the sort column is changed through Set_Sort_Column_Id
   --  </signals>

   Signal_Sort_Column_Changed : constant Glib.Signal_Name :=
                                  "sort_column_changed";

   procedure Sort_Column_Changed (Sortable : Gtk_Tree_Sortable);
   --  Emits sort_column_changed signal

private
   pragma Import (C, Get_Type, "gtk_tree_sortable_get_type");
   pragma Import
     (C, Set_Sort_Column_Id, "gtk_tree_sortable_set_sort_column_id");
   pragma Import
     (C, Sort_Column_Changed, "gtk_tree_sortable_sort_column_changed");
end Gtk.Tree_Sortable;
