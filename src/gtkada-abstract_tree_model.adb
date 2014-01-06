------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Glib.Object;     use Glib.Object;
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtk.Tree_Model;  use Gtk.Tree_Model;
with System;

package body Gtkada.Abstract_Tree_Model is

   use type Gtk.Tree_Model.Gtk_Tree_Iter;

   type Gtk_Tree_Iter_Access is access all Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Convention (C, Gtk_Tree_Iter_Access);

   type GTypeInterface is record
      g_type          : Glib.GType;
      g_instance_type : Glib.GType;
   end record;
   pragma Convention (C, GTypeInterface);

   type GtkTreeModelInterface is record
      g_type                : GTypeInterface;
      row_changed           : System.Address := System.Null_Address;
      row_inserted          : System.Address := System.Null_Address;
      row_has_child_toggled : System.Address := System.Null_Address;
      row_deleted           : System.Address := System.Null_Address;
      rows_reordered        : System.Address := System.Null_Address;
      get_flags             : System.Address := System.Null_Address;
      get_n_columns         : System.Address := System.Null_Address;
      get_column_type       : System.Address := System.Null_Address;
      get_iter              : System.Address := System.Null_Address;
      get_path              : System.Address := System.Null_Address;
      get_value             : System.Address := System.Null_Address;
      iter_next             : System.Address := System.Null_Address;
      iter_previous         : System.Address := System.Null_Address;
      iter_children         : System.Address := System.Null_Address;
      iter_has_child        : System.Address := System.Null_Address;
      iter_n_children       : System.Address := System.Null_Address;
      iter_nth_child        : System.Address := System.Null_Address;
      iter_parent           : System.Address := System.Null_Address;
      ref_node              : System.Address := System.Null_Address;
      unref_node            : System.Address := System.Null_Address;
   end record;
   type GtkTreeModelInterface_Access is access GtkTreeModelInterface;
   pragma Convention (C, GtkTreeModelInterface);

   function Convert is new Ada.Unchecked_Conversion
      (System.Address, GtkTreeModelInterface_Access);

   procedure Tree_Model_Interface_Init (Iface, Data : System.Address);
   pragma Convention (C, Tree_Model_Interface_Init);

   function Dispatch_Get_Flags
     (Tree_Model : Gtk_Tree_Model) return Gtk.Tree_Model.Tree_Model_Flags;
   pragma Convention (C, Dispatch_Get_Flags);

   function Dispatch_Get_N_Columns
     (Tree_Model : Gtk_Tree_Model) return Glib.Gint;
   pragma Convention (C, Dispatch_Get_N_Columns);

   function Dispatch_Get_Column_Type
     (Tree_Model : Gtk_Tree_Model;
      Index      : Glib.Gint) return Glib.GType;
   pragma Convention (C, Dispatch_Get_Column_Type);

   function Dispatch_Get_Iter
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter;
      Path       : System.Address) return Glib.Gboolean;
   pragma Convention (C, Dispatch_Get_Iter);

   function Dispatch_Get_Path
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter)
      return System.Address;
   pragma Convention (C, Dispatch_Get_Path);

   procedure Dispatch_Get_Value
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Glib.Gint;
      Value      : out Glib.Values.GValue);
   pragma Convention (C, Dispatch_Get_Value);

   function Dispatch_Iter_Next
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gboolean;
   pragma Convention (C, Dispatch_Iter_Next);

   function Dispatch_Iter_Children
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter_Access)
      return Glib.Gboolean;
   pragma Convention (C, Dispatch_Iter_Children);

   function Dispatch_Iter_Has_Child
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gboolean;
   pragma Convention (C, Dispatch_Iter_Has_Child);

   function Dispatch_Iter_N_Children
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter_Access)
      return Glib.Gint;
   pragma Convention (C, Dispatch_Iter_N_Children);

   function Dispatch_Iter_Nth_Child
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter_Access;
      N          : Glib.Gint) return Glib.Gboolean;
   pragma Convention (C, Dispatch_Iter_Nth_Child);

   function Dispatch_Iter_Parent
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter;
      Child      : not null access Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gboolean;
   pragma Convention (C, Dispatch_Iter_Parent);

   procedure Dispatch_Ref_Node
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter_Access);
   pragma Convention (C, Dispatch_Ref_Node);

   procedure Dispatch_Unref_Node
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter_Access);
   pragma Convention (C, Dispatch_Unref_Node);

   Class_Record : aliased Glib.Object.Ada_GObject_Class :=
      Glib.Object.Uninitialized_Class;

   ------------------------------
   -- Dispatch_Get_Column_Type --
   ------------------------------

   function Dispatch_Get_Column_Type
     (Tree_Model : Gtk_Tree_Model;
      Index      : Glib.Gint)
      return Glib.GType
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
   begin
      return T.Get_Column_Type (Index);
   exception
      when E : others =>
         Process_Exception (E);
         return Glib.GType_None;
   end Dispatch_Get_Column_Type;

   function Get_Column_Type
      (Self  : access Gtk_Abstract_Tree_Model_Record;
       Index : Glib.Gint) return Glib.GType is
   begin
      --  The default implementation is to call gtk+ default function
      return Gtk.Tree_Model.Get_Column_Type (+Self, Index);
   end Get_Column_Type;

   ------------------------
   -- Dispatch_Get_Flags --
   ------------------------

   function Dispatch_Get_Flags
     (Tree_Model : Gtk_Tree_Model)
      return Gtk.Tree_Model.Tree_Model_Flags
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
   begin
      return T.Get_Flags;
   exception
      when E : others =>
         Process_Exception (E);
         return 0;
   end Dispatch_Get_Flags;

   function Get_Flags
      (Self : access Gtk_Abstract_Tree_Model_Record) return Tree_Model_Flags
   is
      pragma Unreferenced (Self);
   begin
      --  We cannot call gtk+'s function here, or that will result in a
      --  Storage_Error if the user hasn't overridden this function.
      return 0;
   end Get_Flags;

   -----------------------
   -- Dispatch_Get_Iter --
   -----------------------

   function Dispatch_Get_Iter
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter;
      Path       : System.Address) return Glib.Gboolean
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
      P : constant Gtk_Tree_Path := From_Object (Path);
   begin
      Iter.all := T.Get_Iter (P);
      if Iter.all = Gtk.Tree_Model.Null_Iter then
         return 0;
      else
         return 1;
      end if;
   exception
      when E : others =>
         Process_Exception (E);
         return 0;
   end Dispatch_Get_Iter;

   function Get_Iter
      (Self : access Gtk_Abstract_Tree_Model_Record;
       Path : Gtk_Tree_Path) return Gtk_Tree_Iter
   is
   begin
      return Gtk.Tree_Model.Get_Iter (+Self, Path);
   end Get_Iter;

   -----------------------
   -- Dispatch_Get_Path --
   -----------------------

   function Dispatch_Get_Path
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter)
      return System.Address
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
   begin
      return T.Get_Path (Iter.all).Get_Object;
   exception
      when E : others =>
         Process_Exception (E);
         return System.Null_Address;
   end Dispatch_Get_Path;

   function Get_Path
      (Self : access Gtk_Abstract_Tree_Model_Record;
       Iter : Gtk_Tree_Iter) return Gtk_Tree_Path
   is
   begin
      return Gtk.Tree_Model.Get_Path (+Self, Iter);
   end Get_Path;

   ----------------------------
   -- Dispatch_Get_N_Columns --
   ----------------------------

   function Dispatch_Get_N_Columns
     (Tree_Model : Gtk_Tree_Model)
      return Glib.Gint
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
   begin
      return T.Get_N_Columns;
   exception
      when E : others =>
         Process_Exception (E);
         return 0;
   end Dispatch_Get_N_Columns;

   function Get_N_Columns
      (Self : access Gtk_Abstract_Tree_Model_Record) return Glib.Gint
   is
   begin
      return Gtk.Tree_Model.Get_N_Columns (+Self);
   end Get_N_Columns;

   ------------------------
   -- Dispatch_Get_Value --
   ------------------------

   procedure Dispatch_Get_Value
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Glib.Gint;
      Value      : out Glib.Values.GValue)
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
   begin
      if Iter.all = Null_Iter then
         raise Program_Error with "passing a null_iter to Get_Value";
      end if;
      T.Get_Value (Iter.all, Column, Value);
   exception
      when E : others =>
         Process_Exception (E);
         Glib.Values.Unset (Value);
   end Dispatch_Get_Value;

   procedure Get_Value
      (Self   : access Gtk_Abstract_Tree_Model_Record;
       Iter   : Gtk_Tree_Iter;
       Column : Glib.Gint;
       Value  : out Glib.Values.GValue)
   is
   begin
      Gtk.Tree_Model.Get_Value (+Self, Iter, Column, Value);
   end Get_Value;

   ----------------------------
   -- Dispatch_Iter_Children --
   ----------------------------

   function Dispatch_Iter_Children
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter_Access)
      return Glib.Gboolean
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
      Real_Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      --  Gtk should normally never pass null to that function. Unfortunately,
      --  When using e.g. PyGtk, Parent may be null, probably due to a bug.
      --  Using a null parent and passing Null_Iter in this case is a
      --  workaround to that problem.
      if Parent /= null then
         Real_Parent := Parent.all;
      else
         Real_Parent := Gtk.Tree_Model.Null_Iter;
      end if;

      Iter.all := T.Children (Real_Parent);
      if Iter.all = Gtk.Tree_Model.Null_Iter then
         return 0;
      else
         return 1;
      end if;
   exception
      when E : others =>
         Process_Exception (E);
         return 0;
   end Dispatch_Iter_Children;

   function Children
      (Self : access Gtk_Abstract_Tree_Model_Record;
       Parent : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
   begin
      return Gtk.Tree_Model.Children (+Self, Parent);
   end Children;

   -----------------------------
   -- Dispatch_Iter_Has_Child --
   -----------------------------

   function Dispatch_Iter_Has_Child
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gboolean
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
   begin
      if T.Has_Child (Iter.all) then
         return 1;
      else
         return 0;
      end if;
   exception
      when E : others =>
         Process_Exception (E);
         return 0;
   end Dispatch_Iter_Has_Child;

   function Has_Child
      (Self : access Gtk_Abstract_Tree_Model_Record;
       Iter : Gtk_Tree_Iter) return Boolean
   is
   begin
      return Gtk.Tree_Model.Has_Child (+Self, Iter);
   end Has_Child;

   ------------------------------
   -- Dispatch_Iter_N_Children --
   ------------------------------

   function Dispatch_Iter_N_Children
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter_Access)
      return Glib.Gint
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
   begin
      if Iter = null then
         return T.N_Children (Null_Iter);
      else
         return T.N_Children (Iter.all);
      end if;
   exception
      when E : others =>
         Process_Exception (E);
         return 0;
   end Dispatch_Iter_N_Children;

   function N_Children
      (Self : access Gtk_Abstract_Tree_Model_Record;
       Iter : Gtk_Tree_Iter := Null_Iter) return Glib.Gint
   is
   begin
      return Gtk.Tree_Model.N_Children (+Self, Iter);
   end N_Children;

   ------------------------
   -- Dispatch_Iter_Next --
   ------------------------

   function Dispatch_Iter_Next
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gboolean
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
   begin
      T.Next (Iter.all);
      if Iter.all = Gtk.Tree_Model.Null_Iter then
         return 0;
      else
         return 1;
      end if;
   exception
      when E : others =>
         Process_Exception (E);
         return 0;
   end Dispatch_Iter_Next;

   procedure Next
      (Self : access Gtk_Abstract_Tree_Model_Record;
       Iter : in out Gtk_Tree_Iter)
   is
   begin
      Gtk.Tree_Model.Next (+Self, Iter);
   end Next;

   -----------------------------
   -- Dispatch_Iter_Nth_Child --
   -----------------------------

   function Dispatch_Iter_Nth_Child
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter_Access;
      N          : Glib.Gint) return Glib.Gboolean
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
   begin
      --  Parent can be null, GtkAda assumes it can't.

      if Parent = null then
         Iter.all := T.Nth_Child (Null_Iter, N);
      else
         Iter.all := T.Nth_Child (Parent.all, N);
      end if;

      if Iter.all = Gtk.Tree_Model.Null_Iter then
         return 0;
      else
         return 1;
      end if;
   exception
      when E : others =>
         Process_Exception (E);
         return 0;
   end Dispatch_Iter_Nth_Child;

   function Nth_Child
      (Self   : access Gtk_Abstract_Tree_Model_Record;
       Parent : Gtk_Tree_Iter;
       N      : Glib.Gint) return Gtk_Tree_Iter is
   begin
      return Gtk.Tree_Model.Nth_Child (+Self, Parent, N);
   end Nth_Child;

   --------------------------
   -- Dispatch_Iter_Parent --
   --------------------------

   function Dispatch_Iter_Parent
     (Tree_Model : Gtk_Tree_Model;
      Iter       : not null access Gtk.Tree_Model.Gtk_Tree_Iter;
      Child      : not null access Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gboolean
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
   begin
      Iter.all := T.Parent (Child.all);
      if Iter.all = Gtk.Tree_Model.Null_Iter then
         return 0;
      else
         return 1;
      end if;
   exception
      when E : others =>
         Process_Exception (E);
         return 0;
   end Dispatch_Iter_Parent;

   function Parent
      (Self  : access Gtk_Abstract_Tree_Model_Record;
       Child : Gtk_Tree_Iter) return Gtk_Tree_Iter is
   begin
      return Gtk.Tree_Model.Parent (+Self, Child);
   end Parent;

   -----------------------
   -- Dispatch_Ref_Node --
   -----------------------

   procedure Dispatch_Ref_Node
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter_Access)
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
   begin
      if Iter /= null then
         T.Ref_Node (Iter.all);
      end if;
   exception
      when E : others =>
         Process_Exception (E);
   end Dispatch_Ref_Node;

   procedure Ref_Node
      (Self : access Gtk_Abstract_Tree_Model_Record;
       Iter : Gtk_Tree_Iter)
   is
      pragma Unreferenced (Self, Iter);
   begin
      --  We cannot call gtk+'s function here, or that will result in a
      --  Storage_Error if the user hasn't overridden this function.
      null;
      --  Gtk.Tree_Model.Ref_Node (+Self, Iter);
   end Ref_Node;

   -------------------------
   -- Dispatch_Unref_Node --
   -------------------------

   procedure Dispatch_Unref_Node
     (Tree_Model : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter_Access)
   is
      T : constant Gtk_Abstract_Tree_Model := -Tree_Model;
   begin
      if Iter /= null then
         T.Unref_Node (Iter.all);
      end if;
   exception
      when E : others =>
         Process_Exception (E);
   end Dispatch_Unref_Node;

   procedure Unref_Node
      (Self : access Gtk_Abstract_Tree_Model_Record;
       Iter : Gtk_Tree_Iter)
   is
      pragma Unreferenced (Self, Iter);
   begin
      --  We cannot call gtk+'s function here, or that will result in a
      --  Storage_Error if the user hasn't overridden this function.
      null;
      --  Gtk.Tree_Model.Unref_Node (+Self, Iter);
   end Unref_Node;

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType is
   begin
      if Glib.Object.Initialize_Class_Record
        (Ancestor     => Glib.GType_Object,
         Class_Record => Class_Record'Access,
         Type_Name    => "GtkAdaAbstractTreeModel")
      then
         Add_Interface
           (Class_Record,
            Gtk.Tree_Model.Get_Type,
            new GInterface_Info'
               (Interface_Init => Tree_Model_Interface_Init'Access,
                Interface_Finalize => null,
                Interface_Data     => System.Null_Address));
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Gtk_Abstract_Tree_Model_Record'Class) is
   begin
      G_New (Self, Get_Type);
   end Initialize;

   -------------------------------
   -- Tree_Model_Interface_Init --
   -------------------------------

   procedure Tree_Model_Interface_Init (Iface, Data : System.Address) is
      pragma Unreferenced (Data);
      F : constant GtkTreeModelInterface_Access := Convert (Iface);
   begin
      F.get_flags       := Dispatch_Get_Flags'Address;
      F.get_n_columns   := Dispatch_Get_N_Columns'Address;
      F.get_column_type := Dispatch_Get_Column_Type'Address;
      F.get_iter        := Dispatch_Get_Iter'Address;
      F.get_path        := Dispatch_Get_Path'Address;
      F.get_value       := Dispatch_Get_Value'Address;
      F.iter_next       := Dispatch_Iter_Next'Address;
      F.iter_children   := Dispatch_Iter_Children'Address;
      F.iter_has_child  := Dispatch_Iter_Has_Child'Address;
      F.iter_n_children := Dispatch_Iter_N_Children'Address;
      F.iter_nth_child  := Dispatch_Iter_Nth_Child'Address;
      F.iter_parent     := Dispatch_Iter_Parent'Address;
      F.ref_node        := Dispatch_Ref_Node'Address;
      F.unref_node      := Dispatch_Unref_Node'Address;
   end Tree_Model_Interface_Init;

end Gtkada.Abstract_Tree_Model;
