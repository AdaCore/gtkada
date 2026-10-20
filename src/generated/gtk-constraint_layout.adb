------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Types;               use Gtkada.Types;
with Interfaces.C;

package body Gtk.Constraint_Layout is

   function Add_Constraints_From_Description
     (Layout : not null access Gtk_Constraint_Layout_Record;
      Lines : GNAT.Strings.String_List;
      Views : Named_View_Array;
      Error : out Glib.Error.GError;
      Hspacing : Glib.Gint := -1;
      Vspacing : Glib.Gint := -1)
   return Glib.Object.Object_Simple_List.Glist
   is
      use Gtkada.Types;
      use type System.Address;
      use type GNAT.Strings.String_Access;
      type Hash_Func is access function (Key : Chars_Ptr) return Glib.Guint;
      pragma Convention (C, Hash_Func);
      type Equal_Func is access function (A, B : Chars_Ptr) return Glib.Gboolean;
      pragma Convention (C, Equal_Func);
      function Str_Hash (Key : Chars_Ptr) return Glib.Guint;
      pragma Import (C, Str_Hash, "g_str_hash");
      function Str_Equal (A, B : Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Str_Equal, "g_str_equal");
      function New_Table (Hash : Hash_Func; Equal : Equal_Func) return System.Address;
      pragma Import (C, New_Table, "g_hash_table_new");
      function Insert
        (Table : System.Address; Key : Chars_Ptr; Value : System.Address)
      return Glib.Gboolean;
      pragma Import (C, Insert, "g_hash_table_insert");
      procedure Unref (Table : System.Address);
      pragma Import (C, Unref, "g_hash_table_unref");
      function Internal
        (Layout : System.Address;
         Lines : System.Address;
         N_Lines : Gsize;
         Hspacing, Vspacing : Glib.Gint;
         Views : System.Address;
         Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal,
         "gtk_constraint_layout_add_constraints_from_descriptionv");
      C_Lines : Chars_Ptr_Array (1 .. Interfaces.C.size_t (Lines'Length)) :=
        (others => Null_Ptr);
      C_Names : Chars_Ptr_Array (1 .. Interfaces.C.size_t (Views'Length)) :=
        (others => Null_Ptr);
      Table : System.Address := New_Table (Str_Hash'Access, Str_Equal'Access);
      C_Error : aliased Glib.Error.GError := null;
      Result : Glib.Object.Object_Simple_List.Glist;
      Ignored : Glib.Gboolean;
   begin
      for I in Lines'Range loop
         C_Lines (Interfaces.C.size_t (I - Lines'First + 1)) := New_String
           ((if Lines (I) = null then "" else Lines (I).all));
      end loop;
      for I in Views'Range loop
         C_Names (Interfaces.C.size_t (I - Views'First + 1)) := New_String
           (Ada.Strings.Unbounded.To_String (Views (I).Name));
         Ignored := Insert
           (Table, C_Names (Interfaces.C.size_t (I - Views'First + 1)),
            System.Address (Views (I).Target));
      end loop;
      Glib.Object.Object_Simple_List.Set_Object
        (Result, Internal (Get_Object (Layout), C_Lines'Address,
            Gsize (Lines'Length), Hspacing, Vspacing, Table, C_Error'Access));
      Error := C_Error;
      Unref (Table);
      Table := System.Null_Address;
      Free (C_Names);
      Free (C_Lines);
      return Result;
   exception
      when others =>
         if Table /= System.Null_Address then
            Unref (Table);
         end if;
         Free (C_Names);
         Free (C_Lines);
         raise;
      end Add_Constraints_From_Description;

   package Type_Conversion_Gtk_Constraint_Layout is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Constraint_Layout_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Constraint_Layout);

   package Type_Conversion_Gtk_Constraint_Layout_Child is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Constraint_Layout_Child_Get_Type'Access, Gtk_Constraint_Layout_Child_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Constraint_Layout_Child);

   -------------------------------
   -- Gtk_Constraint_Layout_New --
   -------------------------------

   function Gtk_Constraint_Layout_New return Gtk_Constraint_Layout is
      Layout : constant Gtk_Constraint_Layout := new Gtk_Constraint_Layout_Record;
   begin
      Gtk.Constraint_Layout.Initialize (Layout);
      return Layout;
   end Gtk_Constraint_Layout_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Layout : out Gtk_Constraint_Layout) is
   begin
      Layout := new Gtk_Constraint_Layout_Record;
      Gtk.Constraint_Layout.Initialize (Layout);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Layout : not null access Gtk_Constraint_Layout_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_constraint_layout_new");
   begin
      if not Layout.Is_Created then
         Set_Object (Layout, Internal);
      end if;
   end Initialize;

   --------------------
   -- Add_Constraint --
   --------------------

   procedure Add_Constraint
      (Layout     : not null access Gtk_Constraint_Layout_Record;
       Constraint : not null access Gtk.Constraint.Gtk_Constraint_Record'Class)
   is
      procedure Internal
         (Layout     : System.Address;
          Constraint : System.Address);
      pragma Import (C, Internal, "gtk_constraint_layout_add_constraint");
   begin
      Internal (Get_Object (Layout), Get_Object (Constraint));
   end Add_Constraint;

   ---------------
   -- Add_Guide --
   ---------------

   procedure Add_Guide
      (Layout : not null access Gtk_Constraint_Layout_Record;
       Guide  : not null access Gtk.Constraint_Guide.Gtk_Constraint_Guide_Record'Class)
   is
      procedure Internal (Layout : System.Address; Guide : System.Address);
      pragma Import (C, Internal, "gtk_constraint_layout_add_guide");
   begin
      Internal (Get_Object (Layout), Get_Object (Guide));
   end Add_Guide;

   -------------------------
   -- Observe_Constraints --
   -------------------------

   function Observe_Constraints
      (Layout : not null access Gtk_Constraint_Layout_Record)
       return Glib.List_Model.Glist_Model
   is
      function Internal
         (Layout : System.Address) return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_constraint_layout_observe_constraints");
   begin
      return Internal (Get_Object (Layout));
   end Observe_Constraints;

   --------------------
   -- Observe_Guides --
   --------------------

   function Observe_Guides
      (Layout : not null access Gtk_Constraint_Layout_Record)
       return Glib.List_Model.Glist_Model
   is
      function Internal
         (Layout : System.Address) return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_constraint_layout_observe_guides");
   begin
      return Internal (Get_Object (Layout));
   end Observe_Guides;

   ----------------------------
   -- Remove_All_Constraints --
   ----------------------------

   procedure Remove_All_Constraints
      (Layout : not null access Gtk_Constraint_Layout_Record)
   is
      procedure Internal (Layout : System.Address);
      pragma Import (C, Internal, "gtk_constraint_layout_remove_all_constraints");
   begin
      Internal (Get_Object (Layout));
   end Remove_All_Constraints;

   -----------------------
   -- Remove_Constraint --
   -----------------------

   procedure Remove_Constraint
      (Layout     : not null access Gtk_Constraint_Layout_Record;
       Constraint : not null access Gtk.Constraint.Gtk_Constraint_Record'Class)
   is
      procedure Internal
         (Layout     : System.Address;
          Constraint : System.Address);
      pragma Import (C, Internal, "gtk_constraint_layout_remove_constraint");
   begin
      Internal (Get_Object (Layout), Get_Object (Constraint));
   end Remove_Constraint;

   ------------------
   -- Remove_Guide --
   ------------------

   procedure Remove_Guide
      (Layout : not null access Gtk_Constraint_Layout_Record;
       Guide  : not null access Gtk.Constraint_Guide.Gtk_Constraint_Guide_Record'Class)
   is
      procedure Internal (Layout : System.Address; Guide : System.Address);
      pragma Import (C, Internal, "gtk_constraint_layout_remove_guide");
   begin
      Internal (Get_Object (Layout), Get_Object (Guide));
   end Remove_Guide;

end Gtk.Constraint_Layout;
