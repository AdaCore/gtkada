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
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Builder_List_Item_Factory is

   package Type_Conversion_Gtk_Builder_List_Item_Factory is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Builder_List_Item_Factory_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Builder_List_Item_Factory);

   --------------------------------------------------
   -- Gtk_Builder_List_Item_Factory_New_From_Bytes --
   --------------------------------------------------

   function Gtk_Builder_List_Item_Factory_New_From_Bytes
      (Scope : Gtk.Builder_Scope.Gtk_Builder_Scope;
       Bytes : Glib.Bytes.Gbytes) return Gtk_Builder_List_Item_Factory
   is
      Self : constant Gtk_Builder_List_Item_Factory := new Gtk_Builder_List_Item_Factory_Record;
   begin
      Gtk.Builder_List_Item_Factory.Initialize_From_Bytes (Self, Scope, Bytes);
      return Self;
   end Gtk_Builder_List_Item_Factory_New_From_Bytes;

   -----------------------------------------------------
   -- Gtk_Builder_List_Item_Factory_New_From_Resource --
   -----------------------------------------------------

   function Gtk_Builder_List_Item_Factory_New_From_Resource
      (Scope         : Gtk.Builder_Scope.Gtk_Builder_Scope;
       Resource_Path : UTF8_String) return Gtk_Builder_List_Item_Factory
   is
      Self : constant Gtk_Builder_List_Item_Factory := new Gtk_Builder_List_Item_Factory_Record;
   begin
      Gtk.Builder_List_Item_Factory.Initialize_From_Resource (Self, Scope, Resource_Path);
      return Self;
   end Gtk_Builder_List_Item_Factory_New_From_Resource;

   ------------------------
   -- Gtk_New_From_Bytes --
   ------------------------

   procedure Gtk_New_From_Bytes
      (Self  : out Gtk_Builder_List_Item_Factory;
       Scope : Gtk.Builder_Scope.Gtk_Builder_Scope;
       Bytes : Glib.Bytes.Gbytes)
   is
   begin
      Self := new Gtk_Builder_List_Item_Factory_Record;
      Gtk.Builder_List_Item_Factory.Initialize_From_Bytes (Self, Scope, Bytes);
   end Gtk_New_From_Bytes;

   ---------------------------
   -- Gtk_New_From_Resource --
   ---------------------------

   procedure Gtk_New_From_Resource
      (Self          : out Gtk_Builder_List_Item_Factory;
       Scope         : Gtk.Builder_Scope.Gtk_Builder_Scope;
       Resource_Path : UTF8_String)
   is
   begin
      Self := new Gtk_Builder_List_Item_Factory_Record;
      Gtk.Builder_List_Item_Factory.Initialize_From_Resource (Self, Scope, Resource_Path);
   end Gtk_New_From_Resource;

   ---------------------------
   -- Initialize_From_Bytes --
   ---------------------------

   procedure Initialize_From_Bytes
      (Self  : not null access Gtk_Builder_List_Item_Factory_Record'Class;
       Scope : Gtk.Builder_Scope.Gtk_Builder_Scope;
       Bytes : Glib.Bytes.Gbytes)
   is
      function Internal
         (Scope : Gtk.Builder_Scope.Gtk_Builder_Scope;
          Bytes : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_builder_list_item_factory_new_from_bytes");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Scope, Get_Object (Bytes)));
      end if;
   end Initialize_From_Bytes;

   ------------------------------
   -- Initialize_From_Resource --
   ------------------------------

   procedure Initialize_From_Resource
      (Self          : not null access Gtk_Builder_List_Item_Factory_Record'Class;
       Scope         : Gtk.Builder_Scope.Gtk_Builder_Scope;
       Resource_Path : UTF8_String)
   is
      function Internal
         (Scope         : Gtk.Builder_Scope.Gtk_Builder_Scope;
          Resource_Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_builder_list_item_factory_new_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Return        : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Scope, Tmp_Resource_Path);
         Free (Tmp_Resource_Path);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_From_Resource;

   ---------------
   -- Get_Bytes --
   ---------------

   function Get_Bytes
      (Self : not null access Gtk_Builder_List_Item_Factory_Record)
       return Glib.Bytes.Gbytes
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_builder_list_item_factory_get_bytes");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Bytes;

   ------------------
   -- Get_Resource --
   ------------------

   function Get_Resource
      (Self : not null access Gtk_Builder_List_Item_Factory_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_builder_list_item_factory_get_resource");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Resource;

   ---------------
   -- Get_Scope --
   ---------------

   function Get_Scope
      (Self : not null access Gtk_Builder_List_Item_Factory_Record)
       return Gtk.Builder_Scope.Gtk_Builder_Scope
   is
      function Internal
         (Self : System.Address) return Gtk.Builder_Scope.Gtk_Builder_Scope;
      pragma Import (C, Internal, "gtk_builder_list_item_factory_get_scope");
   begin
      return Internal (Get_Object (Self));
   end Get_Scope;

end Gtk.Builder_List_Item_Factory;
