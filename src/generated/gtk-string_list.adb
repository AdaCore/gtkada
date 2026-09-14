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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.String_List is

   package Type_Conversion_Gtk_String_List is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_String_List_Record);
   pragma Unreferenced (Type_Conversion_Gtk_String_List);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self    : out Gtk_String_List;
       Strings : GNAT.Strings.String_List)
   is
   begin
      Self := new Gtk_String_List_Record;
      Gtk.String_List.Initialize (Self, Strings);
   end Gtk_New;

   -------------------------
   -- Gtk_String_List_New --
   -------------------------

   function Gtk_String_List_New
      (Strings : GNAT.Strings.String_List) return Gtk_String_List
   is
      Self : constant Gtk_String_List := new Gtk_String_List_Record;
   begin
      Gtk.String_List.Initialize (Self, Strings);
      return Self;
   end Gtk_String_List_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self    : not null access Gtk_String_List_Record'Class;
       Strings : GNAT.Strings.String_List)
   is
      function Internal
         (Strings : Gtkada.Types.chars_ptr_array) return System.Address;
      pragma Import (C, Internal, "gtk_string_list_new");
      Tmp_Strings : Gtkada.Types.chars_ptr_array := From_String_List (Strings);
      Tmp_Return  : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Strings);
         Set_Object (Self, Tmp_Return);
      end if;
      Gtkada.Types.Free (Tmp_Strings);
   end Initialize;

   ------------
   -- Append --
   ------------

   procedure Append
      (Self   : not null access Gtk_String_List_Record;
       String : UTF8_String)
   is
      procedure Internal
         (Self   : System.Address;
          String : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_string_list_append");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
   begin
      Internal (Get_Object (Self), Tmp_String);
      Free (Tmp_String);
   end Append;

   ----------
   -- Find --
   ----------

   function Find
      (Self   : not null access Gtk_String_List_Record;
       String : UTF8_String) return Guint
   is
      function Internal
         (Self   : System.Address;
          String : Gtkada.Types.Chars_Ptr) return Guint;
      pragma Import (C, Internal, "gtk_string_list_find");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_String);
      Free (Tmp_String);
      return Tmp_Return;
   end Find;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
      (Self     : not null access Gtk_String_List_Record;
       Position : Guint) return UTF8_String
   is
      function Internal
         (Self     : System.Address;
          Position : Guint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_string_list_get_string");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self), Position));
   end Get_String;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Self     : not null access Gtk_String_List_Record;
       Position : Guint)
   is
      procedure Internal (Self : System.Address; Position : Guint);
      pragma Import (C, Internal, "gtk_string_list_remove");
   begin
      Internal (Get_Object (Self), Position);
   end Remove;

   ------------
   -- Splice --
   ------------

   procedure Splice
      (Self       : not null access Gtk_String_List_Record;
       Position   : Guint;
       N_Removals : Guint;
       Additions  : GNAT.Strings.String_List)
   is
      procedure Internal
         (Self       : System.Address;
          Position   : Guint;
          N_Removals : Guint;
          Additions  : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_string_list_splice");
      Tmp_Additions : Gtkada.Types.chars_ptr_array := From_String_List (Additions);
   begin
      Internal (Get_Object (Self), Position, N_Removals, Tmp_Additions);
      Gtkada.Types.Free (Tmp_Additions);
   end Splice;

   ----------
   -- Take --
   ----------

   procedure Take
      (Self   : not null access Gtk_String_List_Record;
       String : UTF8_String)
   is
      procedure Internal
         (Self   : System.Address;
          String : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_string_list_take");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
   begin
      Internal (Get_Object (Self), Tmp_String);
   end Take;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self     : not null access Gtk_String_List_Record;
       Position : Guint) return Glib.Object.GObject
   is
      function Internal
         (Self     : System.Address;
          Position : Guint) return System.Address;
      pragma Import (C, Internal, "g_list_model_get_object");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self), Position), Stub_GObject);
   end Get_Item;

   -------------------
   -- Get_Item_Type --
   -------------------

   function Get_Item_Type
      (Self : not null access Gtk_String_List_Record) return GType
   is
      function Internal (Self : System.Address) return GType;
      pragma Import (C, Internal, "g_list_model_get_item_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Item_Type;

   -----------------
   -- Get_N_Items --
   -----------------

   function Get_N_Items
      (Self : not null access Gtk_String_List_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "g_list_model_get_n_items");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Items;

   -------------------
   -- Items_Changed --
   -------------------

   procedure Items_Changed
      (Self     : not null access Gtk_String_List_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Guint;
          Removed  : Guint;
          Added    : Guint);
      pragma Import (C, Internal, "g_list_model_items_changed");
   begin
      Internal (Get_Object (Self), Position, Removed, Added);
   end Items_Changed;

end Gtk.String_List;
