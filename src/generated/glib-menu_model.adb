------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;

package body Glib.Menu_Model is

   package Type_Conversion_Gmenu_Model is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type_Menu_Model'Access, Gmenu_Model_Record);
   pragma Unreferenced (Type_Conversion_Gmenu_Model);

   package Type_Conversion_Gmenu_Attribute_Iter is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type_Menu_Attribute_Iter'Access, Gmenu_Attribute_Iter_Record);
   pragma Unreferenced (Type_Conversion_Gmenu_Attribute_Iter);

   package Type_Conversion_Gmenu_Link_Iter is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type_Menu_Link_Iter'Access, Gmenu_Link_Iter_Record);
   pragma Unreferenced (Type_Conversion_Gmenu_Link_Iter);

   ------------------------------
   -- Get_Item_Attribute_Value --
   ------------------------------

   function Get_Item_Attribute_Value
      (Self          : not null access Gmenu_Model_Record;
       Item_Index    : Glib.Gint;
       Attribute     : UTF8_String;
       Expected_Type : Glib.Variant.Gvariant_Type)
       return Glib.Variant.Gvariant
   is
      function Internal
         (Self          : System.Address;
          Item_Index    : Glib.Gint;
          Attribute     : Gtkada.Types.Chars_Ptr;
          Expected_Type : Glib.Variant.Gvariant_Type) return System.Address;
      pragma Import (C, Internal, "g_menu_model_get_item_attribute_value");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Item_Index, Tmp_Attribute, Expected_Type);
      Free (Tmp_Attribute);
      return From_Object (Tmp_Return);
   end Get_Item_Attribute_Value;

   -------------------
   -- Get_Item_Link --
   -------------------

   function Get_Item_Link
      (Self       : not null access Gmenu_Model_Record;
       Item_Index : Glib.Gint;
       Link       : UTF8_String) return Gmenu_Model
   is
      function Internal
         (Self       : System.Address;
          Item_Index : Glib.Gint;
          Link       : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_menu_model_get_item_link");
      Tmp_Link         : Gtkada.Types.Chars_Ptr := New_String (Link);
      Stub_Gmenu_Model : Gmenu_Model_Record;
      Tmp_Return       : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Item_Index, Tmp_Link);
      Free (Tmp_Link);
      return Glib.Menu_Model.Gmenu_Model (Get_User_Data (Tmp_Return, Stub_Gmenu_Model));
   end Get_Item_Link;

   -----------------
   -- Get_N_Items --
   -----------------

   function Get_N_Items
      (Self : not null access Gmenu_Model_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_menu_model_get_n_items");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Items;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gmenu_Attribute_Iter_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_menu_attribute_iter_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gmenu_Link_Iter_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_menu_link_iter_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Self : not null access Gmenu_Attribute_Iter_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_menu_attribute_iter_get_value");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Self : not null access Gmenu_Link_Iter_Record) return Gmenu_Model
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_menu_link_iter_get_value");
      Stub_Gmenu_Model : Gmenu_Model_Record;
   begin
      return Glib.Menu_Model.Gmenu_Model (Get_User_Data (Internal (Get_Object (Self)), Stub_Gmenu_Model));
   end Get_Value;

   ----------------
   -- Is_Mutable --
   ----------------

   function Is_Mutable
      (Self : not null access Gmenu_Model_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_menu_model_is_mutable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Mutable;

   -------------------
   -- Items_Changed --
   -------------------

   procedure Items_Changed
      (Self     : not null access Gmenu_Model_Record;
       Position : Glib.Gint;
       Removed  : Glib.Gint;
       Added    : Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Glib.Gint;
          Removed  : Glib.Gint;
          Added    : Glib.Gint);
      pragma Import (C, Internal, "g_menu_model_items_changed");
   begin
      Internal (Get_Object (Self), Position, Removed, Added);
   end Items_Changed;

   -----------------------------
   -- Iterate_Item_Attributes --
   -----------------------------

   function Iterate_Item_Attributes
      (Self       : not null access Gmenu_Model_Record;
       Item_Index : Glib.Gint) return Gmenu_Attribute_Iter
   is
      function Internal
         (Self       : System.Address;
          Item_Index : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "g_menu_model_iterate_item_attributes");
      Stub_Gmenu_Attribute_Iter : Gmenu_Attribute_Iter_Record;
   begin
      return Glib.Menu_Model.Gmenu_Attribute_Iter (Get_User_Data (Internal (Get_Object (Self), Item_Index), Stub_Gmenu_Attribute_Iter));
   end Iterate_Item_Attributes;

   ------------------------
   -- Iterate_Item_Links --
   ------------------------

   function Iterate_Item_Links
      (Self       : not null access Gmenu_Model_Record;
       Item_Index : Glib.Gint) return Gmenu_Link_Iter
   is
      function Internal
         (Self       : System.Address;
          Item_Index : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "g_menu_model_iterate_item_links");
      Stub_Gmenu_Link_Iter : Gmenu_Link_Iter_Record;
   begin
      return Glib.Menu_Model.Gmenu_Link_Iter (Get_User_Data (Internal (Get_Object (Self), Item_Index), Stub_Gmenu_Link_Iter));
   end Iterate_Item_Links;

   ----------
   -- Next --
   ----------

   function Next
      (Self : not null access Gmenu_Attribute_Iter_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_menu_attribute_iter_next");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Next;

   ----------
   -- Next --
   ----------

   function Next
      (Self : not null access Gmenu_Link_Iter_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_menu_link_iter_next");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Next;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gmenu_Model_Gint_Gint_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gmenu_Model_Gint_Gint_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Gint_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Gint_Gint_Void);

   procedure Connect
      (Object  : access Gmenu_Model_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmenu_Model_Gint_Gint_Gint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gmenu_Model_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gint_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Gint_Gint_Void);

   procedure Marsh_Gmenu_Model_Gint_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gmenu_Model_Gint_Gint_Gint_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gmenu_Model_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmenu_Model_Gint_Gint_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gmenu_Model_Gint_Gint_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gmenu_Model_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Gint_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------------------
   -- Marsh_GObject_Gint_Gint_Gint_Void --
   ---------------------------------------

   procedure Marsh_GObject_Gint_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Gint_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Gint (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Gint_Gint_Void;

   -------------------------------------------
   -- Marsh_Gmenu_Model_Gint_Gint_Gint_Void --
   -------------------------------------------

   procedure Marsh_Gmenu_Model_Gint_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gmenu_Model_Gint_Gint_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gmenu_Model := Gmenu_Model (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Gint (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Gmenu_Model_Gint_Gint_Gint_Void;

   ----------------------
   -- On_Items_Changed --
   ----------------------

   procedure On_Items_Changed
      (Self  : not null access Gmenu_Model_Record;
       Call  : Cb_Gmenu_Model_Gint_Gint_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "items-changed" & ASCII.NUL, Call, After);
   end On_Items_Changed;

   ----------------------
   -- On_Items_Changed --
   ----------------------

   procedure On_Items_Changed
      (Self  : not null access Gmenu_Model_Record;
       Call  : Cb_GObject_Gint_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "items-changed" & ASCII.NUL, Call, After, Slot);
   end On_Items_Changed;

end Glib.Menu_Model;
