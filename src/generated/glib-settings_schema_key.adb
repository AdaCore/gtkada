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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Glib.Settings_Schema_Key is

   function From_Object_Free
     (B : access Gsettings_Schema_Key'Class) return Gsettings_Schema_Key
   is
      Result : constant Gsettings_Schema_Key := Gsettings_Schema_Key (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gsettings_Schema_Key is
      S : Gsettings_Schema_Key;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -----------------------
   -- Get_Default_Value --
   -----------------------

   function Get_Default_Value
      (Self : Gsettings_Schema_Key) return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_settings_schema_key_get_default_value");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Default_Value;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description (Self : Gsettings_Schema_Key) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_settings_schema_key_get_description");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Description;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Gsettings_Schema_Key) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_settings_schema_key_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   ---------------
   -- Get_Range --
   ---------------

   function Get_Range
      (Self : Gsettings_Schema_Key) return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_settings_schema_key_get_range");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Range;

   -----------------
   -- Get_Summary --
   -----------------

   function Get_Summary (Self : Gsettings_Schema_Key) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_settings_schema_key_get_summary");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Summary;

   --------------------
   -- Get_Value_Type --
   --------------------

   function Get_Value_Type
      (Self : Gsettings_Schema_Key) return Glib.Variant.Gvariant_Type
   is
      function Internal
         (Self : System.Address) return Glib.Variant.Gvariant_Type;
      pragma Import (C, Internal, "g_settings_schema_key_get_value_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Value_Type;

   -----------------
   -- Range_Check --
   -----------------

   function Range_Check
      (Self  : Gsettings_Schema_Key;
       Value : Glib.Variant.Gvariant) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Value : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_schema_key_range_check");
   begin
      return Internal (Get_Object (Self), Get_Object (Value)) /= 0;
   end Range_Check;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gsettings_Schema_Key) return Gsettings_Schema_Key is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_settings_schema_key_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gsettings_Schema_Key) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_settings_schema_key_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Glib.Settings_Schema_Key;
