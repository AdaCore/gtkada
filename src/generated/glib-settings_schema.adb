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

package body Glib.Settings_Schema is

   function From_Object_Free
     (B : access Gsettings_Schema'Class) return Gsettings_Schema
   is
      Result : constant Gsettings_Schema := Gsettings_Schema (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gsettings_Schema is
      S : Gsettings_Schema;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (Self : Gsettings_Schema) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_settings_schema_get_id");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Id;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
      (Self : Gsettings_Schema;
       Name : UTF8_String)
       return Glib.Settings_Schema_Key.Gsettings_Schema_Key
   is
      function Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_settings_schema_get_key");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
      return From_Object (Tmp_Return);
   end Get_Key;

   --------------
   -- Get_Path --
   --------------

   function Get_Path (Self : Gsettings_Schema) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_settings_schema_get_path");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Path;

   -------------
   -- Has_Key --
   -------------

   function Has_Key
      (Self : Gsettings_Schema;
       Name : UTF8_String) return Boolean
   is
      function Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_schema_has_key");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
      return Tmp_Return /= 0;
   end Has_Key;

   -------------------
   -- List_Children --
   -------------------

   function List_Children
      (Self : Gsettings_Schema) return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_settings_schema_list_children");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self)));
   end List_Children;

   ---------------
   -- List_Keys --
   ---------------

   function List_Keys
      (Self : Gsettings_Schema) return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_settings_schema_list_keys");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self)));
   end List_Keys;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gsettings_Schema) return Gsettings_Schema is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_settings_schema_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gsettings_Schema) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_settings_schema_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Glib.Settings_Schema;
