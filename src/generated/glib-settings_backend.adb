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

package body Glib.Settings_Backend is

   function Memory_New return Gsettings_Backend is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_memory_settings_backend_new");
      Stub : Gsettings_Backend_Record;
   begin
      return Gsettings_Backend (Get_User_Data (Internal, Stub));
   end Memory_New;

   package Type_Conversion_Gsettings_Backend is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gsettings_Backend_Record);
   pragma Unreferenced (Type_Conversion_Gsettings_Backend);

   -------------
   -- Changed --
   -------------

   procedure Changed
      (Self       : not null access Gsettings_Backend_Record;
       Key        : UTF8_String;
       Origin_Tag : System.Address)
   is
      procedure Internal
         (Self       : System.Address;
          Key        : Gtkada.Types.Chars_Ptr;
          Origin_Tag : System.Address);
      pragma Import (C, Internal, "g_settings_backend_changed");
      Tmp_Key : Gtkada.Types.Chars_Ptr := New_String (Key);
   begin
      Internal (Get_Object (Self), Tmp_Key, Origin_Tag);
      Free (Tmp_Key);
   end Changed;

   ------------------
   -- Keys_Changed --
   ------------------

   procedure Keys_Changed
      (Self       : not null access Gsettings_Backend_Record;
       Path       : UTF8_String;
       Items      : GNAT.Strings.String_List;
       Origin_Tag : System.Address)
   is
      procedure Internal
         (Self       : System.Address;
          Path       : Gtkada.Types.Chars_Ptr;
          Items      : Gtkada.Types.chars_ptr_array;
          Origin_Tag : System.Address);
      pragma Import (C, Internal, "g_settings_backend_keys_changed");
      Tmp_Path  : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Items : Gtkada.Types.chars_ptr_array := From_String_List (Items);
   begin
      Internal (Get_Object (Self), Tmp_Path, Tmp_Items, Origin_Tag);
      Gtkada.Types.Free (Tmp_Items);
      Free (Tmp_Path);
   end Keys_Changed;

   ------------------
   -- Path_Changed --
   ------------------

   procedure Path_Changed
      (Self       : not null access Gsettings_Backend_Record;
       Path       : UTF8_String;
       Origin_Tag : System.Address)
   is
      procedure Internal
         (Self       : System.Address;
          Path       : Gtkada.Types.Chars_Ptr;
          Origin_Tag : System.Address);
      pragma Import (C, Internal, "g_settings_backend_path_changed");
      Tmp_Path : Gtkada.Types.Chars_Ptr := New_String (Path);
   begin
      Internal (Get_Object (Self), Tmp_Path, Origin_Tag);
      Free (Tmp_Path);
   end Path_Changed;

   ---------------------------
   -- Path_Writable_Changed --
   ---------------------------

   procedure Path_Writable_Changed
      (Self : not null access Gsettings_Backend_Record;
       Path : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_settings_backend_path_writable_changed");
      Tmp_Path : Gtkada.Types.Chars_Ptr := New_String (Path);
   begin
      Internal (Get_Object (Self), Tmp_Path);
      Free (Tmp_Path);
   end Path_Writable_Changed;

   ----------------------
   -- Writable_Changed --
   ----------------------

   procedure Writable_Changed
      (Self : not null access Gsettings_Backend_Record;
       Key  : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_settings_backend_writable_changed");
      Tmp_Key : Gtkada.Types.Chars_Ptr := New_String (Key);
   begin
      Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
   end Writable_Changed;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gsettings_Backend is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_settings_backend_get_default");
      Stub_Gsettings_Backend : Gsettings_Backend_Record;
   begin
      return Glib.Settings_Backend.Gsettings_Backend (Get_User_Data (Internal, Stub_Gsettings_Backend));
   end Get_Default;

end Glib.Settings_Backend;
