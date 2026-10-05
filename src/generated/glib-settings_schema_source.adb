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
with Glib.Error;
with Gtkada.Bindings; use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

use type Glib.Error.GError;

package body Glib.Settings_Schema_Source is

   function From_Object_Free
     (B : access Gsettings_Schema_Source'Class) return Gsettings_Schema_Source
   is
      Result : constant Gsettings_Schema_Source := Gsettings_Schema_Source (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gsettings_Schema_Source is
      S : Gsettings_Schema_Source;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   --------------------------
   -- G_New_From_Directory --
   --------------------------

   procedure G_New_From_Directory
      (Self      : out Gsettings_Schema_Source;
       Directory : UTF8_String;
       Parent    : Gsettings_Schema_Source;
       Trusted   : Boolean;
       Error     : out Glib.Error.GError)
   is
      function Internal
         (Directory : Gtkada.Types.Chars_Ptr;
          Parent    : System.Address;
          Trusted   : Glib.Gboolean;
          Acc_Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_settings_schema_source_new_from_directory");
      Acc_Error     : aliased Glib.Error.GError;
      Tmp_Directory : Gtkada.Types.Chars_Ptr := New_String (Directory);
      Tmp_Return    : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Directory, Get_Object (Parent), Boolean'Pos (Trusted), Acc_Error'Access);
      Error := Acc_Error;
      if Error = null then
         Self.Set_Object (Tmp_Return);
      end if;
      Free (Tmp_Directory);
   end G_New_From_Directory;

   ------------------------------------------------
   -- Gsettings_Schema_Source_New_From_Directory --
   ------------------------------------------------

   function Gsettings_Schema_Source_New_From_Directory
      (Directory : UTF8_String;
       Parent    : Gsettings_Schema_Source;
       Trusted   : Boolean;
       Error     : out Glib.Error.GError) return Gsettings_Schema_Source
   is
      function Internal
         (Directory : Gtkada.Types.Chars_Ptr;
          Parent    : System.Address;
          Trusted   : Glib.Gboolean;
          Acc_Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_settings_schema_source_new_from_directory");
      Acc_Error     : aliased Glib.Error.GError;
      Tmp_Directory : Gtkada.Types.Chars_Ptr := New_String (Directory);
      Tmp_Return    : System.Address;
      Self          : Gsettings_Schema_Source;
   begin
      Tmp_Return := Internal (Tmp_Directory, Get_Object (Parent), Boolean'Pos (Trusted), Acc_Error'Access);
      Error := Acc_Error;
      if Error = null then
         Self.Set_Object (Tmp_Return);
      end if;
      Free (Tmp_Directory);
      return Self;
   end Gsettings_Schema_Source_New_From_Directory;

   ------------------
   -- List_Schemas --
   ------------------

   procedure List_Schemas
      (Self            : Gsettings_Schema_Source;
       Recursive       : Boolean;
       Non_Relocatable : out GNAT.Strings.String_List_Access;
       Relocatable     : out GNAT.Strings.String_List_Access)
   is
      procedure Internal
        (Self : System.Address; Recursive : Gboolean;
         Non_Relocatable, Relocatable : out chars_ptr_array_access);
      pragma Import (C, Internal, "g_settings_schema_source_list_schemas");
      Fixed, Movable : chars_ptr_array_access;
   begin
      Internal (Get_Object (Self), Boolean'Pos (Recursive), Fixed, Movable);
      Non_Relocatable := new GNAT.Strings.String_List'(To_String_List_And_Free (Fixed));
      Relocatable := new GNAT.Strings.String_List'(To_String_List_And_Free (Movable));
   end List_Schemas;

   ------------
   -- Lookup --
   ------------

   function Lookup
      (Self      : Gsettings_Schema_Source;
       Schema_Id : UTF8_String;
       Recursive : Boolean) return Glib.Settings_Schema.Gsettings_Schema
   is
      function Internal
         (Self      : System.Address;
          Schema_Id : Gtkada.Types.Chars_Ptr;
          Recursive : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "g_settings_schema_source_lookup");
      Tmp_Schema_Id : Gtkada.Types.Chars_Ptr := New_String (Schema_Id);
      Tmp_Return    : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Schema_Id, Boolean'Pos (Recursive));
      Free (Tmp_Schema_Id);
      return From_Object (Tmp_Return);
   end Lookup;

   ---------
   -- Ref --
   ---------

   function Ref
      (Self : Gsettings_Schema_Source) return Gsettings_Schema_Source
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_settings_schema_source_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gsettings_Schema_Source) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_settings_schema_source_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gsettings_Schema_Source is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_settings_schema_source_get_default");
   begin
      return From_Object (Internal);
   end Get_Default;

end Glib.Settings_Schema_Source;
