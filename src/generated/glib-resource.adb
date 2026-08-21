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
with Glib.Object;     use Glib.Object;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

use type Glib.Error.GError;

package body Glib.Resource is

   function From_Object_Free
     (B : access Gresource'Class) return Gresource
   is
      Result : constant Gresource := Gresource (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gresource is
      S : Gresource;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   ---------------------
   -- G_New_From_Data --
   ---------------------

   procedure G_New_From_Data
      (Self  : out Gresource;
       Data  : Glib.Bytes.Gbytes;
       Error : out Glib.Error.GError)
   is
      function Internal
         (Data      : System.Address;
          Acc_Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_resource_new_from_data");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Data), Acc_Error'Access);
      Error := Acc_Error;
      if Acc_Error = null then
         Self.Set_Object (Tmp_Return);
      end if;
   end G_New_From_Data;

   -----------------------------
   -- Gresource_New_From_Data --
   -----------------------------

   function Gresource_New_From_Data
      (Data  : Glib.Bytes.Gbytes;
       Error : out Glib.Error.GError) return Gresource
   is
      function Internal
         (Data      : System.Address;
          Acc_Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_resource_new_from_data");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : System.Address;
      Self       : Gresource;
   begin
      Tmp_Return := Internal (Get_Object (Data), Acc_Error'Access);
      Error := Acc_Error;
      if Acc_Error = null then
         Self.Set_Object (Tmp_Return);
      end if;
      return Self;
   end Gresource_New_From_Data;

   ------------------------
   -- Enumerate_Children --
   ------------------------

   function Enumerate_Children
      (Self         : Gresource;
       Path         : UTF8_String;
       Lookup_Flags : Resource_Lookup_Flags;
       Error        : out Glib.Error.GError) return GNAT.Strings.String_List
   is
      function Internal
         (Self         : System.Address;
          Path         : Gtkada.Types.Chars_Ptr;
          Lookup_Flags : Resource_Lookup_Flags;
          Acc_Error    : access Glib.Error.GError)
          return chars_ptr_array_access;
      pragma Import (C, Internal, "g_resource_enumerate_children");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Path, Lookup_Flags, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Path);
      if Acc_Error = null then
         return To_String_List_And_Free (Tmp_Return);
      else
         g_strfreev (Tmp_Return);
         return (1..0 => null);
      end if;
   end Enumerate_Children;

   --------------
   -- Get_Info --
   --------------

   function Get_Info
      (Self         : Gresource;
       Path         : UTF8_String;
       Lookup_Flags : Resource_Lookup_Flags;
       Size         : access Gsize := null;
       Flags        : access Guint32 := null;
       Error        : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self         : System.Address;
          Path         : Gtkada.Types.Chars_Ptr;
          Lookup_Flags : Resource_Lookup_Flags;
          Size         : access Gsize;
          Flags        : access Guint32;
          Acc_Error    : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_resource_get_info");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Path, Lookup_Flags, Size, Flags, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Path);
      return Tmp_Return /= 0;
   end Get_Info;

   ------------------
   -- Has_Children --
   ------------------

   function Has_Children
      (Self : Gresource;
       Path : UTF8_String) return Boolean
   is
      function Internal
         (Self : System.Address;
          Path : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_resource_has_children");
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Path);
      Free (Tmp_Path);
      return Tmp_Return /= 0;
   end Has_Children;

   -----------------
   -- Lookup_Data --
   -----------------

   function Lookup_Data
      (Self         : Gresource;
       Path         : UTF8_String;
       Lookup_Flags : Resource_Lookup_Flags;
       Error        : out Glib.Error.GError) return Glib.Bytes.Gbytes
   is
      function Internal
         (Self         : System.Address;
          Path         : Gtkada.Types.Chars_Ptr;
          Lookup_Flags : Resource_Lookup_Flags;
          Acc_Error    : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_resource_lookup_data");
      Acc_Error  : aliased Glib.Error.GError;
      Return_Obj : Glib.Bytes.Gbytes;
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Path, Lookup_Flags, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Path);
      if Acc_Error = null then
         Return_Obj := From_Object (Tmp_Return);
      end if;
      return Return_Obj;
   end Lookup_Data;

   -----------------
   -- Open_Stream --
   -----------------

   function Open_Stream
      (Self         : Gresource;
       Path         : UTF8_String;
       Lookup_Flags : Resource_Lookup_Flags;
       Error        : out Glib.Error.GError)
       return Glib.Input_Stream.Ginput_Stream
   is
      function Internal
         (Self         : System.Address;
          Path         : Gtkada.Types.Chars_Ptr;
          Lookup_Flags : Resource_Lookup_Flags;
          Acc_Error    : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_resource_open_stream");
      Acc_Error          : aliased Glib.Error.GError;
      Return_Obj         : Glib.Input_Stream.Ginput_Stream;
      Tmp_Path           : Gtkada.Types.Chars_Ptr := New_String (Path);
      Stub_Ginput_Stream : Glib.Input_Stream.Ginput_Stream_Record;
      Tmp_Return         : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Path, Lookup_Flags, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Path);
      if Acc_Error = null then
         Return_Obj := Glib.Input_Stream.Ginput_Stream (Get_User_Data (Tmp_Return, Stub_Ginput_Stream));
      end if;
      return Return_Obj;
   end Open_Stream;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gresource) return Gresource is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_resource_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   --------------
   -- Register --
   --------------

   procedure Register (Self : Gresource) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_resources_register");
   begin
      Internal (Get_Object (Self));
   end Register;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gresource) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_resource_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Self : Gresource) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_resources_unregister");
   begin
      Internal (Get_Object (Self));
   end Unregister;

   ----------
   -- Load --
   ----------

   function Load
      (Filename : UTF8_String;
       Error    : out Glib.Error.GError) return Gresource
   is
      function Internal
         (Filename  : Gtkada.Types.Chars_Ptr;
          Acc_Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_resource_load");
      Acc_Error    : aliased Glib.Error.GError;
      Return_Obj   : Gresource;
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Filename, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Filename);
      if Acc_Error = null then
         Return_Obj := From_Object (Tmp_Return);
      end if;
      return Return_Obj;
   end Load;

end Glib.Resource;
