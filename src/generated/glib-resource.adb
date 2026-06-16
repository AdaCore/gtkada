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
      (Self : out Gresource;
       Data : Glib.Bytes.Gbytes)
   is
      function Internal (Data : System.Address) return System.Address;
      pragma Import (C, Internal, "g_resource_new_from_data");
   begin
      Self.Set_Object (Internal (Get_Object (Data)));
   end G_New_From_Data;

   -----------------------------
   -- Gresource_New_From_Data --
   -----------------------------

   function Gresource_New_From_Data
      (Data : Glib.Bytes.Gbytes) return Gresource
   is
      function Internal (Data : System.Address) return System.Address;
      pragma Import (C, Internal, "g_resource_new_from_data");
      Self : Gresource;
   begin
      Self.Set_Object (Internal (Get_Object (Data)));
      return Self;
   end Gresource_New_From_Data;

   ------------------------
   -- Enumerate_Children --
   ------------------------

   function Enumerate_Children
      (Self         : Gresource;
       Path         : UTF8_String;
       Lookup_Flags : Resource_Lookup_Flags) return GNAT.Strings.String_List
   is
      function Internal
         (Self         : System.Address;
          Path         : Gtkada.Types.Chars_Ptr;
          Lookup_Flags : Resource_Lookup_Flags)
          return chars_ptr_array_access;
      pragma Import (C, Internal, "g_resource_enumerate_children");
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Path, Lookup_Flags);
      Free (Tmp_Path);
      return To_String_List_And_Free (Tmp_Return);
   end Enumerate_Children;

   --------------
   -- Get_Info --
   --------------

   function Get_Info
      (Self         : Gresource;
       Path         : UTF8_String;
       Lookup_Flags : Resource_Lookup_Flags;
       Size         : access Gsize;
       Flags        : access Guint32) return Boolean
   is
      function Internal
         (Self         : System.Address;
          Path         : Gtkada.Types.Chars_Ptr;
          Lookup_Flags : Resource_Lookup_Flags;
          Acc_Size     : access Gsize;
          Acc_Flags    : access Guint32) return Glib.Gboolean;
      pragma Import (C, Internal, "g_resource_get_info");
      Acc_Size   : aliased Gsize;
      Acc_Flags  : aliased Guint32;
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Path, Lookup_Flags, Acc_Size'Access, Acc_Flags'Access);
      Free (Tmp_Path);
      if Size /= null then
         Size.all := Acc_Size;
      end if;
      if Flags /= null then
         Flags.all := Acc_Flags;
      end if;
      return Tmp_Return /= 0;
   end Get_Info;

   -----------------
   -- Lookup_Data --
   -----------------

   function Lookup_Data
      (Self         : Gresource;
       Path         : UTF8_String;
       Lookup_Flags : Resource_Lookup_Flags) return Glib.Bytes.Gbytes
   is
      function Internal
         (Self         : System.Address;
          Path         : Gtkada.Types.Chars_Ptr;
          Lookup_Flags : Resource_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "g_resource_lookup_data");
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Path, Lookup_Flags);
      Free (Tmp_Path);
      return From_Object (Tmp_Return);
   end Lookup_Data;

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

   function Load (Filename : UTF8_String) return Gresource is
      function Internal
         (Filename : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_resource_load");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Filename);
      Free (Tmp_Filename);
      return From_Object (Tmp_Return);
   end Load;

end Glib.Resource;
