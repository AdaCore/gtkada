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
pragma Warnings(On);

package body Gdk.Pixbuf_Format is

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description
      (Self : Gdk_Pixbuf_Format_Access) return UTF8_String
   is
      function Internal
         (Self : Gdk_Pixbuf_Format_Access) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_pixbuf_format_get_description");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Description;

   --------------------
   -- Get_Extensions --
   --------------------

   function Get_Extensions
      (Self : Gdk_Pixbuf_Format_Access) return GNAT.Strings.String_List
   is
      function Internal
         (Self : Gdk_Pixbuf_Format_Access) return chars_ptr_array_access;
      pragma Import (C, Internal, "gdk_pixbuf_format_get_extensions");
   begin
      return To_String_List_And_Free (Internal (Self));
   end Get_Extensions;

   -----------------
   -- Get_License --
   -----------------

   function Get_License (Self : Gdk_Pixbuf_Format_Access) return UTF8_String is
      function Internal
         (Self : Gdk_Pixbuf_Format_Access) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_pixbuf_format_get_license");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_License;

   --------------------
   -- Get_Mime_Types --
   --------------------

   function Get_Mime_Types
      (Self : Gdk_Pixbuf_Format_Access) return GNAT.Strings.String_List
   is
      function Internal
         (Self : Gdk_Pixbuf_Format_Access) return chars_ptr_array_access;
      pragma Import (C, Internal, "gdk_pixbuf_format_get_mime_types");
   begin
      return To_String_List_And_Free (Internal (Self));
   end Get_Mime_Types;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Gdk_Pixbuf_Format_Access) return UTF8_String is
      function Internal
         (Self : Gdk_Pixbuf_Format_Access) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_pixbuf_format_get_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Name;

   -----------------
   -- Is_Disabled --
   -----------------

   function Is_Disabled (Self : Gdk_Pixbuf_Format_Access) return Boolean is
      function Internal
         (Self : Gdk_Pixbuf_Format_Access) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_format_is_disabled");
   begin
      return Internal (Self) /= 0;
   end Is_Disabled;

   ------------------------------
   -- Is_Save_Option_Supported --
   ------------------------------

   function Is_Save_Option_Supported
      (Self       : Gdk_Pixbuf_Format_Access;
       Option_Key : UTF8_String) return Boolean
   is
      function Internal
         (Self       : Gdk_Pixbuf_Format_Access;
          Option_Key : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_format_is_save_option_supported");
      Tmp_Option_Key : Gtkada.Types.Chars_Ptr := New_String (Option_Key);
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Option_Key);
      Free (Tmp_Option_Key);
      return Tmp_Return /= 0;
   end Is_Save_Option_Supported;

   -----------------
   -- Is_Scalable --
   -----------------

   function Is_Scalable (Self : Gdk_Pixbuf_Format_Access) return Boolean is
      function Internal
         (Self : Gdk_Pixbuf_Format_Access) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_format_is_scalable");
   begin
      return Internal (Self) /= 0;
   end Is_Scalable;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable (Self : Gdk_Pixbuf_Format_Access) return Boolean is
      function Internal
         (Self : Gdk_Pixbuf_Format_Access) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_format_is_writable");
   begin
      return Internal (Self) /= 0;
   end Is_Writable;

   ------------------
   -- Set_Disabled --
   ------------------

   procedure Set_Disabled
      (Self     : Gdk_Pixbuf_Format_Access;
       Disabled : Boolean)
   is
      procedure Internal
         (Self     : Gdk_Pixbuf_Format_Access;
          Disabled : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_pixbuf_format_set_disabled");
   begin
      Internal (Self, Boolean'Pos (Disabled));
   end Set_Disabled;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Gdk_Pixbuf_Module_Pattern) return Gdk_Pixbuf_Module_Pattern
   is
      Result : constant Gdk_Pixbuf_Module_Pattern := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Gdk_Pixbuf_Format) return Gdk_Pixbuf_Format
   is
      Result : constant Gdk_Pixbuf_Format := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

end Gdk.Pixbuf_Format;
