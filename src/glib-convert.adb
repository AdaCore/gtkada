------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

package body Glib.Convert is

   use type Gtkada.Types.Chars_Ptr;

   function g_convert
     (Str           : String;
      Len           : Gsize;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : access Gsize;
      Bytes_Written : access Gsize;
      Error         : GError_Access) return Gtkada.Types.Chars_Ptr;
   pragma Import (C, g_convert, "g_convert");

   -------------
   -- Convert --
   -------------

   procedure Convert
     (Str           : String;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String)
   is
      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : Gtkada.Types.Chars_Ptr;

   begin
      S := g_convert
        (Str, Str'Length, To_Codeset & ASCII.NUL, From_Codeset & ASCII.NUL,
         Read'Access, Written'Access, Error);
      Bytes_Read := Natural (Read);
      Bytes_Written := Natural (Written);

      declare
         Res : constant String := Gtkada.Types.Value (S);
      begin
         Result (Result'First .. Result'First + Bytes_Written - 1) := Res;
      end;

      Gtkada.Types.Free (S);
   end Convert;

   function Convert
     (Str           : String;
      To_Codeset    : String;
      From_Codeset  : String;
      Error         : GError_Access := null) return String
   is
      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : Gtkada.Types.Chars_Ptr;

   begin
      S := g_convert
        (Str, Str'Length, To_Codeset & ASCII.NUL, From_Codeset & ASCII.NUL,
         Read'Access, Written'Access, Error);

      if S = Gtkada.Types.Null_Ptr then
         return "";
      else
         declare
            Res : constant String := Gtkada.Types.Value (S);
         begin
            Gtkada.Types.Free (S);
            return Res;
         end;
      end if;
   end Convert;

   function Convert
     (Str           : String;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return Gtkada.Types.Chars_Ptr
   is
      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : Gtkada.Types.Chars_Ptr;

   begin
      S := g_convert
        (Str, Str'Length, To_Codeset & ASCII.NUL, From_Codeset & ASCII.NUL,
         Read'Access, Written'Access, Error);
      Bytes_Read.all := Natural (Read);
      Bytes_Written.all := Natural (Written);
      return S;
   end Convert;

   -----------------------
   -- Filename_From_URI --
   -----------------------

   function Filename_From_URI
     (URI      : String;
      Hostname : access Gtkada.Types.Chars_Ptr;
      Error    : GError_Access := null) return String
   is
      function Internal
        (URI      : String;
         Hostname : access Gtkada.Types.Chars_Ptr;
         Error    : GError_Access) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "ada_g_filename_from_uri");

      S   : Gtkada.Types.Chars_Ptr :=
        Internal (URI & ASCII.NUL, Hostname, Error);
      Str : constant String := Gtkada.Types.Value (S);

   begin
      Gtkada.Types.Free (S);
      return Str;
   end Filename_From_URI;

   ------------------------
   -- Filename_From_UTF8 --
   ------------------------

   function Filename_From_UTF8
     (UTF8_String : String;
      Error       : GError_Access := null) return String
   is
      function Internal
        (UTF8_String   : String;
         Len           : Gsize;
         Bytes_Read    : System.Address := System.Null_Address;
         Bytes_Written : System.Address := System.Null_Address;
         Error         : GError_Access) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "ada_g_filename_from_utf8");

      S   : Gtkada.Types.Chars_Ptr := Internal
        (UTF8_String, UTF8_String'Length, Error => Error);
      Str : constant String := Gtkada.Types.Value (S);

   begin
      Gtkada.Types.Free (S);
      return Str;
   end Filename_From_UTF8;

   ---------------------
   -- Filename_To_URI --
   ---------------------

   function Filename_To_URI
     (Filename : String;
      Hostname : String := "";
      Error    : GError_Access := null) return String
   is
      function Internal
        (URI      : String;
         Hostname : System.Address;
         Error    : GError_Access) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "ada_g_filename_to_uri");

      S    : Gtkada.Types.Chars_Ptr;
      Host : aliased constant String := Hostname & ASCII.NUL;

   begin
      if Hostname = "" then
         S := Internal (Filename & ASCII.NUL, System.Null_Address, Error);
      else
         S := Internal (Filename & ASCII.NUL, Host'Address, Error);
      end if;

      declare
         Str : constant String := Gtkada.Types.Value (S);
      begin
         Gtkada.Types.Free (S);
         return Str;
      end;
   end Filename_To_URI;

   ----------------------
   -- Filename_To_UTF8 --
   ----------------------

   function Filename_To_UTF8
     (OS_String : String;
      Error     : GError_Access := null) return String
   is
      function Internal
        (OS_String     : String;
         Len           : Gsize;
         Bytes_Read    : System.Address := System.Null_Address;
         Bytes_Written : System.Address := System.Null_Address;
         Error         : GError_Access) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "ada_g_filename_to_utf8");

      S   : Gtkada.Types.Chars_Ptr := Internal
        (OS_String, OS_String'Length, Error => Error);
   begin
      if S /= Gtkada.Types.Null_Ptr then
         return Str : constant String := Gtkada.Types.Value (S) do
            Gtkada.Types.Free (S);
         end return;
      else
         Gtkada.Types.Free (S);
         return "";
      end if;
   end Filename_To_UTF8;

   ----------------------
   -- Locale_From_UTF8 --
   ----------------------

   procedure Locale_From_UTF8
     (UTF8_String   : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String)
   is
      function Internal
        (UTF8_String   : String;
         Len           : Gsize;
         Bytes_Read    : access Gsize;
         Bytes_Written : access Gsize;
         Error         : GError_Access) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_locale_from_utf8");

      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : Gtkada.Types.Chars_Ptr;

   begin
      S := Internal
        (UTF8_String, UTF8_String'Length, Read'Access, Written'Access, Error);
      Bytes_Read := Natural (Read);
      Bytes_Written := Natural (Written);

      declare
         Res : constant String := Gtkada.Types.Value (S);
      begin
         Result (Result'First .. Result'First + Bytes_Written - 1) := Res;
      end;

      Gtkada.Types.Free (S);
   end Locale_From_UTF8;

   function Locale_From_UTF8
     (UTF8_String   : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return Gtkada.Types.Chars_Ptr
   is
      function Internal
        (UTF8_String   : String;
         Len           : Gsize;
         Bytes_Read    : access Gsize;
         Bytes_Written : access Gsize;
         Error         : GError_Access) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_locale_from_utf8");

      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : Gtkada.Types.Chars_Ptr;

   begin
      S := Internal
        (UTF8_String, UTF8_String'Length, Read'Access, Written'Access, Error);
      Bytes_Read.all := Natural (Read);
      Bytes_Written.all := Natural (Written);
      return S;
   end Locale_From_UTF8;

   function Locale_From_UTF8 (UTF8_String : String) return String is
      function Internal
        (UTF8_String   : String;
         Len           : Gsize;
         Bytes_Read    : System.Address := System.Null_Address;
         Bytes_Written : System.Address := System.Null_Address;
         Error         : GError_Access := null) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_locale_from_utf8");

      S : Gtkada.Types.Chars_Ptr :=
        Internal (UTF8_String, UTF8_String'Length);

   begin
      if S = Gtkada.Types.Null_Ptr then
         return "";
      else
         declare
            Str : constant String := Gtkada.Types.Value (S);
         begin
            Gtkada.Types.Free (S);
            return Str;
         end;
      end if;
   end Locale_From_UTF8;

   --------------------
   -- Locale_To_UTF8 --
   --------------------

   procedure Locale_To_UTF8
     (OS_String     : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String)
   is
      function Internal
        (UTF8_String   : String;
         Len           : Gsize;
         Bytes_Read    : access Gsize;
         Bytes_Written : access Gsize;
         Error         : GError_Access) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_locale_to_utf8");

      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : Gtkada.Types.Chars_Ptr;

   begin
      S := Internal
        (OS_String, OS_String'Length, Read'Access, Written'Access, Error);

      Bytes_Read := Natural (Read);
      Bytes_Written := Natural (Written);

      if S = Gtkada.Types.Null_Ptr then
         return;
      end if;

      declare
         Res : constant String := Gtkada.Types.Value (S);
      begin
         Result (Result'First .. Result'First + Bytes_Written - 1) := Res;
      end;

      Gtkada.Types.Free (S);
   end Locale_To_UTF8;

   function Locale_To_UTF8
     (OS_String     : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return Gtkada.Types.Chars_Ptr
   is
      function Internal
        (OS_String     : String;
         Len           : Gsize;
         Bytes_Read    : access Gsize;
         Bytes_Written : access Gsize;
         Error         : GError_Access) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_locale_to_utf8");

      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : Gtkada.Types.Chars_Ptr;

   begin
      S := Internal
        (OS_String, OS_String'Length, Read'Access, Written'Access, Error);
      Bytes_Read.all := Natural (Read);
      Bytes_Written.all := Natural (Written);
      return S;
   end Locale_To_UTF8;

   function Locale_To_UTF8 (OS_String : String) return String is
      function Internal
        (OS_String     : String;
         Len           : Gsize;
         Bytes_Read    : System.Address := System.Null_Address;
         Bytes_Written : System.Address := System.Null_Address;
         Error         : GError_Access := null) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_locale_to_utf8");

      S : Gtkada.Types.Chars_Ptr :=
        Internal (OS_String, OS_String'Length);

   begin
      if S = Gtkada.Types.Null_Ptr then
         return "";

      else
         declare
            Str : constant String := Gtkada.Types.Value (S);
         begin
            Gtkada.Types.Free (S);
            return Str;
         end;
      end if;
   end Locale_To_UTF8;

   -----------------
   -- Escape_Text --
   -----------------

   function Escape_Text (S : String) return String is
      function Internal (S : String; L : Integer) return
        Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_markup_escape_text");

      C_Res  : Gtkada.Types.Chars_Ptr :=
        Internal (S, S'Length);
      Result : constant String := Gtkada.Types.Value (C_Res);

   begin
      Gtkada.Types.Free (C_Res);
      return Result;
   end Escape_Text;

end Glib.Convert;
