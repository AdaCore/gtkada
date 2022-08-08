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
with Gtkada.Bindings; use Gtkada.Bindings;

package body Glib.String is

   function From_Object_Free (B : access Gstring) return Gstring is
      Result : constant Gstring := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   ------------
   -- Append --
   ------------

   function Append (Self : Gstring; Val : UTF8_String) return Gstring is
      function Internal
         (Self : Gstring;
          Val  : Gtkada.Types.Chars_Ptr) return access Gstring;
      pragma Import (C, Internal, "g_string_append");
      Tmp_Val    : Gtkada.Types.Chars_Ptr := New_String (Val);
      Tmp_Return : access Gstring;
   begin
      Tmp_Return := Internal (Self, Tmp_Val);
      Free (Tmp_Val);
      return From_Object_Free (Tmp_Return);
   end Append;

   ----------------
   -- Append_Len --
   ----------------

   function Append_Len
      (Self : Gstring;
       Val  : UTF8_String;
       Len  : Gssize) return Gstring
   is
      function Internal
         (Self : Gstring;
          Val  : Gtkada.Types.Chars_Ptr;
          Len  : Gssize) return access Gstring;
      pragma Import (C, Internal, "g_string_append_len");
      Tmp_Val    : Gtkada.Types.Chars_Ptr := New_String (Val);
      Tmp_Return : access Gstring;
   begin
      Tmp_Return := Internal (Self, Tmp_Val, Len);
      Free (Tmp_Val);
      return From_Object_Free (Tmp_Return);
   end Append_Len;

   ------------------------
   -- Append_Uri_Escaped --
   ------------------------

   function Append_Uri_Escaped
      (Self                   : Gstring;
       Unescaped              : UTF8_String;
       Reserved_Chars_Allowed : UTF8_String;
       Allow_Utf8             : Boolean) return Gstring
   is
      function Internal
         (Self                   : Gstring;
          Unescaped              : Gtkada.Types.Chars_Ptr;
          Reserved_Chars_Allowed : Gtkada.Types.Chars_Ptr;
          Allow_Utf8             : Glib.Gboolean) return access Gstring;
      pragma Import (C, Internal, "g_string_append_uri_escaped");
      Tmp_Unescaped              : Gtkada.Types.Chars_Ptr := New_String (Unescaped);
      Tmp_Reserved_Chars_Allowed : Gtkada.Types.Chars_Ptr := New_String (Reserved_Chars_Allowed);
      Tmp_Return                 : access Gstring;
   begin
      Tmp_Return := Internal (Self, Tmp_Unescaped, Tmp_Reserved_Chars_Allowed, Boolean'Pos (Allow_Utf8));
      Free (Tmp_Reserved_Chars_Allowed);
      Free (Tmp_Unescaped);
      return From_Object_Free (Tmp_Return);
   end Append_Uri_Escaped;

   ------------
   -- Assign --
   ------------

   function Assign (Self : Gstring; Rval : UTF8_String) return Gstring is
      function Internal
         (Self : Gstring;
          Rval : Gtkada.Types.Chars_Ptr) return access Gstring;
      pragma Import (C, Internal, "g_string_assign");
      Tmp_Rval   : Gtkada.Types.Chars_Ptr := New_String (Rval);
      Tmp_Return : access Gstring;
   begin
      Tmp_Return := Internal (Self, Tmp_Rval);
      Free (Tmp_Rval);
      return From_Object_Free (Tmp_Return);
   end Assign;

   -----------
   -- Equal --
   -----------

   function Equal (Self : Gstring; V2 : Gstring) return Boolean is
      function Internal (Self : Gstring; V2 : Gstring) return Glib.Gboolean;
      pragma Import (C, Internal, "g_string_equal");
   begin
      return Internal (Self, V2) /= 0;
   end Equal;

   ----------
   -- Free --
   ----------

   function Free (Self : Gstring; Free_Segment : Boolean) return UTF8_String is
      function Internal
         (Self         : Gstring;
          Free_Segment : Glib.Gboolean) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_string_free");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self, Boolean'Pos (Free_Segment)));
   end Free;

   ------------
   -- Insert --
   ------------

   function Insert
      (Self : Gstring;
       Pos  : Gssize;
       Val  : UTF8_String) return Gstring
   is
      function Internal
         (Self : Gstring;
          Pos  : Gssize;
          Val  : Gtkada.Types.Chars_Ptr) return access Gstring;
      pragma Import (C, Internal, "g_string_insert");
      Tmp_Val    : Gtkada.Types.Chars_Ptr := New_String (Val);
      Tmp_Return : access Gstring;
   begin
      Tmp_Return := Internal (Self, Pos, Tmp_Val);
      Free (Tmp_Val);
      return From_Object_Free (Tmp_Return);
   end Insert;

   ----------------
   -- Insert_Len --
   ----------------

   function Insert_Len
      (Self : Gstring;
       Pos  : Gssize;
       Val  : UTF8_String;
       Len  : Gssize) return Gstring
   is
      function Internal
         (Self : Gstring;
          Pos  : Gssize;
          Val  : Gtkada.Types.Chars_Ptr;
          Len  : Gssize) return access Gstring;
      pragma Import (C, Internal, "g_string_insert_len");
      Tmp_Val    : Gtkada.Types.Chars_Ptr := New_String (Val);
      Tmp_Return : access Gstring;
   begin
      Tmp_Return := Internal (Self, Pos, Tmp_Val, Len);
      Free (Tmp_Val);
      return From_Object_Free (Tmp_Return);
   end Insert_Len;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
      (Self : Gstring;
       Pos  : Gsize;
       Val  : UTF8_String) return Gstring
   is
      function Internal
         (Self : Gstring;
          Pos  : Gsize;
          Val  : Gtkada.Types.Chars_Ptr) return access Gstring;
      pragma Import (C, Internal, "g_string_overwrite");
      Tmp_Val    : Gtkada.Types.Chars_Ptr := New_String (Val);
      Tmp_Return : access Gstring;
   begin
      Tmp_Return := Internal (Self, Pos, Tmp_Val);
      Free (Tmp_Val);
      return From_Object_Free (Tmp_Return);
   end Overwrite;

   -------------------
   -- Overwrite_Len --
   -------------------

   function Overwrite_Len
      (Self : Gstring;
       Pos  : Gsize;
       Val  : UTF8_String;
       Len  : Gssize) return Gstring
   is
      function Internal
         (Self : Gstring;
          Pos  : Gsize;
          Val  : Gtkada.Types.Chars_Ptr;
          Len  : Gssize) return access Gstring;
      pragma Import (C, Internal, "g_string_overwrite_len");
      Tmp_Val    : Gtkada.Types.Chars_Ptr := New_String (Val);
      Tmp_Return : access Gstring;
   begin
      Tmp_Return := Internal (Self, Pos, Tmp_Val, Len);
      Free (Tmp_Val);
      return From_Object_Free (Tmp_Return);
   end Overwrite_Len;

   -------------
   -- Prepend --
   -------------

   function Prepend (Self : Gstring; Val : UTF8_String) return Gstring is
      function Internal
         (Self : Gstring;
          Val  : Gtkada.Types.Chars_Ptr) return access Gstring;
      pragma Import (C, Internal, "g_string_prepend");
      Tmp_Val    : Gtkada.Types.Chars_Ptr := New_String (Val);
      Tmp_Return : access Gstring;
   begin
      Tmp_Return := Internal (Self, Tmp_Val);
      Free (Tmp_Val);
      return From_Object_Free (Tmp_Return);
   end Prepend;

   -----------------
   -- Prepend_Len --
   -----------------

   function Prepend_Len
      (Self : Gstring;
       Val  : UTF8_String;
       Len  : Gssize) return Gstring
   is
      function Internal
         (Self : Gstring;
          Val  : Gtkada.Types.Chars_Ptr;
          Len  : Gssize) return access Gstring;
      pragma Import (C, Internal, "g_string_prepend_len");
      Tmp_Val    : Gtkada.Types.Chars_Ptr := New_String (Val);
      Tmp_Return : access Gstring;
   begin
      Tmp_Return := Internal (Self, Tmp_Val, Len);
      Free (Tmp_Val);
      return From_Object_Free (Tmp_Return);
   end Prepend_Len;

end Glib.String;
