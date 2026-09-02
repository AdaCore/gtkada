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

package body Glib.String is

   function From_Object_Free (B : access Gstring) return Gstring is
      Result : constant Gstring := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gstring; Init : UTF8_String := "") is
      function Internal (Init : Gtkada.Types.Chars_Ptr) return Gstring;
      pragma Import (C, Internal, "g_string_new");
      Tmp_Init   : Gtkada.Types.Chars_Ptr;
      Tmp_Return : Gstring;
   begin
      Tmp_Init :=
        (if Init = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Init));
      Tmp_Return := Internal (Tmp_Init);
      Self := Tmp_Return;
      Free (Tmp_Init);
   end G_New;

   ---------------
   -- G_New_Len --
   ---------------

   procedure G_New_Len
      (Self : out Gstring;
       Init : UTF8_String;
       Len  : Gssize)
   is
      function Internal
         (Init : Gtkada.Types.Chars_Ptr;
          Len  : Gssize) return Gstring;
      pragma Import (C, Internal, "g_string_new_len");
      Tmp_Init   : Gtkada.Types.Chars_Ptr := New_String (Init);
      Tmp_Return : Gstring;
   begin
      Tmp_Return := Internal (Tmp_Init, Len);
      Self := Tmp_Return;
      Free (Tmp_Init);
   end G_New_Len;

   ----------------
   -- G_New_Take --
   ----------------

   procedure G_New_Take (Self : out Gstring; Init : UTF8_String := "") is
      function Internal (Init : Gtkada.Types.Chars_Ptr) return Gstring;
      pragma Import (C, Internal, "g_string_new_take");
      Tmp_Init   : Gtkada.Types.Chars_Ptr;
      Tmp_Return : Gstring;
   begin
      Tmp_Init :=
        (if Init = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Init));
      Tmp_Return := Internal (Tmp_Init);
      Self := Tmp_Return;
      Free (Tmp_Init);
   end G_New_Take;

   -----------------
   -- G_Sized_New --
   -----------------

   procedure G_Sized_New (Self : out Gstring; Dfl_Size : Gsize) is
      function Internal (Dfl_Size : Gsize) return Gstring;
      pragma Import (C, Internal, "g_string_sized_new");
   begin
      Self := Internal (Dfl_Size);
   end G_Sized_New;

   -----------------
   -- Gstring_New --
   -----------------

   function Gstring_New (Init : UTF8_String := "") return Gstring is
      function Internal (Init : Gtkada.Types.Chars_Ptr) return Gstring;
      pragma Import (C, Internal, "g_string_new");
      Tmp_Init   : Gtkada.Types.Chars_Ptr;
      Tmp_Return : Gstring;
      Self       : Gstring;
   begin
      Tmp_Init :=
        (if Init = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Init));
      Tmp_Return := Internal (Tmp_Init);
      Self := Tmp_Return;
      Free (Tmp_Init);
      return Self;
   end Gstring_New;

   ---------------------
   -- Gstring_New_Len --
   ---------------------

   function Gstring_New_Len
      (Init : UTF8_String;
       Len  : Gssize) return Gstring
   is
      function Internal
         (Init : Gtkada.Types.Chars_Ptr;
          Len  : Gssize) return Gstring;
      pragma Import (C, Internal, "g_string_new_len");
      Tmp_Init   : Gtkada.Types.Chars_Ptr := New_String (Init);
      Tmp_Return : Gstring;
      Self       : Gstring;
   begin
      Tmp_Return := Internal (Tmp_Init, Len);
      Self := Tmp_Return;
      Free (Tmp_Init);
      return Self;
   end Gstring_New_Len;

   ----------------------
   -- Gstring_New_Take --
   ----------------------

   function Gstring_New_Take (Init : UTF8_String := "") return Gstring is
      function Internal (Init : Gtkada.Types.Chars_Ptr) return Gstring;
      pragma Import (C, Internal, "g_string_new_take");
      Tmp_Init   : Gtkada.Types.Chars_Ptr;
      Tmp_Return : Gstring;
      Self       : Gstring;
   begin
      Tmp_Init :=
        (if Init = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Init));
      Tmp_Return := Internal (Tmp_Init);
      Self := Tmp_Return;
      Free (Tmp_Init);
      return Self;
   end Gstring_New_Take;

   -----------------------
   -- Gstring_Sized_New --
   -----------------------

   function Gstring_Sized_New (Dfl_Size : Gsize) return Gstring is
      function Internal (Dfl_Size : Gsize) return Gstring;
      pragma Import (C, Internal, "g_string_sized_new");
      Self : Gstring;
   begin
      Self := Internal (Dfl_Size);
      return Self;
   end Gstring_Sized_New;

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
      return Tmp_Return.all;
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
      return Tmp_Return.all;
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
      return Tmp_Return.all;
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
      return Tmp_Return.all;
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

   --------------------
   -- Free_And_Steal --
   --------------------

   function Free_And_Steal (Self : Gstring) return UTF8_String is
      function Internal (Self : Gstring) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_string_free_and_steal");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Free_And_Steal;

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
      return Tmp_Return.all;
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
      return Tmp_Return.all;
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
      return Tmp_Return.all;
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
      return Tmp_Return.all;
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
      return Tmp_Return.all;
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
      return Tmp_Return.all;
   end Prepend_Len;

   -------------
   -- Replace --
   -------------

   function Replace
      (Self    : Gstring;
       Find    : UTF8_String;
       Replace : UTF8_String;
       Limit   : Guint) return Guint
   is
      function Internal
         (Self    : Gstring;
          Find    : Gtkada.Types.Chars_Ptr;
          Replace : Gtkada.Types.Chars_Ptr;
          Limit   : Guint) return Guint;
      pragma Import (C, Internal, "g_string_replace");
      Tmp_Find    : Gtkada.Types.Chars_Ptr := New_String (Find);
      Tmp_Replace : Gtkada.Types.Chars_Ptr := New_String (Replace);
      Tmp_Return  : Guint;
   begin
      Tmp_Return := Internal (Self, Tmp_Find, Tmp_Replace, Limit);
      Free (Tmp_Replace);
      Free (Tmp_Find);
      return Tmp_Return;
   end Replace;

end Glib.String;
