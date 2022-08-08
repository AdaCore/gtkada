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
with Ada.Unchecked_Conversion;
with Gtkada.Bindings;                      use Gtkada.Bindings;
with System.Address_To_Access_Conversions;

package body Gtk.Text_Iter is

   function From_Object_Free (B : access Gtk_Text_Iter) return Gtk_Text_Iter is
      Result : constant Gtk_Text_Iter := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   type Gtk_Text_Iter_Access is access Gtk_Text_Iter;
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Text_Iter_Access);
   use type System.Address;

   ------------------
   -- Iter_Or_Null --
   ------------------

   function Iter_Or_Null (Iter : System.Address) return System.Address is
   begin
      if Convert (Iter).all = Null_Text_Iter then--  null iter
         return System.Null_Address;
      else
         return Iter;
      end if;
   end Iter_Or_Null;

   ----------
   -- Copy --
   ----------

   procedure Copy (Source : Gtk_Text_Iter; Dest : out Gtk_Text_Iter) is
   begin
      Dest := Source;
   end Copy;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (Iter : Gtk_Text_Iter) return Character is
      Result         : constant Gunichar := Get_Char (Iter);
      Eight_LSB_Mask : constant := 2#1111_1111#;

   begin
      --  This function relies on the Get_Char function provided by gtk+,
      --  which returns a gunichar value. Only the 8 least significant bits
      --  are then kept to deduce the associated character.

      return Character'Val (Result and Eight_LSB_Mask);
   end Get_Char;

   -------------------
   -- Get_Text_Iter --
   -------------------

   package Iter_Access_Address_Conversions is
      new System.Address_To_Access_Conversions (Gtk_Text_Iter);

   procedure Get_Text_Iter
     (Val  : Glib.Values.GValue;
      Iter : out Gtk_Text_Iter) is
   begin
      Copy
        (Source => Iter_Access_Address_Conversions.To_Pointer
           (Glib.Values.Get_Address (Val)).all,
         Dest   => Iter);
   end Get_Text_Iter;

   ---------------
   -- Get_Slice --
   ---------------

   function Get_Slice
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter) return Gtkada.Types.Chars_Ptr
   is
      function Internal
        (Start   : Gtk_Text_Iter;
         The_End : Gtk_Text_Iter) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_slice");

   begin
      return Internal (Start, The_End);
   end Get_Slice;

   -------------------
   -- Backward_Char --
   -------------------

   procedure Backward_Char
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_char");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Char;

   --------------------
   -- Backward_Chars --
   --------------------

   procedure Backward_Chars
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_chars");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Chars;

   ------------------------------
   -- Backward_Cursor_Position --
   ------------------------------

   procedure Backward_Cursor_Position
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_cursor_position");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Cursor_Position;

   -------------------------------
   -- Backward_Cursor_Positions --
   -------------------------------

   procedure Backward_Cursor_Positions
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_cursor_positions");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Cursor_Positions;

   -------------------
   -- Backward_Line --
   -------------------

   procedure Backward_Line
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_line");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Line;

   --------------------
   -- Backward_Lines --
   --------------------

   procedure Backward_Lines
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_lines");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Lines;

   ---------------------
   -- Backward_Search --
   ---------------------

   procedure Backward_Search
      (Iter        : in out Gtk_Text_Iter;
       Str         : UTF8_String;
       Flags       : Gtk_Text_Search_Flags;
       Match_Start : out Gtk_Text_Iter;
       Match_End   : out Gtk_Text_Iter;
       Limit       : Gtk_Text_Iter := Null_Text_Iter;
       Result      : out Boolean)
   is
      function Internal
         (Acc_Iter        : access Gtk_Text_Iter;
          Str             : Gtkada.Types.Chars_Ptr;
          Flags           : Gtk_Text_Search_Flags;
          Acc_Match_Start : access Gtk_Text_Iter;
          Acc_Match_End   : access Gtk_Text_Iter;
          Limit           : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_search");
      Acc_Iter            : aliased Gtk_Text_Iter := Iter;
      Acc_Match_Start     : aliased Gtk_Text_Iter;
      Acc_Match_End       : aliased Gtk_Text_Iter;
      Tmp_Str             : Gtkada.Types.Chars_Ptr := New_String (Str);
      Tmp_Acc_Match_Start : aliased Gtk_Text_Iter;
      Tmp_Acc_Match_End   : aliased Gtk_Text_Iter;
      Tmp_Return          : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Tmp_Str, Flags, Tmp_Acc_Match_Start'Access, Tmp_Acc_Match_End'Access, Iter_Or_Null (Limit'Address));
      Acc_Match_End := Tmp_Acc_Match_End;
      Acc_Match_Start := Tmp_Acc_Match_Start;
      Free (Tmp_Str);
      Iter := Acc_Iter;
      Match_Start := Acc_Match_Start;
      Match_End := Acc_Match_End;
      Result := Tmp_Return /= 0;
   end Backward_Search;

   -----------------------------
   -- Backward_Sentence_Start --
   -----------------------------

   procedure Backward_Sentence_Start
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_sentence_start");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Sentence_Start;

   ------------------------------
   -- Backward_Sentence_Starts --
   ------------------------------

   procedure Backward_Sentence_Starts
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_sentence_starts");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Sentence_Starts;

   ----------------------------
   -- Backward_To_Tag_Toggle --
   ----------------------------

   procedure Backward_To_Tag_Toggle
      (Iter   : in out Gtk_Text_Iter;
       Tag    : Gtk.Text_Tag.Gtk_Text_Tag := null;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Tag      : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_to_tag_toggle");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Get_Object_Or_Null (GObject (Tag)));
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_To_Tag_Toggle;

   --------------------------------------
   -- Backward_Visible_Cursor_Position --
   --------------------------------------

   procedure Backward_Visible_Cursor_Position
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_visible_cursor_position");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Visible_Cursor_Position;

   ---------------------------------------
   -- Backward_Visible_Cursor_Positions --
   ---------------------------------------

   procedure Backward_Visible_Cursor_Positions
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_visible_cursor_positions");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Visible_Cursor_Positions;

   ---------------------------
   -- Backward_Visible_Line --
   ---------------------------

   procedure Backward_Visible_Line
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_visible_line");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Visible_Line;

   ----------------------------
   -- Backward_Visible_Lines --
   ----------------------------

   procedure Backward_Visible_Lines
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_visible_lines");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Visible_Lines;

   ---------------------------------
   -- Backward_Visible_Word_Start --
   ---------------------------------

   procedure Backward_Visible_Word_Start
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_visible_word_start");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Visible_Word_Start;

   ----------------------------------
   -- Backward_Visible_Word_Starts --
   ----------------------------------

   procedure Backward_Visible_Word_Starts
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_visible_word_starts");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Visible_Word_Starts;

   -------------------------
   -- Backward_Word_Start --
   -------------------------

   procedure Backward_Word_Start
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_word_start");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Word_Start;

   --------------------------
   -- Backward_Word_Starts --
   --------------------------

   procedure Backward_Word_Starts
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_word_starts");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backward_Word_Starts;

   ----------------
   -- Begins_Tag --
   ----------------

   function Begins_Tag
      (Iter : Gtk_Text_Iter;
       Tag  : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class) return Boolean
   is
      function Internal
         (Iter : Gtk_Text_Iter;
          Tag  : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_begins_tag");
   begin
      return Internal (Iter, Get_Object_Or_Null (GObject (Tag))) /= 0;
   end Begins_Tag;

   ----------------
   -- Can_Insert --
   ----------------

   function Can_Insert
      (Iter                : Gtk_Text_Iter;
       Default_Editability : Boolean) return Boolean
   is
      function Internal
         (Iter                : Gtk_Text_Iter;
          Default_Editability : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_can_insert");
   begin
      return Internal (Iter, Boolean'Pos (Default_Editability)) /= 0;
   end Can_Insert;

   --------------
   -- Editable --
   --------------

   function Editable
      (Iter            : Gtk_Text_Iter;
       Default_Setting : Boolean) return Boolean
   is
      function Internal
         (Iter            : Gtk_Text_Iter;
          Default_Setting : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_editable");
   begin
      return Internal (Iter, Boolean'Pos (Default_Setting)) /= 0;
   end Editable;

   ---------------
   -- Ends_Line --
   ---------------

   function Ends_Line (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_ends_line");
   begin
      return Internal (Iter) /= 0;
   end Ends_Line;

   -------------------
   -- Ends_Sentence --
   -------------------

   function Ends_Sentence (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_ends_sentence");
   begin
      return Internal (Iter) /= 0;
   end Ends_Sentence;

   --------------
   -- Ends_Tag --
   --------------

   function Ends_Tag
      (Iter : Gtk_Text_Iter;
       Tag  : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class) return Boolean
   is
      function Internal
         (Iter : Gtk_Text_Iter;
          Tag  : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_ends_tag");
   begin
      return Internal (Iter, Get_Object_Or_Null (GObject (Tag))) /= 0;
   end Ends_Tag;

   ---------------
   -- Ends_Word --
   ---------------

   function Ends_Word (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_ends_word");
   begin
      return Internal (Iter) /= 0;
   end Ends_Word;

   -----------
   -- Equal --
   -----------

   function Equal (Iter : Gtk_Text_Iter; Rhs : Gtk_Text_Iter) return Boolean is
      function Internal
         (Iter : Gtk_Text_Iter;
          Rhs  : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_equal");
   begin
      return Internal (Iter, Rhs) /= 0;
   end Equal;

   ------------------
   -- Forward_Char --
   ------------------

   procedure Forward_Char
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_char");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Char;

   -------------------
   -- Forward_Chars --
   -------------------

   procedure Forward_Chars
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_chars");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Chars;

   -----------------------------
   -- Forward_Cursor_Position --
   -----------------------------

   procedure Forward_Cursor_Position
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_cursor_position");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Cursor_Position;

   ------------------------------
   -- Forward_Cursor_Positions --
   ------------------------------

   procedure Forward_Cursor_Positions
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_cursor_positions");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Cursor_Positions;

   ------------------
   -- Forward_Line --
   ------------------

   procedure Forward_Line
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_line");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Line;

   -------------------
   -- Forward_Lines --
   -------------------

   procedure Forward_Lines
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_lines");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Lines;

   --------------------
   -- Forward_Search --
   --------------------

   procedure Forward_Search
      (Iter        : in out Gtk_Text_Iter;
       Str         : UTF8_String;
       Flags       : Gtk_Text_Search_Flags;
       Match_Start : out Gtk_Text_Iter;
       Match_End   : out Gtk_Text_Iter;
       Limit       : Gtk_Text_Iter := Null_Text_Iter;
       Result      : out Boolean)
   is
      function Internal
         (Acc_Iter        : access Gtk_Text_Iter;
          Str             : Gtkada.Types.Chars_Ptr;
          Flags           : Gtk_Text_Search_Flags;
          Acc_Match_Start : access Gtk_Text_Iter;
          Acc_Match_End   : access Gtk_Text_Iter;
          Limit           : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_search");
      Acc_Iter            : aliased Gtk_Text_Iter := Iter;
      Acc_Match_Start     : aliased Gtk_Text_Iter;
      Acc_Match_End       : aliased Gtk_Text_Iter;
      Tmp_Str             : Gtkada.Types.Chars_Ptr := New_String (Str);
      Tmp_Acc_Match_Start : aliased Gtk_Text_Iter;
      Tmp_Acc_Match_End   : aliased Gtk_Text_Iter;
      Tmp_Return          : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Tmp_Str, Flags, Tmp_Acc_Match_Start'Access, Tmp_Acc_Match_End'Access, Iter_Or_Null (Limit'Address));
      Acc_Match_End := Tmp_Acc_Match_End;
      Acc_Match_Start := Tmp_Acc_Match_Start;
      Free (Tmp_Str);
      Iter := Acc_Iter;
      Match_Start := Acc_Match_Start;
      Match_End := Acc_Match_End;
      Result := Tmp_Return /= 0;
   end Forward_Search;

   --------------------------
   -- Forward_Sentence_End --
   --------------------------

   procedure Forward_Sentence_End
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_sentence_end");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Sentence_End;

   ---------------------------
   -- Forward_Sentence_Ends --
   ---------------------------

   procedure Forward_Sentence_Ends
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_sentence_ends");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Sentence_Ends;

   -------------------------
   -- Forward_To_Line_End --
   -------------------------

   procedure Forward_To_Line_End
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_to_line_end");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_To_Line_End;

   ---------------------------
   -- Forward_To_Tag_Toggle --
   ---------------------------

   procedure Forward_To_Tag_Toggle
      (Iter   : in out Gtk_Text_Iter;
       Tag    : Gtk.Text_Tag.Gtk_Text_Tag := null;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Tag      : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_to_tag_toggle");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Get_Object_Or_Null (GObject (Tag)));
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_To_Tag_Toggle;

   -------------------------------------
   -- Forward_Visible_Cursor_Position --
   -------------------------------------

   procedure Forward_Visible_Cursor_Position
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_visible_cursor_position");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Visible_Cursor_Position;

   --------------------------------------
   -- Forward_Visible_Cursor_Positions --
   --------------------------------------

   procedure Forward_Visible_Cursor_Positions
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_visible_cursor_positions");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Visible_Cursor_Positions;

   --------------------------
   -- Forward_Visible_Line --
   --------------------------

   procedure Forward_Visible_Line
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_visible_line");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Visible_Line;

   ---------------------------
   -- Forward_Visible_Lines --
   ---------------------------

   procedure Forward_Visible_Lines
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_visible_lines");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Visible_Lines;

   ------------------------------
   -- Forward_Visible_Word_End --
   ------------------------------

   procedure Forward_Visible_Word_End
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_visible_word_end");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Visible_Word_End;

   -------------------------------
   -- Forward_Visible_Word_Ends --
   -------------------------------

   procedure Forward_Visible_Word_Ends
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_visible_word_ends");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Visible_Word_Ends;

   ----------------------
   -- Forward_Word_End --
   ----------------------

   procedure Forward_Word_End
      (Iter   : in out Gtk_Text_Iter;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_word_end");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Word_End;

   -----------------------
   -- Forward_Word_Ends --
   -----------------------

   procedure Forward_Word_Ends
      (Iter   : in out Gtk_Text_Iter;
       Count  : Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Acc_Iter : access Gtk_Text_Iter;
          Count    : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_word_ends");
      Acc_Iter   : aliased Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_Iter'Access, Count);
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Forward_Word_Ends;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
      (Iter   : Gtk_Text_Iter;
       Values : access Gtk.Text_Attributes.Gtk_Text_Attributes)
       return Boolean
   is
      function Internal
         (Iter       : Gtk_Text_Iter;
          Acc_Values : access Gtk.Text_Attributes.Gtk_Text_Attributes)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_get_attributes");
      Acc_Values     : aliased Gtk.Text_Attributes.Gtk_Text_Attributes;
      Tmp_Acc_Values : aliased Gtk.Text_Attributes.Gtk_Text_Attributes;
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Iter, Tmp_Acc_Values'Access);
      Acc_Values := Tmp_Acc_Values;
      Values.all := Acc_Values;
      return Tmp_Return /= 0;
   end Get_Attributes;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
      (Iter : Gtk_Text_Iter) return Pango.Language.Pango_Language
   is
      function Internal (Iter : Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_language");
   begin
      return From_Object (Internal (Iter));
   end Get_Language;

   ---------------
   -- Get_Marks --
   ---------------

   function Get_Marks
      (Iter : Gtk_Text_Iter) return Glib.Object.Object_List.GSlist
   is
      function Internal (Iter : Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_marks");
      Tmp_Return : Glib.Object.Object_List.GSlist;
   begin
      Glib.Object.Object_List.Set_Object (Tmp_Return, Internal (Iter));
      return Tmp_Return;
   end Get_Marks;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf (Iter : Gtk_Text_Iter) return Gdk.Pixbuf.Gdk_Pixbuf is
      function Internal (Iter : Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_pixbuf");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Iter), Stub_Gdk_Pixbuf));
   end Get_Pixbuf;

   ---------------
   -- Get_Slice --
   ---------------

   function Get_Slice
      (Iter    : Gtk_Text_Iter;
       The_End : Gtk_Text_Iter) return UTF8_String
   is
      function Internal
         (Iter    : Gtk_Text_Iter;
          The_End : Gtk_Text_Iter) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_slice");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Iter, The_End));
   end Get_Slice;

   --------------
   -- Get_Tags --
   --------------

   function Get_Tags
      (Iter : Gtk_Text_Iter) return Gtk.Text_Tag.Text_Tag_List.GSlist
   is
      function Internal (Iter : Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_tags");
      Tmp_Return : Gtk.Text_Tag.Text_Tag_List.GSlist;
   begin
      Gtk.Text_Tag.Text_Tag_List.Set_Object (Tmp_Return, Internal (Iter));
      return Tmp_Return;
   end Get_Tags;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Iter    : Gtk_Text_Iter;
       The_End : Gtk_Text_Iter) return UTF8_String
   is
      function Internal
         (Iter    : Gtk_Text_Iter;
          The_End : Gtk_Text_Iter) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Iter, The_End));
   end Get_Text;

   ----------------------
   -- Get_Toggled_Tags --
   ----------------------

   function Get_Toggled_Tags
      (Iter       : Gtk_Text_Iter;
       Toggled_On : Boolean) return Gtk.Text_Tag.Text_Tag_List.GSlist
   is
      function Internal
         (Iter       : Gtk_Text_Iter;
          Toggled_On : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_toggled_tags");
      Tmp_Return : Gtk.Text_Tag.Text_Tag_List.GSlist;
   begin
      Gtk.Text_Tag.Text_Tag_List.Set_Object (Tmp_Return, Internal (Iter, Boolean'Pos (Toggled_On)));
      return Tmp_Return;
   end Get_Toggled_Tags;

   -----------------------
   -- Get_Visible_Slice --
   -----------------------

   function Get_Visible_Slice
      (Iter    : Gtk_Text_Iter;
       The_End : Gtk_Text_Iter) return UTF8_String
   is
      function Internal
         (Iter    : Gtk_Text_Iter;
          The_End : Gtk_Text_Iter) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_visible_slice");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Iter, The_End));
   end Get_Visible_Slice;

   ----------------------
   -- Get_Visible_Text --
   ----------------------

   function Get_Visible_Text
      (Iter    : Gtk_Text_Iter;
       The_End : Gtk_Text_Iter) return UTF8_String
   is
      function Internal
         (Iter    : Gtk_Text_Iter;
          The_End : Gtk_Text_Iter) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_visible_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Iter, The_End));
   end Get_Visible_Text;

   -------------
   -- Has_Tag --
   -------------

   function Has_Tag
      (Iter : Gtk_Text_Iter;
       Tag  : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class)
       return Boolean
   is
      function Internal
         (Iter : Gtk_Text_Iter;
          Tag  : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_has_tag");
   begin
      return Internal (Iter, Get_Object (Tag)) /= 0;
   end Has_Tag;

   --------------
   -- In_Range --
   --------------

   function In_Range
      (Iter    : Gtk_Text_Iter;
       Start   : Gtk_Text_Iter;
       The_End : Gtk_Text_Iter) return Boolean
   is
      function Internal
         (Iter    : Gtk_Text_Iter;
          Start   : Gtk_Text_Iter;
          The_End : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_in_range");
   begin
      return Internal (Iter, Start, The_End) /= 0;
   end In_Range;

   ---------------------
   -- Inside_Sentence --
   ---------------------

   function Inside_Sentence (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_inside_sentence");
   begin
      return Internal (Iter) /= 0;
   end Inside_Sentence;

   -----------------
   -- Inside_Word --
   -----------------

   function Inside_Word (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_inside_word");
   begin
      return Internal (Iter) /= 0;
   end Inside_Word;

   ------------------------
   -- Is_Cursor_Position --
   ------------------------

   function Is_Cursor_Position (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_is_cursor_position");
   begin
      return Internal (Iter) /= 0;
   end Is_Cursor_Position;

   ------------
   -- Is_End --
   ------------

   function Is_End (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_is_end");
   begin
      return Internal (Iter) /= 0;
   end Is_End;

   --------------
   -- Is_Start --
   --------------

   function Is_Start (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_is_start");
   begin
      return Internal (Iter) /= 0;
   end Is_Start;

   -----------------
   -- Starts_Line --
   -----------------

   function Starts_Line (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_starts_line");
   begin
      return Internal (Iter) /= 0;
   end Starts_Line;

   ---------------------
   -- Starts_Sentence --
   ---------------------

   function Starts_Sentence (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_starts_sentence");
   begin
      return Internal (Iter) /= 0;
   end Starts_Sentence;

   ----------------
   -- Starts_Tag --
   ----------------

   function Starts_Tag
      (Iter : Gtk_Text_Iter;
       Tag  : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class) return Boolean
   is
      function Internal
         (Iter : Gtk_Text_Iter;
          Tag  : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_starts_tag");
   begin
      return Internal (Iter, Get_Object_Or_Null (GObject (Tag))) /= 0;
   end Starts_Tag;

   -----------------
   -- Starts_Word --
   -----------------

   function Starts_Word (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_starts_word");
   begin
      return Internal (Iter) /= 0;
   end Starts_Word;

   -----------------
   -- Toggles_Tag --
   -----------------

   function Toggles_Tag
      (Iter : Gtk_Text_Iter;
       Tag  : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class) return Boolean
   is
      function Internal
         (Iter : Gtk_Text_Iter;
          Tag  : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_toggles_tag");
   begin
      return Internal (Iter, Get_Object_Or_Null (GObject (Tag))) /= 0;
   end Toggles_Tag;

end Gtk.Text_Iter;
