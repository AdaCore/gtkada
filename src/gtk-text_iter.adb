-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

package body Gtk.Text_Iter is

   --  Note (1): In the subprograms imported from C, the most usual declaration
   --  for a GtkTextIter parameter would be "in System.Address". However, in
   --  order to avoid unnecessary uses of the 'Address attribute, they are
   --  directly declared as "in Gtk_Text_Iter". The "Import (C," pragma then
   --  ensures that the structure is passed by reference, as expected at the
   --  gtk+ level.
   --
   --  Also note that this method also applies to cases where the iterator is
   --  an "in out" parameter at the GtkAda level. Even if declared as an "in"
   --  parameter at the imported C function level, the fact that the
   --  GtkTextIter parameter is passed by reference (see above) ensures that
   --  the "in out" semantics is respected, despite "in" mode in the profile of
   --  the imported function.
   --
   --  Note (2): On the other hand, (1) is not appropriate if the iterator
   --  at the GtkAda level is an "out" parameter: the compiler would generate
   --  an unitialized parameter warning. In these rare cases, the
   --  address of the parameter is passed by using the 'Address attribute.
   --  The portability of this construct is ensured by the fact that the
   --  Gtk_Text_Iter type is a limited record.
   --
   --  If for some reason Gtk_Text_Iter can not remain limited, taking the
   --  'Address of the parameter should not be done anymore. In that case,
   --  the proper method is to use the 'Address of a local Gtk_Text_Iter.

   use Gtk.Text_Tag;

   package Iter_Access_Address_Conversions is
     new System.Address_To_Access_Conversions (Gtk_Text_Iter);

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Source : Gtk_Text_Iter;
      Dest   : out Gtk_Text_Iter)
   is
      procedure Internal
        (Source : Gtk_Text_Iter;
         Dest   : System.Address);
      pragma Import (C, Internal, "ada_text_iter_copy");
   begin
      Internal (Source, Dest'Address);
   end Copy;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (Iter : Gtk_Text_Iter) return Gunichar
   is
      function Internal (Iter : Gtk_Text_Iter) return Gunichar;
      pragma Import (C, Internal, "gtk_text_iter_get_char");
      --  Note that the Get_Char function could have been directly imported
      --  from C, rather than going through this Internal function. This
      --  solution was prefered for cosmetic reasons: Having several different
      --  Get_Char functions, the "Import C" pragma would need to be located
      --  close to the function declaration, which we would like to avoid.
   begin
      return Internal (Iter);
   end Get_Char;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (Iter : Gtk_Text_Iter) return Character is
      Result : constant Gunichar := Get_Char (Iter);
      Eight_LSB_Mask : constant := 2#1111_1111#;
   begin
      --  This function relies on the Get_Char function provided by gtk+,
      --  which returns a gunichar value. Only the 8 least significant bits
      --  are then kept to deduce the associated character.
      return Character'Val (Result and Eight_LSB_Mask);
   end Get_Char;

   ---------------
   -- Get_Slice --
   ---------------

   function Get_Slice
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter) return String
   is
      function Internal
        (Start   : Gtk_Text_Iter;
         The_End : Gtk_Text_Iter) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_slice");

      Str : Interfaces.C.Strings.chars_ptr := Internal (Start, The_End);

   begin
      declare
         S : constant String := Interfaces.C.Strings.Value (Str);
      begin
         Interfaces.C.Strings.Free (Str);
         return S;
      end;
   end Get_Slice;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter) return String
   is
      function Internal
        (Start   : Gtk_Text_Iter;
         The_End : Gtk_Text_Iter)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_text");

      Str : Interfaces.C.Strings.chars_ptr := Internal (Start, The_End);

   begin
      declare
         S : constant String := Interfaces.C.Strings.Value (Str);
      begin
         Interfaces.C.Strings.Free (Str);
         return S;
      end;
   end Get_Text;

   -----------------------
   -- Get_Visible_Slice --
   -----------------------

   function Get_Visible_Slice
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter) return String
   is
      function Internal
        (Start   : Gtk_Text_Iter;
         The_End : Gtk_Text_Iter) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_visible_slice");

      Str : Interfaces.C.Strings.chars_ptr := Internal (Start, The_End);

   begin
      declare
         S : constant String := Interfaces.C.Strings.Value (Str);
      begin
         Interfaces.C.Strings.Free (Str);
         return S;
      end;
   end Get_Visible_Slice;

   ----------------------
   -- Get_Visible_Text --
   ----------------------

   function Get_Visible_Text
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter) return String
   is
      function Internal
        (Start   : Gtk_Text_Iter;
         The_End : Gtk_Text_Iter) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_visible_text");

      Str : Interfaces.C.Strings.chars_ptr := Internal (Start, The_End);

   begin
      declare
         S : constant String := Interfaces.C.Strings.Value (Str);
      begin
         Interfaces.C.Strings.Free (Str);
         return S;
      end;
   end Get_Visible_Text;

   ---------------
   -- Get_Marks --
   ---------------

   --  function Get_Marks (Iter : access Gtk_Text_Iter) return GSList is
   --     function Internal (Iter : System.Address) return System.Address;
   --     pragma Import (C, Internal, "gtk_text_iter_get_marks");
   --  begin
   --     return Internal (Iter);
   --  end Get_Marks;

   ----------------------
   -- Get_Child_Anchor --
   ----------------------

   function Get_Child_Anchor
      (Iter : Gtk_Text_Iter)
       return Gtk.Text_Child.Gtk_Text_Child_Anchor
   is
      function Internal (Iter : Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_child_anchor");

      Stub : Gtk.Text_Child.Gtk_Text_Child_Anchor_Record;

   begin
      return Gtk.Text_Child.Gtk_Text_Child_Anchor
               (Get_User_Data (Internal (Iter), Stub));
   end Get_Child_Anchor;

   ----------------------
   -- Get_Toggled_Tags --
   ----------------------

   --  function Get_Toggled_Tags
   --    (Iter       : access Gtk_Text_Iter;
   --     Toggled_On : Boolean)
   --     return GSList
   --  is
   --     function Internal
   --       (Iter       : System.Address;
   --        Toggled_On : Gboolean)
   --        return System.Address;
   --     pragma Import (C, Internal, "gtk_text_iter_get_toggled_tags");
   --  begin
   --     return Internal (Iter, Boolean'Pos (Toggled_On));
   --  end Get_Toggled_Tags;

   ----------------
   -- Begins_Tag --
   ----------------

   function Begins_Tag
     (Iter : Gtk_Text_Iter;
      Tag  : Gtk.Text_Tag.Gtk_Text_Tag := null) return Boolean
   is
      function Internal
        (Iter : Gtk_Text_Iter; Tag : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_begins_tag");

   begin
      if Tag = null then
         return To_Boolean (Internal (Iter, System.Null_Address));
      else
         return To_Boolean (Internal (Iter, Get_Object (Tag)));
      end if;
   end Begins_Tag;

   --------------
   -- Ends_Tag --
   --------------

   function Ends_Tag
     (Iter   : Gtk_Text_Iter;
      Tag    : Gtk.Text_Tag.Gtk_Text_Tag := null) return Boolean
   is
      function Internal
        (Iter : Gtk_Text_Iter; Tag : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_ends_tag");

   begin
      if Tag = null then
         return To_Boolean (Internal (Iter, System.Null_Address));
      else
         return To_Boolean (Internal (Iter, Get_Object (Tag)));
      end if;
   end Ends_Tag;

   -----------------
   -- Toggles_Tag --
   -----------------

   function Toggles_Tag
     (Iter : Gtk_Text_Iter;
      Tag  : Gtk.Text_Tag.Gtk_Text_Tag := null) return Boolean
   is
      function Internal
        (Iter : Gtk_Text_Iter;
         Tag  : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_toggles_tag");

   begin
      if Tag = null then
         return To_Boolean (Internal (Iter, System.Null_Address));
      else
         return To_Boolean (Internal (Iter, Get_Object (Tag)));
      end if;
   end Toggles_Tag;

   -------------
   -- Has_Tag --
   -------------

   function Has_Tag
     (Iter : Gtk_Text_Iter;
      Tag  : Gtk.Text_Tag.Gtk_Text_Tag := null) return Boolean
   is
      function Internal
        (Iter : Gtk_Text_Iter; Tag : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_has_tag");

   begin
      if Tag = null then
         return To_Boolean (Internal (Iter, System.Null_Address));
      else
         return To_Boolean (Internal (Iter, Get_Object (Tag)));
      end if;
   end Has_Tag;

   --------------
   -- Get_Tags --
   --------------

   --  function Get_Tags (Iter : access Gtk_Text_Iter) return GSList is
   --     function Internal (Iter : System.Address) return System.Address;
   --     pragma Import (C, Internal, "gtk_text_iter_get_tags");
   --  begin
   --     return Internal (Iter);
   --  end Get_Tags;

   --------------
   -- Editable --
   --------------

   function Editable
     (Iter            : Gtk_Text_Iter;
      Default_Setting : Boolean := True) return Boolean
   is
      function Internal
        (Iter            : Gtk_Text_Iter;
         Default_Setting : Gboolean) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_editable");

   begin
      return To_Boolean (Internal (Iter, To_Gboolean (Default_Setting)));
   end Editable;

   -----------------
   -- Starts_Word --
   -----------------

   function Starts_Word (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_starts_word");

   begin
      return To_Boolean (Internal (Iter));
   end Starts_Word;

   ---------------
   -- Ends_Word --
   ---------------

   function Ends_Word (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_ends_word");

   begin
      return To_Boolean (Internal (Iter));
   end Ends_Word;

   -----------------
   -- Inside_Word --
   -----------------

   function Inside_Word (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_inside_word");

   begin
      return To_Boolean (Internal (Iter));
   end Inside_Word;

   ---------------------
   -- Starts_Sentence --
   ---------------------

   function Starts_Sentence (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_starts_sentence");

   begin
      return To_Boolean (Internal (Iter));
   end Starts_Sentence;

   -------------------
   -- Ends_Sentence --
   -------------------

   function Ends_Sentence (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_ends_sentence");

   begin
      return To_Boolean (Internal (Iter));
   end Ends_Sentence;

   ---------------------
   -- Inside_Sentence --
   ---------------------

   function Inside_Sentence (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_inside_sentence");

   begin
      return To_Boolean (Internal (Iter));
   end Inside_Sentence;

   -----------------
   -- Starts_Line --
   -----------------

   function Starts_Line (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_starts_line");

   begin
      return To_Boolean (Internal (Iter));
   end Starts_Line;

   ---------------
   -- Ends_Line --
   ---------------

   function Ends_Line (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_ends_line");

   begin
      return To_Boolean (Internal (Iter));
   end Ends_Line;

   ------------------------
   -- Is_Cursor_Position --
   ------------------------

   function Is_Cursor_Position (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_is_cursor_position");

   begin
      return To_Boolean (Internal (Iter));
   end Is_Cursor_Position;

   --------------------
   -- Get_Attributes --
   --------------------

   --  function Get_Attributes
   --    (Iter   : access Gtk_Text_Iter;
   --     Values : access Gtk.Text_Attributes.Gtk_Text_Attributes_Record'Class)
   --     return Boolean
   --  is
   --     function Internal
   --       (Iter   : System.Address;
   --        Values : System.Address)
   --        return Gboolean;
   --     pragma Import (C, Internal, "gtk_text_iter_get_attributes");
   --  begin
   --     return To_Boolean (Internal (Iter, Get_Object (Values)));
   --  end Get_Attributes;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language (Iter : Gtk_Text_Iter) return String is
      function Internal
        (Iter : Gtk_Text_Iter) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_language");

      Str : Interfaces.C.Strings.chars_ptr := Internal (Iter);

   begin
      declare
         S : constant String := Interfaces.C.Strings.Value (Str);
      begin
         Interfaces.C.Strings.Free (Str);
         return S;
      end;
   end Get_Language;

   ------------
   -- Is_End --
   ------------

   function Is_End (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_is_end");

   begin
      return To_Boolean (Internal (Iter));
   end Is_End;

   --------------
   -- Is_Start --
   --------------

   function Is_Start (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_is_start");

   begin
      return To_Boolean (Internal (Iter));
   end Is_Start;

   ------------------
   -- Forward_Char --
   ------------------

   procedure Forward_Char
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_char");

   begin
      Result := To_Boolean (Internal (Iter));
   end Forward_Char;

   -------------------
   -- Backward_Char --
   -------------------

   procedure Backward_Char
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_char");

   begin
      Result := To_Boolean (Internal (Iter));
   end Backward_Char;

   -------------------
   -- Forward_Chars --
   -------------------

   procedure Forward_Chars
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_chars");
   begin
      Result := To_Boolean (Internal (Iter, Count));
   end Forward_Chars;

   --------------------
   -- Backward_Chars --
   --------------------

   procedure Backward_Chars
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_chars");
   begin
      Result := To_Boolean (Internal (Iter, Count));
   end Backward_Chars;

   ------------------
   -- Forward_Line --
   ------------------

   procedure Forward_Line
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_line");
   begin
      Result := To_Boolean (Internal (Iter));
   end Forward_Line;

   -------------------
   -- Backward_Line --
   -------------------

   procedure Backward_Line
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_line");
   begin
      Result := To_Boolean (Internal (Iter));
   end Backward_Line;

   -------------------
   -- Forward_Lines --
   -------------------

   procedure Forward_Lines
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_lines");
   begin
      Result := To_Boolean (Internal (Iter, Count));
   end Forward_Lines;

   --------------------
   -- Backward_Lines --
   --------------------

   procedure Backward_Lines
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_lines");
   begin
      Result := To_Boolean (Internal (Iter, Count));
   end Backward_Lines;

   ----------------------
   -- Forward_Word_End --
   ----------------------

   procedure Forward_Word_End
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_word_end");
   begin
      Result := To_Boolean (Internal (Iter));
   end Forward_Word_End;

   -------------------------
   -- Backward_Word_Start --
   -------------------------

   procedure Backward_Word_Start
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_word_start");
   begin
      Result := To_Boolean (Internal (Iter));
   end Backward_Word_Start;

   -----------------------
   -- Forward_Word_Ends --
   -----------------------

   procedure Forward_Word_Ends
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_word_ends");
   begin
      Result := To_Boolean (Internal (Iter, Count));
   end Forward_Word_Ends;

   --------------------------
   -- Backward_Word_Starts --
   --------------------------

   procedure Backward_Word_Starts
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_word_starts");
   begin
      Result := To_Boolean (Internal (Iter, Count));
   end Backward_Word_Starts;

   --------------------------
   -- Forward_Sentence_End --
   --------------------------

   procedure Forward_Sentence_End
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_sentence_end");
   begin
      Result := To_Boolean (Internal (Iter));
   end Forward_Sentence_End;

   -----------------------------
   -- Backward_Sentence_Start --
   -----------------------------

   procedure Backward_Sentence_Start
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_sentence_start");
   begin
      Result := To_Boolean (Internal (Iter));
   end Backward_Sentence_Start;

   ---------------------------
   -- Forward_Sentence_Ends --
   ---------------------------

   procedure Forward_Sentence_Ends
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_sentence_ends");
   begin
      Result := To_Boolean (Internal (Iter, Count));
   end Forward_Sentence_Ends;

   ------------------------------
   -- Backward_Sentence_Starts --
   ------------------------------

   procedure Backward_Sentence_Starts
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_sentence_starts");
   begin
      Result := To_Boolean (Internal (Iter, Count));
   end Backward_Sentence_Starts;

   -----------------------------
   -- Forward_Cursor_Position --
   -----------------------------

   procedure Forward_Cursor_Position
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_cursor_position");
   begin
      Result := To_Boolean (Internal (Iter));
   end Forward_Cursor_Position;

   ------------------------------
   -- Backward_Cursor_Position --
   ------------------------------

   procedure Backward_Cursor_Position
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_cursor_position");
   begin
      Result := To_Boolean (Internal (Iter));
   end Backward_Cursor_Position;

   ------------------------------
   -- Forward_Cursor_Positions --
   ------------------------------

   procedure Forward_Cursor_Positions
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_cursor_positions");
   begin
      Result := To_Boolean (Internal (Iter, Count));
   end Forward_Cursor_Positions;

   -------------------------------
   -- Backward_Cursor_Positions --
   -------------------------------

   procedure Backward_Cursor_Positions
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_cursor_positions");
   begin
      Result := To_Boolean (Internal (Iter, Count));
   end Backward_Cursor_Positions;

   ----------------
   -- Set_Offset --
   ----------------

   procedure Set_Offset
     (Iter        : in out Gtk_Text_Iter;
      Char_Offset : Gint)
   is
      procedure Internal
        (Iter        : Gtk_Text_Iter;
         Char_Offset : Gint);
      pragma Import (C, Internal, "gtk_text_iter_set_offset");
   begin
      Internal (Iter, Char_Offset);
   end Set_Offset;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (Iter        : in out Gtk_Text_Iter;
      Line_Number : Gint)
   is
      procedure Internal
        (Iter        : Gtk_Text_Iter;
         Line_Number : Gint);
      pragma Import (C, Internal, "gtk_text_iter_set_line");
   begin
      Internal (Iter, Line_Number);
   end Set_Line;

   ---------------------
   -- Set_Line_Offset --
   ---------------------

   procedure Set_Line_Offset
     (Iter         : in out Gtk_Text_Iter;
      Char_On_Line : Gint)
   is
      procedure Internal
        (Iter         : Gtk_Text_Iter;
         Char_On_Line : Gint);
      pragma Import (C, Internal, "gtk_text_iter_set_line_offset");
   begin
      Internal (Iter, Char_On_Line);
   end Set_Line_Offset;

   --------------------
   -- Set_Line_Index --
   --------------------

   procedure Set_Line_Index
     (Iter         : in out Gtk_Text_Iter;
      Byte_On_Line : Gint)
   is
      procedure Internal
        (Iter         : Gtk_Text_Iter;
         Byte_On_Line : Gint);
      pragma Import (C, Internal, "gtk_text_iter_set_line_index");
   begin
      Internal (Iter, Byte_On_Line);
   end Set_Line_Index;

   -------------------------
   -- Forward_To_Line_End --
   -------------------------

   procedure Forward_To_Line_End
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_to_line_end");
   begin
      Result := To_Boolean (Internal (Iter));
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
        (Iter : Gtk_Text_Iter; Tag : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_to_tag_toggle");
   begin
      if Tag = null then
         Result := To_Boolean (Internal (Iter, System.Null_Address));
      else
         Result := To_Boolean (Internal (Iter, Get_Object (Tag)));
      end if;
   end Forward_To_Tag_Toggle;

   ----------------------------
   -- Backward_To_Tag_Toggle --
   ----------------------------

   procedure Backward_To_Tag_Toggle
     (Iter   : in out Gtk_Text_Iter;
      Tag    : Gtk.Text_Tag.Gtk_Text_Tag := null;
      Result : out Boolean)
   is
      function Internal
        (Iter : Gtk_Text_Iter; Tag : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_to_tag_toggle");

   begin
      if Tag = null then
         Result := To_Boolean (Internal (Iter, System.Null_Address));
      else
         Result := To_Boolean (Internal (Iter, Get_Object (Tag)));
      end if;
   end Backward_To_Tag_Toggle;

   -----------------------
   -- Forward_Find_Char --
   -----------------------

   --  function Forward_Find_Char
   --    (Iter      : access Gtk_Text_Iter;
   --     Pred      : Gtk_Text_Char_Predicate;
   --     User_Data : gpointer;
   --     Limit     : access Gtk_Text_Iter)
   --     return Boolean
   --  is
   --     function Internal
   --       (Iter      : System.Address;
   --        Pred      : Gint;
   --        User_Data : Integer;
   --        Limit     : System.Address)
   --        return Gboolean;
   --     pragma Import (C, Internal, "gtk_text_iter_forward_find_char");
   --  begin
   --     return To_Boolean (Internal (Iter,
   --                                  Gtk_Text_Char_Predicate'Pos (Pred),
   --                                  User_Data,
   --                                  Limit));
   --  end Forward_Find_Char;

   ------------------------
   -- Backward_Find_Char --
   ------------------------

   --  function Backward_Find_Char
   --    (Iter      : access Gtk_Text_Iter;
   --     Pred      : Gtk_Text_Char_Predicate;
   --     User_Data : gpointer;
   --     Limit     : access Gtk_Text_Iter)
   --     return Boolean
   --  is
   --     function Internal
   --       (Iter      : System.Address;
   --        Pred      : Gint;
   --        User_Data : Integer;
   --        Limit     : System.Address)
   --        return GBoolean;
   --     pragma Import (C, Internal, "gtk_text_iter_backward_find_char");
   --  begin
   --     return To_Boolean (Internal (Iter,
   --                                  Gtk_Text_Char_Predicate'Pos (Pred),
   --                                  User_Data,
   --                                  Limit));
   --  end Backward_Find_Char;

   --------------------
   -- Forward_Search --
   --------------------

   procedure Forward_Search
     (Iter         : Gtk_Text_Iter;
      Str          : String;
      Visible_Only : Boolean := False;
      Slice        : Boolean;
      Match_Start  : out Gtk_Text_Iter;
      Match_End    : out Gtk_Text_Iter;
      Limit        : Gtk_Text_Iter;
      Result       : out Boolean)
   is
      function Internal
        (Iter         : Gtk_Text_Iter;
         Str          : String;
         Visible_Only : Gboolean;
         Slice        : Gboolean;
         Match_Start  : System.Address;
         Match_End    : System.Address;
         Limit        : Gtk_Text_Iter)
         return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_search");
   begin
      Result := To_Boolean
        (Internal
          (Iter,
           Str & ASCII.NUL,
           To_Gboolean (Visible_Only),
           To_Gboolean (Slice),
           Match_Start'Address,
           Match_End'Address,
           Limit));
   end Forward_Search;

   ---------------------
   -- Backward_Search --
   ---------------------

   procedure Backward_Search
     (Iter         : Gtk_Text_Iter;
      Str          : String;
      Visible_Only : Boolean := False;
      Slice        : Boolean;
      Match_Start  : out Gtk_Text_Iter;
      Match_End    : out Gtk_Text_Iter;
      Limit        : Gtk_Text_Iter;
      Result       : out Boolean)
   is
      function Internal
        (Iter         : Gtk_Text_Iter;
         Str          : String;
         Visible_Only : Gboolean;
         Slice        : Gboolean;
         Match_Start  : System.Address;
         Match_End    : System.Address;
         Limit        : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_search");

   begin
      Result := To_Boolean
        (Internal
          (Iter,
           Str & ASCII.NUL,
           To_Gboolean (Visible_Only),
           To_Gboolean (Slice),
           Match_Start'Address,
           Match_End'Address,
           Limit));
   end Backward_Search;

   -----------
   -- Equal --
   -----------

   function Equal (Lhs : Gtk_Text_Iter; Rhs : Gtk_Text_Iter) return Boolean is
      function Internal
        (Lhs : Gtk_Text_Iter; Rhs : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_equal");

   begin
      return To_Boolean (Internal (Lhs, Rhs));
   end Equal;

   --------------
   -- In_Range --
   --------------

   function In_Range
     (Iter    :  Gtk_Text_Iter;
      Start   :  Gtk_Text_Iter;
      The_End :  Gtk_Text_Iter) return Boolean
   is
      function Internal
        (Iter    : Gtk_Text_Iter;
         Start   : Gtk_Text_Iter;
         The_End : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_in_range");
   begin
      return To_Boolean (Internal (Iter, Start, The_End));
   end In_Range;

   -------------------
   -- Get_Text_Iter --
   -------------------

   procedure Get_Text_Iter
     (Val  : Glib.Values.GValue;
      Iter : out Gtk_Text_Iter) is
   begin
      Copy
        (Source =>  Iter_Access_Address_Conversions.To_Pointer
                      (Glib.Values.Get_Address (Val)).all,
         Dest => Iter);
   end Get_Text_Iter;

   -------------------
   -- Get_Text_Iter --
   -------------------

   function Get_Text_Iter (Val : Glib.Values.GValue) return Gtk_Text_Iter is
   begin
      return Iter_Access_Address_Conversions.To_Pointer
               (Glib.Values.Get_Address (Val)).all;
   end Get_Text_Iter;

end Gtk.Text_Iter;
