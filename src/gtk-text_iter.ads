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

--  with Gdk.Pixbuf;
--  ??? commented out for the moment, as gdk-pixbuf port to gtk2 not done yet
with Gtk.Text_Child;
with Gtk.Text_Tag;
with System;

package Gtk.Text_Iter is

   type Gtk_Text_Iter is limited private;

   --  | function Get_Buffer (Iter   : access Gtk_Text_Iter)
   --  |                      return Gtk.Text_Buffer.Gtk_Text_Buffer;
   --  This function is bound in Gtk.Text_Buffer to avoid a circular
   --  dependency problem.
   --  ??? Remember to bind this function in Gtk.Text_Buffer.

   --  | GtkTextIter *gtk_text_iter_copy     (const GtkTextIter *iter);
   --  | void         gtk_text_iter_free     (GtkTextIter       *iter);
   --  These 2 functions do not need to be bound, they are only needed
   --  by certain bindings. In our toolkit, we use direct assignment.
   --  The deallocation is automatically performed as soons as a
   --  Gtk_Text_Iter goes out of scope.

   -----------------------------------------
   -- Convert to different kinds of index --
   -----------------------------------------

   function Get_Offset (Iter : Gtk_Text_Iter) return Gint;

   function Get_Line (Iter : Gtk_Text_Iter) return Gint;

   function Get_Line_Offset (Iter : Gtk_Text_Iter) return Gint;

   function Get_Line_Index (Iter : Gtk_Text_Iter) return Gint;

   ---------------------------
   -- Dereference operators --
   ---------------------------

   --  function Get_Char
   --    (Iter   : access Gtk_Text_Iter)
   --     return gunichar;
   --  ??? gunichar (and maybe all unicode.h) needs to be bound
   --  ??? first, before this function can be bound.

   function Get_Slice
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter)
      return String;
   --  ??? There is probably going to be a memory leak here.

   function Get_Text
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter)
      return String;
   --  ??? There is probably going to be a memory leak here.

   function Get_Visible_Slice
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter)
      return String;
   --  ??? There is probably going to be a memory leak here.

   function Get_Visible_Text
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter)
      return String;
   --  ??? There is probably going to be a memory leak here.

   --  function Get_Pixbuf
   --    (Iter : Gtk_Text_Iter)
   --     return Gdk.Pixbuf.Gdk_Pixbuf;
   --  ??? Commented out as gdk-pixbuf port to gtk2 not done yet.

   --  function Get_Marks
   --    (Iter   : access Gtk_Text_Iter)
   --     return GSList;
   --  ??? Need a GSList of Marks to bind this function

   function Get_Child_Anchor
     (Iter : Gtk_Text_Iter)
      return Gtk.Text_Child.Gtk_Text_Child_Anchor;

   --  function Get_Toggled_Tags
   --    (Iter       : access Gtk_Text_Iter;
   --     Toggled_On : Boolean)
   --     return GSList;
   --  ??? Need a GSList of Tags to bind this function

   function Begins_Tag
     (Iter : Gtk_Text_Iter;
      Tag  : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class)
      return Boolean;

   function Ends_Tag
     (Iter : Gtk_Text_Iter;
      Tag  : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class)
      return Boolean;

   function Toggles_Tag
     (Iter : Gtk_Text_Iter;
      Tag  : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class)
      return Boolean;

   function Has_Tag
     (Iter : Gtk_Text_Iter;
      Tag  : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class)
      return Boolean;

   --  function Get_Tags (Iter : Gtk_Text_Iter)
   --                     return GSList;
   --  ??? Need a GSLists of Tags to bind this function

   function Editable
     (Iter            : Gtk_Text_Iter;
      Default_Setting : Boolean := True)
      return Boolean;

   function Starts_Word (Iter : Gtk_Text_Iter) return Boolean;

   function Ends_Word (Iter : Gtk_Text_Iter) return Boolean;

   function Inside_Word (Iter : Gtk_Text_Iter) return Boolean;

   function Starts_Sentence (Iter : Gtk_Text_Iter) return Boolean;

   function Ends_Sentence (Iter : Gtk_Text_Iter) return Boolean;

   function Inside_Sentence (Iter : Gtk_Text_Iter) return Boolean;

   function Starts_Line (Iter : Gtk_Text_Iter) return Boolean;

   function Ends_Line (Iter : Gtk_Text_Iter) return Boolean;

   function Is_Cursor_Position (Iter : Gtk_Text_Iter) return Boolean;

   function Get_Chars_In_Line (Iter : Gtk_Text_Iter) return Gint;

   function Get_Bytes_In_Line (Iter : Gtk_Text_Iter) return Gint;

   --  function Get_Attributes
   --    (Iter   : Gtk_Text_Iter;
   --     Values : access Gtk.Text_Attributes.Gtk_Text_Attributes_Record'Class)
   --     return Boolean;
   --  ??? Gtk_Text_Attributes is defined in gtktexttag.h

   function Get_Language (Iter : Gtk_Text_Iter) return String;
   --  ??? There will probably be a memory leak here.

   function Is_End (Iter : Gtk_Text_Iter) return Boolean;

   function Is_First (Iter : Gtk_Text_Iter) return Boolean;

   ------------------------------
   -- Moving around the buffer --
   ------------------------------

   procedure Forward_Char
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean);

   procedure Backward_Char
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean);

   procedure Forward_Chars
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean);

   procedure Backward_Chars
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean);

   procedure Forward_Line
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean);

   procedure Backward_Line
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean);

   procedure Forward_Lines
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean);

   procedure Backward_Lines
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean);

   procedure Forward_Word_End
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean);

   procedure Backward_Word_Start
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean);

   procedure Forward_Word_Ends
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean);

   procedure Backward_Word_Starts
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean);

   procedure Forward_Sentence_End
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean);

   procedure Backward_Sentence_Start
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean);

   procedure Forward_Sentence_Ends
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean);

   procedure Backward_Sentence_Starts
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean);

   procedure Forward_Cursor_Position
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean);

   procedure Backward_Cursor_Position
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean);

   procedure Forward_Cursor_Positions
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean);

   procedure Backward_Cursor_Positions
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean);

   procedure Set_Offset
     (Iter        : in out Gtk_Text_Iter;
      Char_Offset : Gint);

   procedure Set_Line
     (Iter        : in out Gtk_Text_Iter;
      Line_Number : Gint);

   procedure Set_Line_Offset
     (Iter         : in out Gtk_Text_Iter;
      Char_On_Line : Gint);

   procedure Set_Line_Index
     (Iter         : in out Gtk_Text_Iter;
      Byte_On_Line : Gint);

   procedure Forward_To_End
     (Iter : in out Gtk_Text_Iter);

   procedure Forward_To_Line_End
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean);

   procedure Forward_To_Tag_Toggle
     (Iter   : in out Gtk_Text_Iter;
      Tag    : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
      Result : out Boolean);

   procedure Backward_To_Tag_Toggle
     (Iter   : in out Gtk_Text_Iter;
      Tag    : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
      Result : out Boolean);

   --  function Forward_Find_Char
   --    (Iter      : access Gtk_Text_Iter;
   --     Pred      : Gtk_Text_Char_Predicate;
   --     User_Data : gpointer;
   --     Limit     : access Gtk_Text_Iter)
   --     return Boolean;
   --  ??? Need to be put in a generic package...
   --  ??? And also needs a binding to gunichar

   --  function Backward_Find_Char
   --    (Iter      : access Gtk_Text_Iter;
   --     Pred      : Gtk_Text_Char_Predicate;
   --     User_Data : gpointer;
   --     Limit     : access Gtk_Text_Iter)
   --     return Boolean;
   --  ??? Need to be put in a generic package.
   --  ??? And also needs a binding to gunichar.

   procedure Forward_Search
     (Iter         : Gtk_Text_Iter;
      Str          : String;
      Visible_Only : Boolean := False;
      Slice        : Boolean;
      Match_Start  : out Gtk_Text_Iter;
      Match_End    : out Gtk_Text_Iter;
      Limit        : Gtk_Text_Iter;
      Result       : out Boolean);

   procedure Backward_Search
     (Iter         : Gtk_Text_Iter;
      Str          : String;
      Visible_Only : Boolean := False;
      Slice        : Boolean;
      Match_Start  : out Gtk_Text_Iter;
      Match_End    : out Gtk_Text_Iter;
      Limit        : Gtk_Text_Iter;
      Result       : out Boolean);

   -----------------
   -- Comparisons --
   -----------------

   function Equal
     (Lhs : Gtk_Text_Iter;
      Rhs : Gtk_Text_Iter)
      return Boolean;

   function Compare
     (Lhs : Gtk_Text_Iter;
      Rhs : Gtk_Text_Iter)
      return Gint;

   function In_Range
     (Iter    : Gtk_Text_Iter;
      Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter)
      return Boolean;

   procedure Reorder
     (First  : in out Gtk_Text_Iter;
      Second : in out Gtk_Text_Iter);

   -----------
   -- Debug --
   -----------

   procedure Spew
     (Iter : Gtk_Text_Iter;
      Desc : String);

private

   type Gtk_Text_Iter is limited record
      Dummy1  : System.Address;
      Dummy2  : System.Address;
      Dummy3  : Gint;
      Dummy4  : Gint;
      Dummy5  : Gint;
      Dummy6  : Gint;
      Dummy7  : Gint;
      Dummy8  : Gint;
      Dummy9  : System.Address;
      Dummy10 : System.Address;
      Dummy11 : Gint;
      Dummy12 : Gint;
   end record;
   pragma Convention (C, Gtk_Text_Iter);
   --  Note that part of the implementation of this package assumes that this
   --  type is a limited record. If for some reason this can no longer remain
   --  the case, then it needs to be modified. (See note (2) at the beginning
   --  of the body of this package).
   --  Similarly, part of the implementation of the following packages depend
   --  on this assumption:
   --    - Gtk.Text_View

   pragma Import (C, Get_Offset, "gtk_text_iter_get_offset");
   pragma Import (C, Get_Line, "gtk_text_iter_get_line");
   pragma Import (C, Get_Line_Offset, "gtk_text_iter_get_line_offset");
   pragma Import (C, Get_Line_Index, "gtk_text_iter_get_line_index");
   pragma Import (C, Get_Chars_In_Line, "gtk_text_iter_get_chars_in_line");
   pragma Import (C, Get_Bytes_In_Line, "gtk_text_iter_get_bytes_in_line");
   pragma Import (C, Forward_To_End, "gtk_text_iter_forward_to_end");
   pragma Import (C, Compare, "gtk_text_iter_compare");
   pragma Import (C, Reorder, "gtk_text_iter_reorder");

end Gtk.Text_Iter;
