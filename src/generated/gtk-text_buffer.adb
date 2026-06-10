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
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;

package body Gtk.Text_Buffer is

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : Gtkada.Types.Chars_Ptr)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
         Text   : Gtkada.Types.Chars_Ptr;
         Len    : Gint := -1);
      pragma Import (C, Internal, "gtk_text_buffer_insert");

   begin
      Internal (Get_Object (Buffer), Iter, Text);
   end Insert;

   ----------------------
   -- Insert_At_Cursor --
   ----------------------

   procedure Insert_At_Cursor
     (Buffer : access Gtk_Text_Buffer_Record;
      Text   : Gtkada.Types.Chars_Ptr;
      Len    : Gint := -1)
   is
      procedure Internal
        (Buffer : System.Address;
         Text   : Gtkada.Types.Chars_Ptr;
         Len    : Gint);
      pragma Import (C, Internal, "gtk_text_buffer_insert_at_cursor");

   begin
      Internal (Get_Object (Buffer), Text, Len);
   end Insert_At_Cursor;

   ----------------------
   -- Insert_With_Tags --
   ----------------------

   procedure Insert_With_Tags
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : UTF8_String;
      Tag    : Gtk.Text_Tag.Gtk_Text_Tag)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
         Text   : UTF8_String;
         Len    : Gint;
         Tag    : System.Address);
      pragma Import (C, Internal, "ada_gtk_text_buffer_insert_with_tags");

   begin
      Internal
        (Get_Object (Buffer), Iter, Text, Text'Length, Get_Object (Tag));
   end Insert_With_Tags;

   procedure Insert_With_Tags
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : Gtkada.Types.Chars_Ptr;
      Tag    : Gtk.Text_Tag.Gtk_Text_Tag)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
         Text   : Gtkada.Types.Chars_Ptr;
         Len    : Gint := -1;
         Tag    : System.Address);
      pragma Import (C, Internal, "ada_gtk_text_buffer_insert_with_tags");

   begin
      Internal (Get_Object (Buffer), Iter, Text, Tag => Get_Object (Tag));
   end Insert_With_Tags;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Buffer               : access Gtk_Text_Buffer_Record;
      Start                : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
      Include_Hidden_Chars : Boolean := False) return Gtkada.Types.Chars_Ptr
   is
      function Internal
        (Buffer               : System.Address;
         Start                : Gtk.Text_Iter.Gtk_Text_Iter;
         The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
         Include_Hidden_Chars : Gboolean)
      return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_text_buffer_get_text");

   begin
      return Internal
        (Get_Object (Buffer),
         Start,
         The_End,
         Boolean'Pos (Include_Hidden_Chars));
   end Get_Text;

   ----------------------
   -- Selection_Exists --
   ----------------------

   function Selection_Exists
     (Buffer : access Gtk_Text_Buffer_Record) return Boolean
   is
      Ignored_A, Ignored_B : Gtk_Text_Iter;
      Has_Selection : Boolean;
   begin
      Get_Selection_Bounds (Buffer, Ignored_A, Ignored_B, Has_Selection);
      return Has_Selection;
   end Selection_Exists;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Iter : Gtk_Text_Iter) return Gtk.Text_Buffer.Gtk_Text_Buffer
   is
      function Internal (Iter : Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_buffer");
      Stub_Gtk_Text_Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer_Record;
   begin
      return Gtk.Text_Buffer.Gtk_Text_Buffer (Get_User_Data (Internal (Iter), Stub_Gtk_Text_Buffer));
   end Get_Buffer;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Mark : Gtk_Text_Mark)
   return Gtk.Text_Buffer.Gtk_Text_Buffer
   is
      function Internal (Mark : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_mark_get_buffer");
      Stub_Gtk_Text_Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer_Record;
   begin
      return Gtk.Text_Buffer.Gtk_Text_Buffer (Get_User_Data (Internal (Get_Object (Mark)), Stub_Gtk_Text_Buffer));
   end Get_Buffer;

   ----------------
   -- Create_Tag --
   ----------------

   function Create_Tag
     (Buffer              : access Gtk_Text_Buffer_Record;
      Tag_Name            : String := "")
   return Gtk_Text_Tag
   is
      function Internal
        (Buffer              : System.Address;
         Tag_Name            : Gtkada.Types.Chars_Ptr)
      return System.Address;
      pragma Import (C, Internal, "ada_gtk_text_buffer_create_tag");
      Stub : Gtk_Text_Tag_Record;
      Str  : Gtkada.Types.Chars_Ptr := String_Or_Null (Tag_Name);
      Tag  : Gtk_Text_Tag;
   begin
      Tag := Gtk_Text_Tag
        (Get_User_Data (Internal (Get_Object (Buffer), Str), Stub));
      Free (Str);
      return Tag;
   end Create_Tag;

   package Type_Conversion_Gtk_Text_Buffer is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Text_Buffer_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Text_Buffer);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Buffer : out Gtk_Text_Buffer;
       Table  : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null)
   is
   begin
      Buffer := new Gtk_Text_Buffer_Record;
      Gtk.Text_Buffer.Initialize (Buffer, Table);
   end Gtk_New;

   -------------------------
   -- Gtk_Text_Buffer_New --
   -------------------------

   function Gtk_Text_Buffer_New
      (Table : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null)
       return Gtk_Text_Buffer
   is
      Buffer : constant Gtk_Text_Buffer := new Gtk_Text_Buffer_Record;
   begin
      Gtk.Text_Buffer.Initialize (Buffer, Table);
      return Buffer;
   end Gtk_Text_Buffer_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Buffer : not null access Gtk_Text_Buffer_Record'Class;
       Table  : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null)
   is
      function Internal (Table : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_new");
   begin
      if not Buffer.Is_Created then
         Set_Object (Buffer, Internal (Get_Object_Or_Null (GObject (Table))));
      end if;
   end Initialize;

   --------------
   -- Add_Mark --
   --------------

   procedure Add_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Mark   : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
       Where  : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer : System.Address;
          Mark   : System.Address;
          Where  : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_add_mark");
   begin
      Internal (Get_Object (Buffer), Get_Object (Mark), Where);
   end Add_Mark;

   -----------------------------
   -- Add_Selection_Clipboard --
   -----------------------------

   procedure Add_Selection_Clipboard
      (Buffer    : not null access Gtk_Text_Buffer_Record;
       Clipboard : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class)
   is
      procedure Internal
         (Buffer    : System.Address;
          Clipboard : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_add_selection_clipboard");
   begin
      Internal (Get_Object (Buffer), Get_Object (Clipboard));
   end Add_Selection_Clipboard;

   ---------------
   -- Apply_Tag --
   ---------------

   procedure Apply_Tag
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Tag     : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer  : System.Address;
          Tag     : System.Address;
          Start   : Gtk.Text_Iter.Gtk_Text_Iter;
          The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_apply_tag");
   begin
      Internal (Get_Object (Buffer), Get_Object (Tag), Start, The_End);
   end Apply_Tag;

   -----------------------
   -- Apply_Tag_By_Name --
   -----------------------

   procedure Apply_Tag_By_Name
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Name    : UTF8_String;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer  : System.Address;
          Name    : Gtkada.Types.Chars_Ptr;
          Start   : Gtk.Text_Iter.Gtk_Text_Iter;
          The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_apply_tag_by_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Buffer), Tmp_Name, Start, The_End);
      Free (Tmp_Name);
   end Apply_Tag_By_Name;

   ---------------
   -- Backspace --
   ---------------

   procedure Backspace
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Iter             : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Interactive      : Boolean;
       Default_Editable : Boolean := True;
       Result           : out Boolean)
   is
      function Internal
         (Buffer           : System.Address;
          Acc_Iter         : access Gtk.Text_Iter.Gtk_Text_Iter;
          Interactive      : Glib.Gboolean;
          Default_Editable : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_backspace");
      Acc_Iter   : aliased Gtk.Text_Iter.Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Buffer), Acc_Iter'Access, Boolean'Pos (Interactive), Boolean'Pos (Default_Editable));
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Backspace;

   -------------------------------
   -- Begin_Irreversible_Action --
   -------------------------------

   procedure Begin_Irreversible_Action
      (Buffer : not null access Gtk_Text_Buffer_Record)
   is
      procedure Internal (Buffer : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_begin_irreversible_action");
   begin
      Internal (Get_Object (Buffer));
   end Begin_Irreversible_Action;

   -----------------------
   -- Begin_User_Action --
   -----------------------

   procedure Begin_User_Action
      (Buffer : not null access Gtk_Text_Buffer_Record)
   is
      procedure Internal (Buffer : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_begin_user_action");
   begin
      Internal (Get_Object (Buffer));
   end Begin_User_Action;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard
      (Buffer    : not null access Gtk_Text_Buffer_Record;
       Clipboard : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class)
   is
      procedure Internal
         (Buffer    : System.Address;
          Clipboard : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_copy_clipboard");
   begin
      Internal (Get_Object (Buffer), Get_Object (Clipboard));
   end Copy_Clipboard;

   -------------------------
   -- Create_Child_Anchor --
   -------------------------

   function Create_Child_Anchor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : Gtk.Text_Iter.Gtk_Text_Iter)
       return Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor
   is
      function Internal
         (Buffer : System.Address;
          Iter   : Gtk.Text_Iter.Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_create_child_anchor");
      Stub_Gtk_Text_Child_Anchor : Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor_Record;
   begin
      return Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor (Get_User_Data (Internal (Get_Object (Buffer), Iter), Stub_Gtk_Text_Child_Anchor));
   end Create_Child_Anchor;

   -----------------
   -- Create_Mark --
   -----------------

   function Create_Mark
      (Buffer       : not null access Gtk_Text_Buffer_Record;
       Mark_Name    : UTF8_String := "";
       Where        : Gtk.Text_Iter.Gtk_Text_Iter;
       Left_Gravity : Boolean := True) return Gtk.Text_Mark.Gtk_Text_Mark
   is
      function Internal
         (Buffer       : System.Address;
          Mark_Name    : Gtkada.Types.Chars_Ptr;
          Where        : Gtk.Text_Iter.Gtk_Text_Iter;
          Left_Gravity : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_create_mark");
      Tmp_Mark_Name      : Gtkada.Types.Chars_Ptr;
      Stub_Gtk_Text_Mark : Gtk.Text_Mark.Gtk_Text_Mark_Record;
      Tmp_Return         : System.Address;
   begin
      if Mark_Name = "" then
         Tmp_Mark_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Mark_Name := New_String (Mark_Name);
      end if;
      Tmp_Return := Internal (Get_Object (Buffer), Tmp_Mark_Name, Where, Boolean'Pos (Left_Gravity));
      Free (Tmp_Mark_Name);
      return Gtk.Text_Mark.Gtk_Text_Mark (Get_User_Data (Tmp_Return, Stub_Gtk_Text_Mark));
   end Create_Mark;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Clipboard        : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class;
       Default_Editable : Boolean := True)
   is
      procedure Internal
         (Buffer           : System.Address;
          Clipboard        : System.Address;
          Default_Editable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_text_buffer_cut_clipboard");
   begin
      Internal (Get_Object (Buffer), Get_Object (Clipboard), Boolean'Pos (Default_Editable));
   end Cut_Clipboard;

   ------------
   -- Delete --
   ------------

   procedure Delete
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Start   : in out Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : in out Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer  : System.Address;
          Start   : in out Gtk.Text_Iter.Gtk_Text_Iter;
          The_End : in out Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_delete");
      Tmp_Start   : aliased Gtk.Text_Iter.Gtk_Text_Iter := Start;
      Tmp_The_End : aliased Gtk.Text_Iter.Gtk_Text_Iter := The_End;
   begin
      Internal (Get_Object (Buffer), Tmp_Start, Tmp_The_End);
      The_End := Tmp_The_End;
      Start := Tmp_Start;
   end Delete;

   ------------------------
   -- Delete_Interactive --
   ------------------------

   procedure Delete_Interactive
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Start_Iter       : in out Gtk.Text_Iter.Gtk_Text_Iter;
       End_Iter         : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Default_Editable : Boolean := True;
       Result           : out Boolean)
   is
      function Internal
         (Buffer           : System.Address;
          Acc_Start_Iter   : access Gtk.Text_Iter.Gtk_Text_Iter;
          Acc_End_Iter     : access Gtk.Text_Iter.Gtk_Text_Iter;
          Default_Editable : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_delete_interactive");
      Acc_Start_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter := Start_Iter;
      Acc_End_Iter   : aliased Gtk.Text_Iter.Gtk_Text_Iter := End_Iter;
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Buffer), Acc_Start_Iter'Access, Acc_End_Iter'Access, Boolean'Pos (Default_Editable));
      Start_Iter := Acc_Start_Iter;
      End_Iter := Acc_End_Iter;
      Result := Tmp_Return /= 0;
   end Delete_Interactive;

   -----------------
   -- Delete_Mark --
   -----------------

   procedure Delete_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Mark   : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
   is
      procedure Internal (Buffer : System.Address; Mark : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_delete_mark");
   begin
      Internal (Get_Object (Buffer), Get_Object (Mark));
   end Delete_Mark;

   -------------------------
   -- Delete_Mark_By_Name --
   -------------------------

   procedure Delete_Mark_By_Name
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Name   : UTF8_String)
   is
      procedure Internal
         (Buffer : System.Address;
          Name   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_text_buffer_delete_mark_by_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Buffer), Tmp_Name);
      Free (Tmp_Name);
   end Delete_Mark_By_Name;

   ----------------------
   -- Delete_Selection --
   ----------------------

   function Delete_Selection
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Interactive      : Boolean;
       Default_Editable : Boolean := True) return Boolean
   is
      function Internal
         (Buffer           : System.Address;
          Interactive      : Glib.Gboolean;
          Default_Editable : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_delete_selection");
   begin
      return Internal (Get_Object (Buffer), Boolean'Pos (Interactive), Boolean'Pos (Default_Editable)) /= 0;
   end Delete_Selection;

   -----------------------------
   -- End_Irreversible_Action --
   -----------------------------

   procedure End_Irreversible_Action
      (Buffer : not null access Gtk_Text_Buffer_Record)
   is
      procedure Internal (Buffer : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_end_irreversible_action");
   begin
      Internal (Get_Object (Buffer));
   end End_Irreversible_Action;

   ---------------------
   -- End_User_Action --
   ---------------------

   procedure End_User_Action
      (Buffer : not null access Gtk_Text_Buffer_Record)
   is
      procedure Internal (Buffer : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_end_user_action");
   begin
      Internal (Get_Object (Buffer));
   end End_User_Action;

   ----------------
   -- Get_Bounds --
   ----------------

   procedure Get_Bounds
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : out Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer  : System.Address;
          Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
          The_End : out Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_get_bounds");
      Tmp_Start   : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_The_End : aliased Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Internal (Get_Object (Buffer), Tmp_Start, Tmp_The_End);
      The_End := Tmp_The_End;
      Start := Tmp_Start;
   end Get_Bounds;

   ------------------
   -- Get_Can_Redo --
   ------------------

   function Get_Can_Redo
      (Buffer : not null access Gtk_Text_Buffer_Record) return Boolean
   is
      function Internal (Buffer : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_get_can_redo");
   begin
      return Internal (Get_Object (Buffer)) /= 0;
   end Get_Can_Redo;

   ------------------
   -- Get_Can_Undo --
   ------------------

   function Get_Can_Undo
      (Buffer : not null access Gtk_Text_Buffer_Record) return Boolean
   is
      function Internal (Buffer : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_get_can_undo");
   begin
      return Internal (Get_Object (Buffer)) /= 0;
   end Get_Can_Undo;

   --------------------
   -- Get_Char_Count --
   --------------------

   function Get_Char_Count
      (Buffer : not null access Gtk_Text_Buffer_Record) return Glib.Gint
   is
      function Internal (Buffer : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_text_buffer_get_char_count");
   begin
      return Internal (Get_Object (Buffer));
   end Get_Char_Count;

   ---------------------
   -- Get_Enable_Undo --
   ---------------------

   function Get_Enable_Undo
      (Buffer : not null access Gtk_Text_Buffer_Record) return Boolean
   is
      function Internal (Buffer : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_get_enable_undo");
   begin
      return Internal (Get_Object (Buffer)) /= 0;
   end Get_Enable_Undo;

   ------------------
   -- Get_End_Iter --
   ------------------

   procedure Get_End_Iter
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : out Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer : System.Address;
          Iter   : out Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_get_end_iter");
      Tmp_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Internal (Get_Object (Buffer), Tmp_Iter);
      Iter := Tmp_Iter;
   end Get_End_Iter;

   -----------------------
   -- Get_Has_Selection --
   -----------------------

   function Get_Has_Selection
      (Buffer : not null access Gtk_Text_Buffer_Record) return Boolean
   is
      function Internal (Buffer : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_get_has_selection");
   begin
      return Internal (Get_Object (Buffer)) /= 0;
   end Get_Has_Selection;

   ----------------
   -- Get_Insert --
   ----------------

   function Get_Insert
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gtk.Text_Mark.Gtk_Text_Mark
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_get_insert");
      Stub_Gtk_Text_Mark : Gtk.Text_Mark.Gtk_Text_Mark_Record;
   begin
      return Gtk.Text_Mark.Gtk_Text_Mark (Get_User_Data (Internal (Get_Object (Buffer)), Stub_Gtk_Text_Mark));
   end Get_Insert;

   ------------------------------
   -- Get_Iter_At_Child_Anchor --
   ------------------------------

   procedure Get_Iter_At_Child_Anchor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
       Anchor : not null access Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor_Record'Class)
   is
      procedure Internal
         (Buffer : System.Address;
          Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
          Anchor : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_get_iter_at_child_anchor");
      Tmp_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Internal (Get_Object (Buffer), Tmp_Iter, Get_Object (Anchor));
      Iter := Tmp_Iter;
   end Get_Iter_At_Child_Anchor;

   ----------------------
   -- Get_Iter_At_Line --
   ----------------------

   function Get_Iter_At_Line
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Iter        : access Gtk.Text_Iter.Gtk_Text_Iter;
       Line_Number : Glib.Gint) return Boolean
   is
      function Internal
         (Buffer      : System.Address;
          Acc_Iter    : access Gtk.Text_Iter.Gtk_Text_Iter;
          Line_Number : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_get_iter_at_line");
      Acc_Iter     : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Acc_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Buffer), Tmp_Acc_Iter'Access, Line_Number);
      Acc_Iter := Tmp_Acc_Iter;
      Iter.all := Acc_Iter;
      return Tmp_Return /= 0;
   end Get_Iter_At_Line;

   ----------------------------
   -- Get_Iter_At_Line_Index --
   ----------------------------

   function Get_Iter_At_Line_Index
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Iter        : access Gtk.Text_Iter.Gtk_Text_Iter;
       Line_Number : Glib.Gint;
       Byte_Index  : Glib.Gint) return Boolean
   is
      function Internal
         (Buffer      : System.Address;
          Acc_Iter    : access Gtk.Text_Iter.Gtk_Text_Iter;
          Line_Number : Glib.Gint;
          Byte_Index  : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_get_iter_at_line_index");
      Acc_Iter     : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Acc_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Buffer), Tmp_Acc_Iter'Access, Line_Number, Byte_Index);
      Acc_Iter := Tmp_Acc_Iter;
      Iter.all := Acc_Iter;
      return Tmp_Return /= 0;
   end Get_Iter_At_Line_Index;

   -----------------------------
   -- Get_Iter_At_Line_Offset --
   -----------------------------

   function Get_Iter_At_Line_Offset
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Iter        : access Gtk.Text_Iter.Gtk_Text_Iter;
       Line_Number : Glib.Gint;
       Char_Offset : Glib.Gint) return Boolean
   is
      function Internal
         (Buffer      : System.Address;
          Acc_Iter    : access Gtk.Text_Iter.Gtk_Text_Iter;
          Line_Number : Glib.Gint;
          Char_Offset : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_get_iter_at_line_offset");
      Acc_Iter     : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Acc_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Buffer), Tmp_Acc_Iter'Access, Line_Number, Char_Offset);
      Acc_Iter := Tmp_Acc_Iter;
      Iter.all := Acc_Iter;
      return Tmp_Return /= 0;
   end Get_Iter_At_Line_Offset;

   ----------------------
   -- Get_Iter_At_Mark --
   ----------------------

   procedure Get_Iter_At_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
       Mark   : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
   is
      procedure Internal
         (Buffer : System.Address;
          Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
          Mark   : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_get_iter_at_mark");
      Tmp_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Internal (Get_Object (Buffer), Tmp_Iter, Get_Object (Mark));
      Iter := Tmp_Iter;
   end Get_Iter_At_Mark;

   ------------------------
   -- Get_Iter_At_Offset --
   ------------------------

   procedure Get_Iter_At_Offset
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
       Char_Offset : Glib.Gint)
   is
      procedure Internal
         (Buffer      : System.Address;
          Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
          Char_Offset : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_buffer_get_iter_at_offset");
      Tmp_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Internal (Get_Object (Buffer), Tmp_Iter, Char_Offset);
      Iter := Tmp_Iter;
   end Get_Iter_At_Offset;

   --------------------
   -- Get_Line_Count --
   --------------------

   function Get_Line_Count
      (Buffer : not null access Gtk_Text_Buffer_Record) return Glib.Gint
   is
      function Internal (Buffer : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_text_buffer_get_line_count");
   begin
      return Internal (Get_Object (Buffer));
   end Get_Line_Count;

   --------------
   -- Get_Mark --
   --------------

   function Get_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Name   : UTF8_String) return Gtk.Text_Mark.Gtk_Text_Mark
   is
      function Internal
         (Buffer : System.Address;
          Name   : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_get_mark");
      Tmp_Name           : Gtkada.Types.Chars_Ptr := New_String (Name);
      Stub_Gtk_Text_Mark : Gtk.Text_Mark.Gtk_Text_Mark_Record;
      Tmp_Return         : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Buffer), Tmp_Name);
      Free (Tmp_Name);
      return Gtk.Text_Mark.Gtk_Text_Mark (Get_User_Data (Tmp_Return, Stub_Gtk_Text_Mark));
   end Get_Mark;

   -------------------------
   -- Get_Max_Undo_Levels --
   -------------------------

   function Get_Max_Undo_Levels
      (Buffer : not null access Gtk_Text_Buffer_Record) return Guint
   is
      function Internal (Buffer : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_text_buffer_get_max_undo_levels");
   begin
      return Internal (Get_Object (Buffer));
   end Get_Max_Undo_Levels;

   ------------------
   -- Get_Modified --
   ------------------

   function Get_Modified
      (Buffer : not null access Gtk_Text_Buffer_Record) return Boolean
   is
      function Internal (Buffer : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_get_modified");
   begin
      return Internal (Get_Object (Buffer)) /= 0;
   end Get_Modified;

   -------------------------
   -- Get_Selection_Bound --
   -------------------------

   function Get_Selection_Bound
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gtk.Text_Mark.Gtk_Text_Mark
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_get_selection_bound");
      Stub_Gtk_Text_Mark : Gtk.Text_Mark.Gtk_Text_Mark_Record;
   begin
      return Gtk.Text_Mark.Gtk_Text_Mark (Get_User_Data (Internal (Get_Object (Buffer)), Stub_Gtk_Text_Mark));
   end Get_Selection_Bound;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : out Gtk.Text_Iter.Gtk_Text_Iter;
       Result  : out Boolean)
   is
      function Internal
         (Buffer      : System.Address;
          Acc_Start   : access Gtk.Text_Iter.Gtk_Text_Iter;
          Acc_The_End : access Gtk.Text_Iter.Gtk_Text_Iter)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_get_selection_bounds");
      Acc_Start       : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Acc_The_End     : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Acc_Start   : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Acc_The_End : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Return      : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Buffer), Tmp_Acc_Start'Access, Tmp_Acc_The_End'Access);
      Acc_The_End := Tmp_Acc_The_End;
      Acc_Start := Tmp_Acc_Start;
      Start := Acc_Start;
      The_End := Acc_The_End;
      Result := Tmp_Return /= 0;
   end Get_Selection_Bounds;

   ---------------------------
   -- Get_Selection_Content --
   ---------------------------

   function Get_Selection_Content
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gdk.Content_Provider.Gdk_Content_Provider
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_get_selection_content");
      Stub_Gdk_Content_Provider : Gdk.Content_Provider.Gdk_Content_Provider_Record;
   begin
      return Gdk.Content_Provider.Gdk_Content_Provider (Get_User_Data (Internal (Get_Object (Buffer)), Stub_Gdk_Content_Provider));
   end Get_Selection_Content;

   ---------------
   -- Get_Slice --
   ---------------

   function Get_Slice
      (Buffer               : not null access Gtk_Text_Buffer_Record;
       Start                : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
       Include_Hidden_Chars : Boolean := False) return UTF8_String
   is
      function Internal
         (Buffer               : System.Address;
          Start                : Gtk.Text_Iter.Gtk_Text_Iter;
          The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
          Include_Hidden_Chars : Glib.Gboolean)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_text_buffer_get_slice");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Buffer), Start, The_End, Boolean'Pos (Include_Hidden_Chars)));
   end Get_Slice;

   --------------------
   -- Get_Start_Iter --
   --------------------

   procedure Get_Start_Iter
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : out Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer : System.Address;
          Iter   : out Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_get_start_iter");
      Tmp_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Internal (Get_Object (Buffer), Tmp_Iter);
      Iter := Tmp_Iter;
   end Get_Start_Iter;

   -------------------
   -- Get_Tag_Table --
   -------------------

   function Get_Tag_Table
      (Buffer : not null access Gtk_Text_Buffer_Record)
       return Gtk.Text_Tag_Table.Gtk_Text_Tag_Table
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_get_tag_table");
      Stub_Gtk_Text_Tag_Table : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table_Record;
   begin
      return Gtk.Text_Tag_Table.Gtk_Text_Tag_Table (Get_User_Data (Internal (Get_Object (Buffer)), Stub_Gtk_Text_Tag_Table));
   end Get_Tag_Table;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Buffer               : not null access Gtk_Text_Buffer_Record;
       Start                : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
       Include_Hidden_Chars : Boolean := False) return UTF8_String
   is
      function Internal
         (Buffer               : System.Address;
          Start                : Gtk.Text_Iter.Gtk_Text_Iter;
          The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
          Include_Hidden_Chars : Glib.Gboolean)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_text_buffer_get_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Buffer), Start, The_End, Boolean'Pos (Include_Hidden_Chars)));
   end Get_Text;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Text   : UTF8_String)
   is
      procedure Internal
         (Buffer : System.Address;
          Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
          Text   : Gtkada.Types.Chars_Ptr;
          Len    : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_buffer_insert");
      Tmp_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter := Iter;
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Buffer), Tmp_Iter, Tmp_Text, -1);
      Free (Tmp_Text);
      Iter := Tmp_Iter;
   end Insert;

   ----------------------
   -- Insert_At_Cursor --
   ----------------------

   procedure Insert_At_Cursor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Text   : UTF8_String)
   is
      procedure Internal
         (Buffer : System.Address;
          Text   : Gtkada.Types.Chars_Ptr;
          Len    : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_buffer_insert_at_cursor");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Buffer), Tmp_Text, -1);
      Free (Tmp_Text);
   end Insert_At_Cursor;

   -------------------------
   -- Insert_Child_Anchor --
   -------------------------

   procedure Insert_Child_Anchor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Anchor : not null access Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor_Record'Class)
   is
      procedure Internal
         (Buffer : System.Address;
          Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
          Anchor : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_insert_child_anchor");
      Tmp_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter := Iter;
   begin
      Internal (Get_Object (Buffer), Tmp_Iter, Get_Object (Anchor));
      Iter := Tmp_Iter;
   end Insert_Child_Anchor;

   ------------------------
   -- Insert_Interactive --
   ------------------------

   function Insert_Interactive
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Iter             : access Gtk.Text_Iter.Gtk_Text_Iter;
       Text             : UTF8_String;
       Default_Editable : Boolean := True) return Boolean
   is
      function Internal
         (Buffer           : System.Address;
          Acc_Iter         : access Gtk.Text_Iter.Gtk_Text_Iter;
          Text             : Gtkada.Types.Chars_Ptr;
          Len              : Glib.Gint;
          Default_Editable : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_insert_interactive");
      Acc_Iter     : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Acc_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Text     : Gtkada.Types.Chars_Ptr := New_String (Text);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Buffer), Tmp_Acc_Iter'Access, Tmp_Text, -1, Boolean'Pos (Default_Editable));
      Free (Tmp_Text);
      Acc_Iter := Tmp_Acc_Iter;
      Iter.all := Acc_Iter;
      return Tmp_Return /= 0;
   end Insert_Interactive;

   ----------------------------------
   -- Insert_Interactive_At_Cursor --
   ----------------------------------

   function Insert_Interactive_At_Cursor
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Text             : UTF8_String;
       Default_Editable : Boolean := True) return Boolean
   is
      function Internal
         (Buffer           : System.Address;
          Text             : Gtkada.Types.Chars_Ptr;
          Len              : Glib.Gint;
          Default_Editable : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_insert_interactive_at_cursor");
      Tmp_Text   : Gtkada.Types.Chars_Ptr := New_String (Text);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Buffer), Tmp_Text, -1, Boolean'Pos (Default_Editable));
      Free (Tmp_Text);
      return Tmp_Return /= 0;
   end Insert_Interactive_At_Cursor;

   -------------------
   -- Insert_Markup --
   -------------------

   procedure Insert_Markup
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Markup : UTF8_String)
   is
      procedure Internal
         (Buffer : System.Address;
          Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
          Markup : Gtkada.Types.Chars_Ptr;
          Len    : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_buffer_insert_markup");
      Tmp_Iter   : aliased Gtk.Text_Iter.Gtk_Text_Iter := Iter;
      Tmp_Markup : Gtkada.Types.Chars_Ptr := New_String (Markup);
   begin
      Internal (Get_Object (Buffer), Tmp_Iter, Tmp_Markup, -1);
      Free (Tmp_Markup);
      Iter := Tmp_Iter;
   end Insert_Markup;

   ----------------------
   -- Insert_Paintable --
   ----------------------

   procedure Insert_Paintable
      (Buffer    : not null access Gtk_Text_Buffer_Record;
       Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Paintable : Gdk.Paintable.Gdk_Paintable)
   is
      procedure Internal
         (Buffer    : System.Address;
          Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
          Paintable : Gdk.Paintable.Gdk_Paintable);
      pragma Import (C, Internal, "gtk_text_buffer_insert_paintable");
      Tmp_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter := Iter;
   begin
      Internal (Get_Object (Buffer), Tmp_Iter, Paintable);
      Iter := Tmp_Iter;
   end Insert_Paintable;

   ------------------
   -- Insert_Range --
   ------------------

   procedure Insert_Range
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Iter    : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer  : System.Address;
          Iter    : in out Gtk.Text_Iter.Gtk_Text_Iter;
          Start   : Gtk.Text_Iter.Gtk_Text_Iter;
          The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_insert_range");
      Tmp_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter := Iter;
   begin
      Internal (Get_Object (Buffer), Tmp_Iter, Start, The_End);
      Iter := Tmp_Iter;
   end Insert_Range;

   ------------------------------
   -- Insert_Range_Interactive --
   ------------------------------

   procedure Insert_Range_Interactive
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Iter             : in out Gtk.Text_Iter.Gtk_Text_Iter;
       Start            : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End          : Gtk.Text_Iter.Gtk_Text_Iter;
       Default_Editable : Boolean := True;
       Result           : out Boolean)
   is
      function Internal
         (Buffer           : System.Address;
          Acc_Iter         : access Gtk.Text_Iter.Gtk_Text_Iter;
          Start            : Gtk.Text_Iter.Gtk_Text_Iter;
          The_End          : Gtk.Text_Iter.Gtk_Text_Iter;
          Default_Editable : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_insert_range_interactive");
      Acc_Iter   : aliased Gtk.Text_Iter.Gtk_Text_Iter := Iter;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Buffer), Acc_Iter'Access, Start, The_End, Boolean'Pos (Default_Editable));
      Iter := Acc_Iter;
      Result := Tmp_Return /= 0;
   end Insert_Range_Interactive;

   ---------------
   -- Move_Mark --
   ---------------

   procedure Move_Mark
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Mark   : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
       Where  : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer : System.Address;
          Mark   : System.Address;
          Where  : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_move_mark");
   begin
      Internal (Get_Object (Buffer), Get_Object (Mark), Where);
   end Move_Mark;

   -----------------------
   -- Move_Mark_By_Name --
   -----------------------

   procedure Move_Mark_By_Name
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Name   : UTF8_String;
       Where  : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer : System.Address;
          Name   : Gtkada.Types.Chars_Ptr;
          Where  : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_move_mark_by_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Buffer), Tmp_Name, Where);
      Free (Tmp_Name);
   end Move_Mark_By_Name;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard
      (Buffer           : not null access Gtk_Text_Buffer_Record;
       Clipboard        : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class;
       Default_Editable : Boolean := True)
   is
      procedure Internal
         (Buffer            : System.Address;
          Clipboard         : System.Address;
          Override_Location : System.Address;
          Default_Editable  : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_text_buffer_paste_clipboard");
   begin
      Internal (Get_Object (Buffer), Get_Object (Clipboard), System.Null_Address, Boolean'Pos (Default_Editable));
   end Paste_Clipboard;

   ------------------
   -- Place_Cursor --
   ------------------

   procedure Place_Cursor
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Where  : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer : System.Address;
          Where  : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_place_cursor");
   begin
      Internal (Get_Object (Buffer), Where);
   end Place_Cursor;

   ----------
   -- Redo --
   ----------

   procedure Redo (Buffer : not null access Gtk_Text_Buffer_Record) is
      procedure Internal (Buffer : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_redo");
   begin
      Internal (Get_Object (Buffer));
   end Redo;

   ---------------------
   -- Remove_All_Tags --
   ---------------------

   procedure Remove_All_Tags
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer  : System.Address;
          Start   : Gtk.Text_Iter.Gtk_Text_Iter;
          The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_remove_all_tags");
   begin
      Internal (Get_Object (Buffer), Start, The_End);
   end Remove_All_Tags;

   --------------------------------
   -- Remove_Selection_Clipboard --
   --------------------------------

   procedure Remove_Selection_Clipboard
      (Buffer    : not null access Gtk_Text_Buffer_Record;
       Clipboard : not null access Gdk.Clipboard.Gdk_Clipboard_Record'Class)
   is
      procedure Internal
         (Buffer    : System.Address;
          Clipboard : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_remove_selection_clipboard");
   begin
      Internal (Get_Object (Buffer), Get_Object (Clipboard));
   end Remove_Selection_Clipboard;

   ----------------
   -- Remove_Tag --
   ----------------

   procedure Remove_Tag
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Tag     : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer  : System.Address;
          Tag     : System.Address;
          Start   : Gtk.Text_Iter.Gtk_Text_Iter;
          The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_remove_tag");
   begin
      Internal (Get_Object (Buffer), Get_Object (Tag), Start, The_End);
   end Remove_Tag;

   ------------------------
   -- Remove_Tag_By_Name --
   ------------------------

   procedure Remove_Tag_By_Name
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Name    : UTF8_String;
       Start   : Gtk.Text_Iter.Gtk_Text_Iter;
       The_End : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer  : System.Address;
          Name    : Gtkada.Types.Chars_Ptr;
          Start   : Gtk.Text_Iter.Gtk_Text_Iter;
          The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_remove_tag_by_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Buffer), Tmp_Name, Start, The_End);
      Free (Tmp_Name);
   end Remove_Tag_By_Name;

   ------------------
   -- Select_Range --
   ------------------

   procedure Select_Range
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Ins    : Gtk.Text_Iter.Gtk_Text_Iter;
       Bound  : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
         (Buffer : System.Address;
          Ins    : Gtk.Text_Iter.Gtk_Text_Iter;
          Bound  : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_select_range");
   begin
      Internal (Get_Object (Buffer), Ins, Bound);
   end Select_Range;

   ---------------------
   -- Set_Enable_Undo --
   ---------------------

   procedure Set_Enable_Undo
      (Buffer      : not null access Gtk_Text_Buffer_Record;
       Enable_Undo : Boolean)
   is
      procedure Internal
         (Buffer      : System.Address;
          Enable_Undo : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_text_buffer_set_enable_undo");
   begin
      Internal (Get_Object (Buffer), Boolean'Pos (Enable_Undo));
   end Set_Enable_Undo;

   -------------------------
   -- Set_Max_Undo_Levels --
   -------------------------

   procedure Set_Max_Undo_Levels
      (Buffer          : not null access Gtk_Text_Buffer_Record;
       Max_Undo_Levels : Guint)
   is
      procedure Internal (Buffer : System.Address; Max_Undo_Levels : Guint);
      pragma Import (C, Internal, "gtk_text_buffer_set_max_undo_levels");
   begin
      Internal (Get_Object (Buffer), Max_Undo_Levels);
   end Set_Max_Undo_Levels;

   ------------------
   -- Set_Modified --
   ------------------

   procedure Set_Modified
      (Buffer  : not null access Gtk_Text_Buffer_Record;
       Setting : Boolean)
   is
      procedure Internal (Buffer : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_text_buffer_set_modified");
   begin
      Internal (Get_Object (Buffer), Boolean'Pos (Setting));
   end Set_Modified;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Buffer : not null access Gtk_Text_Buffer_Record;
       Text   : UTF8_String)
   is
      procedure Internal
         (Buffer : System.Address;
          Text   : Gtkada.Types.Chars_Ptr;
          Len    : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_buffer_set_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Buffer), Tmp_Text, -1);
      Free (Tmp_Text);
   end Set_Text;

   ----------
   -- Undo --
   ----------

   procedure Undo (Buffer : not null access Gtk_Text_Buffer_Record) is
      procedure Internal (Buffer : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_undo");
   begin
      Internal (Get_Object (Buffer));
   end Undo;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_Buffer_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_Buffer_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Text_Iter_Gdk_Paintable_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Text_Iter_Gdk_Paintable_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_Buffer_Gtk_Text_Mark_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_Buffer_Gtk_Text_Mark_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Text_Mark_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Text_Mark_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_Buffer_Gdk_Clipboard_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_Buffer_Gdk_Clipboard_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Clipboard_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Clipboard_Void);

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Mark_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gdk_Clipboard_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Iter_Gdk_Paintable_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Mark_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Clipboard_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdk_Clipboard_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Clipboard_Void);

   procedure Marsh_GObject_Gtk_Text_Iter_Gdk_Paintable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Text_Iter_Gdk_Paintable_Void);

   procedure Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void);

   procedure Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void);

   procedure Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void);

   procedure Marsh_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void);

   procedure Marsh_GObject_Gtk_Text_Mark_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Text_Mark_Void);

   procedure Marsh_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Text_Buffer_Gdk_Clipboard_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_Buffer_Gdk_Clipboard_Void);

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void);

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void);

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void);

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void);

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void);

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Mark_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_Buffer_Gtk_Text_Mark_Void);

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void);

   procedure Marsh_Gtk_Text_Buffer_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_Buffer_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_Buffer_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Mark_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_Buffer_Gtk_Text_Mark_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_Buffer_Gdk_Clipboard_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_Buffer_Gdk_Clipboard_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Iter_Gdk_Paintable_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Text_Iter_Gdk_Paintable_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Mark_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Text_Mark_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Text_Buffer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Clipboard_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Clipboard_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------------
   -- Marsh_GObject_Gdk_Clipboard_Void --
   --------------------------------------

   procedure Marsh_GObject_Gdk_Clipboard_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Clipboard_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gdk.Clipboard.Gdk_Clipboard (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Clipboard_Void;

   ----------------------------------------------------
   -- Marsh_GObject_Gtk_Text_Iter_Gdk_Paintable_Void --
   ----------------------------------------------------

   procedure Marsh_GObject_Gtk_Text_Iter_Gdk_Paintable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Text_Iter_Gdk_Paintable_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Text_Iter (Params, 1), Gdk.Paintable.Gdk_Paintable (Unchecked_To_Interface (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Text_Iter_Gdk_Paintable_Void;

   ------------------------------------------------------------
   -- Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void --
   ------------------------------------------------------------

   procedure Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Text_Iter (Params, 1), Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor (Unchecked_To_Object (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void;

   ----------------------------------------------------
   -- Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void --
   ----------------------------------------------------

   procedure Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Text_Iter (Params, 1), Unchecked_To_Gtk_Text_Iter (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void;

   ----------------------------------------------------
   -- Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void --
   ----------------------------------------------------

   procedure Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Text_Iter (Params, 1), Gtk.Text_Mark.Gtk_Text_Mark (Unchecked_To_Object (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void;

   -------------------------------------------------------
   -- Marsh_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void --
   -------------------------------------------------------

   procedure Marsh_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Text_Iter (Params, 1), Unchecked_To_UTF8_String (Params, 2), Unchecked_To_Gint (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void;

   --------------------------------------
   -- Marsh_GObject_Gtk_Text_Mark_Void --
   --------------------------------------

   procedure Marsh_GObject_Gtk_Text_Mark_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Text_Mark_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Text_Mark.Gtk_Text_Mark (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Text_Mark_Void;

   -----------------------------------------------------------------
   -- Marsh_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void --
   -----------------------------------------------------------------

   procedure Marsh_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Text_Tag.Gtk_Text_Tag (Unchecked_To_Object (Params, 1)), Unchecked_To_Gtk_Text_Iter (Params, 2), Unchecked_To_Gtk_Text_Iter (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   ----------------------------------------------
   -- Marsh_Gtk_Text_Buffer_Gdk_Clipboard_Void --
   ----------------------------------------------

   procedure Marsh_Gtk_Text_Buffer_Gdk_Clipboard_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_Buffer_Gdk_Clipboard_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_Buffer := Gtk_Text_Buffer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gdk.Clipboard.Gdk_Clipboard (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_Buffer_Gdk_Clipboard_Void;

   ------------------------------------------------------------
   -- Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void --
   ------------------------------------------------------------

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_Buffer := Gtk_Text_Buffer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Text_Iter (Params, 1), Gdk.Paintable.Gdk_Paintable (Unchecked_To_Interface (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void;

   --------------------------------------------------------------------
   -- Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void --
   --------------------------------------------------------------------

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_Buffer := Gtk_Text_Buffer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Text_Iter (Params, 1), Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor (Unchecked_To_Object (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void;

   ------------------------------------------------------------
   -- Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void --
   ------------------------------------------------------------

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_Buffer := Gtk_Text_Buffer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Text_Iter (Params, 1), Unchecked_To_Gtk_Text_Iter (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void;

   ------------------------------------------------------------
   -- Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void --
   ------------------------------------------------------------

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_Buffer := Gtk_Text_Buffer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Text_Iter (Params, 1), Gtk.Text_Mark.Gtk_Text_Mark (Unchecked_To_Object (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void;

   ---------------------------------------------------------------
   -- Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void --
   ---------------------------------------------------------------

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_Buffer := Gtk_Text_Buffer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Text_Iter (Params, 1), Unchecked_To_UTF8_String (Params, 2), Unchecked_To_Gint (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void;

   ----------------------------------------------
   -- Marsh_Gtk_Text_Buffer_Gtk_Text_Mark_Void --
   ----------------------------------------------

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Mark_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_Buffer_Gtk_Text_Mark_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_Buffer := Gtk_Text_Buffer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Text_Mark.Gtk_Text_Mark (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_Buffer_Gtk_Text_Mark_Void;

   -------------------------------------------------------------------------
   -- Marsh_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void --
   -------------------------------------------------------------------------

   procedure Marsh_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_Buffer := Gtk_Text_Buffer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Text_Tag.Gtk_Text_Tag (Unchecked_To_Object (Params, 1)), Unchecked_To_Gtk_Text_Iter (Params, 2), Unchecked_To_Gtk_Text_Iter (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;

   --------------------------------
   -- Marsh_Gtk_Text_Buffer_Void --
   --------------------------------

   procedure Marsh_Gtk_Text_Buffer_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_Buffer_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_Buffer := Gtk_Text_Buffer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_Buffer_Void;

   ------------------
   -- On_Apply_Tag --
   ------------------

   procedure On_Apply_Tag
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "apply-tag" & ASCII.NUL, Call, After);
   end On_Apply_Tag;

   ------------------
   -- On_Apply_Tag --
   ------------------

   procedure On_Apply_Tag
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "apply-tag" & ASCII.NUL, Call, After, Slot);
   end On_Apply_Tag;

   --------------------------
   -- On_Begin_User_Action --
   --------------------------

   procedure On_Begin_User_Action
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "begin-user-action" & ASCII.NUL, Call, After);
   end On_Begin_User_Action;

   --------------------------
   -- On_Begin_User_Action --
   --------------------------

   procedure On_Begin_User_Action
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "begin-user-action" & ASCII.NUL, Call, After, Slot);
   end On_Begin_User_Action;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

   ---------------------
   -- On_Delete_Range --
   ---------------------

   procedure On_Delete_Range
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "delete-range" & ASCII.NUL, Call, After);
   end On_Delete_Range;

   ---------------------
   -- On_Delete_Range --
   ---------------------

   procedure On_Delete_Range
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "delete-range" & ASCII.NUL, Call, After, Slot);
   end On_Delete_Range;

   ------------------------
   -- On_End_User_Action --
   ------------------------

   procedure On_End_User_Action
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "end-user-action" & ASCII.NUL, Call, After);
   end On_End_User_Action;

   ------------------------
   -- On_End_User_Action --
   ------------------------

   procedure On_End_User_Action
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "end-user-action" & ASCII.NUL, Call, After, Slot);
   end On_End_User_Action;

   ----------------------------
   -- On_Insert_Child_Anchor --
   ----------------------------

   procedure On_Insert_Child_Anchor
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "insert-child-anchor" & ASCII.NUL, Call, After);
   end On_Insert_Child_Anchor;

   ----------------------------
   -- On_Insert_Child_Anchor --
   ----------------------------

   procedure On_Insert_Child_Anchor
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Iter_Gtk_Text_Child_Anchor_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "insert-child-anchor" & ASCII.NUL, Call, After, Slot);
   end On_Insert_Child_Anchor;

   -------------------------
   -- On_Insert_Paintable --
   -------------------------

   procedure On_Insert_Paintable
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gdk_Paintable_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "insert-paintable" & ASCII.NUL, Call, After);
   end On_Insert_Paintable;

   -------------------------
   -- On_Insert_Paintable --
   -------------------------

   procedure On_Insert_Paintable
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Iter_Gdk_Paintable_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "insert-paintable" & ASCII.NUL, Call, After, Slot);
   end On_Insert_Paintable;

   --------------------
   -- On_Insert_Text --
   --------------------

   procedure On_Insert_Text
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_UTF8_String_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "insert-text" & ASCII.NUL, Call, After);
   end On_Insert_Text;

   --------------------
   -- On_Insert_Text --
   --------------------

   procedure On_Insert_Text
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Iter_UTF8_String_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "insert-text" & ASCII.NUL, Call, After, Slot);
   end On_Insert_Text;

   ---------------------
   -- On_Mark_Deleted --
   ---------------------

   procedure On_Mark_Deleted
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Mark_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "mark-deleted" & ASCII.NUL, Call, After);
   end On_Mark_Deleted;

   ---------------------
   -- On_Mark_Deleted --
   ---------------------

   procedure On_Mark_Deleted
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Mark_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "mark-deleted" & ASCII.NUL, Call, After, Slot);
   end On_Mark_Deleted;

   -----------------
   -- On_Mark_Set --
   -----------------

   procedure On_Mark_Set
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Iter_Gtk_Text_Mark_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "mark-set" & ASCII.NUL, Call, After);
   end On_Mark_Set;

   -----------------
   -- On_Mark_Set --
   -----------------

   procedure On_Mark_Set
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Iter_Gtk_Text_Mark_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "mark-set" & ASCII.NUL, Call, After, Slot);
   end On_Mark_Set;

   -------------------------
   -- On_Modified_Changed --
   -------------------------

   procedure On_Modified_Changed
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "modified-changed" & ASCII.NUL, Call, After);
   end On_Modified_Changed;

   -------------------------
   -- On_Modified_Changed --
   -------------------------

   procedure On_Modified_Changed
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "modified-changed" & ASCII.NUL, Call, After, Slot);
   end On_Modified_Changed;

   -------------------
   -- On_Paste_Done --
   -------------------

   procedure On_Paste_Done
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gdk_Clipboard_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "paste-done" & ASCII.NUL, Call, After);
   end On_Paste_Done;

   -------------------
   -- On_Paste_Done --
   -------------------

   procedure On_Paste_Done
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gdk_Clipboard_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "paste-done" & ASCII.NUL, Call, After, Slot);
   end On_Paste_Done;

   -------------
   -- On_Redo --
   -------------

   procedure On_Redo
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "redo" & ASCII.NUL, Call, After);
   end On_Redo;

   -------------
   -- On_Redo --
   -------------

   procedure On_Redo
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "redo" & ASCII.NUL, Call, After, Slot);
   end On_Redo;

   -------------------
   -- On_Remove_Tag --
   -------------------

   procedure On_Remove_Tag
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "remove-tag" & ASCII.NUL, Call, After);
   end On_Remove_Tag;

   -------------------
   -- On_Remove_Tag --
   -------------------

   procedure On_Remove_Tag
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Gtk_Text_Tag_Gtk_Text_Iter_Gtk_Text_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "remove-tag" & ASCII.NUL, Call, After, Slot);
   end On_Remove_Tag;

   -------------
   -- On_Undo --
   -------------

   procedure On_Undo
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_Gtk_Text_Buffer_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "undo" & ASCII.NUL, Call, After);
   end On_Undo;

   -------------
   -- On_Undo --
   -------------

   procedure On_Undo
      (Self  : not null access Gtk_Text_Buffer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "undo" & ASCII.NUL, Call, After, Slot);
   end On_Undo;

end Gtk.Text_Buffer;
