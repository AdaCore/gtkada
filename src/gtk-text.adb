-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                  Copyright (C) 2001 ACT-Europe                    --
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

with System;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Glib; use Glib;
with Glib.Object;
with Glib.Values;
with Gdk.Color; use Gdk.Color;
with Gdk.Window; use Gdk.Window;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Text_Mark;
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Pango.Font; use Pango.Font;

pragma Elaborate_All (Gtk.Handlers);

package body Gtk.Text is

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;
   --  A pointer to the 'class record'.

   Signals : Interfaces.C.Strings.chars_ptr_array :=
     (1  => New_String ("changed"),
      2  => New_String ("insert_text"),
      3  => New_String ("delete_text"),
      --  The "activate" signal will not be supported
      4  => New_String ("set-editable"),
      5  => New_String ("move_cursor_compat"),
      6  => New_String ("move_word"),
      7  => New_String ("move_page"),
      8  => New_String ("move_to_row"),
      9  => New_String ("move_to_column"),
      10 => New_String ("kill_char"),
      11 => New_String ("kill_word"),
      12 => New_String ("kill_line"));
   --  The list of new signals supported by this widget.

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1  => (GType_None,   GType_None, GType_None),
      2  => (GType_String, GType_Int,  GType_Pointer),
      3  => (GType_Int,    GType_Int,  GType_None),
      4  => (GType_Bool,   GType_None, GType_None),
      5  => (GType_Int,    GType_Int,  GType_None),
      6  => (GType_Int,    GType_None, GType_None),
      7  => (GType_Int,    GType_Int,  GType_None),
      8  => (GType_Int,    GType_None, GType_None),
      9  => (GType_Int,    GType_None, GType_None),
      10 => (GType_Int,    GType_None, GType_None),
      11 => (GType_Int,    GType_None, GType_None),
      12 => (GType_Int,    GType_None, GType_None));
   --  The parameters associated to each new signals.

   package Text_Callback is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Text_Record);

   package Buffer_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Text_Buffer.Gtk_Text_Buffer_Record,
      User_Type   => Gtk_Text);

   package Text_Callback_Boolean_Marshaller is
     new Text_Callback.Marshallers.Generic_Marshaller
       (Boolean, Glib.Values.Get_Boolean);

   --------------------------
   -- Forward declarations --
   --------------------------

   procedure Get_Range_Iter
     (Buffer      : Text_Buffer.Gtk_Text_Buffer;
      Start_Pos   : Gint := 0;
      End_Pos     : Gint := -1;
      Start_Iter  : out Text_Iter.Gtk_Text_Iter;
      End_Iter    : out Text_Iter.Gtk_Text_Iter;
      Valid_Range : out Boolean);
   --  Convert the Character offset range into a range delimited by
   --  Start_Iter and End_Iter. End_Iter is set to the end of the buffer
   --  if End_Pos is negative.
   --  If Start_Pos .. End_Pos do not represent a valid range (that is
   --  End_Pos > Start_Pos), then Valid_Range is set to False and the
   --  Iterators returned are undefined.

   --
   --  Signal handlers...
   --

   procedure Changed_Cb
     (Widget : access Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Text   : Gtk_Text);

   procedure Delete_Range_Handler
     (Widget : access Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      Text   : Gtk_Text);

   procedure Insert_Text_Handler
     (Widget : access Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      Text   : Gtk_Text);

   procedure Move_Cursor_Handler
     (Widget : access Gtk_Text_Record'Class;
      Params : Glib.Values.GValues);

   procedure Move_Word_Cb
     (Widget : access Gtk_Text_Record'Class;
      N      : Gint);

   procedure Set_Editable_Cb
     (Widget      : access Gtk_Text_Record'Class;
      Is_Editable : Boolean);

   --------------------
   -- Get_Range_Iter --
   --------------------

   procedure Get_Range_Iter
     (Buffer      : Text_Buffer.Gtk_Text_Buffer;
      Start_Pos   : Gint := 0;
      End_Pos     : Gint := -1;
      Start_Iter  : out Text_Iter.Gtk_Text_Iter;
      End_Iter    : out Text_Iter.Gtk_Text_Iter;
      Valid_Range : out Boolean) is
   begin
      --  Check the validity of the range
      if End_Pos >= 0 and then End_Pos <= Start_Pos then
         Valid_Range := False;
         return;
      end if;

      Valid_Range := True;

      --  Get Start_Iter
      if Start_Pos < 0 then
         Text_Buffer.Get_Iter_At_Offset (Buffer, Start_Iter, Char_Offset => 0);
      else
         Text_Buffer.Get_Iter_At_Offset (Buffer, Start_Iter, Start_Pos);
      end if;

      --  Get End_Iter
      if End_Pos < 0 then
         Text_Buffer.Get_End_Iter (Buffer, End_Iter);
      else
         Text_Buffer.Get_Iter_At_Offset (Buffer, End_Iter, End_Pos);
      end if;
   end Get_Range_Iter;

   ----------------
   -- Changed_Cb --
   ----------------

   procedure Changed_Cb
     (Widget : access Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Text   : Gtk_Text) is
   begin
      Text_Callback.Emit_By_Name (Object => Text, Name => "changed");
   end Changed_Cb;

   --------------------------
   -- Delete_Range_Handler --
   --------------------------

   procedure Delete_Range_Handler
     (Widget : access Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      Text   : Gtk_Text)
   is
      Start_Pos : constant Gint :=
        Text_Iter.Get_Offset
          (Text_Iter.Get_Text_Iter (Glib.Values.Nth (Params, 1)));
      End_Pos   : constant Gint :=
        Text_Iter.Get_Offset
          (Text_Iter.Get_Text_Iter (Glib.Values.Nth (Params, 2)));

      procedure Emit_By_Name
        (Object    : System.Address;
         Name      : String;
         Start_Pos : Gint;
         End_Pos   : Gint);
      pragma Import (C, Emit_By_Name, "g_signal_emit_by_name");

   begin
      Emit_By_Name
        (Get_Object (Text), "delete_text" & ASCII.NUL, Start_Pos, End_Pos);
   end Delete_Range_Handler;

   -------------------------
   -- Insert_Text_Handler --
   -------------------------

   procedure Insert_Text_Handler
     (Widget : access Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      Text   : Gtk_Text)
   is
      Text_Length : constant Gint :=
        Glib.Values.Get_Int (Value => Glib.Values.Nth (Params, 3));
      Str         : constant String :=
        Glib.Values.Get_String
          (Value => Glib.Values.Nth (Params, 2), Length => Text_Length);
      Position    : aliased Gint;

      procedure Emit_By_Name
        (Object   : System.Address;
         Name     : String;
         Text     : String;
         Length   : Gint;
         Position : System.Address);
      pragma Import (C, Emit_By_Name, "g_signal_emit_by_name");

   begin
      if Text.Handling_Insert_Text then
         return;
      end if;

      Position :=
        Text_Iter.Get_Offset
          (Text_Iter.Get_Text_Iter (Glib.Values.Nth (Params, 1)));

      --  Avoid infinite recursion

      Text.Handling_Insert_Text := True;
      Emit_By_Name
        (Get_Object (Text), "insert_text" & ASCII.NUL,
         Str, Text_Length, Position'Address);
      Text.Handling_Insert_Text := False;
   end Insert_Text_Handler;

   -------------------------
   -- Move_Cursor_Handler --
   -------------------------

   procedure Move_Cursor_Handler
     (Widget : access Gtk_Text_Record'Class;
      Params : Glib.Values.GValues)
   is
      Buffer : constant Text_Buffer.Gtk_Text_Buffer := Get_Buffer (Widget);
      X      : constant Gint :=
        Glib.Values.Get_Int (Glib.Values.Nth (Params, 1));
      Y      : constant Gint :=
        Glib.Values.Get_Int (Glib.Values.Nth (Params, 2));
      Iter, End_Iter : Text_Iter.Gtk_Text_Iter;
      Line, Offset, Last_Line_Index : Gint;

   begin
      Text_Buffer.Get_Iter_At_Mark
        (Buffer, Iter, Text_Buffer.Get_Insert (Buffer));
      Line := Text_Iter.Get_Line (Iter);
      Offset := Text_Iter.Get_Line_Offset (Iter);

      Text_Buffer.Get_End_Iter (Buffer, End_Iter);
      Last_Line_Index := Text_Iter.Get_Line (End_Iter);

      --  Move Iter to the new line, being careful not to go out of the buffer.
      Line := Line + Y;

      if Line < 0 then
         Line := 0;
      elsif Line > Last_Line_Index then
         Line := Last_Line_Index;
      end if;

      Text_Buffer.Get_Iter_At_Line (Buffer, Iter, Line);

      --  Move Iter to the new offset, being careful not to go out of the line.
      Offset := Offset + X;

      if Offset < 0 then
         Offset := 0;
      elsif Offset > Text_Iter.Get_Chars_In_Line (Iter) then
         Offset := Text_Iter.Get_Chars_In_Line (Iter);
      end if;

      Text_Iter.Set_Line_Offset (Iter, Offset);

      --  Now move the cursor to the position of Iter
      Text_Buffer.Place_Cursor (Buffer, Iter);
   end Move_Cursor_Handler;

   ------------------
   -- Move_Word_Cb --
   ------------------

   procedure Move_Word_Cb
     (Widget : access Gtk_Text_Record'Class;
      N      : Gint)
   is
      Buffer         : constant Text_Buffer.Gtk_Text_Buffer :=
        Get_Buffer (Widget);
      Iter           : Text_Iter.Gtk_Text_Iter;
      Ignored_Result : Boolean;

   begin
      if N = 0 then
         return;
      end if;

      Text_Buffer.Get_Iter_At_Mark
        (Buffer, Iter, Text_Buffer.Get_Insert (Buffer));

      if N > 0 then
         Text_Iter.Forward_Word_Ends (Iter, N, Ignored_Result);
      else
         Text_Iter.Backward_Word_Starts (Iter, -N, Ignored_Result);
      end if;

      Text_Buffer.Place_Cursor (Buffer, Iter);
   end Move_Word_Cb;

   ---------------------
   -- Set_Editable_Cb --
   ---------------------

   procedure Set_Editable_Cb
     (Widget      : access Gtk_Text_Record'Class;
      Is_Editable : Boolean) is
   begin
      Set_Editable (Widget, Is_Editable);
   end Set_Editable_Cb;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Text : out Gtk_Text;
      Hadj : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadj : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment) is
   begin
      Text := new Gtk_Text_Record;
      Initialize (Text, Hadj, Vadj);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Text : access Gtk_Text_Record'Class;
      Hadj : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadj : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment)
   is
      H_Adj : Gtk.Adjustment.Gtk_Adjustment := Hadj;
      V_Adj : Gtk.Adjustment.Gtk_Adjustment := Vadj;
      use type Adjustment.Gtk_Adjustment;
   begin
      Text_View.Initialize (Text, Buffer => null);

      --  The following call is required to initialize the class record,
      --  and the new signals created for this widget.
      --  Note also that we keep Class_Record, so that the memory allocation
      --  is done only once.
      Glib.Object.Initialize_Class_Record
        (Text, Signals, Class_Record, "GtkadaText", Signal_Parameters);

      --  If no Adjustment specified, then create new ones
      if H_Adj = Adjustment.Null_Adjustment then
         Adjustment.Gtk_New (H_Adj, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
      end if;

      if V_Adj = Adjustment.Null_Adjustment then
         Adjustment.Gtk_New (V_Adj, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
      end if;

      Set_Adjustments (Text, Hadj => H_Adj, Vadj => V_Adj);

      Buffer_Callback.Connect
        (Widget    => Get_Buffer (Text),
         Name      => "changed",
         Marsh     => Buffer_Callback.To_Marshaller (Changed_Cb'Access),
         User_Data => Text.all'Access);

      Buffer_Callback.Connect
        (Widget    => Get_Buffer (Text),
         Name      => "insert_text",
         Cb        => Insert_Text_Handler'Access,
         User_Data => Text.all'Access,
         After     => False);

      Buffer_Callback.Connect
        (Widget    => Get_Buffer (Text),
         Name      => "delete_range",
         Cb        => Delete_Range_Handler'Access,
         User_Data => Text.all'Access,
         After     => False);

      Text_Callback.Connect
        (Widget => Text,
         Name   => "set-editable",
         Marsh  => Text_Callback_Boolean_Marshaller.To_Marshaller
                     (Set_Editable_Cb'Access),
         After  => True);
      --  Connecting after will give the user a chance to override
      --  the default behavior. [1]

      Text_Callback.Connect
        (Widget => Text,
         Name   => "move_cursor_compat",
         Cb     => Move_Cursor_Handler'Access,
         After  => True);
      --  See [1] above.

      Text_Callback.Connect
        (Widget => Text,
         Name   => "move_word",
         Marsh  => Text_Callback.To_Marshaller (Move_Word_Cb'Access),
         After  => True);
      --  See [1] above.

      --  "move_page" signal handling not supported for the moment.???
   end Initialize;

   -------------
   -- Changed --
   -------------

   procedure Changed (Editable : access Gtk_Text_Record) is
   begin
      null;
      --  ??? Not implemented yet ???
   end Changed;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard
     (Editable : access Gtk_Text_Record;
      Time     : Guint32) is
   begin
      Text_Buffer.Copy_Clipboard (Get_Buffer (Editable));
   end Copy_Clipboard;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard
     (Editable : access Gtk_Text_Record;
      Time     : Guint32) is
   begin
      Text_Buffer.Cut_Clipboard
        (Get_Buffer (Editable), Default_Editable => True);
   end Cut_Clipboard;

   ----------------------
   -- Delete_Selection --
   ----------------------

   procedure Delete_Selection (Editable : access Gtk_Text_Record) is
      Dummy : Boolean;
   begin
      Dummy :=
        Text_Buffer.Delete_Selection
          (Get_Buffer (Editable),
           Interactive => False,
           Default_Editable => True);
   end Delete_Selection;

   -----------------
   -- Delete_Text --
   -----------------

   procedure Delete_Text
     (Editable  : access Gtk_Text_Record;
      Start_Pos : Gint := 0;
      End_Pos   : Gint := -1)
   is
      Start_Iter     : Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter       : Gtk.Text_Iter.Gtk_Text_Iter;
      Buffer         : constant Text_Buffer.Gtk_Text_Buffer :=
        Get_Buffer (Editable);
      Range_Is_Valid : Boolean;

   begin
      Get_Range_Iter
        (Buffer, Start_Pos, End_Pos, Start_Iter, End_Iter, Range_Is_Valid);

      if not Range_Is_Valid then
         return;
      end if;

      Text_Buffer.Delete (Buffer, Start_Iter, End_Iter);
   end Delete_Text;

   function Get_Chars
     (Editable  : access Gtk_Text_Record;
      Start_Pos : Gint := 0;
      End_Pos   : Gint := -1) return String
   is
      Start_Iter     : Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter       : Gtk.Text_Iter.Gtk_Text_Iter;
      Buffer         : constant Text_Buffer.Gtk_Text_Buffer :=
        Get_Buffer (Editable);
      Range_Is_Valid : Boolean;

   begin
      Get_Range_Iter
        (Buffer, Start_Pos, End_Pos, Start_Iter, End_Iter, Range_Is_Valid);

      if not Range_Is_Valid then
         return "";
      end if;

      return Text_Buffer.Get_Text (Buffer, Start_Iter, End_Iter);
   end Get_Chars;

   --  function Get_Has_Selection
   --    (Widget : access Gtk_Text_Record) return Boolean;
   --  Not implemented.

   ---------------------------
   -- Get_Selection_End_Pos --
   ---------------------------

   function Get_Selection_End_Pos
     (Widget : access Gtk_Text_Record) return Guint
   is
      Start_Iter       : Text_Iter.Gtk_Text_Iter;
      End_Iter         : Text_Iter.Gtk_Text_Iter;
      Selection_Exists : Boolean;
      Buffer           : constant Text_Buffer.Gtk_Text_Buffer :=
        Get_Buffer (Widget);

   begin
      Text_Buffer.Get_Selection_Bounds
        (Buffer, Start_Iter, End_Iter, Selection_Exists);

      if Selection_Exists then
         return Guint (Text_Iter.Get_Offset (End_Iter));
      else
         return 0;
      end if;
   end Get_Selection_End_Pos;

   -----------------------------
   -- Get_Selection_Start_Pos --
   -----------------------------

   function Get_Selection_Start_Pos
     (Widget : access Gtk_Text_Record) return Guint
   is
      Start_Iter       : Text_Iter.Gtk_Text_Iter;
      End_Iter         : Text_Iter.Gtk_Text_Iter;
      Selection_Exists : Boolean;
      Buffer           : constant Text_Buffer.Gtk_Text_Buffer :=
        Get_Buffer (Widget);

   begin
      Text_Buffer.Get_Selection_Bounds
        (Buffer, Start_Iter, End_Iter, Selection_Exists);

      if Selection_Exists then
         return Guint (Text_Iter.Get_Offset (Start_Iter));
      else
         return 0;
      end if;
   end Get_Selection_Start_Pos;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
     (Editable : access Gtk_Text_Record;
      New_Text : String;
      Position : in out Gint)
   is
      Buffer     : constant Text_Buffer.Gtk_Text_Buffer :=
        Get_Buffer (Editable);
      Start_Iter : Text_Iter.Gtk_Text_Iter;

   begin
      Text_Buffer.Get_Iter_At_Offset (Buffer, Start_Iter, Position);
      Text_Buffer.Insert (Buffer, Start_Iter, New_Text);
   end Insert_Text;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard
     (Editable : access Gtk_Text_Record;
      Time     : Guint32) is
   begin
      Text_Buffer.Paste_Clipboard
        (Get_Buffer (Editable), Default_Editable => True);
   end Paste_Clipboard;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
     (Editable : access Gtk_Text_Record;
      Start    : Gint;
      The_End  : Gint := -1)
   is
      Buffer         : constant Text_Buffer.Gtk_Text_Buffer :=
        Get_Buffer (Editable);
      Selection_Mark : Text_Mark.Gtk_Text_Mark;
      Insert_Mark    : Text_Mark.Gtk_Text_Mark;
      Start_Iter     : Text_Iter.Gtk_Text_Iter;
      End_Iter       : Text_Iter.Gtk_Text_Iter;
      Range_Is_Valid : Boolean;

   begin
      Get_Range_Iter
        (Buffer, Start, The_End, Start_Iter, End_Iter, Range_Is_Valid);

      if not Range_Is_Valid then
         return;
      end if;

      Selection_Mark := Text_Buffer.Get_Selection_Bound (Buffer);
      Insert_Mark := Text_Buffer.Get_Insert (Buffer);

      Text_Buffer.Move_Mark (Buffer, Selection_Mark, Start_Iter);
      Text_Buffer.Move_Mark (Buffer, Insert_Mark, End_Iter);
   end Select_Region;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Editable : access Gtk_Text_Record;
      Position : Gint)
   is
      Buffer : constant Text_Buffer.Gtk_Text_Buffer := Get_Buffer (Editable);
      Iter   : Text_Iter.Gtk_Text_Iter;

   begin
      Text_Buffer.Get_Iter_At_Offset (Buffer, Iter, Position);
      Text_Buffer.Place_Cursor (Buffer, Iter);
   end Set_Position;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position (Editable : access Gtk_Text_Record) return Gint is
      Buffer      : constant Text_Buffer.Gtk_Text_Buffer :=
        Get_Buffer (Editable);
      Insert_Mark : Text_Mark.Gtk_Text_Mark := Text_Buffer.Get_Insert (Buffer);
      Insert_Iter : Text_Iter.Gtk_Text_Iter;

   begin
      Text_Buffer.Get_Iter_At_Mark (Buffer, Insert_Iter, Insert_Mark);
      return Text_Iter.Get_Offset (Insert_Iter);
   end Get_Position;

   -------------------
   -- Get_Text_Area --
   -------------------

   function Get_Text_Area
     (Text : access Gtk_Text_Record) return Gdk.Gdk_Window is
   begin
      return Get_Window (Text, Enums.Text_Window_Text);
   end Get_Text_Area;

   ---------------------
   -- Backward_Delete --
   ---------------------

   function Backward_Delete
     (Text : access Gtk_Text_Record; Nchars : in Guint) return Boolean
   is
      Buffer        : constant Text_Buffer.Gtk_Text_Buffer :=
        Get_Buffer (Text);
      Insert_Iter   : Text_Iter.Gtk_Text_Iter;
      Start_Iter    : Text_Iter.Gtk_Text_Iter;
      Insert_Offset : Gint;

   begin
      Text_Buffer.Get_Iter_At_Mark
        (Buffer, Insert_Iter, Text_Buffer.Get_Insert (Buffer));
      Insert_Offset := Text_Iter.Get_Offset (Insert_Iter);

      if Insert_Offset < Gint (Nchars) then
         return False;
      end if;

      Text_Buffer.Get_Iter_At_Offset
        (Buffer, Start_Iter, Char_Offset => Insert_Offset - Gint (Nchars));
      Text_Buffer.Delete (Buffer, Start_Iter, Insert_Iter);

      return True;
   end Backward_Delete;

   --------------------
   -- Forward_Delete --
   --------------------

   function Forward_Delete
     (Text   : access Gtk_Text_Record;
      Nchars : in Guint) return Boolean
   is
      Buffer        : constant Text_Buffer.Gtk_Text_Buffer :=
        Get_Buffer (Text);
      Insert_Iter   : Text_Iter.Gtk_Text_Iter;
      End_Iter      : Text_Iter.Gtk_Text_Iter;
      Insert_Offset : Gint;

   begin
      Text_Buffer.Get_Iter_At_Mark
        (Buffer, Insert_Iter, Text_Buffer.Get_Insert (Buffer));
      Insert_Offset := Text_Iter.Get_Offset (Insert_Iter);

      if Gint (Nchars) > Gint (Get_Length (Text)) - Insert_Offset then
         return False;
      end if;

      Text_Buffer.Get_Iter_At_Offset
        (Buffer, End_Iter, Char_Offset => Insert_Offset + Gint (Nchars));
      Text_Buffer.Delete (Buffer, Insert_Iter, End_Iter);

      return True;
   end Forward_Delete;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Text : access Gtk_Text_Record) is
   begin
      null;
      --  ??? Not implemented yet.
   end Freeze;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Text : access Gtk_Text_Record) is
   begin
      null;
      --  ??? Not implemented yet.
   end Thaw;

   --------------
   -- Get_Hadj --
   --------------

   function Get_Hadj
     (Text : access Gtk_Text_Record) return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Text : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_text_view_get_hadj");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Text)), Stub));
   end Get_Hadj;

   --------------
   -- Get_Vadj --
   --------------

   function Get_Vadj
     (Text : access Gtk_Text_Record) return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Text : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_text_view_get_vadj");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Text)), Stub));
   end Get_Vadj;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length (Text : access Gtk_Text_Record) return Guint is
   begin
      return Guint (Text_Buffer.Get_Char_Count (Get_Buffer (Text)));
   end Get_Length;

   ---------------
   -- Set_Point --
   ---------------

   procedure Set_Point
     (Text  : access Gtk_Text_Record;
      Index : Guint) is
   begin
      Set_Position (Text, Gint (Index));
   end Set_Point;

   ---------------
   -- Get_Point --
   ---------------

   function Get_Point (Text : access Gtk_Text_Record) return Guint is
      Buffer      : constant Text_Buffer.Gtk_Text_Buffer := Get_Buffer (Text);
      Insert_Iter : Text_Iter.Gtk_Text_Iter;
   begin
      Text_Buffer.Get_Iter_At_Mark
        (Buffer, Insert_Iter, Text_Buffer.Get_Insert (Buffer));
      return Guint (Text_Iter.Get_Offset (Insert_Iter));
   end Get_Point;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Text   : access Gtk_Text_Record;
      Font   : Gdk.Font.Gdk_Font := Gdk.Font.Null_Font;
      Fore   : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Back   : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Chars  : String := "";
      Length : Gint := -1) is
   begin
      Insert
        (Text, Font_Desc => null,
         Fore => Fore, Back => Back,
         Chars => Chars, Length => Length);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Text      : access Gtk_Text_Record;
      Font_Desc : Pango.Font.Pango_Font_Description := null;
      Fore      : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Back      : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Chars     : String := "";
      Length    : Gint := -1)
   is
      Buffer        : constant Text_Buffer.Gtk_Text_Buffer :=
        Get_Buffer (Text);
      Insert_Mark   : Text_Mark.Gtk_Text_Mark :=
        Text_Buffer.Get_Insert (Buffer);
      Start_Mark    : Text_Mark.Gtk_Text_Mark;
      Total_Length  : Natural;
      Start_Iter    : Text_Iter.Gtk_Text_Iter;
      End_Iter      : Text_Iter.Gtk_Text_Iter;
      Text_Prop_Tag : Text_Tag.Gtk_Text_Tag;

      use type Gdk.Color.Gdk_Color;

   begin
      if Length < 0 then
         Total_Length := Chars'Length;
      else
         Total_Length := Natural (Length);
      end if;

      --  Create an anonymous mark at the point of insertion to be able
      --  to change the color properties of the inserted text. We do this
      --  only if specific colors were specified.

      if Font_Desc /= null
        or else Fore /= Gdk.Color.Null_Color
        or else Back /= Gdk.Color.Null_Color
      then
         Text_Buffer.Get_Iter_At_Mark (Buffer, Start_Iter, Insert_Mark);
         Start_Mark :=
           Text_Buffer.Create_Mark (Buffer, Where => Start_Iter);
      end if;

      Text_Buffer.Insert_At_Cursor
        (Buffer, Chars (Chars'First .. Chars'First + Total_Length - 1));

      --  Create an anonymous tag to change the text properties if necessary.
      --  (note that we ignore the Font since the tags mechanism of the
      --  text_view widget does not support the usage of Gdk_Font).
      if Font_Desc /= null
        or else Fore /= Gdk.Color.Null_Color
        or else Back /= Gdk.Color.Null_Color
      then
         Text_Tag.Gtk_New (Text_Prop_Tag);

         if Font_Desc /= null then
            Set_Property (Text_Prop_Tag, Font_Desc_Property, Font_Desc);
         end if;

         if Fore /= Gdk.Color.Null_Color then
            Set_Property (Text_Prop_Tag, Foreground_Gdk_Property, Fore);
         end if;

         if Back /= Gdk.Color.Null_Color then
            Set_Property (Text_Prop_Tag, Background_Gdk_Property, Back);
         end if;

         --  Get iter at location before text insertion from saved Start_Mark
         Text_Buffer.Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);

         --  Get iter at current cursor position
         Insert_Mark := Text_Buffer.Get_Insert (Buffer);
         Text_Buffer.Get_Iter_At_Mark (Buffer, End_Iter, Insert_Mark);

         --  And apply the text properties tag between those two iterators.
         Text_Buffer.Apply_Tag (Buffer, Text_Prop_Tag, Start_Iter, End_Iter);
      end if;
   end Insert;

   ---------------------
   -- Set_Adjustments --
   ---------------------

   procedure Set_Adjustments
     (Text : access Gtk_Text_Record;
      Hadj : Gtk.Adjustment.Gtk_Adjustment;
      Vadj : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (Text : System.Address;
         Name : String;
         Hadj : System.Address;
         Vadj : System.Address);
      pragma Import (C, Internal, "g_signal_emit_by_name");
      Hadj_Addr : System.Address := System.Null_Address;
      Vadj_Addr : System.Address := System.Null_Address;
      use type Adjustment.Gtk_Adjustment;
   begin
      if Hadj /= Adjustment.Null_Adjustment then
         Hadj_Addr := Get_Object (Hadj);
      end if;
      if Vadj /= Adjustment.Null_Adjustment then
         Vadj_Addr := Get_Object (Vadj);
      end if;
      Internal
        (Get_Object (Text), "set_scroll_adjustments" & ASCII.NUL,
         Hadj_Addr, Vadj_Addr);
   end Set_Adjustments;

   -------------------
   -- Set_Line_Wrap --
   -------------------

   procedure Set_Line_Wrap
     (Text      : access Gtk_Text_Record;
      Line_Wrap : Boolean := True)
   is
      Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode := Get_Wrap_Mode (Text);
   begin
      if Line_Wrap then
         case Wrap_Mode is
            when Gtk.Enums.Wrapmode_None =>
               Wrap_Mode := Gtk.Enums.Wrapmode_Char;
            when Gtk.Enums.Wrapmode_Char |
                 Gtk.Enums.Wrapmode_Word =>
               null;
         end case;

      else
         Wrap_Mode := Gtk.Enums.Wrapmode_None;
      end if;

      Set_Wrap_Mode (Text, Wrap_Mode);
   end Set_Line_Wrap;

   -------------------
   -- Set_Word_Wrap --
   -------------------

   procedure Set_Word_Wrap
     (Text      : access Gtk_Text_Record;
      Word_Wrap : Boolean := True)
   is
      Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode := Get_Wrap_Mode (Text);
   begin
      if Word_Wrap then
         Wrap_Mode := Gtk.Enums.Wrapmode_Word;
      else
         case Wrap_Mode is
            when Gtk.Enums.Wrapmode_None |
                 Gtk.Enums.Wrapmode_Char =>
               null;
            when Gtk.Enums.Wrapmode_Word =>
               Wrap_Mode := Gtk.Enums.Wrapmode_Char;
         end case;
      end if;

      Set_Wrap_Mode (Text, Wrap_Mode);
   end Set_Word_Wrap;

end Gtk.Text;
