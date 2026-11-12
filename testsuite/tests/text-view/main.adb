--  GtkAda-side coverage for Gtk.Text_View. GTK has no standalone textview.c
--  test, so this exercises the public TextView API directly, including
--  properties, geometry, child operations, accessibility, and signals. The
--  child-anchor cases from textbuffer.c are covered by text-buffer.
--
--  Geometry tests map a window under Xvfb. signals.c emits representative
--  GObject signals to exercise Ada callback marshalling.

with Ada.Command_Line;
with System;

with Glib;                  use Glib;
with Glib.Main;
with Glib.Menu;
with Glib.Menu_Model;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;

with Gdk.Rectangle;
with Gtk.Accessible;
with Gtk.Adjustment;
with Gtk.Atcontext;
with Gtk.Border;
with Gtk.Label;
with Gtk.Main;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Text_Buffer;       use Gtk.Text_Buffer;
with Gtk.Text_Child_Anchor;
with Gtk.Text_Iter;         use Gtk.Text_Iter;
with Gtk.Text_Mark;         use Gtk.Text_Mark;
with Gtk.Text_View;         use Gtk.Text_View;
with Gtk.Widget;
with Gtk.Window;
with Pango.Context;
with Pango.Tabs;

use type Gtk.Adjustment.Gtk_Adjustment;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Accessible.Gtk_Accessible_Role;
use type Gtk.Label.Gtk_Label;
use type Gtk.Text_Buffer.Gtk_Text_Buffer;
use type Gtk.Text_View.Gtk_Text_View;
use type Gtk.Widget.Gtk_Widget;
use type Glib.Menu_Model.Gmenu_Model;
use type Pango.Context.Pango_Context;
use type Pango.Tabs.Pango_Tab_Align;

procedure Main is

   Callback_Count : Gint := 0;

   --  These tiny callbacks only count deliveries. The signal test below
   --  connects both Gtk_Text_View callbacks and generic GObject slots.
   procedure Signal_Void (Self : access Gtk_Text_View_Record'Class);

   procedure Slot_Void (Self : access GObject_Record'Class);

   --  Delete callbacks accept the enum and count passed through GObject.
   procedure Signal_Delete
     (Self     : access Gtk_Text_View_Record'Class;
      The_Type : Gtk.Enums.Gtk_Delete_Type;
      Count    : Gint);

   procedure Slot_Delete
     (Self     : access GObject_Record'Class;
      The_Type : Gtk.Enums.Gtk_Delete_Type;
      Count    : Gint);

   --  The extend-selection signal returns whether a handler accepted the range.
   function Signal_Extend
     (Self        : access Gtk_Text_View_Record'Class;
      Granularity : Gtk_Text_Extend_Selection;
      Location    : Gtk_Text_Iter;
      Start       : Gtk_Text_Iter;
      The_End     : Gtk_Text_Iter) return Boolean;

   function Slot_Extend
     (Self        : access GObject_Record'Class;
      Granularity : Gtk_Text_Extend_Selection;
      Location    : Gtk_Text_Iter;
      Start       : Gtk_Text_Iter;
      The_End     : Gtk_Text_Iter) return Boolean;

   --  String callbacks cover both UTF-8 argument marshallers.
   procedure Signal_String
     (Self   : access Gtk_Text_View_Record'Class;
      String : UTF8_String);

   procedure Slot_String
     (Self   : access GObject_Record'Class;
      String : UTF8_String);

   --  Movement callbacks receive a step, count, and selection flag.
   procedure Signal_Movement
     (Self             : access Gtk_Text_View_Record'Class;
      Step             : Gtk.Enums.Gtk_Movement_Step;
      Count            : Gint;
      Extend_Selection : Boolean);

   procedure Slot_Movement
     (Self             : access GObject_Record'Class;
      Step             : Gtk.Enums.Gtk_Movement_Step;
      Count            : Gint;
      Extend_Selection : Boolean);

   --  Viewport callbacks use GtkScrollStep and an integer count.
   procedure Signal_Scroll
     (Self  : access Gtk_Text_View_Record'Class;
      Step  : Gtk.Enums.Gtk_Scroll_Step;
      Count : Gint);

   procedure Slot_Scroll
     (Self  : access GObject_Record'Class;
      Step  : Gtk.Enums.Gtk_Scroll_Step;
      Count : Gint);

   --  Select-all callbacks receive the requested selection state.
   procedure Signal_Select
     (Self       : access Gtk_Text_View_Record'Class;
      Gtk_Select : Boolean);

   procedure Slot_Select
     (Self       : access GObject_Record'Class;
      Gtk_Select : Boolean);

   procedure Emit_Text_View_Signals (View : System.Address);
   pragma Import
     (C, Emit_Text_View_Signals, "gtkada_test_emit_text_view_signals");

   procedure Test_New_And_Buffer
   with Convention => C;

   procedure Test_Properties
   with Convention => C;

   procedure Test_Scroll
   with Convention => C;

   procedure Test_Iter_At_Position
   with Convention => C;

   procedure Test_Factories_And_Extended_Properties
   with Convention => C;

   procedure Test_Layout_And_Child_Operations
   with Convention => C;

   procedure Test_Accessibility_And_Scrollable
   with Convention => C;

   procedure Test_Signal_Connections
   with Convention => C;

   procedure Signal_Void (Self : access Gtk_Text_View_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Callback_Count := Callback_Count + 1;
   end Signal_Void;

   procedure Slot_Void (Self : access GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Callback_Count := Callback_Count + 1;
   end Slot_Void;

   procedure Signal_Delete
     (Self     : access Gtk_Text_View_Record'Class;
      The_Type : Gtk.Enums.Gtk_Delete_Type;
      Count    : Gint) is
      pragma Unreferenced (Self, The_Type, Count);
   begin
      Callback_Count := Callback_Count + 1;
   end Signal_Delete;

   procedure Slot_Delete
     (Self     : access GObject_Record'Class;
      The_Type : Gtk.Enums.Gtk_Delete_Type;
      Count    : Gint) is
      pragma Unreferenced (Self, The_Type, Count);
   begin
      Callback_Count := Callback_Count + 1;
   end Slot_Delete;

   function Signal_Extend
     (Self        : access Gtk_Text_View_Record'Class;
      Granularity : Gtk_Text_Extend_Selection;
      Location    : Gtk_Text_Iter;
      Start       : Gtk_Text_Iter;
      The_End     : Gtk_Text_Iter) return Boolean is
      pragma Unreferenced (Self, Granularity, Location, Start, The_End);
   begin
      Callback_Count := Callback_Count + 1;
      --  Returning True marks the selection request as handled before GTK's
      --  default handler tries to use its own iterator state.
      return True;
   end Signal_Extend;

   function Slot_Extend
     (Self        : access GObject_Record'Class;
      Granularity : Gtk_Text_Extend_Selection;
      Location    : Gtk_Text_Iter;
      Start       : Gtk_Text_Iter;
      The_End     : Gtk_Text_Iter) return Boolean is
      pragma Unreferenced (Self, Granularity, Location, Start, The_End);
   begin
      Callback_Count := Callback_Count + 1;
      return False;
   end Slot_Extend;

   procedure Signal_String
     (Self   : access Gtk_Text_View_Record'Class;
      String : UTF8_String) is
      pragma Unreferenced (Self, String);
   begin
      Callback_Count := Callback_Count + 1;
   end Signal_String;

   procedure Slot_String
     (Self   : access GObject_Record'Class;
      String : UTF8_String) is
      pragma Unreferenced (Self, String);
   begin
      Callback_Count := Callback_Count + 1;
   end Slot_String;

   procedure Signal_Movement
     (Self             : access Gtk_Text_View_Record'Class;
      Step             : Gtk.Enums.Gtk_Movement_Step;
      Count            : Gint;
      Extend_Selection : Boolean) is
      pragma Unreferenced (Self, Step, Count, Extend_Selection);
   begin
      Callback_Count := Callback_Count + 1;
   end Signal_Movement;

   procedure Slot_Movement
     (Self             : access GObject_Record'Class;
      Step             : Gtk.Enums.Gtk_Movement_Step;
      Count            : Gint;
      Extend_Selection : Boolean) is
      pragma Unreferenced (Self, Step, Count, Extend_Selection);
   begin
      Callback_Count := Callback_Count + 1;
   end Slot_Movement;

   procedure Signal_Scroll
     (Self  : access Gtk_Text_View_Record'Class;
      Step  : Gtk.Enums.Gtk_Scroll_Step;
      Count : Gint) is
      pragma Unreferenced (Self, Step, Count);
   begin
      Callback_Count := Callback_Count + 1;
   end Signal_Scroll;

   procedure Slot_Scroll
     (Self  : access GObject_Record'Class;
      Step  : Gtk.Enums.Gtk_Scroll_Step;
      Count : Gint) is
      pragma Unreferenced (Self, Step, Count);
   begin
      Callback_Count := Callback_Count + 1;
   end Slot_Scroll;

   procedure Signal_Select
     (Self       : access Gtk_Text_View_Record'Class;
      Gtk_Select : Boolean) is
      pragma Unreferenced (Self, Gtk_Select);
   begin
      Callback_Count := Callback_Count + 1;
   end Signal_Select;

   procedure Slot_Select
     (Self       : access GObject_Record'Class;
      Gtk_Select : Boolean) is
      pragma Unreferenced (Self, Gtk_Select);
   begin
      Callback_Count := Callback_Count + 1;
   end Slot_Select;

   --------------------------
   -- Test_New_And_Buffer --
   --------------------------

   --  A freshly created view owns a buffer; Set_Buffer swaps it, and the
   --  buffer-taking constructor honours the buffer it is given.

   procedure Test_New_And_Buffer is
      View   : Gtk_Text_View;
      Buffer : Gtk_Text_Buffer;
   begin
      Gtk_New (View);
      Assert_True (View.Get_Buffer /= null);

      Gtk_New (Buffer);
      View.Set_Buffer (Buffer);
      Assert_True (View.Get_Buffer = Buffer);

      View.Ref_Sink;

      --  The buffer-taking constructor.
      declare
         View2 : Gtk_Text_View;
      begin
         Gtk_New (View2, Buffer);
         Assert_True (View2.Get_Buffer = Buffer);
         View2.Ref_Sink;
      end;
   end Test_New_And_Buffer;

   ---------------------
   -- Test_Properties --
   ---------------------

   --  Round-trip the common editability, cursor, wrap-mode, monospace and
   --  margin properties.

   procedure Test_Properties is
      View : Gtk_Text_View;
   begin
      Gtk_New (View);

      View.Set_Editable (False);
      Assert_False (View.Get_Editable);
      View.Set_Editable (True);
      Assert_True (View.Get_Editable);

      View.Set_Cursor_Visible (False);
      Assert_False (View.Get_Cursor_Visible);
      View.Set_Cursor_Visible (True);
      Assert_True (View.Get_Cursor_Visible);

      View.Set_Wrap_Mode (Wrap_Word);
      Assert_True (View.Get_Wrap_Mode = Wrap_Word);
      View.Set_Wrap_Mode (Wrap_Char);
      Assert_True (View.Get_Wrap_Mode = Wrap_Char);

      View.Set_Monospace (True);
      Assert_True (View.Get_Monospace);

      View.Set_Left_Margin (7);
      Assert_Cmpint_Eq (View.Get_Left_Margin, 7);
      View.Set_Right_Margin (9);
      Assert_Cmpint_Eq (View.Get_Right_Margin, 9);
      View.Set_Top_Margin (11);
      Assert_Cmpint_Eq (View.Get_Top_Margin, 11);

      View.Ref_Sink;
   end Test_Properties;

   -----------------
   -- Test_Scroll --
   -----------------

   --  Scroll_To_Mark / Scroll_Mark_Onscreen against a mark in the buffer.
   --  Without a realized window these have no visible effect, but they
   --  exercise the bindings end-to-end.

   procedure Test_Scroll is
      View   : Gtk_Text_View;
      Buffer : Gtk_Text_Buffer;
      Iter   : Gtk_Text_Iter;
      Mark   : Gtk_Text_Mark;
   begin
      Gtk_New (View);
      Buffer := View.Get_Buffer;

      Buffer.Set_Text
        ("line one" & ASCII.LF & "line two" & ASCII.LF & "line three");

      Buffer.Get_End_Iter (Iter);
      Mark := Buffer.Create_Mark ("end-mark", Iter, Left_Gravity => True);

      View.Scroll_Mark_Onscreen (Mark);
      View.Scroll_To_Mark
        (Mark,
         Within_Margin => 0.0,
         Use_Align     => True,
         Xalign        => 0.0,
         Yalign        => 1.0);

      Assert_False (Mark.Get_Deleted);

      View.Ref_Sink;
   end Test_Scroll;

   ----------------------------
   -- Test_Iter_At_Position --
   ----------------------------

   --  Trailing is an optional output: the C side accepts a NULL for it, so
   --  the binding must let a caller decline it and return the same iterator
   --  either way.

   procedure Test_Iter_At_Position is
      View     : Gtk_Text_View;
      Buffer   : Gtk_Text_Buffer;
      Iter     : Gtk_Text_Iter;
      Iter2    : Gtk_Text_Iter;
      Trailing : aliased Gint := -1;
      Dummy    : Boolean;
   begin
      Gtk_New (View);
      Buffer := View.Get_Buffer;
      Buffer.Set_Text ("line one" & ASCII.LF & "line two");

      Dummy := View.Get_Iter_At_Position (Iter, Trailing'Access, 0, 0);
      Assert_True (Trailing >= 0);

      Dummy := View.Get_Iter_At_Position (Iter2, null, 0, 0);
      Assert_Cmpint_Eq (Get_Offset (Iter2), Get_Offset (Iter));

      --  Trailing defaults to null, so it can also just be left out.
      Dummy := View.Get_Iter_At_Position (Iter2, X => 0, Y => 0);
      Assert_Cmpint_Eq (Get_Offset (Iter2), Get_Offset (Iter));

      View.Ref_Sink;
   end Test_Iter_At_Position;

   ------------------------------------------
   -- Test_Factories_And_Extended_Properties --
   ------------------------------------------

   --  Exercise the convenience constructors and property pairs that are not
   --  part of the common editability and wrap-mode checks above.

   procedure Test_Factories_And_Extended_Properties is
      Buffer     : Gtk_Text_Buffer;
      View       : Gtk_Text_View;
      View2      : Gtk_Text_View;
      View3      : Gtk_Text_View;
      Menu       : Glib.Menu.Gmenu;
      Gutter     : Gtk.Label.Gtk_Label;
      H_Adjust   : Gtk.Adjustment.Gtk_Adjustment;
      V_Adjust   : Gtk.Adjustment.Gtk_Adjustment;
      Window     : Gtk.Window.Gtk_Window;
      Tabs       : Pango.Tabs.Pango_Tab_Array;
      Read_Tabs  : Pango.Tabs.Pango_Tab_Array;
      Alignment  : Pango.Tabs.Pango_Tab_Align;
      Location   : Gint;
   begin
      Gtk_New (Buffer);

      --  Exercise both functional constructors as well as Gtk_New forms.
      View := Gtk_Text_View_New;
      Assert_True (View.Get_Buffer /= null);
      View2 := Gtk_Text_View_New_With_Buffer (Buffer);
      Assert_True (View2.Get_Buffer = Buffer);
      View3 := Gtk_Text_View_New;

      View3.Set_Accepts_Tab (False);
      Assert_False (View3.Get_Accepts_Tab);
      View3.Set_Accepts_Tab (True);
      Assert_True (View3.Get_Accepts_Tab);

      View3.Set_Bottom_Margin (13);
      Assert_Cmpint_Eq (View3.Get_Bottom_Margin, 13);
      View3.Set_Indent (-4);
      Assert_Cmpint_Eq (View3.Get_Indent, -4);

      --  Input hints are a bit flag; purpose and justification are enum values.
      View3.Set_Input_Hints (Input_Hint_Spellcheck);
      Assert_True (View3.Get_Input_Hints = Input_Hint_Spellcheck);
      View3.Set_Input_Purpose (Input_Purpose_Email);
      Assert_True (View3.Get_Input_Purpose = Input_Purpose_Email);
      View3.Set_Justification (Justify_Center);
      Assert_True (View3.Get_Justification = Justify_Center);

      View3.Set_Overwrite (True);
      Assert_True (View3.Get_Overwrite);
      View3.Set_Overwrite (False);
      Assert_False (View3.Get_Overwrite);

      View3.Set_Pixels_Above_Lines (2);
      Assert_Cmpint_Eq (View3.Get_Pixels_Above_Lines, 2);
      View3.Set_Pixels_Below_Lines (3);
      Assert_Cmpint_Eq (View3.Get_Pixels_Below_Lines, 3);
      View3.Set_Pixels_Inside_Wrap (4);
      Assert_Cmpint_Eq (View3.Get_Pixels_Inside_Wrap, 4);

      --  Extra menus and gutters are optional children; round-trip each one
      --  before clearing it from the view.
      Menu := Glib.Menu.Gmenu_New;
      View3.Set_Extra_Menu (Menu);
      Assert_True
        (View3.Get_Extra_Menu = Glib.Menu_Model.Gmenu_Model (Menu));
      View3.Set_Extra_Menu (null);
      Assert_True (View3.Get_Extra_Menu = null);

      --  Install the gutter before mapping the view, then read it back after
      --  GTK has allocated the left text window.
      Gutter := Gtk.Label.Gtk_Label_New ("line numbers");
      View3.Set_Gutter
        (Text_Window_Left, Gtk.Widget.Gtk_Widget (Gutter));
      Window := Gtk.Window.Gtk_Window_New;
      Window.Set_Child (View3);
      Window.Set_Default_Size (300, 160);
      Window.Present;
      for I in 1 .. 8 loop
         declare
            Dispatched : constant Boolean :=
              Glib.Main.Main_Context_Iteration (May_Block => False);
         begin
            pragma Unreferenced (Dispatched);
         end;
      end loop;

      --  GTK returns its internal gutter container rather than the supplied
      --  label, so verify that the returned widget belongs to this view.
      declare
         Current_Gutter : constant Gtk.Widget.Gtk_Widget :=
           View3.Get_Gutter (Text_Window_Left);
      begin
         Assert_True (Current_Gutter /= null);
         Assert_True
           (Gtk.Widget.Get_Parent (Current_Gutter) =
            Gtk.Widget.Gtk_Widget (View3));
      end;
      View3.Set_Gutter (Text_Window_Left, null);
      Assert_True (View3.Get_Gutter (Text_Window_Left) = null);

      --  Use a caller-owned tab array, then free the copy returned by Get_Tabs.
      Tabs := Pango.Tabs.Pango_Tab_Array_New (2, Positions_In_Pixels => True);
      Pango.Tabs.Set_Tab (Tabs, 0, Pango.Tabs.Pango_Tab_Left, 24);
      Pango.Tabs.Set_Tab (Tabs, 1, Pango.Tabs.Pango_Tab_Left, 48);
      View3.Set_Tabs (Tabs);
      Read_Tabs := View3.Get_Tabs;
      Assert_Cmpint_Eq (Pango.Tabs.Get_Size (Read_Tabs), 2);
      Pango.Tabs.Get_Tab (Read_Tabs, 1, Alignment, Location);
      Assert_True (Alignment = Pango.Tabs.Pango_Tab_Left);
      Assert_Cmpint_Eq (Location, 48);
      Pango.Tabs.Free (Read_Tabs);
      Pango.Tabs.Free (Tabs);

      --  GtkScrollable adjustments are reference-counted GObjects; the view
      --  keeps its own references after these setters return.
      H_Adjust := Gtk.Adjustment.Gtk_Adjustment_New (0.0, 0.0, 100.0, 1.0, 10.0);
      V_Adjust := Gtk.Adjustment.Gtk_Adjustment_New (0.0, 0.0, 200.0, 1.0, 20.0);
      View3.Set_Hadjustment (H_Adjust);
      View3.Set_Vadjustment (V_Adjust);
      Assert_True (View3.Get_Hadjustment = H_Adjust);
      Assert_True (View3.Get_Vadjustment = V_Adjust);
      View3.Set_Hscroll_Policy (Scroll_Natural);
      View3.Set_Vscroll_Policy (Scroll_Natural);
      Assert_True (View3.Get_Hscroll_Policy = Scroll_Natural);
      Assert_True (View3.Get_Vscroll_Policy = Scroll_Natural);

      Window.Destroy;
      View.Ref_Sink;
      View2.Ref_Sink;
      Buffer.Ref_Sink;
   end Test_Factories_And_Extended_Properties;

   --------------------------------------
   -- Test_Layout_And_Child_Operations --
   --------------------------------------

   --  Map a view with several lines so coordinate conversions and display-line
   --  queries run against an allocated Pango layout.

   procedure Test_Layout_And_Child_Operations is
      Window      : Gtk.Window.Gtk_Window;
      View        : Gtk_Text_View;
      Buffer      : Gtk_Text_Buffer;
      Start       : Gtk_Text_Iter;
      Finish      : Gtk_Text_Iter;
      At_Location : Gtk_Text_Iter;
      At_Position : Gtk_Text_Iter;
      At_Y        : Gtk_Text_Iter;
      Mark        : Gtk_Text_Mark;
      Anchor      : Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor;
      Overlay     : Gtk.Widget.Gtk_Widget;
      Child       : Gtk.Widget.Gtk_Widget;
      Location    : Gdk.Rectangle.Gdk_Rectangle;
      Strong      : Gdk.Rectangle.Gdk_Rectangle;
      Weak        : Gdk.Rectangle.Gdk_Rectangle;
      Visible     : Gdk.Rectangle.Gdk_Rectangle;
      Line_Top    : Gint;
      Line_Y      : Gint;
      Line_Height : Gint;
      Window_X    : Gint;
      Window_Y    : Gint;
      Buffer_X    : Gint;
      Buffer_Y    : Gint;
      X_Offset    : Gdouble;
      Y_Offset    : Gdouble;
      Found       : Boolean;
      Trailing    : aliased Gint := -1;
      Dummy       : Boolean;
   begin
      Window := Gtk.Window.Gtk_Window_New;
      View := Gtk_Text_View_New;
      Buffer := View.Get_Buffer;
      Buffer.Set_Text
        ("First line of text" & ASCII.LF &
         "Second line of text" & ASCII.LF &
         "Third line of text");
      Window.Set_Child (View);
      Window.Set_Default_Size (360, 220);
      Window.Present;

      --  Finish mapping before asking the view for geometry.
      for I in 1 .. 8 loop
         Dummy := Glib.Main.Main_Context_Iteration (May_Block => False);
      end loop;

      Buffer.Get_Start_Iter (Start);
      Buffer.Get_End_Iter (Finish);
      View.Get_Iter_Location (Start, Location);
      Assert_True (Location.Height > 0);
      Assert_True (View.Get_Ltr_Context /= null);
      Assert_True (View.Get_Rtl_Context /= null);

      Found := View.Get_Iter_At_Location
        (At_Location, Location.X, Location.Y);
      Assert_True (Found);
      Assert_True
        (Get_Offset (At_Location) >= 0 and
         Get_Offset (At_Location) <= Buffer.Get_Char_Count);

      Found := View.Get_Iter_At_Position
        (At_Position, Trailing'Access, Location.X, Location.Y);
      Assert_True (Found);
      Assert_True (Trailing >= 0);

      View.Get_Cursor_Locations (Start, Strong, Weak);
      Assert_True (Strong.Height > 0);
      Assert_True (Weak.Height > 0);

      View.Get_Line_At_Y (At_Y, Location.Y, Line_Top);
      View.Get_Line_Yrange (At_Y, Line_Y, Line_Height);
      Assert_True (Line_Height >= 0);
      Assert_True (Line_Top >= 0 and Line_Y >= 0);

      Assert_True (View.Starts_Display_Line (Start));
      Assert_True (View.Forward_Display_Line (Start));
      Assert_True (View.Forward_Display_Line_End (Start));
      Assert_True (View.Backward_Display_Line (Finish));
      Assert_True (View.Backward_Display_Line_Start (Finish));
      Assert_True (View.Move_Visually (Start, 1));

      View.Get_Visible_Rect (Visible);
      Assert_True (Visible.Width >= 0);
      View.Get_Visible_Offset (X_Offset, Y_Offset);
      Assert_True (X_Offset >= 0.0 and Y_Offset >= 0.0);

      View.Buffer_To_Window_Coords
        (Text_Window_Text, Visible.X, Visible.Y, Window_X, Window_Y);
      View.Window_To_Buffer_Coords
        (Text_Window_Text, Window_X, Window_Y, Buffer_X, Buffer_Y);
      Assert_Cmpint_Eq (Buffer_X, Visible.X);
      Assert_Cmpint_Eq (Buffer_Y, Visible.Y);

      Buffer.Get_End_Iter (Finish);
      Dummy := View.Scroll_To_Iter
        (Finish, Within_Margin => 0.1, Use_Align => True,
         Xalign => 0.0, Yalign => 1.0);
      Mark := Buffer.Create_Mark ("onscreen", Start, Left_Gravity => True);
      Dummy := View.Move_Mark_Onscreen (Mark);
      Dummy := View.Place_Cursor_Onscreen;

      --  An overlay can be repositioned while it remains visible.
      Overlay := Gtk.Widget.Gtk_Widget (Gtk.Label.Gtk_Label_New ("overlay"));
      View.Add_Overlay (Overlay, 2, 3);
      Assert_True (Gtk.Widget.Get_Visible (Overlay));
      View.Move_Overlay (Overlay, 5, 7);
      Assert_True (Gtk.Widget.Get_Visible (Overlay));

      --  Remove applies to widgets attached at text child anchors. Retaining
      --  the anchor lets the test verify the view dropped its child reference.
      Anchor := Buffer.Create_Child_Anchor (Start);
      Child := Gtk.Widget.Gtk_Widget (Gtk.Label.Gtk_Label_New ("anchored"));
      View.Add_Child_At_Anchor (Child, Anchor);
      Assert_Cmpuint_Eq
        (Gtk.Text_Child_Anchor.Get_Widgets (Anchor)'Length, 1);
      View.Remove (Child);
      Assert_Cmpuint_Eq
        (Gtk.Text_Child_Anchor.Get_Widgets (Anchor)'Length, 0);

      View.Reset_Cursor_Blink;
      View.Reset_Im_Context;
      Window.Destroy;
   end Test_Layout_And_Child_Operations;

   -------------------------------------
   -- Test_Accessibility_And_Scrollable --
   -------------------------------------

   --  Read the accessible role and bounds of a mapped view, then exercise its
   --  accessible update/reset operations and GtkScrollable interface.

   procedure Test_Accessibility_And_Scrollable is
      Window     : Gtk.Window.Gtk_Window;
      View       : Gtk_Text_View;
      Border     : Gtk.Border.Gtk_Border;
      X          : Gint;
      Y          : Gint;
      Width      : Gint;
      Height     : Gint;
      Has_Border : Boolean;
   begin
      Window := Gtk.Window.Gtk_Window_New;
      View := Gtk_Text_View_New;
      Window.Set_Child (View);
      Window.Set_Default_Size (300, 160);
      Window.Present;

      --  Pump without blocking until GTK has allocated the widget.
      for I in 1 .. 8 loop
         declare
            Dispatched : constant Boolean :=
              Glib.Main.Main_Context_Iteration (May_Block => False);
         begin
            pragma Unreferenced (Dispatched);
         end;
      end loop;

      Assert_Cmpstr_Eq (View.Get_Accessible_Id, "");
      Assert_True
        (View.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Text_Box);
      --  The coverage driver sets GTK_A11Y=none, so GTK has no platform
      --  accessibility context to return in this isolated test process.
      Assert_True (View.Get_At_Context = null);

      --  A theme may omit a scrollable border; a mapped widget has bounds.
      Has_Border := View.Get_Border (Border);
      if Has_Border then
         Assert_True (Border.Left >= 0 and Border.Right >= 0);
      end if;
      Assert_True (View.Get_Bounds (X, Y, Width, Height));
      Assert_True (Width >= 0 and Height >= 0);

      --  Accessible tree navigation is nullable for this leaf. Query all three
      --  methods and verify the view still reports its fixed text-box role.
      declare
         Parent  : constant Gtk.Accessible.Gtk_Accessible :=
           View.Get_Accessible_Parent;
         Child   : constant Gtk.Accessible.Gtk_Accessible :=
           View.Get_First_Accessible_Child;
         Sibling : constant Gtk.Accessible.Gtk_Accessible :=
           View.Get_Next_Accessible_Sibling;
      begin
         pragma Unreferenced (Parent, Child, Sibling);
         Assert_True
           (View.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Text_Box);
      end;

      --  Query the platform state without assuming whether the test backend
      --  considers a newly mapped widget focused.
      if View.Get_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable)
      then
         Assert_True
           (View.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Text_Box);
      else
         Assert_False
           (View.Get_Platform_State
              (Gtk.Accessible.Accessible_Platform_State_Focusable));
      end if;

      View.Announce
        ("Text view test announcement",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      View.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      View.Reset_Relation (Gtk.Accessible.Accessible_Relation_Labelled_By);
      View.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      View.Update_Caret_Position;
      View.Update_Contents
        (Accessible_Text_Content_Change_Insert, 0, 0);
      View.Update_Selection_Bound;
      View.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);
      View.Update_Next_Accessible_Sibling (Gtk.Accessible.Null_Gtk_Accessible);
      View.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);

      Window.Destroy;
   end Test_Accessibility_And_Scrollable;

   -----------------------------
   -- Test_Signal_Connections --
   -----------------------------

   --  Connect both generated callback forms for each signal. The C helper
   --  emits one signal for every marshaller signature with valid GTK values.

   procedure Test_Signal_Connections is
      View : Gtk_Text_View;
      Slot : Gtk.Label.Gtk_Label;
   begin
      View := Gtk_Text_View_New;
      Slot := Gtk.Label.Gtk_Label_New ("callback slot");
      View.Get_Buffer.Set_Text ("signal text");

      --  Void signals share a marshaller, but each public overload has its own
      --  generated connector and is registered here.
      View.On_Backspace (Signal_Void'Unrestricted_Access);
      View.On_Backspace (Slot_Void'Unrestricted_Access, Slot);
      View.On_Copy_Clipboard (Signal_Void'Unrestricted_Access);
      View.On_Copy_Clipboard (Slot_Void'Unrestricted_Access, Slot);
      View.On_Cut_Clipboard (Signal_Void'Unrestricted_Access);
      View.On_Cut_Clipboard (Slot_Void'Unrestricted_Access, Slot);
      View.On_Insert_Emoji (Signal_Void'Unrestricted_Access);
      View.On_Insert_Emoji (Slot_Void'Unrestricted_Access, Slot);
      View.On_Paste_Clipboard (Signal_Void'Unrestricted_Access);
      View.On_Paste_Clipboard (Slot_Void'Unrestricted_Access, Slot);
      View.On_Set_Anchor (Signal_Void'Unrestricted_Access);
      View.On_Set_Anchor (Slot_Void'Unrestricted_Access, Slot);
      View.On_Toggle_Cursor_Visible (Signal_Void'Unrestricted_Access);
      View.On_Toggle_Cursor_Visible (Slot_Void'Unrestricted_Access, Slot);
      View.On_Toggle_Overwrite (Signal_Void'Unrestricted_Access);
      View.On_Toggle_Overwrite (Slot_Void'Unrestricted_Access, Slot);

      --  Typed signal arguments exercise the specialized connector overloads.
      View.On_Delete_From_Cursor (Signal_Delete'Unrestricted_Access);
      View.On_Delete_From_Cursor (Slot_Delete'Unrestricted_Access, Slot);
      View.On_Extend_Selection (Slot_Extend'Unrestricted_Access, Slot);
      View.On_Extend_Selection (Signal_Extend'Unrestricted_Access);
      View.On_Insert_At_Cursor (Signal_String'Unrestricted_Access);
      View.On_Insert_At_Cursor (Slot_String'Unrestricted_Access, Slot);
      View.On_Preedit_Changed (Signal_String'Unrestricted_Access);
      View.On_Preedit_Changed (Slot_String'Unrestricted_Access, Slot);
      View.On_Move_Cursor (Signal_Movement'Unrestricted_Access);
      View.On_Move_Cursor (Slot_Movement'Unrestricted_Access, Slot);
      View.On_Move_Viewport (Signal_Scroll'Unrestricted_Access);
      View.On_Move_Viewport (Slot_Scroll'Unrestricted_Access, Slot);
      View.On_Select_All (Signal_Select'Unrestricted_Access);
      View.On_Select_All (Slot_Select'Unrestricted_Access, Slot);

      Callback_Count := 0;
      Emit_Text_View_Signals (Get_Object (View));
      Assert_Cmpint_Eq (Callback_Count, 14);
      Assert_True (View.Get_Buffer.Get_Char_Count >= 1);

      View.Ref_Sink;
   end Test_Signal_Connections;

begin
   Glib.Test.Init;

   --  Buffers and views cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/text-view/new-and-buffer", Test_New_And_Buffer'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-view/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-view/scroll", Test_Scroll'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-view/iter-at-position",
      Test_Iter_At_Position'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-view/factories-and-extended-properties",
      Test_Factories_And_Extended_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-view/layout-and-child-operations",
      Test_Layout_And_Child_Operations'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-view/accessibility-and-scrollable",
      Test_Accessibility_And_Scrollable'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/text-view/signal-connections",
      Test_Signal_Connections'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
