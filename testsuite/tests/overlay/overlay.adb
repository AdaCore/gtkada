--  GTK has no testsuite/gtk/overlay.c to port, so these cases are ours:
--  they cover the surface the Gtk.Overlay binding adds, and no more.

with Ada.Command_Line;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Gtk.Button;       use Gtk.Button;
with Gtk.Label;        use Gtk.Label;
with Gtk.Main;
with Gtk.Overlay;      use Gtk.Overlay;
with Gtk.Widget;       use Gtk.Widget;
with Interfaces.C.Strings;
with System;

procedure Overlay is

   procedure Emit_Child_Position
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      Child    : System.Address;
      Rect     : access Gdk_Rectangle;
      Result   : access Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  Emit "get-child-position", whose GdkRectangle is an out parameter and
   --  whose return value is a gboolean.

   procedure Test_Defaults with Convention => C;
   procedure Test_Child with Convention => C;
   procedure Test_Overlays with Convention => C;
   procedure Test_Flags with Convention => C;
   procedure Test_Position with Convention => C;

   Handler_Calls : Natural := 0;

   function On_Child_Position
     (Self       : access Gtk_Overlay_Record'Class;
      Widget     : not null access Gtk_Widget_Record'Class;
      Allocation : out Gdk_Rectangle) return Boolean;

   -----------------------
   -- On_Child_Position --
   -----------------------

   function On_Child_Position
     (Self       : access Gtk_Overlay_Record'Class;
      Widget     : not null access Gtk_Widget_Record'Class;
      Allocation : out Gdk_Rectangle) return Boolean
   is
      pragma Unreferenced (Self, Widget);
   begin
      Handler_Calls := Handler_Calls + 1;
      Allocation := (X => 10, Y => 20, Width => 30, Height => 40);
      return True;
   end On_Child_Position;

   -------------------
   -- Test_Defaults --
   -------------------

   procedure Test_Defaults is
      O : constant Gtk_Overlay := Gtk_Overlay_New;
   begin
      Ref_Sink (O);
      Assert_True (O.Get_Child = null);
      Unref (O);
   end Test_Defaults;

   ----------------
   -- Test_Child --
   ----------------

   procedure Test_Child is
      O     : constant Gtk_Overlay := Gtk_Overlay_New;
      Label : constant Gtk_Label := Gtk_Label_New ("child");
   begin
      Ref_Sink (O);

      O.Set_Child (Label);
      Assert_True (O.Get_Child = Gtk_Widget (Label));

      O.Set_Child (null);
      Assert_True (O.Get_Child = null);

      Unref (O);
   end Test_Child;

   ---------------------
   -- Test_Overlays --
   ---------------------

   procedure Test_Overlays is
      O      : constant Gtk_Overlay := Gtk_Overlay_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label ("over");
   begin
      Ref_Sink (O);
      --  Keep the button alive across Remove_Overlay, which drops the
      --  overlay's reference.
      Ref_Sink (Button);

      Assert_True (Button.Get_Parent = null);

      O.Add_Overlay (Button);
      Assert_True (Button.Get_Parent = Gtk_Widget (O));
      --  An overlay is not the main child
      Assert_True (O.Get_Child = null);

      O.Remove_Overlay (Button);
      Assert_True (Button.Get_Parent = null);

      Unref (Button);
      Unref (O);
   end Test_Overlays;

   ----------------
   -- Test_Flags --
   ----------------

   procedure Test_Flags is
      O      : constant Gtk_Overlay := Gtk_Overlay_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label ("b");
   begin
      Ref_Sink (O);
      O.Add_Overlay (Button);

      Assert_False (O.Get_Clip_Overlay (Button));
      O.Set_Clip_Overlay (Button, True);
      Assert_True (O.Get_Clip_Overlay (Button));
      O.Set_Clip_Overlay (Button, False);
      Assert_False (O.Get_Clip_Overlay (Button));

      Assert_False (O.Get_Measure_Overlay (Button));
      O.Set_Measure_Overlay (Button, True);
      Assert_True (O.Get_Measure_Overlay (Button));
      O.Set_Measure_Overlay (Button, False);
      Assert_False (O.Get_Measure_Overlay (Button));

      Unref (O);
   end Test_Flags;

   -------------------
   -- Test_Position --
   -------------------

   procedure Test_Position is
      O      : constant Gtk_Overlay := Gtk_Overlay_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label ("b");
      Rect   : aliased Gdk_Rectangle := (0, 0, 0, 0);
      Result : aliased Gboolean := 0;
      Name   : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("get-child-position");
   begin
      Ref_Sink (O);
      O.Add_Overlay (Button);
      O.On_Get_Child_Position (On_Child_Position'Unrestricted_Access);

      Emit_Child_Position
        (Get_Object (O), Name, Get_Object (Button), Rect'Access,
         Result'Access);
      Interfaces.C.Strings.Free (Name);

      Assert_Cmpuint_Eq (Guint (Handler_Calls), 1);
      Assert_True (Result /= 0);
      Assert_Cmpint_Eq (Rect.X, 10);
      Assert_Cmpint_Eq (Rect.Y, 20);
      Assert_Cmpint_Eq (Rect.Width, 30);
      Assert_Cmpint_Eq (Rect.Height, 40);

      Unref (O);
   end Test_Position;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/overlay/defaults", Test_Defaults'Unrestricted_Access);
   Glib.Test.Add_Func ("/overlay/child", Test_Child'Unrestricted_Access);
   Glib.Test.Add_Func ("/overlay/overlays", Test_Overlays'Unrestricted_Access);
   Glib.Test.Add_Func ("/overlay/flags", Test_Flags'Unrestricted_Access);
   Glib.Test.Add_Func ("/overlay/position", Test_Position'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Overlay;
