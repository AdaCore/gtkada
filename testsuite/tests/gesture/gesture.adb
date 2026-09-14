--  Exercises the gesture bindings and Gtk.Widget.Add_Controller.
--
--  Driving real pointer input is out of reach here, so what is checked is
--  the part that the bindings are responsible for: that a gesture can be
--  built, configured, attached to a widget and detached again, that the
--  controller reports the widget it was attached to, and that a gesture
--  which has seen no input reports itself inactive.

with Ada.Command_Line;
with Glib;               use Glib;
with Glib.Test;          use Glib.Test;
with Gtk.Drawing_Area;   use Gtk.Drawing_Area;
with Gtk.Event_Controller; use Gtk.Event_Controller;
with Gtk.Gesture;        use Gtk.Gesture;
with Gtk.Gesture_Click;  use Gtk.Gesture_Click;
with Gtk.Gesture_Drag;   use Gtk.Gesture_Drag;
with Gtk.Gesture_Single; use Gtk.Gesture_Single;
with Gtk.Main;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Window;         use Gtk.Window;

procedure Gesture is

   Button_Secondary : constant Guint := 3;
   --  GDK_BUTTON_SECONDARY; the GDK_BUTTON_* constants are not bound.

   procedure Test_Drag
   with Convention => C;

   procedure Test_Click
   with Convention => C;

   procedure Test_Remove
   with Convention => C;

   ---------------
   -- Test_Drag --
   ---------------

   procedure Test_Drag is
      Window : constant Gtk_Window := Gtk_Window_New;
      Area   : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
      Drag   : constant Gtk_Gesture_Drag := Gtk_Gesture_Drag_New;
   begin
      Window.Set_Child (Area);

      --  0 means "any button".
      Gtk_Gesture_Single (Drag).Set_Button (0);
      Assert_Cmpuint_Eq (Gtk_Gesture_Single (Drag).Get_Button, 0);

      --  Not attached yet, so it belongs to no widget.
      Assert_True (Gtk_Event_Controller (Drag).Get_Widget = null);

      Area.Add_Controller (Drag);
      Assert_True
        (Gtk_Event_Controller (Drag).Get_Widget = Gtk_Widget (Area));

      --  Nothing has been pressed, so the gesture is idle.
      Assert_True (not Gtk_Gesture (Drag).Is_Active);
      Assert_True (not Gtk_Gesture (Drag).Is_Recognized);
   end Test_Drag;

   ----------------
   -- Test_Click --
   ----------------

   procedure Test_Click is
      Window : constant Gtk_Window := Gtk_Window_New;
      Area   : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
      Click  : constant Gtk_Gesture_Click := Gtk_Gesture_Click_New;
   begin
      Window.Set_Child (Area);

      Gtk_Gesture_Single (Click).Set_Button (Button_Secondary);
      Assert_Cmpuint_Eq
        (Gtk_Gesture_Single (Click).Get_Button, Button_Secondary);

      Area.Add_Controller (Click);
      Assert_True
        (Gtk_Event_Controller (Click).Get_Widget = Gtk_Widget (Area));

      --  A gesture that has seen no press has no sequences to report.
      Assert_Cmpuint_Eq
        (Gdk_Event_Sequence_List.Length (Gtk_Gesture (Click).Get_Sequences),
         0);
   end Test_Click;

   -----------------
   -- Test_Remove --
   -----------------

   procedure Test_Remove is
      Window : constant Gtk_Window := Gtk_Window_New;
      Area   : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
      Drag   : constant Gtk_Gesture_Drag := Gtk_Gesture_Drag_New;
   begin
      Window.Set_Child (Area);

      Area.Add_Controller (Drag);
      Assert_True
        (Gtk_Event_Controller (Drag).Get_Widget = Gtk_Widget (Area));

      --  Add_Controller took ownership, so removing it is also the last
      --  reference: Drag must not be touched after this point.
      Area.Remove_Controller (Drag);
   end Test_Remove;

begin
   Glib.Test.Init;

   --  Widgets cannot be created before GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/gesture/drag", Test_Drag'Unrestricted_Access);
   Glib.Test.Add_Func ("/gesture/click", Test_Click'Unrestricted_Access);
   Glib.Test.Add_Func ("/gesture/remove", Test_Remove'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Gesture;
