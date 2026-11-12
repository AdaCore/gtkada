--  Exercise Gtk.Center_Layout constructors, properties, and child accessors.
--  No matching C testsuite case exists to port for GtkCenterLayout.

with Ada.Command_Line;
with Glib.Test;
with Gtk.Center_Layout;
with Gtk.Enums;
with Gtk.Label;
with Gtk.Main;
with Gtk.Widget;

procedure Main is
   use type Gtk.Enums.Gtk_Baseline_Position;
   use type Gtk.Enums.Gtk_Orientation;
   use type Gtk.Center_Layout.Gtk_Center_Layout;
   use type Gtk.Widget.Gtk_Widget;

   procedure Test_Constructors
   with Convention => C;

   procedure Test_Properties
   with Convention => C;

   procedure Test_Child_Widgets
   with Convention => C;

   --  Both public constructor forms initialize the GObject. Calling Initialize
   --  again is also covered because its documented behavior is idempotent.
   procedure Test_Constructors is
      By_Function  : Gtk.Center_Layout.Gtk_Center_Layout :=
        Gtk.Center_Layout.Gtk_Center_Layout_New;
      By_Procedure : Gtk.Center_Layout.Gtk_Center_Layout;
   begin
      --  Verify both construction styles produce a usable layout manager.
      Gtk.Center_Layout.Gtk_New (By_Procedure);
      Gtk.Center_Layout.Initialize (By_Function);
      Gtk.Center_Layout.Initialize (By_Procedure);

      --  Distinct enum values show that each constructed object is independent.
      Gtk.Center_Layout.Set_Baseline_Position
        (By_Function, Gtk.Enums.Baseline_Position_Center);
      Gtk.Center_Layout.Set_Baseline_Position
        (By_Procedure, Gtk.Enums.Baseline_Position_Bottom);
      Glib.Test.Assert_True
        (Gtk.Center_Layout.Get_Baseline_Position (By_Function) =
           Gtk.Enums.Baseline_Position_Center);
      Glib.Test.Assert_True
        (Gtk.Center_Layout.Get_Baseline_Position (By_Procedure) =
           Gtk.Enums.Baseline_Position_Bottom);
   end Test_Constructors;

   --  Check each supported enum value and both Boolean states through the
   --  public setters and getters, without relying on GTK's default values.
   procedure Test_Properties is
      Layout : Gtk.Center_Layout.Gtk_Center_Layout :=
        Gtk.Center_Layout.Gtk_Center_Layout_New;
   begin
      --  Round-trip every baseline option, including the less common extremes.
      for Position in Gtk.Enums.Gtk_Baseline_Position loop
         Layout.Set_Baseline_Position (Position);
         Glib.Test.Assert_True
           (Layout.Get_Baseline_Position = Position);
      end loop;

      --  Both orientations are valid and must be returned unchanged.
      Layout.Set_Orientation (Gtk.Enums.Orientation_Horizontal);
      Glib.Test.Assert_True
        (Layout.Get_Orientation = Gtk.Enums.Orientation_Horizontal);
      Layout.Set_Orientation (Gtk.Enums.Orientation_Vertical);
      Glib.Test.Assert_True
        (Layout.Get_Orientation = Gtk.Enums.Orientation_Vertical);

      --  Test false and true explicitly rather than assuming a default.
      Layout.Set_Shrink_Center_Last (False);
      Glib.Test.Assert_False (Layout.Get_Shrink_Center_Last);
      Layout.Set_Shrink_Center_Last (True);
      Glib.Test.Assert_True (Layout.Get_Shrink_Center_Last);
   end Test_Properties;

   --  The manager holds each slot independently. The getters return the same
   --  widgets, and passing null clears the corresponding slot as documented.
   procedure Test_Child_Widgets is
      Layout : Gtk.Center_Layout.Gtk_Center_Layout :=
        Gtk.Center_Layout.Gtk_Center_Layout_New;
      Start  : Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New ("start");
      Center : Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New ("center");
      Finish : Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New ("end");
   begin
      --  A new manager starts with no widgets in any of its three slots.
      Glib.Test.Assert_True (Layout.Get_Start_Widget = null);
      Glib.Test.Assert_True (Layout.Get_Center_Widget = null);
      Glib.Test.Assert_True (Layout.Get_End_Widget = null);

      --  Install separate labels to verify each getter reports its own slot.
      Layout.Set_Start_Widget (Start);
      Layout.Set_Center_Widget (Center);
      Layout.Set_End_Widget (Finish);
      Glib.Test.Assert_True
        (Layout.Get_Start_Widget = Gtk.Widget.Gtk_Widget (Start));
      Glib.Test.Assert_True
        (Layout.Get_Center_Widget = Gtk.Widget.Gtk_Widget (Center));
      Glib.Test.Assert_True
        (Layout.Get_End_Widget = Gtk.Widget.Gtk_Widget (Finish));

      Layout.Set_Start_Widget (null);
      Layout.Set_Center_Widget (null);
      Layout.Set_End_Widget (null);
      Glib.Test.Assert_True (Layout.Get_Start_Widget = null);
      Glib.Test.Assert_True (Layout.Get_Center_Widget = null);
      Glib.Test.Assert_True (Layout.Get_End_Widget = null);
   end Test_Child_Widgets;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/center-layout/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/center-layout/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/center-layout/child-widgets", Test_Child_Widgets'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
