--  Exercises Glib.Generic_Properties' Ada enum registration and boxed-value
--  helpers.  No C test covers these GtkAda-specific generic wrappers.

with Ada.Command_Line;
with Interfaces.C;

with Gdk.RGBA;
with Glib;
with Glib.Generic_Properties;
with Glib.Object;
with Glib.Properties.Creation;
with Glib.Test;
with Glib.Values;
with Gtk.Main;
with Gtk.Text_Tag;

procedure Main is
   use Glib.Test;

   use type Glib.GType;
   use type Interfaces.C.C_float;

   --  C representation lets GLib store these values as enum integers.
   type Coverage_Enum is (First, Middle, Last);
   for Coverage_Enum use (First => 0, Middle => 1, Last => 2);
   pragma Convention (C, Coverage_Enum);

   --  A unique type name keeps registration independent of other tests.
   package Enum_Properties is new Glib.Generic_Properties
     .Generic_Enumeration_Property
       ("GtkAdaCoverageGenericPropertiesEnum", Coverage_Enum);

   procedure Test_Enumeration with Convention => C;
   procedure Test_Boxed_Values with Convention => C;
   procedure Test_Boxed_Property with Convention => C;

   ----------------------
   -- Test_Enumeration --
   ----------------------

   --  Check type registration, its cached path, ParamSpec creation, and the
   --  conversion between an Ada enumeration and a GValue.
   procedure Test_Enumeration is
      First_Type : constant Glib.GType := Enum_Properties.Get_Type;
      Spec       : constant Glib.Param_Spec := Enum_Properties.Gnew_Enum
        ("coverage-enum", "Coverage enum", "Ada enum property", Last);
      Value      : Glib.Values.GValue;
   begin
      Assert_True (First_Type /= Glib.GType_Invalid);
      Assert_True (Enum_Properties.Get_Type = First_Type);
      Assert_True
        (Glib.Properties.Creation.Value_Type (Spec) = First_Type);

      --  Set_Enum initializes the GValue with the registered enum type.
      Enum_Properties.Set_Enum (Value, Middle);
      Assert_True (Glib.Values.Type_Of (Value) = First_Type);
      Assert_True (Enum_Properties.Get_Enum (Value) = Middle);

      Glib.Values.Unset (Value);
      Glib.Properties.Creation.Unref (Spec);
   end Test_Enumeration;

   -----------------------
   -- Test_Boxed_Values --
   -----------------------

   --  Exercise the initially-unset, matching-type, and mismatched-type GValue
   --  paths.  Gdk.RGBA supplies a registered boxed type with safe copy/free.
   procedure Test_Boxed_Values is
      Color : constant Gdk.RGBA.Gdk_RGBA := (0.25, 0.5, 0.75, 1.0);
      Value : Glib.Values.GValue;
      Read  : Gdk.RGBA.Gdk_RGBA;
      Raised : Boolean := False;
   begin
      --  An empty GValue is initialized by Set_Value, then reused at the same
      --  type to take the no-reinitialization branch.
      Gdk.RGBA.Set_Value (Value, Color);
      Gdk.RGBA.Set_Value (Value, Color);
      Assert_True (Glib.Values.Type_Of (Value) = Gdk.RGBA.Get_Type);
      Read := Gdk.RGBA.Get_Value (Value);
      Assert_True (Read.Red = Color.Red and Read.Green = Color.Green);
      Assert_True (Read.Blue = Color.Blue and Read.Alpha = Color.Alpha);
      Glib.Values.Unset (Value);

      --  Replacing a string value checks that Set_Value unsets and reinitializes
      --  a GValue whose old type differs from the boxed type.
      Glib.Values.Init (Value, Glib.GType_String);
      Glib.Values.Set_String (Value, "replaced");
      Gdk.RGBA.Set_Value (Value, Color);
      Assert_True (Glib.Values.Type_Of (Value) = Gdk.RGBA.Get_Type);
      Glib.Values.Unset (Value);

      --  Null boxed data has no Ada record to return and raises Unset_Value.
      Glib.Values.Init (Value, Gdk.RGBA.Get_Type);
      begin
         Read := Gdk.RGBA.Get_Value (Value);
      exception
         when Glib.Generic_Properties.Unset_Value =>
            Raised := True;
      end;
      Assert_True (Raised);
      Glib.Values.Unset (Value);
   end Test_Boxed_Values;

   -------------------------
   -- Test_Boxed_Property --
   -------------------------

   --  A GtkTextTag has a real Gdk.RGBA boxed property, so these calls test the
   --  generic GObject property bridge as well as the GValue conversion.
   procedure Test_Boxed_Property is
      Tag   : Gtk.Text_Tag.Gtk_Text_Tag;
      Color : constant Gdk.RGBA.Gdk_RGBA := (0.1, 0.3, 0.6, 0.9);
      Read  : Gdk.RGBA.Gdk_RGBA;
   begin
      Gtk.Text_Tag.Gtk_New (Tag, "generic-properties-coverage");
      Gdk.RGBA.Set_Property
        (Tag, Gtk.Text_Tag.Foreground_Rgba_Property, Color);
      Read := Gdk.RGBA.Get_Property
        (Tag, Gtk.Text_Tag.Foreground_Rgba_Property);
      Assert_True (Read.Red = Color.Red and Read.Green = Color.Green);
      Assert_True (Read.Blue = Color.Blue and Read.Alpha = Color.Alpha);

      --  Release the caller-owned GObject reference after the property check.
      Glib.Object.Unref
        (Glib.Object.GObject_Record (Tag.all)'Access);
   end Test_Boxed_Property;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/glib/generic-properties/enumeration",
      Test_Enumeration'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib/generic-properties/boxed-values",
      Test_Boxed_Values'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib/generic-properties/boxed-property",
      Test_Boxed_Property'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
