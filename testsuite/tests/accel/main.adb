--  Tests Gtk.Accel_Group's accelerator parsing, naming, labels, and validity.
--  The parse and label assertions port the applicable checks from
--  testsuite/c_tests/accel.c (test_one_accel and accel2); that C test also
--  checks with_keycode variants, which this GtkAda unit does not expose.

with Ada.Command_Line;
with Glib;
with Glib.Test;
with Gdk.Enums;
with Gtk.Accel_Group;
with Gtk.Main;

procedure Main is
   use type Glib.Guint;
   use type Gdk.Enums.Gdk_Modifier_Type;

   --  Port accel2's key and modifier round trip through GTK's public parser.
   procedure Test_Parse_And_Labels
   with Convention => C;

   procedure Test_Parse_And_Labels is
      Key       : aliased Glib.Guint := 0;
      Modifiers : aliased Gdk.Enums.Gdk_Modifier_Type :=
        Gdk.Enums.Gdk_No_Modifier_Mask;
      Default_Mask : constant Gdk.Enums.Gdk_Modifier_Type :=
        Gtk.Accel_Group.Accelerator_Get_Default_Mod_Mask;
   begin
      --  The C test confirms both keyval and modifier outputs for accel2.
      Glib.Test.Assert_True
        (Gtk.Accel_Group.Accelerator_Parse
           ("<Control><Alt>z", Key'Access, Modifiers'Access));
      Glib.Test.Assert_Cmpuint_Eq (Key, Glib.Guint (Character'Pos ('z')));
      Glib.Test.Assert_True
        (Modifiers = (Gdk.Enums.Gdk_Control_Mask or Gdk.Enums.Gdk_Alt_Mask));

      --  Names are parseable, while labels are formatted for users and ATs.
      Glib.Test.Assert_Cmpstr_Eq
        (Gtk.Accel_Group.Accelerator_Name (Key, Modifiers),
         "<Control><Alt>z");
      Glib.Test.Assert_Cmpstr_Eq
        (Gtk.Accel_Group.Accelerator_Get_Label (Key, Modifiers),
         "Ctrl+Alt+Z");
      Glib.Test.Assert_Cmpstr_Eq
        (Gtk.Accel_Group.Accelerator_Get_Accessible_Label (Key, Modifiers),
         "Control+Alt+Z");

      --  The parser accepts omitted output pointers, as its C API does.
      Glib.Test.Assert_True
        (Gtk.Accel_Group.Accelerator_Parse ("<Control><Alt>z"));

      --  Lock is excluded, but ordinary keyboard modifiers are significant.
      Glib.Test.Assert_True
        ((Default_Mask and Gdk.Enums.Gdk_Lock_Mask) = 0);
      Glib.Test.Assert_True
        ((Default_Mask and Gdk.Enums.Gdk_Control_Mask) /= 0);
      Glib.Test.Assert_True
        ((Default_Mask and Gdk.Enums.Gdk_Shift_Mask) /= 0);
      Glib.Test.Assert_True
        ((Default_Mask and Gdk.Enums.Gdk_Alt_Mask) /= 0);
   end Test_Parse_And_Labels;

   --  Check failure outputs and the documented rejection of modifier keys.
   procedure Test_Invalid_Accelerators
   with Convention => C;

   procedure Test_Invalid_Accelerators is
      Key       : aliased Glib.Guint := 1;
      Modifiers : aliased Gdk.Enums.Gdk_Modifier_Type :=
        Gdk.Enums.Gdk_Control_Mask;
   begin
      --  Unknown key names fail cleanly and reset both requested outputs.
      Glib.Test.Assert_False
        (Gtk.Accel_Group.Accelerator_Parse
           ("<Control>DefinitelyNotAKey", Key'Access, Modifiers'Access));
      Glib.Test.Assert_Cmpuint_Eq (Key, 0);
      Glib.Test.Assert_True
        (Modifiers = Gdk.Enums.Gdk_No_Modifier_Mask);

      --  A printable key is a valid accelerator; the Control key itself is not.
      Glib.Test.Assert_True
        (Gtk.Accel_Group.Accelerator_Valid
           (Glib.Guint (Character'Pos ('z')), Gdk.Enums.Gdk_Control_Mask));
      Glib.Test.Assert_False
        (Gtk.Accel_Group.Accelerator_Valid
           (16#FFE3#, Gdk.Enums.Gdk_Control_Mask));
   end Test_Invalid_Accelerators;

begin
   Glib.Test.Init;
   --  GTK initialization mirrors gtk_test_init in the source C test.
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/accel/parse-and-labels", Test_Parse_And_Labels'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/accel/invalid", Test_Invalid_Accelerators'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
