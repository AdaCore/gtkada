--  GtkAccessibleText tests for its identity conversion and allocated range
--  helper. GTK's C testsuite has no GtkAccessibleText-specific test to port.

with Ada.Command_Line;
with System;
with System.Address_To_Access_Conversions;

with Glib;                use Glib;
with Glib.Test;           use Glib.Test;
with Gtk.Accessible_Text;
with Gtk.Label;           use Gtk.Label;
with Gtk.Main;

procedure Main is

   use type Glib.Gsize;
   use type Gtk.Accessible_Text.Gtk_Accessible_Text;
   use type Gtk.Label.Gtk_Label;

   --  The helper takes ownership of a range allocated by GLib, so this
   --  conversion gives the test a typed view of memory from g_malloc.
   package Range_Pointers is new System.Address_To_Access_Conversions
     (Gtk.Accessible_Text.Gtk_Accessible_Text_Range);

   function G_Malloc (N_Bytes : Glib.Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Test_Interface_Identity with Convention => C;
   procedure Test_Allocated_Range_Copy with Convention => C;

   ----------------------------
   -- Test_Interface_Identity --
   ----------------------------

   procedure Test_Interface_Identity is
      Label : constant Gtk_Label := Gtk_Label_New ("accessible text");
      --  The conversion from Label produces a borrowed interface view.
      Interface_View : constant Gtk.Accessible_Text.Gtk_Accessible_Text :=
        +Label;
      Same_Interface : constant Gtk.Accessible_Text.Gtk_Accessible_Text :=
        Gtk.Accessible_Text."+" (Interface_View);
   begin
      --  The interface operator preserves the view and its underlying widget.
      Label.Ref_Sink;
      Assert_True (Same_Interface = Interface_View);
      Assert_True (-Same_Interface = Label);
      Label.Unref;
   end Test_Interface_Identity;

   --------------------------------
   -- Test_Allocated_Range_Copy --
   --------------------------------

   procedure Test_Allocated_Range_Copy is
      --  Allocate using GLib because From_Object_Free releases this pointer
      --  with g_free; an Ada stack or heap object would have the wrong owner.
      Bytes : constant Glib.Gsize := Glib.Gsize
        (Gtk.Accessible_Text.Gtk_Accessible_Text_Range'Size /
           System.Storage_Unit);
      Range_Pointer : constant Range_Pointers.Object_Pointer :=
        Range_Pointers.To_Pointer (G_Malloc (Bytes));
      Actual : Gtk.Accessible_Text.Gtk_Accessible_Text_Range;
   begin
      Range_Pointer.all := (Start => 3, Length => 7);

      --  Check the copied fields after the helper has freed the C allocation.
      Actual := Gtk.Accessible_Text.From_Object_Free (Range_Pointer);
      Assert_True (Actual.Start = 3);
      Assert_True (Actual.Length = 7);
   end Test_Allocated_Range_Copy;

begin
   Glib.Test.Init;
   --  Gtk initialization is required before constructing the label widget.
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/accessible-text/interface-identity",
      Test_Interface_Identity'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/accessible-text/allocated-range-copy",
      Test_Allocated_Range_Copy'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
