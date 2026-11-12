--  Tests Gtk.Alternative_Trigger and Gtk.Mnemonic_Trigger constructors and
--  accessors. The alternative-trigger checks follow
--  testsuite/c_tests/shortcuts.c, test_trigger_parse_alternative; the mnemonic
--  constructor and getter checks port test_trigger_basic from that C file.

with Ada.Command_Line;
with System;
with Gdk.Enums;              use Gdk.Enums;
with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;
with Glib.Test;              use Glib.Test;
with Gtk.Alternative_Trigger; use Gtk.Alternative_Trigger;
with Gtk.Keyval_Trigger;
with Gtk.Mnemonic_Trigger;
with Gtk.Shortcut_Trigger;   use Gtk.Shortcut_Trigger;

procedure Main is
   use type System.Address;

   procedure Test_Constructors with Convention => C;
   procedure Test_Mnemonic_Trigger with Convention => C;
   procedure Test_Keyval_Trigger with Convention => C;

   ----------------------
   -- Test_Constructors --
   ----------------------

   procedure Test_Constructors is
      First : constant Gtk_Shortcut_Trigger := Gtk_Shortcut_Trigger
        (Gtk.Keyval_Trigger.Gtk_Keyval_Trigger_New
           (Guint (Character'Pos ('a')), Gdk_Control_Mask));
      Second : constant Gtk_Shortcut_Trigger := Gtk_Shortcut_Trigger
        (Gtk.Mnemonic_Trigger.Gtk_Mnemonic_Trigger_New
           (Guint (Character'Pos ('b'))));
      By_Function : constant Gtk_Alternative_Trigger :=
        Gtk_Alternative_Trigger_New (First, Second);
      By_Procedure : Gtk_Alternative_Trigger;
   begin
      --  The constructor takes ownership of both supplied trigger references.
      Assert_True (By_Function.Get_First.Get_Object = First.Get_Object);
      Assert_True (By_Function.Get_Second.Get_Object = Second.Get_Object);
      Unref (By_Function);

      declare
         Procedure_First : constant Gtk_Shortcut_Trigger := Gtk_Shortcut_Trigger
           (Gtk.Keyval_Trigger.Gtk_Keyval_Trigger_New
              (Guint (Character'Pos ('c')), Gdk_Shift_Mask));
         Procedure_Second : constant Gtk_Shortcut_Trigger := Gtk_Shortcut_Trigger
           (Gtk.Mnemonic_Trigger.Gtk_Mnemonic_Trigger_New
              (Guint (Character'Pos ('d'))));
         Replacement_First : constant Gtk_Shortcut_Trigger := Gtk_Shortcut_Trigger
           (Gtk.Keyval_Trigger.Gtk_Keyval_Trigger_New
              (Guint (Character'Pos ('e')), Gdk_Alt_Mask));
         Replacement_Second : constant Gtk_Shortcut_Trigger := Gtk_Shortcut_Trigger
           (Gtk.Mnemonic_Trigger.Gtk_Mnemonic_Trigger_New
              (Guint (Character'Pos ('f'))));
      begin
         Gtk_New (By_Procedure, Procedure_First, Procedure_Second);
         Assert_True
           (By_Procedure.Get_First.Get_Object = Procedure_First.Get_Object);
         Assert_True
           (By_Procedure.Get_Second.Get_Object = Procedure_Second.Get_Object);

         --  Initialize is a no-op on an already-created object. Its Ada guard
         --  leaves these replacement objects owned by this test.
         Gtk.Alternative_Trigger.Initialize
           (By_Procedure, Replacement_First, Replacement_Second);
         Assert_True
           (By_Procedure.Get_First.Get_Object = Procedure_First.Get_Object);
         Assert_True
           (By_Procedure.Get_Second.Get_Object = Procedure_Second.Get_Object);

         Unref (Replacement_First);
         Unref (Replacement_Second);
         Unref (By_Procedure);
      end;
   end Test_Constructors;

   -------------------------
   -- Test_Mnemonic_Trigger --
   -------------------------

   procedure Test_Mnemonic_Trigger is
      Keyval : constant Guint := Guint (Character'Pos ('u'));
      By_Function : constant Gtk.Mnemonic_Trigger.Gtk_Mnemonic_Trigger :=
        Gtk.Mnemonic_Trigger.Gtk_Mnemonic_Trigger_New (Keyval);
      By_Procedure : Gtk.Mnemonic_Trigger.Gtk_Mnemonic_Trigger;
   begin
      --  This is the same ASCII keyval as GDK_KEY_u in the C test.
      Assert_Cmpuint_Eq (By_Function.Get_Keyval, Keyval);
      Unref (Gtk_Shortcut_Trigger (By_Function));

      --  Exercise the Gtk_New constructor and check the key stored in GTK.
      Gtk.Mnemonic_Trigger.Gtk_New (By_Procedure, Keyval);
      Assert_Cmpuint_Eq (By_Procedure.Get_Keyval, Keyval);

      --  Initialize is documented as a no-op once the object has been made.
      --  The original key must remain, and the replacement is not adopted.
      Gtk.Mnemonic_Trigger.Initialize
        (By_Procedure, Guint (Character'Pos ('v')));
      Assert_Cmpuint_Eq (By_Procedure.Get_Keyval, Keyval);
      Unref (Gtk_Shortcut_Trigger (By_Procedure));
   end Test_Mnemonic_Trigger;

   -----------------------
   -- Test_Keyval_Trigger --
   -----------------------

   --  Port the keyval constructor and accessor checks from
   --  shortcuts.c:test_trigger_basic, and cover the Ada Gtk_New overload.
   procedure Test_Keyval_Trigger is
      Keyval : constant Guint := Guint (Character'Pos ('a'));
      By_Function : constant Gtk.Keyval_Trigger.Gtk_Keyval_Trigger :=
        Gtk.Keyval_Trigger.Gtk_Keyval_Trigger_New
          (Keyval, Gdk_Control_Mask);
      By_Procedure : Gtk.Keyval_Trigger.Gtk_Keyval_Trigger;
   begin
      --  The getters must return the key and modifiers supplied to GTK.
      Assert_Cmpuint_Eq (By_Function.Get_Keyval, Keyval);
      Assert_True (By_Function.Get_Modifiers = Gdk_Control_Mask);
      Unref (Gtk_Shortcut_Trigger (By_Function));

      --  Gtk_New reaches the out-parameter constructor with an independent
      --  object, and its getters must report that object's values.
      Gtk.Keyval_Trigger.Gtk_New (By_Procedure, Keyval, Gdk_Shift_Mask);
      Assert_Cmpuint_Eq (By_Procedure.Get_Keyval, Keyval);
      Assert_True (By_Procedure.Get_Modifiers = Gdk_Shift_Mask);

      --  Initialize is a no-op after construction, preserving the first key
      --  and modifier mask when a caller attempts to replace them.
      Gtk.Keyval_Trigger.Initialize
        (By_Procedure, Guint (Character'Pos ('b')), Gdk_Alt_Mask);
      Assert_Cmpuint_Eq (By_Procedure.Get_Keyval, Keyval);
      Assert_True (By_Procedure.Get_Modifiers = Gdk_Shift_Mask);
      Unref (Gtk_Shortcut_Trigger (By_Procedure));
   end Test_Keyval_Trigger;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/alternative-trigger/constructors",
                       Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func ("/alternative-trigger/mnemonic-trigger",
                       Test_Mnemonic_Trigger'Unrestricted_Access);
   Glib.Test.Add_Func ("/alternative-trigger/keyval-trigger",
                       Test_Keyval_Trigger'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
