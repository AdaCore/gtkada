--  Exercises Gdk.Display.Translate_Key, one of the keymap entry points that
--  the GdkModifierType binding unblocked. It is the only bound subprogram
--  taking a Gdk_Modifier_Type both in and out, so it also checks that the
--  modular type survives a round trip through C.
--
--  Keycodes are properties of the X server the test runs against, not of GDK,
--  so nothing here hardcodes one: the tests look up the keycode carrying a
--  given keyval and skip when the layout has no such key.

with Ada.Command_Line;

with Glib;        use Glib;
with Glib.Test;   use Glib.Test;

with Gdk;        use Gdk;
with Gdk.Display; use Gdk.Display;
with Gdk.Enums;   use Gdk.Enums;

with Gtk.Main;

procedure Translate_Key is

   First_Keycode : constant Guint := 8;
   Last_Keycode  : constant Guint := 255;
   --  The range of keycodes an X server may report.

   Keyval_A : constant Guint := 16#61#;
   Keyval_Shift_A : constant Guint := 16#41#;
   --  GDK_KEY_a and GDK_KEY_A, which are the Latin-1 code points.

   function Find_Keycode
     (Display : Gdk.Gdk_Display;
      Keyval  : Guint;
      Found   : out Boolean) return Guint;
   --  Return the lowest unmodified keycode translating to Keyval on the
   --  current layout, or (with Found set to False) the lowest keycode bound to
   --  nothing at all when Keyval is 0.

   procedure Test_Letter_Key with Convention => C;
   procedure Test_Unbound_Keycode with Convention => C;

   ------------------
   -- Find_Keycode --
   ------------------

   function Find_Keycode
     (Display : Gdk.Gdk_Display;
      Keyval  : Guint;
      Found   : out Boolean) return Guint
   is
      Translated : aliased Guint;
   begin
      for Code in First_Keycode .. Last_Keycode loop
         declare
            Bound : constant Boolean :=
              Translate_Key
                (Display,
                 Keycode => Code,
                 State   => Gdk_No_Modifier_Mask,
                 Group   => 0,
                 Keyval  => Translated'Access);
         begin
            if (Keyval = 0 and then not Bound)
              or else (Bound and then Translated = Keyval)
            then
               Found := True;
               return Code;
            end if;
         end;
      end loop;

      Found := False;
      return 0;
   end Find_Keycode;

   ---------------------
   -- Test_Letter_Key --
   ---------------------

   procedure Test_Letter_Key is
      Display : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;

      Keyval          : aliased Guint;
      Effective_Group : aliased Glib.Gint;
      Level           : aliased Glib.Gint;
      Consumed        : aliased Gdk_Modifier_Type;

      Found   : Boolean;
      Keycode : Guint;
   begin
      Assert_True (Display /= null);

      Keycode := Find_Keycode (Display, Keyval_A, Found);
      if not Found then
         Skip ("no key produces GDK_KEY_a on this layout");
         return;
      end if;

      --  Unshifted: the first level of the first group.

      Assert_True
        (Translate_Key
           (Display, Keycode, Gdk_No_Modifier_Mask, 0,
            Keyval'Access, Effective_Group'Access, Level'Access,
            Consumed'Access));
      Assert_Cmphex_Eq (Keyval, Keyval_A);
      Assert_Cmpint_Eq (Effective_Group, 0);
      Assert_Cmpint_Eq (Level, 0);

      --  Shifted: the second level, and Shift is reported as consumed by the
      --  translation, so a caller comparing against an accelerator knows to
      --  mask it out.

      Assert_True
        (Translate_Key
           (Display, Keycode, Gdk_Shift_Mask, 0,
            Keyval'Access, Effective_Group'Access, Level'Access,
            Consumed'Access));
      Assert_Cmphex_Eq (Keyval, Keyval_Shift_A);
      Assert_Cmpint_Eq (Effective_Group, 0);
      Assert_Cmpint_Eq (Level, 1);
      Assert_True ((Consumed and Gdk_Shift_Mask) /= Gdk_No_Modifier_Mask);

      --  All four return locations are optional; dropping them must not
      --  change the answer.

      Assert_True (Translate_Key (Display, Keycode, Gdk_Shift_Mask, 0));
   end Test_Letter_Key;

   -------------------------
   -- Test_Unbound_Keycode --
   -------------------------

   procedure Test_Unbound_Keycode is
      Display : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;

      Keyval  : aliased Guint := Keyval_A;
      Found   : Boolean;
      Keycode : Guint;
   begin
      Keycode := Find_Keycode (Display, 0, Found);
      if not Found then
         Skip ("every keycode is bound on this layout");
         return;
      end if;

      Assert_False
        (Translate_Key
           (Display, Keycode, Gdk_No_Modifier_Mask, 0, Keyval'Access));
      Assert_False (Translate_Key (Display, Keycode, Gdk_Shift_Mask, 0));
   end Test_Unbound_Keycode;

begin
   Glib.Test.Init;

   --  Translate_Key needs a display; Gtk.Main.Init opens the default one.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/display/translate-key/letter", Test_Letter_Key'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/display/translate-key/unbound",
      Test_Unbound_Keycode'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Translate_Key;
