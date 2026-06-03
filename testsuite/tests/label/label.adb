--  Ada port of GTK's testsuite/gtk/label.c (GTK 4.22.2).
--
--  Note: /label/markup-parse is only partially ported. The original C test
--  also compares a stringified PangoAttrList dump, which cannot be reproduced
--  with the current bindings (Pango.Attributes lacks an attribute iterator and
--  per-attribute accessors). See transition_notes.txt.

with Glib;         use Glib;
with Glib.Test;    use Glib.Test;
with Ada.Command_Line;
with Gtk.Label;    use Gtk.Label;
with Gtk.Main;
with Gtk.Window;   use Gtk.Window;
with Pango.Layout; use Pango.Layout;

procedure Label is

   --  GDK keyvals. The GDK_KEY_* constants live in Gdk.Types.Keysyms, which is
   --  part of the gtk3 sources and not built into the gtk4 library, so we use
   --  the corresponding keyval literals directly.
   Key_t      : constant Guint := 16#74#;  --  GDK_KEY_t / 't'
   Key_r      : constant Guint := 16#72#;  --  GDK_KEY_r / 'r'
   Key_i      : constant Guint := 16#69#;  --  GDK_KEY_i / 'i'
   Key_d      : constant Guint := 16#64#;  --  GDK_KEY_d / 'd'
   VoidSymbol : constant Guint := 16#FFFFFF#;  --  GDK_KEY_VoidSymbol

   procedure Test_Markup_Parse
   with Convention => C;

   procedure Test_Underline_Parse
   with Convention => C;

   procedure Test_Parse_More
   with Convention => C;

   procedure Test_Markup_Parse is
      Window : Gtk_Window;
      Label  : Gtk_Label;
   begin
      Window := Gtk_Window_New;
      Label := Gtk_Label_New ("");

      Window.Set_Child (Label);
      Window.Set_Mnemonics_Visible (True);

      Label.Set_Use_Underline (True);
      Label.Set_Use_Markup (True);
      Label.Set_Label
        ("<a href=""test""><span font_style=""italic"">abc</span> _def</a>");

      Assert_Cmpuint_Eq (Label.Get_Mnemonic_Keyval, Key_d);

      Assert_Cmpstr_Eq (Get_Text (Label.Get_Layout), "abc def");

      --  The original C test compares the full PangoAttrList dump here (and
      --  again below for each state). That comparison is omitted because the
      --  Ada bindings provide no Pango attribute iterator/accessors. See
      --  transition_notes.md.

      Window.Set_Mnemonics_Visible (False);
      Assert_Cmpstr_Eq (Get_Text (Label.Get_Layout), "abc def");

      Window.Set_Mnemonics_Visible (True);
      Label.Set_Use_Underline (False);
      Assert_Cmpstr_Eq (Get_Text (Label.Get_Layout), "abc _def");

      Window.Destroy;
   end Test_Markup_Parse;

   procedure Test_Underline_Parse is
      Window : Gtk_Window;
      Label  : Gtk_Label;
   begin
      Window := Gtk_Window_New;
      Label := Gtk_Label_New ("");

      Window.Set_Child (Label);
      Window.Set_Mnemonics_Visible (True);

      Label.Set_Use_Markup (False);
      Label.Set_Use_Underline (True);
      Label.Set_Label ("tes_t & no markup <<");

      Assert_Cmpuint_Eq (Label.Get_Mnemonic_Keyval, Key_t);

      Window.Destroy;
   end Test_Underline_Parse;

   procedure Test_Parse_More is
      Label : constant Gtk_Label := Gtk_Label_New ("");

      procedure Check
        (Input : UTF8_String;
         UL    : Boolean;
         UM    : Boolean;
         Text  : UTF8_String;
         Accel : Guint);

      procedure Check
        (Input : UTF8_String;
         UL    : Boolean;
         UM    : Boolean;
         Text  : UTF8_String;
         Accel : Guint) is
      begin
         Label.Set_Use_Underline (UL);
         Label.Set_Use_Markup (UM);
         Label.Set_Label (Input);

         Assert_Cmpstr_Eq (Label.Get_Label, Input);
         Assert_Cmpstr_Eq (Label.Get_Text, Text);
         Assert_Cmpuint_Eq (Label.Get_Mnemonic_Keyval, Accel);
      end Check;

   begin
      Check ("tes_t m__e mo_re", True, False, "test m_e more", Key_t);
      Check ("test m__e mo_re", True, False, "test m_e more", Key_r);
      Check ("tes_t m__e mo_re", False, False, "tes_t m__e mo_re", VoidSymbol);
      Check ("test m__e more", True, False, "test m_e more", VoidSymbol);
      Check
        ("<span font='test_font'>test <a href='bla'>w_ith</a> bla</span>",
         True,
         True,
         "test with bla",
         Key_i);
      Check
        ("<span font='test_font'>test <a href='bla'>w_ith</a> bla</span>",
         False,
         True,
         "test w_ith bla",
         VoidSymbol);
      Check
        ("<span font='test_font'>test <a href='bla'>with</a> bla</span>",
         True,
         True,
         "test with bla",
         VoidSymbol);
   end Test_Parse_More;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; widgets
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/label/markup-parse", Test_Markup_Parse'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/label/underline-parse", Test_Underline_Parse'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/label/parse-more", Test_Parse_More'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Label;
