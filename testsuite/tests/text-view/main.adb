--  GtkAda-side coverage for Gtk.Text_View. GTK ships no standalone
--  textview.c test, so this is an idiomatic smoke test of the bound
--  surface rather than a port: view construction, the buffer get/set
--  relationship, the common boolean / enum / margin properties, and the
--  scroll-to-mark family. The child-anchor cases from textbuffer.c
--  (test_iter_with_anchor / test_get_text_with_anchor) are covered, with
--  a child widget attached through the view, by the text-buffer test.
--
--  Like the text-buffer test, everything here runs without a realized
--  window: the scroll calls are exercised for their bindings, not their
--  visible effect.

with Ada.Command_Line;

with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;

with Gtk.Main;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Text_Buffer;       use Gtk.Text_Buffer;
with Gtk.Text_Iter;         use Gtk.Text_Iter;
with Gtk.Text_Mark;         use Gtk.Text_Mark;
with Gtk.Text_View;         use Gtk.Text_View;

procedure Main is

   procedure Test_New_And_Buffer
   with Convention => C;

   procedure Test_Properties
   with Convention => C;

   procedure Test_Scroll
   with Convention => C;

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

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
