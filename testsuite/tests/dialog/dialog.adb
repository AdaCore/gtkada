--  Headless regression guard for the Gtk.Dialog and Gtk.Message_Dialog
--  bindings exercised by the gtkada_demo "Dialogs" page (see
--  gtkada_demo/create_dialog).
--
--  A dialog is only interesting once a user has clicked one of its buttons,
--  which cannot be driven without a human. This test therefore covers what
--  can be checked headlessly: the response-id bookkeeping, the content area,
--  the message-dialog constructors, and -- the one piece that genuinely
--  needs GObject's help -- the ::response marshaller, driven by emitting the
--  signal explicitly through Gtk.Dialog.Response.

with Ada.Command_Line;
with Glib.Properties;     use Glib.Properties;
with Glib.Test;           use Glib.Test;
with Gtk.Box;             use Gtk.Box;
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.Label;           use Gtk.Label;
with Gtk.Main;
with Gtk.Message_Dialog;  use Gtk.Message_Dialog;
with Gtk.Widget;          use Gtk.Widget;

procedure Dialog is

   Seen_Response : Gtk_Response_Type := Gtk_Response_None;
   --  Set by On_Response below, read back by Test_Response_Signal.

   procedure Test_Construction
   with Convention => C;

   procedure Test_Response_Ids
   with Convention => C;

   procedure Test_Message_Dialog
   with Convention => C;

   procedure Test_Response_Signal
   with Convention => C;

   procedure On_Response
     (Self        : access Gtk_Dialog_Record'Class;
      Response_Id : Gtk_Response_Type);

   -----------------------
   -- Test_Construction --
   -----------------------

   procedure Test_Construction is
      D     : Gtk_Dialog;
      Area  : Gtk_Box;
      Label : Gtk_Label;
   begin
      Gtk_New (D, "A title", null, Dialog_Modal);
      Assert_Nonnull (D.all'Address);
      Assert_Cmpstr_Eq (D.Get_Title, "A title");
      Assert_True (D.Get_Modal);

      --  Get_Content_Area is typed as Gtk_Box rather than the Gtk_Widget the
      --  GIR file advertises; make sure the override survives regeneration.

      Area := D.Get_Content_Area;
      Assert_Nonnull (Area.all'Address);

      --  The content area is a plain box, to be filled like any other; this
      --  is what the "Dialogs" demo does with its grid.

      Gtk_New (Label, "Are you sure?");
      Area.Append (Label);
      Assert_True (Area.Get_First_Child = Gtk_Widget (Label));

      D.Destroy;
   end Test_Construction;

   -----------------------
   -- Test_Response_Ids --
   -----------------------

   procedure Test_Response_Ids is
      D      : Gtk_Dialog;
      Button : Gtk_Widget;
   begin
      Gtk_New (D, "Buttons", null, 0);

      Button := D.Add_Button ("_OK", Gtk_Response_OK);
      Assert_Nonnull (Button.all'Address);

      Assert_True (D.Get_Response_For_Widget (Button) = Gtk_Response_OK);
      Assert_True (D.Get_Widget_For_Response (Gtk_Response_OK) = Button);

      --  A response id nobody registered has no widget.

      Assert_True (D.Get_Widget_For_Response (Gtk_Response_Apply) = null);

      D.Set_Response_Sensitive (Gtk_Response_OK, False);
      Assert_False (Button.Get_Sensitive);

      D.Set_Response_Sensitive (Gtk_Response_OK, True);
      Assert_True (Button.Get_Sensitive);

      D.Destroy;
   end Test_Response_Ids;

   -------------------------
   -- Test_Message_Dialog --
   -------------------------

   procedure Test_Message_Dialog is
      D : Gtk_Message_Dialog;
   begin
      for The_Type in Gtk_Message_Type loop
         Gtk_New
           (D, null, Dialog_Modal, The_Type, Buttons_Ok, "A message");
         Assert_Nonnull (D.all'Address);
         Assert_Nonnull (D.Get_Message_Area.all'Address);

         D.Format_Secondary_Text ("Some details");
         D.Destroy;
      end loop;

      --  The message is a printf format string -- that is the price of
      --  recovering these varargs constructors -- so a literal per-cent has
      --  to be doubled. Lock that down: it is what the generated
      --  documentation promises callers.

      Gtk_New
        (D, null, 0, Message_Info, Buttons_Ok, "50%% done");
      Assert_Cmpstr_Eq (Get_Property (D, Text_Property), "50% done");
      D.Destroy;

      --  Set_Markup, by contrast, is not variadic and takes its argument
      --  as it stands.

      Gtk_New_With_Markup
        (D, null, 0, Message_Question, Buttons_Ok_Cancel, "Really?");
      D.Set_Markup ("<b>100% sure?</b>");
      Assert_Cmpstr_Eq (Get_Property (D, Text_Property), "<b>100% sure?</b>");
      D.Format_Secondary_Markup ("<i>There is no undo</i>");
      Assert_Nonnull (D.Get_Message_Area.all'Address);
      D.Destroy;
   end Test_Message_Dialog;

   -----------------
   -- On_Response --
   -----------------

   procedure On_Response
     (Self        : access Gtk_Dialog_Record'Class;
      Response_Id : Gtk_Response_Type)
   is
      pragma Unreferenced (Self);
   begin
      Seen_Response := Response_Id;
   end On_Response;

   --------------------------
   -- Test_Response_Signal --
   --------------------------

   procedure Test_Response_Signal is
      D : Gtk_Dialog;
   begin
      Gtk_New (D, "Signals", null, 0);
      D.On_Response (On_Response'Unrestricted_Access);

      --  The marshaller extracts the response id from a GValue holding a
      --  gint; getting that wrong yields 0 rather than a compile error.

      Seen_Response := Gtk_Response_None;
      D.Response (Gtk_Response_Cancel);
      Assert_True (Seen_Response = Gtk_Response_Cancel);

      Seen_Response := Gtk_Response_None;
      D.Response (42);
      Assert_True (Seen_Response = 42);

      D.Destroy;
   end Test_Response_Signal;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/dialog/construction", Test_Construction'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dialog/response-ids", Test_Response_Ids'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dialog/message-dialog", Test_Message_Dialog'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dialog/response-signal", Test_Response_Signal'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Dialog;
