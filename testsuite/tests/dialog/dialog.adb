--  Headless regression guard for the Gtk.Dialog and Gtk.Message_Dialog
--  bindings exercised by the gtkada_demo "Dialogs" page (see
--  gtkada_demo/create_dialog).
--  It ports the basic GtkDialog construction and header-bar checks from
--  testsuite/c_tests/templates.c:test_dialog_basic and
--  test_dialog_override_property, and the message constructor check from
--  test_message_dialog_basic. The remaining cases check the Ada API surface.
--
--  A dialog is only interesting once a user has clicked one of its buttons,
--  which cannot be driven without a human. This test therefore covers what
--  can be checked headlessly: the response-id bookkeeping, the content area,
--  the message-dialog constructors, and -- the one piece that genuinely
--  needs GObject's help -- the ::response marshaller, driven by emitting the
--  signal explicitly through Gtk.Dialog.Response.

with Ada.Command_Line;
with Gdk;                 use Gdk;
with Gsk.Renderer;        use Gsk.Renderer;
with Glib;                use Glib;
with Glib.Object;
with Glib.Properties;     use Glib.Properties;
with Glib.Test;           use Glib.Test;
with Gtk.Accessible;      use Gtk.Accessible;
with Gtk.Atcontext;       use Gtk.Atcontext;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.Label;           use Gtk.Label;
with Gtk.Main;
with Gtk.Message_Dialog;  use Gtk.Message_Dialog;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Types;        use Gtkada.Types;
with System;

procedure Dialog is

   use type Glib.Object.GObject;

   --  g_signal_emit_by_name lets the keybinding-only GtkDialog::close signal
   --  be exercised deterministically without synthesizing keyboard input.
   procedure Emit_Void_Signal
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   Seen_Response : Gtk_Response_Type := Gtk_Response_None;
   --  Set by On_Response below, read back by Test_Response_Signal.

   Seen_Slot_Response : Gtk_Response_Type := Gtk_Response_None;
   Seen_Slot_Response_Object : Glib.Object.GObject := null;
   Seen_Close_Count : Natural := 0;
   Seen_Slot_Close_Count : Natural := 0;
   Seen_Slot_Close_Object : Glib.Object.GObject := null;
   --  These globals let the generic GObject callback overloads verify that
   --  GTK passes the supplied slot object and the response payload correctly.

   procedure Test_Construction
   with Convention => C;

   procedure Test_Response_Ids
   with Convention => C;

   procedure Test_Message_Dialog
   with Convention => C;

   procedure Test_Message_Dialog_Accessibility_And_Root
   with Convention => C;

   procedure Test_Response_Signal
   with Convention => C;

   procedure Test_Constructors_And_Actions
   with Convention => C;

   procedure Test_Accessibility_And_Root
   with Convention => C;

   procedure Test_Signal_Overloads
   with Convention => C;

   procedure On_Response
     (Self        : access Gtk_Dialog_Record'Class;
      Response_Id : Gtk_Response_Type);

   procedure On_Slot_Response
     (Self        : access Glib.Object.GObject_Record'Class;
      Response_Id : Gtk_Response_Type);

   procedure On_Close (Self : access Gtk_Dialog_Record'Class);

   procedure On_Slot_Close
     (Self : access Glib.Object.GObject_Record'Class);

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
      Factory_Dialog : constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (null, 0, Message_Info, Buttons_Ok, "factory message");
      Markup_Dialog : constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New_With_Markup
          (null, 0, Message_Info, Buttons_Ok, "<b>factory markup</b>");
   begin
      --  The factory functions complement Gtk_New and must preserve both
      --  plain text and Pango markup in the primary message.
      Assert_Cmpstr_Eq
        (Get_Property (Factory_Dialog, Text_Property), "factory message");
      Assert_Cmpstr_Eq
        (Get_Property (Markup_Dialog, Text_Property), "<b>factory markup</b>");
      Assert_Nonnull (Factory_Dialog.Get_Message_Area.all'Address);
      Assert_Nonnull (Markup_Dialog.Get_Message_Area.all'Address);
      Factory_Dialog.Destroy;
      Markup_Dialog.Destroy;

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

   -------------------------------------------------
   -- Test_Message_Dialog_Accessibility_And_Root --
   -------------------------------------------------

   procedure Test_Message_Dialog_Accessibility_And_Root is
      D          : constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (null, 0, Message_Info, Buttons_Ok, "accessible message");
      Content    : constant Gtk_Box := D.Get_Content_Area;
      Label      : Gtk_Label;
      Child      : Gtk_Accessible;
      Focus      : Gtk_Widget;
      X, Y       : Glib.Gint;
      Width      : Glib.Gint;
      Height     : Glib.Gint;
      Surface_X  : Glib.Gdouble;
      Surface_Y  : Glib.Gdouble;
      Has_Bounds : Boolean;
   begin
      --  Before presentation the native surface and accessible context are
      --  absent, while the dialog's role, id, and platform state are queryable.
      Assert_True (D.Get_Accessible_Role = Accessible_Role_Dialog);
      Assert_True (D.Get_Accessible_Id /= "");
      Assert_True (D.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True (D.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_True (D.Get_At_Context = null);
      Assert_False
        (D.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Add a real accessible child so the first-child wrapper returns it.
      Gtk_New (Label, "message dialog content");
      Content.Append (Label);
      Child := D.Get_First_Accessible_Child;
      Assert_True (Child /= Null_Gtk_Accessible);

      --  Accessibility updates are valid without an accessibility service;
      --  confirm the parent and sibling queries still return the null values.
      D.Announce ("dialog announcement", Accessible_Announcement_Priority_Low);
      D.Reset_Property (Accessible_Property_Label);
      D.Reset_Relation (Accessible_Relation_Labelled_By);
      D.Reset_State (Accessible_State_Busy);
      D.Update_Platform_State (Accessible_Platform_State_Focused);
      D.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (D.Get_Accessible_Parent = Null_Gtk_Accessible);
      D.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (D.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  GtkRoot focus and display can be checked without mapping the window.
      Gtk_New (Label, "focus target");
      Content.Append (Label);
      D.Set_Focus (Gtk_Widget (Label));
      Focus := D.Get_Focus;
      Assert_True (Focus = Gtk_Widget (Label));
      Assert_Nonnull (D.Get_Display.all'Address);

      --  An unrealized native has no surface, renderer, or screen bounds.
      Assert_True (D.Get_Surface = null);
      Assert_True (D.Get_Renderer = null);
      D.Get_Surface_Transform (Surface_X, Surface_Y);
      Assert_True (Surface_X = 0.0 and Surface_Y = 0.0);
      Has_Bounds := D.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      D.Destroy;
   end Test_Message_Dialog_Accessibility_And_Root;

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

   ----------------------------------
   -- Test_Constructors_And_Actions --
   ----------------------------------

   procedure Test_Constructors_And_Actions is
      Empty_Dialog : constant Gtk_Dialog := Gtk_Dialog_New;
      Dialog_2     : Gtk_Dialog;
      Dialog_3     : constant Gtk_Dialog :=
        Gtk_Dialog_New ("Factory title", null, 0);
      Dialog_4     : Gtk_Dialog;
      Dialog_5     : Gtk_Dialog := new Gtk_Dialog_Record;
      Action       : Gtk_Button;
      Header_Dialog : constant Gtk_Dialog :=
        Gtk_Dialog_New ("Header", null, Dialog_Use_Header_Bar);
   begin
      --  Exercise both factory and Gtk_New constructor forms. Calling
      --  Initialize on a created instance checks the documented no-op path.
      Assert_Nonnull (Empty_Dialog.all'Address);
      Assert_Nonnull (Empty_Dialog.Get_Content_Area.all'Address);
      Gtk.Dialog.Initialize (Empty_Dialog);
      Gtk_New (Dialog_2);
      Assert_Nonnull (Dialog_2.Get_Content_Area.all'Address);
      Gtk.Dialog.Initialize (Dialog_2);

      Assert_Cmpstr_Eq (Dialog_3.Get_Title, "Factory title");
      Gtk_New (Dialog_4, "Gtk_New title", null, 0);
      Assert_Cmpstr_Eq (Dialog_4.Get_Title, "Gtk_New title");
      Gtk.Dialog.Initialize (Dialog_4, "Ignored title", null, 0);
      Assert_Cmpstr_Eq (Dialog_4.Get_Title, "Gtk_New title");

      --  G_New_Dialog is the Ada subclassing constructor. The header-bar flag
      --  is consumed during creation and can be observed through its getter.
      G_New_Dialog (Dialog_5, Dialog_Use_Header_Bar);
      Assert_Nonnull (Dialog_5.Get_Content_Area.all'Address);
      Assert_Cmpint_Eq
        (Get_Property (Dialog_5, Use_Header_Bar_Property), 1);
      Assert_Nonnull (Header_Dialog.Get_Header_Bar.all'Address);

      --  Add_Action_Widget associates a caller-created button with a response
      --  just like Add_Button, and Set_Default_Response selects that button.
      Gtk_New (Action, "Apply");
      Dialog_3.Add_Action_Widget (Action, Gtk_Response_Apply);
      Assert_True
        (Dialog_3.Get_Response_For_Widget (Gtk_Widget (Action)) =
           Gtk_Response_Apply);
      Dialog_3.Set_Default_Response (Gtk_Response_Apply);
      Assert_True
        (Dialog_3.Get_Default_Widget = Gtk_Widget (Action));

      Empty_Dialog.Destroy;
      Dialog_2.Destroy;
      Dialog_3.Destroy;
      Dialog_4.Destroy;
      Dialog_5.Destroy;
      Header_Dialog.Destroy;
   end Test_Constructors_And_Actions;

   -----------------------------------
   -- Test_Accessibility_And_Root --
   -----------------------------------

   procedure Test_Accessibility_And_Root is
      D          : constant Gtk_Dialog := Gtk_Dialog_New;
      Content    : constant Gtk_Box := D.Get_Content_Area;
      Label      : Gtk_Label;
      Child      : Gtk_Accessible;
      Focus      : Gtk_Widget;
      X, Y       : Glib.Gint;
      Width      : Glib.Gint;
      Height     : Glib.Gint;
      Surface_X  : Glib.Gdouble;
      Surface_Y  : Glib.Gdouble;
      Has_Bounds : Boolean;
   begin
      --  An unrooted dialog has a stable accessible role but no external
      --  parent, AT context, screen bounds, or platform focus state.
      Assert_True (D.Get_Accessible_Role = Accessible_Role_Dialog);
      Assert_Cmpstr_Eq (D.Get_Accessible_Id, "GtkDialog");
      Assert_True (D.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True (D.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_True (D.Get_At_Context = null);
      Assert_False
        (D.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Put an accessible child in the content area so GTK can return it from
      --  the first-child query before the dialog is presented.
      Gtk_New (Label, "accessible content");
      Content.Append (Label);
      Child := D.Get_First_Accessible_Child;
      Assert_True (Child /= Null_Gtk_Accessible);

      --  These updates are safe without an accessibility service; the
      --  getters confirm that null parent/sibling values remain consistent.
      D.Announce ("dialog announcement", Accessible_Announcement_Priority_Low);
      D.Reset_Property (Accessible_Property_Label);
      D.Reset_Relation (Accessible_Relation_Labelled_By);
      D.Reset_State (Accessible_State_Busy);
      D.Update_Platform_State (Accessible_Platform_State_Focused);
      D.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (D.Get_Accessible_Parent = Null_Gtk_Accessible);
      D.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (D.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  GtkRoot focus and display are available before mapping a toplevel.
      Gtk_New (Label, "focus target");
      Content.Append (Label);
      D.Set_Focus (Gtk_Widget (Label));
      Focus := D.Get_Focus;
      Assert_True (Focus = Gtk_Widget (Label));
      Assert_Nonnull (D.Get_Display.all'Address);

      --  Surface access remains null until GTK realizes the native window.
      Assert_True (D.Get_Surface = null);
      Assert_True (D.Get_Renderer = null);
      D.Get_Surface_Transform (Surface_X, Surface_Y);
      Assert_True (Surface_X = 0.0 and Surface_Y = 0.0);
      Has_Bounds := D.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      D.Destroy;
   end Test_Accessibility_And_Root;

   -------------------------
   -- Test_Signal_Overloads --
   -------------------------

   procedure Test_Signal_Overloads is
      D           : constant Gtk_Dialog := Gtk_Dialog_New;
      Slot        : constant Gtk_Label := Gtk_Label_New ("signal slot");
      Signal_Name : Gtkada.Types.Chars_Ptr := New_String ("close");
   begin
      --  Both response signal callback shapes are public. The generic form
      --  receives the slot object as its first argument.
      D.On_Response
        (On_Slot_Response'Unrestricted_Access, Slot => Slot);
      Seen_Slot_Response := Gtk_Response_None;
      Seen_Slot_Response_Object := null;
      D.Response (Gtk_Response_Yes);
      Assert_True (Seen_Slot_Response = Gtk_Response_Yes);
      Assert_True
        (Seen_Slot_Response_Object = Glib.Object.GObject (Slot));

      --  The close signal is normally reached from Escape; emit the signal
      --  explicitly to keep this test independent of keyboard input.
      D.On_Close (On_Close'Unrestricted_Access);
      D.On_Close (On_Slot_Close'Unrestricted_Access, Slot => Slot);
      Seen_Close_Count := 0;
      Seen_Slot_Close_Count := 0;
      Seen_Slot_Close_Object := null;
      Emit_Void_Signal (Glib.Object.Get_Object (D), Signal_Name);
      Assert_Cmpuint_Eq (Glib.Guint (Seen_Close_Count), 1);
      Assert_Cmpuint_Eq (Glib.Guint (Seen_Slot_Close_Count), 1);
      Assert_True (Seen_Slot_Close_Object = Glib.Object.GObject (Slot));
      Free (Signal_Name);
      D.Destroy;
   end Test_Signal_Overloads;

   --------------------
   -- On_Slot_Response --
   --------------------

   procedure On_Slot_Response
     (Self        : access Glib.Object.GObject_Record'Class;
      Response_Id : Gtk_Response_Type)
   is
   begin
      Seen_Slot_Response := Response_Id;
      Seen_Slot_Response_Object := Glib.Object.GObject (Self);
   end On_Slot_Response;

   --------------
   -- On_Close --
   --------------

   procedure On_Close (Self : access Gtk_Dialog_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Seen_Close_Count := Seen_Close_Count + 1;
   end On_Close;

   ------------------
   -- On_Slot_Close --
   ------------------

   procedure On_Slot_Close
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      Seen_Slot_Close_Count := Seen_Slot_Close_Count + 1;
      Seen_Slot_Close_Object := Glib.Object.GObject (Self);
   end On_Slot_Close;

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
     ("/dialog/message-dialog-accessibility-root",
      Test_Message_Dialog_Accessibility_And_Root'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dialog/response-signal", Test_Response_Signal'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dialog/constructors-actions",
      Test_Constructors_And_Actions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dialog/accessibility-root",
      Test_Accessibility_And_Root'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dialog/signal-overloads", Test_Signal_Overloads'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Dialog;
