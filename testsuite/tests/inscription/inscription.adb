--  Exercise Gtk.Inscription's constructors, text and sizing API, properties,
--  accessibility wrappers, and conversions to its implemented interfaces.
--  GTK's C testsuite has no gtk_inscription_* test to port, so these cases
--  check the documented GtkInscription behavior directly.

with Ada.Command_Line;
with Glib;
with Glib.Test;
with Glib.Properties;
with Interfaces.C;
with Gtk.Accessible;
with Gtk.Accessible_Text;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Enums;
with Gtk.Inscription;
with Gtk.Main;
with Pango.Attributes;
with Pango.Enums;

procedure Inscription is
   use Glib;
   use Glib.Test;
   use Gtk.Inscription;

   use type Interfaces.C.C_float;
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Accessible_Text.Gtk_Accessible_Text;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Buildable.Gtk_Buildable;
   use type Gtk.Constraint_Target.Gtk_Constraint_Target;
   use type Gtk.Inscription.Gtk_Inscription;
   use type Pango.Attributes.Pango_Attr_List;
   use type Pango.Enums.Wrap_Mode;

   --  Separate cases keep the test report useful when an API wrapper fails.
   procedure Test_Constructors with Convention => C;
   procedure Test_Text_And_Attributes with Convention => C;
   procedure Test_Sizing_And_Properties with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Interface_Conversions with Convention => C;

   procedure Test_Constructors is
      By_Function : constant Gtk_Inscription :=
        Gtk_Inscription_New ("created by function");
      By_Procedure : Gtk_Inscription;
      Manual : constant Gtk_Inscription := new Gtk_Inscription_Record;
      Empty : constant Gtk_Inscription := Gtk_Inscription_New;
   begin
      --  GTK registers this type once the package's constructor is used.
      Assert_True (Gtk.Inscription.Get_Type /= 0);
      Assert_Cmpstr_Eq (By_Function.Get_Text, "created by function");

      --  The output-parameter constructor must initialize the same text.
      Gtk_New (By_Procedure, "created by procedure");
      Assert_Cmpstr_Eq (By_Procedure.Get_Text, "created by procedure");

      --  Initialize is intended for derived types and is idempotent after the
      --  first call; the second text must not replace the original text.
      Initialize (Manual, "initialized once");
      Initialize (Manual, "ignored second initialization");
      Assert_Cmpstr_Eq (Manual.Get_Text, "initialized once");
      Assert_Cmpstr_Eq (Empty.Get_Text, "");
   end Test_Constructors;

   procedure Test_Text_And_Attributes is
      View : constant Gtk_Inscription := Gtk_Inscription_New ("initial");
      Attrs : constant Pango.Attributes.Pango_Attr_List :=
        Pango.Attributes.Pango_Attr_List_New;
   begin
      --  Sink the floating widget reference because it is not parented.
      View.Ref_Sink;

      --  Plain text treats markup-looking characters literally.
      View.Set_Text ("<b>literal</b>");
      Assert_Cmpstr_Eq (View.Get_Text, "<b>literal</b>");

      --  Markup is parsed into displayed text, and its style list is exposed.
      View.Set_Markup ("<b>formatted</b>");
      Assert_Cmpstr_Eq (View.Get_Text, "formatted");
      Assert_True
        (View.Get_Attributes /= Pango.Attributes.Null_Pango_Attr_List);

      --  An explicit Pango list can also be installed and read back.
      View.Set_Attributes (Attrs);
      Assert_True
        (View.Get_Attributes /= Pango.Attributes.Null_Pango_Attr_List);

      --  Empty strings exercise the binding's null-pointer conversion paths.
      View.Set_Markup ("");
      Assert_Cmpstr_Eq (View.Get_Text, "");
      View.Set_Text ("");
      Assert_Cmpstr_Eq (View.Get_Text, "");

      --  The attribute list is boxed and owned by this test independently.
      Pango.Attributes.Unref (Attrs);
      View.Unref;
   end Test_Text_And_Attributes;

   procedure Test_Sizing_And_Properties is
      View : constant Gtk_Inscription := Gtk_Inscription_New ("size me");
   begin
      --  Keep the stand-alone widget alive until all property checks finish.
      View.Ref_Sink;

      --  The four sizing hints round-trip independently through their methods.
      View.Set_Min_Chars (3);
      View.Set_Min_Lines (2);
      View.Set_Nat_Chars (18);
      View.Set_Nat_Lines (4);
      Assert_Cmpuint_Eq (View.Get_Min_Chars, 3);
      Assert_Cmpuint_Eq (View.Get_Min_Lines, 2);
      Assert_Cmpuint_Eq (View.Get_Nat_Chars, 18);
      Assert_Cmpuint_Eq (View.Get_Nat_Lines, 4);

      --  Each overflow policy is a valid value of the C-backed enumeration.
      for Policy in Gtk_Inscription_Overflow loop
         View.Set_Text_Overflow (Policy);
         Assert_True (View.Get_Text_Overflow = Policy);
      end loop;

      --  Wrapping and alignment also have getters for verifying the setters.
      View.Set_Wrap_Mode (Pango.Enums.Pango_Wrap_Char);
      Assert_True (View.Get_Wrap_Mode = Pango.Enums.Pango_Wrap_Char);
      View.Set_Wrap_Mode (Pango.Enums.Pango_Wrap_Word);
      Assert_True (View.Get_Wrap_Mode = Pango.Enums.Pango_Wrap_Word);
      View.Set_Xalign (0.25);
      View.Set_Yalign (0.75);
      Assert_True (abs (View.Get_Xalign - 0.25) < 0.001);
      Assert_True (abs (View.Get_Yalign - 0.75) < 0.001);

      --  The generated GObject property descriptors provide the same values.
      Glib.Properties.Set_Property
        (View, Min_Chars_Property, 5);
      Assert_Cmpuint_Eq
        (Glib.Properties.Get_Property (View, Min_Chars_Property), 5);
      Glib.Properties.Set_Property
        (View, Min_Lines_Property, 3);
      Assert_Cmpuint_Eq
        (Glib.Properties.Get_Property (View, Min_Lines_Property), 3);
      Glib.Properties.Set_Property
        (View, Nat_Chars_Property, 24);
      Assert_Cmpuint_Eq
        (Glib.Properties.Get_Property (View, Nat_Chars_Property), 24);
      Glib.Properties.Set_Property
        (View, Nat_Lines_Property, 6);
      Assert_Cmpuint_Eq
        (Glib.Properties.Get_Property (View, Nat_Lines_Property), 6);
      Gtk_Inscription_Overflow_Properties.Set_Property
        (View,
         Gtk_Inscription_Overflow_Properties.Property
           (Text_Overflow_Property),
         Ellipsize_Middle);
      Assert_True
        (Gtk_Inscription_Overflow_Properties.Get_Property
           (View,
            Gtk_Inscription_Overflow_Properties.Property
              (Text_Overflow_Property))
         = Ellipsize_Middle);
      Pango.Enums.Wrap_Mode_Properties.Set_Property
        (View,
         Pango.Enums.Wrap_Mode_Properties.Property (Wrap_Mode_Property),
         Pango.Enums.Pango_Wrap_Word_Char);
      Assert_True
        (Pango.Enums.Wrap_Mode_Properties.Get_Property
           (View,
            Pango.Enums.Wrap_Mode_Properties.Property (Wrap_Mode_Property))
         = Pango.Enums.Pango_Wrap_Word_Char);
      Glib.Properties.Set_Property (View, Xalign_Property, 0.4);
      Glib.Properties.Set_Property (View, Yalign_Property, 0.6);
      Assert_True
        (abs (Glib.Properties.Get_Property (View, Xalign_Property) - 0.4)
         < 0.001);
      Assert_True
        (abs (Glib.Properties.Get_Property (View, Yalign_Property) - 0.6)
         < 0.001);

      --  Text is readable and writable as a property; markup is write-only.
      Glib.Properties.Set_Property (View, Text_Property, "from property");
      Assert_Cmpstr_Eq
        (Glib.Properties.Get_Property (View, Text_Property), "from property");
      Glib.Properties.Set_Property (View, Markup_Property, "<i>from markup</i>");
      Assert_Cmpstr_Eq (View.Get_Text, "from markup");

      View.Unref;
   end Test_Sizing_And_Properties;

   procedure Test_Accessibility is
      View : constant Gtk_Inscription :=
        Gtk_Inscription_New ("accessible inscription");
      X, Y, Width, Height : Gint;
   begin
      --  A sink is needed for this detached widget's explicit final Unref.
      View.Ref_Sink;

      --  An unrooted inscription has no context, bounds, parent, or children.
      Assert_True
        (View.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Label);
      Assert_Cmpstr_Eq (View.Get_Accessible_Id, "");
      Assert_True
        (View.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (View.Get_First_Accessible_Child = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (View.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (View.Get_At_Context = null);
      Assert_False (View.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (View.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  These inherited accessibility methods are safe for a detached widget.
      View.Announce
        ("inscription ready",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      View.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      View.Reset_Relation (Gtk.Accessible.Accessible_Relation_Described_By);
      View.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      View.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);
      View.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (View.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      View.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (View.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);

      --  Accessible-text updates notify assistive technology of text changes.
      View.Update_Caret_Position;
      View.Update_Contents
        (Gtk.Enums.Accessible_Text_Content_Change_Insert, 0, 9);
      View.Update_Selection_Bound;
      Assert_Cmpstr_Eq (View.Get_Text, "accessible inscription");

      View.Unref;
   end Test_Accessibility;

   procedure Test_Interface_Conversions is
      View : constant Gtk_Inscription := Gtk_Inscription_New ("interfaces");
      Accessible : constant Gtk.Accessible.Gtk_Accessible := +View;
      Accessible_Text : constant Gtk.Accessible_Text.Gtk_Accessible_Text := +View;
      Buildable : constant Gtk.Buildable.Gtk_Buildable := +View;
      Target : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +View;
   begin
      --  Interface views are borrowed aliases of this same widget instance.
      View.Ref_Sink;

      --  Each implemented GType interface converts back to the original object.
      Assert_True (-Accessible = View);
      Assert_True (-Accessible_Text = View);
      Assert_True (-Buildable = View);
      Assert_True (-Target = View);
      Assert_Cmpstr_Eq
        (Gtk.Buildable.Get_Buildable_Id (Buildable), "");

      View.Unref;
   end Test_Interface_Conversions;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/inscription/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/inscription/text-and-attributes",
      Test_Text_And_Attributes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/inscription/sizing-and-properties",
      Test_Sizing_And_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/inscription/accessibility", Test_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/inscription/interface-conversions",
      Test_Interface_Conversions'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Inscription;
