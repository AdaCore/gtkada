--  Exercise the Gtk.Progress_Bar API through the generated Ada binding.

with Ada.Command_Line;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Test;       use Glib.Test;
with Gtk.Accessible;  use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Main;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Pango.Layout;     use Pango.Layout;

procedure Progress_Bar is
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Atcontext.Gtk_Atcontext;

   procedure Test_Defaults with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Text with Convention => C;
   procedure Test_Pulse with Convention => C;
   procedure Test_Accessibility with Convention => C;

   procedure Test_Defaults is
      Bar : Gtk_Progress_Bar;
   begin
      Gtk_New (Bar);
      Ref_Sink (Bar);
      Assert_Cmpfloat_Eq (Bar.Get_Fraction, 0.0);
      Assert_Cmpfloat_Eq (Bar.Get_Pulse_Step, 0.1);
      Assert_False (Bar.Get_Inverted);
      Assert_False (Bar.Get_Show_Text);
      Assert_Cmpstr_Eq (Bar.Get_Text, "");
      Assert_True (Bar.Get_Ellipsize = Ellipsize_None);
      Assert_True (Bar.Get_Orientation = Orientation_Horizontal);
      Unref (Bar);
   end Test_Defaults;

   procedure Test_Properties is
      Bar : constant Gtk_Progress_Bar := Gtk_Progress_Bar_New;
   begin
      Ref_Sink (Bar);
      for I in 0 .. 4 loop
         Bar.Set_Fraction (Gdouble (I) / 4.0);
         Assert_Cmpfloat_Eq (Bar.Get_Fraction, Gdouble (I) / 4.0);
      end loop;
      Assert_Cmpfloat_Eq (Get_Property (Bar, Fraction_Property), 1.0);
      Set_Property (Bar, Fraction_Property, 0.5);
      Assert_Cmpfloat_Eq (Bar.Get_Fraction, 0.5);
      Bar.Set_Inverted (True);
      Assert_True (Bar.Get_Inverted);
      Bar.Set_Inverted (False);
      Assert_False (Bar.Get_Inverted);
      Bar.Set_Orientation (Orientation_Vertical);
      Assert_True (Bar.Get_Orientation = Orientation_Vertical);
      for Mode in Pango_Ellipsize_Mode loop
         Bar.Set_Ellipsize (Mode);
         Assert_True (Bar.Get_Ellipsize = Mode);
      end loop;
      Unref (Bar);
   end Test_Properties;

   procedure Test_Text is
      Bar : constant Gtk_Progress_Bar := Gtk_Progress_Bar_New;
   begin
      Ref_Sink (Bar);
      Bar.Set_Show_Text (True);
      Assert_True (Bar.Get_Show_Text);
      Bar.Set_Text ("Loading — café");
      Assert_Cmpstr_Eq (Bar.Get_Text, "Loading — café");
      Bar.Set_Text;
      Assert_Cmpstr_Eq (Bar.Get_Text, "");
      Bar.Set_Show_Text (False);
      Assert_False (Bar.Get_Show_Text);
      Unref (Bar);
   end Test_Text;

   procedure Test_Pulse is
      Bar : constant Gtk_Progress_Bar := Gtk_Progress_Bar_New;
   begin
      Ref_Sink (Bar);
      Bar.Set_Fraction (0.5);
      Bar.Set_Pulse_Step (0.2);
      Assert_Cmpfloat_Eq (Bar.Get_Pulse_Step, 0.2);
      for I in 1 .. 10 loop
         Bar.Pulse;
      end loop;
      --  Activity mode leaves the stored fraction intact. Setting a new
      --  fraction returns the widget to determinate progress.
      Assert_Cmpfloat_Eq (Bar.Get_Fraction, 0.5);
      Bar.Set_Fraction (0.75);
      Assert_Cmpfloat_Eq (Bar.Get_Fraction, 0.75);
      Unref (Bar);
   end Test_Pulse;

   --  A detached progress bar has no display bounds, accessibility context,
   --  parent, children, or following sibling. Its inherited accessibility
   --  methods still accept updates and valid enum values without a screen
   --  reader or a realized window.
   procedure Test_Accessibility is
      Bar : constant Gtk_Progress_Bar := Gtk_Progress_Bar_New;
      Parent : Gtk_Accessible;
      Child  : Gtk_Accessible;
      Next   : Gtk_Accessible;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Bar);
      Assert_True (Bar.Get_Accessible_Role = Accessible_Role_Progress_Bar);
      Assert_Cmpstr_Eq (Bar.Get_Accessible_Id, "");
      Assert_True (Bar.Get_At_Context = null);
      Assert_False (Bar.Get_Bounds (X, Y, Width, Height));

      Parent := Bar.Get_Accessible_Parent;
      Child := Bar.Get_First_Accessible_Child;
      Next := Bar.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Null_Gtk_Accessible);
      --  GtkProgressBar exposes its internal progress indicator as an
      --  accessible child even before the widget is rooted.
      Assert_True (Child /= Null_Gtk_Accessible);
      Assert_True (Next = Null_Gtk_Accessible);
      Assert_False
        (Bar.Get_Platform_State (Accessible_Platform_State_Focused));

      --  The platform state reflects the widget's focusability when the
      --  corresponding accessibility state is updated.
      Bar.Set_Focusable (True);
      Bar.Update_Platform_State (Accessible_Platform_State_Focusable);
      Assert_True
        (Bar.Get_Platform_State (Accessible_Platform_State_Focusable));

      --  Valid accessibility values exercise each reset path while preserving
      --  the progress bar's fixed role.
      Bar.Reset_Property (Accessible_Property_Label);
      Bar.Reset_Relation (Accessible_Relation_Labelled_By);
      Bar.Reset_State (Accessible_State_Busy);
      Assert_True (Bar.Get_Accessible_Role = Accessible_Role_Progress_Bar);

      --  A detached widget has no accessible tree endpoints to install.
      Bar.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Bar.Get_Accessible_Parent = Null_Gtk_Accessible);
      Bar.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Bar.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Announcements may be ignored by the headless accessibility backend.
      Bar.Announce
        ("progress bar accessibility test", Accessible_Announcement_Priority_Low);
      Unref (Bar);
   end Test_Accessibility;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/progress-bar/defaults", Test_Defaults'Unrestricted_Access);
   Add_Func ("/progress-bar/properties", Test_Properties'Unrestricted_Access);
   Add_Func ("/progress-bar/text", Test_Text'Unrestricted_Access);
   Add_Func ("/progress-bar/pulse", Test_Pulse'Unrestricted_Access);
   Add_Func
     ("/progress-bar/accessibility", Test_Accessibility'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Progress_Bar;
