with Ada.Command_Line;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Test;         use Glib.Test;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Main;
with Gtk.Spinner;       use Gtk.Spinner;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Accessible.Gtk_Accessible_Role;
use type Gtk.Atcontext.Gtk_Atcontext;

procedure Spinner is

   procedure Test_Start_Stop with Convention => C;
   procedure Test_Property with Convention => C;
   procedure Test_Bind with Convention => C;
   procedure Test_Accessibility with Convention => C;

   procedure Test_Start_Stop is
      S : constant Gtk_Spinner := Gtk_Spinner_New;
   begin
      Ref_Sink (S);
      Assert_False (S.Get_Spinning);
      S.Start;
      Assert_True (S.Get_Spinning);
      S.Start;
      Assert_True (S.Get_Spinning);
      S.Stop;
      Assert_False (S.Get_Spinning);
      S.Stop;
      Assert_False (S.Get_Spinning);
      Unref (S);
   end Test_Start_Stop;

   procedure Test_Property is
      S : Gtk_Spinner;
   begin
      Gtk_New (S);
      Ref_Sink (S);
      Assert_False (Get_Property (S, Spinning_Property));
      S.Set_Spinning (True);
      Assert_True (Get_Property (S, Spinning_Property));
      S.Set_Spinning (False);
      Assert_False (Get_Property (S, Spinning_Property));
      Set_Property (S, Spinning_Property, True);
      Assert_True (S.Get_Spinning);
      Set_Property (S, Spinning_Property, False);
      Assert_False (S.Get_Spinning);
      S.Start;
      Assert_True (Get_Property (S, Spinning_Property));
      S.Stop;
      Assert_False (Get_Property (S, Spinning_Property));
      Unref (S);
   end Test_Property;

   procedure Test_Bind is
      S : constant Gtk_Spinner := Gtk_Spinner_New;
      B : constant Gtk_Toggle_Button := Gtk_Toggle_Button_New;
   begin
      Ref_Sink (S);
      Ref_Sink (B);
      B.Bind_Property ("active", S, "spinning");
      B.Set_Active (True);
      Assert_True (S.Get_Spinning);
      B.Set_Active (False);
      Assert_False (S.Get_Spinning);
      Unref (B);
      Unref (S);
   end Test_Bind;

   --  An unrooted spinner has no screen bounds or AT context, but exposes its
   --  documented progress-bar role and accepts accessible interface updates.
   procedure Test_Accessibility is
      S : constant Gtk_Spinner := Gtk_Spinner_New;
      Parent, Child, Next : Gtk.Accessible.Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Glib.Gint;
   begin
      Ref_Sink (S);

      --  Check the stable accessible metadata and the empty detached tree.
      Assert_Cmpstr_Eq (S.Get_Accessible_Id, "");
      Assert_True
        (S.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Progress_Bar);
      Parent := S.Get_Accessible_Parent;
      Child := S.Get_First_Accessible_Child;
      Next := S.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Child = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Next = Gtk.Accessible.Null_Gtk_Accessible);

      --  A detached widget has no AT context or screen-coordinate bounds.
      Context := S.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (S.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (S.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  These requests are valid without a screen reader or accessible tree.
      S.Announce
        ("spinner accessibility test",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      S.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      S.Reset_Relation (Gtk.Accessible.Accessible_Relation_Labelled_By);
      S.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      S.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);
      S.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (S.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      S.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (S.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);

      --  Release the reference acquired before exercising the interface.
      Unref (S);
   end Test_Accessibility;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/spinner/start-stop", Test_Start_Stop'Unrestricted_Access);
   Add_Func ("/spinner/property", Test_Property'Unrestricted_Access);
   Add_Func ("/spinner/bind", Test_Bind'Unrestricted_Access);
   Add_Func ("/spinner/accessibility", Test_Accessibility'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Spinner;
