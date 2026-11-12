--  Ada port of GTK's testsuite/gtk/searchbar.c (GTK 4.22.2), extended with
--  a check of the bar's own properties and of Connect_Entry.

with Ada.Command_Line;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Gtk.Accessible;   use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Button;       use Gtk.Button;
with Gtk.Main;
with Gtk.Search_Bar;   use Gtk.Search_Bar;
with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtk.Widget;       use Gtk.Widget;

procedure Search_Bar is

   use type Gtk.Atcontext.Gtk_Atcontext;

   procedure Test_Capture_Widget_Destroy with Convention => C;
   procedure Test_Capture_Widget_Unset with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Connect_Entry with Convention => C;
   procedure Test_Accessibility with Convention => C;

   ---------------------------------
   -- Test_Capture_Widget_Destroy --
   ---------------------------------

   procedure Test_Capture_Widget_Destroy is
      Bar    : constant Gtk_Search_Bar := Gtk_Search_Bar_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label;
   begin
      Ref_Sink (Bar);
      Ref_Sink (Button);

      Bar.Set_Key_Capture_Widget (Button);
      Assert_True (Bar.Get_Key_Capture_Widget = Gtk_Widget (Button));

      --  Dropping the last reference to the capture widget must clear the
      --  bar's pointer to it rather than leave it dangling.
      Unref (Button);
      Assert_True (Bar.Get_Key_Capture_Widget = null);

      Unref (Bar);
   end Test_Capture_Widget_Destroy;

   -------------------------------
   -- Test_Capture_Widget_Unset --
   -------------------------------

   procedure Test_Capture_Widget_Unset is
      Bar    : constant Gtk_Search_Bar := Gtk_Search_Bar_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label;
   begin
      Ref_Sink (Bar);
      Ref_Sink (Button);

      Bar.Set_Key_Capture_Widget (Button);
      Assert_True (Bar.Get_Key_Capture_Widget = Gtk_Widget (Button));

      Bar.Set_Key_Capture_Widget (null);
      Assert_True (Bar.Get_Key_Capture_Widget = null);

      Unref (Bar);
      Unref (Button);
   end Test_Capture_Widget_Unset;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      Bar : constant Gtk_Search_Bar := Gtk_Search_Bar_New;
   begin
      Assert_False (Bar.Get_Search_Mode);
      Bar.Set_Search_Mode (True);
      Assert_True (Bar.Get_Search_Mode);
      Bar.Set_Search_Mode (False);
      Assert_False (Bar.Get_Search_Mode);

      --  Both properties default to False.
      Assert_False (Bar.Get_Show_Close_Button);
      Bar.Set_Show_Close_Button (True);
      Assert_True (Bar.Get_Show_Close_Button);
      Bar.Set_Show_Close_Button (False);
      Assert_False (Bar.Get_Show_Close_Button);
   end Test_Properties;

   -------------------------
   -- Test_Connect_Entry --
   -------------------------

   procedure Test_Connect_Entry is
      Bar   : constant Gtk_Search_Bar := Gtk_Search_Bar_New;
      Entry_Widget : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
   begin
      Assert_True (Bar.Get_Child = null);

      Bar.Set_Child (Entry_Widget);
      Assert_True (Bar.Get_Child = Gtk_Widget (Entry_Widget));

      --  In Gtk4 Connect_Entry takes a Gtk_Editable, not a Gtk_Entry, so any
      --  editable (here the search entry itself) can be driven by the bar.
      Bar.Connect_Entry (+Entry_Widget);
   end Test_Connect_Entry;

   -------------------------
   -- Test_Accessibility --
   -------------------------

   procedure Test_Accessibility is
      Bar : Gtk_Search_Bar;
      Entry_Widget : Gtk_Search_Entry;
      First : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
   begin
      --  Use the procedure constructor as well as the function constructor
      --  covered by the basic search-bar cases.
      Gtk.Search_Bar.Gtk_New (Bar);
      Ref_Sink (Bar);
      Entry_Widget := Gtk_Search_Entry_New;
      Ref_Sink (Entry_Widget);
      Bar.Set_Child (Entry_Widget);

      --  GtkSearchBar has the documented search role. Detached widgets have
      --  no accessible parent, sibling, platform bounds, or AT context.
      Assert_True (Bar.Get_Accessible_Role = Accessible_Role_Search);
      Assert_True (Bar.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True
        (Bar.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      First := Bar.Get_First_Accessible_Child;
      Assert_True (First /= Null_Gtk_Accessible);
      Assert_True (First = Bar.Get_First_Accessible_Child);
      Assert_Cmpstr_Eq (Bar.Get_Accessible_Id, "");
      Context := Bar.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (Bar.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Bar.Get_Platform_State (Accessible_Platform_State_Focused));

      --  These inherited mutations are safe on a detached widget. A null
      --  accessible parent and sibling leave its accessible tree unchanged.
      Bar.Announce
        ("search bar test", Accessible_Announcement_Priority_Low);
      Bar.Reset_Property (Accessible_Property_Label);
      Bar.Reset_Relation (Accessible_Relation_Labelled_By);
      Bar.Reset_State (Accessible_State_Busy);
      Bar.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Bar.Get_Accessible_Parent = Null_Gtk_Accessible);
      Bar.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True
        (Bar.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Bar.Update_Platform_State (Accessible_Platform_State_Focusable);
      Assert_True (Bar.Get_Accessible_Role = Accessible_Role_Search);

      Unref (Bar);
      Unref (Entry_Widget);
   end Test_Accessibility;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; widgets
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/searchbar/capture-widget-destroy",
      Test_Capture_Widget_Destroy'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchbar/capture-widget-unset",
      Test_Capture_Widget_Unset'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchbar/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchbar/connect-entry", Test_Connect_Entry'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchbar/accessibility", Test_Accessibility'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Search_Bar;
