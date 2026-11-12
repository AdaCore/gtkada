--  GtkAccessibleHyperlink tests. GTK 4.22 has no matching C tests in
--  testsuite/c_tests, so these cases exercise the public constructor and
--  accessible-object methods directly.

with Ada.Command_Line;

with Glib;                   use Glib;
with Glib.Test;              use Glib.Test;
with Gtk.Accessible;         use Gtk.Accessible;
with Gtk.Accessible_Hyperlink; use Gtk.Accessible_Hyperlink;
with Gtk.Accessible_Hypertext; use Gtk.Accessible_Hypertext;
with Gtk.Accessible_Text;
with Gtk.Atcontext;
with Gtk.Label;              use Gtk.Label;
with Gtk.Main;

procedure Main is

   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible_Hypertext.Gtk_Accessible_Hypertext;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Accessible_Hyperlink.Gtk_Accessible_Hyperlink;

   --  Both constructors receive a label, which implements AccessibleHypertext.
   procedure Test_Constructors
   with Convention => C;

   --  Exercise the inherited GtkAccessible methods on the hyperlink object.
   procedure Test_Accessible_Methods
   with Convention => C;

   --  Call the interface-level wrappers themselves, rather than the
   --  object-specific inherited wrappers generated for GtkAccessibleHyperlink.
   procedure Test_Accessible_Interface_Wrappers
   with Convention => C;

   -----------------------
   -- Test_Constructors --
   -----------------------

   procedure Test_Constructors is
      Label       : Gtk_Label;
      Parent      : Gtk_Accessible_Hypertext;
      By_Function : Gtk_Accessible_Hyperlink;
      By_Procedure : Gtk_Accessible_Hyperlink;
      Bounds      : constant Gtk.Accessible_Text.Gtk_Accessible_Text_Range :=
        (Start => 2, Length => 5);
   begin
      Label := Gtk_Label_New ("link text");
      Label.Ref_Sink;
      Parent := +Label;

      --  The interface package's identity operator preserves its address.
      Assert_True (Gtk.Accessible_Hypertext."+" (Parent) = Parent);

      --  The function constructor and Gtk_New both initialize fresh objects.
      By_Function := Gtk_Accessible_Hyperlink_New
        (Parent, 0, "https://gtk.test/first", Bounds);
      Gtk_New (By_Procedure, Parent, 1, "https://gtk.test/second", Bounds);
      Assert_True (By_Function /= null);
      Assert_True (By_Procedure /= null);

      --  Initialize is documented as a no-op after an object already exists.
      By_Function.Initialize
        (Parent, 2, "https://gtk.test/ignored", Bounds);
      Assert_True
        (By_Function.Get_Accessible_Role = Accessible_Role_Link);
      Assert_True (By_Function.Get_Accessible_Parent = +Label);

      --  The generated interface conversions preserve the same object.
      declare
         As_Accessible : constant Gtk_Accessible := +By_Function;
      begin
         Assert_True (-As_Accessible = By_Function);
      end;

      By_Function.Unref;
      By_Procedure.Unref;
      Label.Unref;
   end Test_Constructors;

   ----------------------------
   -- Test_Accessible_Methods --
   ----------------------------

   procedure Test_Accessible_Methods is
      Label   : Gtk_Label;
      Parent  : Gtk_Accessible_Hypertext;
      Link    : Gtk_Accessible_Hyperlink;
      X, Y, Width, Height : Gint;
      Bounds  : constant Gtk.Accessible_Text.Gtk_Accessible_Text_Range :=
        (Start => 0, Length => 4);
   begin
      Label := Gtk_Label_New ("hypertext parent");
      Label.Ref_Sink;
      Parent := +Label;

      --  A link can be represented by a text range without mapping a window.
      Link := Gtk_Accessible_Hyperlink_New
        (Parent, 0, "https://gtk.test/link", Bounds);
      Assert_True (Gtk.Accessible_Hyperlink.Get_Type /= GType_Invalid);
      Assert_True (Link.Get_Accessible_Role = Accessible_Role_Link);
      Assert_True (Link.Get_Accessible_Parent = +Label);
      Assert_Cmpstr_Eq (Link.Get_Accessible_Id, "");
      Assert_True
        (Link.Get_First_Accessible_Child = Null_Gtk_Accessible);
      Assert_True
        (Link.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  The label is unrooted, so there is no AT context or screen bounds.
      Assert_True (Link.Get_At_Context = null);
      Assert_False (Link.Get_Bounds (X, Y, Width, Height));

      --  This plain-text label has no hyperlink sibling list; the parent is
      --  still observable, and the null sibling remains null after an update.
      Link.Set_Accessible_Parent (+Label, Null_Gtk_Accessible);
      Assert_True (Link.Get_Accessible_Parent = +Label);
      Link.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True
        (Link.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Platform states can be set and queried; update notifies GTK of a
      --  platform change, while reset methods exercise the inherited API.
      Assert_False
        (Link.Get_Platform_State (Accessible_Platform_State_Active));
      Link.Set_Platform_State (Accessible_Platform_State_Active, True);
      Assert_True
        (Link.Get_Platform_State (Accessible_Platform_State_Active));
      Link.Set_Platform_State (Accessible_Platform_State_Active, False);
      Assert_False
        (Link.Get_Platform_State (Accessible_Platform_State_Active));
      Link.Update_Platform_State (Accessible_Platform_State_Focusable);
      Link.Announce
        ("accessible hyperlink test", Accessible_Announcement_Priority_Low);
      Link.Reset_Property (Accessible_Property_Label);
      Link.Reset_Relation (Accessible_Relation_Described_By);
      Link.Reset_State (Accessible_State_Busy);

      Link.Unref;
      Label.Unref;
   end Test_Accessible_Methods;

   ----------------------------------------
   -- Test_Accessible_Interface_Wrappers --
   ----------------------------------------

   procedure Test_Accessible_Interface_Wrappers is
      Label : Gtk_Label;
      Parent : Gtk_Accessible_Hypertext;
      Link : Gtk_Accessible_Hyperlink;
      Accessible : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Bounds : constant Gtk.Accessible_Text.Gtk_Accessible_Text_Range :=
        (Start => 0, Length => 4);
   begin
      Label := Gtk_Label_New ("interface wrapper parent");
      Label.Ref_Sink;
      Parent := +Label;
      Link := Gtk_Accessible_Hyperlink_New
        (Parent, 0, "https://gtk.test/interface", Bounds);

      --  Convert the hyperlink through its implemented interface, then use
      --  Gtk.Accessible's identity conversion and wrappers explicitly.
      Accessible := +Link;
      Accessible := Gtk.Accessible."+" (Accessible);
      Assert_Cmpstr_Eq
        (Gtk.Accessible.Get_Accessible_Id (Accessible), "");

      --  This unattached accessible object has no context or screen bounds.
      Context := Gtk.Accessible.Get_At_Context (Accessible);
      Assert_True (Context = null);
      Assert_False
        (Gtk.Accessible.Get_Bounds (Accessible, X, Y, Width, Height));
      Assert_False
        (Gtk.Accessible.Get_Platform_State
           (Accessible, Accessible_Platform_State_Focused));

      --  Announcements are safe with GTK's test environment's disabled AT.
      Gtk.Accessible.Announce
        (Accessible, "interface wrapper test",
         Accessible_Announcement_Priority_Low);

      Link.Unref;
      Label.Unref;
   end Test_Accessible_Interface_Wrappers;

begin
   Glib.Test.Init;
   --  GTK initialization is required before creating the label and button.
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/accessible-hyperlink/constructors",
      Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/accessible-hyperlink/methods",
      Test_Accessible_Methods'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/accessible-hyperlink/interface-wrappers",
      Test_Accessible_Interface_Wrappers'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
