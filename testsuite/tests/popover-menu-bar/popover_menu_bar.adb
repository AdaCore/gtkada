--  Exercises Gtk.Popover_Menu_Bar's model constructors, custom-child API,
--  accessibility methods, and implemented interface conversions.
--  There is no direct popover-menu-bar case in testsuite/c_tests; the GTK UI
--  fixtures test ordinary menu models, so this case builds its model in Ada.

with Ada.Command_Line;
with Glib.Menu;          use Glib.Menu;
with Glib.Menu_Model;    use Glib.Menu_Model;
with Glib.Object;        use Glib.Object;
with Glib.Test;          use Glib.Test;
with Glib.Variant;       use Glib.Variant;
with Gtk.Accessible;     use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Label;
with Gtk.Main;
with Gtk.Popover_Menu_Bar; use Gtk.Popover_Menu_Bar;
with Gtk.Widget;
with System;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Buildable.Gtk_Buildable;
use type Gtk.Constraint_Target.Gtk_Constraint_Target;
use type Glib.Menu_Model.Gmenu_Model;
use type System.Address;

procedure Popover_Menu_Bar is

   procedure Test_Model_And_Custom_Children
   with Convention => C;

   -------------------------------
   -- Test_Model_And_Custom_Children --
   -------------------------------

   procedure Test_Model_And_Custom_Children is
      --  GtkPopoverMenuBar expects a submenu at each top-level model entry.
      Menu             : constant Gmenu := Gmenu_New;
      Submenu          : constant Gmenu := Gmenu_New;
      Custom_Item      : constant Gmenu_Item := Gmenu_Item_New;
      Replacement      : constant Gmenu := Gmenu_New;
      Replacement_Menu : constant Gmenu := Gmenu_New;
      Bar              : Gtk_Popover_Menu_Bar;
      By_Procedure     : Gtk_Popover_Menu_Bar;
      Child            : constant Gtk.Label.Gtk_Label :=
        Gtk.Label.Gtk_Label_New ("inserted custom widget");
      Parent           : Gtk_Accessible;
      First            : Gtk_Accessible;
      Context          : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Glib.Gint;
   begin
      --  The custom attribute names the slot to which Add_Child attaches.
      Custom_Item.Set_Attribute_Value
        ("custom", Gvariant_New_String ("test-slot"));
      Submenu.Append_Item (Custom_Item);
      Menu.Append_Submenu ("File", Gmenu_Model (Submenu));
      Replacement_Menu.Append ("replacement item", "app.replacement");
      Replacement.Append_Submenu
        ("Edit", Gmenu_Model (Replacement_Menu));
      --  These unparented widgets retain their floating references in the test.
      Child.Ref_Sink;

      --  Exercise both public constructor forms and the already-created guard.
      Bar := Gtk_Popover_Menu_Bar_New_From_Model (Menu);
      Ref_Sink (Bar);
      Gtk_New_From_Model (By_Procedure, Replacement);
      Ref_Sink (By_Procedure);
      Initialize_From_Model (By_Procedure, Menu);
      Assert_True (By_Procedure.Get_Menu_Model = Gmenu_Model (Replacement));
      Assert_True (Bar.Get_Menu_Model = Gmenu_Model (Menu));

      --  A matching model slot accepts the caller-owned custom widget.
      Assert_True (Bar.Add_Child (Child, "test-slot"));
      Assert_True (Bar.Remove_Child (Child));

      --  Replacing and clearing the model are reflected by the getter.
      Bar.Set_Menu_Model (Replacement);
      Assert_True (Bar.Get_Menu_Model = Gmenu_Model (Replacement));
      Bar.Set_Menu_Model (null);
      Assert_True (Bar.Get_Menu_Model = null);
      Bar.Set_Menu_Model (Menu);
      Assert_True (Bar.Get_Menu_Model = Gmenu_Model (Menu));

      --  An unrooted bar has no screen bounds or AT context, but has the
      --  menubar role and a stable accessible-child result.
      Assert_True (Bar.Get_Accessible_Role = Accessible_Role_Menu_Bar);
      Assert_Cmpstr_Eq (Bar.Get_Accessible_Id, "");
      Parent := Bar.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Assert_True (Bar.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      First := Bar.Get_First_Accessible_Child;
      Assert_True (Bar.Get_First_Accessible_Child = First);
      Context := Bar.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (Bar.Get_Bounds (X, Y, Width, Height));

      --  Accessibility updates are queryable without attaching a real window.
      Assert_False
        (Bar.Get_Platform_State (Accessible_Platform_State_Focusable));
      Bar.Set_Focusable (True);
      Bar.Update_Platform_State (Accessible_Platform_State_Focusable);
      Assert_True
        (Bar.Get_Platform_State (Accessible_Platform_State_Focusable));
      Bar.Reset_Property (Accessible_Property_Label);
      Bar.Reset_Relation (Accessible_Relation_Labelled_By);
      Bar.Reset_State (Accessible_State_Busy);
      Bar.Announce ("popover menu bar test", Accessible_Announcement_Priority_Low);
      Bar.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Bar.Get_Accessible_Parent = Null_Gtk_Accessible);
      Bar.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Bar.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Each declared interface conversion must round-trip to the same GObject.
      declare
         Accessible : constant Gtk.Accessible.Gtk_Accessible := +Bar;
         Round_Trip : constant Gtk_Popover_Menu_Bar := -Accessible;
      begin
         Assert_True (Round_Trip.Get_Object = Bar.Get_Object);
      end;
      declare
         Buildable : constant Gtk.Buildable.Gtk_Buildable := +Bar;
         Round_Trip : constant Gtk_Popover_Menu_Bar := -Buildable;
      begin
         Assert_True (Round_Trip.Get_Object = Bar.Get_Object);
      end;
      declare
         Target : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Bar;
         Round_Trip : constant Gtk_Popover_Menu_Bar := -Target;
      begin
         Assert_True (Round_Trip.Get_Object = Bar.Get_Object);
      end;

      Unref (By_Procedure);
      Unref (Bar);
      Child.Unref;
      Unref (Custom_Item);
      Unref (Replacement_Menu);
      Unref (Replacement);
      Unref (Submenu);
      Unref (Menu);
   end Test_Model_And_Custom_Children;

begin
   Glib.Test.Init;
   --  As with the C GTK tests, initialize GTK before constructing widgets.
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/popover-menu-bar/model-and-custom-children",
      Test_Model_And_Custom_Children'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Popover_Menu_Bar;
