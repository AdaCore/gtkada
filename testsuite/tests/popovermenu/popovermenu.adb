--  Ada port of GTK's testsuite/gtk/popovermenu.c (test_set_model), extended
--  to cover Gtk.Popover_Menu constructors, custom children, flags, inherited
--  accessibility methods, and its implemented interfaces.
--
--  GTK's C test creates its models through GtkBuilder XML. GtkBuilder is not
--  bound here, so the equivalent GMenu models are built programmatically.

with Ada.Command_Line;
with Gdk;
with Glib;
with Glib.Menu;            use Glib.Menu;
with Glib.Menu_Model;      use Glib.Menu_Model;
with Glib.Object;          use Glib.Object;
with Glib.Test;            use Glib.Test;
with Glib.Variant;         use Glib.Variant;
with Gsk.Renderer;
with Gtk.Accessible;       use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Label;
with Gtk.Main;
with Gtk.Native;
with Gtk.Popover_Menu;     use Gtk.Popover_Menu;
with Gtk.Shortcut_Manager;
with Gtk.Widget;
with System;

use type Gdk.Gdk_Surface;
use type Glib.Gdouble;
use type Glib.Menu_Model.Gmenu_Model;
use type Gsk.Renderer.Gsk_Renderer;
use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Buildable.Gtk_Buildable;
use type Gtk.Constraint_Target.Gtk_Constraint_Target;
use type Gtk.Native.Gtk_Native;
use type Gtk.Shortcut_Manager.Gtk_Shortcut_Manager;
use type Gtk.Widget.Gtk_Widget;
use type System.Address;

procedure Popovermenu is

   procedure Test_Set_Model
   with Convention => C;

   procedure Test_Constructors_And_Children
   with Convention => C;

   procedure Test_Accessibility_And_Interfaces
   with Convention => C;

   --------------------
   -- Test_Set_Model --
   --------------------

   procedure Test_Set_Model is
      --  This case is the direct Ada port of test_set_model in popovermenu.c.
      Menu1   : constant Gmenu := Gmenu_New;
      Menu2   : constant Gmenu := Gmenu_New;
      Popover : Gtk_Popover_Menu;
   begin
      Menu1.Append ("Record events", "record.record-events");
      Menu2.Append ("Do not record events", "record.no-record-events");

      --  Confirm model replacement and clearing on a constructed popover.
      Popover := Gtk_Popover_Menu_New_From_Model (Menu1);
      Assert_True (Popover.Get_Menu_Model = Gmenu_Model (Menu1));
      Popover.Set_Menu_Model (null);
      Assert_True (Popover.Get_Menu_Model = null);

      --  Keep the position changes from the C test around the model changes.
      Popover.Set_Position (Pos_Left);
      Popover.Set_Menu_Model (Menu2);
      Assert_True (Popover.Get_Menu_Model = Gmenu_Model (Menu2));
      Popover.Set_Position (Pos_Bottom);

      --  Sink the floating widget reference before releasing the test object.
      Ref_Sink (Popover);
      Popover.Unref;
      Menu1.Unref;
      Menu2.Unref;
   end Test_Set_Model;

   --------------------------------------
   -- Test_Constructors_And_Children --
   --------------------------------------

   procedure Test_Constructors_And_Children is
      --  The custom attribute in the model defines the child insertion slot.
      Primary      : constant Gmenu := Gmenu_New;
      Alternate    : constant Gmenu := Gmenu_New;
      Custom_Item  : constant Gmenu_Item := Gmenu_Item_New;
      By_Function  : Gtk_Popover_Menu;
      By_Procedure : Gtk_Popover_Menu;
      Full_Function  : Gtk_Popover_Menu;
      Full_Procedure : Gtk_Popover_Menu;
      Child        : constant Gtk.Label.Gtk_Label :=
        Gtk.Label.Gtk_Label_New ("custom menu widget");
   begin
      Custom_Item.Set_Attribute_Value
        ("custom", Gvariant_New_String ("test-slot"));
      Primary.Append_Item (Custom_Item);
      Alternate.Append ("alternate item", "app.alternate");

      --  Exercise the two default-sliding constructors and retain their refs.
      By_Function := Gtk_Popover_Menu_New_From_Model (Primary);
      Ref_Sink (By_Function);
      Gtk_New_From_Model (By_Procedure, Primary);
      Ref_Sink (By_Procedure);
      Assert_True (By_Function.Get_Menu_Model = Gmenu_Model (Primary));
      Assert_True (By_Procedure.Get_Menu_Model = Gmenu_Model (Primary));
      Assert_True (By_Function.Get_Flags = Popover_Menu_Sliding);

      --  A full constructor preserves the requested nested-menu flag.
      Full_Function :=
        Gtk_Popover_Menu_New_From_Model_Full (Alternate, Popover_Menu_Nested);
      Ref_Sink (Full_Function);
      Gtk_New_From_Model_Full
        (Full_Procedure, Alternate, Popover_Menu_Nested);
      Ref_Sink (Full_Procedure);
      Assert_True (Full_Function.Get_Menu_Model = Gmenu_Model (Alternate));
      Assert_True (Full_Procedure.Get_Menu_Model = Gmenu_Model (Alternate));
      Assert_True (Full_Function.Get_Flags = Popover_Menu_Nested);
      Assert_True (Full_Procedure.Get_Flags = Popover_Menu_Nested);

      --  Initialize* is a no-op after construction, as its documentation says.
      By_Procedure.Initialize_From_Model (Alternate);
      Full_Procedure.Initialize_From_Model_Full
        (Primary, Popover_Menu_Sliding);
      Assert_True (By_Procedure.Get_Menu_Model = Gmenu_Model (Primary));
      Assert_True (Full_Procedure.Get_Menu_Model = Gmenu_Model (Alternate));
      Assert_True (Full_Procedure.Get_Flags = Popover_Menu_Nested);

      --  Flags can be changed independently after construction.
      By_Function.Set_Flags (Popover_Menu_Nested);
      Assert_True (By_Function.Get_Flags = Popover_Menu_Nested);
      By_Function.Set_Flags (Popover_Menu_Sliding);
      Assert_True (By_Function.Get_Flags = Popover_Menu_Sliding);

      --  A mismatched ID is rejected; the model's custom slot accepts the
      --  caller-owned widget, which can then be removed exactly once.
      Child.Ref_Sink;
      Assert_False (By_Function.Add_Child (Child, "missing-slot"));
      Assert_True (By_Function.Add_Child (Child, "test-slot"));
      Assert_True (Child.Get_Parent /= null);
      Assert_True (By_Function.Remove_Child (Child));

      --  Replacing the model through the setter is visible through the getter.
      By_Function.Set_Menu_Model (Alternate);
      Assert_True (By_Function.Get_Menu_Model = Gmenu_Model (Alternate));
      By_Function.Set_Menu_Model (null);
      Assert_True (By_Function.Get_Menu_Model = null);

      --  Release every sunk widget and model reference created by this case.
      Child.Unref;
      Unref (Full_Procedure);
      Unref (Full_Function);
      Unref (By_Procedure);
      Unref (By_Function);
      Unref (Custom_Item);
      Unref (Alternate);
      Unref (Primary);
   end Test_Constructors_And_Children;

   --------------------------------------------
   -- Test_Accessibility_And_Interfaces --
   --------------------------------------------

   procedure Test_Accessibility_And_Interfaces is
      --  Without a root window, an unpresented popover has no native surface.
      Menu       : constant Gmenu := Gmenu_New;
      Popover    : Gtk_Popover_Menu;
      Parent     : Gtk_Accessible;
      First      : Gtk_Accessible;
      At_Context : Gtk.Atcontext.Gtk_Atcontext;
      Renderer   : Gsk.Renderer.Gsk_Renderer;
      Surface    : Gdk.Gdk_Surface;
      X, Y, Width, Height : Glib.Gint;
      Surface_X, Surface_Y : Glib.Gdouble;
   begin
      Menu.Append ("accessible item", "app.accessible");
      Popover := Gtk_Popover_Menu_New_From_Model (Menu);
      Ref_Sink (Popover);

      --  Check the role and the stable, empty identity of this unrooted widget.
      Assert_True (Popover.Get_Accessible_Role = Accessible_Role_Menu);
      Assert_Cmpstr_Eq (Popover.Get_Accessible_Id, "");
      Parent := Popover.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Assert_True
        (Popover.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      First := Popover.Get_First_Accessible_Child;
      Assert_True (Popover.Get_First_Accessible_Child = First);
      At_Context := Popover.Get_At_Context;
      Assert_True (At_Context = null);
      Assert_False (Popover.Get_Bounds (X, Y, Width, Height));

      --  Exercise accessible updates that do not require an accessibility
      --  backend; the focusable platform state can be queried immediately.
      Popover.Set_Focusable (True);
      Popover.Update_Platform_State (Accessible_Platform_State_Focusable);
      Assert_True
        (Popover.Get_Platform_State (Accessible_Platform_State_Focusable));
      Popover.Reset_Property (Accessible_Property_Label);
      Popover.Reset_Relation (Accessible_Relation_Labelled_By);
      Popover.Reset_State (Accessible_State_Busy);
      Popover.Announce
        ("popover menu test", Accessible_Announcement_Priority_Low);
      Popover.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True
        (Popover.Get_Accessible_Parent = Null_Gtk_Accessible);
      Popover.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True
        (Popover.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Native queries are safe on an unrooted native widget and return no
      --  renderer or surface until GTK realizes the popover in a window.
      Renderer := Popover.Get_Renderer;
      Surface := Popover.Get_Surface;
      Assert_True (Renderer = null);
      Assert_True (Surface = null);
      Popover.Get_Surface_Transform (Surface_X, Surface_Y);
      Assert_True (Surface_X = 0.0 and Surface_Y = 0.0);

      --  Round-trip every interface conversion declared by Gtk.Popover_Menu.
      declare
         Accessible : constant Gtk.Accessible.Gtk_Accessible := +Popover;
         Back       : constant Gtk_Popover_Menu := -Accessible;
      begin
         Assert_True (Back.Get_Object = Popover.Get_Object);
      end;
      declare
         Buildable : constant Gtk.Buildable.Gtk_Buildable := +Popover;
         Back      : constant Gtk_Popover_Menu := -Buildable;
      begin
         Assert_True (Back.Get_Object = Popover.Get_Object);
      end;
      declare
         Target : constant Gtk.Constraint_Target.Gtk_Constraint_Target :=
           +Popover;
         Back : constant Gtk_Popover_Menu := -Target;
      begin
         Assert_True (Back.Get_Object = Popover.Get_Object);
      end;
      declare
         Native : constant Gtk.Native.Gtk_Native := +Popover;
         Back   : constant Gtk_Popover_Menu := -Native;
      begin
         Assert_True (Back.Get_Object = Popover.Get_Object);
      end;
      declare
         Manager : constant Gtk.Shortcut_Manager.Gtk_Shortcut_Manager :=
           +Popover;
         Back : constant Gtk_Popover_Menu := -Manager;
      begin
         --  The manager interface's unary plus is an identity conversion.
         Assert_True (+Manager = Manager);
         Assert_True (Back.Get_Object = Popover.Get_Object);
      end;

      Unref (Popover);
      Unref (Menu);
   end Test_Accessibility_And_Interfaces;

begin
   Glib.Test.Init;

   --  GTK initialization is required before any widget constructor is called.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/popover/menu/set-model", Test_Set_Model'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/popover/menu/constructors-and-children",
      Test_Constructors_And_Children'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/popover/menu/accessibility-and-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);

   --  Return with the GLib test runner's exit code.
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Popovermenu;
