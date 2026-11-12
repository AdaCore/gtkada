--  Exercise Glib.Notification constructors and every setter. GLib has no
--  public getters for the notification's private title, body, actions, or
--  priority, so these tests also verify the live GObject type after mutation.
--  There is no focused C notification test to port; notify.c tests GObject
--  property-change signals and does not use GNotification.

with Ada.Command_Line;

with Glib.G_Icon;
with Glib.Notification;
with Glib.Object;
with Glib.Test;
with Glib.Variant;
with Gtkada.Types;

procedure Main is

   use Glib.Notification;
   use Glib.Test;
   use type Glib.GType;
   use type Glib.G_Icon.G_Icon;

   --  GtkAda exposes GIcon as an interface proxy without a constructor. Use
   --  GLib's themed-icon constructor to provide Set_Icon with a valid icon.
   function New_Themed_Icon
     (Name : Gtkada.Types.Chars_Ptr) return Glib.G_Icon.G_Icon;
   pragma Import (C, New_Themed_Icon, "g_themed_icon_new");

   procedure Unref_Icon (Icon : Glib.G_Icon.G_Icon);
   pragma Import (C, Unref_Icon, "g_object_unref");

   --  Check the observable GObject state shared by all constructors and
   --  retained after the notification's private data is updated.
   procedure Assert_Notification (Item : Gnotification) is
   begin
      Assert_True (Item /= null);
      Assert_True (Glib.Object.Is_Created (Item.all));
      Assert_True
        (Glib.Object.Get_Type (Glib.Object.GObject (Item)) =
           Glib.Notification.Get_Type);
   end Assert_Notification;

   --  Cover the out-parameter, function-returning, and explicit Initialize
   --  construction paths, including Initialize's already-created no-op path.
   procedure Test_Constructors with Convention => C;
   procedure Test_Constructors is
      From_Out      : Gnotification;
      From_Function : constant Gnotification := Gnotification_New ("Function");
      From_Initialize : constant Gnotification := new Gnotification_Record;
   begin
      G_New (From_Out, "Out parameter");
      Initialize (From_Initialize, "Initialized");
      Assert_Notification (From_Out);
      Assert_Notification (From_Function);
      Assert_Notification (From_Initialize);

      --  Reinitializing an existing instance is documented to leave it intact.
      Initialize (From_Initialize, "Ignored replacement");
      Assert_Notification (From_Initialize);

      Glib.Object.Unref (Glib.Object.GObject (From_Out));
      Glib.Object.Unref (Glib.Object.GObject (From_Function));
      Glib.Object.Unref (Glib.Object.GObject (From_Initialize));
   end Test_Constructors;

   --  Exercise text setters with real values and clearable category state.
   procedure Test_Text_And_Priority with Convention => C;
   procedure Test_Text_And_Priority is
      Item : constant Gnotification := Gnotification_New ("Initial title");
   begin
      Set_Title (Item, "Updated title");
      Set_Body (Item, "A notification body");
      Set_Category (Item, "email.arrived");
      Set_Category (Item);

      --  Each public priority value is accepted by the wrapper's C enum call.
      for Priority in GNotification_Priority loop
         Set_Priority (Item, Priority);
      end loop;

      Assert_Notification (Item);
      Glib.Object.Unref (Glib.Object.GObject (Item));
   end Test_Text_And_Priority;

   --  Exercise action overloads, GVariant targets, icon passing, and the
   --  deprecated compatibility setter. Notification clicks are not dispatched
   --  here because that requires a desktop notification service.
   procedure Test_Actions_And_Icon with Convention => C;
   procedure Test_Actions_And_Icon is
      Item      : constant Gnotification := Gnotification_New ("Actions");
      Target    : constant Glib.Variant.Gvariant :=
        Glib.Variant.Gvariant_New_String ("document-42");
      Icon_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("text-x-generic");
      Icon      : constant Glib.G_Icon.G_Icon := New_Themed_Icon (Icon_Name);
   begin
      Gtkada.Types.Free (Icon_Name);
      Assert_True (Icon /= null);

      Add_Button (Item, "Open", "app.open");
      Add_Button_With_Target_Value (Item, "Open document", "app.open", Target);
      Add_Button_With_Target_Value
        (Item, "Open default", "app.open", Glib.Variant.Null_Gvariant);
      Set_Default_Action (Item, "app.open");
      Set_Default_Action_And_Target_Value (Item, "app.open", Target);
      Set_Default_Action_And_Target_Value
        (Item, "app.open", Glib.Variant.Null_Gvariant);
      Set_Icon (Item, Icon);

      --  Set_Urgent is obsolete but remains a supported binding entry point.
      pragma Warnings (Off);
      Set_Urgent (Item, True);
      Set_Urgent (Item, False);
      pragma Warnings (On);

      Assert_Notification (Item);
      Unref_Icon (Icon);
      Glib.Object.Unref (Glib.Object.GObject (Item));
   end Test_Actions_And_Icon;

begin
   Glib.Test.Init;
   Add_Func ("/glib-notification/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func ("/glib-notification/text-priority", Test_Text_And_Priority'Unrestricted_Access);
   Add_Func ("/glib-notification/actions-icon", Test_Actions_And_Icon'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
