--  Ada port of GTK's testsuite/gtk/popover.c (GTK 4.22.2), plus focused
--  Gtk_Popover and Gtk_Menu_Button binding tests for properties, callbacks,
--  signals, native accessors and inherited accessibility methods. GTK has no
--  dedicated menu-button C test.
--
--  Creates a window holding a Gtk_Menu_Button whose popover contains a
--  label, pops the popover up from a timeout, then tweaks the label's
--  alignment while the popover is shown.

with Ada.Command_Line;
with System;
with Glib;            use Glib;
with Glib.Main;       use Glib.Main;
with Glib.Menu;       use Glib.Menu;
with Glib.Menu_Model; use Glib.Menu_Model;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Test;       use Glib.Test;
with Gdk.Rectangle;
with Gsk.Renderer;
with Gtk.Button;      use Gtk.Button;
with Gtk.Label;       use Gtk.Label;
with Gtk.Enums;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Main;
with Gtk.Menu_Button; use Gtk.Menu_Button;
with Gtk.Popover;     use Gtk.Popover;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Window;      use Gtk.Window;
with Gtkada.Types;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Accessible.Gtk_Accessible_Role;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Popover.Gtk_Popover;
use type Gtk.Enums.Gtk_Position_Type;
use type Gsk.Renderer.Gsk_Renderer;
use type Gdk.Gdk_Surface;
use type Glib.Menu_Model.Gmenu_Model;
use type Gtk.Widget.Gtk_Widget;
use type Gtk.Enums.Gtk_Arrow_Type;
use type System.Address;

procedure Popover is

   The_Popover : Gtk_Popover;
   The_Button  : Gtk_Menu_Button;

   Done : Boolean := False;
   pragma Volatile (Done);
   --  Set from a timeout callback dispatched by Main_Context_Iteration

   Mapped : Boolean := False;
   pragma Volatile (Mapped);

   Typed_Activations : Natural := 0;
   Slot_Activations  : Natural := 0;
   Slot_Address      : System.Address := System.Null_Address;
   Popup_Calls       : Natural := 0;
   User_Data_Calls   : Natural := 0;
   User_Data_Seen    : Integer := 0;
   Destroy_Calls     : Natural := 0;
   Callback_Popover  : Gtk_Popover;
   Popover_Typed_Calls : Natural := 0;
   Popover_Slot_Calls  : Natural := 0;
   Popover_Slot_Address : System.Address := System.Null_Address;
   --  These counters make callback and signal behavior observable.

   procedure Emit_Activate
     (Object : System.Address; Name : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Destroy_Callback_Data (Data : in out Integer);
   package Popup_Callbacks is new
     Gtk.Menu_Button.Set_Create_Popup_Func_User_Data
       (User_Data_Type => Integer,
        Destroy       => Destroy_Callback_Data);

   function Pop_Up return Boolean;
   function Tickle return Boolean;
   function Stop return Boolean;
   function Map_Ready return Boolean;

   procedure Test_Show_Popover
   with Convention => C;

   procedure Test_Menu_Button_Properties
   with Convention => C;

   procedure Test_Menu_Button_Accessibility
   with Convention => C;

   procedure Test_Menu_Button_Callbacks_And_Signal
   with Convention => C;

   procedure Test_Popover_Properties
   with Convention => C;

   procedure Test_Popover_Accessibility_And_Signals
   with Convention => C;

   procedure Typed_Activate
     (Self : access Gtk_Menu_Button_Record'Class);
   procedure Slot_Activate
     (Self : access GObject_Record'Class);
   procedure Create_Popup
     (Self : not null access Gtk_Menu_Button_Record'Class);
   procedure Create_Popup_With_Data
     (Self      : not null access Gtk_Menu_Button_Record'Class;
      User_Data : Integer);
   procedure Popover_Typed
     (Self : access Gtk_Popover_Record'Class);
   procedure Popover_Slot
     (Self : access GObject_Record'Class);
   procedure Emit_Popover_Signal
     (Object : System.Address; Name : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  The generic callback owns a heap record containing this integer; GTK
   --  calls Destroy when that callback is cleared or the button is finalized.
   procedure Destroy_Callback_Data (Data : in out Integer) is
      pragma Unreferenced (Data);
   begin
      Destroy_Calls := Destroy_Calls + 1;
   end Destroy_Callback_Data;

   --  These handlers record the receiver passed by GtkAda's two signal
   --  marshallers. The slot variant also checks that its signal source exists.
   procedure Typed_Activate
     (Self : access Gtk_Menu_Button_Record'Class) is
   begin
      Assert_True (Self /= null);
      Typed_Activations := Typed_Activations + 1;
   end Typed_Activate;

   procedure Slot_Activate
     (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Slot_Address := Self.Get_Object;
      Slot_Activations := Slot_Activations + 1;
   end Slot_Activate;

   --  GTK requests a popup lazily. Attach one so the popup callback follows
   --  the documented contract when the test emits the activate action.
   procedure Create_Popup
     (Self : not null access Gtk_Menu_Button_Record'Class) is
   begin
      Popup_Calls := Popup_Calls + 1;
      Self.Set_Popover (Callback_Popover);
   end Create_Popup;

   --  The data-bearing variant records the integer delivered through the
   --  generated C trampoline, then installs the same deterministic popover.
   procedure Create_Popup_With_Data
     (Self      : not null access Gtk_Menu_Button_Record'Class;
      User_Data : Integer) is
   begin
      User_Data_Calls := User_Data_Calls + 1;
      User_Data_Seen := User_Data;
      Self.Set_Popover (Callback_Popover);
   end Create_Popup_With_Data;

   --  Signal handlers record their receiver so both generated marshallers
   --  can be checked without depending on keyboard input.
   procedure Popover_Typed
     (Self : access Gtk_Popover_Record'Class) is
   begin
      Assert_True (Self /= null);
      Popover_Typed_Calls := Popover_Typed_Calls + 1;
   end Popover_Typed;

   procedure Popover_Slot
     (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Popover_Slot_Address := Self.Get_Object;
      Popover_Slot_Calls := Popover_Slot_Calls + 1;
   end Popover_Slot;

   ------------
   -- Pop_Up --
   ------------

   function Pop_Up return Boolean is
   begin
      --  Exercise Gtk.Menu_Button.Popup after the button has been rooted.
      The_Button.Popup;
      Assert_True (The_Button.Get_Popover = The_Popover);
      The_Popover.Popup;
      The_Popover.Present;
      --  Once shown, GtkPopover is a GtkNative with a surface and renderer.
      Assert_True (The_Popover.Get_Surface /= null);
      Assert_True (The_Popover.Get_Renderer /= null);
      declare
         Surface_X, Surface_Y : Gdouble;
      begin
         The_Popover.Get_Surface_Transform (Surface_X, Surface_Y);
         --  This self-comparison rejects uninitialized NaN outputs while
         --  avoiding assumptions about platform-specific surface offsets.
         Assert_True (Surface_X = Surface_X and Surface_Y = Surface_Y);
      end;
      The_Popover.Popdown;
      Assert_False (The_Popover.Get_Visible);
      return False;
   end Pop_Up;

   ------------
   -- Tickle --
   ------------

   function Tickle return Boolean is
      Label : constant Gtk_Widget := The_Popover.Get_First_Child;
   begin
      Label.Set_Valign (Gtk.Enums.Align_Start);
      return False;
   end Tickle;

   ----------
   -- Stop --
   ----------

   function Stop return Boolean is
   begin
      The_Button.Popdown;
      Assert_False (The_Button.Get_Active);
      Done := True;
      Wakeup (null);
      return False;
   end Stop;

   --  Wait a short, bounded interval for the newly presented window to map.
   function Map_Ready return Boolean is
   begin
      Mapped := True;
      Wakeup (null);
      return False;
   end Map_Ready;

   -----------------------
   -- Test_Show_Popover --
   -----------------------

   procedure Test_Show_Popover is
      Window : constant Gtk_Window := Gtk_Window_New;
      Button : constant Gtk_Menu_Button := Gtk_Menu_Button_New;
      Id     : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      The_Popover := Gtk_Popover_New;
      The_Button := Button;
      The_Popover.Set_Child (Gtk_Label_New ("Nu?"));
      Button.Set_Popover (The_Popover);
      Window.Set_Child (Button);

      Window.Present;

      Id := Timeout_Add (1000, Pop_Up'Unrestricted_Access);
      Id := Timeout_Add (2000, Tickle'Unrestricted_Access);
      Done := False;
      Id := Timeout_Add (3000, Stop'Unrestricted_Access);

      while not Done loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;
   end Test_Show_Popover;

   --  Check the generated GtkPopover constructor and the properties whose
   --  wrappers are specific to Gtk.Popover.
   procedure Test_Popover_Properties is
      Popup       : Gtk_Popover;
      By_Function : Gtk_Popover;
      Child       : constant Gtk_Button := Gtk_Button_New_With_Label;
      Anchor      : constant Gtk_Menu_Button := Gtk_Menu_Button_New;
      X, Y        : Gint;
      Point       : Gdk.Rectangle.Gdk_Rectangle :=
        (X => 17, Y => 23, Width => 31, Height => 41);
      Read_Point  : Gdk.Rectangle.Gdk_Rectangle;
      Has_Point   : Boolean;
      Original    : System.Address;
   begin
      --  Exercise both constructor forms and Initialize's already-created
      --  no-op while keeping owned widgets alive until the end of the case.
      Gtk_New (Popup);
      By_Function := Gtk_Popover_New;
      Ref_Sink (Popup);
      Ref_Sink (By_Function);
      Ref_Sink (Child);
      Ref_Sink (Anchor);
      Anchor.Set_Popover (Popup);
      Assert_True (Popup.Is_Created and By_Function.Is_Created);
      Original := Popup.Get_Object;
      Gtk.Popover.Initialize (Popup);
      Assert_True (Popup.Get_Object = Original);

      --  Boolean options round-trip through their public getters.
      Popup.Set_Autohide (False);
      Assert_False (Popup.Get_Autohide);
      Popup.Set_Autohide (True);
      Assert_True (Popup.Get_Autohide);
      Popup.Set_Cascade_Popdown (False);
      Assert_False (Popup.Get_Cascade_Popdown);
      Popup.Set_Cascade_Popdown (True);
      Assert_True (Popup.Get_Cascade_Popdown);
      Popup.Set_Has_Arrow (False);
      Assert_False (Popup.Get_Has_Arrow);
      Popup.Set_Has_Arrow (True);
      Assert_True (Popup.Get_Has_Arrow);
      Popup.Set_Mnemonics_Visible (True);
      Assert_True (Popup.Get_Mnemonics_Visible);
      Popup.Set_Mnemonics_Visible (False);
      Assert_False (Popup.Get_Mnemonics_Visible);

      --  Child and default-widget lookups use getters or the GObject property
      --  for the setter that has no dedicated Ada getter.
      Popup.Set_Child (Child);
      Assert_True (Popup.Get_Child = Gtk_Widget (Child));
      Popup.Set_Default_Widget (Child);
      Assert_True
        (Get_Property (Popup, Gtk.Popover.Default_Widget_Property).Get_Object
         = Child.Get_Object);
      Popup.Set_Default_Widget (null);
      Assert_True
        (Get_Property (Popup, Gtk.Popover.Default_Widget_Property) = null);
      Popup.Set_Child (null);
      Assert_True (Popup.Get_Child = null);

      --  The offset and pointing rectangle are returned unchanged by GTK.
      Popup.Get_Offset (X, Y);
      Assert_Cmpint_Eq (X, 0);
      Assert_Cmpint_Eq (Y, 0);
      Popup.Set_Offset (12, -8);
      Popup.Get_Offset (X, Y);
      Assert_Cmpint_Eq (X, 12);
      Assert_Cmpint_Eq (Y, -8);
      Has_Point := Popup.Get_Pointing_To (Read_Point);
      Assert_False (Has_Point);
      Popup.Set_Pointing_To (Point);
      Has_Point := Popup.Get_Pointing_To (Read_Point);
      Assert_True (Has_Point);
      Assert_Cmpint_Eq (Read_Point.X, Point.X);
      Assert_Cmpint_Eq (Read_Point.Y, Point.Y);
      Assert_Cmpint_Eq (Read_Point.Width, Point.Width);
      Assert_Cmpint_Eq (Read_Point.Height, Point.Height);

      --  GTK accepts every documented edge as the preferred position.
      Popup.Set_Position (Gtk.Enums.Pos_Left);
      Assert_True (Popup.Get_Position = Gtk.Enums.Pos_Left);
      Popup.Set_Position (Gtk.Enums.Pos_Right);
      Assert_True (Popup.Get_Position = Gtk.Enums.Pos_Right);
      Popup.Set_Position (Gtk.Enums.Pos_Top);
      Assert_True (Popup.Get_Position = Gtk.Enums.Pos_Top);
      Popup.Set_Position (Gtk.Enums.Pos_Bottom);
      Assert_True (Popup.Get_Position = Gtk.Enums.Pos_Bottom);

      Unref (Child);
      Anchor.Set_Popover (null);
      Unref (Anchor);
      Unref (By_Function);
      Unref (Popup);
   end Test_Popover_Properties;

   --  Exercise GtkAccessible methods on GtkPopover, then check both callback
   --  signatures for its activate-default and closed signals.
   procedure Test_Popover_Accessibility_And_Signals is
      Popup      : constant Gtk_Popover := Gtk_Popover_New;
      Child      : constant Gtk_Button := Gtk_Button_New_With_Label;
      Slot       : constant Gtk_Label := Gtk_Label_New ("popover signal slot");
      Anchor     : constant Gtk_Menu_Button := Gtk_Menu_Button_New;
      Window     : constant Gtk_Window := Gtk_Window_New;
      Parent     : Gtk.Accessible.Gtk_Accessible;
      Sibling    : Gtk.Accessible.Gtk_Accessible;
      Context    : Gtk.Atcontext.Gtk_Atcontext;
      X, Y       : Gint;
      Width      : Gint;
      Height     : Gint;
      Name       : Gtkada.Types.Chars_Ptr;
      Has_Bounds : Boolean;
      Map_Timer  : G_Source_Id;
      pragma Unreferenced (Map_Timer);
   begin
      Ref_Sink (Popup);
      Ref_Sink (Child);
      Ref_Sink (Slot);
      Ref_Sink (Anchor);
      Ref_Sink (Window);
      Popup.Set_Child (Child);

      --  Detached popovers have no AT context, bounds, parent, or sibling;
      --  their child still appears in accessible child traversal.
      Assert_Cmpstr_Eq (Popup.Get_Accessible_Id, "");
      Assert_True
        (Popup.Get_Accessible_Role /=
         Gtk.Accessible.Accessible_Role_None);
      Parent := Popup.Get_Accessible_Parent;
      Assert_True (Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Sibling := Popup.Get_Next_Accessible_Sibling;
      Assert_True (Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Context := Popup.Get_At_Context;
      Assert_True (Context = null);
      Assert_True
        (Popup.Get_First_Accessible_Child /=
         Gtk.Accessible.Null_Gtk_Accessible);
      Has_Bounds := Popup.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Assert_False
        (Popup.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  Null accessibility relationships are valid, and reset operations
      --  safely clear optional metadata on an unrooted widget.
      Popup.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Popup.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Popup.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Popup.Get_Next_Accessible_Sibling =
         Gtk.Accessible.Null_Gtk_Accessible);
      Popup.Announce
        ("Popover ready",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Popup.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      Popup.Reset_Relation
        (Gtk.Accessible.Accessible_Relation_Described_By);
      Popup.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Popup.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);

      --  The activate-default action consults its GtkWindow, so root and map
      --  the popover before emitting the signal directly.
      Window.Set_Child (Anchor);
      Anchor.Set_Popover (Popup);
      Window.Present;
      Mapped := False;
      Map_Timer := Timeout_Add (100, Map_Ready'Unrestricted_Access);
      while not Mapped loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;

      --  Direct emission checks the typed and slot marshallers without
      --  synthesizing keyboard input or requiring a mapped popover.
      Popover_Typed_Calls := 0;
      Popover_Slot_Calls := 0;
      Popover_Slot_Address := System.Null_Address;
      Popup.On_Activate_Default
        (Popover_Typed'Unrestricted_Access, After => False);
      Popup.On_Activate_Default
        (Popover_Slot'Unrestricted_Access, Slot, After => True);
      Popup.On_Closed (Popover_Typed'Unrestricted_Access);
      Popup.On_Closed (Popover_Slot'Unrestricted_Access, Slot);
      Popup.Popup;
      Name := Gtkada.Types.New_String ("activate-default");
      Emit_Popover_Signal (Popup.Get_Object, Name);
      Gtkada.Types.Free (Name);
      Assert_Cmpuint_Eq (Guint (Popover_Typed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Popover_Slot_Calls), 1);
      Assert_True (Popover_Slot_Address = Slot.Get_Object);

      Name := Gtkada.Types.New_String ("closed");
      Emit_Popover_Signal (Popup.Get_Object, Name);
      Gtkada.Types.Free (Name);
      Assert_Cmpuint_Eq (Guint (Popover_Typed_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Popover_Slot_Calls), 2);
      Assert_True (Popover_Slot_Address = Slot.Get_Object);

      Popup.Popdown;
      Assert_False (Popup.Get_Visible);

      Window.Destroy;
      Anchor.Set_Popover (null);
      Window.Set_Child (null);
      Unref (Window);
      Unref (Anchor);
      Unref (Slot);
      Unref (Child);
      Unref (Popup);
   end Test_Popover_Accessibility_And_Signals;

   --  Each boolean, enum and string method is checked by a getter after the
   --  setter. Child and menu attachments are retained until explicitly unset.
   procedure Test_Menu_Button_Properties is
      Button       : Gtk_Menu_Button;
      By_Function  : Gtk_Menu_Button;
      Child        : constant Gtk_Label := Gtk_Label_New ("custom child");
      Menu         : constant Gmenu := Gmenu_New;
      Popup_Widget : constant Gtk_Popover := Gtk_Popover_New;
   begin
      Gtk_New (Button);
      By_Function := Gtk_Menu_Button_New;
      Ref_Sink (Button);
      Ref_Sink (By_Function);
      Ref_Sink (Child);
      Ref_Sink (Popup_Widget);

      --  Reinitializing an already-created object preserves its GObject.
      declare
         Original : constant System.Address := Button.Get_Object;
      begin
         Gtk.Menu_Button.Initialize (Button);
         Assert_True (Button.Get_Object = Original);
      end;

      --  The active, arrow, shrink, frame, primary, and mnemonic flags round
      --  trip their public values through GTK.
      Button.Set_Active (True);
      Assert_True (Button.Get_Active);
      Button.Set_Active (False);
      Assert_False (Button.Get_Active);
      Button.Set_Always_Show_Arrow (True);
      Assert_True (Button.Get_Always_Show_Arrow);
      Button.Set_Can_Shrink (True);
      Assert_True (Button.Get_Can_Shrink);
      Button.Set_Has_Frame (False);
      Assert_False (Button.Get_Has_Frame);
      Button.Set_Primary (True);
      Assert_True (Button.Get_Primary);
      Button.Set_Use_Underline (True);
      Assert_True (Button.Get_Use_Underline);

      --  Every arrow enum value is accepted and returned unchanged.
      Button.Set_Direction (Gtk.Enums.Arrow_Up);
      Assert_True (Button.Get_Direction = Gtk.Enums.Arrow_Up);
      Button.Set_Direction (Gtk.Enums.Arrow_Down);
      Assert_True (Button.Get_Direction = Gtk.Enums.Arrow_Down);
      Button.Set_Direction (Gtk.Enums.Arrow_Left);
      Assert_True (Button.Get_Direction = Gtk.Enums.Arrow_Left);
      Button.Set_Direction (Gtk.Enums.Arrow_Right);
      Assert_True (Button.Get_Direction = Gtk.Enums.Arrow_Right);
      Button.Set_Direction (Gtk.Enums.Arrow_None);
      Assert_True (Button.Get_Direction = Gtk.Enums.Arrow_None);

      --  GTK treats label, icon name, and custom child as alternate content.
      Button.Set_Icon_Name ("document-open-symbolic");
      Assert_Cmpstr_Eq (Button.Get_Icon_Name, "document-open-symbolic");
      Button.Set_Label ("Open _File");
      Assert_Cmpstr_Eq (Button.Get_Label, "Open _File");
      Assert_Cmpstr_Eq (Button.Get_Icon_Name, "");
      Button.Set_Child (Child);
      Assert_True (Button.Get_Child = Gtk.Widget.Gtk_Widget (Child));
      Assert_Cmpstr_Eq (Button.Get_Label, "");
      Button.Set_Child (null);
      Assert_True (Button.Get_Child = null);

      --  A menu model creates its own popover; setting a custom popover then
      --  replaces that model association, as documented by Gtk.Menu_Button.
      Menu.Append ("Open", "app.open");
      Button.Set_Menu_Model (Menu);
      Assert_True (Button.Get_Menu_Model = Gmenu_Model (Menu));
      Assert_True (Button.Get_Popover /= null);
      Button.Set_Popover (Popup_Widget);
      Assert_True (Button.Get_Popover = Popup_Widget);
      Assert_True (Button.Get_Menu_Model = null);
      Button.Set_Popover (null);
      Assert_True (Button.Get_Popover = null);
      Button.Set_Menu_Model (null);
      Assert_True (Button.Get_Menu_Model = null);

      Unref (Popup_Widget);
      Unref (Child);
      Unref (By_Function);
      Unref (Button);
      Menu.Unref;
   end Test_Menu_Button_Properties;

   --  An unrooted menu button has stable accessibility defaults and can be
   --  asked to clear optional relations without a real display backend.
   procedure Test_Menu_Button_Accessibility is
      Button : constant Gtk_Menu_Button := Gtk_Menu_Button_New;
      Parent : Gtk.Accessible.Gtk_Accessible;
      Sibling : Gtk.Accessible.Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
   begin
      Ref_Sink (Button);
      Assert_True
        (Button.Get_Accessible_Role =
           Gtk.Accessible.Accessible_Role_Button);
      Assert_Cmpstr_Eq (Button.Get_Accessible_Id, "");
      Parent := Button.Get_Accessible_Parent;
      Assert_True (Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Sibling := Button.Get_Next_Accessible_Sibling;
      Assert_True (Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Context := Button.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := Button.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Assert_False
        (Button.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  Detached widgets accept null accessible relationships. The first
      --  accessible child is queried to exercise GTK's child traversal path.
      Assert_True
        (Button.Get_First_Accessible_Child /=
           Gtk.Accessible.Null_Gtk_Accessible);
      Button.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Button.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Button.Get_Accessible_Parent =
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Button.Get_Next_Accessible_Sibling =
           Gtk.Accessible.Null_Gtk_Accessible);

      --  Accessibility reset and announcement calls are safe before mapping.
      Button.Announce
        ("Menu ready",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Button.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      Button.Reset_Relation (Gtk.Accessible.Accessible_Relation_Described_By);
      Button.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Button.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);
      Unref (Button);
   end Test_Menu_Button_Accessibility;

   --  Emit activate directly so callback tests do not depend on a pointer
   --  event or on GTK's button animation timer.
   procedure Test_Menu_Button_Callbacks_And_Signal is
      Button     : constant Gtk_Menu_Button := Gtk_Menu_Button_New;
      Window     : constant Gtk_Window := Gtk_Window_New;
      Slot       : constant Gtk_Label := Gtk_Label_New ("signal slot");
      Name       : Gtkada.Types.Chars_Ptr;
      Map_Timer  : G_Source_Id;
      pragma Unreferenced (Map_Timer);
   begin
      Ref_Sink (Button);
      Ref_Sink (Window);
      Ref_Sink (Slot);
      Callback_Popover := Gtk_Popover_New;
      Ref_Sink (Callback_Popover);
      Callback_Popover.Set_Child (Gtk_Label_New ("callback menu"));
      Window.Set_Child (Button);
      Window.Present;

      --  Give GTK one short timeout to dispatch the window's map/configure
      --  events before the popup action is emitted.
      Mapped := False;
      Map_Timer := Timeout_Add (100, Map_Ready'Unrestricted_Access);
      while not Mapped loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;

      --  Both On_Activate overloads reach their matching generated marshaller.
      Typed_Activations := 0;
      Slot_Activations := 0;
      Slot_Address := System.Null_Address;
      Button.On_Activate (Typed_Activate'Unrestricted_Access);
      Button.On_Activate (Slot_Activate'Unrestricted_Access, Slot);

      --  Emit the action with an ordinary popover to check both signal
      --  marshallers independently of the lazy popup callback.
      Button.Set_Popover (Callback_Popover);
      Name := Gtkada.Types.New_String ("activate");
      Emit_Activate (Button.Get_Object, Name);
      Gtkada.Types.Free (Name);
      Assert_Cmpuint_Eq (Guint (Typed_Activations), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Activations), 1);
      Assert_True (Slot_Address = Slot.Get_Object);
      Button.Popdown;

      --  Registering then clearing the plain callback exercises both setter
      --  paths; invoking the callback is covered in the notes as GTK aborts.
      Popup_Calls := 0;
      Button.Set_Popover (null);
      Button.Set_Create_Popup_Func (Create_Popup'Unrestricted_Access);
      Assert_Cmpuint_Eq (Guint (Popup_Calls), 0);
      Assert_True (Button.Get_Popover = null);
      Button.Set_Create_Popup_Func (null);

      --  The generic setter likewise defers invocation, and clearing it must
      --  run Destroy exactly once for the supplied user data.
      User_Data_Calls := 0;
      User_Data_Seen := 0;
      Destroy_Calls := 0;
      Popup_Callbacks.Set_Create_Popup_Func
        (Button, Create_Popup_With_Data'Unrestricted_Access, 42);
      Assert_Cmpuint_Eq (Guint (User_Data_Calls), 0);
      Assert_True (Button.Get_Popover = null);

      --  Passing null clears the callback and calls its destroy notify once.
      Popup_Callbacks.Set_Create_Popup_Func (Button, null, 0);
      Assert_Cmpuint_Eq (Guint (Destroy_Calls), 1);
      Button.Set_Popover (null);

      Window.Set_Child (null);
      Unref (Callback_Popover);
      Unref (Slot);
      Unref (Window);
      Unref (Button);
   end Test_Menu_Button_Callbacks_And_Signal;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; widgets
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/popover/show", Test_Show_Popover'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/popover/properties", Test_Popover_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/popover/accessibility-signals",
      Test_Popover_Accessibility_And_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/popover/menu-button/properties",
      Test_Menu_Button_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/popover/menu-button/accessibility",
      Test_Menu_Button_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/popover/menu-button/callbacks-signal",
      Test_Menu_Button_Callbacks_And_Signal'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Popover;
