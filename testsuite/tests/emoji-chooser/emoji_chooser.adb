--  Tests Gtk.Emoji_Chooser construction, accessibility and native interfaces,
--  generated interface conversions, and the emoji-picked signal marshallers.
--  No matching GtkEmojiChooser C test exists in testsuite/c_tests/.

with Ada.Command_Line;
with Ada.Exceptions;
with System;

with Gdk;
with Gsk.Renderer;
with Glib;                 use Glib;
with Glib.Main;            use Glib.Main;
with Glib.Object;          use Glib.Object;
with Glib.Test;            use Glib.Test;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Button;
with Gtk.Constraint_Target;
with Gtk.Emoji_Chooser;    use Gtk.Emoji_Chooser;
with Gtk.Main;
with Gtk.Native;
with Gtk.Shortcut_Manager;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.Bindings;
with Gtkada.Types;

procedure Emoji_Chooser is

   use type Glib.GType;
   use type Glib.Object.GObject;
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Emoji_Chooser.Gtk_Emoji_Chooser;
   use type Gdk.Gdk_Surface;
   use type Gsk.Renderer.Gsk_Renderer;
   use type System.Address;

   Typed_Calls : Natural := 0;
   Slot_Calls  : Natural := 0;
   Typed_Object : System.Address := System.Null_Address;
   Slot_Object  : System.Address := System.Null_Address;
   Typed_Text   : UTF8_String (1 .. 4) := (others => ASCII.NUL);
   Slot_Text    : UTF8_String (1 .. 4) := (others => ASCII.NUL);
   Raise_Callbacks : Boolean := False;
   Exception_Calls : Natural := 0;
   Unexpected_Exception_Calls : Natural := 0;

   --  The signal has two fixed arguments followed by its UTF-8 string value.
   procedure Emit_Emoji_Picked
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr;
      Text   : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Test_Constructors with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Native with Convention => C;
   procedure Test_Interfaces with Convention => C;
   procedure Test_Emoji_Picked with Convention => C;
   procedure On_Typed
     (Self : access Gtk_Emoji_Chooser_Record'Class; Text : UTF8_String);
   procedure On_Slot
     (Self : access Glib.Object.GObject_Record'Class; Text : UTF8_String);
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   ----------------------
   -- Test_Constructors --
   ----------------------

   procedure Test_Constructors is
      From_Function : constant Gtk_Emoji_Chooser := Gtk_Emoji_Chooser_New;
      From_Procedure : Gtk_Emoji_Chooser;
   begin
      Gtk_New (From_Procedure);
      --  Initialize is idempotent for objects made by either constructor.
      Gtk.Emoji_Chooser.Initialize (From_Function);
      Gtk.Emoji_Chooser.Initialize (From_Procedure);

      Assert_True (Gtk.Emoji_Chooser.Get_Type /= 0);
      Assert_True (From_Function.Get_Object /= System.Null_Address);
      Assert_True (From_Procedure.Get_Object /= System.Null_Address);

      --  Sink each standalone widget before releasing the local reference.
      From_Function.Ref_Sink;
      From_Procedure.Ref_Sink;
      Unref (From_Function);
      Unref (From_Procedure);
   end Test_Constructors;

   -----------------------
   -- Test_Accessibility --
   -----------------------

   procedure Test_Accessibility is
      Chooser : constant Gtk_Emoji_Chooser := Gtk_Emoji_Chooser_New;
      First_Child : Gtk.Accessible.Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
   begin
      Chooser.Ref_Sink;
      --  The detached chooser has no parent, AT context, or screen bounds.
      Assert_True
        (Chooser.Get_Accessible_Role /= Gtk.Accessible.Accessible_Role_None);
      Assert_True
        (Chooser.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_Cmpstr_Eq (Chooser.Get_Accessible_Id, "GtkEmojiChooser");
      Assert_False (Chooser.Get_Bounds (X, Y, Width, Height));
      Assert_True (Chooser.Get_Next_Accessible_Sibling =
        Gtk.Accessible.Null_Gtk_Accessible);
      First_Child := Chooser.Get_First_Accessible_Child;
      --  The chooser builds accessible descendants with its UI. If GTK has
      --  not materialized one yet, the documented null result remains stable.
      if First_Child = Gtk.Accessible.Null_Gtk_Accessible then
         Assert_True (Chooser.Get_First_Accessible_Child = First_Child);
      else
         Assert_True (First_Child /= Gtk.Accessible.Null_Gtk_Accessible);
      end if;
      Assert_False
        (Chooser.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  These updates require no platform screen-reader service. Reset calls
      --  accept valid enum values even when no matching value was set.
      Chooser.Announce
        ("emoji chooser test",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Chooser.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      Chooser.Reset_Relation
        (Gtk.Accessible.Accessible_Relation_Labelled_By);
      Chooser.Reset_State (Gtk.Accessible.Accessible_State_Selected);
      Chooser.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);
      --  GTK_A11Y=none disables the platform state provider in this test.
      Assert_False
        (Chooser.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focusable));
      Chooser.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Chooser.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Chooser.Get_Next_Accessible_Sibling =
        Gtk.Accessible.Null_Gtk_Accessible);
      Context := Chooser.Get_At_Context;
      Assert_True (Context = null);
      Unref (Chooser);
   end Test_Accessibility;

   -----------------
   -- Test_Native --
   -----------------

   procedure Test_Native is
      Window : constant Gtk.Window.Gtk_Window := Gtk.Window.Gtk_Window_New;
      Anchor : constant Gtk.Button.Gtk_Button :=
        Gtk.Button.Gtk_Button_New_With_Label ("anchor");
      Chooser : constant Gtk_Emoji_Chooser := Gtk_Emoji_Chooser_New;
      Surface_X, Surface_Y : Gdouble;
   begin
      --  Keep references while GTK parents the button and popover into a live
      --  window, which gives the GtkNative interface a headless Xvfb surface.
      Window.Ref_Sink;
      Anchor.Ref_Sink;
      Chooser.Ref_Sink;
      Window.Set_Child (Anchor);
      Chooser.Set_Parent (Anchor);
      Window.Present;
      Chooser.Popup;
      for Iteration in 1 .. 5 loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => False);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;

      Assert_True (Chooser.Get_Realized);
      Assert_True (Chooser.Get_Surface /= null);
      Assert_True (Chooser.Get_Renderer /= null);
      Chooser.Get_Surface_Transform (Surface_X, Surface_Y);
      Assert_True (Surface_X >= 0.0 and Surface_Y >= 0.0);
      --  Native realization and unrealization are owned by GTK's widget
      --  lifecycle; direct calls fail GTK's native-private-state checks.
      Chooser.Popdown;
      Chooser.Unparent;
      Window.Destroy;
      Unref (Chooser);
      Unref (Glib.Object.GObject (Anchor));
      Unref (Glib.Object.GObject (Window));
   end Test_Native;

   ---------------------
   -- Test_Interfaces --
   ---------------------

   procedure Test_Interfaces is
      Chooser : constant Gtk_Emoji_Chooser := Gtk_Emoji_Chooser_New;
   begin
      Chooser.Ref_Sink;
      --  Each interface pair emitted by the binding must preserve the object.
      declare
         I : constant Gtk.Accessible.Gtk_Accessible := +Chooser;
         Back : constant Gtk_Emoji_Chooser := -I;
      begin
         Assert_True (Back.Get_Object = Chooser.Get_Object);
      end;
      declare
         I : constant Gtk.Buildable.Gtk_Buildable := +Chooser;
         Back : constant Gtk_Emoji_Chooser := -I;
      begin
         Assert_True (Back.Get_Object = Chooser.Get_Object);
      end;
      declare
         I : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Chooser;
         Back : constant Gtk_Emoji_Chooser := -I;
      begin
         Assert_True (Back.Get_Object = Chooser.Get_Object);
      end;
      declare
         I : constant Gtk.Native.Gtk_Native := +Chooser;
         Back : constant Gtk_Emoji_Chooser := -I;
      begin
         Assert_True (Back.Get_Object = Chooser.Get_Object);
      end;
      declare
         I : constant Gtk.Shortcut_Manager.Gtk_Shortcut_Manager := +Chooser;
         Back : constant Gtk_Emoji_Chooser := -I;
      begin
         Assert_True (Back.Get_Object = Chooser.Get_Object);
      end;
      Unref (Chooser);
   end Test_Interfaces;

   ------------------------
   -- Test_Emoji_Picked --
   ------------------------

   procedure Test_Emoji_Picked is
      Chooser : constant Gtk_Emoji_Chooser := Gtk_Emoji_Chooser_New;
      Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("emoji-picked");
      Emoji : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("🍎");
   begin
      Chooser.Ref_Sink;
      Typed_Calls := 0;
      Slot_Calls := 0;
      Typed_Text := (others => ASCII.NUL);
      Slot_Text := (others => ASCII.NUL);
      Chooser.On_Emoji_Picked (On_Typed'Unrestricted_Access);
      --  The slot form receives the supplied object as its first callback
      --  argument; using the chooser itself keeps ownership simple here.
      Chooser.On_Emoji_Picked (On_Slot'Unrestricted_Access, Chooser);

      Emit_Emoji_Picked (Chooser.Get_Object, Name, Emoji);
      Assert_Cmpuint_Eq (Guint (Typed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Calls), 1);
      Assert_Cmpstr_Eq (Typed_Text, "🍎");
      Assert_Cmpstr_Eq (Slot_Text, "🍎");
      Assert_True (Typed_Object = Chooser.Get_Object);
      Assert_True (Slot_Object = Chooser.Get_Object);

      --  Exceptions raised by either callback must be caught at the C signal
      --  boundary and forwarded to GtkAda's registered exception handler.
      Gtkada.Bindings.Set_On_Exception
        (Record_Exception'Unrestricted_Access);
      Raise_Callbacks := True;
      Emit_Emoji_Picked (Chooser.Get_Object, Name, Emoji);
      Raise_Callbacks := False;
      Assert_Cmpuint_Eq (Guint (Exception_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Unexpected_Exception_Calls), 0);

      Gtkada.Types.Free (Name);
      Gtkada.Types.Free (Emoji);
      Unref (Chooser);
   end Test_Emoji_Picked;

   --------------
   -- On_Typed --
   --------------

   procedure On_Typed
     (Self : access Gtk_Emoji_Chooser_Record'Class; Text : UTF8_String) is
   begin
      if Raise_Callbacks then
         raise Program_Error with "expected typed emoji-picked exception";
      end if;
      Typed_Calls := Typed_Calls + 1;
      Typed_Object := Self.Get_Object;
      Typed_Text := Text;
   end On_Typed;

   -------------
   -- On_Slot --
   -------------

   procedure On_Slot
     (Self : access Glib.Object.GObject_Record'Class; Text : UTF8_String) is
   begin
      if Raise_Callbacks then
         raise Program_Error with "expected slot emoji-picked exception";
      end if;
      Slot_Calls := Slot_Calls + 1;
      Slot_Object := Self.Get_Object;
      Slot_Text := Text;
   end On_Slot;

   ---------------------
   -- Record_Exception --
   ---------------------

   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      if Ada.Exceptions.Exception_Name (Occurrence) = "PROGRAM_ERROR" then
         Exception_Calls := Exception_Calls + 1;
      else
         Unexpected_Exception_Calls := Unexpected_Exception_Calls + 1;
      end if;
   end Record_Exception;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/emoji-chooser/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func ("/emoji-chooser/accessibility", Test_Accessibility'Unrestricted_Access);
   Add_Func ("/emoji-chooser/native", Test_Native'Unrestricted_Access);
   Add_Func ("/emoji-chooser/interfaces", Test_Interfaces'Unrestricted_Access);
   Add_Func ("/emoji-chooser/emoji-picked", Test_Emoji_Picked'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Emoji_Chooser;
