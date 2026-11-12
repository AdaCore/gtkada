--  Exercises Glib.App_Launch_Context constructors, methods, and signal
--  marshallers. There is no matching app-launch-context C test to port.

with Ada.Command_Line;
with GNAT.Strings;
with System;

with Glib;
with Glib.App_Info;
with Glib.App_Launch_Context;
with Glib.Error;
with Glib.GFile;
with Glib.Object;
with Glib.Test;
with Glib.Types;
with Glib.Variant;
with Gtkada.Types;

procedure Main is
   use Glib.App_Info;
   use Glib.App_Launch_Context;
   use Glib.Test;

   use type GNAT.Strings.String_Access;
   use type Glib.App_Info.Gapp_Info;
   use type Glib.App_Launch_Context.Gapp_Launch_Context;
   use type Glib.Error.GError;
   use type Glib.Variant.Gvariant;
   use type System.Address;

   Context : Gapp_Launch_Context;
   Info    : Gapp_Info := Null_Gapp_Info;
   Variant : Glib.Variant.Gvariant := Glib.Variant.Null_Gvariant;
   Setup_Error : Glib.Error.GError;

   Failed_Context_Calls : Natural := 0;
   Failed_Slot_Calls    : Natural := 0;
   Started_Context_Calls : Natural := 0;
   Started_Slot_Calls    : Natural := 0;
   Launched_Context_Calls : Natural := 0;
   Launched_Slot_Calls    : Natural := 0;
   Saw_Failed_Id          : Boolean := False;
   Saw_Started_Info       : Boolean := False;
   Saw_Started_Variant    : Boolean := False;
   Saw_Launched_Info      : Boolean := False;
   Saw_Launched_Variant   : Boolean := False;
   Saw_Slot_Object        : Boolean := False;

   --  Fixed C profiles pass the pointer arguments to GLib's variadic signal
   --  emitter using the platform C calling convention.
   procedure Emit_Info_Variant
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr;
      App    : System.Address;
      Data   : System.Address);
   pragma Import (C, Emit_Info_Variant, "g_signal_emit_by_name");

   procedure Test_Methods with Convention => C;
   procedure Test_Signals with Convention => C;

   procedure On_Failed_Context
     (Self : access Gapp_Launch_Context_Record'Class;
      Id   : Glib.UTF8_String);
   procedure On_Failed_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Id   : Glib.UTF8_String);
   procedure On_Started_Context
     (Self : access Gapp_Launch_Context_Record'Class;
      App  : Gapp_Info;
      Data : Glib.Variant.Gvariant);
   procedure On_Started_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      App  : Gapp_Info;
      Data : Glib.Variant.Gvariant);
   procedure On_Launched_Context
     (Self : access Gapp_Launch_Context_Record'Class;
      App  : Gapp_Info;
      Data : Glib.Variant.Gvariant);
   procedure On_Launched_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      App  : Gapp_Info;
      Data : Glib.Variant.Gvariant);

   --  The callbacks record arguments for assertions after signal emission.
   procedure On_Failed_Context
     (Self : access Gapp_Launch_Context_Record'Class;
      Id   : Glib.UTF8_String) is
   begin
      Failed_Context_Calls := Failed_Context_Calls + 1;
      Saw_Failed_Id := Saw_Failed_Id or else Id = "gtkada-startup-id";
   end On_Failed_Context;

   procedure On_Failed_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Id   : Glib.UTF8_String) is
   begin
      Failed_Slot_Calls := Failed_Slot_Calls + 1;
      Saw_Failed_Id := Saw_Failed_Id or else Id = "gtkada-startup-id";
      Saw_Slot_Object := Saw_Slot_Object or else Self /= null;
   end On_Failed_Slot;

   procedure On_Started_Context
     (Self : access Gapp_Launch_Context_Record'Class;
      App  : Gapp_Info;
      Data : Glib.Variant.Gvariant) is
   begin
      Started_Context_Calls := Started_Context_Calls + 1;
      Saw_Started_Info := Saw_Started_Info or else App = Info;
      Saw_Started_Variant := Saw_Started_Variant or else
        (Data /= Glib.Variant.Null_Gvariant
         and then Glib.Variant.Get_String (Data) = "platform-data");
   end On_Started_Context;

   procedure On_Started_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      App  : Gapp_Info;
      Data : Glib.Variant.Gvariant) is
   begin
      Started_Slot_Calls := Started_Slot_Calls + 1;
      Saw_Started_Info := Saw_Started_Info or else App = Info;
      Saw_Started_Variant := Saw_Started_Variant or else
        (Data /= Glib.Variant.Null_Gvariant
         and then Glib.Variant.Get_String (Data) = "platform-data");
      Saw_Slot_Object := Saw_Slot_Object or else Self /= null;
   end On_Started_Slot;

   procedure On_Launched_Context
     (Self : access Gapp_Launch_Context_Record'Class;
      App  : Gapp_Info;
      Data : Glib.Variant.Gvariant) is
   begin
      Launched_Context_Calls := Launched_Context_Calls + 1;
      Saw_Launched_Info := Saw_Launched_Info or else App = Info;
      Saw_Launched_Variant := Saw_Launched_Variant or else
        (Data /= Glib.Variant.Null_Gvariant
         and then Glib.Variant.Get_String (Data) = "platform-data");
   end On_Launched_Context;

   procedure On_Launched_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      App  : Gapp_Info;
      Data : Glib.Variant.Gvariant) is
   begin
      Launched_Slot_Calls := Launched_Slot_Calls + 1;
      Saw_Launched_Info := Saw_Launched_Info or else App = Info;
      Saw_Launched_Variant := Saw_Launched_Variant or else
        (Data /= Glib.Variant.Null_Gvariant
         and then Glib.Variant.Get_String (Data) = "platform-data");
      Saw_Slot_Object := Saw_Slot_Object or else Self /= null;
   end On_Launched_Slot;

   --  GNAT.Strings owns each copied environment string returned by the
   --  binding, so the test releases those strings after each assertion.
   procedure Free_Entries (Items : in out GNAT.Strings.String_List);

   --  Check that each child environment entry has the expected full value.
   function Has_Entry (Value : String) return Boolean;

   function Has_Entry (Value : String) return Boolean is
      Items : GNAT.Strings.String_List := Context.Get_Environment;
      Found : Boolean := False;
   begin
      for Item of Items loop
         if Item /= null and then Item.all = Value then
            Found := True;
            exit;
         end if;
      end loop;
      Free_Entries (Items);
      return Found;
   end Has_Entry;

   procedure Free_Entries (Items : in out GNAT.Strings.String_List) is
   begin
      for Item of Items loop
         if Item /= null then
            GNAT.Strings.Free (Item);
         end if;
      end loop;
   end Free_Entries;

   ------------------
   -- Test_Methods --
   ------------------

   --  Check both constructors, idempotent initialization, display and startup
   --  metadata, and the child environment setters against observable results.
   procedure Test_Methods is
      Procedure_Context : Gapp_Launch_Context;
      Function_Context  : Gapp_Launch_Context;
      Found       : Boolean;
   begin
      --  Exercise both constructor forms and the already-initialized branch.
      G_New (Procedure_Context);
      Assert_True (Procedure_Context /= null);
      Glib.App_Launch_Context.Initialize (Procedure_Context);
      Function_Context := Gapp_Launch_Context_New;
      Assert_True (Function_Context /= null);

      --  Use the pre-created command-line app only as stable method input.
      Assert_True (Info /= Null_Gapp_Info);
      Assert_Cmpstr_Eq
        (Context.Get_Display (Info, Glib.GFile.Gfile_List.Null_List), "");
      Assert_Cmpstr_Eq
        (Context.Get_Startup_Notify_Id
           (Info, Glib.GFile.Gfile_List.Null_List), "");

      --  A private variable avoids assumptions about the test process env.
      Context.Setenv ("GTKADA_APP_LAUNCH_CONTEXT_TEST", "first-value");
      --  Replacing the same key must expose the latest value to child launches.
      Found := Has_Entry ("GTKADA_APP_LAUNCH_CONTEXT_TEST=first-value");
      Assert_True (Found);

      Context.Setenv ("GTKADA_APP_LAUNCH_CONTEXT_TEST", "second-value");
      Found := Has_Entry ("GTKADA_APP_LAUNCH_CONTEXT_TEST=second-value");
      Assert_True (Found);

      Context.Unsetenv ("GTKADA_APP_LAUNCH_CONTEXT_TEST");
      Found := Has_Entry ("GTKADA_APP_LAUNCH_CONTEXT_TEST=second-value");
      Assert_False (Found);

      --  The base implementation reports launch failure through its signal.
      Glib.App_Launch_Context.On_Launch_Failed
        (Context, On_Failed_Context'Unrestricted_Access);
      Glib.App_Launch_Context.On_Launch_Failed
        (Context, On_Failed_Slot'Unrestricted_Access,
         Glib.Types.To_Object (Glib.Types.GType_Interface (Info)));
      Context.Launch_Failed ("gtkada-startup-id");
      Assert_Cmpuint_Eq (Glib.Guint (Failed_Context_Calls), 1);
      Assert_Cmpuint_Eq (Glib.Guint (Failed_Slot_Calls), 1);
      Assert_True (Saw_Failed_Id);
      Assert_True (Saw_Slot_Object);
   end Test_Methods;

   ------------------
   -- Test_Signals --
   ------------------

   --  Check that both callback overloads receive the signal's app info and
   --  platform data, including the slot object passed to the generic form.
   procedure Test_Signals is
      Object : constant Glib.Object.GObject :=
        Glib.Types.To_Object (Glib.Types.GType_Interface (Info));
      Name   : Gtkada.Types.Chars_Ptr;
   begin
      --  The slot overloads receive this independent GObject as their Self.
      Glib.App_Launch_Context.On_Launch_Started
        (Context, On_Started_Context'Unrestricted_Access);
      Glib.App_Launch_Context.On_Launch_Started
        (Context, On_Started_Slot'Unrestricted_Access, Object);
      Glib.App_Launch_Context.On_Launched
        (Context, On_Launched_Context'Unrestricted_Access);
      Glib.App_Launch_Context.On_Launched
        (Context, On_Launched_Slot'Unrestricted_Access, Object);

      --  These emissions exercise the two GAppInfo/GVariant marshaller pairs
      --  without starting a process.
      Name := Gtkada.Types.New_String ("launch-started");
      Emit_Info_Variant
        (Glib.Object.Get_Object (Context), Name,
         Glib.Object.Get_Object (Object), Glib.Get_Object (Variant));
      Gtkada.Types.Free (Name);

      Name := Gtkada.Types.New_String ("launched");
      --  The variant was sunk above so the first emission did not consume it.
      Emit_Info_Variant
        (Glib.Object.Get_Object (Context), Name,
         Glib.Object.Get_Object (Object), Glib.Get_Object (Variant));
      Gtkada.Types.Free (Name);

      Assert_Cmpuint_Eq (Glib.Guint (Started_Context_Calls), 1);
      Assert_Cmpuint_Eq (Glib.Guint (Started_Slot_Calls), 1);
      Assert_Cmpuint_Eq (Glib.Guint (Launched_Context_Calls), 1);
      Assert_Cmpuint_Eq (Glib.Guint (Launched_Slot_Calls), 1);
      Assert_True (Saw_Started_Info and Saw_Started_Variant);
      Assert_True (Saw_Launched_Info and Saw_Launched_Variant);
      Assert_True (Saw_Slot_Object);
   end Test_Signals;

begin
   --  The base context can be exercised without initializing GTK or a display.
   Glib.Test.Init;
   Context := Gapp_Launch_Context_New;
   Info := Create_From_Commandline
     ("gtkada-app-launch-context-probe", Flags => G_App_Info_Create_None,
      Error => Setup_Error);
   Assert_True (Setup_Error = null);
   Assert_True (Info /= Null_Gapp_Info);
   --  Sink the constructor's floating reference so both signal emissions can
   --  borrow the same immutable variant without destroying it on first use.
   Variant := Glib.Variant.Ref_Sink
     (Glib.Variant.Gvariant_New_String ("platform-data"));

   Add_Func ("/app-launch-context/methods", Test_Methods'Unrestricted_Access);
   Add_Func ("/app-launch-context/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Run);
end Main;
