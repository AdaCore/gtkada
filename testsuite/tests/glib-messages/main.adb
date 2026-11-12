--  Exercise Glib.Messages logging, callbacks, handler removal, and fatal masks.
--  No focused C logging test covers these Ada bindings, so these cases check
--  the public behavior directly through GLib's logging facility.

with Ada.Command_Line;
with Interfaces.C.Strings;

with Glib;
with Glib.Messages; use Glib.Messages;
with Glib.Test;     use Glib.Test;

procedure Main is

   Calls        : Natural := 0;
   Seen_Domain  : String (1 .. 40) := (others => ' ');
   Seen_Length  : Natural := 0;
   Seen_Message : String (1 .. 40) := (others => ' ');
   Message_Size : Natural := 0;
   Seen_Level   : Log_Level_Flags := 0;

   --  The Ada API takes a String domain and therefore cannot pass NULL; this
   --  native call supplies it to test C_Log_Func's null-domain conversion.
   procedure Native_Log
     (Log_Domain : Interfaces.C.Strings.chars_ptr;
      Log_Level  : Log_Level_Flags;
      Format     : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Native_Log, "g_log");

   --  Keep the callback at library level: Log_Set_Handler stores its address
   --  until the handler is removed, so a nested callback would not be safe.
   procedure Capture_Log
     (Log_Domain : String;
      Log_Level  : Log_Level_Flags;
      Message    : Glib.UTF8_String);

   procedure Capture_Log
     (Log_Domain : String;
      Log_Level  : Log_Level_Flags;
      Message    : Glib.UTF8_String) is
   begin
      Calls := Calls + 1;
      Seen_Length := Log_Domain'Length;
      Seen_Domain (1 .. Seen_Length) := Log_Domain;
      Message_Size := Message'Length;
      Seen_Message (1 .. Message_Size) := Message;
      Seen_Level := Log_Level;
   end Capture_Log;

   procedure Test_Custom_Handler
   with Convention => C;

   procedure Test_Custom_Handler is
      Handler : Log_Handler_Id;
   begin
      --  A registered message handler receives the original domain, level,
      --  and text; removing the returned id unregisters that callback.
      Calls := 0;
      Handler := Log_Set_Handler
        ("gtkada-messages-test", Log_Level_Message,
         Capture_Log'Unrestricted_Access);
      Assert_Cmpuint_Gt (Glib.Guint (Handler), 0);

      Log ("gtkada-messages-test", Log_Level_Message, "captured text");
      Assert_Cmpuint_Eq (Glib.Guint (Calls), 1);
      Assert_True
        (Seen_Domain (1 .. Seen_Length) = "gtkada-messages-test");
      Assert_Cmpuint_Eq (Glib.Guint (Seen_Level),
                         Glib.Guint (Log_Level_Message));
      Assert_True (Seen_Message (1 .. Message_Size) = "captured text");

      Log_Remove_Handler ("gtkada-messages-test", Handler);
   end Test_Custom_Handler;

   procedure Test_Empty_Domain
   with Convention => C;

   procedure Test_Empty_Domain is
      Handler        : Log_Handler_Id;
      Native_Message : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("null domain");
   begin
      --  An empty Ada domain reaches the callback as an empty string.
      Calls := 0;
      Handler := Log_Set_Handler
        ("", Log_Level_Message, Capture_Log'Unrestricted_Access);
      Log ("", Log_Level_Message, "empty domain");
      Assert_Cmpuint_Eq (Glib.Guint (Calls), 1);
      Assert_Cmpuint_Eq (Glib.Guint (Seen_Length), 0);
      Assert_True (Seen_Message (1 .. Message_Size) = "empty domain");

      --  NULL is the same GLib domain, and exercises the wrapper's NULL case.
      Native_Log
        (Interfaces.C.Strings.Null_Ptr, Log_Level_Message, Native_Message);
      Assert_Cmpuint_Eq (Glib.Guint (Calls), 2);
      Assert_Cmpuint_Eq (Glib.Guint (Seen_Length), 0);
      Assert_True (Seen_Message (1 .. Message_Size) = "null domain");
      Interfaces.C.Strings.Free (Native_Message);
      Log_Remove_Handler ("", Handler);
   end Test_Empty_Domain;

   procedure Test_Default_And_Fatal_Handlers
   with Convention => C;

   procedure Test_Default_And_Fatal_Handlers is
      Handler      : Log_Handler_Id;
      Original_Mask : Log_Level_Flags;
   begin
      --  Calling GLib's default handler writes the supplied message directly
      --  instead of redispatching it through the registered custom handler.
      Calls := 0;
      Handler := Log_Set_Handler
        ("gtkada-messages-test", Log_Level_Info,
         Capture_Log'Unrestricted_Access);
      Log_Default_Handler
        ("gtkada-messages-test", Log_Level_Info, "default text");
      Assert_Cmpuint_Eq (Glib.Guint (Calls), 0);
      Log_Remove_Handler ("gtkada-messages-test", Handler);

      --  Set a domain-specific mask, verify GLib returns the previous mask,
      --  and restore it so subsequent tests retain their original behavior.
      Original_Mask := Log_Set_Fatal_Mask
        ("gtkada-messages-test", Log_Level_Critical);
      Assert_Cmpuint_Eq
        (Glib.Guint
           (Log_Set_Fatal_Mask
              ("gtkada-messages-test", Original_Mask)),
         Glib.Guint (Log_Level_Critical or Log_Level_Error));
   end Test_Default_And_Fatal_Handlers;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/glib-messages/custom-handler", Test_Custom_Handler'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-messages/empty-domain", Test_Empty_Domain'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-messages/default-and-fatal", Test_Default_And_Fatal_Handlers'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
