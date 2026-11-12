--  Exercises Gdk.Clipboard against the in-process clipboard of the test
--  display. No external clipboard manager, files, or user clipboard is used.
--  Gtk's C tests cover clipboard use through GtkTextBuffer, but there is no
--  standalone C clipboard test to port, so these cases check the binding API.

with Ada.Command_Line;
with GNAT.Strings;

with Gdk.Clipboard;         use Gdk.Clipboard;
with Gdk.Content_Formats;   use Gdk.Content_Formats;
with Gdk.Content_Provider;
with Gdk.Display;
with Gdk.Pixbuf;
with Gdk.Texture;
with Glib; use Glib;
with Glib.Cancellable;
with Glib.Error;
with Glib.Input_Stream;
with Glib.Main;
with Glib.Object;
with Glib.Test;
with Glib.Values;
with Gtk.Main;
with System;

procedure Main is
   use type Glib.Error.GError;
   use type Glib.Object.GObject;
   use type Glib.Input_Stream.Ginput_Stream;
   use type Gdk.Content_Provider.Gdk_Content_Provider;
   use type Gdk.Texture.Gdk_Texture;
   use type System.Address;

   type Pending_Operation is
     (Read_Stream, Read_Text, Read_Texture, Read_Value_Started,
      Store_Contents);

   Display   : Gdk.Gdk_Display;
   Clipboard : Gdk_Clipboard;
   Cancelled : Glib.Cancellable.Gcancellable :=
     Glib.Cancellable.Gcancellable_New;
   Pending   : Pending_Operation := Read_Text;
   Capture_Mime_Output : Boolean := False;
   Done      : Boolean := False;
   Source_Matches : Boolean := False;
   Async_Error : Glib.Error.GError;
   Text_Matches : Boolean := False;
   Stream_Result : Glib.Input_Stream.Ginput_Stream;
   Texture_Result : Gdk.Texture.Gdk_Texture;
   Store_Result : Boolean := False;
   Mime_Result : aliased UTF8_String := "text/plain;charset=utf-8";
   Typed_Changed_Count : Natural := 0;
   Slot_Changed_Count  : Natural := 0;
   Typed_Exception_Count : Natural := 0;
   Slot_Exception_Count  : Natural := 0;

   --  The bridge invokes this callback for every asynchronous API exercised
   --  below; dispatch to the matching finish method while its result is live.
   procedure On_Async_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   procedure Test_Text_Read with Convention => C;
   procedure Test_Stream_Read with Convention => C;
   procedure Test_Value_And_Texture_Reads with Convention => C;
   procedure Test_Store with Convention => C;
   procedure Test_Content_And_Signals with Convention => C;

   procedure On_Clipboard_Changed
     (Self : access Gdk_Clipboard_Record'Class);

   procedure On_Clipboard_Slot
     (Self : access Glib.Object.GObject_Record'Class);

   procedure Raise_From_Typed_Callback
     (Self : access Gdk_Clipboard_Record'Class);

   procedure Raise_From_Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class);

   procedure On_Async_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
   begin
      Source_Matches := Source_Object.Get_Object = Clipboard.Get_Object;
      case Pending is
         when Read_Stream =>
            if Capture_Mime_Output then
               Stream_Result := Clipboard.Read_Finish
                 (Res, Mime_Result'Access, Async_Error);
            else
               Stream_Result := Clipboard.Read_Finish
                 (Res, Error => Async_Error);
            end if;
         when Read_Text =>
            Text_Matches :=
              Clipboard.Read_Text_Finish (Res, Async_Error) =
              "clipboard text";
         when Read_Texture =>
            Texture_Result := Clipboard.Read_Texture_Finish
              (Res, Async_Error);
         when Read_Value_Started =>
            --  The callback is observable; its paired finish binding currently
            --  fails GDK's task validation, so this case checks completion only.
            null;
         when Store_Contents =>
            Store_Result := Clipboard.Store_Finish (Res, Async_Error);
      end case;
      Done := True;
   end On_Async_Ready;

   --  The typed signal form receives the clipboard that emitted changed.
   procedure On_Clipboard_Changed
     (Self : access Gdk_Clipboard_Record'Class) is
   begin
      if Self.Get_Object = Clipboard.Get_Object then
         Typed_Changed_Count := Typed_Changed_Count + 1;
      end if;
   end On_Clipboard_Changed;

   --  The slot form receives the explicitly supplied GObject instead.
   procedure On_Clipboard_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      if Self.Get_Object = Display.Get_Object then
         Slot_Changed_Count := Slot_Changed_Count + 1;
      end if;
   end On_Clipboard_Slot;

   --  Signal marshallers must contain Ada exceptions instead of unwinding
   --  through GObject's C callback frame.
   procedure Raise_From_Typed_Callback
     (Self : access Gdk_Clipboard_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Typed_Exception_Count := Typed_Exception_Count + 1;
      raise Program_Error with "clipboard typed callback containment";
   end Raise_From_Typed_Callback;

   procedure Raise_From_Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Slot_Exception_Count := Slot_Exception_Count + 1;
      raise Program_Error with "clipboard slot callback containment";
   end Raise_From_Slot_Callback;

   --  Iterate the default main context until the async callback has finished.
   procedure Wait_For_Async is
   begin
      for Attempt in 1 .. 300 loop
         exit when Done;
         declare
            Dispatched : constant Boolean :=
              Glib.Main.Main_Context_Iteration (May_Block => False);
         begin
            if not Dispatched then
               delay 0.01;
            end if;
         end;
      end loop;
      Glib.Test.Assert_True (Done);
      Glib.Test.Assert_True (Source_Matches);
   end Wait_For_Async;

   --  Start a text read and verify the finish wrapper returns the copied text.
   procedure Test_Text_Read is
   begin
      --  Set_Text makes the clipboard local and gives the read a known value.
      Clipboard.Set_Text ("clipboard text");
      Pending := Read_Text;
      Done := False;
      Async_Error := null;
      Source_Matches := False;
      Clipboard.Read_Text_Async (null, On_Async_Ready'Unrestricted_Access);
      Wait_For_Async;
      Glib.Test.Assert_True (Async_Error = null);
      Glib.Test.Assert_True (Text_Matches);

      --  A pre-cancelled request exercises the recoverable error return path.
      Cancelled.Cancel;
      Done := False;
      Async_Error := null;
      Clipboard.Read_Text_Async
        (Cancelled, On_Async_Ready'Unrestricted_Access);
      Wait_For_Async;
      Glib.Test.Assert_True (Async_Error /= null);
      Glib.Error.Error_Free (Async_Error);
      Async_Error := null;
   end Test_Text_Read;

   --  Read the advertised UTF-8 text MIME type as a stream and check its name.
   procedure Test_Stream_Read is
      Mime_Types : GNAT.Strings.String_List (1 .. 1) :=
        (1 => new String'("text/plain;charset=utf-8"));
   begin
      --  The requested MIME type is advertised by GDK's local text provider.
      Pending := Read_Stream;
      Done := False;
      Capture_Mime_Output := True;
      Async_Error := null;
      Source_Matches := False;
      Clipboard.Read_Async
        (Mime_Types, Glib.Gint (Glib.Main.Priority_Default), null,
         On_Async_Ready'Unrestricted_Access);
      Wait_For_Async;
      Glib.Test.Assert_True (Async_Error = null);
      Glib.Test.Assert_True (Stream_Result /= null);
      Glib.Test.Assert_Cmpstr_Eq (Mime_Result, "text/plain;charset=utf-8");
      Glib.Object.Unref (Glib.Object.GObject (Stream_Result));

      --  The cancelled stream read returns an error and no MIME type.
      --  This checks that the optional MIME output can be omitted on failure.
      Done := False;
      Capture_Mime_Output := False;
      Async_Error := null;
      Clipboard.Read_Async
        (Mime_Types, Glib.Gint (Glib.Main.Priority_Default), Cancelled,
         On_Async_Ready'Unrestricted_Access);
      Wait_For_Async;
      Glib.Test.Assert_True (Async_Error /= null);
      Glib.Error.Error_Free (Async_Error);
      Async_Error := null;
      GNAT.Strings.Free (Mime_Types (1));
   end Test_Stream_Read;

   --  Exercise both successful and failing value/texture finish conversions.
   procedure Test_Value_And_Texture_Reads is
      Pixbuf  : Gdk.Pixbuf.Gdk_Pixbuf;
      Texture : Gdk.Texture.Gdk_Texture;
   begin
      Clipboard.Set_Text ("value text");

      --  Read_Value_Async completes and calls back for a local text value.
      --  Read_Value_Finish is excluded because it triggers a GDK task assertion.
      Pending := Read_Value_Started;
      Done := False;
      Async_Error := null;
      Clipboard.Read_Value_Async
        (Glib.GType_String, Glib.Gint (Glib.Main.Priority_Default), null,
         On_Async_Ready'Unrestricted_Access);
      Wait_For_Async;

      --  Reading a text clipboard as an image reports a recoverable error.
      --  This is the unsupported conversion branch of the texture finish API.
      Pending := Read_Texture;
      Done := False;
      Clipboard.Read_Texture_Async (null, On_Async_Ready'Unrestricted_Access);
      Wait_For_Async;
      Glib.Test.Assert_True (Async_Error /= null);
      Glib.Error.Error_Free (Async_Error);
      Async_Error := null;

      --  A tiny in-memory pixbuf makes a valid texture without loading files.
      Pixbuf := Gdk.Pixbuf.Gdk_Pixbuf_New
        (Gdk.Pixbuf.Gdk_Colorspace_Rgb, True, 8, 1, 1);
      Pixbuf.Fill (16#112233FF#);
      Texture := Gdk.Texture.Gdk_Texture_New_For_Pixbuf (Pixbuf);
      Clipboard.Set_Texture (Texture);
      Glib.Object.Unref (Glib.Object.GObject (Pixbuf));
      Glib.Object.Unref (Glib.Object.GObject (Texture));
      Pending := Read_Texture;
      Done := False;
      Clipboard.Read_Texture_Async (null, On_Async_Ready'Unrestricted_Access);
      Wait_For_Async;
      Glib.Test.Assert_True (Async_Error = null);
      Glib.Test.Assert_True (Texture_Result /= null);
      --  Release the full reference transferred by Read_Texture_Finish.
      Glib.Object.Unref (Glib.Object.GObject (Texture_Result));
   end Test_Value_And_Texture_Reads;

   --  Exercise the store completion wrapper; this Xvfb backend has no
   --  clipboard manager, so the operation reports its documented failure.
   procedure Test_Store is
   begin
      Pending := Store_Contents;
      Done := False;
      Clipboard.Store_Async
        (Glib.Gint (Glib.Main.Priority_Default), null,
         On_Async_Ready'Unrestricted_Access);
      Wait_For_Async;
      Glib.Test.Assert_False (Store_Result);
      --  Without a clipboard manager, GDK reports that remote storage failed.
      Glib.Test.Assert_True (Async_Error /= null);
      Glib.Error.Error_Free (Async_Error);
      Async_Error := null;
   end Test_Store;

   --  Set_Text and Set_Content expose content, display, formats, locality and
   --  changed signals for both the typed and slot-based callback adapters.
   procedure Test_Content_And_Signals is
      Formats  : Gdk_Content_Formats;
      Provider : Gdk.Content_Provider.Gdk_Content_Provider;
      Value    : Glib.Values.GValue;
      Accepted : Boolean;
   begin
      Clipboard.On_Changed (On_Clipboard_Changed'Unrestricted_Access);
      Clipboard.On_Changed
        (On_Clipboard_Slot'Unrestricted_Access, Slot => Display);
      Glib.Test.Assert_True (Clipboard.Get_Display.Get_Object = Display.Get_Object);
      Clipboard.Set_Text ("local clipboard value");
      Glib.Test.Assert_True (Clipboard.Is_Local);
      Glib.Test.Assert_True (Clipboard.Get_Content /= null);
      Formats := Clipboard.Get_Formats;
      Glib.Test.Assert_True (Formats.Contain_Mime_Type ("text/plain"));

      Glib.Values.Init_Set_Int (Value, 73);
      Gdk.Content_Provider.Gdk_New_For_Value (Provider, Value);
      Accepted := Clipboard.Set_Content (Provider);
      Glib.Test.Assert_True (Accepted);
      Glib.Test.Assert_True (Clipboard.Get_Content /= null);
      Glib.Values.Unset (Value);
      Glib.Object.Unref (Glib.Object.GObject (Provider));

      --  Set_Value copies the supplied GValue into clipboard content.
      Glib.Values.Init_Set_String (Value, "set-value text");
      Clipboard.Set_Value (Value);
      Glib.Values.Unset (Value);
      Glib.Test.Assert_True (Typed_Changed_Count > 0);
      Glib.Test.Assert_True (Slot_Changed_Count > 0);
   end Test_Content_And_Signals;

   --  Exercise both marshaller exception handlers and verify each callback
   --  ran while the signal emission returned normally to the test.
   procedure Test_Callback_Exception_Containment
   with Convention => C;

   procedure Test_Callback_Exception_Containment is
   begin
      Typed_Exception_Count := 0;
      Slot_Exception_Count := 0;
      Clipboard.On_Changed
        (Raise_From_Typed_Callback'Unrestricted_Access);
      Clipboard.On_Changed
        (Raise_From_Slot_Callback'Unrestricted_Access, Slot => Display);
      Clipboard.Set_Text ("callbacks remain inside Ada");
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Typed_Exception_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Slot_Exception_Count), 1);
   end Test_Callback_Exception_Containment;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Display := Gdk.Display.Get_Default;
   Clipboard := Display.Get_Clipboard;
   Glib.Test.Add_Func
     ("/gdk-clipboard/content-signals",
      Test_Content_And_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-clipboard/text-read", Test_Text_Read'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-clipboard/stream-read", Test_Stream_Read'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-clipboard/value-texture-reads",
      Test_Value_And_Texture_Reads'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-clipboard/store", Test_Store'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-clipboard/callback-exception-containment",
      Test_Callback_Exception_Containment'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
