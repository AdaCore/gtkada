--  Signal callbacks used to verify both Gtk.IM_Context callback signatures.
--  The counters and captured arguments let the main test assert that GTK
--  delivered the expected values through each generated marshaller.

with Gtkada.Types; use Gtkada.Types;

package body Imcontext_Handlers is
   --  Save bounded callback text without allocating memory in a signal hook.
   procedure Save (Target : out String; Length : out Natural; Text : UTF8_String) is
   begin
      Length := Natural'Min (Target'Length, Text'Length);
      Target (Target'First .. Target'First + Length - 1) := Text (Text'First .. Text'First + Length - 1);
   end Save;

   --  Commit callbacks capture the exact UTF-8 payload from each overload.
   procedure Context_Commit
     (Self : access Gtk.IM_Context.Gtk_IM_Context_Record'Class;
      Text : UTF8_String) is
      pragma Unreferenced (Self);
   begin
      Context_Void_Calls := Context_Void_Calls + 1;
      Save (Context_String, Context_String_Length, Text);
   end Context_Commit;

   procedure Slot_Commit
     (Self : access Glib.Object.GObject_Record'Class;
      Text : UTF8_String) is
      pragma Unreferenced (Self);
   begin
      Slot_Void_Calls := Slot_Void_Calls + 1;
      Save (Slot_String, Slot_String_Length, Text);
   end Slot_Commit;

   --  Return False first and True second to test boolean signal accumulation.
   function Context_Delete
     (Self : access Gtk.IM_Context.Gtk_IM_Context_Record'Class;
      Offset, N_Chars : Glib.Gint) return Boolean is
      pragma Unreferenced (Self);
   begin
      Last_Offset := Offset;
      Last_Count := N_Chars;
      return False;
   end Context_Delete;

   function Slot_Delete
     (Self : access Glib.Object.GObject_Record'Class;
      Offset, N_Chars : Glib.Gint) return Boolean is
      pragma Unreferenced (Self);
   begin
      Last_Offset := Offset;
      Last_Count := N_Chars;
      return True;
   end Slot_Delete;

   --  These callbacks are connected even though normal tests cannot emit this
   --  GTK 4.22 signal without a real invalid key composition event.
   function Context_Invalid
     (Self : access Gtk.IM_Context.Gtk_IM_Context_Record'Class;
      Text : UTF8_String) return Boolean is
      pragma Unreferenced (Self);
   begin
      Context_Void_Calls := Context_Void_Calls + 1;
      Save (Context_String, Context_String_Length, Text);
      return False;
   end Context_Invalid;

   function Slot_Invalid
     (Self : access Glib.Object.GObject_Record'Class;
      Text : UTF8_String) return Boolean is
      pragma Unreferenced (Self);
   begin
      Slot_Void_Calls := Slot_Void_Calls + 1;
      Save (Slot_String, Slot_String_Length, Text);
      return True;
   end Slot_Invalid;

   --  Void callbacks make it easy to count each preedit signal delivery.
   procedure Context_Void
     (Self : access Gtk.IM_Context.Gtk_IM_Context_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Context_Void_Calls := Context_Void_Calls + 1;
   end Context_Void;

   procedure Slot_Void
     (Self : access Glib.Object.GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Slot_Void_Calls := Slot_Void_Calls + 1;
   end Slot_Void;

   --  Publish a range in response to the retrieve-surrounding signal. The
   --  slot callback accepts the request after the context callback declines.
   function Context_Retrieve
     (Self : access Gtk.IM_Context.Gtk_IM_Context_Record'Class)
      return Boolean is
   begin
      Self.Set_Surrounding_With_Selection ("abcd", 4, 2, 3);
      return False;
   end Context_Retrieve;

   function Slot_Retrieve
     (Self : access Glib.Object.GObject_Record'Class)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      Current_Context.Set_Surrounding_With_Selection
        ("abcd", 4, 2, Retrieve_Anchor);
      return True;
   end Slot_Retrieve;
end Imcontext_Handlers;
