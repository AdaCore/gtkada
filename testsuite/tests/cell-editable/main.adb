--  Exercises Gtk.Cell_Editable's methods and both overloads of its signals.
--  No matching gtk_cell_editable C test exists to port.

pragma Warnings (Off, "*obsolescent*");

with Ada.Command_Line;
with Ada.Exceptions;
with System;

with Gdk.Event;
with Glib; use Glib;
with Glib.Object;
with Glib.Test; use Glib.Test;
with Gtk.Cell_Editable;
with Gtk.GEntry;
with Gtk.Label;
with Gtk.Main;
with Gtkada.Bindings;

procedure Main is
   use type Gtk.Cell_Editable.Gtk_Cell_Editable;
   use type System.Address;

   Editable : Gtk.Cell_Editable.Gtk_Cell_Editable :=
     Gtk.Cell_Editable.Null_Gtk_Cell_Editable;
   Slot : Gtk.Label.Gtk_Label;
   Slot_Address : System.Address := System.Null_Address;

   Editing_Done_Typed_Calls : Natural := 0;
   Editing_Done_Slot_Calls : Natural := 0;
   Remove_Widget_Typed_Calls : Natural := 0;
   Remove_Widget_Slot_Calls : Natural := 0;
   Exception_Calls : Natural := 0;
   Raise_In_Callbacks : Boolean := False;

   procedure Test_Interface_Methods_And_Signals with Convention => C;
   procedure On_Editing_Done_Typed
     (Self : Gtk.Cell_Editable.Gtk_Cell_Editable);
   procedure On_Editing_Done_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure On_Remove_Widget_Typed
     (Self : Gtk.Cell_Editable.Gtk_Cell_Editable);
   procedure On_Remove_Widget_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   --  Typed callbacks receive the emitting interface; slot callbacks receive
   --  the separate label passed when the signal connection was made.
   procedure On_Editing_Done_Typed
     (Self : Gtk.Cell_Editable.Gtk_Cell_Editable) is
   begin
      Assert_True (Self = Editable);
      Editing_Done_Typed_Calls := Editing_Done_Typed_Calls + 1;
      if Raise_In_Callbacks then
         raise Program_Error with "expected editing-done callback exception";
      end if;
   end On_Editing_Done_Typed;

   procedure On_Editing_Done_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) = Slot_Address);
      Editing_Done_Slot_Calls := Editing_Done_Slot_Calls + 1;
      if Raise_In_Callbacks then
         raise Program_Error with "expected editing-done slot exception";
      end if;
   end On_Editing_Done_Slot;

   procedure On_Remove_Widget_Typed
     (Self : Gtk.Cell_Editable.Gtk_Cell_Editable) is
   begin
      Assert_True (Self = Editable);
      Remove_Widget_Typed_Calls := Remove_Widget_Typed_Calls + 1;
      if Raise_In_Callbacks then
         raise Program_Error with "expected remove-widget callback exception";
      end if;
   end On_Remove_Widget_Typed;

   procedure On_Remove_Widget_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) = Slot_Address);
      Remove_Widget_Slot_Calls := Remove_Widget_Slot_Calls + 1;
      if Raise_In_Callbacks then
         raise Program_Error with "expected remove-widget slot exception";
      end if;
   end On_Remove_Widget_Slot;

   --  GtkAda catches callback exceptions before returning through GTK's C
   --  signal emitter and reports each one through this hook.
   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Exception_Calls := Exception_Calls + 1;
   end Record_Callback_Exception;

   --  A null event is the documented way to begin editing programmatically.
   --  The signal wrappers are then checked by observing the actual callbacks.
   procedure Test_Interface_Methods_And_Signals is
      Event : Gdk.Event.Gdk_Event;
   begin
      --  Gtk.Entry is a concrete GtkCellEditable implementation, so no fake
      --  GObject or hand-built interface implementation is needed. Create
      --  widgets after Gtk.Main.Init, as required by GTK.
      declare
         Editable_Widget : constant Gtk.GEntry.Gtk_Entry :=
           Gtk.GEntry.Gtk_Entry_New;
      begin
         Editable := Gtk.GEntry."+" (Editable_Widget);
         Slot := Gtk.Label.Gtk_Label_New;
         Slot_Address := Glib.Object.Get_Object (Slot);
      end;

      Assert_True (Gtk.Cell_Editable."+" (Editable) = Editable);

      --  Use both connection overloads on each signal. The slot overloads
      --  request after-handlers so that the optional ordering argument is
      --  forwarded along with the connected slot object.
      Gtk.Cell_Editable.On_Editing_Done
        (Editable, On_Editing_Done_Typed'Unrestricted_Access);
      Gtk.Cell_Editable.On_Editing_Done
        (Editable, On_Editing_Done_Slot'Unrestricted_Access,
         Slot => Slot, After => True);
      Gtk.Cell_Editable.On_Remove_Widget
        (Editable, On_Remove_Widget_Typed'Unrestricted_Access);
      Gtk.Cell_Editable.On_Remove_Widget
        (Editable, On_Remove_Widget_Slot'Unrestricted_Access,
         Slot => Slot, After => True);

      Gtk.Cell_Editable.Start_Editing (Editable, Event);
      Gtk.Cell_Editable.Editing_Done (Editable);
      Assert_Cmpuint_Eq (Guint (Editing_Done_Typed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Editing_Done_Slot_Calls), 1);

      Gtk.Cell_Editable.Remove_Widget (Editable);
      Assert_Cmpuint_Eq (Guint (Remove_Widget_Typed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Remove_Widget_Slot_Calls), 1);

      --  Raising in each handler reaches the exception arm of both generated
      --  marshallers without allowing an Ada exception across the C boundary.
      Gtkada.Bindings.Set_On_Exception
        (Record_Callback_Exception'Unrestricted_Access);
      Raise_In_Callbacks := True;
      Gtk.Cell_Editable.Editing_Done (Editable);
      Gtk.Cell_Editable.Remove_Widget (Editable);
      Assert_Cmpuint_Eq (Guint (Exception_Calls), 4);
      Assert_Cmpuint_Eq (Guint (Editing_Done_Typed_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Editing_Done_Slot_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Remove_Widget_Typed_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Remove_Widget_Slot_Calls), 2);
   end Test_Interface_Methods_And_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/cell-editable/methods-signals",
      Test_Interface_Methods_And_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
