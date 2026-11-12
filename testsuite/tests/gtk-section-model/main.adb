--  Exercise Gtk.Section_Model's typed and slot sections-changed callbacks.
--  The C singleselection tests query sections and emit section notifications;
--  this test covers the GtkAda signal wrapper overloads and their ABI adapters.

with Ada.Command_Line;
with Ada.Exceptions;
with GNAT.Strings;
with System;

with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with Glib.Test;               use Glib.Test;
with Gtk.Main;
with Gtk.Section_Model;       use Gtk.Section_Model;
with Gtk.Single_Selection;    use Gtk.Single_Selection;
with Gtk.String_List;         use Gtk.String_List;
with Gtkada.Bindings;

procedure Main is

   use type Gtk_Section_Model;
   use type System.Address;

   Typed_Position : Guint := Guint'Last;
   Typed_Count    : Guint := Guint'Last;
   Typed_Calls    : Natural := 0;
   Slot_Position  : Guint := Guint'Last;
   Slot_Count     : Guint := Guint'Last;
   Slot_Calls     : Natural := 0;
   Slot_Matches   : Boolean := False;
   Exception_Count : Natural := 0;
   Expected_Slot : Gtk_String_List;

   procedure Test_Section_Signals with Convention => C;

   --  The typed callback records the range emitted by Gtk.Section_Model.
   procedure On_Sections_Changed
     (Self : Gtk_Section_Model; Position, N_Items : Guint);

   --  The slot callback receives the supplied GObject as Self.
   procedure On_Sections_Changed_Slot
     (Self : access GObject_Record'Class; Position, N_Items : Guint);

   --  Raising handlers exercise the exception boundary around each marshaller.
   procedure Raise_From_Typed_Callback
     (Self : Gtk_Section_Model; Position, N_Items : Guint);
   procedure Raise_From_Slot_Callback
     (Self : access GObject_Record'Class; Position, N_Items : Guint);

   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   -----------------------------
   -- Test_Section_Signals --
   -----------------------------

   procedure Test_Section_Signals is
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Store : constant Gtk_String_List := Gtk_String_List_New (Empty);
      Selection : Gtk_Single_Selection;
      Sections : Gtk_Section_Model;
   begin
      --  GTK validates emitted ranges against the section model's item count.
      for Index in 1 .. 10 loop
         Append (Store, "item");
      end loop;

      --  Keep the creator's reference while the selection retains its model.
      Ref (Store);
      Selection := Gtk_Single_Selection_New (+Store);
      Expected_Slot := Gtk_String_List_New (Empty);
      Sections := +Selection;

      --  The GtkAda interface conversion operator preserves the same model.
      Assert_True (Gtk.Section_Model."+" (Sections) = Sections);

      --  Connect both overloads, then check their signal arguments and slot.
      On_Sections_Changed
        (Sections, On_Sections_Changed'Unrestricted_Access, After => True);
      On_Sections_Changed
        (Sections, On_Sections_Changed_Slot'Unrestricted_Access,
         Slot => Expected_Slot, After => True);
      Sections_Changed (Sections, 3, 5);
      Assert_Cmpuint_Eq (Typed_Position, 3);
      Assert_Cmpuint_Eq (Typed_Count, 5);
      Assert_Cmpuint_Eq (Guint (Typed_Calls), 1);
      Assert_Cmpuint_Eq (Slot_Position, 3);
      Assert_Cmpuint_Eq (Slot_Count, 5);
      Assert_Cmpuint_Eq (Guint (Slot_Calls), 1);
      Assert_True (Slot_Matches);

      --  Route both deliberately raised callbacks through GtkAda's exception
      --  hook; the callback boundary must keep them from unwinding into C.
      Gtkada.Bindings.Set_On_Exception
        (Record_Callback_Exception'Unrestricted_Access);
      On_Sections_Changed
        (Sections, Raise_From_Typed_Callback'Unrestricted_Access);
      On_Sections_Changed
        (Sections, Raise_From_Slot_Callback'Unrestricted_Access,
         Slot => Expected_Slot);
      Sections_Changed (Sections, 7, 2);
      Assert_Cmpuint_Eq (Guint (Exception_Count), 2);

      --  Drop the signal source before releasing the GObject used as its slot.
      Unref (Selection);
      Unref (Expected_Slot);
      Unref (Store);
   end Test_Section_Signals;

   -------------------------
   -- Signal callbacks --
   -------------------------

   procedure On_Sections_Changed
     (Self : Gtk_Section_Model; Position, N_Items : Guint)
   is
      pragma Unreferenced (Self);
   begin
      Typed_Position := Position;
      Typed_Count := N_Items;
      Typed_Calls := Typed_Calls + 1;
   end On_Sections_Changed;

   procedure On_Sections_Changed_Slot
     (Self : access GObject_Record'Class; Position, N_Items : Guint)
   is
   begin
      Slot_Position := Position;
      Slot_Count := N_Items;
      Slot_Calls := Slot_Calls + 1;
      Slot_Matches := Get_Object (Self) = Get_Object (Expected_Slot);
   end On_Sections_Changed_Slot;

   procedure Raise_From_Typed_Callback
     (Self : Gtk_Section_Model; Position, N_Items : Guint)
   is
      pragma Unreferenced (Self, Position, N_Items);
   begin
      raise Program_Error with "typed sections-changed callback";
   end Raise_From_Typed_Callback;

   procedure Raise_From_Slot_Callback
     (Self : access GObject_Record'Class; Position, N_Items : Guint)
   is
      pragma Unreferenced (Self, Position, N_Items);
   begin
      raise Program_Error with "slot sections-changed callback";
   end Raise_From_Slot_Callback;

   --  Count expected callback exceptions without writing diagnostics to stderr.
   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Occurrence);
   begin
      Exception_Count := Exception_Count + 1;
   end Record_Callback_Exception;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func
     ("/gtk-section-model/section-signals",
      Test_Section_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
