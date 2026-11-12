--  Exercises Gtk.Cell_Renderer's state, geometry, rendering, activation,
--  editing wrappers, accel, toggle, combo, progress, spin and spinner renderers.

pragma Warnings (Off, "*obsolescent*");

with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C;
with Interfaces.C.Strings;
with System;

with Gdk.Event;
with Gdk.Enums;
with Gdk.Rectangle;         use Gdk.Rectangle;
with Glib;                  use Glib;
with Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;
with Glib.Test;             use Glib.Test;
with Gtk.Cell_Editable;
with Gtk.Cell_Renderer;     use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Accel;
with Gtk.Cell_Renderer_Combo;
with Gtk.Cell_Renderer_Progress;
with Gtk.Cell_Renderer_Spin;
with Gtk.Cell_Renderer_Spinner;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Adjustment;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Label;
with Gtk.List_Store;
with Gtk.Main;
with Gtk.Orientable;
with Gtk.Snapshot;
with Gtk.Tree_Model;
with Gtk.Widget;
with Gtkada.Bindings;
with Gsk.Render_Node;

procedure Main is
   use type Interfaces.C.C_float;
   use type Gtk.Cell_Editable.Gtk_Cell_Editable;
   use type Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   use type Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle;
   use type Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo;
   use type Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel;
   use type Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
   use type Gtk.Cell_Renderer_Spin.Gtk_Cell_Renderer_Spin;
   use type Gtk.Cell_Renderer_Spinner.Gtk_Cell_Renderer_Spinner;
   use type Gdk.Enums.Gdk_Modifier_Type;
   use type Gtk.Tree_Model.Gtk_Tree_Iter;
   use type Glib.Types.GType_Interface;
   use type Gtk.Enums.Gtk_State_Flags;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   Current_Cell : Gtk_Cell_Renderer := null;
   Current_Slot_Address : System.Address := System.Null_Address;
   Current_Toggle : Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle := null;
   Current_Toggle_Slot_Address : System.Address := System.Null_Address;
   Current_Combo : Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo := null;
   Current_Combo_Iter : Gtk.Tree_Model.Gtk_Tree_Iter :=
     Gtk.Tree_Model.Null_Iter;
   Current_Combo_Slot_Address : System.Address := System.Null_Address;
   Raise_In_Callbacks : Boolean := False;
   Typed_Start_Calls : Natural := 0;
   Slot_Start_Calls : Natural := 0;
   Typed_Cancel_Calls : Natural := 0;
   Slot_Cancel_Calls : Natural := 0;
   Exception_Calls : Natural := 0;
   Toggle_Typed_Calls : Natural := 0;
   Toggle_Slot_Calls : Natural := 0;
   Toggle_Exception_Calls : Natural := 0;
   Toggle_Typed_Self_Matches : Boolean := False;
   Toggle_Slot_Self_Matches : Boolean := False;
   Toggle_Typed_Path_Matches : Boolean := False;
   Toggle_Slot_Path_Matches : Boolean := False;
   Combo_Typed_Calls : Natural := 0;
   Combo_Slot_Calls : Natural := 0;
   Combo_Exception_Calls : Natural := 0;
   Combo_Typed_Self_Matches : Boolean := False;
   Combo_Slot_Self_Matches : Boolean := False;
   Combo_Typed_Arguments_Match : Boolean := False;
   Combo_Slot_Arguments_Match : Boolean := False;
   Combo_Raising_Arguments_Match : Boolean := False;
   Accel_Typed_Cleared_Calls : Natural := 0;
   Accel_Slot_Cleared_Calls : Natural := 0;
   Accel_Typed_Edited_Calls : Natural := 0;
   Accel_Slot_Edited_Calls : Natural := 0;
   Accel_Exception_Calls : Natural := 0;
   Current_Accel : Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel := null;
   Current_Accel_Slot_Address : System.Address := System.Null_Address;
   Accel_Cleared_Arguments_Match : Boolean := True;
   Accel_Edited_Arguments_Match : Boolean := True;
   Accel_Raising_Arguments_Match : Boolean := True;
   Current_Text_Cell : Gtk_Cell_Renderer_Text := null;
   Current_Text_Slot_Address : System.Address := System.Null_Address;
   Text_Typed_Calls : Natural := 0;
   Text_Slot_Calls : Natural := 0;
   Text_Exception_Calls : Natural := 0;
   Text_Edited_Arguments_Match : Boolean := True;
   Text_Raising_Arguments_Match : Boolean := True;
   Raising_Typed_Path_Matches : Boolean := False;
   Raising_Slot_Path_Matches : Boolean := False;
   Typed_Start_Self_Matches : Boolean := False;
   Slot_Start_Self_Matches : Boolean := False;
   Start_Path_Matches : Boolean := False;
   Started_Editable : Gtk.Cell_Editable.Gtk_Cell_Editable :=
     Gtk.Cell_Editable.Null_Gtk_Cell_Editable;

   procedure Test_State_And_Size with Convention => C;
   procedure Test_Rendering_And_Activation with Convention => C;
   procedure Test_Editing_Signals with Convention => C;
   procedure Test_Toggle_State_And_Signals with Convention => C;
   procedure Test_Combo_Properties_And_Signals with Convention => C;
   procedure Test_Progress_Properties_And_Orientation with Convention => C;
   procedure Test_Accel_Constructors_And_Signals with Convention => C;
   procedure Test_Text_Constructors_And_Fixed_Height with Convention => C;
   procedure Test_Text_Edited_Signals with Convention => C;
   procedure Test_Spin_Constructors_And_Properties with Convention => C;
   procedure Test_Spinner_Constructors_And_Properties with Convention => C;

   --  These emitters pass signal arguments through g_signal_emit_by_name's
   --  C varargs interface, avoiding a dependency on physical keyboard input.
   procedure Emit_Accel_Cleared
     (Object : System.Address;
      Name   : Interfaces.C.Strings.chars_ptr;
      Path   : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Accel_Edited
     (Object  : System.Address;
      Name    : Interfaces.C.Strings.chars_ptr;
      Path    : Interfaces.C.Strings.chars_ptr;
      Key     : Interfaces.C.unsigned;
      Mods    : Interfaces.C.unsigned;
      Keycode : Interfaces.C.unsigned)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  The text renderer's edited signal carries the row path and new text.
   procedure Emit_Text_Edited
     (Object   : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      Path     : Interfaces.C.Strings.chars_ptr;
      New_Text : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  These callbacks check the instance and both strings passed by GTK.
   procedure On_Text_Edited_Typed
     (Self : access Gtk_Cell_Renderer_Text_Record'Class;
      Path : UTF8_String;
      New_Text : UTF8_String);
   procedure On_Text_Edited_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String;
      New_Text : UTF8_String);

   --  Raising callbacks exercise each edited marshaller's exception handler.
   procedure On_Text_Edited_Raising_Typed
     (Self : access Gtk_Cell_Renderer_Text_Record'Class;
      Path : UTF8_String;
      New_Text : UTF8_String);
   procedure On_Text_Edited_Raising_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String;
      New_Text : UTF8_String);
   procedure Record_Text_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   --  The cleared signal has a renderer instance and only the row path.
   procedure On_Accel_Cleared_Typed
     (Self : access Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel_Record'Class;
      Path : UTF8_String);
   --  The overload with a slot intentionally substitutes that object's Self.
   procedure On_Accel_Cleared_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String);
   procedure On_Accel_Edited_Typed
     (Self : access Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel_Record'Class;
      Path : UTF8_String;
      Key  : Guint;
      Mods : Gdk.Enums.Gdk_Modifier_Type;
      Keycode : Guint);
   --  The slot version must receive the same four signal values as typed.
   procedure On_Accel_Edited_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String;
      Key  : Guint;
      Mods : Gdk.Enums.Gdk_Modifier_Type;
      Keycode : Guint);

   --  The typed callback receives the accelerator renderer itself.
   procedure On_Accel_Cleared_Typed
     (Self : access Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel_Record'Class;
      Path : UTF8_String) is
   begin
      Accel_Typed_Cleared_Calls := Accel_Typed_Cleared_Calls + 1;
      Accel_Cleared_Arguments_Match := Accel_Cleared_Arguments_Match
        and then Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Current_Accel)
        and then Path = "row/1";
   end On_Accel_Cleared_Typed;

   --  Slot handlers run with the separately connected GObject as Self.
   procedure On_Accel_Cleared_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String) is
   begin
      Accel_Slot_Cleared_Calls := Accel_Slot_Cleared_Calls + 1;
      Accel_Cleared_Arguments_Match := Accel_Cleared_Arguments_Match
        and then Glib.Object.Get_Object (Self) = Current_Accel_Slot_Address
        and then Path = "row/1";
   end On_Accel_Cleared_Slot;

   --  Record every edited argument so both typed and GObject marshallers are
   --  checked for their object, path, key value, modifier mask and keycode.
   procedure On_Accel_Edited_Typed
     (Self : access Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel_Record'Class;
      Path : UTF8_String;
      Key  : Guint;
      Mods : Gdk.Enums.Gdk_Modifier_Type;
      Keycode : Guint) is
   begin
      Accel_Typed_Edited_Calls := Accel_Typed_Edited_Calls + 1;
      Accel_Edited_Arguments_Match := Accel_Edited_Arguments_Match
        and then Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Current_Accel)
        and then Path = "row/1" and then Key = 16#61#
        and then Mods = Gdk.Enums.Gdk_Control_Mask and then Keycode = 38;
   end On_Accel_Edited_Typed;

   procedure On_Accel_Edited_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String;
      Key  : Guint;
      Mods : Gdk.Enums.Gdk_Modifier_Type;
      Keycode : Guint) is
   begin
      Accel_Slot_Edited_Calls := Accel_Slot_Edited_Calls + 1;
      Accel_Edited_Arguments_Match := Accel_Edited_Arguments_Match
        and then Glib.Object.Get_Object (Self) = Current_Accel_Slot_Address
        and then Path = "row/1" and then Key = 16#61#
        and then Mods = Gdk.Enums.Gdk_Control_Mask and then Keycode = 38;
   end On_Accel_Edited_Slot;

   --  Raising handlers exercise each accelerator marshaller's exception arm.
   --  Raising typed callbacks also validate the converted signal arguments.
   procedure On_Accel_Cleared_Raising_Typed
     (Self : access Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel_Record'Class;
      Path : UTF8_String);
   --  Raising slot callbacks verify the swapped instance before they fail.
   procedure On_Accel_Cleared_Raising_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String);
   --  Edited exceptions must be contained after all four values are converted.
   procedure On_Accel_Edited_Raising_Typed
     (Self : access Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel_Record'Class;
      Path : UTF8_String;
      Key  : Guint;
      Mods : Gdk.Enums.Gdk_Modifier_Type;
      Keycode : Guint);
   --  The GObject callback gets the slot even when its callback raises.
   procedure On_Accel_Edited_Raising_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String;
      Key  : Guint;
      Mods : Gdk.Enums.Gdk_Modifier_Type;
      Keycode : Guint);

   procedure On_Accel_Cleared_Raising_Typed
     (Self : access Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel_Record'Class;
      Path : UTF8_String) is
   begin
      Accel_Raising_Arguments_Match := Accel_Raising_Arguments_Match
        and then Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Current_Accel) and then Path = "row/1";
      raise Program_Error;
   end On_Accel_Cleared_Raising_Typed;

   procedure On_Accel_Cleared_Raising_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String) is
   begin
      Accel_Raising_Arguments_Match := Accel_Raising_Arguments_Match
        and then Glib.Object.Get_Object (Self) = Current_Accel_Slot_Address
        and then Path = "row/1";
      raise Program_Error;
   end On_Accel_Cleared_Raising_Slot;

   procedure On_Accel_Edited_Raising_Typed
     (Self : access Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel_Record'Class;
      Path : UTF8_String;
      Key  : Guint;
      Mods : Gdk.Enums.Gdk_Modifier_Type;
      Keycode : Guint) is
   begin
      Accel_Raising_Arguments_Match := Accel_Raising_Arguments_Match
        and then Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Current_Accel)
        and then Path = "row/1" and then Key = 16#61#
        and then Mods = Gdk.Enums.Gdk_Control_Mask and then Keycode = 38;
      raise Program_Error;
   end On_Accel_Edited_Raising_Typed;

   procedure On_Accel_Edited_Raising_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String;
      Key  : Guint;
      Mods : Gdk.Enums.Gdk_Modifier_Type;
      Keycode : Guint) is
   begin
      Accel_Raising_Arguments_Match := Accel_Raising_Arguments_Match
        and then Glib.Object.Get_Object (Self) = Current_Accel_Slot_Address
        and then Path = "row/1" and then Key = 16#61#
        and then Mods = Gdk.Enums.Gdk_Control_Mask and then Keycode = 38;
      raise Program_Error;
   end On_Accel_Edited_Raising_Slot;

   --  Count callback exceptions reported back by GtkAda's C signal bridge.
   procedure Record_Accel_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      Accel_Exception_Calls := Accel_Exception_Calls + 1;
   end Record_Accel_Exception;

   --  The typed callback checks the renderer instance and the editing path.
   procedure On_Started
     (Self     : access Gtk_Cell_Renderer_Record'Class;
      Editable : Gtk.Cell_Editable.Gtk_Cell_Editable;
      Path     : UTF8_String) is
   begin
      Typed_Start_Calls := Typed_Start_Calls + 1;
      Typed_Start_Self_Matches :=
        Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Current_Cell);
      Start_Path_Matches := Path = "0";
      Started_Editable := Editable;
      if Raise_In_Callbacks then
         raise Program_Error;
      end if;
   end On_Started;

   --  A swapped slot callback receives the connected slot as its instance.
   procedure On_Started_Slot
     (Self     : access Glib.Object.GObject_Record'Class;
      Editable : Gtk.Cell_Editable.Gtk_Cell_Editable;
      Path     : UTF8_String) is
   begin
      Slot_Start_Calls := Slot_Start_Calls + 1;
      Slot_Start_Self_Matches :=
        Glib.Object.Get_Object (Self) = Current_Slot_Address;
      Start_Path_Matches := Start_Path_Matches and Path = "0";
      if Editable = Gtk.Cell_Editable.Null_Gtk_Cell_Editable then
         Start_Path_Matches := False;
      end if;
      if Raise_In_Callbacks then
         raise Program_Error;
      end if;
   end On_Started_Slot;

   --  Cancellation is a void signal and has no extra signal parameters.
   procedure On_Canceled
     (Self : access Gtk_Cell_Renderer_Record'Class) is
   begin
      Typed_Cancel_Calls := Typed_Cancel_Calls + 1;
      if Gtk_Cell_Renderer (Self) /= Current_Cell then
         Typed_Start_Self_Matches := False;
      end if;
      if Raise_In_Callbacks then
         raise Program_Error;
      end if;
   end On_Canceled;

   --  The alternate cancellation callback uses GObject for its instance.
   procedure On_Canceled_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Slot_Cancel_Calls := Slot_Cancel_Calls + 1;
      if Glib.Object.Get_Object (Self) /= Current_Slot_Address
      then
         Slot_Start_Self_Matches := False;
      end if;
      if Raise_In_Callbacks then
         raise Program_Error;
      end if;
   end On_Canceled_Slot;

   --  GtkAda reports exceptions raised by signal callbacks through this hook.
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      Exception_Calls := Exception_Calls + 1;
   end Record_Exception;

   --  Typed callbacks receive the text renderer that emitted the signal.
   procedure On_Text_Edited_Typed
     (Self : access Gtk_Cell_Renderer_Text_Record'Class;
      Path : UTF8_String;
      New_Text : UTF8_String) is
   begin
      Text_Typed_Calls := Text_Typed_Calls + 1;
      Text_Edited_Arguments_Match := Text_Edited_Arguments_Match
        and then Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Current_Text_Cell)
        and then Path = "row/3" and then New_Text = "changed text";
   end On_Text_Edited_Typed;

   --  With a slot, GtkAda supplies the connected GObject as callback Self.
   procedure On_Text_Edited_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String;
      New_Text : UTF8_String) is
   begin
      Text_Slot_Calls := Text_Slot_Calls + 1;
      Text_Edited_Arguments_Match := Text_Edited_Arguments_Match
        and then Glib.Object.Get_Object (Self) = Current_Text_Slot_Address
        and then Path = "row/3" and then New_Text = "changed text";
   end On_Text_Edited_Slot;

   --  Check converted arguments before raising through the typed marshaller.
   procedure On_Text_Edited_Raising_Typed
     (Self : access Gtk_Cell_Renderer_Text_Record'Class;
      Path : UTF8_String;
      New_Text : UTF8_String) is
   begin
      Text_Raising_Arguments_Match := Text_Raising_Arguments_Match
        and then Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Current_Text_Cell)
        and then Path = "row/3" and then New_Text = "changed text";
      raise Program_Error;
   end On_Text_Edited_Raising_Typed;

   --  The swapped marshaller must preserve the slot and both string values.
   procedure On_Text_Edited_Raising_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String;
      New_Text : UTF8_String) is
   begin
      Text_Raising_Arguments_Match := Text_Raising_Arguments_Match
        and then Glib.Object.Get_Object (Self) = Current_Text_Slot_Address
        and then Path = "row/3" and then New_Text = "changed text";
      raise Program_Error;
   end On_Text_Edited_Raising_Slot;

   --  GtkAda reports callback failures instead of unwinding into the C signal.
   procedure Record_Text_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      Text_Exception_Calls := Text_Exception_Calls + 1;
   end Record_Text_Exception;

   --  g_signal_emit_by_name takes signal arguments as C varargs after its
   --  fixed object and signal-name arguments.
   procedure Emit_Toggled
     (Object : System.Address;
      Name   : Interfaces.C.Strings.chars_ptr;
      Path   : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  The combo renderer's changed signal takes a path string and an iterator.
   --  GtkAda's small C helper forwards both pointer-valued varargs unchanged.
   procedure Emit_Combo_Changed
     (Object : System.Address;
      Name   : Interfaces.C.Strings.chars_ptr;
      Path   : Interfaces.C.Strings.chars_ptr;
      Iter   : System.Address)
   with Import, Convention => C,
        External_Name => "ada_g_signal_emit_by_name_ptr_ptr";

   --  The callbacks verify both signal arguments and the connected instance.
   procedure On_Combo_Changed_Typed
     (Self : access Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo_Record'Class;
      Path : UTF8_String;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   procedure On_Combo_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   procedure On_Combo_Raising_Typed
     (Self : access Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo_Record'Class;
      Path : UTF8_String;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   procedure On_Combo_Raising_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   procedure Record_Combo_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   --  These regular handlers check that the signal bridge returns the renderer,
   --  its row path, and the iterator passed by the C signal emitter.
   procedure On_Combo_Changed_Typed
     (Self : access Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo_Record'Class;
      Path : UTF8_String;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      Combo_Typed_Calls := Combo_Typed_Calls + 1;
      Combo_Typed_Self_Matches :=
        Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Current_Combo);
      Combo_Typed_Arguments_Match :=
        Path = "0" and then Iter = Current_Combo_Iter;
   end On_Combo_Changed_Typed;

   --  The swapped callback receives its slot object as Self, as documented.
   procedure On_Combo_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      Combo_Slot_Calls := Combo_Slot_Calls + 1;
      Combo_Slot_Self_Matches :=
        Glib.Object.Get_Object (Self) = Current_Combo_Slot_Address;
      Combo_Slot_Arguments_Match :=
        Path = "0" and then Iter = Current_Combo_Iter;
   end On_Combo_Changed_Slot;

   --  Raising once through each marshaller checks its exception containment.
   procedure On_Combo_Raising_Typed
     (Self : access Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo_Record'Class;
      Path : UTF8_String;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      Combo_Raising_Arguments_Match :=
        Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Current_Combo)
        and then Path = "0" and then Iter = Current_Combo_Iter;
      raise Program_Error;
   end On_Combo_Raising_Typed;

   procedure On_Combo_Raising_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      Combo_Raising_Arguments_Match :=
        Combo_Raising_Arguments_Match
        and then Glib.Object.Get_Object (Self) = Current_Combo_Slot_Address
        and then Path = "0" and then Iter = Current_Combo_Iter;
      raise Program_Error;
   end On_Combo_Raising_Slot;

   --  GtkAda reports callback exceptions without letting them cross C frames.
   procedure Record_Combo_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      Combo_Exception_Calls := Combo_Exception_Calls + 1;
   end Record_Combo_Exception;

   --  These callbacks record values for assertions after GTK returns from the
   --  signal dispatch, avoiding Ada assertions inside a C callback.
   procedure On_Toggled_Typed
     (Self : access Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path : UTF8_String) is
   begin
      Toggle_Typed_Calls := Toggle_Typed_Calls + 1;
      Toggle_Typed_Self_Matches :=
        Glib.Object.Get_Object (Self) =
          Glib.Object.Get_Object (Current_Toggle);
      Toggle_Typed_Path_Matches := Path = "2:4";
   end On_Toggled_Typed;

   --  The GObject overload receives the connected slot as the callback object.
   procedure On_Toggled_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String) is
   begin
      Toggle_Slot_Calls := Toggle_Slot_Calls + 1;
      Toggle_Slot_Self_Matches :=
        Glib.Object.Get_Object (Self) = Current_Toggle_Slot_Address;
      Toggle_Slot_Path_Matches := Path = "2:4";
   end On_Toggled_Slot;

   --  Raising from each callback checks that GtkAda reports exceptions and
   --  keeps them from crossing the C signal dispatcher.
   procedure On_Toggled_Raising_Typed
     (Self : access Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path : UTF8_String) is
   begin
      Raising_Typed_Path_Matches :=
        Glib.Object.Get_Object (Self) = Glib.Object.Get_Object (Current_Toggle)
        and then Path = "2:4";
      raise Program_Error;
   end On_Toggled_Raising_Typed;

   procedure On_Toggled_Raising_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String) is
   begin
      Raising_Slot_Path_Matches :=
        Glib.Object.Get_Object (Self) = Current_Toggle_Slot_Address
        and then Path = "2:4";
      raise Program_Error;
   end On_Toggled_Raising_Slot;

   --  Count toggle signal failures separately from the editing signal checks.
   procedure Record_Toggle_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      Toggle_Exception_Calls := Toggle_Exception_Calls + 1;
   end Record_Toggle_Exception;

   --------------------------
   -- Test_State_And_Size --
   --------------------------

   -----------------------------------------------------
   -- Test_Text_Constructors_And_Fixed_Height --
   -----------------------------------------------------

   --  Check both constructors, idempotent Initialize, and the deprecated
   --  fixed-height setter while measuring the resulting renderer height.
   procedure Test_Text_Constructors_And_Fixed_Height is
      By_Function : constant Gtk_Cell_Renderer_Text :=
        Gtk_Cell_Renderer_Text_New;
      By_Out : Gtk_Cell_Renderer_Text;
      Widget : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New;
      Original_Object : System.Address;
      Min_Height : Gint;
      Natural_Height : Gint;
   begin
      By_Function.Ref_Sink;
      Gtk.Cell_Renderer_Text.Gtk_New (By_Out);
      By_Out.Ref_Sink;
      Widget.Ref_Sink;

      --  Calling Initialize twice leaves the same underlying GObject intact.
      Original_Object := Glib.Object.Get_Object (By_Out);
      Gtk.Cell_Renderer_Text.Initialize (By_Out);
      Assert_True (Original_Object /= System.Null_Address);
      Assert_True (Glib.Object.Get_Object (By_Out) = Original_Object);
      Assert_True (Gtk.Cell_Renderer_Text.Get_Type /= Glib.GType_Invalid);

      --  A positive row count fixes the reported height; -1 restores sizing.
      Set_Fixed_Height_From_Font (By_Out, 3);
      Gtk.Cell_Renderer.Get_Preferred_Height
        (Gtk_Cell_Renderer (By_Out), Widget, Min_Height, Natural_Height);
      Assert_True (Min_Height >= 0 and Natural_Height >= Min_Height);
      Assert_True (Natural_Height > 0);
      Set_Fixed_Height_From_Font (By_Out, -1);
      Gtk.Cell_Renderer.Get_Preferred_Height
        (Gtk_Cell_Renderer (By_Out), Widget, Min_Height, Natural_Height);
      Assert_True (Min_Height >= 0 and Natural_Height >= Min_Height);

      --  Release the objects created by this constructor and sizing check.
      Glib.Object.Unref (Glib.Object.GObject (By_Function));
      Glib.Object.Unref (Glib.Object.GObject (By_Out));
      Glib.Object.Unref (Glib.Object.GObject (Widget));
   end Test_Text_Constructors_And_Fixed_Height;

   -----------------------------------
   -- Test_Text_Edited_Signals --
   -----------------------------------

   --  Emit edited directly so both generated marshallers run without user
   --  input, then verify that callback exceptions are contained by GtkAda.
   procedure Test_Text_Edited_Signals is
      Cell : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Error_Cell : constant Gtk_Cell_Renderer_Text :=
        Gtk_Cell_Renderer_Text_New;
      Slot : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New;
      Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("edited");
      Path : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("row/3");
      New_Text : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("changed text");
   begin
      Cell.Ref_Sink;
      Error_Cell.Ref_Sink;
      Slot.Ref_Sink;
      Text_Typed_Calls := 0;
      Text_Slot_Calls := 0;
      Text_Exception_Calls := 0;
      Text_Edited_Arguments_Match := True;
      Text_Raising_Arguments_Match := True;
      Current_Text_Cell := Cell;
      Current_Text_Slot_Address := Glib.Object.Get_Object (Slot);

      --  The two overloads pass either the renderer or the connected slot.
      On_Edited (Cell, On_Text_Edited_Typed'Unrestricted_Access);
      On_Edited
        (Cell, On_Text_Edited_Slot'Unrestricted_Access, Slot => Slot);
      Emit_Text_Edited
        (Glib.Object.Get_Object (Cell), Name, Path, New_Text);
      Assert_Cmpuint_Eq (Guint (Text_Typed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Text_Slot_Calls), 1);
      Assert_True (Text_Edited_Arguments_Match);

      --  A second renderer sends each callback form through its exception arm.
      Current_Text_Cell := Error_Cell;
      On_Edited
        (Error_Cell, On_Text_Edited_Raising_Typed'Unrestricted_Access);
      On_Edited
        (Error_Cell, On_Text_Edited_Raising_Slot'Unrestricted_Access,
         Slot => Slot);
      Gtkada.Bindings.Set_On_Exception
        (Record_Text_Exception'Unrestricted_Access);
      Emit_Text_Edited
        (Glib.Object.Get_Object (Error_Cell), Name, Path, New_Text);
      Assert_Cmpuint_Eq (Guint (Text_Exception_Calls), 2);
      Assert_True (Text_Raising_Arguments_Match);

      --  Release strings and objects after all signal closures have run.
      Interfaces.C.Strings.Free (Name);
      Interfaces.C.Strings.Free (Path);
      Interfaces.C.Strings.Free (New_Text);
      Glib.Object.Unref (Glib.Object.GObject (Cell));
      Glib.Object.Unref (Glib.Object.GObject (Error_Cell));
      Glib.Object.Unref (Glib.Object.GObject (Slot));
   end Test_Text_Edited_Signals;

   --  Round-trip renderer flags and check all four size-query variants.
   procedure Test_State_And_Size is
      Cell      : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Toggle    : constant Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle :=
        Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_New;
      Widget    : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New;
      Xalign    : Interfaces.C.C_float;
      Yalign    : Interfaces.C.C_float;
      Width     : Gint;
      Height    : Gint;
      Xpad      : Gint;
      Ypad      : Gint;
      Min_Size  : Gint;
      Nat_Size  : Gint;
      Min_Height : Gint;
      Nat_Height : Gint;
      Min_Width : Gint;
      Nat_Width : Gint;
      Minimum   : Gtk.Widget.Gtk_Requisition;
      Natural   : Gtk.Widget.Gtk_Requisition;
   begin
      --  Convert is public and supports both Ada-to-C and C-to-Ada handles.
      Assert_True (Convert (Gtk_Cell_Renderer (Cell)) =
        Glib.Object.Get_Object (Cell));
      Assert_True
        (Convert (Glib.Object.Get_Object (Cell)) = Gtk_Cell_Renderer (Cell));

      --  Alignment and padding getters must reflect the values just set.
      Set_Alignment (Cell, 0.25, 0.75);
      Get_Alignment (Cell, Xalign, Yalign);
      Assert_Cmpfloat_Eq (Gdouble (Xalign), 0.25);
      Assert_Cmpfloat_Eq (Gdouble (Yalign), 0.75);
      Set_Padding (Cell, 3, 5);
      Get_Padding (Cell, Xpad, Ypad);
      Assert_Cmpint_Eq (Xpad, 3);
      Assert_Cmpint_Eq (Ypad, 5);

      --  Fixed size, visibility, sensitivity and expander flags round-trip.
      Set_Fixed_Size (Cell, 42, 23);
      Get_Fixed_Size (Cell, Width, Height);
      Assert_Cmpint_Eq (Width, 42);
      Assert_Cmpint_Eq (Height, 23);
      Set_Is_Expanded (Cell, True);
      Set_Is_Expander (Cell, True);
      Assert_True (Get_Is_Expanded (Cell));
      Assert_True (Get_Is_Expander (Cell));
      Set_Is_Expanded (Cell, False);
      Set_Is_Expander (Cell, False);
      Assert_False (Get_Is_Expanded (Cell));
      Assert_False (Get_Is_Expander (Cell));
      Set_Sensitive (Cell, False);
      Assert_False (Get_Sensitive (Cell));
      Set_Sensitive (Cell, True);
      Assert_True (Get_Sensitive (Cell));
      Set_Visible (Cell, False);
      Assert_False (Get_Visible (Cell));
      Set_Visible (Cell, True);
      Assert_True (Get_Visible (Cell));
      --  Text cells are inert by default, whereas toggle cells can activate.
      Assert_False (Is_Activatable (Cell));
      Assert_True (Is_Activatable (Gtk_Cell_Renderer (Toggle)));

      --  Preferred sizes are nonnegative and natural requests are not smaller.
      Get_Preferred_Height (Cell, Widget, Min_Size, Nat_Size);
      Assert_True (Min_Size >= 0 and Nat_Size >= Min_Size);
      Get_Preferred_Height_For_Width
        (Cell, Widget, 100, Min_Height, Nat_Height);
      Assert_True (Min_Height >= 0 and Nat_Height >= Min_Height);
      Get_Preferred_Size (Cell, Widget, Minimum, Natural);
      Assert_True
        (Minimum.Width >= 0 and Natural.Width >= Minimum.Width);
      Assert_True
        (Minimum.Height >= 0 and Natural.Height >= Minimum.Height);
      Get_Preferred_Width (Cell, Widget, Min_Size, Nat_Size);
      Assert_True (Min_Size >= 0 and Nat_Size >= Min_Size);
      Get_Preferred_Width_For_Height
        (Cell, Widget, 30, Min_Width, Nat_Width);
      Assert_True (Min_Width >= 0 and Nat_Width >= Min_Width);
      Assert_True
        (Get_Request_Mode (Cell) in Constant_Size | Height_For_Width |
           Width_For_Height);

      --  Selected renderer state maps to GTK's selected widget-state flag.
      Assert_True
        ((Get_State (Cell, Widget, Cell_Renderer_Selected) and
          Gtk_State_Flag_Selected) /= 0);
   end Test_State_And_Size;

   ------------------------------------
   -- Test_Rendering_And_Activation --
   ------------------------------------

   --  Snapshot text into a render node and exercise hit-area queries. An
   --  inert text renderer must decline activation even with a null event.
   procedure Test_Rendering_And_Activation is
      Cell : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Widget : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New;
      Snap : constant Gtk.Snapshot.Gtk_Snapshot := Gtk.Snapshot.Gtk_Snapshot_New;
      Event : Gdk.Event.Gdk_Event;
      Cell_View : aliased Gtk_Cell_Renderer_Record :=
        Gtk_Cell_Renderer_Record (Cell.all);
      Node : Gsk.Render_Node.Gsk_Render_Node;
      Area : constant Gdk_Rectangle := (X => 0, Y => 0, Width => 160, Height => 40);
      Aligned : Gdk_Rectangle;
      Was_Activated : Boolean;
   begin
      Set_Property (Cell, Text_Property, "snapshot cell");
      Get_Aligned_Area
        (Cell, Widget, Cell_Renderer_Focused, Area, Aligned);
      Assert_True (Aligned.Width >= 0 and Aligned.Height >= 0);
      Assert_True (Aligned.X >= Area.X and Aligned.Y >= Area.Y);
      Assert_True
        (Aligned.X + Aligned.Width <= Area.X + Area.Width);
      Assert_True
        (Aligned.Y + Aligned.Height <= Area.Y + Area.Height);

      Gtk.Cell_Renderer.Snapshot
        (Cell_View'Access, Snap, Widget, Area, Area, Cell_Renderer_Focused);
      Node := Gtk.Snapshot.To_Node (Snap);
      Assert_True (Node /= null and then Node.Is_Created);

      Was_Activated := Activate
        (Cell, Event, Widget, "0", Area, Area, 0);
      Assert_False (Was_Activated);
   end Test_Rendering_And_Activation;

   ----------------------------
   -- Test_Editing_Signals --
   ----------------------------

   --  GtkCellRendererText creates its editable widget for an editable mode;
   --  that real signal emission covers both marshallers on each signal.
   procedure Test_Editing_Signals is
      Widget : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New;
      Slot_Object : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New;
      Event : Gdk.Event.Gdk_Event;
      Area : constant Gdk_Rectangle := (X => 0, Y => 0, Width => 120, Height => 30);
      Cell : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Editable : Gtk.Cell_Editable.Gtk_Cell_Editable;
      Error_Cell : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Error_Editable : Gtk.Cell_Editable.Gtk_Cell_Editable;
   begin
      --  Keep renderer handles so both public Convert overloads are checked.
      Current_Cell := Gtk_Cell_Renderer (Cell);
      Current_Slot_Address := Glib.Object.Get_Object (Slot_Object);
      Assert_True (Current_Cell /= null);
      Set_Property (Cell, Gtk.Cell_Renderer.Mode_Property,
                    Gtk.Cell_Renderer.Cell_Renderer_Mode_Editable);
      Set_Property (Cell, Editable_Property, True);
      On_Editing_Started (Cell, On_Started'Unrestricted_Access);
      On_Editing_Started
        (Cell, On_Started_Slot'Unrestricted_Access, Slot => Slot_Object);
      On_Editing_Canceled (Cell, On_Canceled'Unrestricted_Access);
      On_Editing_Canceled
        (Cell, On_Canceled_Slot'Unrestricted_Access, Slot => Slot_Object);

      --  A null event starts editing programmatically, as documented by GTK.
      Editable := Start_Editing
        (Cell, Event, Widget, "0", Area, Area, 0);
      Assert_True (Editable /= Gtk.Cell_Editable.Null_Gtk_Cell_Editable);
      Assert_Cmpuint_Eq (Guint (Typed_Start_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Start_Calls), 1);
      Assert_True (Typed_Start_Self_Matches);
      Assert_True (Slot_Start_Self_Matches);
      Assert_True (Start_Path_Matches);
      Assert_True
        (Started_Editable /= Gtk.Cell_Editable.Null_Gtk_Cell_Editable);
      Stop_Editing (Cell, True);
      Assert_Cmpuint_Eq (Guint (Typed_Cancel_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Cancel_Calls), 1);

      --  Deliberately raised callback exceptions must be caught and reported
      --  by each generated signal marshaller rather than crossing the C call.
      Current_Cell := Gtk_Cell_Renderer (Error_Cell);
      Set_Property (Error_Cell, Gtk.Cell_Renderer.Mode_Property,
                    Gtk.Cell_Renderer.Cell_Renderer_Mode_Editable);
      Set_Property (Error_Cell, Editable_Property, True);
      On_Editing_Started (Error_Cell, On_Started'Unrestricted_Access);
      On_Editing_Started
        (Error_Cell, On_Started_Slot'Unrestricted_Access, Slot => Slot_Object);
      On_Editing_Canceled (Error_Cell, On_Canceled'Unrestricted_Access);
      On_Editing_Canceled
        (Error_Cell, On_Canceled_Slot'Unrestricted_Access, Slot => Slot_Object);
      Gtkada.Bindings.Set_On_Exception (Record_Exception'Unrestricted_Access);
      Raise_In_Callbacks := True;
      Error_Editable := Start_Editing
        (Error_Cell, Event, Widget, "0", Area, Area, 0);
      Assert_True
        (Error_Editable /= Gtk.Cell_Editable.Null_Gtk_Cell_Editable);
      Stop_Editing (Error_Cell, True);
      Assert_Cmpuint_Eq (Guint (Exception_Calls), 4);
      Assert_Cmpuint_Eq (Guint (Typed_Start_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Slot_Start_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Typed_Cancel_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Slot_Cancel_Calls), 2);
   end Test_Editing_Signals;

   --------------------------------------
   -- Test_Toggle_State_And_Signals --
   --------------------------------------

   --  Round-trip toggle flags and exercise typed and slot signal marshallers.
   procedure Test_Toggle_State_And_Signals is
      Toggle : Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle;
      Slot_Object : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New;
      Signal_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("toggled");
      Path : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("2:4");
   begin
      --  Gtk_New covers the out-parameter constructor; Initialize is a no-op
      --  after construction, as its public contract promises.
      Gtk.Cell_Renderer_Toggle.Gtk_New (Toggle);
      Assert_True (Toggle /= null);
      Gtk.Cell_Renderer_Toggle.Initialize (Toggle);
      Current_Toggle := Toggle;

      --  Set both values for every boolean accessor and verify the readback.
      Set_Activatable (Toggle, False);
      Assert_False (Get_Activatable (Toggle));
      Set_Activatable (Toggle, True);
      Assert_True (Get_Activatable (Toggle));
      Set_Active (Toggle, True);
      Assert_True (Get_Active (Toggle));
      Set_Active (Toggle, False);
      Assert_False (Get_Active (Toggle));
      Set_Radio (Toggle, True);
      Assert_True (Get_Radio (Toggle));
      Set_Radio (Toggle, False);
      Assert_False (Get_Radio (Toggle));

      --  The swapped overload holds a reference to this separate GObject slot.
      Current_Toggle_Slot_Address := Glib.Object.Get_Object (Slot_Object);
      On_Toggled (Toggle, On_Toggled_Typed'Unrestricted_Access);
      On_Toggled
        (Toggle, On_Toggled_Slot'Unrestricted_Access,
         Slot => Slot_Object, After => True);

      --  Emit a real signal with a row path to exercise both generated
      --  marshallers and verify their instance and string arguments.
      Emit_Toggled
        (Glib.Object.Get_Object (Toggle), Signal_Name, Path);
      Assert_Cmpuint_Eq (Guint (Toggle_Typed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Toggle_Slot_Calls), 1);
      Assert_True (Toggle_Typed_Self_Matches);
      Assert_True (Toggle_Slot_Self_Matches);
      Assert_True (Toggle_Typed_Path_Matches);
      Assert_True (Toggle_Slot_Path_Matches);

      --  Raising callbacks exercise each marshaller's exception-reporting arm.
      Gtkada.Bindings.Set_On_Exception
        (Record_Toggle_Exception'Unrestricted_Access);
      On_Toggled
        (Toggle, On_Toggled_Raising_Typed'Unrestricted_Access, After => True);
      On_Toggled
        (Toggle, On_Toggled_Raising_Slot'Unrestricted_Access,
         Slot => Slot_Object);
      Emit_Toggled
        (Glib.Object.Get_Object (Toggle), Signal_Name, Path);
      Assert_Cmpuint_Eq (Guint (Toggle_Typed_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Toggle_Slot_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Toggle_Exception_Calls), 2);
      Assert_True (Raising_Typed_Path_Matches);
      Assert_True (Raising_Slot_Path_Matches);

      --  The signal emitter does not retain its temporary C strings.
      Interfaces.C.Strings.Free (Signal_Name);
      Interfaces.C.Strings.Free (Path);
   end Test_Toggle_State_And_Signals;

   ------------------------------------------
   -- Test_Combo_Properties_And_Signals --
   ------------------------------------------

   --  Exercise GtkCellRendererCombo construction, its model and scalar
   --  properties, and the changed signal's typed and swapped callback paths.
   procedure Test_Combo_Properties_And_Signals is
      Combo : Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo;
      Via_Out : Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo;
      Store : Gtk.List_Store.Gtk_List_Store;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Slot_Object : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New;
      Model_View : Glib.Types.GType_Interface;
      Signal_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("changed");
      Path : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("0");
   begin
      --  Exercise both constructor forms; a repeated Initialize is specified
      --  to leave an already-created renderer intact.
      Combo := Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo_New;
      Gtk.Cell_Renderer_Combo.Gtk_New (Via_Out);
      Assert_True (Combo /= null);
      Assert_True (Via_Out /= null);
      --  Consume GTK's floating references before the explicit unrefs below.
      Combo.Ref_Sink;
      Via_Out.Ref_Sink;
      Slot_Object.Ref_Sink;
      Gtk.Cell_Renderer_Combo.Initialize (Combo);
      Gtk.Cell_Renderer_Combo.Initialize (Via_Out);

      --  The model contains a valid row so the changed callback gets a real
      --  GtkTreeIter, and the model property round-trips its interface view.
      Gtk.List_Store.Gtk_New (Store, (0 => GType_String));
      Gtk.List_Store.Append (Store, Iter);
      Gtk.List_Store.Set (Store, Iter, 0, "choice");
      Current_Combo := Combo;
      Current_Combo_Iter := Iter;
      Model_View := Glib.Types.GType_Interface
        (Gtk.List_Store.Implements_Gtk_Tree_Model.To_Interface (Store));
      Set_Property
        (Combo, Gtk.Cell_Renderer_Combo.Model_Property, Model_View);
      Assert_True
        (Get_Property
           (Combo, Gtk.Cell_Renderer_Combo.Model_Property) = Model_View);

      --  Verify both values of the entry flag and the configured text column.
      Set_Property (Combo, Gtk.Cell_Renderer_Combo.Has_Entry_Property, True);
      Assert_True
        (Get_Property (Combo, Gtk.Cell_Renderer_Combo.Has_Entry_Property));
      Set_Property (Combo, Gtk.Cell_Renderer_Combo.Has_Entry_Property, False);
      Assert_False
        (Get_Property (Combo, Gtk.Cell_Renderer_Combo.Has_Entry_Property));
      Set_Property (Combo, Gtk.Cell_Renderer_Combo.Text_Column_Property, 0);
      Assert_Cmpint_Eq
        (Get_Property (Combo, Gtk.Cell_Renderer_Combo.Text_Column_Property), 0);
      Assert_Cmpstr_Eq
        (String (Gtk.Cell_Renderer_Combo.Signal_Changed), "changed");

      --  Emitting a changed signal checks both marshallers and their arguments.
      Current_Combo_Slot_Address := Glib.Object.Get_Object (Slot_Object);
      Gtk.Cell_Renderer_Combo.On_Changed
        (Combo, On_Combo_Changed_Typed'Unrestricted_Access);
      Gtk.Cell_Renderer_Combo.On_Changed
        (Combo, On_Combo_Changed_Slot'Unrestricted_Access,
         Slot => Slot_Object, After => True);
      Emit_Combo_Changed
        (Glib.Object.Get_Object (Combo), Signal_Name, Path,
         Gtk.Tree_Model.To_Address (Iter));
      Assert_Cmpuint_Eq (Guint (Combo_Typed_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Combo_Slot_Calls), 1);
      Assert_True (Combo_Typed_Self_Matches);
      Assert_True (Combo_Slot_Self_Matches);
      Assert_True (Combo_Typed_Arguments_Match);
      Assert_True (Combo_Slot_Arguments_Match);

      --  Raising handlers exercise Process_Exception in both generated
      --  marshallers while the non-raising handlers continue to be checked.
      Gtkada.Bindings.Set_On_Exception
        (Record_Combo_Exception'Unrestricted_Access);
      Gtk.Cell_Renderer_Combo.On_Changed
        (Combo, On_Combo_Raising_Typed'Unrestricted_Access, After => True);
      Gtk.Cell_Renderer_Combo.On_Changed
        (Combo, On_Combo_Raising_Slot'Unrestricted_Access, Slot => Slot_Object);
      Emit_Combo_Changed
        (Glib.Object.Get_Object (Combo), Signal_Name, Path,
         Gtk.Tree_Model.To_Address (Iter));
      Assert_Cmpuint_Eq (Guint (Combo_Typed_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Combo_Slot_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Combo_Exception_Calls), 2);
      Assert_True (Combo_Raising_Arguments_Match);

      --  Release C strings and objects whose lifetime is owned by this test.
      Interfaces.C.Strings.Free (Signal_Name);
      Interfaces.C.Strings.Free (Path);
      Glib.Object.Unref (Glib.Object.GObject (Via_Out));
      Glib.Object.Unref (Glib.Object.GObject (Combo));
      Glib.Object.Unref (Glib.Object.GObject (Store));
      Glib.Object.Unref (Glib.Object.GObject (Slot_Object));
   end Test_Combo_Properties_And_Signals;

   ---------------------------------------------
   -- Test_Accel_Constructors_And_Signals --
   ---------------------------------------------

   --  Exercise both public constructors and all four accel signal overloads.
   procedure Test_Accel_Constructors_And_Signals is
      Accel : constant Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel :=
        Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel_New;
      Via_Out : Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel;
      Slot_Object : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New;
      Cleared_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("accel-cleared");
      Edited_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("accel-edited");
      Path : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("row/1");
   begin
      --  Check the function and out-parameter constructors, and verify that
      --  repeated Initialize calls preserve an already-created GObject.
      --  The two handles must each wrap a live GObject after initialization.
      Gtk.Cell_Renderer_Accel.Gtk_New (Via_Out);
      Assert_True (Accel /= null and then Via_Out /= null);
      Accel.Ref_Sink;
      Via_Out.Ref_Sink;
      Slot_Object.Ref_Sink;
      Gtk.Cell_Renderer_Accel.Initialize (Accel);
      Gtk.Cell_Renderer_Accel.Initialize (Via_Out);
      Assert_True (Accel.Is_Created and then Via_Out.Is_Created);

      --  Connect both callback forms for accel-cleared and accel-edited.
      Current_Accel := Accel;
      Current_Accel_Slot_Address := Glib.Object.Get_Object (Slot_Object);
      Gtk.Cell_Renderer_Accel.On_Accel_Cleared
        (Accel, On_Accel_Cleared_Typed'Unrestricted_Access);
      Gtk.Cell_Renderer_Accel.On_Accel_Cleared
        (Accel, On_Accel_Cleared_Slot'Unrestricted_Access,
         Slot => Slot_Object, After => True);
      Gtk.Cell_Renderer_Accel.On_Accel_Edited
        (Accel, On_Accel_Edited_Typed'Unrestricted_Access);
      Gtk.Cell_Renderer_Accel.On_Accel_Edited
        (Accel, On_Accel_Edited_Slot'Unrestricted_Access,
         Slot => Slot_Object, After => True);

      --  Signal emission checks the callbacks' instance and all converted
      --  path and accelerator values without synthesizing keyboard events.
      Emit_Accel_Cleared
        (Glib.Object.Get_Object (Accel), Cleared_Name, Path);
      Emit_Accel_Edited
        (Glib.Object.Get_Object (Accel), Edited_Name, Path,
         Interfaces.C.unsigned (16#61#),
         Interfaces.C.unsigned (Gdk.Enums.Gdk_Control_Mask),
         Interfaces.C.unsigned (38));
      Assert_Cmpuint_Eq (Guint (Accel_Typed_Cleared_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Accel_Slot_Cleared_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Accel_Typed_Edited_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Accel_Slot_Edited_Calls), 1);
      Assert_True (Accel_Cleared_Arguments_Match);
      Assert_True (Accel_Edited_Arguments_Match);

      --  Each raising callback verifies its values before raising. GtkAda
      --  reports all four exceptions without letting one cross the C frame.
      Gtkada.Bindings.Set_On_Exception
        (Record_Accel_Exception'Unrestricted_Access);
      Gtk.Cell_Renderer_Accel.On_Accel_Cleared
        (Accel, On_Accel_Cleared_Raising_Typed'Unrestricted_Access,
         After => True);
      Gtk.Cell_Renderer_Accel.On_Accel_Cleared
        (Accel, On_Accel_Cleared_Raising_Slot'Unrestricted_Access,
         Slot => Slot_Object);
      Gtk.Cell_Renderer_Accel.On_Accel_Edited
        (Accel, On_Accel_Edited_Raising_Typed'Unrestricted_Access,
         After => True);
      Gtk.Cell_Renderer_Accel.On_Accel_Edited
        (Accel, On_Accel_Edited_Raising_Slot'Unrestricted_Access,
         Slot => Slot_Object);
      Emit_Accel_Cleared
        (Glib.Object.Get_Object (Accel), Cleared_Name, Path);
      Emit_Accel_Edited
        (Glib.Object.Get_Object (Accel), Edited_Name, Path,
         Interfaces.C.unsigned (16#61#),
         Interfaces.C.unsigned (Gdk.Enums.Gdk_Control_Mask),
         Interfaces.C.unsigned (38));
      Assert_Cmpuint_Eq (Guint (Accel_Typed_Cleared_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Accel_Slot_Cleared_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Accel_Typed_Edited_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Accel_Slot_Edited_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Accel_Exception_Calls), 4);
      Assert_True (Accel_Raising_Arguments_Match);

      --  The signal closures keep their slot alive until renderer disposal;
      --  dropping our references after the signal checks releases both.
      Interfaces.C.Strings.Free (Cleared_Name);
      Interfaces.C.Strings.Free (Edited_Name);
      Interfaces.C.Strings.Free (Path);
      Glib.Object.Unref (Glib.Object.GObject (Accel));
      Glib.Object.Unref (Glib.Object.GObject (Via_Out));
      Glib.Object.Unref (Glib.Object.GObject (Slot_Object));
   end Test_Accel_Constructors_And_Signals;

   --------------------------------------------------
   -- Test_Spin_Constructors_And_Properties --
   --------------------------------------------------

   --  Exercise both spin-renderer constructors, the idempotent initializer,
   --  and all three properties declared by Gtk.Cell_Renderer_Spin.
   procedure Test_Spin_Constructors_And_Properties is
      By_Function : constant Gtk.Cell_Renderer_Spin.Gtk_Cell_Renderer_Spin :=
        Gtk.Cell_Renderer_Spin.Gtk_Cell_Renderer_Spin_New;
      By_Out : Gtk.Cell_Renderer_Spin.Gtk_Cell_Renderer_Spin;
      Adj : constant Gtk.Adjustment.Gtk_Adjustment :=
        Gtk.Adjustment.Gtk_Adjustment_New (4.0, 0.0, 10.0, 1.0, 2.0);
      Read_Back : Glib.Object.GObject;
   begin
      --  The out-parameter constructor reaches the same Initialize routine;
      --  calling it again confirms an already-created renderer is preserved.
      Gtk.Cell_Renderer_Spin.Gtk_New (By_Out);
      By_Function.Ref_Sink;
      By_Out.Ref_Sink;
      Adj.Ref_Sink;
      Assert_True (By_Function /= null and By_Out /= null);
      Gtk.Cell_Renderer_Spin.Initialize (By_Out);
      Assert_True
        (Gtk.Cell_Renderer_Spin.Get_Type /= Glib.GType_Invalid);

      --  GtkCellRendererSpin exposes climb-rate and digits as scalar GObject
      --  properties; round-tripping verifies the typed property descriptors.
      Set_Property
        (By_Function, Gtk.Cell_Renderer_Spin.Climb_Rate_Property, 0.25);
      Assert_Cmpfloat_Eq
        (Get_Property
           (By_Function, Gtk.Cell_Renderer_Spin.Climb_Rate_Property), 0.25);
      Set_Property
        (By_Function, Gtk.Cell_Renderer_Spin.The_Digits_Property, 3);
      Assert_Cmpuint_Eq
        (Get_Property
           (By_Function, Gtk.Cell_Renderer_Spin.The_Digits_Property), 3);

      --  The adjustment property owns the range used by an edited spin cell.
      Set_Property
        (By_Function, Gtk.Cell_Renderer_Spin.Adjustment_Property, Adj);
      Read_Back :=
        Get_Property (By_Function, Gtk.Cell_Renderer_Spin.Adjustment_Property);
      Assert_True
        (Glib.Object.Get_Object (Read_Back) =
         Glib.Object.Get_Object (Adj));

      --  Each renderer and the separately allocated adjustment now has one
      --  reference from this test; releasing it also drops the property ref.
      Glib.Object.Unref (Glib.Object.GObject (By_Function));
      Glib.Object.Unref (Glib.Object.GObject (By_Out));
      Glib.Object.Unref (Glib.Object.GObject (Adj));
   end Test_Spin_Constructors_And_Properties;

   ------------------------------------------------------
   -- Test_Spinner_Constructors_And_Properties --
   ------------------------------------------------------

   --  Exercise both spinner-renderer constructors, idempotent initialization,
   --  and the active, pulse and icon-size properties.
   procedure Test_Spinner_Constructors_And_Properties is
      use type Gtk.Enums.Gtk_Icon_Size;
      By_Function : constant Gtk.Cell_Renderer_Spinner.Gtk_Cell_Renderer_Spinner :=
        Gtk.Cell_Renderer_Spinner.Gtk_Cell_Renderer_Spinner_New;
      By_Out : Gtk.Cell_Renderer_Spinner.Gtk_Cell_Renderer_Spinner;
   begin
      --  Both construction paths return initialized GObjects; repeating
      --  Initialize covers the already-created branch without replacing it.
      Gtk.Cell_Renderer_Spinner.Gtk_New (By_Out);
      By_Function.Ref_Sink;
      By_Out.Ref_Sink;
      Assert_True (By_Function /= null and By_Out /= null);
      Gtk.Cell_Renderer_Spinner.Initialize (By_Out);
      Assert_True
        (Gtk.Cell_Renderer_Spinner.Get_Type /= Glib.GType_Invalid);

      --  The properties can be set and read back through their typed
      --  descriptors, including the flags-based GtkIconSize value.
      Set_Property (By_Function, Gtk.Cell_Renderer_Spinner.Active_Property, True);
      Assert_True
        (Get_Property (By_Function, Gtk.Cell_Renderer_Spinner.Active_Property));
      Set_Property (By_Function, Gtk.Cell_Renderer_Spinner.Active_Property, False);
      Assert_False
        (Get_Property (By_Function, Gtk.Cell_Renderer_Spinner.Active_Property));
      Set_Property (By_Function, Gtk.Cell_Renderer_Spinner.Pulse_Property, 23);
      Assert_Cmpuint_Eq
        (Get_Property (By_Function, Gtk.Cell_Renderer_Spinner.Pulse_Property), 23);
      Set_Property
        (By_Function, Gtk.Cell_Renderer_Spinner.Size_Property,
         Gtk.Enums.Icon_Size_Large);
      Assert_True
        (Get_Property (By_Function, Gtk.Cell_Renderer_Spinner.Size_Property) =
         Gtk.Enums.Icon_Size_Large);

      --  Release each constructor's reference after all property checks.
      Glib.Object.Unref (Glib.Object.GObject (By_Function));
      Glib.Object.Unref (Glib.Object.GObject (By_Out));
   end Test_Spinner_Constructors_And_Properties;

   ----------------------------------------------------
   -- Test_Progress_Properties_And_Orientation --
   ----------------------------------------------------

   --  Exercise both progress-renderer constructors, scalar properties, and
   --  the GtkOrientable conversion and orientation wrappers.
   procedure Test_Progress_Properties_And_Orientation is
      use type Gtk.Cell_Renderer_Progress.Gtk_Cell_Renderer_Progress;
      By_Function : constant Gtk.Cell_Renderer_Progress.Gtk_Cell_Renderer_Progress :=
        Gtk.Cell_Renderer_Progress.Gtk_Cell_Renderer_Progress_New;
      By_Out : Gtk.Cell_Renderer_Progress.Gtk_Cell_Renderer_Progress;
      Orientable : Gtk.Orientable.Gtk_Orientable;
      Back_To_Renderer : Gtk.Cell_Renderer_Progress.Gtk_Cell_Renderer_Progress;
   begin
      --  Gtk_New exercises the out-parameter constructor. Initialize is safe
      --  to call again and must leave the existing GObject intact.
      Gtk.Cell_Renderer_Progress.Gtk_New (By_Out);
      By_Function.Ref_Sink;
      By_Out.Ref_Sink;
      Assert_True (By_Function /= null and By_Out /= null);
      Gtk.Cell_Renderer_Progress.Initialize (By_Out);
      Assert_True
        (Gtk.Cell_Renderer_Progress.Get_Type /= Glib.GType_Invalid);

      --  Each property round-trips through GtkAda's generic GObject helpers.
      Set_Property
        (By_Function, Gtk.Cell_Renderer_Progress.Inverted_Property, True);
      Assert_True
        (Get_Property
           (By_Function, Gtk.Cell_Renderer_Progress.Inverted_Property));
      Set_Property
        (By_Function, Gtk.Cell_Renderer_Progress.Pulse_Property, 7);
      Assert_Cmpint_Eq
        (Get_Property
           (By_Function, Gtk.Cell_Renderer_Progress.Pulse_Property), 7);
      Set_Property
        (By_Function, Gtk.Cell_Renderer_Progress.Text_Property, "working");
      Assert_Cmpstr_Eq
        (Get_Property
           (By_Function, Gtk.Cell_Renderer_Progress.Text_Property), "working");
      Set_Property
        (By_Function, Gtk.Cell_Renderer_Progress.Text_Xalign_Property, 0.25);
      Assert_Cmpfloat_Eq
        (Gdouble (Get_Property
           (By_Function, Gtk.Cell_Renderer_Progress.Text_Xalign_Property)),
         0.25);
      Set_Property
        (By_Function, Gtk.Cell_Renderer_Progress.Text_Yalign_Property, 0.75);
      Assert_Cmpfloat_Eq
        (Gdouble (Get_Property
           (By_Function, Gtk.Cell_Renderer_Progress.Text_Yalign_Property)),
         0.75);
      Set_Property
        (By_Function, Gtk.Cell_Renderer_Progress.Value_Property, 63);
      Assert_Cmpint_Eq
        (Get_Property
           (By_Function, Gtk.Cell_Renderer_Progress.Value_Property), 63);

      --  The object and interface views refer to the same orientable object.
      Assert_True
        (Gtk.Cell_Renderer_Progress.Get_Orientation (By_Function) =
         Orientation_Horizontal);
      Gtk.Cell_Renderer_Progress.Set_Orientation
        (By_Function, Orientation_Vertical);
      Assert_True
        (Gtk.Cell_Renderer_Progress.Get_Orientation (By_Function) =
         Orientation_Vertical);
      Orientable := Gtk.Cell_Renderer_Progress."+" (By_Function);
      Back_To_Renderer := Gtk.Cell_Renderer_Progress."-" (Orientable);
      Assert_True
        (Glib.Object.Get_Object (Back_To_Renderer) =
         Glib.Object.Get_Object (By_Function));
      Assert_True
        (Gtk.Cell_Renderer_Progress.Get_Orientation (Back_To_Renderer) =
         Orientation_Vertical);

      --  Release the two floating GObjects after the API assertions finish.
      Glib.Object.Unref (Glib.Object.GObject (By_Function));
      Glib.Object.Unref (Glib.Object.GObject (By_Out));
   end Test_Progress_Properties_And_Orientation;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func ("/cell-renderer/state-size", Test_State_And_Size'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cell-renderer/text-constructors-fixed-height",
      Test_Text_Constructors_And_Fixed_Height'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cell-renderer/text-edited-signals",
      Test_Text_Edited_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cell-renderer/render-activation",
      Test_Rendering_And_Activation'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cell-renderer/editing-signals",
      Test_Editing_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cell-renderer/toggle-state-signals",
      Test_Toggle_State_And_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cell-renderer/combo-properties-signals",
      Test_Combo_Properties_And_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cell-renderer/progress-properties-orientation",
      Test_Progress_Properties_And_Orientation'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cell-renderer/accel-constructors-signals",
      Test_Accel_Constructors_And_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cell-renderer/spin-constructors-properties",
      Test_Spin_Constructors_And_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cell-renderer/spinner-constructors-properties",
      Test_Spinner_Constructors_And_Properties'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
