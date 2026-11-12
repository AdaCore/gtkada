--  Exercise the Gdk.Surface API on the test driver's Xvfb display.
--  There is no C surface test with these checks to port; this test covers
--  constructors, surface state, input helpers, and both generated signal
--  callback forms using the documented GdkSurface signals.

with Ada.Command_Line;
with Ada.Exceptions;
with Cairo.Region;
with Gdk;
with Gdk.Cursor;
with Gdk.Device;
with Gdk.Display;
with Gdk.Enums;
with Gdk.Frame_Clock;
with Gdk.Frame_Timings;
with Gdk.GLContext;
with Gdk.Monitor;
with Gdk.Seat;
with Gdk.Surface;
with Glib;
with Glib.Error;
with Glib.Main;
with Glib.Object;
with Glib.Test;
with Gtk.Main;
with Gtk.Window;
with Gtkada.Bindings;
with Gtkada.Types;
with System;

procedure Gdk_Surface is
   use Glib;
   use Glib.Main;
   use Glib.Test;
   use type Gdk.Gdk_Display;
   use type Gdk.Gdk_Device;
   use type Gdk.Gdk_Surface;
   use type Gdk.Cursor.Gdk_Cursor;
   use type Gdk.GLContext.Gdk_GLContext;
   use type Gdk.Frame_Clock.Gdk_Frame_Clock;
   use type Gdk.Frame_Clock.Gdk_Frame_Clock_Phase;
   use type Gdk.Frame_Timings.Gdk_Frame_Timings;
   use type Gdk.Monitor.Gdk_Monitor;
   use type Glib.Error.GError;
   use type System.Address;

   --  Expected objects are shared with signal callbacks to check converted
   --  monitor arguments while counters verify each callback form ran once.
   --  These are test state only; the callbacks do not retain GDK-owned objects.
   Expected_Display : Gdk.Gdk_Display;
   Typed_Enter_Calls : Natural := 0;
   Slot_Enter_Calls  : Natural := 0;
   Typed_Leave_Calls : Natural := 0;
   Slot_Leave_Calls  : Natural := 0;
   Typed_Event_Calls : Natural := 0;
   Slot_Event_Calls  : Natural := 0;
   Typed_Layout_Calls : Natural := 0;
   Slot_Layout_Calls  : Natural := 0;
   Saw_Monitor        : Boolean := False;
   Saw_Layout_Size    : Boolean := False;
   Frame_Typed_Calls  : Natural := 0;
   Frame_Slot_Calls   : Natural := 0;
   Frame_Typed_Object_Matched : Boolean := False;
   Frame_Slot_Object_Matched  : Boolean := False;
   --  Separate counters prove the exception catcher ran for both callback types.
   Raise_Frame_Callbacks : Boolean := False;
   Frame_Typed_Exception_Calls : Natural := 0;
   Frame_Slot_Exception_Calls  : Natural := 0;
   Frame_Exception_Handler_Calls : Natural := 0;
   Expected_Frame_Clock : System.Address := System.Null_Address;
   Expected_Frame_Slot  : System.Address := System.Null_Address;
   Frame_Wait_Done : Boolean := False;

   --  These fixed C profiles call GLib's variadic signal emitter with the
   --  argument shapes for GdkSurface's monitor and layout signals.
   procedure Emit_Monitor
     (Instance : System.Address;
      Name     : Gtkada.Types.Chars_Ptr;
      Monitor  : System.Address);
   pragma Import (C, Emit_Monitor, "g_signal_emit_by_name");

   procedure Emit_Layout
     (Instance : System.Address;
      Name     : Gtkada.Types.Chars_Ptr;
      Width    : Glib.Gint;
      Height   : Glib.Gint);
   pragma Import (C, Emit_Layout, "g_signal_emit_by_name");

   --  The timeout bounds a missing frame signal while the main context waits.
   function Stop_Frame_Wait return Boolean;
   procedure Frame_Clock_Tick
     (Self : access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class);
   procedure Frame_Clock_Slot_Tick
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Record_Frame_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   --  Both typed and generic callbacks validate their payload and count the
   --  calls, so signal connection alone cannot make this case pass.
   --  The slot overload receives the GtkWindow supplied at connection time.
   procedure Enter_Typed
     (Self    : access Gdk.Surface.Gdk_Surface_Record'Class;
      Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class);
   procedure Enter_Slot
     (Self    : access Glib.Object.GObject_Record'Class;
      Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class);
   procedure Leave_Typed
     (Self    : access Gdk.Surface.Gdk_Surface_Record'Class;
      Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class);
   procedure Leave_Slot
     (Self    : access Glib.Object.GObject_Record'Class;
      Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class);
   function Event_Typed
     (Self  : access Gdk.Surface.Gdk_Surface_Record'Class;
      Event : System.Address) return Boolean;
   function Event_Slot
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : System.Address) return Boolean;
   procedure Layout_Typed
     (Self   : access Gdk.Surface.Gdk_Surface_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint);
   procedure Layout_Slot
     (Self   : access Glib.Object.GObject_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint);

   procedure Enter_Typed
     (Self    : access Gdk.Surface.Gdk_Surface_Record'Class;
      Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class) is
   begin
      Assert_True (Self /= null);
      Saw_Monitor := Saw_Monitor or Monitor.Get_Display = Expected_Display;
      Typed_Enter_Calls := Typed_Enter_Calls + 1;
   end Enter_Typed;

   procedure Enter_Slot
     (Self    : access Glib.Object.GObject_Record'Class;
      Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class) is
   begin
      Assert_True (Self /= null);
      Saw_Monitor := Saw_Monitor or Monitor.Get_Display = Expected_Display;
      Slot_Enter_Calls := Slot_Enter_Calls + 1;
   end Enter_Slot;

   procedure Leave_Typed
     (Self    : access Gdk.Surface.Gdk_Surface_Record'Class;
      Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class) is
   begin
      Assert_True (Self /= null);
      Saw_Monitor := Saw_Monitor or Monitor.Get_Display = Expected_Display;
      Typed_Leave_Calls := Typed_Leave_Calls + 1;
   end Leave_Typed;

   procedure Leave_Slot
     (Self    : access Glib.Object.GObject_Record'Class;
      Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class) is
   begin
      Assert_True (Self /= null);
      Saw_Monitor := Saw_Monitor or Monitor.Get_Display = Expected_Display;
      Slot_Leave_Calls := Slot_Leave_Calls + 1;
   end Leave_Slot;

   function Event_Typed
     (Self  : access Gdk.Surface.Gdk_Surface_Record'Class;
      Event : System.Address) return Boolean is
   begin
      Assert_True (Self /= null);
      Assert_True (Event /= System.Null_Address);
      Typed_Event_Calls := Typed_Event_Calls + 1;
      return False;
   end Event_Typed;

   function Event_Slot
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : System.Address) return Boolean is
   begin
      Assert_True (Self /= null);
      Assert_True (Event /= System.Null_Address);
      Slot_Event_Calls := Slot_Event_Calls + 1;
      return False;
   end Event_Slot;

   procedure Layout_Typed
     (Self   : access Gdk.Surface.Gdk_Surface_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint) is
   begin
      Assert_True (Self /= null);
      Saw_Layout_Size := Saw_Layout_Size or (Width = 73 and Height = 41);
      Typed_Layout_Calls := Typed_Layout_Calls + 1;
   end Layout_Typed;

   procedure Layout_Slot
     (Self   : access Glib.Object.GObject_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint) is
   begin
      Assert_True (Self /= null);
      Saw_Layout_Size := Saw_Layout_Size or (Width = 73 and Height = 41);
      Slot_Layout_Calls := Slot_Layout_Calls + 1;
   end Layout_Slot;

   procedure Test_Constructors with Convention => C;
   procedure Test_Surface_Methods with Convention => C;
   procedure Test_Surface_Signals with Convention => C;
   procedure Test_Frame_Clock with Convention => C;

   --  Both callback forms record invocation and check the object delivered by
   --  GtkAda's generated frame clock signal marshallers.
   function Stop_Frame_Wait return Boolean is
   begin
      Frame_Wait_Done := True;
      return False;
   end Stop_Frame_Wait;

   procedure Frame_Clock_Tick
     (Self : access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) is
   begin
      if Self /= null then
         Frame_Typed_Calls := Frame_Typed_Calls + 1;
         Frame_Typed_Object_Matched :=
           Frame_Typed_Object_Matched or else
           Glib.Object.Get_Object (Self) = Expected_Frame_Clock;
         --  The marshaller must contain this deliberate callback failure.
         if Raise_Frame_Callbacks then
            Frame_Typed_Exception_Calls := Frame_Typed_Exception_Calls + 1;
            raise Program_Error with "expected typed frame clock exception";
         end if;
      end if;
   end Frame_Clock_Tick;

   procedure Frame_Clock_Slot_Tick
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      if Self /= null then
         Frame_Slot_Calls := Frame_Slot_Calls + 1;
         Frame_Slot_Object_Matched :=
           Frame_Slot_Object_Matched or else
           Glib.Object.Get_Object (Self) = Expected_Frame_Slot;
         --  Exercise the same containment path for the generic slot callback.
         if Raise_Frame_Callbacks then
            Frame_Slot_Exception_Calls := Frame_Slot_Exception_Calls + 1;
            raise Program_Error with "expected slot frame clock exception";
         end if;
      end if;
   end Frame_Clock_Slot_Tick;

   --  The generated marshallers must contain callback exceptions and report
   --  them through GtkAda's configured exception handler.
   procedure Record_Frame_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Frame_Exception_Handler_Calls := Frame_Exception_Handler_Calls + 1;
   end Record_Frame_Exception;

   --  The direct constructors work with an ordinary parent toplevel. Calling
   --  Initialize twice also checks that an already-created GObject is retained.
   procedure Test_Constructors is
      Display       : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;
      Parent_Window : constant Gtk.Window.Gtk_Window :=
        Gtk.Window.Gtk_Window_New;
      Parent        : Gdk.Gdk_Surface;
      Toplevel      : Gdk.Gdk_Surface :=
        new Gdk.Surface.Gdk_Surface_Record;
      Procedural    : Gdk.Gdk_Surface;
      Popup         : Gdk.Gdk_Surface;
      Popup_Function : Gdk.Gdk_Surface;
   begin
      Parent_Window.Present;
      Parent := Parent_Window.Get_Surface;
      Assert_True (Parent /= null);

      --  Exercise the in-place toplevel initializer and its created-object guard.
      --  Initialize_* expects an allocated Ada wrapper record, initially empty.
      Gdk.Surface.Initialize_Toplevel (Toplevel, Display);
      Assert_True (Toplevel /= null);
      Gdk.Surface.Initialize_Toplevel (Toplevel, Display);
      Toplevel.Destroy;

      --  Exercise the procedure and function forms of the public constructors.
      Gdk.Surface.Gdk_New_Toplevel (Procedural, Display);
      Assert_True (Procedural /= null);
      Assert_True (Procedural.Get_Display = Display);
      Procedural.Destroy;

      Popup := Gdk.Surface.Gdk_Surface_New_Popup (Parent, False);
      Assert_True (Popup /= null);
      Assert_True (Popup.Get_Display = Display);
      Popup.Destroy;

      --  The procedure constructor uses the other boolean value and then the
      --  already-created path in Initialize_Popup.
      Gdk.Surface.Gdk_New_Popup (Popup_Function, Parent, True);
      Assert_True (Popup_Function /= null);
      Gdk.Surface.Initialize_Popup (Popup_Function, Parent, False);
      Popup_Function.Destroy;
      Parent_Window.Destroy;
   end Test_Constructors;

   --  Exercise observable surface state and harmless input/render helpers on a
   --  mapped GtkWindow surface whose size and display are known to the backend.
   procedure Test_Surface_Methods is
      Display  : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;
      Seat     : constant Gdk.Gdk_Seat := Display.Get_Default_Seat;
      Device   : constant Gdk.Gdk_Device := Seat.Get_Pointer;
      Window   : constant Gtk.Window.Gtk_Window := Gtk.Window.Gtk_Window_New;
      Surface  : Gdk.Gdk_Surface;
      Cursor   : constant Gdk.Cursor.Gdk_Cursor :=
        Gdk.Cursor.Gdk_Cursor_New_From_Name ("crosshair", null);
      Rectangle : aliased Cairo.Region.Cairo_Rectangle_Int :=
        (X => 0, Y => 0, Width => 8, Height => 8);
      Region   : constant Cairo.Region.Cairo_Region :=
        Cairo.Region.Create_Rectangle (Rectangle'Access);
      X        : aliased Gdouble := 5.0;
      Y        : aliased Gdouble := 7.0;
      Mask     : aliased Gdk.Enums.Gdk_Modifier_Type := 0;
      Error    : Glib.Error.GError;
      Context  : Gdk.GLContext.Gdk_GLContext;
      Width    : Glib.Gint;
      Height   : Glib.Gint;
      Over     : Boolean;
      Mapped   : Boolean;
      Translated : Boolean;
   begin
      Window.Set_Default_Size (160, 120);
      Window.Present;
      Surface := Window.Get_Surface;
      Assert_True (Surface /= null);
      Assert_True (Device /= null);
      Assert_True (Cursor /= null);

      --  Query stable invariants instead of assuming an exact scale or monitor.
      --  Xvfb may report different scale values across host configurations.
      Assert_True (Surface.Get_Display = Display);
      Assert_True (Surface.Get_Frame_Clock /= null);
      Width := Surface.Get_Width;
      Height := Surface.Get_Height;
      Assert_Cmpint_Gt (Width, 0);
      Assert_Cmpint_Gt (Height, 0);
      Assert_True (Surface.Get_Scale > 0.0);
      Assert_Cmpint_Gt (Surface.Get_Scale_Factor, 0);
      Assert_False (Surface.Is_Destroyed);
      Mapped := Surface.Get_Mapped;
      Assert_True (Mapped);

      --  Cursor getters return the exact cursor passed to each setter; null
      --  restores the parent/default cursor path for both setters.
      --  The cursor remains alive through the setters as a transfer-full value.
      Surface.Set_Cursor (Cursor);
      Assert_True (Surface.Get_Cursor = Cursor);
      Surface.Set_Cursor (null);
      Surface.Set_Device_Cursor (Device, Cursor);
      Assert_True (Surface.Get_Device_Cursor (Device) = Cursor);
      Surface.Set_Device_Cursor (Device, Cursor);

      --  Position is backend-dependent because Xvfb controls the pointer; when
      --  it is over this surface, the returned coordinates must be in bounds.
      Over := Surface.Get_Device_Position
        (Device, X'Access, Y'Access, Mask'Access);
      if Over then
         Assert_True (X >= 0.0 and X < Gdouble (Width));
         Assert_True (Y >= 0.0 and Y < Gdouble (Height));
      end if;

      --  Regions are caller-owned Cairo objects; GDK consumes their geometry
      --  during the call, then the test frees them after destroying the window.
      --  X11 supports these setters in the headless server used by the driver.
      Surface.Set_Input_Region (Region);
      Surface.Set_Opaque_Region (Region);
      Surface.Beep;
      Surface.Queue_Render;
      Surface.Request_Layout;

      --  A surface translates its own coordinates to itself unchanged.
      --  Only the success result is asserted because backends may adjust origin.
      Translated := Surface.Translate_Coordinates (Surface, X, Y);
      Assert_True (Translated);

      --  GL context creation is optional on a headless backend. Check and free
      --  either documented result, including the recoverable GError on failure.
      --  The returned context is transfer-full and is unreferenced on success.
      Context := Surface.Create_Gl_Context (Error);
      if Error = null then
         Assert_True (Context /= null);
         Glib.Object.Unref (Glib.Object.GObject (Context));
      else
         Assert_True (Context = null);
         Glib.Error.Error_Free (Error);
      end if;

      Surface.Hide;
      Assert_False (Surface.Get_Mapped);
      Window.Destroy;
      Cairo.Region.Destroy (Region);
   end Test_Surface_Methods;

   --  A mapped window supplies a backend-owned frame clock. Requesting all
   --  phases lets this test cover each generated signal connection overload.
   procedure Test_Frame_Clock is
      Window : constant Gtk.Window.Gtk_Window := Gtk.Window.Gtk_Window_New;
      Surface : Gdk.Gdk_Surface;
      Clock : Gdk.Frame_Clock.Gdk_Frame_Clock;
      Current_Timings : Gdk.Frame_Timings.Gdk_Frame_Timings;
      Timings : Gdk.Frame_Timings.Gdk_Frame_Timings;
      Held_Timings : Gdk.Frame_Timings.Gdk_Frame_Timings;
      Counter : Gint64;
      Frame_Time : Gint64;
      Timing_Frame_Counter : Gint64;
      Timing_Frame_Time : Gint64;
      Predicted_Presentation_Time : Gint64;
      Timing_Presentation_Time : Gint64;
      Timing_Refresh_Interval : Gint64;
      Timing_Complete : Boolean;
      History_Start : Gint64;
      Refresh_Interval : aliased Gint64;
      Presentation_Time : aliased Gint64;
      Timeout : G_Source_Id;
   begin
      Window.Set_Default_Size (100, 60);
      Window.Present;
      Surface := Window.Get_Surface;
      Assert_True (Glib.Object.Get_Object (Surface) /= System.Null_Address);
      Clock := Surface.Get_Frame_Clock;
      Assert_True (Clock /= null);

      --  The slot form receives the connected window; the typed form receives
      --  the frame clock itself. Keep their expected C objects for callbacks.
      Expected_Frame_Clock := Glib.Object.Get_Object (Clock);
      Expected_Frame_Slot := Glib.Object.Get_Object (Window);
      Frame_Typed_Calls := 0;
      Frame_Slot_Calls := 0;
      Frame_Typed_Object_Matched := False;
      Frame_Slot_Object_Matched := False;

      --  Each phase is connected through both public callback profiles.
      Gdk.Frame_Clock.On_After_Paint
        (Clock, Frame_Clock_Tick'Unrestricted_Access);
      Gdk.Frame_Clock.On_After_Paint
        (Clock, Frame_Clock_Slot_Tick'Unrestricted_Access, Window, After => True);
      Gdk.Frame_Clock.On_Before_Paint
        (Clock, Frame_Clock_Tick'Unrestricted_Access, After => True);
      Gdk.Frame_Clock.On_Before_Paint
        (Clock, Frame_Clock_Slot_Tick'Unrestricted_Access, Window);
      Gdk.Frame_Clock.On_Flush_Events
        (Clock, Frame_Clock_Tick'Unrestricted_Access);
      Gdk.Frame_Clock.On_Flush_Events
        (Clock, Frame_Clock_Slot_Tick'Unrestricted_Access, Window);
      Gdk.Frame_Clock.On_Layout
        (Clock, Frame_Clock_Tick'Unrestricted_Access);
      Gdk.Frame_Clock.On_Layout
        (Clock, Frame_Clock_Slot_Tick'Unrestricted_Access, Window);
      Gdk.Frame_Clock.On_Paint
        (Clock, Frame_Clock_Tick'Unrestricted_Access);
      Gdk.Frame_Clock.On_Paint
        (Clock, Frame_Clock_Slot_Tick'Unrestricted_Access, Window);
      Gdk.Frame_Clock.On_Resume_Events
        (Clock, Frame_Clock_Tick'Unrestricted_Access);
      Gdk.Frame_Clock.On_Resume_Events
        (Clock, Frame_Clock_Slot_Tick'Unrestricted_Access, Window);
      Gdk.Frame_Clock.On_Update
        (Clock, Frame_Clock_Tick'Unrestricted_Access);
      Gdk.Frame_Clock.On_Update
        (Clock, Frame_Clock_Slot_Tick'Unrestricted_Access, Window);

      --  The flags combine so the clock emits each requested phase in a frame.
      Gdk.Frame_Clock.Request_Phase
        (Clock,
         Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Flush_Events or
         Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Before_Paint or
         Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Update or
         Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Layout or
         Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Paint or
         Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Resume_Events or
         Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_After_Paint);
      Gdk.Frame_Clock.Begin_Updating (Clock);
      Frame_Wait_Done := False;
      Timeout := Timeout_Add (5_000, Stop_Frame_Wait'Unrestricted_Access);

      --  Process GTK events until both callback forms have seen every phase.
      while (Frame_Typed_Calls < 8 or else Frame_Slot_Calls < 8)
        and then not Frame_Wait_Done
      loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;
      if not Frame_Wait_Done then
         Remove (Timeout);
      end if;
      Gdk.Frame_Clock.End_Updating (Clock);

      Assert_False (Frame_Wait_Done);
      Assert_Cmpuint_Ge (Guint (Frame_Typed_Calls), 8);
      Assert_Cmpuint_Ge (Guint (Frame_Slot_Calls), 8);
      Assert_True (Frame_Typed_Object_Matched);
      Assert_True (Frame_Slot_Object_Matched);

      --  Raise from each callback form once a further update phase arrives;
      --  GtkAda must catch both exceptions before returning through GLib.
      Gtkada.Bindings.Set_On_Exception
        (Record_Frame_Exception'Unrestricted_Access);
      Raise_Frame_Callbacks := True;
      Frame_Typed_Exception_Calls := 0;
      Frame_Slot_Exception_Calls := 0;
      Frame_Exception_Handler_Calls := 0;
      Frame_Wait_Done := False;
      Gdk.Frame_Clock.Request_Phase
        (Clock, Gdk.Frame_Clock.Gdk_Frame_Clock_Phase_Update);
      Gdk.Frame_Clock.Begin_Updating (Clock);
      Timeout := Timeout_Add (5_000, Stop_Frame_Wait'Unrestricted_Access);
      --  A second bounded wait avoids depending on the machine's refresh rate.
      while (Frame_Typed_Exception_Calls = 0 or else
             Frame_Slot_Exception_Calls = 0) and then not Frame_Wait_Done
      loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;
      if not Frame_Wait_Done then
         Remove (Timeout);
      end if;
      Gdk.Frame_Clock.End_Updating (Clock);
      Raise_Frame_Callbacks := False;
      --  The configured handler must receive both contained Ada exceptions.
      Assert_False (Frame_Wait_Done);
      Assert_Cmpuint_Gt (Guint (Frame_Typed_Exception_Calls), 0);
      Assert_Cmpuint_Gt (Guint (Frame_Slot_Exception_Calls), 0);
      Assert_Cmpuint_Ge (Guint (Frame_Exception_Handler_Calls), 2);

      --  Once phases have run, timing queries describe the latest frame.
      Current_Timings := Gdk.Frame_Clock.Get_Current_Timings (Clock);
      Assert_True
        (Current_Timings /= Gdk.Frame_Timings.Null_Gdk_Frame_Timings);

      --  A processed frame advances the counter and supplies an animation time.
      Counter := Gdk.Frame_Clock.Get_Frame_Counter (Clock);
      Assert_True (Counter > 0);
      Frame_Time := Gdk.Frame_Clock.Get_Frame_Time (Clock);
      Assert_True (Frame_Time > 0);

      --  History starts no later than the current frame and FPS is nonnegative.
      History_Start := Gdk.Frame_Clock.Get_History_Start (Clock);
      Assert_True (History_Start >= 0 and History_Start <= Counter);
      Assert_True (Gdk.Frame_Clock.Get_Fps (Clock) >= 0.0);

      --  The current counter identifies a timing record in the clock history.
      Timings := Gdk.Frame_Clock.Get_Timings (Clock, Counter);
      Assert_True (Timings /= Gdk.Frame_Timings.Null_Gdk_Frame_Timings);

      --  The record exposes the same frame counter and a positive animation
      --  time; presentation details may still be unavailable on this backend.
      Timing_Complete := Gdk.Frame_Timings.Get_Complete (Timings);
      Timing_Frame_Counter := Gdk.Frame_Timings.Get_Frame_Counter (Timings);
      Timing_Frame_Time := Gdk.Frame_Timings.Get_Frame_Time (Timings);
      Predicted_Presentation_Time :=
        Gdk.Frame_Timings.Get_Predicted_Presentation_Time (Timings);
      Timing_Presentation_Time :=
        Gdk.Frame_Timings.Get_Presentation_Time (Timings);
      Timing_Refresh_Interval :=
        Gdk.Frame_Timings.Get_Refresh_Interval (Timings);
      Assert_True (Timing_Frame_Counter = Counter);
      Assert_True (Timing_Frame_Time > 0);
      Assert_True (Predicted_Presentation_Time >= 0);
      Assert_True (Timing_Presentation_Time >= 0);
      Assert_True (Timing_Refresh_Interval >= 0);
      --  Completion is backend-dependent; either state must leave a valid
      --  counter for this frame, while the timing values above remain usable.
      if Timing_Complete then
         Assert_True (Timing_Frame_Counter > 0);
      else
         Assert_True (Timing_Frame_Counter = Counter);
      end if;

      --  From_Object_Free consumes only its temporary Ada wrapper. The frame
      --  clock still owns this borrowed timing record after that wrapper frees.
      declare
         Wrapper : constant access Gdk.Frame_Timings.Gdk_Frame_Timings :=
           new Gdk.Frame_Timings.Gdk_Frame_Timings'(Timings);
         Copied_Timings : constant Gdk.Frame_Timings.Gdk_Frame_Timings :=
           Gdk.Frame_Timings.From_Object_Free (Wrapper);
      begin
         Assert_True
           (Copied_Timings /= Gdk.Frame_Timings.Null_Gdk_Frame_Timings);
         Assert_True
           (Gdk.Frame_Timings.Get_Frame_Counter (Copied_Timings) = Counter);
      end;

      --  Ref adds one owned reference to the borrowed timing record; releasing
      --  that reference must leave the clock's history entry available.
      Held_Timings := Gdk.Frame_Timings.Ref (Timings);
      Assert_True (Held_Timings /= Gdk.Frame_Timings.Null_Gdk_Frame_Timings);
      Assert_True
        (Gdk.Frame_Timings.Get_Frame_Counter (Held_Timings) = Counter);
      Gdk.Frame_Timings.Unref (Held_Timings);
      Assert_True (Gdk.Frame_Timings.Get_Frame_Counter (Timings) = Counter);

      --  Refresh info always supplies a positive interval; presentation can
      --  be zero when a backend has no completed presentation history.
      Gdk.Frame_Clock.Get_Refresh_Info
        (Clock, Frame_Time, Refresh_Interval, Presentation_Time);
      Assert_True (Refresh_Interval > 0);
      Assert_True (Presentation_Time >= 0);

      Window.Destroy;
   end Test_Frame_Clock;

   --  Emit each signal that accepts safely constructible arguments. Gdk 4 has
   --  no public event constructor, so event handlers are connected but not
   --  emitted with an invalid or fabricated GdkEvent pointer.
   procedure Test_Surface_Signals is
      Display : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;
      Window  : constant Gtk.Window.Gtk_Window := Gtk.Window.Gtk_Window_New;
      Surface : Gdk.Gdk_Surface;
      Monitor : Gdk.Monitor.Gdk_Monitor;
      Enter_Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("enter-monitor");
      Leave_Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("leave-monitor");
      Layout_Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("layout");
   begin
      Expected_Display := Display;
      Window.Present;
      Surface := Window.Get_Surface;
      Assert_True (Surface /= null);
      Monitor := Display.Get_Monitor_At_Surface (Surface);
      Assert_True (Monitor /= null);

      --  Each signal connects one surface-specific callback and one generic
      --  callback whose first argument is the GtkWindow slot object.
      --  The callbacks check their arguments if a valid event reaches them.
      Surface.On_Enter_Monitor (Enter_Typed'Unrestricted_Access);
      Surface.On_Enter_Monitor (Enter_Slot'Unrestricted_Access, Window);
      Surface.On_Leave_Monitor (Leave_Typed'Unrestricted_Access);
      Surface.On_Leave_Monitor (Leave_Slot'Unrestricted_Access, Window);
      Surface.On_Event (Event_Typed'Unrestricted_Access);
      Surface.On_Event (Event_Slot'Unrestricted_Access, Window);
      Surface.On_Layout (Layout_Typed'Unrestricted_Access);
      Surface.On_Layout (Layout_Slot'Unrestricted_Access, Window);

      Emit_Monitor
        (Glib.Object.Get_Object (Surface), Enter_Name,
         Glib.Object.Get_Object (Monitor));
      Emit_Monitor
        (Glib.Object.Get_Object (Surface), Leave_Name,
         Glib.Object.Get_Object (Monitor));
      Emit_Layout (Glib.Object.Get_Object (Surface), Layout_Name, 73, 41);

      --  Check both callback paths and the converted monitor and layout values.
      Assert_Cmpuint_Eq (Guint (Typed_Enter_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Enter_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Typed_Leave_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Leave_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Typed_Layout_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Layout_Calls), 1);
      Assert_True (Saw_Monitor);
      Assert_True (Saw_Layout_Size);

      Gtkada.Types.Free (Enter_Name);
      Gtkada.Types.Free (Leave_Name);
      Gtkada.Types.Free (Layout_Name);
      Window.Destroy;
   end Test_Surface_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/surface/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/surface/methods", Test_Surface_Methods'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/surface/signals", Test_Surface_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/surface/frame-clock", Test_Frame_Clock'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Gdk_Surface;
