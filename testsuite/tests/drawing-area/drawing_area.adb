--  Exercises the Gtk.Drawing_Area binding.
--
--  /drawing-area/content-size checks that the content width and height
--  round-trip and that they are what the widget asks for as its natural size.
--
--  /drawing-area/draw-func is the interesting one: it puts a drawing area in
--  a window, presents it, and pumps the main loop until the draw function has
--  actually run. That proves the whole path works -- the callback is
--  marshalled, the cairo_t reaches Ada as a usable Cairo_Context, and the
--  width and height handed over are the size the widget was allocated.

with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C;
with Glib;             use Glib;
with Glib.Main;        use Glib.Main;
with Glib.Test;        use Glib.Test;
with Glib.Object;      use Glib.Object;
with Cairo;            use Cairo;
with Gtk.Accessible;   use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Main;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;
with Gtkada.Bindings;
with Gtkada.Types;
with System;

procedure Drawing_Area is
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type System.Address;

   Drawn : Boolean := False;
   pragma Volatile (Drawn);
   --  Set from the draw function, which is dispatched by the main loop.

   Drawn_Width, Drawn_Height : Gint := -1;
   --  The size the draw function was told about.

   --  These observations check user data passed through the generic callback
   --  and its destroy notifier when GTK releases the callback registration.
   type Draw_Data is record
      Marker : Gint;
   end record;
   Data_Draw_Called : Boolean := False;
   Data_Draw_Marker : Gint := -1;
   Destroy_Count : Natural := 0;
   Destroyed_Marker : Gint := -1;

   procedure Destroy_Draw_Data (Data : in out Draw_Data);

   package User_Data_Draw is new Set_Draw_Func_User_Data
     (Draw_Data, Destroy_Draw_Data);

   procedure Draw_With_Data
     (Area      : not null access Gtk_Drawing_Area_Record'Class;
      Cr        : Cairo_Context;
      Width     : Gint;
      Height    : Gint;
      User_Data : Draw_Data);

   --  Save the expected objects so the resize callback tests can check both
   --  the typed adapter and the slot-based adapter against actual identities.
   Typed_Resize_Called : Boolean := False;
   Slot_Resize_Called : Boolean := False;
   Slot_Resize_Object_Valid : Boolean := False;
   Resize_Width, Resize_Height : Gint := -1;
   Expected_Signal_Area : Gtk_Drawing_Area := null;
   Expected_Signal_Slot : Gtk_Window := null;
   Caught_Exceptions : Natural := 0;

   --  This fixed Ada profile emits the two integer arguments of the resize
   --  signal through GLib's variadic signal API.
   procedure Emit_Resize
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr;
      Width  : Interfaces.C.int;
      Height : Interfaces.C.int)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Typed_Resize
     (Area : access Gtk_Drawing_Area_Record'Class;
      Width, Height : Gint);
   procedure Slot_Resize
     (Self : access GObject_Record'Class;
      Width, Height : Gint);

   procedure Raise_Typed_Resize
     (Area : access Gtk_Drawing_Area_Record'Class;
      Width, Height : Gint);
   procedure Raise_Slot_Resize
     (Self : access GObject_Record'Class;
      Width, Height : Gint);

   Done : Boolean := False;
   pragma Volatile (Done);

   function Stop return Boolean;

   procedure Draw
     (Area   : not null access Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo_Context;
      Width  : Gint;
      Height : Gint);

   procedure Test_Content_Size
   with Convention => C;


   procedure Test_Draw_Func
   with Convention => C;

   procedure Test_Accessibility
   with Convention => C;

   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      --  Signal marshallers route callback failures to GtkAda's hook.
      Caught_Exceptions := Caught_Exceptions + 1;
   end Record_Callback_Exception;

   ----------
   -- Stop --
   ----------

   function Stop return Boolean is
   begin
      Done := True;
      Wakeup (null);
      return False;
   end Stop;

   -----------------------
   -- Destroy_Draw_Data --
   -----------------------

   procedure Destroy_Draw_Data (Data : in out Draw_Data) is
   begin
      --  Record the value; assertions stay in the test thread rather than
      --  propagating exceptions through GTK's C destroy-notifier call.
      Destroyed_Marker := Data.Marker;
      Destroy_Count := Destroy_Count + 1;
   end Destroy_Draw_Data;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Area   : not null access Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo_Context;
      Width  : Gint;
      Height : Gint)
   is
      pragma Unreferenced (Area);
   begin
      --  Draw something, so that the context is not merely received but used.
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Rectangle (Cr, 0.0, 0.0, Gdouble (Width), Gdouble (Height));
      Fill (Cr);

      Drawn_Width := Width;
      Drawn_Height := Height;
      Drawn := True;
   end Draw;

   ----------------------
   -- Draw_With_Data --
   ----------------------

   procedure Draw_With_Data
     (Area      : not null access Gtk_Drawing_Area_Record'Class;
      Cr        : Cairo_Context;
      Width     : Gint;
      Height    : Gint;
      User_Data : Draw_Data)
   is
      pragma Unreferenced (Area);
   begin
      --  Use the context as in the non-generic callback and capture the
      --  callback's user data for assertions after returning to the main loop.
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Rectangle (Cr, 0.0, 0.0, Gdouble (Width), Gdouble (Height));
      Fill (Cr);
      Data_Draw_Marker := User_Data.Marker;
      Data_Draw_Called := True;
   end Draw_With_Data;

   ------------------
   -- Typed_Resize --
   ------------------

   procedure Typed_Resize
     (Area : access Gtk_Drawing_Area_Record'Class;
      Width, Height : Gint)
   is
   begin
      --  Validate identity after the callback returns, outside the C signal
      --  marshaller, so a failed check remains an ordinary test assertion.
      Typed_Resize_Called := Area /= null and then
        Get_Object (Area) = Get_Object (Expected_Signal_Area);
      Resize_Width := Width;
      Resize_Height := Height;
   end Typed_Resize;

   ----------------
   -- Slot_Resize --
   ----------------

   procedure Slot_Resize
     (Self : access GObject_Record'Class;
      Width, Height : Gint)
   is
   begin
      --  A slot connection passes the selected slot object as Self.
      Slot_Resize_Called := True;
      Slot_Resize_Object_Valid := Self /= null and then
        Get_Object (Self) = Get_Object (Expected_Signal_Slot);
      if Width /= Resize_Width or else Height /= Resize_Height then
         Slot_Resize_Object_Valid := False;
      end if;
   end Slot_Resize;

   -----------------------
   -- Raise_Typed_Resize --
   -----------------------

   procedure Raise_Typed_Resize
     (Area : access Gtk_Drawing_Area_Record'Class;
      Width, Height : Gint)
   is
      pragma Unreferenced (Area, Width, Height);
   begin
      raise Program_Error with "expected typed resize callback exception";
   end Raise_Typed_Resize;

   ---------------------
   -- Raise_Slot_Resize --
   ---------------------

   procedure Raise_Slot_Resize
     (Self : access GObject_Record'Class;
      Width, Height : Gint)
   is
      pragma Unreferenced (Self, Width, Height);
   begin
      raise Program_Error with "expected slot resize callback exception";
   end Raise_Slot_Resize;

   -----------------------
   -- Test_Content_Size --
   -----------------------

   procedure Test_Content_Size is
      Area : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
      Other : Gtk_Drawing_Area;
   begin
      --  Check Gtk_New and repeat initialization to cover its already-created
      --  path without replacing the widget returned by the first call.
      Gtk_New (Other);
      Other.Ref_Sink;
      Assert_Cmpint_Eq (Other.Get_Content_Width, 0);
      Gtk.Drawing_Area.Initialize (Other);
      Assert_Cmpint_Eq (Other.Get_Content_Height, 0);
      Other.Unref;

      Area.Set_Content_Width (123);
      Area.Set_Content_Height (45);

      Assert_Cmpint_Eq (Area.Get_Content_Width, 123);
      Assert_Cmpint_Eq (Area.Get_Content_Height, 45);
   end Test_Content_Size;

   --------------------
   -- Test_Draw_Func --
   --------------------

   procedure Test_Draw_Func is
      Window : constant Gtk_Window := Gtk_Window_New;
      Area   : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
      Id     : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      Area.Set_Content_Width (80);
      Area.Set_Content_Height (60);
      Area.Set_Draw_Func (Draw'Unrestricted_Access);

      Window.Set_Child (Area);
      Window.Present;

      Done := False;
      Id := Timeout_Add (3000, Stop'Unrestricted_Access);

      --  Stop as soon as the draw function has run; the timeout is only
      --  there so the test fails instead of hanging if it never does.
      while not Drawn and not Done loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;

      Assert_True (Drawn);

      --  The draw function is handed the size the widget was actually
      --  allocated, not the content size: the area is the window's only
      --  child, so it is stretched to fill it. The content size acts as a
      --  floor on that allocation.
      Assert_Cmpint_Eq (Drawn_Width, Area.Get_Width);
      Assert_Cmpint_Eq (Drawn_Height, Area.Get_Height);
      Assert_Cmpint_Ge (Drawn_Width, 80);
      Assert_Cmpint_Ge (Drawn_Height, 60);

      --  Emit resize with fixed dimensions so both signal adapters and their
      --  exception guards can be checked without relying on window timing.
      Expected_Signal_Area := Area;
      Expected_Signal_Slot := Window;
      Typed_Resize_Called := False;
      Slot_Resize_Called := False;
      Slot_Resize_Object_Valid := False;
      Caught_Exceptions := 0;
      Area.On_Resize (Typed_Resize'Unrestricted_Access);
      Area.On_Resize (Slot_Resize'Unrestricted_Access, Window);
      Area.On_Resize (Raise_Typed_Resize'Unrestricted_Access);
      Area.On_Resize (Raise_Slot_Resize'Unrestricted_Access, Window);
      declare
         Signal_Name : Gtkada.Types.Chars_Ptr :=
           Gtkada.Types.New_String ("resize");
      begin
         Emit_Resize
           (Area.Get_Object, Signal_Name, Interfaces.C.int (123),
            Interfaces.C.int (45));
         Gtkada.Types.Free (Signal_Name);
      end;

      --  The normal callbacks preserve object identity and dimensions, while
      --  the failing callbacks are contained and delivered to the hook.
      Assert_True (Typed_Resize_Called);
      Assert_True (Slot_Resize_Called);
      Assert_True (Slot_Resize_Object_Valid);
      Assert_Cmpint_Eq (Resize_Width, 123);
      Assert_Cmpint_Eq (Resize_Height, 45);
      Assert_Cmpuint_Eq (Guint (Caught_Exceptions), 2);

      --  Clear the direct callback and cancel its timeout after a successful
      --  draw. This covers GTK's null callback branch without leaving a timer.
      if not Done then
         Remove (Id);
      end if;
      Area.Set_Draw_Func (null);

      --  Redraw with the generic callback to verify its user data trampoline.
      Data_Draw_Called := False;
      Data_Draw_Marker := -1;
      Destroy_Count := 0;
      Destroyed_Marker := -1;
      Done := False;
      User_Data_Draw.Set_Draw_Func
        (Area, Draw_With_Data'Unrestricted_Access, (Marker => 42));
      Area.Queue_Draw;
      Id := Timeout_Add (3000, Stop'Unrestricted_Access);
      while not Data_Draw_Called and not Done loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;
      Assert_True (Data_Draw_Called);
      Assert_Cmpint_Eq (Data_Draw_Marker, 42);

      --  Removing the generic callback invokes its destroy notifier once.
      User_Data_Draw.Set_Draw_Func (Area, null, (Marker => 0));
      Assert_Cmpuint_Eq (Guint (Destroy_Count), 1);
      Assert_Cmpint_Eq (Destroyed_Marker, 42);
      if not Done then
         Remove (Id);
      end if;
      Window.Destroy;
      Expected_Signal_Area := null;
      Expected_Signal_Slot := null;
   end Test_Draw_Func;

   ------------------------
   -- Test_Accessibility --
   ------------------------

   procedure Test_Accessibility is
      Area : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
      Parent, Next, First : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
   begin
      Area.Ref_Sink;

      --  An unparented drawing area has no exposed children, screen bounds or
      --  focused platform state with the testsuite's accessibility backend off.
      Assert_True (Area.Get_Accessible_Role /= Accessible_Role_None);
      Assert_Cmpstr_Eq (Area.Get_Accessible_Id, "");
      Context := Area.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := Area.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Parent := Area.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Next := Area.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);
      First := Area.Get_First_Accessible_Child;
      Assert_True (First = Null_Gtk_Accessible);
      Assert_False
        (Area.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Exercise every accessible mutation wrapper with valid enum values,
      --  then check that null parent and sibling relationships remain null.
      Area.Announce ("DrawingArea accessibility test",
                    Accessible_Announcement_Priority_Low);
      Area.Reset_Property (Accessible_Property_Label);
      Area.Reset_Relation (Accessible_Relation_Labelled_By);
      Area.Reset_State (Accessible_State_Busy);
      Area.Update_Platform_State (Accessible_Platform_State_Focused);
      Area.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Area.Get_Accessible_Parent = Null_Gtk_Accessible);
      Area.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Area.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Area.Unref;
   end Test_Accessibility;

begin
   Glib.Test.Init;

   --  Widgets cannot be created before GTK is initialized.
   Gtk.Main.Init;
   Gtkada.Bindings.Set_On_Exception
     (Record_Callback_Exception'Unrestricted_Access);

   Glib.Test.Add_Func
     ("/drawing-area/content-size", Test_Content_Size'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/drawing-area/draw-func", Test_Draw_Func'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/drawing-area/accessibility", Test_Accessibility'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Drawing_Area;
