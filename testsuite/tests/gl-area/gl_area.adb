--  Verify GLArea configuration, accessible methods, and emitted GLArea signals.
--  Render and framebuffer operations need a real GL context, unavailable on
--  every headless display used by this testsuite.
with Ada.Command_Line;
with Gdk.GLContext; use Gdk.GLContext;
with Glib;         use Glib;
with Glib.Error;   use Glib.Error;
with Glib.Object;  use Glib.Object;
with Glib.Test;    use Glib.Test;
with Gtk.Accessible; use Gtk.Accessible;
with Gtk.GLArea;   use Gtk.GLArea;
with Gtk.Atcontext;
with Gtk.Main;
with Gtk.Window;   use Gtk.Window;
with Glib.Values;
with System;

procedure GL_Area is
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Glib.Signal_Id;

   Context_Called      : Boolean := False;
   Context_Slot_Called : Boolean := False;
   Resized             : Boolean := False;
   Resize_Slot_Called  : Boolean := False;
   Signal_Source       : System.Address := System.Null_Address;
   Create_Context_Id   : Glib.Signal_Id := Glib.Null_Signal_Id;
   Resize_Id           : Glib.Signal_Id := Glib.Null_Signal_Id;

   --  This introspection call verifies that the generated On_* wrappers
   --  attached handlers without emitting a GL-dependent signal.
   function Has_Handler_Pending
     (Object         : System.Address;
      Signal         : Glib.Signal_Id;
      Detail         : Glib.GQuark;
      May_Be_Blocked : Glib.Gboolean) return Glib.Gboolean;
   pragma Import (C, Has_Handler_Pending, "g_signal_has_handler_pending");

   --  Emit signals through GValue so GtkAda marshallers can be checked
   --  without asking this test display to create an OpenGL context.
   procedure Emitv
     (Instance_And_Params : System.Address;
      Id                  : Glib.Signal_Id;
      Detail              : Glib.GQuark;
      Return_Value        : System.Address);
   pragma Import (C, Emitv, "g_signal_emitv");

   --  The callbacks stop the GL-dependent class handlers on the test signals.
   procedure Stop_Emission
     (Instance : System.Address;
      Id       : Glib.Signal_Id;
      Detail   : Glib.GQuark);
   pragma Import (C, Stop_Emission, "g_signal_stop_emission");

   function Render
     (Area    : access Gtk_GLArea_Record'Class;
      Context : not null access Gdk_GLContext_Record'Class) return Boolean
   is
   begin
      Assert_True (Area.Get_Context = Gdk_GLContext (Context));
      --  Both methods are meaningful once the signal guarantees a current
      --  context and a realized framebuffer.
      Area.Make_Current;
      Area.Attach_Buffers;
      return True;
   end Render;

   procedure Resize
     (Area : access Gtk_GLArea_Record'Class; Width, Height : Gint)
   is
      pragma Unreferenced (Area);
   begin
      Assert_Cmpint_Eq (Width, 320);
      Assert_Cmpint_Eq (Height, 240);
      Resized := True;
   end Resize;

   function Create_Context
     (Area : access Gtk_GLArea_Record'Class) return Gdk_GLContext
   is
   begin
      Assert_True (Area /= null);
      Context_Called := True;
      Stop_Emission (Area.Get_Object, Create_Context_Id, 0);
      --  A null return normally selects GTK's context creation; the test
      --  stops the class handler so it can check marshalling without GL.
      return null;
   end Create_Context;

   function Create_Context_Slot
     (Self : access GObject_Record'Class) return Gdk_GLContext
   is
   begin
      Assert_True (Self /= null);
      Context_Slot_Called := True;
      Stop_Emission (Signal_Source, Create_Context_Id, 0);
      return null;
   end Create_Context_Slot;

   function Render_Slot
     (Self    : access GObject_Record'Class;
      Context : not null access Gdk_GLContext_Record'Class) return Boolean
   is
   begin
      Assert_True (Self /= null);
      Assert_True (Context /= null);
      return True;
   end Render_Slot;

   procedure Resize_Slot
     (Self : access GObject_Record'Class; Width, Height : Gint)
   is
   begin
      Assert_True (Self /= null);
      Assert_Cmpint_Eq (Width, 320);
      Assert_Cmpint_Eq (Height, 240);
      Resize_Slot_Called := True;
      Stop_Emission (Signal_Source, Resize_Id, 0);
   end Resize_Slot;

   --  Stop the signal's class handler inside each emission; it would create
   --  a real GL context or issue GL commands during widget realization.
   procedure Emit_Create_Context (Area : access Gtk_GLArea_Record'Class) is
      Params : Glib.Values.GValue_Array (0 .. 0);
      Result : Glib.Values.GValue;
   begin
      Glib.Values.Init (Params (0), Gtk.GLArea.Get_Type);
      Glib.Values.Set_Object (Params (0), Area);
      Glib.Values.Init (Result, Gdk.GLContext.Get_Type);
      Signal_Source := Area.Get_Object;
      Emitv (Params'Address, Create_Context_Id, 0, Result'Address);
      Signal_Source := System.Null_Address;
      Assert_True (Glib.Values.Get_Object (Result) = null);
      Glib.Values.Unset (Result);
      Glib.Values.Unset (Params (0));
   end Emit_Create_Context;

   --  The slot handler suppresses GTK's GL viewport setup after both Ada
   --  handlers have checked their decoded width and height.
   procedure Emit_Resize
     (Area : access Gtk_GLArea_Record'Class; Width, Height : Gint)
   is
      Params : Glib.Values.GValue_Array (0 .. 2);
   begin
      Glib.Values.Init (Params (0), Gtk.GLArea.Get_Type);
      Glib.Values.Set_Object (Params (0), Area);
      Glib.Values.Init_Set_Int (Params (1), Width);
      Glib.Values.Init_Set_Int (Params (2), Height);
      Signal_Source := Area.Get_Object;
      Emitv (Params'Address, Resize_Id, 0, System.Null_Address);
      Signal_Source := System.Null_Address;
      Glib.Values.Unset (Params (0));
      Glib.Values.Unset (Params (1));
      Glib.Values.Unset (Params (2));
   end Emit_Resize;

   procedure Test_Configuration with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Create_Context with Convention => C;
   procedure Test_Render with Convention => C;

   procedure Test_Configuration is
      Area : constant Gtk_GLArea := Gtk_GLArea_New;
      Other : Gtk_GLArea;
      Major, Minor : Gint;
      Error : constant GError := Error_New
        (Quark_From_String ("gtkada-gl-area-test"), 42, "GLArea test error");
   begin
      Area.Ref_Sink;
      --  Both public constructors initialize the same GTK widget type.
      Gtk_New (Other);
      Other.Ref_Sink;
      Gtk.GLArea.Initialize (Other);
      Assert_True (Other.Get_Auto_Render);
      Assert_False (Other.Get_Has_Depth_Buffer);
      Other.Unref;

      --  GL API selection and version requirements are readable before
      --  realization, so these checks need no hardware context.
      Area.Set_Allowed_Apis (Gdk_Gl_Api_Gl + Gdk_Gl_Api_Gles);
      Assert_True
        (Area.Get_Allowed_Apis = Gdk_Gl_Api_Gl + Gdk_Gl_Api_Gles);
      Assert_Cmpuint_Eq (Guint (Area.Get_Api), 0);
      Assert_True (Area.Get_Context = null);
      Area.Set_Required_Version (3, 2);
      Area.Get_Required_Version (Major, Minor);
      Assert_Cmpint_Eq (Major, 3);
      Assert_Cmpint_Eq (Minor, 2);
      Area.Set_Has_Depth_Buffer (True);
      Assert_True (Area.Get_Has_Depth_Buffer);
      Area.Set_Has_Stencil_Buffer (True);
      Assert_True (Area.Get_Has_Stencil_Buffer);
      Area.Set_Has_Stencil_Buffer (False);
      Assert_False (Area.Get_Has_Stencil_Buffer);
      Area.Set_Auto_Render (False);
      Assert_False (Area.Get_Auto_Render);
      --  These accessors are deprecated in GTK 4.12, but remain part of the
      --  binding and can be checked before the widget creates a context.
      Area.Set_Use_Es (True);
      Assert_True (Area.Get_Use_Es);
      Area.Set_Use_Es (False);
      Assert_False (Area.Get_Use_Es);
      Area.Set_Error (Error);
      Error_Free (Error);
      --  The area copies the error; Get_Error returns a borrowed reference.
      Assert_Cmpstr_Eq (Get_Message (Area.Get_Error), "GLArea test error");
      Area.Set_Error (null);
      Assert_True (Area.Get_Error = null);
      --  Queue_Render is safe before realization and requests a later draw.
      Area.Queue_Render;
      Assert_True (Area.Get_Error = null);
      Area.Unref;
   end Test_Configuration;

   --  GtkGLArea is an accessible leaf when unparented. The null tree links
   --  and missing bounds are stable without an accessibility backend.
   procedure Test_Accessibility is
      Area : constant Gtk_GLArea := Gtk_GLArea_New;
      Parent, Next, First : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
   begin
      Area.Ref_Sink;
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

      --  Exercise the accessible interface's mutation wrappers with valid
      --  enum values, then verify that detached links remain null.
      Area.Announce ("GLArea accessibility test", Accessible_Announcement_Priority_Low);
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

   --  Exercise the create-context return-value marshallers by emitting the
   --  signal on two unrealized areas and stopping the GL-dependent default.
   procedure Test_Create_Context is
      Area      : constant Gtk_GLArea := Gtk_GLArea_New;
      Slot_Area : constant Gtk_GLArea := Gtk_GLArea_New;
      Realized_Area : constant Gtk_GLArea := Gtk_GLArea_New;
      Slot      : constant Gtk_Window := Gtk_Window_New;
      Window    : constant Gtk_Window := Gtk_Window_New;
   begin
      Context_Called := False;
      Context_Slot_Called := False;
      Area.Ref_Sink;
      Slot_Area.Ref_Sink;
      Slot.Ref_Sink;
      Create_Context_Id :=
        Glib.Object.Lookup
          (Gtk.GLArea.Get_Type, Signal_Create_Context & ASCII.NUL);
      Assert_True (Create_Context_Id /= Glib.Null_Signal_Id);
      Assert_False
        (Has_Handler_Pending (Area.Get_Object, Create_Context_Id, 0, 0) /= 0);
      Assert_False
        (Has_Handler_Pending
           (Slot_Area.Get_Object, Create_Context_Id, 0, 0) /= 0);
      Area.On_Create_Context (Create_Context'Unrestricted_Access);
      Slot_Area.On_Create_Context
        (Create_Context_Slot'Unrestricted_Access, Slot);
      Realized_Area.On_Create_Context
        (Create_Context'Unrestricted_Access);
      Assert_True
        (Has_Handler_Pending (Area.Get_Object, Create_Context_Id, 0, 0) /= 0);
      Assert_True
        (Has_Handler_Pending
           (Slot_Area.Get_Object, Create_Context_Id, 0, 0) /= 0);
      Emit_Create_Context (Area);
      Emit_Create_Context (Slot_Area);
      Assert_True (Context_Called);
      Assert_True (Context_Slot_Called);
      Assert_True (Area.Get_Context = null);
      Assert_True (Area.Get_Error = null);
      --  During normal realization this callback stops GTK's context creator.
      --  The realized area then has no context, so these methods take their
      --  documented no-context paths without making any OpenGL calls.
      Window.Set_Child (Realized_Area);
      Window.Present;
      Assert_True (Realized_Area.Get_Realized);
      Assert_True (Realized_Area.Get_Context = null);
      Assert_True (Realized_Area.Get_Error /= null);
      Realized_Area.Make_Current;
      Realized_Area.Attach_Buffers;
      Assert_True (Realized_Area.Get_Error /= null);
      Window.Destroy;
      Slot.Unref;
      Slot_Area.Unref;
      Area.Unref;
   end Test_Create_Context;

   --  Both resize callbacks decode the supplied dimensions. The render
   --  wrappers are registered, but emitting render requires a real GL context.
   procedure Test_Render is
      Area   : constant Gtk_GLArea := Gtk_GLArea_New;
      Slot   : constant Gtk_Window := Gtk_Window_New;
      Render_Id : constant Glib.Signal_Id :=
        Glib.Object.Lookup (Gtk.GLArea.Get_Type, Signal_Render & ASCII.NUL);
      Local_Resize_Id : constant Glib.Signal_Id :=
        Glib.Object.Lookup (Gtk.GLArea.Get_Type, Signal_Resize & ASCII.NUL);
   begin
      Resized := False;
      Resize_Slot_Called := False;
      Area.Ref_Sink;
      Slot.Ref_Sink;
      Assert_True (Render_Id /= Glib.Null_Signal_Id);
      Assert_True (Local_Resize_Id /= Glib.Null_Signal_Id);
      Assert_False
        (Has_Handler_Pending (Area.Get_Object, Render_Id, 0, 0) /= 0);
      Assert_False
        (Has_Handler_Pending (Area.Get_Object, Local_Resize_Id, 0, 0) /= 0);
      Area.On_Render (Render'Unrestricted_Access);
      Assert_True
        (Has_Handler_Pending (Area.Get_Object, Render_Id, 0, 0) /= 0);
      Area.On_Render (Render_Slot'Unrestricted_Access, Slot);
      Area.On_Resize (Resize'Unrestricted_Access);
      Assert_True
        (Has_Handler_Pending (Area.Get_Object, Local_Resize_Id, 0, 0) /= 0);
      Area.On_Resize (Resize_Slot'Unrestricted_Access, Slot);
      Assert_True
        (Has_Handler_Pending (Area.Get_Object, Local_Resize_Id, 0, 0) /= 0);
      Resize_Id := Local_Resize_Id;
      --  Resize_Slot stops the default viewport setup after both handlers
      --  validate 320x240; this keeps the test independent of OpenGL support.
      Emit_Resize (Area, 320, 240);
      Assert_True (Resized);
      Assert_True (Resize_Slot_Called);
      Assert_True (Area.Get_Error = null);
      Slot.Unref;
      Area.Unref;
   end Test_Render;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/gl-area/configuration", Test_Configuration'Unrestricted_Access);
   Add_Func ("/gl-area/accessibility", Test_Accessibility'Unrestricted_Access);
   Add_Func ("/gl-area/create-context", Test_Create_Context'Unrestricted_Access);
   Add_Func ("/gl-area/render", Test_Render'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end GL_Area;
