--  Verify GLArea configuration, borrowed errors, and the render/resize
--  callbacks with a real context when the display supports OpenGL.
with Ada.Command_Line;
with Gdk.GLContext; use Gdk.GLContext;
with Glib;         use Glib;
with Glib.Error;   use Glib.Error;
with Glib.Main;    use Glib.Main;
with Glib.Test;    use Glib.Test;
with Gtk.GLArea;   use Gtk.GLArea;
with Gtk.Main;
with Gtk.Window;   use Gtk.Window;

procedure GL_Area is
   Rendered : Boolean := False;
   Resized  : Boolean := False;
   Expired  : Boolean := False;

   function Stop return Boolean is
   begin
      Expired := True;
      return False;
   end Stop;

   function Render
     (Area    : access Gtk_GLArea_Record'Class;
      Context : not null access Gdk_GLContext_Record'Class) return Boolean
   is
   begin
      Assert_True (Area.Get_Context = Gdk_GLContext (Context));
      Rendered := True;
      return True;
   end Render;

   procedure Resize
     (Area : access Gtk_GLArea_Record'Class; Width, Height : Gint)
   is
      pragma Unreferenced (Area);
   begin
      Assert_Cmpint_Gt (Width, 0);
      Assert_Cmpint_Gt (Height, 0);
      Resized := True;
   end Resize;

   procedure Test_Configuration with Convention => C;
   procedure Test_Render with Convention => C;

   procedure Test_Configuration is
      Area : constant Gtk_GLArea := Gtk_GLArea_New;
      Major, Minor : Gint;
      Error : constant GError := Error_New
        (Quark_From_String ("gtkada-gl-area-test"), 42, "GLArea test error");
   begin
      Area.Ref_Sink;
      Area.Set_Required_Version (3, 2);
      Area.Get_Required_Version (Major, Minor);
      Assert_Cmpint_Eq (Major, 3);
      Assert_Cmpint_Eq (Minor, 2);
      Area.Set_Has_Depth_Buffer (True);
      Assert_True (Area.Get_Has_Depth_Buffer);
      Area.Set_Auto_Render (False);
      Assert_False (Area.Get_Auto_Render);
      Area.Set_Error (Error);
      Error_Free (Error);
      --  The area copies the error; Get_Error returns a borrowed reference.
      Assert_Cmpstr_Eq (Get_Message (Area.Get_Error), "GLArea test error");
      Area.Set_Error (null);
      Assert_True (Area.Get_Error = null);
      Area.Unref;
   end Test_Configuration;

   procedure Test_Render is
      Window : constant Gtk_Window := Gtk_Window_New;
      Area   : constant Gtk_GLArea := Gtk_GLArea_New;
      Timer  : G_Source_Id;
   begin
      Rendered := False;
      Resized := False;
      Expired := False;
      Area.Set_Auto_Render (False);
      Area.Set_Required_Version (3, 2);
      Area.Set_Size_Request (80, 60);
      Area.On_Render (Render'Unrestricted_Access);
      Area.On_Resize (Resize'Unrestricted_Access);
      Window.Set_Child (Area);
      Window.Present;
      if Area.Get_Error /= null then
         Skip (Get_Message (Area.Get_Error));
         Window.Destroy;
         return;
      end if;
      Timer := Timeout_Add (3000, Stop'Unrestricted_Access);
      while not Rendered and not Expired loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;
      Assert_True (Rendered);
      --  With automatic rendering disabled, changing the scene must still
      --  be able to request a fresh render through Queue_Render.
      Rendered := False;
      Area.Queue_Render;
      while not Rendered and not Expired loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;
      if not Expired then
         Remove (Timer);
      end if;
      Assert_True (Rendered);
      Assert_True (Resized);
      Window.Destroy;
   end Test_Render;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/gl-area/configuration", Test_Configuration'Unrestricted_Access);
   Add_Func ("/gl-area/render", Test_Render'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end GL_Area;
