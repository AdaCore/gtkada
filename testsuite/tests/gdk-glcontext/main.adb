--  Exercise Gdk.GLContext and its inherited Gdk.Draw_Context methods through
--  the test driver's display. No C test in testsuite/c_tests/
--  calls the gdk_gl_context_* API, so this is a native Ada test of the
--  documented context configuration, frame state and current-context API.

with Ada.Command_Line;
with Cairo.Region;
with Gdk;
with Gdk.Draw_Context;
with Gdk.Display;
with Gdk.GLContext;
with Gdk.Surface;
with Glib;
with Glib.Error;
with Glib.Messages;
with Glib.Object;
with Glib.Test;
with Gtk.Main;

procedure Main is
   use Glib.Test;
   use type Cairo.Region.Cairo_Region;
   use type Glib.Messages.Log_Level_Flags;
   use type Gdk.Gdk_Display;
   use type Gdk.GLContext.Gdk_GLAPI;
   use type Gdk.GLContext.Gdk_GLContext;
   use type Gdk.Gdk_Surface;
   use type Glib.Error.GError;

   --  A specific parent access type selects DrawContext wrappers statically;
   --  class-wide dispatch would choose GLContext's similarly named methods.
   type Static_Draw_Context is access all
     Gdk.Draw_Context.Gdk_Draw_Context_Record;

   procedure Test_Context with Convention => C;

   --  Create a backend context and check every configurable property before
   --  realization; realized-only queries are guarded by Realize's result.
   procedure Test_Context is
      Display : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;
      --  A bare GDK surface avoids Gtk's renderer claiming a Cairo context
      --  first, which would make switching it to a GL draw context a warning.
      Surface : constant Gdk.Gdk_Surface :=
        Gdk.Surface.Gdk_Surface_New_Toplevel (Display);
      Context : Gdk.GLContext.Gdk_GLContext;
      Frame_Rectangle : aliased Cairo.Region.Cairo_Rectangle_Int :=
        (X => 0, Y => 0, Width => 1, Height => 1);
      Frame_Region : constant Cairo.Region.Cairo_Region :=
        Cairo.Region.Create_Rectangle (Frame_Rectangle'Access);
      Error   : Glib.Error.GError;
      Major   : Glib.Gint;
      Minor   : Glib.Gint;
      Realized : Boolean;
      Legacy   : Boolean;
      Previous_Fatal_Mask : Glib.Messages.Log_Level_Flags;
   begin
      --  No current context is required to query or clear the current slot.
      Assert_True (Gdk.GLContext.Get_Current = null);
      Gdk.GLContext.Clear_Current;
      Assert_True (Gdk.GLContext.Get_Current = null);

      Assert_True (Surface /= null);
      Assert_True (Surface.Get_Display = Display);

      --  Context creation is optional on a backend without GL support.
      --  The returned context is transfer-full and released below.
      Context := Surface.Create_Gl_Context (Error);
      if Context = null then
         Assert_True (Error /= null);
         Glib.Error.Error_Free (Error);
         Surface.Destroy;
         Cairo.Region.Destroy (Frame_Region);
         return;
      end if;
      Assert_True (Error = null);
      --  GLContext inherits DrawContext's display and surface accessors. These
      --  return the same objects used to create the context; neither getter
      --  transfers a reference.
      --  The GLContext package also binds similarly named accessors, so
      --  qualify these calls to exercise the inherited DrawContext wrappers.
      Assert_True
        (Gdk.Draw_Context.Get_Display
           (Static_Draw_Context (Context)) = Display);
      Assert_True
        (Gdk.Draw_Context.Get_Surface
           (Static_Draw_Context (Context)) = Surface);
      --  Outside a frame, the deprecated frame queries have documented empty
      --  results. Check them before realization, when no frame can be active.
      Assert_False (Context.Is_In_Frame);
      Assert_True
        (Context.Get_Frame_Region = Cairo.Region.Null_Region);
      --  This deprecated accessor is specified to return null unconditionally.
      Assert_True (Context.Get_Shared_Context = null);

      Context.Set_Allowed_Apis
        (Gdk.GLContext.Gdk_Gl_Api_Gl + Gdk.GLContext.Gdk_Gl_Api_Gles);
      Assert_True
        (Context.Get_Allowed_Apis =
           Gdk.GLContext.Gdk_Gl_Api_Gl + Gdk.GLContext.Gdk_Gl_Api_Gles);
      Assert_True (Context.Get_Api = 0);

      Context.Set_Debug_Enabled (True);
      Assert_True (Context.Get_Debug_Enabled);
      Context.Set_Debug_Enabled (False);
      Assert_False (Context.Get_Debug_Enabled);

      Context.Set_Forward_Compatible (True);
      Assert_True (Context.Get_Forward_Compatible);
      Context.Set_Forward_Compatible (False);
      Assert_False (Context.Get_Forward_Compatible);

      Context.Set_Required_Version (3, 2);
      Context.Get_Required_Version (Major, Minor);
      Assert_Cmpint_Eq (Major, 3);
      Assert_Cmpint_Eq (Minor, 2);

      --  Zero requests the ordinary desktop profile and does not require ES.
      Context.Set_Use_Es (0);
      Assert_False (Context.Get_Use_Es);

      --  GL is optional under Xvfb. If realization succeeds, verify the
      --  realized-only version/profile methods and make/clear-current pair.
      Realized := Context.Realize (Error);
      if Realized then
         Assert_True (Error = null);
         Context.Get_Version (Major, Minor);
         Assert_Cmpint_Gt (Major, 0);
         Assert_Cmpint_Ge (Minor, 0);
         Assert_True
           (Context.Get_Api = Gdk.GLContext.Gdk_Gl_Api_Gl or else
            Context.Get_Api = Gdk.GLContext.Gdk_Gl_Api_Gles);
         Assert_True (Context.Get_Use_Es =
                        (Context.Get_Api = Gdk.GLContext.Gdk_Gl_Api_Gles));
         --  Both predicates are defined for a realized context; a context
         --  always shares its own GL resources with itself.
         Legacy := Context.Is_Legacy;
         Assert_True (Legacy = Context.Is_Legacy);
         Assert_True (Context.Is_Shared (Context));
         --  GDK emits a warning when this deprecated API attaches the GL
         --  context to a bare surface. Temporarily make warnings non-fatal so
         --  the test can check the state transition and release that context.
         Previous_Fatal_Mask := Glib.Messages.Log_Set_Always_Fatal
           (Glib.Messages.Log_Flag_Recursion or Glib.Messages.Log_Level_Error);
         Gdk.Draw_Context.Begin_Frame
           (Static_Draw_Context (Context), Frame_Region);
         Assert_True (Context.Is_In_Frame);
         Gdk.Draw_Context.End_Frame (Static_Draw_Context (Context));
         Assert_False (Context.Is_In_Frame);
         Assert_True
           (Context.Get_Frame_Region = Cairo.Region.Null_Region);

         Context.Make_Current;
         Assert_True (Gdk.GLContext.Get_Current = Context);
         Gdk.GLContext.Clear_Current;
         Assert_True (Gdk.GLContext.Get_Current = null);
      else
         Assert_True (Error /= null);
         Glib.Error.Error_Free (Error);
      end if;

      Glib.Object.Unref (Glib.Object.GObject (Context));
      --  Releasing the manually attached context also produces a GDK warning
      --  while GDK detaches it, so restore fatal-warning handling afterward.
      if Realized then
         Previous_Fatal_Mask := Glib.Messages.Log_Set_Always_Fatal
           (Previous_Fatal_Mask);
      end if;
      Surface.Destroy;
      Cairo.Region.Destroy (Frame_Region);
   end Test_Context;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/gdk/glcontext/methods", Test_Context'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
