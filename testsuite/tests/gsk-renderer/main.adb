--  Exercise Gsk.Renderer on a toplevel surface using the Cairo renderer.
--  The renderer setup, texture render, and unrealize follow the relevant
--  calls in testsuite/c_tests/symbolic.c:test_symbolic; a solid color node
--  keeps this focused test independent of image files and icon themes.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Cairo.Region;
with Gdk;
with Gdk.Display;
with Gdk.RGBA;
with Gdk.Surface;
with Gdk.Texture;
with Glib;
with Glib.Error;
with Glib.Main;
with Glib.Object;
with Glib.Test;
with Graphene.Rect;
with Gsk.Color_Node;
with Gsk.Render_Node;
with Gsk.Renderer;
with Gtk.Main;
with Gtk.Window;
with Interfaces.C;

procedure Main is
   use Glib.Test;
   use type Gdk.Gdk_Surface;
   use type Gdk.Texture.Gdk_Texture;
   use type Glib.Error.GError;
   use type Glib.Gint;
   use type Gsk.Renderer.Gsk_Renderer;

   --  Render nodes are controlled Ada wrappers around GSK references. Freeing
   --  the wrapper runs Finalize and releases the render node reference.
   procedure Free_Render_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Surface_Rendering with Convention => C;

   --------------------------
   -- Test_Surface_Rendering --
   --------------------------

   procedure Test_Surface_Rendering is
      Display : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;
      Surface : constant Gdk.Gdk_Surface :=
        Gdk.Surface.Gdk_Surface_New_Toplevel (Display);
      Window : constant Gtk.Window.Gtk_Window := Gtk.Window.Gtk_Window_New;
      Renderer : Gsk.Renderer.Gsk_Renderer;
      Second_Renderer : Gsk.Renderer.Gsk_Renderer;
      Window_Renderer : Gsk.Renderer.Gsk_Renderer;
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.8, 1.0);
      Bounds : aliased Graphene.Rect.Graphene_Rect_T;
      Viewport : aliased Graphene.Rect.Graphene_Rect_T;
      Bounds_Ref : constant access Graphene.Rect.Graphene_Rect_T :=
        Graphene.Rect.Init (Bounds'Access, 0.0, 0.0, 8.0, 8.0);
      Viewport_Ref : constant access Graphene.Rect.Graphene_Rect_T :=
        Graphene.Rect.Init (Viewport'Access, 0.0, 0.0, 8.0, 8.0);
      Root : Gsk.Render_Node.Gsk_Render_Node;
      Texture : Gdk.Texture.Gdk_Texture;
      Rectangle : aliased Cairo.Region.Cairo_Rectangle_Int :=
        (X => 0, Y => 0, Width => 8, Height => 8);
      Region : constant Cairo.Region.Cairo_Region :=
        Cairo.Region.Create_Rectangle (Rectangle'Access);
      Error : Glib.Error.GError;
      Realized : Boolean;
      pragma Unreferenced (Bounds_Ref, Viewport_Ref);
   begin
      Assert_True (Surface /= null);

      --  The function constructor realizes its renderer for the supplied
      --  surface, which can be read back through both renderer queries.
      Renderer := Gsk.Renderer.Gsk_Renderer_New_For_Surface (Surface);
      Assert_True (Renderer /= null);
      Assert_True (Renderer.Is_Realized);
      Assert_True (Renderer.Get_Surface = Surface);

      --  Initialize_For_Surface is documented as a no-op on an existing
      --  renderer; this second call must preserve its original association.
      Gsk.Renderer.Initialize_For_Surface (Renderer, Surface);
      Assert_True (Renderer.Is_Realized);
      Assert_True (Renderer.Get_Surface = Surface);

      --  Build a small deterministic scene, render it to a texture, and
      --  check the result's dimensions as the C symbolic test does.
      Root := Gsk.Render_Node.Gsk_Render_Node
        (Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access));
      Texture := Renderer.Render_Texture (Root, Viewport'Access);
      Assert_True (Texture /= null);
      Assert_True (Texture.Get_Width = 8 and Texture.Get_Height = 8);

      --  Gdk toplevels created directly are suitable for render_texture but
      --  need a GTK frame before surface rendering. Present a small window and
      --  let GTK create its renderer before exercising Render with a region.
      Window.Set_Default_Size (8, 8);
      Window.Present;
      for Attempt in 1 .. 20 loop
         exit when Window.Get_Renderer /= null;
         declare
            Did_Work : constant Boolean :=
              Glib.Main.Main_Context_Iteration (null, May_Block => False);
            pragma Unreferenced (Did_Work);
         begin
            null;
         end;
      end loop;
      Window_Renderer := Window.Get_Renderer;
      Assert_True (Window_Renderer /= null);
      Window_Renderer.Render (Root, Region);
      Assert_True (Window_Renderer.Is_Realized);
      Window.Destroy;

      --  The procedure constructor has the same documented realized state and
      --  surface association as its function counterpart.
      Gsk.Renderer.Gtk_New_For_Surface (Second_Renderer, Surface);
      Assert_True (Second_Renderer.Is_Realized);
      Assert_True (Second_Renderer.Get_Surface = Surface);
      Second_Renderer.Unrealize;
      Assert_False (Second_Renderer.Is_Realized);

      --  Unrealize permits explicit realization on either a surface or a
      --  display. Check both success results and the documented empty errors.
      Renderer.Unrealize;
      Assert_False (Renderer.Is_Realized);
      Realized := Renderer.Realize (Surface, Error);
      Assert_True (Realized);
      Assert_True (Error = null);
      Assert_True (Renderer.Is_Realized);

      Renderer.Unrealize;
      Assert_False (Renderer.Is_Realized);
      Realized := Renderer.Realize_For_Display (Display, Error);
      Assert_True (Realized);
      Assert_True (Error = null);
      Assert_True (Renderer.Is_Realized);
      Renderer.Unrealize;
      Assert_False (Renderer.Is_Realized);

      --  Release caller-owned objects after the renderer has stopped using
      --  the surface and immutable render-node tree.
      Free_Render_Node (Root);
      Glib.Object.Unref (Glib.Object.GObject (Texture));
      Glib.Object.Unref (Glib.Object.GObject (Renderer));
      Glib.Object.Unref (Glib.Object.GObject (Second_Renderer));
      Cairo.Region.Destroy (Region);
      Surface.Destroy;
   end Test_Surface_Rendering;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/gsk-renderer/surface-rendering", Test_Surface_Rendering'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
