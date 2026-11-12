--  Exercise the GdkGLTexture paintable bindings without requiring an OpenGL
--  context. This is an Ada-only test; testsuite/c_tests has no GL texture
--  case to port. Construct-only dimensions let the inherited paintable API
--  run on a bare GObject while avoiding methods that download GL resources.

with Gdk.GLTexture;
with Gdk.Color_State;
with Gdk.GLContext;
with Gdk.Paintable;
with Gsk.Render_Node;
with Glib;
with Glib.Loadable_Icon;
with Glib.Object;
with Glib.Test;
with Glib.Types;
with Glib.Values;
with Gtk.Snapshot;
with Gtkada.Types;
with System;

package body GLTexture_Tests is
   use Glib;
   use Glib.Test;
   use type Gdk.GLTexture.Gdk_Gltexture;
   use type Gdk.Paintable.Gdk_Paintable;
   use type Gdk.Paintable.Gdk_Paintable_Flags;
   use type Gsk.Render_Node.Gsk_Render_Node;

   --  GdkColorState is boxed, so GValue needs the C object pointer rather
   --  than the Ada tagged wrapper when setting GdkTexture's construct value.
   function C_Get_Srgb return System.Address;
   pragma Import (C, C_Get_Srgb, "gdk_color_state_get_srgb");

   --  The base GdkTexture width and height properties are construct-only.
   --  Setting them here gives the paintable interface useful dimensions, but
   --  deliberately leaves the GL context and texture ID unset.
   function New_Paintable_Texture return Gdk.GLTexture.Gdk_Gltexture is
      Result : constant Gdk.GLTexture.Gdk_Gltexture :=
        new Gdk.GLTexture.Gdk_Gltexture_Record;
      Params : Glib.Values.GParameter_Array (1 .. 3);
   begin
      Params (1).Name := Gtkada.Types.New_String ("width");
      Glib.Values.Init (Params (1).Value, GType_Int);
      Glib.Values.Set_Int (Params (1).Value, 30);
      Params (2).Name := Gtkada.Types.New_String ("height");
      Glib.Values.Init (Params (2).Value, GType_Int);
      Glib.Values.Set_Int (Params (2).Value, 20);
      Params (3).Name := Gtkada.Types.New_String ("color-state");
      Glib.Values.Init (Params (3).Value, Gdk.Color_State.Get_Type);
      Glib.Values.Set_Boxed (Params (3).Value, C_Get_Srgb);
      Glib.Values.G_New (Result, Gdk.GLTexture.Get_Type, Params);
      Glib.Values.Free (Params);
      return Result;
   end New_Paintable_Texture;

   --  Check the GdkPaintable methods implemented by GdkTexture, plus both
   --  interface conversions exposed by Gdk.GLTexture.
   procedure Test_Paintable is
      Texture   : constant Gdk.GLTexture.Gdk_Gltexture :=
        New_Paintable_Texture;
      Paintable : constant Gdk.Paintable.Gdk_Paintable :=
        Gdk.GLTexture."+" (Texture);
      Icon      : constant Glib.Loadable_Icon.Gloadable_Icon :=
        Gdk.GLTexture."+" (Texture);
      Context   : aliased Gdk.GLContext.Gdk_GLContext_Record;
      Current   : Gdk.Paintable.Gdk_Paintable;
      Width, Height : Gdouble;
      Snapshot  : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Node      : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Assert_True (Glib.Object.Is_Created (Texture.all));
      Assert_True
        (Glib.Object.Get_Type (Glib.Object.GObject (Texture))
           = Gdk.GLTexture.Get_Type);
      Assert_True (Gdk.GLTexture."-" (Paintable) = Texture);
      Assert_True (Gdk.GLTexture."-" (Icon) = Texture);

      --  Initialize is documented as a no-op for an existing object. Its
      --  already-created branch does not inspect the required context value.
      Gdk.GLTexture.Initialize
        (Texture, Context'Access, 0, 1, 1, null, System.Null_Address);
      Assert_True (Texture.Get_Intrinsic_Width = 30);
      Assert_True (Texture.Get_Intrinsic_Height = 20);

      --  The texture class advertises fixed size and contents; its aspect
      --  ratio and dimensions come from the construct-only base properties.
      Assert_True
        (Texture.Get_Flags = Gdk.Paintable.Gdk_Paintable_Static_Size
           + Gdk.Paintable.Gdk_Paintable_Static_Contents);
      Assert_True (Texture.Get_Intrinsic_Width = 30);
      Assert_True (Texture.Get_Intrinsic_Height = 20);
      Assert_True (Texture.Get_Intrinsic_Aspect_Ratio = 1.5);
      Current := Texture.Get_Current_Image;
      Assert_True (Gdk.GLTexture."-" (Current) = Texture);

      --  With only width specified, concrete sizing preserves the texture's
      --  3:2 aspect ratio and returns 30 by 20.
      Texture.Compute_Concrete_Size (30.0, 0.0, 12.0, 8.0, Width, Height);
      Assert_True (Width = 30.0 and Height = 20.0);

      --  Snapshotting builds a render node without reading the GL texture's
      --  pixel data, so this path remains usable on the headless test display.
      Texture.Snapshot (Snapshot, 30.0, 20.0);
      Node := Snapshot.To_Node;
      Assert_True (Node /= null);

      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (Current)));
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
      Glib.Object.Unref (Glib.Object.GObject (Texture));
   end Test_Paintable;

end GLTexture_Tests;
