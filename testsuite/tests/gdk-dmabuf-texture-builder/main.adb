--  Exercise the Gdk.Dmabuf_Texture_Builder constructors and property methods.
--  No matching C testsuite case covers this builder. Build needs an actual
--  dma-buf and a graphics backend, neither of which the headless runner has.

with Ada.Command_Line;
with Cairo.Region;
with Gdk.Color_State;
with Gdk.Display;
with Gdk.Dmabuf_Texture_Builder;
with Gdk.Memory_Texture;
with Gdk.Texture;
with Glib;
with Glib.Bytes;
with Glib.Object;
with Glib.Test;
with Gtk.Main;

procedure Main is
   use Glib;
   use Glib.Test;
   use type Cairo.Region.Cairo_Region;
   use type Gdk.Dmabuf_Texture_Builder.Gdk_Dmabuf_Texture_Builder;
   use type Gdk.Display.Gdk_Display;
   use type Gdk.Texture.Gdk_Texture;

   procedure Test_Builder with Convention => C;

   --  Check both constructor forms and round-trip each settable property.
   procedure Test_Builder is
      Builder : Gdk.Dmabuf_Texture_Builder.Gdk_Dmabuf_Texture_Builder;
      By_Procedure : Gdk.Dmabuf_Texture_Builder.Gdk_Dmabuf_Texture_Builder;
      Display : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;
      Color : constant Gdk.Color_State.Gdk_Color_State :=
        Gdk.Color_State.Get_Srgb;
      Pixel_Bytes : constant Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
        ((16#11#, 16#22#, 16#33#, 16#FF#), 4);
      Update_Texture : constant Gdk.Memory_Texture.Gdk_Memory_Texture :=
        Gdk.Memory_Texture.Gdk_Memory_Texture_New
          (1, 1, Gdk.Texture.Gdk_Memory_R8G8B8A8, Pixel_Bytes, 4);
      Rect : aliased Cairo.Region.Cairo_Rectangle_Int :=
        (X => 2, Y => 3, Width => 4, Height => 5);
      Region : constant Cairo.Region.Cairo_Region :=
        Cairo.Region.Create_Rectangle (Rect'Access);
   begin
      --  The function constructor initializes a new wrapper; repeated
      --  Initialize must leave that existing GObject intact.
      Builder :=
        Gdk.Dmabuf_Texture_Builder.Gdk_Dmabuf_Texture_Builder_New;
      Assert_True (Glib.Object.Is_Created (Builder.all));
      Gdk.Dmabuf_Texture_Builder.Initialize (Builder);

      --  The procedure constructor is a separate generated API path.
      Gdk.Dmabuf_Texture_Builder.Gdk_New (By_Procedure);
      Assert_True (Glib.Object.Is_Created (By_Procedure.all));

      --  Display and color-state properties preserve the associated objects.
      Assert_True (Builder.Get_Display /= null);
      Builder.Set_Display (Display);
      Assert_True (Builder.Get_Display = Display);
      Assert_True (Glib.Is_Null (Builder.Get_Color_State));
      Builder.Set_Color_State (Color);
      Assert_True (Gdk.Color_State.Equal (Builder.Get_Color_State, Color));
      Builder.Set_Color_State (Gdk.Color_State.Null_Gdk_Color_State);
      Assert_True (Glib.Is_Null (Builder.Get_Color_State));

      --  Scalar properties report their documented unset values, then retain
      --  values written through the Ada wrappers.
      Assert_True (Builder.Get_Fourcc = 0);
      Assert_Cmpuint_Eq (Builder.Get_Width, 0);
      Assert_Cmpuint_Eq (Builder.Get_Height, 0);
      Builder.Set_Fourcc (16#34325258#);
      Builder.Set_Width (2);
      Builder.Set_Height (2);
      Builder.Set_Modifier (16#123456789ABCDEF0#);
      Assert_True (Builder.Get_Fourcc = 16#34325258#);
      Assert_Cmpuint_Eq (Builder.Get_Width, 2);
      Assert_Cmpuint_Eq (Builder.Get_Height, 2);
      Assert_True (Builder.Get_Modifier = 16#123456789ABCDEF0#);

      --  Configure and inspect both plane slots, including the file descriptor
      --  values without passing fake descriptors to the platform importer.
      Builder.Set_N_Planes (2);
      Assert_Cmpuint_Eq (Builder.Get_N_Planes, 2);
      Builder.Set_Fd (0, 0);
      Builder.Set_Fd (1, 17);
      Builder.Set_Offset (0, 8);
      Builder.Set_Offset (1, 12);
      Builder.Set_Stride (0, 8);
      Builder.Set_Stride (1, 16);
      Assert_Cmpint_Eq (Builder.Get_Fd (0), 0);
      Assert_Cmpint_Eq (Builder.Get_Fd (1), 17);
      Assert_Cmpuint_Eq (Builder.Get_Offset (0), 8);
      Assert_Cmpuint_Eq (Builder.Get_Offset (1), 12);
      Assert_Cmpuint_Eq (Builder.Get_Stride (0), 8);
      Assert_Cmpuint_Eq (Builder.Get_Stride (1), 16);
      Builder.Set_Premultiplied (False);
      Assert_False (Builder.Get_Premultiplied);
      Builder.Set_Premultiplied (True);
      Assert_True (Builder.Get_Premultiplied);

      --  The builder keeps and returns its update region and prior texture.
      Assert_True (Builder.Get_Update_Region = Cairo.Region.Null_Region);
      Builder.Set_Update_Region (Region);
      Assert_True (Builder.Get_Update_Region = Region);
      Builder.Set_Update_Texture (Update_Texture);
      Assert_True
        (Builder.Get_Update_Texture = Gdk.Texture.Gdk_Texture (Update_Texture));
      Builder.Set_Update_Region (Cairo.Region.Null_Region);
      Builder.Set_Update_Texture (null);
      Assert_True (Builder.Get_Update_Region = Cairo.Region.Null_Region);
      Assert_True (Builder.Get_Update_Texture = null);

      --  Release owned GObjects, the Cairo region, and the color-state ref.
      Glib.Object.Unref (Glib.Object.GObject (By_Procedure));
      Glib.Object.Unref (Glib.Object.GObject (Builder));
      Glib.Object.Unref (Glib.Object.GObject (Update_Texture));
      Glib.Bytes.Unref (Pixel_Bytes);
      Cairo.Region.Destroy (Region);
      Gdk.Color_State.Unref (Color);
   end Test_Builder;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func ("/gdk/dmabuf-texture-builder", Test_Builder'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
