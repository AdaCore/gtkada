--  Exercise Gdk.Memory_Texture_Builder constructors, properties, and Build.
--  No matching C testsuite case covers this builder, so this test checks the
--  documented round-trip behavior of its Ada binding directly.

with Ada.Command_Line;
with Cairo.Region;
with Gdk.Color_State;
with Gdk.Memory_Texture;
with Gdk.Memory_Texture_Builder;
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
   use type Gdk.Memory_Texture_Builder.Gdk_Memory_Texture_Builder;
   use type Gdk.Memory_Texture.Gdk_Memory_Texture;
   use type Gdk.Texture.Gdk_Memory_Format;
   use type Gdk.Texture.Gdk_Texture;

   procedure Test_Builder with Convention => C;

   --  Check both constructor forms, scalar and object properties, and build
   --  textures from the configured bytes without using a display resource.
   procedure Test_Builder is
      Builder : Gdk.Memory_Texture_Builder.Gdk_Memory_Texture_Builder;
      By_Procedure : Gdk.Memory_Texture_Builder.Gdk_Memory_Texture_Builder;
      Color : constant Gdk.Color_State.Gdk_Color_State :=
        Gdk.Color_State.Get_Srgb;
      --  Four leading padding bytes let the test use a nonzero plane offset
      --  while still building a valid one-pixel RGBA texture.
      Pixels : constant Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
        ((0, 0, 0, 0, 16#11#, 16#22#, 16#33#, 16#FF#), 8);
      Previous_Texture : constant Gdk.Memory_Texture.Gdk_Memory_Texture :=
        Gdk.Memory_Texture.Gdk_Memory_Texture_New
          (1, 1, Gdk.Texture.Gdk_Memory_R8G8B8A8, Pixels, 4);
      Rect : aliased Cairo.Region.Cairo_Rectangle_Int :=
        (X => 2, Y => 3, Width => 4, Height => 5);
      Region : constant Cairo.Region.Cairo_Region :=
        Cairo.Region.Create_Rectangle (Rect'Access);
      Texture : Gdk.Texture.Gdk_Texture;
      Rebuilt_Texture : Gdk.Texture.Gdk_Texture;
   begin
      --  The function constructor initializes its wrapper. Initialize is a
      --  no-op after creation, and Gdk_New covers the procedure constructor.
      Builder :=
        Gdk.Memory_Texture_Builder.Gdk_Memory_Texture_Builder_New;
      Assert_True (Glib.Object.Is_Created (Builder.all));
      Assert_True
        (Glib.Object.Get_Type (Glib.Object.GObject (Builder)) =
           Gdk.Memory_Texture_Builder.Get_Type);
      Gdk.Memory_Texture_Builder.Initialize (Builder);
      Gdk.Memory_Texture_Builder.Gdk_New (By_Procedure);
      Assert_True (Glib.Object.Is_Created (By_Procedure.all));

      --  Default properties report unset dimensions and strides, while the
      --  format and color state use the documented defaults.
      Assert_True (Glib.Is_Null (Builder.Get_Bytes));
      Assert_True (Gdk.Color_State.Equal (Builder.Get_Color_State, Color));
      Assert_True
        (Builder.Get_Format =
           Gdk.Texture.Gdk_Memory_R8G8B8A8_Premultiplied);
      Assert_Cmpint_Eq (Builder.Get_Height, 0);
      Assert_True (Builder.Get_Offset (0) = 0);
      Assert_True (Builder.Get_Stride = 0);
      Assert_True (Builder.Get_Stride_For_Plane (0) = 0);
      Assert_True (Builder.Get_Update_Region = Cairo.Region.Null_Region);
      Assert_True (Builder.Get_Update_Texture = null);
      Assert_Cmpint_Eq (Builder.Get_Width, 0);

      --  Each setter is checked through its matching getter. Plane 1 is also
      --  set and read to cover the indexed APIs independently of plane 0.
      Builder.Set_Bytes (Pixels);
      Builder.Set_Color_State (Color);
      Builder.Set_Format (Gdk.Texture.Gdk_Memory_R8G8B8A8);
      Builder.Set_Height (1);
      Builder.Set_Offset (0, 4);
      Builder.Set_Offset (1, 12);
      Builder.Set_Stride (4);
      Builder.Set_Stride_For_Plane (0, 4);
      Builder.Set_Stride_For_Plane (1, 8);
      Builder.Set_Update_Region (Region);
      Builder.Set_Update_Texture (Previous_Texture);
      Builder.Set_Width (1);
      Assert_True (Glib.Bytes.Get_Size (Builder.Get_Bytes) = 8);
      Assert_True (Gdk.Color_State.Equal (Builder.Get_Color_State, Color));
      Assert_True (Builder.Get_Format = Gdk.Texture.Gdk_Memory_R8G8B8A8);
      Assert_Cmpint_Eq (Builder.Get_Height, 1);
      Assert_True (Builder.Get_Offset (0) = 4);
      Assert_True (Builder.Get_Offset (1) = 12);
      Assert_True (Builder.Get_Stride = 4);
      Assert_True (Builder.Get_Stride_For_Plane (0) = 4);
      Assert_True (Builder.Get_Stride_For_Plane (1) = 8);
      Assert_True (Builder.Get_Update_Region = Region);
      Assert_True
        (Builder.Get_Update_Texture = Gdk.Texture.Gdk_Texture (Previous_Texture));
      Assert_Cmpint_Eq (Builder.Get_Width, 1);

      --  Build returns a regular Gdk.Texture with the requested dimensions
      --  and format. Reusing the builder produces another valid texture.
      Texture := Builder.Build;
      Assert_True (Texture /= null);
      Assert_Cmpint_Eq (Texture.Get_Width, 1);
      Assert_Cmpint_Eq (Texture.Get_Height, 1);
      Assert_True (Texture.Get_Format = Gdk.Texture.Gdk_Memory_R8G8B8A8);
      Rebuilt_Texture := Builder.Build;
      Assert_True (Rebuilt_Texture /= null);
      Assert_Cmpint_Eq (Rebuilt_Texture.Get_Width, 1);
      Assert_Cmpint_Eq (Rebuilt_Texture.Get_Height, 1);

      --  The update region and prior texture can be cleared by passing null.
      Builder.Set_Update_Region (Cairo.Region.Null_Region);
      Builder.Set_Update_Texture (null);
      Assert_True (Builder.Get_Update_Region = Cairo.Region.Null_Region);
      Assert_True (Builder.Get_Update_Texture = null);

      --  Release the owned GObjects, bytes, Cairo region, and color state.
      Glib.Object.Unref (Glib.Object.GObject (Rebuilt_Texture));
      Glib.Object.Unref (Glib.Object.GObject (Texture));
      Glib.Object.Unref (Glib.Object.GObject (By_Procedure));
      Glib.Object.Unref (Glib.Object.GObject (Builder));
      Glib.Object.Unref (Glib.Object.GObject (Previous_Texture));
      Glib.Bytes.Unref (Pixels);
      Cairo.Region.Destroy (Region);
      Gdk.Color_State.Unref (Color);
   end Test_Builder;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gdk/memory-texture-builder/properties",
      Test_Builder'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
