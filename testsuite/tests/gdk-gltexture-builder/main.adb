--  Exercise Gdk.GLTexture_Builder's constructors and property accessors.
--  testsuite/c_tests has no test for this builder to port. Building the final
--  GL texture is left to a test with a real GL context and texture resource.

with Ada.Command_Line;
with Cairo.Region;
with Gdk.Color_State;
with Gdk.GLContext;
with Gdk.GLTexture_Builder;
with Gdk.Memory_Texture;
with Gdk.Texture;
with Glib;
with Glib.Bytes;
with Glib.Object;
with Glib.Test;
with Gtk.Main;
with System;

procedure Main is
   use Glib;
   use Glib.Test;
   use type Cairo.Region.Cairo_Region;
   use type Gdk.GLTexture_Builder.Gdk_Gltexture_Builder;
   use type Gdk.GLContext.Gdk_GLContext;
   use type Gdk.Memory_Texture.Gdk_Memory_Texture;
   use type Gdk.Texture.Gdk_Memory_Format;
   use type Gdk.Texture.Gdk_Texture;
   use type System.Address;

   procedure Test_Builder with Convention => C;

   --  Round-trip optional object and scalar properties without creating GL
   --  resources; all the tested properties belong to the GObject builder.
   procedure Test_Builder is
      Builder : Gdk.GLTexture_Builder.Gdk_Gltexture_Builder;
      By_Procedure : Gdk.GLTexture_Builder.Gdk_Gltexture_Builder;
      Color : constant Gdk.Color_State.Gdk_Color_State :=
        Gdk.Color_State.Get_Srgb;
      Pixels : constant Glib.Bytes.Gbytes :=
        Glib.Bytes.Gbytes_New ((16#11#, 16#22#, 16#33#, 16#FF#), 4);
      Previous_Texture : constant Gdk.Memory_Texture.Gdk_Memory_Texture :=
        Gdk.Memory_Texture.Gdk_Memory_Texture_New
          (1, 1, Gdk.Texture.Gdk_Memory_R8G8B8A8, Pixels, 4);
      Rect : aliased Cairo.Region.Cairo_Rectangle_Int :=
        (X => 2, Y => 3, Width => 4, Height => 5);
      Region : constant Cairo.Region.Cairo_Region :=
        Cairo.Region.Create_Rectangle (Rect'Access);
      Sync_Value : aliased Integer := 7;
   begin
      --  The function constructor initializes its wrapper; Initialize is a
      --  no-op after creation, while Gdk_New exercises the other constructor.
      Builder := Gdk.GLTexture_Builder.Gdk_Gltexture_Builder_New;
      Assert_True (Glib.Object.Is_Created (Builder.all));
      Assert_True
        (Glib.Object.Get_Type (Glib.Object.GObject (Builder)) =
           Gdk.GLTexture_Builder.Get_Type);
      Gdk.GLTexture_Builder.Initialize (Builder);
      Gdk.GLTexture_Builder.Gdk_New (By_Procedure);
      Assert_True (Glib.Object.Is_Created (By_Procedure.all));

      --  Defaults document unset state, and null inputs clear optional refs.
      Assert_True (Gdk.Color_State.Equal (Builder.Get_Color_State, Color));
      Assert_True (Builder.Get_Context = null);
      Assert_True
        (Builder.Get_Format = Gdk.Texture.Gdk_Memory_R8G8B8A8_Premultiplied);
      Assert_False (Builder.Get_Has_Mipmap);
      Assert_Cmpint_Eq (Builder.Get_Height, 0);
      Assert_Cmpuint_Eq (Builder.Get_Id, 0);
      Assert_True (Builder.Get_Sync = System.Null_Address);
      Assert_True (Builder.Get_Update_Region = Cairo.Region.Null_Region);
      Assert_True (Builder.Get_Update_Texture = null);
      Assert_Cmpint_Eq (Builder.Get_Width, 0);
      Builder.Set_Context (null);
      Assert_True (Builder.Get_Context = null);

      --  Setting sRGB preserves the boxed color-state object.
      Builder.Set_Color_State (Color);
      Assert_True (Gdk.Color_State.Equal (Builder.Get_Color_State, Color));

      --  Scalar setters preserve values; toggling the boolean checks both
      --  directions of its Ada Boolean to C gboolean conversion.
      Builder.Set_Format (Gdk.Texture.Gdk_Memory_R8G8B8A8);
      Builder.Set_Has_Mipmap (True);
      Builder.Set_Height (5);
      Builder.Set_Id (123);
      Builder.Set_Width (7);
      Assert_True (Builder.Get_Format = Gdk.Texture.Gdk_Memory_R8G8B8A8);
      Assert_True (Builder.Get_Has_Mipmap);
      Assert_Cmpint_Eq (Builder.Get_Height, 5);
      Assert_Cmpuint_Eq (Builder.Get_Id, 123);
      Assert_Cmpint_Eq (Builder.Get_Width, 7);
      Builder.Set_Has_Mipmap (False);
      Builder.Set_Height (0);
      Builder.Set_Id (0);
      Builder.Set_Width (0);
      Assert_False (Builder.Get_Has_Mipmap);
      Assert_Cmpint_Eq (Builder.Get_Height, 0);
      Assert_Cmpuint_Eq (Builder.Get_Id, 0);
      Assert_Cmpint_Eq (Builder.Get_Width, 0);

      --  Sync is opaque GL data. An address-sized sentinel checks that Ada
      --  passes it through without dereferencing or taking ownership of it.
      Builder.Set_Sync (Sync_Value'Address);
      Assert_True (Builder.Get_Sync = Sync_Value'Address);
      Builder.Set_Sync (System.Null_Address);
      Assert_True (Builder.Get_Sync = System.Null_Address);

      --  The update pair holds a Cairo region and a normal GdkTexture; passing
      --  null afterward exercises the bindings' nullable object conversions.
      Builder.Set_Update_Region (Region);
      Builder.Set_Update_Texture (Previous_Texture);
      Assert_True (Builder.Get_Update_Region = Region);
      Assert_True
        (Builder.Get_Update_Texture = Gdk.Texture.Gdk_Texture (Previous_Texture));
      Builder.Set_Update_Region (Cairo.Region.Null_Region);
      Builder.Set_Update_Texture (null);
      Assert_True (Builder.Get_Update_Region = Cairo.Region.Null_Region);
      Assert_True (Builder.Get_Update_Texture = null);

      --  Release owned references after the builder has dropped its properties.
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
     ("/gdk/gltexture-builder/properties",
      Test_Builder'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
