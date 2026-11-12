--  Check the registered GType and interface contract of Gdk.Dmabuf_Texture.
--  Creating a texture itself requires an importable Linux DMA-BUF, which the
--  deterministic headless testsuite does not provide.

with Ada.Command_Line;
with Gdk.Dmabuf_Texture;
with Gdk.Paintable;
with Gdk.Texture;
with Glib;
with Glib.Loadable_Icon;
with Glib.Test;
with Glib.Types;

procedure Main is
   use Glib.Test;
   use type Glib.GType;

   procedure Test_Type_Contract with Convention => C;

   --  The registered type must inherit from GdkTexture and implement both
   --  interfaces declared by the generated Ada package.
   procedure Test_Type_Contract is
      Texture_Type : constant Glib.GType := Gdk.Dmabuf_Texture.Get_Type;
   begin
      Assert_True (Texture_Type /= Glib.GType_Invalid);
      Assert_True
        (Glib.Types.Is_A (Texture_Type, Gdk.Texture.Get_Type));
      Assert_True
        (Glib.Types.Is_A (Texture_Type, Gdk.Paintable.Get_Type));
      Assert_True
        (Glib.Types.Is_A (Texture_Type, Glib.Loadable_Icon.Get_Type));
   end Test_Type_Contract;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gdk/dmabuf-texture/type-contract",
      Test_Type_Contract'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
