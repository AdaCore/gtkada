--  Register the GdkGLTexture binding tests with GLib's test runner.

with Ada.Command_Line;
with Glib.Test;
with GLTexture_Tests;

procedure Main is
begin
   Glib.Test.Init;
   --  The case covers paintable accessors and both implemented interfaces.
   Glib.Test.Add_Func
     ("/gdk-gltexture/paintable",
      GLTexture_Tests.Test_Paintable'Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
