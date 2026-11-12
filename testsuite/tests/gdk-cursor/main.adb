--  Exercise Gdk.Cursor constructors and getters against the test driver's
--  Xvfb display. No C test covers these cursor operations directly.

with Ada.Command_Line;
with Gdk.Cursor;
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
   use type Gdk.Cursor.Gdk_Cursor;
   use type Gdk.Memory_Texture.Gdk_Memory_Texture;
   use type Gdk.Texture.Gdk_Texture;
   use type System.Address;

   --  Four opaque pixels make a deterministic image with valid nonzero
   --  hotspot coordinates on both axes.
   Pixels : constant Glib.Bytes.Gbytes :=
     Glib.Bytes.Gbytes_New
       ((16#11#, 16#22#, 16#33#, 16#FF#,
         16#44#, 16#55#, 16#66#, 16#FF#,
         16#77#, 16#88#, 16#99#, 16#FF#,
         16#AA#, 16#BB#, 16#CC#, 16#FF#), 16);

   procedure Test_Name_Cursors with Convention => C;
   procedure Test_Texture_Cursors with Convention => C;

   --  Check both public name constructors and the documented initialize-once
   --  rule. CSS names are shared cursor names, independent of a theme choice.
   procedure Test_Name_Cursors is
      Fallback : constant Gdk.Cursor.Gdk_Cursor :=
        Gdk.Cursor.Gdk_Cursor_New_From_Name ("pointer", null);
      Function_Cursor : constant Gdk.Cursor.Gdk_Cursor :=
        Gdk.Cursor.Gdk_Cursor_New_From_Name ("crosshair", Fallback);
      Procedure_Cursor : Gdk.Cursor.Gdk_Cursor;
   begin
      Assert_True (Fallback /= null and Function_Cursor /= null);
      Assert_True (Function_Cursor.Get_Name = "crosshair");
      Assert_True (Function_Cursor.Get_Fallback = Fallback);

      --  The procedure form creates a fresh wrapper and resolves the name.
      Gdk.Cursor.Gdk_New_From_Name (Procedure_Cursor, "text", Fallback);
      Assert_True (Glib.Object.Is_Created (Procedure_Cursor.all));
      Assert_True (Procedure_Cursor.Get_Name = "text");
      Assert_True (Procedure_Cursor.Get_Fallback = Fallback);

      --  Reinitializing an existing cursor is a no-op and keeps its name.
      Gdk.Cursor.Initialize_From_Name (Procedure_Cursor, "wait", null);
      Assert_True (Procedure_Cursor.Get_Name = "text");
   end Test_Name_Cursors;

   --  Check texture constructors, hotspot values, optional name/fallback, and
   --  that Get_Texture converts the borrowed GdkTexture back to the same GObject.
   procedure Test_Texture_Cursors is
      Texture : constant Gdk.Memory_Texture.Gdk_Memory_Texture :=
        Gdk.Memory_Texture.Gdk_Memory_Texture_New
          (2, 2, Gdk.Texture.Gdk_Memory_R8G8B8A8, Pixels, 8);
      Fallback : constant Gdk.Cursor.Gdk_Cursor :=
        Gdk.Cursor.Gdk_Cursor_New_From_Name ("pointer", null);
      Function_Cursor : constant Gdk.Cursor.Gdk_Cursor :=
        Gdk.Cursor.Gdk_Cursor_New_From_Texture
          (Texture, 1, 0, Fallback);
      Procedure_Cursor : Gdk.Cursor.Gdk_Cursor;
      Returned_Texture : Gdk.Texture.Gdk_Texture;
   begin
      Assert_True (Texture /= null and Function_Cursor /= null);
      Assert_Cmpint_Eq (Function_Cursor.Get_Hotspot_X, 1);
      Assert_Cmpint_Eq (Function_Cursor.Get_Hotspot_Y, 0);
      Assert_True (Function_Cursor.Get_Fallback = Fallback);
      Assert_True (Function_Cursor.Get_Name'Length = 0);
      Returned_Texture := Function_Cursor.Get_Texture;
      Assert_True (Returned_Texture /= null);
      Assert_True
        (Glib.Object.Get_Object (Glib.Object.GObject (Returned_Texture)) =
         Glib.Object.Get_Object (Glib.Object.GObject (Texture)));

      --  The procedure constructor accepts a null fallback independently.
      Gdk.Cursor.Gdk_New_From_Texture
        (Procedure_Cursor, Texture, 0, 1, null);
      Assert_True (Glib.Object.Is_Created (Procedure_Cursor.all));
      Assert_Cmpint_Eq (Procedure_Cursor.Get_Hotspot_X, 0);
      Assert_Cmpint_Eq (Procedure_Cursor.Get_Hotspot_Y, 1);
      Assert_True (Procedure_Cursor.Get_Fallback = null);
      Assert_True (Procedure_Cursor.Get_Texture /= null);

      --  The initializer leaves the original hotspot and texture unchanged.
      Gdk.Cursor.Initialize_From_Texture
        (Procedure_Cursor, Texture, 1, 1, Function_Cursor);
      Assert_Cmpint_Eq (Procedure_Cursor.Get_Hotspot_X, 0);
      Assert_Cmpint_Eq (Procedure_Cursor.Get_Hotspot_Y, 1);
   end Test_Texture_Cursors;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gdk-cursor/name", Test_Name_Cursors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-cursor/texture", Test_Texture_Cursors'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
