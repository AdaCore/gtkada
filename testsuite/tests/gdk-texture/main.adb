--  Gdk.Texture's GError-throwing constructors: Gdk_New_From_Bytes and
--  Gdk_New_From_Filename. Only the failure paths are exercised here (no
--  valid embedded image data) since texture decoding doesn't need a
--  display, but constructing a real one to render/display would.
--
--  No widget is created, so this test needs no display.

with Ada.Command_Line;

with Glib;         use Glib;
with Glib.Bytes;   use Glib.Bytes;
with Glib.Error;   use Glib.Error;
with Glib.Object;  use Glib.Object;
with Glib.Test;    use Glib.Test;
with Gdk.Texture;  use Gdk.Texture;

procedure Main is

   function Garbage_Bytes return Gbytes;
   --  Bytes that are not a valid image of any supported format, built in
   --  memory (copying constructor - alignment-safe regardless of this
   --  array's own alignment).

   -------------------
   -- Garbage_Bytes --
   -------------------

   function Garbage_Bytes return Gbytes is
      Data   : constant Guint8_Array (0 .. 15) := (others => 16#AA#);
      Result : Gbytes;
   begin
      G_New (Result, Data, Data'Length);
      return Result;
   end Garbage_Bytes;

   procedure Test_New_From_Bytes_Invalid
   with Convention => C;

   procedure Test_New_From_Filename_Missing
   with Convention => C;

   -----------------------------------
   -- Test_New_From_Bytes_Invalid --
   -----------------------------------

   procedure Test_New_From_Bytes_Invalid is
      Bundle  : Gbytes := Garbage_Bytes;
      Error   : Glib.Error.GError;
      Texture : Gdk_Texture;
   begin
      Gdk_New_From_Bytes (Texture, Bundle, Error);

      --  A GObject-style constructor: Texture is always allocated (unlike
      --  Gresource's C_Boxed, which defaults to null), so "not created" is
      --  the correct failure signal, not "= null".
      Assert_True (Texture = null);
      Assert_True (Error /= null);
      Assert_True (Get_Message (Error)'Length > 0);

      Error_Free (Error);
      Unref (Bundle);
   end Test_New_From_Bytes_Invalid;

   -------------------------------------
   -- Test_New_From_Filename_Missing --
   -------------------------------------

   procedure Test_New_From_Filename_Missing is
      Error   : Glib.Error.GError;
      Texture : Gdk_Texture;
   begin
      Gdk_New_From_Filename
        (Texture, "/no/such/file-gtkada-texture-test.png", Error);

      Assert_True (Texture = null);
      Assert_True (Error /= null);

      Error_Free (Error);
   end Test_New_From_Filename_Missing;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func
     ("/gdk-texture/new-from-bytes-invalid",
      Test_New_From_Bytes_Invalid'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/new-from-filename-missing",
      Test_New_From_Filename_Missing'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
