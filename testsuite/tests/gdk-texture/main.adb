--  Currently only tests GdkTexture constructor methods.

--  The valid PNG is produced in memory (pixbuf -> texture -> PNG bytes), so
--  no data file is needed. Decoding a texture needs no display, and no
--  widget is created, so this test needs no display.

with Ada.Command_Line;
with Ada.Directories;

with Glib;         use Glib;
with Glib.Bytes;   use Glib.Bytes;
with Glib.Error;   use Glib.Error;
with Glib.GFile;
with Glib.Object;  use Glib.Object;
with Glib.Test;    use Glib.Test;
with Glib.Types;
with Gdk.Pixbuf;   use Gdk.Pixbuf;
with Gdk.Texture;  use Gdk.Texture;
with System;       use System;

procedure Main is

   type Route is (Procedure_Form, Function_Form, Initialize_Form);
   --  The three Ada subprograms generated for each C constructor

   Width  : constant := 3;
   Height : constant := 2;

   Missing_Path : constant String := "/no/such/file-gtkada-texture-test.png";

   function Png_Bytes return Gbytes;
   --  A valid Width x Height PNG, encoded in memory

   function Garbage_Bytes return Gbytes;
   --  Bytes that are not a valid image of any supported format, built in
   --  memory (copying constructor - alignment-safe regardless of this
   --  array's own alignment).

   function Png_File return String;
   --  Write Png_Bytes' image to a file in the current directory and return
   --  its full name. The caller deletes it.

   procedure Release (File : Glib.GFile.Gfile);

   function From_Bytes
     (Via   : Route;
      Bytes : Gbytes;
      Error : out GError) return Gdk_Texture;

   function From_File
     (Via   : Route;
      File  : Glib.GFile.Gfile;
      Error : out GError) return Gdk_Texture;

   function From_Filename
     (Via   : Route;
      Path  : String;
      Error : out GError) return Gdk_Texture;

   procedure Check_Success (Texture : in out Gdk_Texture; Error : GError);
   --  Texture was decoded from Png_Bytes' image. Unrefs it.

   procedure Check_Failure
     (Via     : Route;
      Texture : in out Gdk_Texture;
      Error   : in out GError);
   --  The constructor failed. Frees Error, and Texture if still allocated.

   procedure Test_From_Bytes
   with Convention => C;

   procedure Test_From_Bytes_Invalid
   with Convention => C;

   procedure Test_From_File
   with Convention => C;

   procedure Test_From_File_Missing
   with Convention => C;

   procedure Test_From_Filename
   with Convention => C;

   procedure Test_From_Filename_Missing
   with Convention => C;

   procedure Test_Initialize_Already_Created
   with Convention => C;

   ---------------
   -- Png_Bytes --
   ---------------

   function Png_Bytes return Gbytes is
      Pixbuf  : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, Width, Height);
      Texture : Gdk_Texture;
      Result  : Gbytes;
   begin
      Pixbuf.Fill (16#112233FF#);
      Texture := Gdk_Texture_New_For_Pixbuf (Pixbuf);
      Result := Texture.Save_To_Png_Bytes;
      Unref (Texture);
      Unref (Pixbuf);
      return Result;
   end Png_Bytes;

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

   --------------
   -- Png_File --
   --------------

   function Png_File return String is
      Path    : constant String :=
        Ada.Directories.Full_Name ("gtkada-texture-test.png");
      Bytes   : Gbytes := Png_Bytes;
      Error   : GError;
      Texture : Gdk_Texture := Gdk_Texture_New_From_Bytes (Bytes, Error);
   begin
      Assert_True (Error = null);
      Assert_True (Texture.Save_To_Png (Path));
      Unref (Texture);
      Unref (Bytes);
      return Path;
   end Png_File;

   -------------
   -- Release --
   -------------

   procedure Release (File : Glib.GFile.Gfile) is
   begin
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
   end Release;

   ----------------
   -- From_Bytes --
   ----------------

   function From_Bytes
     (Via   : Route;
      Bytes : Gbytes;
      Error : out GError) return Gdk_Texture
   is
      Texture : Gdk_Texture;
   begin
      case Via is
         when Procedure_Form =>
            Gdk_New_From_Bytes (Texture, Bytes, Error);
         when Function_Form =>
            Texture := Gdk_Texture_New_From_Bytes (Bytes, Error);
         when Initialize_Form =>
            Texture := new Gdk_Texture_Record;
            Initialize_From_Bytes (Texture, Bytes, Error);
      end case;
      return Texture;
   end From_Bytes;

   ---------------
   -- From_File --
   ---------------

   function From_File
     (Via   : Route;
      File  : Glib.GFile.Gfile;
      Error : out GError) return Gdk_Texture
   is
      Texture : Gdk_Texture;
   begin
      case Via is
         when Procedure_Form =>
            Gdk_New_From_File (Texture, File, Error);
         when Function_Form =>
            Texture := Gdk_Texture_New_From_File (File, Error);
         when Initialize_Form =>
            Texture := new Gdk_Texture_Record;
            Initialize_From_File (Texture, File, Error);
      end case;
      return Texture;
   end From_File;

   -------------------
   -- From_Filename --
   -------------------

   function From_Filename
     (Via   : Route;
      Path  : String;
      Error : out GError) return Gdk_Texture
   is
      Texture : Gdk_Texture;
   begin
      case Via is
         when Procedure_Form =>
            Gdk_New_From_Filename (Texture, Path, Error);
         when Function_Form =>
            Texture := Gdk_Texture_New_From_Filename (Path, Error);
         when Initialize_Form =>
            Texture := new Gdk_Texture_Record;
            Initialize_From_Filename (Texture, Path, Error);
      end case;
      return Texture;
   end From_Filename;

   -------------------
   -- Check_Success --
   -------------------

   procedure Check_Success (Texture : in out Gdk_Texture; Error : GError) is
   begin
      Assert_True (Error = null);
      Assert_True (Texture /= null);
      Assert_True (Texture.Is_Created);
      Assert_True (Texture.Get_Width = Width);
      Assert_True (Texture.Get_Height = Height);
      Unref (Texture);
   end Check_Success;

   -------------------
   -- Check_Failure --
   -------------------

   procedure Check_Failure
     (Via     : Route;
      Texture : in out Gdk_Texture;
      Error   : in out GError) is
   begin
      Assert_True (Error /= null);
      Assert_True (Get_Message (Error)'Length > 0);
      Error_Free (Error);

      case Via is
         when Procedure_Form | Function_Form =>
            --  The generated constructor frees its record and returns null
            Assert_True (Texture = null);
         when Initialize_Form =>
            --  The caller's record is left allocated but not created
            Assert_True (Texture /= null);
            Assert_False (Texture.Is_Created);
            Texture.Deallocate;
            Texture := null;
      end case;
   end Check_Failure;

   ---------------------
   -- Test_From_Bytes --
   ---------------------

   procedure Test_From_Bytes is
      Bytes   : Gbytes := Png_Bytes;
      Error   : GError;
      Texture : Gdk_Texture;
   begin
      for Via in Route loop
         Texture := From_Bytes (Via, Bytes, Error);
         Check_Success (Texture, Error);
      end loop;
      Unref (Bytes);
   end Test_From_Bytes;

   -----------------------------
   -- Test_From_Bytes_Invalid --
   -----------------------------

   procedure Test_From_Bytes_Invalid is
      Bytes   : Gbytes := Garbage_Bytes;
      Error   : GError;
      Texture : Gdk_Texture;
   begin
      for Via in Route loop
         Texture := From_Bytes (Via, Bytes, Error);
         Check_Failure (Via, Texture, Error);
      end loop;
      Unref (Bytes);
   end Test_From_Bytes_Invalid;

   --------------------
   -- Test_From_File --
   --------------------

   procedure Test_From_File is
      Path    : constant String := Png_File;
      File    : constant Glib.GFile.Gfile := Glib.GFile.New_For_Path (Path);
      Error   : GError;
      Texture : Gdk_Texture;
   begin
      for Via in Route loop
         Texture := From_File (Via, File, Error);
         Check_Success (Texture, Error);
      end loop;
      Release (File);
      Ada.Directories.Delete_File (Path);
   end Test_From_File;

   ----------------------------
   -- Test_From_File_Missing --
   ----------------------------

   procedure Test_From_File_Missing is
      File    : constant Glib.GFile.Gfile := Glib.GFile.New_For_Path (Missing_Path);
      Error   : GError;
      Texture : Gdk_Texture;
   begin
      for Via in Route loop
         Texture := From_File (Via, File, Error);
         Check_Failure (Via, Texture, Error);
      end loop;
      Release (File);
   end Test_From_File_Missing;

   ------------------------
   -- Test_From_Filename --
   ------------------------

   procedure Test_From_Filename is
      Path    : constant String := Png_File;
      Error   : GError;
      Texture : Gdk_Texture;
   begin
      for Via in Route loop
         Texture := From_Filename (Via, Path, Error);
         Check_Success (Texture, Error);
      end loop;
      Ada.Directories.Delete_File (Path);
   end Test_From_Filename;

   --------------------------------
   -- Test_From_Filename_Missing --
   --------------------------------

   procedure Test_From_Filename_Missing is
      Error   : GError;
      Texture : Gdk_Texture;
   begin
      for Via in Route loop
         Texture := From_Filename (Via, Missing_Path, Error);
         Check_Failure (Via, Texture, Error);
      end loop;
   end Test_From_Filename_Missing;

   -------------------------------------
   -- Test_Initialize_Already_Created --
   -------------------------------------

   procedure Test_Initialize_Already_Created is
      --  Initialize_* on an existing texture is a no-op: it must clear a
      --  stale Error and leave the object alone. The inputs are invalid,
      --  so a loader that did run would set Error.
      Bytes       : Gbytes := Png_Bytes;
      Garbage     : Gbytes := Garbage_Bytes;
      File        : constant Glib.GFile.Gfile := Glib.GFile.New_For_Path (Missing_Path);
      Error       : GError;
      Stale_Error : GError;
      Texture     : Gdk_Texture;
      Failed      : Gdk_Texture;
      Original    : System.Address;
   begin
      --  Any real error will do as the stale value
      Failed := Gdk_Texture_New_From_Bytes (Garbage, Stale_Error);
      Assert_True (Failed = null and Stale_Error /= null);

      Texture := Gdk_Texture_New_From_Bytes (Bytes, Error);
      Assert_True (Error = null);
      Original := Texture.Get_Object;

      for Step in 1 .. 3 loop
         Error := Stale_Error;
         case Step is
            when 1 => Initialize_From_Bytes (Texture, Garbage, Error);
            when 2 => Initialize_From_File (Texture, File, Error);
            when 3 => Initialize_From_Filename (Texture, Missing_Path, Error);
         end case;
         Assert_True (Error = null);
         Assert_True (Texture.Get_Object = Original);
      end loop;

      Error_Free (Stale_Error);
      Unref (Texture);
      Release (File);
      Unref (Garbage);
      Unref (Bytes);
   end Test_Initialize_Already_Created;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func
     ("/gdk-texture/new-from-bytes", Test_From_Bytes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/new-from-bytes-invalid",
      Test_From_Bytes_Invalid'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/new-from-file", Test_From_File'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/new-from-file-missing",
      Test_From_File_Missing'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/new-from-filename",
      Test_From_Filename'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/new-from-filename-missing",
      Test_From_Filename_Missing'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/initialize-already-created",
      Test_Initialize_Already_Created'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
