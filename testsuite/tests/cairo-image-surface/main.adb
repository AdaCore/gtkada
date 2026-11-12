--  Test caller-owned data constructors, text-glyph support, and PNG I/O on
--  Cairo image surfaces. No matching Cairo PNG test exists in
--  testsuite/c_tests/.
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Cairo;
with Cairo.Image_Surface;
with Cairo.Png;
with Cairo.Surface;
with GNAT.OS_Lib;
with Glib.Test; use Glib.Test;
with System;

use type Cairo.Cairo_Status;
use type Cairo.Image_Surface.Cairo_Format;
use type Ada.Directories.File_Size;
use type System.Address;

procedure Main is

   --  Process-specific paths keep parallel test runs from sharing files.
   Process_Id : constant String := Ada.Strings.Fixed.Trim
     (Integer'Image
        (GNAT.OS_Lib.Pid_To_Integer (GNAT.OS_Lib.Current_Process_Id)),
      Ada.Strings.Both);
   Png_Path : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory,
      "gtkada-cairo-png-" & Process_Id & ".png");
   Missing_Directory : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory,
      "gtkada-cairo-png-missing-" & Process_Id);
   Missing_Png_Path : constant String := Ada.Directories.Compose
     (Missing_Directory, "image.png");
   Absent_Png_Path : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory,
      "gtkada-cairo-png-absent-" & Process_Id & ".png");

   --  Each typed constructor must preserve the caller's buffer and geometry.
   --  Status, format, dimensions, stride, and the returned data address are checked.
   --  The test uses small dimensions to verify A8 row padding as well.
   procedure Test_Caller_Data_Constructors
   with Convention => C;

   --  Image surfaces rasterize glyphs and do not preserve text clusters.
   procedure Test_Text_Glyph_Support
   with Convention => C;

   --  PNG output can be loaded back with the original image dimensions, while
   --  inaccessible output and missing input report Cairo's documented errors.
   procedure Test_Png_Round_Trip
   with Convention => C;

   procedure Test_Caller_Data_Constructors is
      --  Use Cairo's own access types so the backing buffers outlive surfaces.
      procedure Free_ARGB is new Ada.Unchecked_Deallocation
        (Cairo.Image_Surface.ARGB32_Array,
         Cairo.Image_Surface.ARGB32_Array_Access);
      procedure Free_RGB is new Ada.Unchecked_Deallocation
        (Cairo.Image_Surface.RGB24_Array,
         Cairo.Image_Surface.RGB24_Array_Access);
      procedure Free_A8 is new Ada.Unchecked_Deallocation
        (Cairo.Image_Surface.Byte_Array,
         Cairo.Image_Surface.Byte_Array_Access);
      --  Cairo uses four bytes per ARGB32 and RGB24 pixel.
      ARGB_Data : Cairo.Image_Surface.ARGB32_Array_Access :=
        new Cairo.Image_Surface.ARGB32_Array'
          (0 .. 5 => (Alpha => 16#FF#, Red => 16#10#,
                      Green => 16#20#, Blue => 16#30#));
      RGB_Data  : Cairo.Image_Surface.RGB24_Array_Access :=
        new Cairo.Image_Surface.RGB24_Array'
          (0 .. 5 => (Red => 16#40#, Green => 16#50#, Blue => 16#60#));
      --  A8 rows are padded to a four-byte stride: 2 rows need 8 bytes.
      A8_Data   : Cairo.Image_Surface.Byte_Array_Access :=
        new Cairo.Image_Surface.Byte_Array'(0 .. 7 => 16#80#);
      Surface   : Cairo.Cairo_Surface;
   begin
      --  Destroy each surface before its stack-allocated backing data expires.
      Surface := Cairo.Image_Surface.Create_For_Data_ARGB32
        (ARGB_Data, 3, 2);
      Assert_True
        (Cairo.Surface.Status (Surface) = Cairo.Cairo_Status_Success);
      Assert_True
        (Cairo.Image_Surface.Get_Format (Surface) =
           Cairo.Image_Surface.Cairo_Format_ARGB32);
      Assert_Cmpint_Eq (Cairo.Image_Surface.Get_Width (Surface), 3);
      Assert_Cmpint_Eq (Cairo.Image_Surface.Get_Height (Surface), 2);
      Assert_Cmpint_Eq (Cairo.Image_Surface.Get_Stride (Surface), 12);
      Assert_True
        (Cairo.Image_Surface.Get_Data_Generic (Surface) =
           ARGB_Data (ARGB_Data'First)'Address);
      Cairo.Surface.Destroy (Surface);
      Free_ARGB (ARGB_Data);

      --  RGB24 uses the same four-byte pixel stride as ARGB32.
      Surface := Cairo.Image_Surface.Create_For_Data_RGB24
        (RGB_Data, 3, 2);
      Assert_True
        (Cairo.Surface.Status (Surface) = Cairo.Cairo_Status_Success);
      Assert_True
        (Cairo.Image_Surface.Get_Format (Surface) =
           Cairo.Image_Surface.Cairo_Format_RGB24);
      Assert_Cmpint_Eq (Cairo.Image_Surface.Get_Width (Surface), 3);
      Assert_Cmpint_Eq (Cairo.Image_Surface.Get_Height (Surface), 2);
      Assert_Cmpint_Eq (Cairo.Image_Surface.Get_Stride (Surface), 12);
      Assert_True
        (Cairo.Image_Surface.Get_Data_Generic (Surface) =
           RGB_Data (RGB_Data'First)'Address);
      Cairo.Surface.Destroy (Surface);
      Free_RGB (RGB_Data);

      --  A8 has one byte per pixel and Cairo pads each row to four bytes.
      Surface := Cairo.Image_Surface.Create_For_Data_A8
        (A8_Data, 3, 2);
      Assert_True
        (Cairo.Surface.Status (Surface) = Cairo.Cairo_Status_Success);
      Assert_True
        (Cairo.Image_Surface.Get_Format (Surface) =
           Cairo.Image_Surface.Cairo_Format_A8);
      Assert_Cmpint_Eq (Cairo.Image_Surface.Get_Width (Surface), 3);
      Assert_Cmpint_Eq (Cairo.Image_Surface.Get_Height (Surface), 2);
      Assert_Cmpint_Eq (Cairo.Image_Surface.Get_Stride (Surface), 4);
      Assert_True
        (Cairo.Image_Surface.Get_Data_Generic (Surface) =
           A8_Data (A8_Data'First)'Address);
      Cairo.Surface.Destroy (Surface);
      Free_A8 (A8_Data);
   end Test_Caller_Data_Constructors;

   -----------------------------
   -- Test_Text_Glyph_Support --
   -----------------------------

   procedure Test_Text_Glyph_Support is
      --  A raster image cannot use the original UTF-8 cluster information.
      Surface : constant Cairo.Cairo_Surface :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32, 8, 8);
   begin
      Assert_False (Cairo.Surface.Has_Show_Text_Glyphs (Surface));
      Cairo.Surface.Destroy (Surface);
   end Test_Text_Glyph_Support;

   -------------------------
   -- Test_Png_Round_Trip --
   -------------------------

   procedure Test_Png_Round_Trip is
      Surface : constant Cairo.Cairo_Surface :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32, 3, 2);
      Loaded : Cairo.Cairo_Surface;
   begin
      --  Remove only artifacts bearing this process's unique name from an
      --  earlier interrupted invocation.
      if Ada.Directories.Exists (Png_Path) then
         Ada.Directories.Delete_File (Png_Path);
      end if;
      if Ada.Directories.Exists (Missing_Directory) then
         Ada.Directories.Delete_Tree (Missing_Directory);
      end if;

      Assert_True
        (Cairo.Surface.Status (Surface) = Cairo.Cairo_Status_Success);
      Assert_True
        (Cairo.Png.Write_To_Png (Surface, Png_Path) =
           Cairo.Cairo_Status_Success);
      Assert_True (Ada.Directories.Exists (Png_Path));
      Assert_True (Ada.Directories.Size (Png_Path) > 0);

      --  Cairo returns a surface carrying the PNG dimensions and success
      --  status; destroy that owned reference after checking it.
      Loaded := Cairo.Png.Create_From_Png (Png_Path);
      Assert_True
        (Cairo.Surface.Status (Loaded) = Cairo.Cairo_Status_Success);
      Assert_Cmpint_Eq (Cairo.Image_Surface.Get_Width (Loaded), 3);
      Assert_Cmpint_Eq (Cairo.Image_Surface.Get_Height (Loaded), 2);
      Cairo.Surface.Destroy (Loaded);

      --  A path below a directory that does not exist exercises the write
      --  error result without depending on permissions or platform behavior.
      Assert_True
        (Cairo.Png.Write_To_Png (Surface, Missing_Png_Path) =
           Cairo.Cairo_Status_Write_Error);

      --  Missing input is represented by an error surface, not a null handle.
      Loaded := Cairo.Png.Create_From_Png (Absent_Png_Path);
      Assert_True
        (Cairo.Surface.Status (Loaded) = Cairo.Cairo_Status_File_Not_Found);
      Cairo.Surface.Destroy (Loaded);

      Cairo.Surface.Destroy (Surface);
      Ada.Directories.Delete_File (Png_Path);
   end Test_Png_Round_Trip;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/cairo-image-surface/caller-data",
      Test_Caller_Data_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cairo-image-surface/text-glyph-support",
      Test_Text_Glyph_Support'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cairo-image-surface/png-round-trip",
      Test_Png_Round_Trip'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
