--  Exercise Cairo.SVG.Create and verify that it writes a usable SVG surface.
--  No matching cairo_svg_surface_create case exists in testsuite/c_tests/.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;

with Cairo;
with Cairo.SVG;
with Cairo.Surface;
with GNAT.OS_Lib;
with Glib.Test; use Glib.Test;

use type Ada.Directories.File_Size;
use type Cairo.Cairo_Status;
use type Cairo.Surface.Cairo_Surface_Type;

procedure Main is

   --  A process-specific name avoids collisions between parallel test runs.
   Process_Id : constant String := Ada.Strings.Fixed.Trim
     (Integer'Image
        (GNAT.OS_Lib.Pid_To_Integer (GNAT.OS_Lib.Current_Process_Id)),
      Ada.Strings.Both);
   Svg_Path : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory,
      "gtkada-cairo-svg-" & Process_Id & ".svg");

   --  The SVG backend should return a successful surface and flush its file
   --  when the caller releases the surface it owns.
   procedure Test_Create
   with Convention => C;

   procedure Test_Create is
      Surface : Cairo.Cairo_Surface;
   begin
      --  Clean a leftover from an earlier interrupted run of this process.
      if Ada.Directories.Exists (Svg_Path) then
         Ada.Directories.Delete_File (Svg_Path);
      end if;

      --  A small page exercises the filename and point dimensions without
      --  requiring a printer or a display.
      Surface := Cairo.SVG.Create (Svg_Path, 72.0, 36.0);
      Assert_True
        (Cairo.Surface.Status (Surface) = Cairo.Cairo_Status_Success);
      Assert_True
        (Cairo.Surface.Get_Type (Surface) =
           Cairo.Surface.Cairo_Surface_Type_Svg);

      --  Destroying the surface finishes the SVG document and writes it out.
      Cairo.Surface.Destroy (Surface);
      Assert_True (Ada.Directories.Exists (Svg_Path));
      Assert_True (Ada.Directories.Size (Svg_Path) > 0);
      Ada.Directories.Delete_File (Svg_Path);
   end Test_Create;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/cairo-svg/create", Test_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
