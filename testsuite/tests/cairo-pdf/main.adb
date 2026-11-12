--  Exercise Cairo.PDF.Create and verify that it creates a usable PDF surface.
--  No C testsuite case calls cairo_pdf_surface_create, so this test is written
--  directly against the documented Cairo surface behavior.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;

with Cairo;
with Cairo.PDF;
with Cairo.Surface;
with GNAT.OS_Lib;
with Glib.Test; use Glib.Test;

use type Ada.Directories.File_Size;
use type Cairo.Cairo_Status;
use type Cairo.Surface.Cairo_Surface_Type;

procedure Main is

   --  A process-specific path lets parallel test runs create separate PDFs.
   Process_Id : constant String := Ada.Strings.Fixed.Trim
     (Integer'Image
        (GNAT.OS_Lib.Pid_To_Integer (GNAT.OS_Lib.Current_Process_Id)),
      Ada.Strings.Both);
   Pdf_Path : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory,
      "gtkada-cairo-pdf-" & Process_Id & ".pdf");

   --  Check the created surface's backend and status, then the finished file.
   procedure Test_Create
   with Convention => C;

   procedure Test_Create is
      Surface : Cairo.Cairo_Surface;
   begin
      --  Remove only a stale artifact carrying this process's unique name.
      if Ada.Directories.Exists (Pdf_Path) then
         Ada.Directories.Delete_File (Pdf_Path);
      end if;

      --  A small page keeps this test cheap and needs no printer or display.
      Surface := Cairo.PDF.Create (Pdf_Path, 72.0, 36.0);
      Assert_True
        (Cairo.Surface.Status (Surface) = Cairo.Cairo_Status_Success);
      Assert_True
        (Cairo.Surface.Get_Type (Surface) =
           Cairo.Surface.Cairo_Surface_Type_Pdf);

      --  Destroying the owned surface finishes the PDF and flushes its output.
      Cairo.Surface.Destroy (Surface);
      Assert_True (Ada.Directories.Exists (Pdf_Path));
      Assert_True (Ada.Directories.Size (Pdf_Path) > 0);
      Ada.Directories.Delete_File (Pdf_Path);
   end Test_Create;

begin
   --  GLib.Test provides the same assertion reporting used by other tests.
   Glib.Test.Init;
   Glib.Test.Add_Func ("/cairo-pdf/create", Test_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
