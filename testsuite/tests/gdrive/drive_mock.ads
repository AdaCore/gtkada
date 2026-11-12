--  A tiny test-only GObject that implements GDrive. Its virtual methods make
--  the interface deterministic without requiring a real removable drive.

with Glib.GDrives;
with Glib.Object;
with System;

package Drive_Mock is

   type Drive_Record is new Glib.Object.GObject_Record with null record;
   type Drive is access all Drive_Record'Class;

   procedure Create (Self : out Drive);
   procedure Destroy (Self : in out Drive);
   function As_Interface (Self : Drive) return Glib.GDrives.Gdrive;
   function Raw_Object (Self : Drive) return System.Address;

   procedure Set_Finish_Succeeds (Success : Boolean);
   function Poll_Count return Natural;

end Drive_Mock;
