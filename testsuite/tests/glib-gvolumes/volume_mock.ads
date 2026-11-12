--  A small deterministic GObject implementation of GVolume for the tests.

with Glib.GVolumes;
with Glib.Object;
with System;

package Volume_Mock is

   type Volume_Record is new Glib.Object.GObject_Record with null record;
   type Volume is access all Volume_Record'Class;

   --  The mock owns one GObject reference. As_Interface is a borrowed view.
   procedure Create (Self : out Volume);
   procedure Destroy (Self : in out Volume);
   function As_Interface (Self : Volume) return Glib.GVolumes.Gvolume;
   function Raw_Object (Self : Volume) return System.Address;

   procedure Set_Finish_Succeeds (Success : Boolean);
   function Eject_Count return Natural;
   function Mount_Count return Natural;

end Volume_Mock;
