--  A small deterministic GObject implementation of GMount for the tests.

with Glib.GMounts;
with Glib.Object;
with System;

package Mount_Mock is

   type Mount_Record is new Glib.Object.GObject_Record with null record;
   type Mount is access all Mount_Record'Class;

   --  Create and destroy own the mock GObject reference; As_Interface is a
   --  non-owning view used by the Glib.GMounts API under test.
   procedure Create (Self : out Mount);
   procedure Destroy (Self : in out Mount);
   function As_Interface (Self : Mount) return Glib.GMounts.Gmount;
   function Raw_Object (Self : Mount) return System.Address;

   procedure Set_Finish_Succeeds (Success : Boolean);
   procedure Set_Sync_Succeeds (Success : Boolean);
   function Eject_Count return Natural;
   function Guess_Count return Natural;
   function Remount_Count return Natural;
   function Unmount_Count return Natural;

end Mount_Mock;
