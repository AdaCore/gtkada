with System;
with Glib;
with Ada.Finalization;

with Glib; use Glib;

package Gtk is

   type Root_Type is abstract tagged private;
   --
   --  This type is an internal type used as a basis for all types
   --  of Gtk.
   --
   --  In the private part, the Get_Object and Set_Object will be
   --  Accessible from the body of any child of Gtk for any descendant
   --  of Gtk_Root_Type.

   function Is_Created (Object : in Root_Type'Class) return Boolean;

   function Major_Version return Guint;
   function Minor_Version return Guint;
   function Micro_Version return Guint;

   procedure Ref (Object : in out Root_Type);
   --  mapping: Ref gtkobject.h gtk_object_ref

   procedure Unref (Object : in out Root_Type);
   --  mapping: Unref gtkobject.h gtk_object_unref

   function Type_Name (Object : in Root_Type'Class)
                       return      String;

   function Get_Object (Object : in Root_Type'Class)
                        return System.Address;
   pragma Inline (Get_Object);

   procedure Set_Object (Object : in out Root_Type'Class;
                         Value  : in     System.Address);
   pragma Inline (Set_Object);

private

   type Root_Type is new Ada.Finalization.Controlled with
      record
         Ptr : System.Address := System.Null_Address;
      end record;

   procedure Adjust (Object : in out Root_Type);
   procedure Finalize (Object : in out Root_Type);
   procedure Initialize (Object : in out Root_Type);

end Gtk;
