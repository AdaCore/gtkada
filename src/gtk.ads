with System;
with Glib;

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

   function Major_Version return Guint;
   function Minor_Version return Guint;
   function Micro_Version return Guint;

private

   type Root_Type is tagged
      record
         Ptr : System.Address := System.Null_Address;
      end record;

   function Get_Object (Object : in Root_Type'Class)
                        return System.Address;
   pragma Inline (Get_Object);

   procedure Set_Object (Object : in out Root_Type'Class;
                         Value  : in     System.Address);
   pragma Inline (Set_Object);

   function To_Boolean (Value : in Gint) return Boolean;
   function To_Gint (Bool : in Boolean) return Gint;

end Gtk;
