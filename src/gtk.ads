with System;
with Glib;
with Ada.Finalization;

with Glib; use Glib;

package Gtk is

   type Root_Type is tagged private;
   --
   --  This type is an internal type used as a basis for all types
   --  of Gtk.

   function Is_Created (Object : in Root_Type) return Boolean;
   function Major_Version return Guint;
   function Minor_Version return Guint;
   function Micro_Version return Guint;

   function Type_Name (Type_Num : in Gint) return      String;

   function Get_Object (Object : in Root_Type'Class)
                        return System.Address;
   pragma Inline (Get_Object);

   procedure Set_Object (Object : in out Root_Type'Class;
                         Value  : in     System.Address);
   pragma Inline (Set_Object);

   generic
      type To is new Root_Type with private;
   function Unchecked_Cast (From : in Root_Type'Class)
                            return To;
   --  This function allows the conversion any two widget types.
   --  Warning : no verification is done at Ada level. The only verification
   --  are done by gtk itself. Whenever possible, avoir using this function

private

   type Root_Type is new Ada.Finalization.Controlled with
      record
         Ptr : System.Address := System.Null_Address;
      end record;

end Gtk;
