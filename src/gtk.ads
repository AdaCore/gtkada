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

private

   type Root_Type_Value is
      record
         Ptr     : System.Address := System.Null_Address;
         Num_Ref : Natural := 0;
      end record;

   type Root_Type_Ptr is access all Root_Type_Value;
   type Root_Type is new Ada.Finalization.Controlled with
      record
         Data : Root_Type_Ptr := null;
      end record;

   procedure Adjust (Object : in out Root_Type);
   procedure Finalize (Object : in out Root_Type);

   procedure Free_Object (Object : in out Root_Type'Class);

   function Get_Data (Object : in Root_Type'Class)
                      return Root_Type_Ptr;
   pragma Inline (Get_Data);

   function Get_Object (Object : in Root_Type'Class)
                        return System.Address;
   pragma Inline (Get_Object);

   function Get_Type (Object : in Root_Type'Class)
                      return Gint;
   pragma Inline (Get_Type);

   procedure Set_Data (Object : in out Root_Type'Class;
                       Data   : in Root_Type_Ptr);
   pragma Inline (Set_Data);

   procedure Set_Object (Object : in out Root_Type'Class;
                         Value  : in     System.Address);
   pragma Inline (Set_Object);

end Gtk;
