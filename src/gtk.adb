package body Gtk is

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Object : in Root_Type'Class)
                        return System.Address is
   begin
      return Object.Ptr;
   end Get_Object;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Object : in Root_Type'Class)
                      return Gint is
      function Internal (Object : System.Address)
                         return Gint;
      pragma Import (C, Internal, "ada_object_get_type");
   begin
      return Internal (Object.Ptr);
   end Get_Type;

   -------------------
   -- Major_Version --
   -------------------

   function Major_Version return Guint is
      Number : Guint;
      pragma Import (C, Number, "gtk_major_version");
   begin
      return Number;
   end Major_Version;


   -------------------
   -- Micro_Version --
   -------------------

   function Micro_Version return Guint is
      Number : Guint;
      pragma Import (C, Number, "gtk_micro_version");
   begin
      return Number;
   end Micro_Version;

   -------------------
   -- Minor_Version --
   -------------------

   function Minor_Version return Guint is
      Number : Guint;
      pragma Import (C, Number, "gtk_minor_version");
   begin
      return Number;
   end Minor_Version;


   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object (Object : in out Root_Type'Class;
                         Value  : in     System.Address) is
      use type System.Address;
   begin
      Object.Ptr := Value;
   end Set_Object;


end Gtk;
