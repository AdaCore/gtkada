with Interfaces.C.Strings;

package body Gtk is

   use type System.Address;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Object : in Root_Type'Class)
                        return System.Address is
   begin
      return Object.Ptr;
   end Get_Object;

   ------------------
   --  Is_Created  --
   ------------------

   function Is_Created (Object : in Root_Type) return Boolean is
      use type System.Address;
   begin
      return Get_Object (Object) /= System.Null_Address;
   end Is_Created;

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

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (Type_Num : in Gint) return      String
   is
      function Internal (Type_Num : in Gint)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_type_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Type_Num));
   end Type_Name;

   --------------------
   -- Unchecked_Cast --
   --------------------

   function Unchecked_Cast (From : in Root_Type'Class)
                            return To
   is
      T : To;
   begin
      Set_Object (T, Get_Object (From));
      return T;
   end Unchecked_Cast;

end Gtk;
