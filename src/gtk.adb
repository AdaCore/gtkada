with Unchecked_Deallocation;

package body Gtk is

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Root_Type) is
   begin
      Object.Data.Num_Ref := Object.Data.Num_Ref + 1;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Root_Type) is
   begin
      if Object.Data /= null then
         Object.Data.Num_Ref := Object.Data.Num_Ref - 1;
      end if;
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free_Object (Object : in out Root_Type'Class) is
      procedure Internal is new Unchecked_Deallocation (Root_Type_Value,
                                                        Root_Type_Ptr);
   begin
      Internal (Object.Data);
      Object.Data := null;
   end Free_Object;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Object : in Root_Type'Class)
                      return Root_Type_Ptr
   is
   begin
      return Object.Data;
   end Get_Data;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Object : in Root_Type'Class)
                        return System.Address is
   begin
      return Object.Data.Ptr;
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
      return Internal (Get_Object (Object));
   end Get_Type;

   ------------------
   --  Is_Created  --
   ------------------

   function Is_Created (Object : in Root_Type'Class) return Boolean is
      use type System.Address;
   begin
      return Get_Data (Object) /= null
        and then Get_Object (Object) /= System.Null_Address;
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

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Object : in out Root_Type'Class;
                       Data   : in Root_Type_Ptr)
   is
   begin
      if Object.Data /= null then
         Object.Data.Num_Ref := Object.Data.Num_Ref - 1;
      end if;

      Object.Data := Data;
      Object.Data.Num_Ref := Object.Data.Num_Ref + 1;
   end Set_Data;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object (Object : in out Root_Type'Class;
                         Value  : in     System.Address) is
      use type System.Address;
   begin
      if Object.Data /= null then
         Object.Data.Num_Ref := Object.Data.Num_Ref - 1;
      end if;
      Object.Data := new Root_Type_Value'(Ptr     => Value,
                                          Num_Ref => 1);
   end Set_Object;


end Gtk;
