package body Gtk is

   use type System.Address;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Root_Type) is
   begin
      Ref (Object);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Root_Type) is
   begin
      Unref (Object);
   end Finalize;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Object : in Root_Type'Class)
                        return System.Address is
   begin
      return Object.Ptr;
   end Get_Object;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Root_Type) is
   begin
      Ref (Object);
   end Initialize;

   ------------------
   --  Is_Created  --
   ------------------

   function Is_Created (Object : in Root_Type'Class) return Boolean is
      use type System.Address;
      function Destroyed (Object : in System.Address) return Guint32;
      pragma Import (C, Destroyed, "ada_object_destroyed");
   begin
      return Get_Object (Object) /= System.Null_Address
        and then not To_Boolean (Destroyed (Get_Object (Object)));
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

   ---------
   -- Ref --
   ---------

   procedure Ref (Object : in out Root_Type'Class) is
      procedure Internal (Object : in System.Address);
      pragma Import (C, Internal, "gtk_object_ref");
   begin
      if Get_Object (Object) /= System.Null_Address then
         Internal (Get_Object (Object));
      end if;
   end Ref;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object (Object : in out Root_Type'Class;
                         Value  : in     System.Address) is
      use type System.Address;
   begin
      Unref (Object);
      Object.Ptr := Value;
      Ref (Object);
   end Set_Object;

   -----------
   -- Unref --
   -----------

   procedure Unref (Object : in out Root_Type'Class) is
      procedure Internal (Object : in System.Address);
      pragma Import (C, Internal, "gtk_object_unref");
   begin
      if Get_Object (Object) /= System.Null_Address then
         Internal (Get_Object (Object));
      end if;
   end Unref;

end Gtk;
