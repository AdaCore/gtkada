package body Gtk.Object is

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Gtk_Object) is
   begin
      Ref (Object);
   end Adjust;

   -----------------
   --  Connected  --
   -----------------

   function Connected (Object : in Gtk_Object'Class) return Boolean is
      function Internal (Object : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_object_connected");
   begin
      return To_Boolean (Internal (Get_Object (Object)));
   end Connected;


   -----------------
   -- Destroy --
   -----------------

   procedure Destroy (Object : in out Gtk_Object'Class)
   is
      procedure Internal  (Object : in System.Address);
      pragma Import (C, Internal, "gtk_object_destroy");
   begin
      Internal (Get_Object (Object));
   end Destroy;

   ----------------
   --  Destroyed --
   ----------------

   function Destroyed (Object : in Gtk_Object'Class) return Boolean is
      function Internal (Object : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_object_destroyed");
   begin
      return To_Boolean (Internal (Get_Object (Object)));
   end Destroyed;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Gtk_Object) is
   begin
      Unref (Object);
   end Finalize;

   -------------
   --  Flags  --
   -------------

   function Flags (Object : in Gtk_Object'Class) return Guint32 is
      function Internal (Object : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_object_flags");
   begin
      return Internal (Get_Object (Object));
   end Flags;


   ----------------
   --  Floating  --
   ----------------

   function Floating (Object : in Gtk_Object'Class) return Boolean is
      function Internal (Object : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_object_floating");
   begin
      return To_Boolean (Internal (Get_Object (Object)));
   end Floating;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Gtk_Object) is
   begin
      Ref (Object);
   end Initialize;

   ------------------
   --  Is_Created  --
   ------------------

   function Is_Created (Object : in Gtk_Object) return Boolean is
      use type System.Address;
      function Destroyed (Object : in System.Address) return Guint32;
      pragma Import (C, Destroyed, "ada_object_destroyed");
   begin
      return Get_Object (Object) /= System.Null_Address
        and then not To_Boolean (Destroyed (Get_Object (Object)));
   end Is_Created;

   ---------
   -- Ref --
   ---------

   procedure Ref (Object : in out Gtk_Object) is
      procedure Internal (Object : in System.Address);
      pragma Import (C, Internal, "gtk_object_ref");
      use type System.Address;
   begin
      if Get_Object (Object) /= System.Null_Address then
         Internal (Get_Object (Object));
      end if;
   end Ref;


   -----------------
   --  Set_Flags  --
   -----------------

   procedure Set_Flags (Object : in out Gtk_Object'Class;
                        Flags  : in     Guint32) is
      procedure Internal (Object : in System.Address;
                          Flags  : in Guint32);
      pragma Import (C, Internal, "ada_object_set_flags");
   begin
      Internal (Get_Object (Object), Flags);
   end Set_Flags;

   -----------
   -- Unref --
   -----------

   procedure Unref (Object : in out Gtk_Object) is
      procedure Internal (Object : in System.Address);
      pragma Import (C, Internal, "gtk_object_unref");
      use type System.Address;
   begin
      if Get_Object (Object) /= System.Null_Address then
         Internal (Get_Object (Object));
      end if;
   end Unref;

   -------------------
   --  Unset_Flags  --
   -------------------

   procedure Unset_Flags (Object : in out Gtk_Object'Class;
                          Flags  : in     Guint32) is
      procedure Internal (Object : in System.Address;
                          Flags  : in Guint32);
      pragma Import (C, Internal, "ada_object_unset_flags");
   begin
      Internal (Get_Object (Object), Flags);
   end Unset_Flags;

end Gtk.Object;
