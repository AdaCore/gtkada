with Unchecked_Conversion;
with Unchecked_Deallocation;

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

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Object : in Gtk_Object'Class) return Gint is
      function Internal (Object : in System.Address) return Gint;
      pragma Import (C, Internal, "ada_object_get_type");
   begin
      return Internal (Get_Object (Object));
   end Get_Type;

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

   ---------------
   -- User_Data --
   ---------------

   package body User_Data is
      type Data_Access is access all Data_Type;
      type Cb_Record is
         record
            Ptr      : Data_Access;
         end record;
      type Cb_Record_Access is access all Cb_Record;

      function Convert is new Unchecked_Conversion (System.Address,
                                                    Cb_Record_Access);
      procedure Free (Data : in System.Address);
      pragma Convention (C, Free);

      ----------
      -- Free --
      ----------

      procedure Free (Data : in System.Address) is
         procedure Internal is new Unchecked_Deallocation (Cb_Record,
                                                           Cb_Record_Access);
         procedure Internal2 is new Unchecked_Deallocation (Data_Type,
                                                            Data_Access);
         D : Cb_Record_Access := Convert (Data);
      begin
         Internal2 (D.Ptr);
         Internal (D);
      end Free;

      ---------
      -- Get --
      ---------

      function Get (Object : in Gtk_Object'Class;
                    Id     : in String := "user_data") return Data_Type is
         function Internal (Object : in System.Address;
                            Key    : in String)
                            return System.Address;
         pragma Import (C, Internal, "gtk_object_get_data");
         D : Cb_Record_Access
           := Convert (Internal (Get_Object (Object),
                                 Id & Ascii.NUL));
      begin
         if D = null then
            raise Constraint_Error;
         end if;
         return D.Ptr.all;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (Object : in Gtk_Object'Class;
                     Data   : in Data_Type;
                     Id     : in String := "user_data")
      is
         function Convert is new Unchecked_Conversion (Cb_Record_Access,
                                                       System.Address);
         procedure Internal (Object  : in System.Address;
                             Key     : in String;
                             Data    : in System.Address;
                             Destroy : in System.Address);
         pragma Import (C, Internal, "gtk_object_set_data_full");
         D : Cb_Record_Access := new Cb_Record'(Ptr => new Data_Type'(Data));
      begin
         Internal (Get_Object (Object),
                   Id & Ascii.NUL,
                   Convert (D),
                   Free'Address);
      end Set;
   end User_Data;

end Gtk.Object;
