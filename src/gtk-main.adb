with Interfaces.C.Strings;
with Unchecked_Deallocation;
with Unchecked_Conversion;

package body Gtk.Main is

   package C renames Interfaces.C;


   ----------------
   -- Set_Locale --
   ----------------

   function Set_Locale return String is
      function Internal return C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_set_locale");
   begin
      return C.Strings.Value (Internal);
   end Set_Locale;


   ----------------
   -- Set_Locale --
   ----------------

   procedure Set_Locale is
      Dummy : constant String := Set_Locale;
      pragma Warnings (Off, Dummy);
   begin
      null;
   end Set_Locale;

   ----------
   -- Idle --
   ----------

   package body Idle is

      type Data_Type_Access is access Data_Type;
      type Cb_Record is
         record
            Func : Callback;
            Data : Data_Type_Access;
         end record;
      type Cb_Record_Access is access Cb_Record;

      procedure Free (D : in System.Address);
      pragma Convention (C, Free);

      function Convert is new Unchecked_Conversion
        (System.Address, Cb_Record_Access);

      function General_Cb (D : in System.Address) return Gint;
      pragma Convention (C, General_Cb);

      ----------
      -- Free --
      ----------

      procedure Free (D : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Cb_Record, Cb_Record_Access);
         procedure Internal2 is new Unchecked_Deallocation
           (Data_Type, Data_Type_Access);
         Data : Cb_Record_Access := Convert (D);
      begin
         Internal2 (Data.Data);
         Internal (Data);
      end Free;

      ----------------
      -- General_Cb --
      ----------------

      function General_Cb (D : in System.Address) return Gint is
         Data : Cb_Record_Access := Convert (D);
      begin
         return Boolean'Pos (Data.Func (Data.Data.all));
      end General_Cb;

      ---------
      -- Add --
      ---------

      function Add (Cb : in Callback;  D : in Data_Type) return Guint is
         function Internal (Priority : in Gint;
                            Func     : in System.Address;
                            Marshal  : in System.Address;
                            Data     : in System.Address;
                            Destroy  : in System.Address)
                            return        Guint;
         pragma Import (C, Internal, "gtk_idle_add_full");
         function Convert is new Unchecked_Conversion
           (Cb_Record_Access, System.Address);
         Data : Cb_Record_Access := new Cb_Record'(Func => Cb,
                                                   Data => new Data_Type'(D));
      begin
         return Internal (0, General_Cb'Address, System.Null_Address,
                          Convert (Data), Free'Address);
      end Add;

   end Idle;

   -------------
   -- Timeout --
   -------------

   package body Timeout is

      type Data_Type_Access is access Data_Type;
      type Cb_Record is
         record
            Func : Callback;
            Data : Data_Type_Access;
         end record;
      type Cb_Record_Access is access Cb_Record;

      procedure Free (D : in System.Address);
      pragma Convention (C, Free);

      function Convert is new Unchecked_Conversion
        (System.Address, Cb_Record_Access);

      function General_Cb (D : in System.Address) return Gint;
      pragma Convention (C, General_Cb);

      ----------
      -- Free --
      ----------

      procedure Free (D : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Cb_Record, Cb_Record_Access);
         procedure Internal2 is new Unchecked_Deallocation
           (Data_Type, Data_Type_Access);
         Data : Cb_Record_Access := Convert (D);
      begin
         Internal2 (Data.Data);
         Internal (Data);
      end Free;

      ----------------
      -- General_Cb --
      ----------------

      function General_Cb (D : in System.Address) return Gint is
         Data : Cb_Record_Access := Convert (D);
      begin
         return Boolean'Pos (Data.Func (Data.Data.all));
      end General_Cb;

      ---------
      -- Add --
      ---------

      function Add (Interval : in Guint32;
                    Func     : in Callback;
                    D : in Data_Type)
                    return Guint
      is
         function Internal (Interval : in Guint32;
                            Func     : in System.Address;
                            Marshal  : in System.Address;
                            Data     : in System.Address;
                            Destroy  : in System.Address)
                            return        Guint;
         pragma Import (C, Internal, "gtk_timeout_add_full");
         function Convert is new Unchecked_Conversion
           (Cb_Record_Access, System.Address);
         Data : Cb_Record_Access := new Cb_Record'(Func => Func,
                                                   Data => new Data_Type'(D));
      begin
         return Internal (Interval, General_Cb'Address, System.Null_Address,
                          Convert (Data), Free'Address);
      end Add;

   end Timeout;

end Gtk.Main;

