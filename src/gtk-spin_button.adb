

package body Gtk.Spin_Button is

   ---------------
   -- Construct --
   ---------------

   procedure Construct
      (Spin_Button : in Gtk_Spin_Button'Class;
       Adjustment  : in Gtk.Adjustment.Gtk_Adjustment'Class;
       Climb_Rate  : in Gfloat;
       The_Digits  : in Gint)
   is
      procedure Internal
         (Spin_Button : in System.Address;
          Adjustment  : in System.Address;
          Climb_Rate  : in Gfloat;
          The_Digits  : in Gint);
      pragma Import (C, Internal, "gtk_spin_button_construct");
   begin
      Internal (Get_Object (Spin_Button),
                Get_Object (Adjustment),
                Climb_Rate,
                The_Digits);
   end Construct;

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment (Spin_Button : in Gtk_Spin_Button'Class)
                            return      Gtk.Adjustment.Gtk_Adjustment'Class
   is
      function Internal (Spin_Button : in System.Address)
                         return           System.Address;
      pragma Import (C, Internal, "gtk_spin_button_get_adjustment");
      Widget : Gtk.Adjustment.Gtk_Adjustment;
   begin
      Set_Object (Widget, Internal (Get_Object (Spin_Button)));
      return Widget;
   end Get_Adjustment;

   ------------------------
   -- Get_Value_As_Float --
   ------------------------

   function Get_Value_As_Float (Spin_Button : in Gtk_Spin_Button'Class)
                                return           Gfloat
   is
      function Internal (Spin_Button : in System.Address)
                         return           Gfloat;
      pragma Import (C, Internal, "gtk_spin_button_get_value_as_float");
   begin
      return Internal (Get_Object (Spin_Button));
   end Get_Value_As_Float;

   ----------------------
   -- Get_Value_As_Int --
   ----------------------

   function Get_Value_As_Int (Spin_Button : in Gtk_Spin_Button'Class)
                              return           Gint
   is
      function Internal (Spin_Button : in System.Address)
                         return           Gint;
      pragma Import (C, Internal, "gtk_spin_button_get_value_as_int");
   begin
      return Internal (Get_Object (Spin_Button));
   end Get_Value_As_Int;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget     : out Gtk_Spin_Button;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class;
       Climb_Rate : in Gfloat;
       The_Digits : in Gint)
   is
      function Internal
         (Adjustment : in System.Address;
          Climb_Rate : in Gfloat;
          The_Digits : in Gint)
          return          System.Address;
      pragma Import (C, Internal, "gtk_spin_button_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Adjustment),
                                    Climb_Rate,
                                    The_Digits));
   end Gtk_New;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
      (Spin_Button : in Gtk_Spin_Button'Class;
       Adjustment  : in Gtk.Adjustment.Gtk_Adjustment'Class)
   is
      procedure Internal
         (Spin_Button : in System.Address;
          Adjustment  : in System.Address);
      pragma Import (C, Internal, "gtk_spin_button_set_adjustment");
   begin
      Internal (Get_Object (Spin_Button),
                Get_Object (Adjustment));
   end Set_Adjustment;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits
      (Spin_Button : in Gtk_Spin_Button'Class;
       The_Digits  : in Gint)
   is
      procedure Internal
         (Spin_Button : in System.Address;
          The_Digits  : in Gint);
      pragma Import (C, Internal, "gtk_spin_button_set_digits");
   begin
      Internal (Get_Object (Spin_Button),
                The_Digits);
   end Set_Digits;

   -----------------
   -- Set_Numeric --
   -----------------

   procedure Set_Numeric
      (Spin_Button : in Gtk_Spin_Button'Class;
       Numeric     : in Gint)
   is
      procedure Internal
         (Spin_Button : in System.Address;
          Numeric     : in Gint);
      pragma Import (C, Internal, "gtk_spin_button_set_numeric");
   begin
      Internal (Get_Object (Spin_Button),
                Numeric);
   end Set_Numeric;

   -----------------------
   -- Set_Update_Policy --
   -----------------------

   procedure Set_Update_Policy
      (Spin_Button : in Gtk_Spin_Button'Class;
       Policy      : in Gtk_Spin_Button_Update_Policy)
   is
      procedure Internal
         (Spin_Button : in System.Address;
          Policy      : in Gint);
      pragma Import (C, Internal, "gtk_spin_button_set_update_policy");
   begin
      Internal (Get_Object (Spin_Button),
                Gtk_Spin_Button_Update_Policy'Pos (Policy));
   end Set_Update_Policy;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
      (Spin_Button : in Gtk_Spin_Button'Class;
       Value       : in Gfloat)
   is
      procedure Internal
         (Spin_Button : in System.Address;
          Value       : in Gfloat);
      pragma Import (C, Internal, "gtk_spin_button_set_value");
   begin
      Internal (Get_Object (Spin_Button),
                Value);
   end Set_Value;

   --------------
   -- Set_Wrap --
   --------------

   procedure Set_Wrap
      (Spin_Button : in Gtk_Spin_Button'Class;
       Wrap        : in Gint)
   is
      procedure Internal
         (Spin_Button : in System.Address;
          Wrap        : in Gint);
      pragma Import (C, Internal, "gtk_spin_button_set_wrap");
   begin
      Internal (Get_Object (Spin_Button),
                Wrap);
   end Set_Wrap;

   ----------
   -- Spin --
   ----------

   procedure Spin
      (Spin_Button : in Gtk_Spin_Button'Class;
       Direction   : in Guint;
       Step        : in Gfloat)
   is
      procedure Internal
         (Spin_Button : in System.Address;
          Direction   : in Guint;
          Step        : in Gfloat);
      pragma Import (C, Internal, "gtk_spin_button_spin");
   begin
      Internal (Get_Object (Spin_Button),
                Direction,
                Step);
   end Spin;

end Gtk.Spin_Button;
