

package body Gtk.Scale is

   ----------------
   -- Draw_Value --
   ----------------

   procedure Draw_Value (Scale : in Gtk_Scale'Class)
   is
      procedure Internal (Scale : in System.Address);
      pragma Import (C, Internal, "gtk_scale_draw_value");
   begin
      Internal (Get_Object (Scale));
   end Draw_Value;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits
      (Scale      : in Gtk_Scale'Class;
       The_Digits : in Gint)
   is
      procedure Internal
         (Scale      : in System.Address;
          The_Digits : in Gint);
      pragma Import (C, Internal, "gtk_scale_set_digits");
   begin
      Internal (Get_Object (Scale),
                The_Digits);
   end Set_Digits;

   --------------------
   -- Set_Draw_Value --
   --------------------

   procedure Set_Draw_Value
      (Scale      : in Gtk_Scale'Class;
       Draw_Value : in Gint)
   is
      procedure Internal
         (Scale      : in System.Address;
          Draw_Value : in Gint);
      pragma Import (C, Internal, "gtk_scale_set_draw_value");
   begin
      Internal (Get_Object (Scale),
                Draw_Value);
   end Set_Draw_Value;

   -------------------
   -- Set_Value_Pos --
   -------------------

   procedure Set_Value_Pos
      (Scale : in Gtk_Scale'Class;
       Pos   : in Gtk_Position_Type)
   is
      procedure Internal
         (Scale : in System.Address;
          Pos   : in Gint);
      pragma Import (C, Internal, "gtk_scale_set_value_pos");
   begin
      Internal (Get_Object (Scale),
                Gtk_Position_Type'Pos (Pos));
   end Set_Value_Pos;

   -----------------
   -- Value_Width --
   -----------------

   function Value_Width (Scale  : in Gtk_Scale'Class)
                         return      Gint
   is
      function Internal (Scale  : in System.Address)
                         return      Gint;
      pragma Import (C, Internal, "gtk_scale_value_width");
   begin
      return Internal (Get_Object (Scale));
   end Value_Width;

end Gtk.Scale;
