

package body Gtk.Ruler is

   --------------
   -- Draw_Pos --
   --------------

   procedure Draw_Pos (Ruler : in Gtk_Ruler'Class)
   is
      procedure Internal (Ruler : in System.Address);
      pragma Import (C, Internal, "gtk_ruler_draw_pos");
   begin
      Internal (Get_Object (Ruler));
   end Draw_Pos;

   ----------------
   -- Draw_Ticks --
   ----------------

   procedure Draw_Ticks (Ruler : in Gtk_Ruler'Class)
   is
      procedure Internal (Ruler : in System.Address);
      pragma Import (C, Internal, "gtk_ruler_draw_ticks");
   begin
      Internal (Get_Object (Ruler));
   end Draw_Ticks;

   ---------------
   -- Get_Lower --
   ---------------

   function Get_Lower (Widget : in Gtk_Ruler'Class)
                       return      Gfloat
   is
      function Internal (Widget : in System.Address)
                         return      Gfloat;
      pragma Import (C, Internal, "ada_ruler_get_lower");
   begin
      return Internal (Get_Object (Widget));
   end Get_Lower;

   ------------------
   -- Get_Max_Size --
   ------------------

   function Get_Max_Size (Widget : in Gtk_Ruler'Class)
                          return      Gfloat
   is
      function Internal (Widget : in System.Address)
                         return      Gfloat;
      pragma Import (C, Internal, "ada_ruler_get_max_size");
   begin
      return Internal (Get_Object (Widget));
   end Get_Max_Size;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position (Widget : in Gtk_Ruler'Class)
                          return      Gfloat
   is
      function Internal (Widget : in System.Address)
                         return      Gfloat;
      pragma Import (C, Internal, "ada_ruler_get_position");
   begin
      return Internal (Get_Object (Widget));
   end Get_Position;

   ---------------
   -- Get_Upper --
   ---------------

   function Get_Upper (Widget : in Gtk_Ruler'Class)
                       return      Gfloat
   is
      function Internal (Widget : in System.Address)
                         return      Gfloat;
      pragma Import (C, Internal, "ada_ruler_get_upper");
   begin
      return Internal (Get_Object (Widget));
   end Get_Upper;

   --------------------
   -- Gtk_New_Hruler --
   --------------------

   procedure Gtk_New_Hruler (Widget : out Gtk_Ruler)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hruler_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New_Hruler;

   --------------------
   -- Gtk_New_Vruler --
   --------------------

   procedure Gtk_New_Vruler (Widget : out Gtk_Ruler)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vruler_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New_Vruler;

   ----------------
   -- Set_Metric --
   ----------------

   procedure Set_Metric
      (Ruler  : in Gtk_Ruler'Class;
       Metric : in Gtk_Metric_Type)
   is
      procedure Internal
         (Ruler  : in System.Address;
          Metric : in Gint);
      pragma Import (C, Internal, "gtk_ruler_set_metric");
   begin
      Internal (Get_Object (Ruler),
                Gtk_Metric_Type'Pos (Metric));
   end Set_Metric;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
      (Ruler    : in Gtk_Ruler'Class;
       Lower    : in Gfloat;
       Upper    : in Gfloat;
       Position : in Gfloat;
       Max_Size : in Gfloat)
   is
      procedure Internal
         (Ruler    : in System.Address;
          Lower    : in Gfloat;
          Upper    : in Gfloat;
          Position : in Gfloat;
          Max_Size : in Gfloat);
      pragma Import (C, Internal, "gtk_ruler_set_range");
   begin
      Internal (Get_Object (Ruler),
                Lower,
                Upper,
                Position,
                Max_Size);
   end Set_Range;

end Gtk.Ruler;

