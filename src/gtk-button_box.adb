package body Gtk.Button_Box is

   ------------------------
   -- Get_Child_Ipadding --
   ------------------------

   procedure Get_Child_Ipadding (Widget : in Gtk_Button_Box'Class;
                                 Ipad_X : out Gint;
                                 Ipad_Y : out Gint)
   is
      procedure Internal (Widget : in System.Address;
                          Ipad_X : out Gint;
                          Ipad_Y : out Gint);
      pragma Import (C, Internal, "gtk_button_box_get_child_ipadding");
   begin
      Internal (Get_Object (Widget), Ipad_X, Ipad_Y);
   end Get_Child_Ipadding;

   --------------------------------
   -- Get_Child_Ipadding_Default --
   --------------------------------

   procedure Get_Child_Ipadding_Default (Ipad_X : out Gint;
                                         Ipad_Y : out Gint)
   is
      procedure Internal (Ipad_X : out Gint;
                          Ipad_Y : out Gint);
      pragma Import (C, Internal, "gtk_button_box_get_child_ipadding_default");
   begin
      Internal (Ipad_X, Ipad_Y);
   end Get_Child_Ipadding_Default;

   --------------------
   -- Get_Child_Size --
   --------------------

   procedure Get_Child_Size (Widget     : in Gtk_Button_Box'Class;
                             Min_Width  : out Gint;
                             Min_Height : out Gint)
   is
      procedure Internal (Widget     : in System.Address;
                          Min_Width  : out Gint;
                          Min_Height : out Gint);
      pragma Import (C, Internal, "gtk_button_box_get_child_size");
   begin
      Internal (Get_Object (Widget), Min_Width, Min_Height);
   end Get_Child_Size;

   ----------------------------
   -- Get_Child_Size_Default --
   ----------------------------

   procedure Get_Child_Size_Default (Min_Width  : out Gint;
                                     Min_Height : out Gint)
   is
      procedure Internal (Min_Width  : out Gint;
                          Min_Height : out Gint);
      pragma Import (C, Internal, "gtk_button_box_get_child_size_default");
   begin
      Internal (Min_Width, Min_Height);
   end Get_Child_Size_Default;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout (Widget : in Gtk_Button_Box'Class)
                        return Style
   is
      function Internal (Widget : in System.Address)
                         return Gint;
      pragma Import (C, Internal, "gtk_button_box_get_layout");
   begin
      return Style'Val (Internal (Get_Object (Widget)));
   end Get_Layout;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing (Widget : in Gtk_Button_Box'Class)
                         return Gint
   is
      function Internal (Widget : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_button_box_get_spacing");
   begin
      return Internal (Get_Object (Widget));
   end Get_Spacing;

   ------------------------
   -- Set_Child_Ipadding --
   ------------------------

   procedure Set_Child_Ipadding (Widget : in Gtk_Button_Box'Class;
                                 Ipad_X : in Gint;
                                 Ipad_Y : in Gint)
   is
      procedure Internal (Widget : in System.Address;
                          Ipad_X : in Gint;
                          Ipad_Y : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_child_ipadding");
   begin
      Internal (Get_Object (Widget), Ipad_X, Ipad_Y);
   end Set_Child_Ipadding;

   --------------------------------
   -- Set_Child_Ipadding_Default --
   --------------------------------

   procedure Set_Child_Ipadding_Default (Ipad_X : in Gint;
                                         Ipad_Y : in Gint)
   is
      procedure Internal (Ipad_X : in Gint;
                          Ipad_Y : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_child_ipadding_default");
   begin
      Internal (Ipad_X, Ipad_Y);
   end Set_Child_Ipadding_Default;

   --------------------
   -- Set_Child_Size --
   --------------------

   procedure Set_Child_Size (Widget     : in Gtk_Button_Box'Class;
                             Min_Width  : in Gint;
                             Min_Height : in Gint)
   is
      procedure Internal (Widget     : in System.Address;
                          Min_Width  : in Gint;
                          Min_Height : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_child_size");
   begin
      Internal (Get_Object (Widget), Min_Width, Min_Height);
   end Set_Child_Size;

   ----------------------------
   -- Set_Child_Size_Default --
   ----------------------------

   procedure Set_Child_Size_Default (Min_Width  : in Gint;
                                     Min_Height : in Gint)
   is
      procedure Internal (Min_Width  : in Gint;
                          Min_Height : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_child_size_default");
   begin
      Internal (Min_Width, Min_Height);
   end Set_Child_Size_Default;

   ----------------
   -- Set_Layout --
   ----------------

   procedure Set_Layout (Widget       : in Gtk_Button_Box'Class;
                         Layout_Style : in Style)
   is
      procedure Internal (Widget       : in System.Address;
                          Layout_Style : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_layout");
   begin
      Internal (Get_Object (Widget), Style'Pos (Layout_Style));
   end Set_Layout;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing (Widget  : in Gtk_Button_Box'Class;
                          Spacing : in Gint)
   is
      procedure Internal (Widget  : in System.Address;
                          Spacing : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_spacing");
   begin
      Internal (Get_Object (Widget), Spacing);
   end Set_Spacing;

end Gtk.Button_Box;
