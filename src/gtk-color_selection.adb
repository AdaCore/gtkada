
package body Gtk.Color_Selection is

   ---------------
   -- Get_Color --
   ---------------

   procedure Get_Color (Colorsel : in Gtk_Color_Selection'Class;
                        Color    : out Color_Array)
   is
      procedure Internal (Colorsel : in System.Address;
                          Color    : out Color_Array);
      pragma Import (C, Internal, "gtk_color_selection_get_color");
   begin
      Color (Opacity) := 0.0;
      Internal (Get_Object (Colorsel), Color);
   end Get_Color;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Color_Selection) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_color_selection_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color (Colorsel : in Gtk_Color_Selection'Class;
                        Color    : in Color_Array)
   is
      procedure Internal (Colorsel : in System.Address;
                          Color    : in System.Address);
      pragma Import (C, Internal, "gtk_color_selection_set_color");
   begin
      Internal (Get_Object (Colorsel), Color'Address);
   end Set_Color;

   -----------------
   -- Set_Opacity --
   -----------------

   procedure Set_Opacity (Colorsel    : in Gtk_Color_Selection'Class;
                          Use_Opacity : in Boolean)
   is
      procedure Internal (Colorsel    : in System.Address;
                          Use_Opacity : in Gint);
      pragma Import (C, Internal, "gtk_color_selection_set_opacity");
   begin
      Internal (Get_Object (Colorsel), Boolean'Pos (Use_Opacity));
   end Set_Opacity;

   -----------------------
   -- Set_Update_Policy --
   -----------------------

   procedure Set_Update_Policy (Colorsel : in Gtk_Color_Selection'Class;
                                Policy   : in Enums.Gtk_Update_Type)
   is
      procedure Internal (Colorsel : in System.Address;
                          Policy   : in Gint);
      pragma Import (C, Internal, "gtk_color_selection_set_update_policy");
   begin
      Internal (Get_Object (Colorsel), Enums.Gtk_Update_Type'Pos (Policy));
   end Set_Update_Policy;

end Gtk.Color_Selection;
