

package body Gtk.Viewport is

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment (Viewport : in Gtk_Viewport'Class)
                             return        Gtk.Adjustment.Gtk_Adjustment'Class
   is
      function Internal (Viewport : in System.Address)
                         return        System.Address;
      pragma Import (C, Internal, "gtk_viewport_get_hadjustment");
      Widget : Gtk.Adjustment.Gtk_Adjustment;
   begin
      Set_Object (Widget, Internal (Get_Object (Viewport)));
      return Widget;
   end Get_Hadjustment;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment (Viewport : in Gtk_Viewport'Class)
                             return        Gtk.Adjustment.Gtk_Adjustment'Class
   is
      function Internal (Viewport : in System.Address)
                         return        System.Address;
      pragma Import (C, Internal, "gtk_viewport_get_vadjustment");
      Widget : Gtk.Adjustment.Gtk_Adjustment;
   begin
      Set_Object (Widget, Internal (Get_Object (Viewport)));
      return Widget;
   end Get_Vadjustment;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget      : out Gtk_Viewport;
       Hadjustment : in Gtk.Adjustment.Gtk_Adjustment'Class;
       Vadjustment : in Gtk.Adjustment.Gtk_Adjustment'Class)
   is
      function Internal
         (Hadjustment : in System.Address;
          Vadjustment : in System.Address)
          return           System.Address;
      pragma Import (C, Internal, "gtk_viewport_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Hadjustment),
                                    Get_Object (Vadjustment)));
   end Gtk_New;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Viewport   : in Gtk_Viewport'Class;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class)
   is
      procedure Internal
         (Viewport   : in System.Address;
          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_viewport_set_hadjustment");
   begin
      Internal (Get_Object (Viewport),
                Get_Object (Adjustment));
   end Set_Hadjustment;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
      (Viewport : in Gtk_Viewport'Class;
       The_Type : in Gtk_Shadow_Type)
   is
      procedure Internal
         (Viewport : in System.Address;
          The_Type : in Gint);
      pragma Import (C, Internal, "gtk_viewport_set_shadow_type");
   begin
      Internal (Get_Object (Viewport),
                Gtk_Shadow_Type'Pos (The_Type));
   end Set_Shadow_Type;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Viewport   : in Gtk_Viewport'Class;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class)
   is
      procedure Internal
         (Viewport   : in System.Address;
          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_viewport_set_vadjustment");
   begin
      Internal (Get_Object (Viewport),
                Get_Object (Adjustment));
   end Set_Vadjustment;

end Gtk.Viewport;
