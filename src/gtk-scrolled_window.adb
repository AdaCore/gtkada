package body Gtk.Scrolled_Window is


   -----------------
   --  Construct  --
   -----------------

   procedure Construct
     (Scrolled_Window : in out Gtk_Scrolled_Window'Class;
      Hadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment;
      Vadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment) is
      procedure Internal (Scrolled_Window : in System.Address;
                          Hadjustment, Vadjustment : in System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_construct");
   begin
      Internal (Get_Object (Scrolled_Window),
                Get_Object (Hadjustment), Get_Object (Vadjustment));
   end Construct;


   ------------------------
   --  Get_Hadjustement  --
   ------------------------

   procedure Get_Hadjustement
     (Scrolled_Window : in     Gtk_Scrolled_Window'Class;
      Hadjustment     :    out Adjustment.Gtk_adjustment'Class) is
      function Internal (Scrolled_Window : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_hadjustment");
   begin
      Set_Object (Hadjustment, Internal (Get_Object (Scrolled_Window)));
   end Get_Hadjustement;


   ------------------------
   --  Get_Vadjustement  --
   ------------------------

   procedure Get_Vadjustement
     (Scrolled_Window : in     Gtk_Scrolled_Window'Class;
      Vadjustment     :    out Adjustment.Gtk_adjustment'Class) is
      function Internal (Scrolled_Window : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_vadjustment");
   begin
      Set_Object (Vadjustment, Internal (Get_Object (Scrolled_Window)));
   end Get_Vadjustement;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New
     (Scrolled_Window :    out Gtk_Scrolled_Window'Class;
      Hadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment;
      Vadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment) is
      function Internal (Hadjustment, Vadjustment : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_new");
   begin
      Set_Object (Scrolled_Window, Internal (Get_Object (Hadjustment),
                                             Get_Object (Vadjustment)));
   end Gtk_New;

   procedure Set_Policy (Scrolled_Window    : in out Gtk_Scrolled_Window'Class;
                         H_Scrollbar_Policy : in     Enums.Policy_Type;
                         V_Scrollbar_Policy : in     Enums.Policy_Type) is
      procedure Internal (Scrolled_Window : in System.Address;
                          H_Scrollbar_Policy : in Enums.Policy_Type;
                          V_Scrollbar_Policy : in Enums.Policy_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_policy");
   begin
      Internal (Get_Object (Scrolled_Window),
                H_Scrollbar_Policy,
                V_Scrollbar_Policy);
   end Set_Policy;


end Gtk.Scrolled_Window;
