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

   function Get_Hadjustment
     (Scrolled_Window : in Gtk_Scrolled_Window'Class)
      return Adjustment.Gtk_Adjustment
   is
      function Internal (Scrolled_Window : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_hadjustment");
      Adjust : Adjustment.Gtk_Adjustment;
   begin
      Set_Object (Adjust, Internal (Get_Object (Scrolled_Window)));
      return Adjust;
   end Get_Hadjustment;


   ------------------------
   --  Get_Vadjustement  --
   ------------------------

   function Get_Vadjustment
     (Scrolled_Window : in Gtk_Scrolled_Window'Class)
      return               Adjustment.Gtk_Adjustment
   is
      function Internal (Scrolled_Window : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_vadjustment");
      Adjust : Adjustment.Gtk_Adjustment;
   begin
      Set_Object (Adjust, Internal (Get_Object (Scrolled_Window)));
      return Adjust;
   end Get_Vadjustment;


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
                         H_Scrollbar_Policy : in     Enums.Gtk_Policy_Type;
                         V_Scrollbar_Policy : in     Enums.Gtk_Policy_Type) is
      procedure Internal (Scrolled_Window : in System.Address;
                          H_Scrollbar_Policy : in Enums.Gtk_Policy_Type;
                          V_Scrollbar_Policy : in Enums.Gtk_Policy_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_policy");
   begin
      Internal (Get_Object (Scrolled_Window),
                H_Scrollbar_Policy,
                V_Scrollbar_Policy);
   end Set_Policy;


end Gtk.Scrolled_Window;
