package body Gtk.Widget is

   ------------------
   -- Activate --
   ------------------

   procedure Activate (Widg : in Widget'Class)
   is
      procedure Internal (Widg : in System.Address);
      pragma Import (C, Internal, "gtk_widget_activate");
   begin
      Internal (Get_Object (Widg));
   end Activate;

   ------------------
   --  Destroy --
   ------------------

   procedure Destroy (Widg : in Widget'Class) is
      procedure Internal (Widg : System.Address);
      pragma Import (C, Internal, "gtk_widget_destroy");
   begin
      Internal (Get_Object (Widg));
   end Destroy;

   --------------
   -- Hide --
   --------------

   procedure Hide (Widg : in Widget'Class)
   is
      procedure Internal (Widg : in System.Address);
      pragma Import (C, Internal, "gtk_widget_hide");
   begin
      Internal (Get_Object (Widg));
   end Hide;

   -----------------------
   -- Set_Sensitive --
   -----------------------

   procedure Set_Sensitive (Widg      : in Widget'Class;
                            Sensitive : in Boolean)
   is
      procedure Internal (Widg      : in System.Address;
                          Sensitive : in GInt);
      pragma Import (C, Internal, "gtk_widget_set_sensitive");
   begin
      Internal (Get_Object (Widg), Boolean'Pos (Sensitive));
   end Set_Sensitive;

   --------------
   -- Show --
   --------------

   procedure Show (Widg : in Widget'Class) is
      procedure Internal (Widg : System.Address);
      pragma Import (C, Internal, "gtk_widget_show");
   begin
      Internal (Get_Object (Widg));
   end Show;

end Gtk.Widget;
