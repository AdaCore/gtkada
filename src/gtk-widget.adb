package body Gtk.Widget is

   ----------------
   --  Activate  --
   ----------------

   procedure Activate (Widget : in Gtk_Widget'Class)
   is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_activate");
   begin
      Internal (Get_Object (Widget));
   end Activate;

   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (Widget : in out Gtk_Widget'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_destroy");
   begin
      Internal (Get_Object (Widget));
   end Destroy;

   ------------
   --  Hide  --
   ------------

   procedure Hide (Widget : in Gtk_Widget'Class)
   is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_hide");
   begin
      Internal (Get_Object (Widget));
   end Hide;

   ---------------------
   --  Set_Sensitive  --
   ---------------------

   procedure Set_Sensitive (Widget    : in Gtk_Widget'Class;
                            Sensitive : in Boolean)
   is
      procedure Internal (Widget      : in System.Address;
                          Sensitive : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_sensitive");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Sensitive));
   end Set_Sensitive;

   ------------
   --  Show  --
   ------------

   procedure Show (Widget : in Gtk_Widget'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show");
   begin
      Internal (Get_Object (Widget));
   end Show;

end Gtk.Widget;
