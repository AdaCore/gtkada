package body Gtk.Widget is

   --------------
   -- Gtk_Show --
   --------------

   procedure Gtk_Show (Widget : in Gtk_Widget'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show");
   begin
      Internal (Get_Object (Widget));
   end Gtk_Show;

   ------------------
   --  Gtk_Destroy --
   ------------------

   procedure Gtk_Destroy (Widget : in Gtk_Widget'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_destroy");
   begin
      Internal (Get_Object (Widget));
   end Gtk_Destroy;
end Gtk.Widget;
