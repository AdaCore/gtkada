with Gtk.Signal;

package body Gtk.Object is


   -----------------
   -- Destroy --
   -----------------

   procedure Destroy (Object : in out Gtk_Object'Class)
   is
      procedure Internal  (Object : in System.Address);
      pragma Import (C, Internal, "gtk_object_destroy");
   begin
      --  When the widget is destroyed, the callbacks are automatically
      --  destroyed too.
      --  In this binding, we have to make sure the memory allocated for
      --  the data is freed. The simple way is to call
      Gtk.Signal.Handlers_Destroy (Object);

      Internal (Get_Object (Object));
   end Destroy;


end Gtk.Object;
