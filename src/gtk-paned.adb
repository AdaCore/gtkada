

package body Gtk.Paned is

   ----------
   -- Add1 --
   ----------

   procedure Add1
      (Paned : in Gtk_Paned'Class;
       Child : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal
         (Paned : in System.Address;
          Child : in System.Address);
      pragma Import (C, Internal, "gtk_paned_add1");
   begin
      Internal (Get_Object (Paned),
                Get_Object (Child));
   end Add1;

   ----------
   -- Add2 --
   ----------

   procedure Add2
      (Paned : in Gtk_Paned'Class;
       Child : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal
         (Paned : in System.Address;
          Child : in System.Address);
      pragma Import (C, Internal, "gtk_paned_add2");
   begin
      Internal (Get_Object (Paned),
                Get_Object (Child));
   end Add2;

   -----------------
   -- Gutter_Size --
   -----------------

   procedure Gutter_Size
      (Paned : in Gtk_Paned'Class;
       Size  : in Guint16)
   is
      procedure Internal
         (Paned : in System.Address;
          Size  : in Gint);
      pragma Import (C, Internal, "gtk_paned_gutter_size");
   begin
      Internal (Get_Object (Paned),
                Guint16'Pos (Size));
   end Gutter_Size;

   -----------------
   -- Handle_Size --
   -----------------

   procedure Handle_Size
      (Paned : in Gtk_Paned'Class;
       Size  : in Guint16)
   is
      procedure Internal
         (Paned : in System.Address;
          Size  : in Gint);
      pragma Import (C, Internal, "gtk_paned_handle_size");
   begin
      Internal (Get_Object (Paned),
                Guint16'Pos (Size));
   end Handle_Size;

end Gtk.Paned;
