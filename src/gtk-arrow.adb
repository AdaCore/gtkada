

package body Gtk.Arrow is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget      : out Gtk_Arrow;
       Arrow_Type  : in Gtk_Arrow_Type;
       Shadow_Type : in Gtk_Shadow_Type)
   is
      function Internal
         (Arrow_Type  : in Gint;
          Shadow_Type : in Gint)
          return           System.Address;
      pragma Import (C, Internal, "gtk_arrow_new");
   begin
      Set_Object (Widget, Internal (Gtk_Arrow_Type'Pos (Arrow_Type),
                                    Gtk_Shadow_Type'Pos (Shadow_Type)));
   end Gtk_New;

   ---------
   -- Set --
   ---------

   procedure Set
      (Arrow       : in Gtk_Arrow'Class;
       Arrow_Type  : in Gtk_Arrow_Type;
       Shadow_Type : in Gtk_Shadow_Type)
   is
      procedure Internal
         (Arrow       : in System.Address;
          Arrow_Type  : in Gint;
          Shadow_Type : in Gint);
      pragma Import (C, Internal, "gtk_arrow_set");
   begin
      Internal (Get_Object (Arrow),
                Gtk_Arrow_Type'Pos (Arrow_Type),
                Gtk_Shadow_Type'Pos (Shadow_Type));
   end Set;

end Gtk.Arrow;
