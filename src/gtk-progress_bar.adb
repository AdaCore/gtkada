

package body Gtk.Progress_Bar is

   --------------------
   -- Get_Percentage --
   --------------------

   function Get_Percentage (Widget : in Gtk_Progress_Bar'Class)
                            return      Gfloat
   is
      function Internal (Widget : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_progress_bar_get_percentage");
   begin
      return Internal (Get_Object (Widget));
   end Get_Percentage;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Progress_Bar)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_progress_bar_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ------------
   -- Update --
   ------------

   procedure Update
      (Pbar       : in Gtk_Progress_Bar'Class;
       Percentage : in Gfloat)
   is
      procedure Internal
         (Pbar       : in System.Address;
          Percentage : in Gfloat);
      pragma Import (C, Internal, "gtk_progress_bar_update");
   begin
      Internal (Get_Object (Pbar),
                Percentage);
   end Update;

end Gtk.Progress_Bar;
