package body About_Dialog_Pkg.Callbacks is

   ---------------------------
   --  On_About_Ok_Clicked  --
   ---------------------------

   procedure On_About_Ok_Clicked
     (Object : access Gtk_Button_Record) is
   begin
      Hide (About_Dialog);
   end On_About_Ok_Clicked;

end About_Dialog_Pkg.Callbacks;
