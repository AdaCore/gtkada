package body Gtk.Window is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget   : out Gtk_Window;
                      The_Type : in  Gtk_Window_Type) is
      function Internal (T : in Integer) return System.Address;
      pragma Import (C, Internal, "gtk_window_new");
   begin
      Set_Object (Widget, Internal (Gtk_Window_Type'Pos (The_Type)));
   end Gtk_New;

   -------------------
   -- Gtk_Set_Title --
   -------------------

   procedure Gtk_Set_Title (Window : in Gtk_Window;
                            Title  : in String) is
      procedure Internal (W : in System.Address;
                          T : in String);
      pragma Import (C, Internal, "gtk_window_set_title");
   begin
      Internal (Get_Object (Window), Title & Ascii.NUL);
   end Gtk_Set_Title;


end Gtk.Window;
