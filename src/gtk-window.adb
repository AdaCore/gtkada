package body Gtk.Window is

   -------------
   -- New --
   -------------

   procedure Gtk_New (Win      : out Gtk_Window;
                      The_Type : in  Window_Type) is
      function Internal (T : in Integer) return System.Address;
      pragma Import (C, Internal, "gtk_window_new");
   begin
      Set_Object (Win, Internal (Window_Type'Pos (The_Type)));
   end Gtk_New;

   -------------------
   -- Set_Title --
   -------------------

   procedure Set_Title (Win   : in Gtk_Window;
                        Title : in String) is
      procedure Internal (W : in System.Address;
                          T : in String);
      pragma Import (C, Internal, "gtk_window_set_title");
   begin
      Internal (Get_Object (Win), Title & Ascii.NUL);
   end Set_Title;


end Gtk.Window;
