package body Gtk.Option_Menu is


   ----------------
   --  Get_Menu  --
   ----------------

   procedure Get_Menu (Option_Menu : in  Gtk_Option_Menu'Class;
                       Menu        : out Widget.Gtk_Widget'Class) is
      function Internal (Option_Menu : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_option_menu_get_menu");
   begin
      Set_Object (Menu, Internal (Get_Object (Option_Menu)));
   end Get_Menu;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Option_Menu : out Gtk_Option_Menu) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_option_menu_new");
   begin
      Set_Object (Option_Menu, Internal);
   end Gtk_New;


   -------------------
   --  Remove_Menu  --
   -------------------

   procedure Remove_Menu (Option_Menu : in out Gtk_Option_Menu'Class;
                          Menu        : in     Widget.Gtk_Widget'Class) is
      procedure Internal (Option_Menu, Menu : in System.Address);
      pragma Import (C, Internal, "gtk_option_menu_remove_menu");
   begin
      Internal (Get_Object (Option_Menu), Get_Object (Menu));
   end Remove_Menu;


   -------------------
   --  Set_History  --
   -------------------

   procedure Set_History (Option_Menu : in out Gtk_Option_Menu'Class;
                          Index       : in     Gint) is
      procedure Internal (Option_Menu : in System.Address; Index : in Gint);
      pragma Import (C, Internal, "gtk_option_menu_set_history");
   begin
      Internal (Get_Object (Option_Menu), Index);
   end Set_History;


   ----------------
   --  Set_Menu  --
   ----------------

   procedure Set_Menu (Option_Menu : in out Gtk_Option_Menu'Class;
                       Menu        : in     Widget.Gtk_Widget'Class) is
      procedure Internal (Option_Menu, Menu : in System.Address);
      pragma Import (C, Internal, "gtk_option_menu_set_menu");
   begin
      Internal (Get_Object (Option_Menu), Get_Object (Menu));
   end Set_Menu;


end Gtk.Option_Menu;
