
package body Gtk.Radio_Button is

   use Widget_SList;

   -----------
   -- Group --
   -----------

   function Group (Button : in Gtk_Radio_Button) return Widget_SList.GSlist is
      function Internal (Button : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_group");
      Group : Widget_SList.GSlist;
   begin
      Set_Object (Group, Internal (Get_Object (Button)));
      return Group;
   end Group;

   -------------
   -- GtK_New --
   -------------

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in Widget_SList.GSlist)
   is
      function Internal (Group : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_new");
   begin
      Set_Object (Button, Internal (Get_Object (Group)));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in  Widget_SList.GSlist;
      Label  : in String)
   is
      function Internal (Group : in System.Address;
                         Label : in String) return System.Address;
      pragma Import (C, Internal,
                     "gtk_radio_button_new_with_label");
   begin
      Set_Object (Button, Internal (Get_Object (Group),
                                    Label & ASCII.NUL));
   end Gtk_New;

   -------------
   -- GtK_New --
   -------------

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in Gtk_Radio_Button)
   is
      function Internal (Group : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_new_from_widget");
   begin
      Set_Object (Button, Internal (Get_Object (Group)));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in Gtk_Radio_Button;
      Label  : in String)
   is
      function Internal (Group : in System.Address;
                         Label : in String) return System.Address;
      pragma Import (C, Internal,
                     "gtk_radio_button_new_with_label_from_widget");
   begin
      Set_Object (Button, Internal (Get_Object (Group),
                                    Label & ASCII.NUL));
   end Gtk_New;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group (Button : in Gtk_Radio_Button;
                        Group  : in Widget_SList.GSlist)
   is
      procedure Internal (Button : in System.Address;
                          Group  : in System.Address);
      pragma Import (C, Internal, "gtk_radio_button_set_group");
   begin
      Internal (Get_Object (Button), Get_Object (Group));
   end Set_Group;


end Gtk.Radio_Button;

