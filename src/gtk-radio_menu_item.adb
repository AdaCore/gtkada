
package body Gtk.Radio_Menu_Item is

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Group : in Group_List) return System.Address is
   begin
      return Group.Ptr;
   end Get_Object;

   -----------
   -- Group --
   -----------

   function Group (Radio_Menu_Item : in Gtk_Radio_Menu_Item'Class)
                   return               Group_List
   is
      function Internal (Radio_Menu_Item : in System.Address)
                         return               System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_group");
      Group : Group_List;
   begin
      Set_Object (Group, Internal (Get_Object (Radio_Menu_Item)));
      return Group;
   end Group;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget : out Gtk_Radio_Menu_Item;
       Group  : in Group_List;
       Label  : in String)
   is
      function Internal
         (Group  : in System.Address;
          Label  : in String)
          return      System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_new_with_label");
   begin
      Set_Object (Widget, Internal (Get_Object (Group),
                                    Label & Ascii.NUL));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Radio_Menu_Item;
                      Group  : in Group_List)
   is
      function Internal (Group  : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Group)));
   end Gtk_New;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object (Group : out Group_List;
                         Value : in System.Address) is
   begin
      Group.Ptr := Value;
   end Set_Object;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
      (Radio_Menu_Item : in Gtk_Radio_Menu_Item'Class;
       Group           : in Group_List)
   is
      procedure Internal
         (Radio_Menu_Item : in System.Address;
          Group           : in System.Address);
      pragma Import (C, Internal, "gtk_radio_menu_item_set_group");
   begin
      Internal (Get_Object (Radio_Menu_Item), Get_Object (Group));
   end Set_Group;

end Gtk.Radio_Menu_Item;
