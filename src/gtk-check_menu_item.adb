
package body Gtk.Check_Menu_Item is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Check_Menu_Item;
                      Label  : in String)
   is
      function Internal (Label  : in String)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_check_menu_item_new_with_label");
   begin
      Set_Object (Widget, Internal (Label & Ascii.NUL));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Check_Menu_Item)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_check_menu_item_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ---------------------
   -- Set_Show_Toggle --
   ---------------------

   procedure Set_Show_Toggle
      (Menu_Item : in Gtk_Check_Menu_Item'Class;
       Always    : in Boolean)
   is
      procedure Internal
         (Menu_Item : in System.Address;
          Always    : in Gint);
      pragma Import (C, Internal, "gtk_check_menu_item_set_show_toggle");
   begin
      Internal (Get_Object (Menu_Item),
                Boolean'Pos (Always));
   end Set_Show_Toggle;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
      (Check_Menu_Item : in Gtk_Check_Menu_Item'Class;
       State           : in Gint)
   is
      procedure Internal
         (Check_Menu_Item : in System.Address;
          State           : in Gint);
      pragma Import (C, Internal, "gtk_check_menu_item_set_state");
   begin
      Internal (Get_Object (Check_Menu_Item),
                State);
   end Set_State;

   -------------
   -- Toggled --
   -------------

   procedure Toggled (Check_Menu_Item : in Gtk_Check_Menu_Item'Class)
   is
      procedure Internal (Check_Menu_Item : in System.Address);
      pragma Import (C, Internal, "gtk_check_menu_item_toggled");
   begin
      Internal (Get_Object (Check_Menu_Item));
   end Toggled;

end Gtk.Check_Menu_Item;
