
package body Gtk.Dialog is

   ---------------------
   -- Get_Action_Area --
   ---------------------

   function Get_Action_Area (Widget : in Gtk_Dialog'Class)
                             return      Gtk.Box.Gtk_Box
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_dialog_get_action_area");
      Tmp : Gtk.Box.Gtk_Box;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Action_Area;

   --------------
   -- Get_Vbox --
   --------------

   function Get_Vbox (Widget : in Gtk_Dialog'Class)
                      return      Gtk.Box.Gtk_Box
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_dialog_get_vbox");
      Tmp : Gtk.Box.Gtk_Box;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Vbox;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Dialog)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_dialog_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

end Gtk.Dialog;

