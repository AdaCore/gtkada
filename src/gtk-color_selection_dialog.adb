
package body Gtk.Color_Selection_Dialog is

   ------------------
   -- Get_Colorsel --
   ------------------

   function Get_Colorsel (Dialog : in Gtk_Color_Selection_Dialog'Class)
                          return Gtk.Color_Selection.Gtk_Color_Selection
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_colorsel");
      Tmp : Gtk.Color_Selection.Gtk_Color_Selection;
   begin
      Set_Object (Tmp, Internal (Get_Object (Dialog)));
      return Tmp;
   end Get_Colorsel;

   -------------------
   -- Get_OK_Button --
   -------------------

   function Get_OK_Button (Dialog : in Gtk_Color_Selection_Dialog'Class)
                           return Gtk.Button.Gtk_Button
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_ok_button");
      Tmp : Gtk.Button.Gtk_Button;
   begin
      Set_Object (Tmp, Internal (Get_Object (Dialog)));
      return Tmp;
   end Get_OK_Button;

   ----------------------
   -- Get_Reset_Button --
   ----------------------

   function Get_Reset_Button (Dialog : in Gtk_Color_Selection_Dialog'Class)
                              return Gtk.Button.Gtk_Button
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_reset_button");
      Tmp : Gtk.Button.Gtk_Button;
   begin
      Set_Object (Tmp, Internal (Get_Object (Dialog)));
      return Tmp;
   end Get_Reset_Button;

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button (Dialog : in  Gtk_Color_Selection_Dialog'Class)
                               return Gtk.Button.Gtk_Button
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_cancel_button");
      Tmp : Gtk.Button.Gtk_Button;
   begin
      Set_Object (Tmp, Internal (Get_Object (Dialog)));
      return Tmp;
   end Get_Cancel_Button;

   ---------------------
   -- Get_Help_Button --
   ---------------------

   function Get_Help_Button (Dialog : in  Gtk_Color_Selection_Dialog'Class)
                             return Gtk.Button.Gtk_Button
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_help_button");
      Tmp : Gtk.Button.Gtk_Button;
   begin
      Set_Object (Tmp, Internal (Get_Object (Dialog)));
      return Tmp;
   end Get_Help_Button;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Color_Selection_Dialog) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_color_selection_dialog_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

end Gtk.Color_Selection_Dialog;
