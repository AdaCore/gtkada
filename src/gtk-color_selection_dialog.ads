
with Gtk.Window;
with Gtk.Button;
with Gtk.Color_Selection;

package Gtk.Color_Selection_Dialog is

   type Gtk_Color_Selection_Dialog is new
     Gtk.Window.Gtk_Window with private;

   procedure Gtk_New (Widget : out Gtk_Color_Selection_Dialog);
   --  mapping: Gtk_New gtkcolorsel.h gtk_color_selection_dialog_new

   function Get_Colorsel (Dialog : in Gtk_Color_Selection_Dialog'Class)
                          return Gtk.Color_Selection.Gtk_Color_Selection;
   function Get_OK_Button (Dialog : in Gtk_Color_Selection_Dialog'Class)
                           return Gtk.Button.Gtk_Button;
   function Get_Reset_Button (Dialog : in Gtk_Color_Selection_Dialog'Class)
                              return Gtk.Button.Gtk_Button;
   function Get_Cancel_Button (Dialog : in  Gtk_Color_Selection_Dialog'Class)
                               return Gtk.Button.Gtk_Button;
   function Get_Help_Button (Dialog : in  Gtk_Color_Selection_Dialog'Class)
                             return Gtk.Button.Gtk_Button;
   --  Functions to get the fields of the dialog

   --  mapping: NOT_IMPLEMENTED gtkcolorsel.h \
   --  mapping:    gtk_color_selection_dialog_get_type


private

   type Gtk_Color_Selection_Dialog is new
     Gtk.Window.Gtk_Window with null record;

end Gtk.Color_Selection_Dialog;
