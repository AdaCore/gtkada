
with Gtk.Bin;

package Gtk.Window is

   type Gtk_Window_Type is (Gtk_Window_Toplevel,
                            Gtk_Window_Dialog,
                            Gtk_Window_Popup);

   type Gtk_Window is new Gtk.Bin.Gtk_Bin with private;

   procedure Gtk_New (Widget   : out Gtk_Window;
                      The_Type : in  Gtk_Window_Type);

   procedure Gtk_Set_Title (Window : in Gtk_Window;
                            Title  : in String);

   --  mapping: Gtk_Window_Type gtkenums.h GtkWindowType
   --  mapping: Gtk_New gtkwindow.h gtk_window_new
   --  mapping: Gtk_Set_Title gtkwindow.h gtk_window_set_title

private

   type Gtk_Window is new Gtk.Bin.Gtk_Bin with null record;

end Gtk.Window;
