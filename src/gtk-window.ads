
with Gtk.Bin;

package Gtk.Window is

   type Window_Type is (Window_Toplevel,
                            Window_Dialog,
                            Window_Popup);

   type Window is new Gtk.Bin.Bin with private;

   procedure Gtk_New (Win      : out Window;
                      The_Type : in  Window_Type);

   procedure Set_Title (Win   : in Window;
                        Title : in String);

   --  mapping: Window_Type gtkenums.h GtkWindowType
   --  mapping: New gtkwindow.h gtk_window_new
   --  mapping: Set_Title gtkwindow.h gtk_window_set_title

private

   type Window is new Gtk.Bin.Bin with null record;

end Gtk.Window;
