
with Gtk.Frame;
with Gtk.Notebook;
with Gtk.Window;

package Main_Windows is

   type Main_Window_Record is new Gtk.Window.Gtk_Window_Record with
      record
         Notebook       : Gtk.Notebook.Gtk_Notebook;
      end record;
   type Main_Window is access all Main_Window_Record'Class;

   procedure Gtk_New (Win : out Main_Window);
   procedure Initialize (Win : access Main_Window_Record'Class);

private

   Gtk_Demo_Frame : Gtk.Frame.Gtk_Frame;
   --  Frame where the gtk demos should be displayed.

end Main_Windows;
