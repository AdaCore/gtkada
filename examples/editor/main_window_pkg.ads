with Gtk.Window; use Gtk.Window;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Box; use Gtk.Box;
with Gtk.Container; use Gtk.Container;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Button; use Gtk.Button;

package Main_Window_Pkg is

   type Main_Window_Record is new Gtk_Window_Record with record
      Vbox1 : Gtk_Vbox;
      Menubar1 : Gtk_Menu_Bar;
      File : Gtk_Menu_Item;
      File_Menu : Gtk_Menu;
      New1 : Gtk_Menu_Item;
      Open : Gtk_Menu_Item;
      Save : Gtk_Menu_Item;
      Save_As : Gtk_Menu_Item;
      Separator1 : Gtk_Menu_Item;
      Quit : Gtk_Menu_Item;
      Edit : Gtk_Menu_Item;
      Edit_Menu : Gtk_Menu;
      Cut : Gtk_Menu_Item;
      Copy : Gtk_Menu_Item;
      Paste : Gtk_Menu_Item;
      Delete : Gtk_Menu_Item;
      Help : Gtk_Menu_Item;
      Help_Menu : Gtk_Menu;
      About : Gtk_Menu_Item;
      Toolbar1 : Gtk_Toolbar;
      Button1 : Gtk_Button;
      Button2 : Gtk_Button;
      Button3 : Gtk_Button;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Text1 : Gtk_Text;
      Statusbar1 : Gtk_Statusbar;
   end record;
   type Main_Window_Access is access all Main_Window_Record'Class;

   procedure Gtk_New (Main_Window : out Main_Window_Access);
   procedure Initialize (Main_Window : access Main_Window_Record'Class);

   Main_Window : Main_Window_Access;

end Main_Window_Pkg;
