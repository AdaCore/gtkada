
with Glib;
with Gtk; use Gtk;
with Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Button; use Gtk.Button;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk.Pixmap; use Gtk.Pixmap;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Callbacks_Gladeedit; use Callbacks_Gladeedit;
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

function Create_Main_Window return Gtk_Window is
   Cb_Id : Glib.Guint;
   The_Accel_Group : Gtk_Accel_Group;
   Main_Window : Gtk_Window;
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

begin
   Window.Gtk_New (Main_Window, Window_Toplevel);
   Widget.Set_USize (Gtk_Widget (Main_Window), 600, 450);
   Cb_Id := Window_Callback.Connect
     (Main_Window, "delete_event", On_Main_Window_Delete_Event'Access);
   Window.Set_Title (Gtk_Window (Main_Window), "The Editor");
   Window.Set_Policy (Gtk_Window (Main_Window), True, True, False);
   Window.Set_Position (Gtk_Window (Main_Window), Win_Pos_None);
   Box.Gtk_New_Vbox (Vbox1, False, 0);
   Container.Add (Gtk_Container (Main_Window), Vbox1);

   Menu_Bar.Gtk_New (Menubar1);
   Box.Pack_Start (Vbox1, Menubar1, False, True, 0);

   Menu_Item.Gtk_New (File, "File");
   Container.Add (Gtk_Container (Menubar1), File);

   Menu.Gtk_New (File_Menu);
   Menu_Item.Set_Submenu (Gtk_Menu_Item (File), File_Menu);

   Menu_Item.Gtk_New (New1, "New");
   Accel_Group.Gtk_New (The_Accel_Group);
   Window.Add_Accel_Group (Main_Window, The_Accel_Group);
   Widget.Add_Accelerator (Gtk_Widget (New1), "activate",
     The_Accel_Group, GDK_N, Gdk.Types.Control_Mask, Accel_Visible);
   Cb_Id := Menu_Item_Callback.Connect
     (New1, "activate", On_New_Activate'Access);
   Container.Add (Gtk_Container (File_Menu), New1);

   Menu_Item.Gtk_New (Open, "Open...");
   Widget.Add_Accelerator (Gtk_Widget (Open), "activate",
     The_Accel_Group, GDK_O, Gdk.Types.Control_Mask, Accel_Visible);
   Cb_Id := Menu_Item_Callback.Connect
     (Open, "activate", On_Open_Activate'Access);
   Container.Add (Gtk_Container (File_Menu), Open);

   Menu_Item.Gtk_New (Save, "Save");
   Widget.Add_Accelerator (Gtk_Widget (Save), "activate",
     The_Accel_Group, GDK_S, Gdk.Types.Control_Mask, Accel_Visible);
   Cb_Id := Menu_Item_Callback.Connect
     (Save, "activate", On_Save_Activate'Access);
   Container.Add (Gtk_Container (File_Menu), Save);

   Menu_Item.Gtk_New (Save_As, "Save As...");
   Cb_Id := Menu_Item_Callback.Connect
     (Save_As, "activate", On_Save_As_Activate'Access);
   Container.Add (Gtk_Container (File_Menu), Save_As);

   Menu_Item.Gtk_New (Separator1);
   Container.Add (Gtk_Container (File_Menu), Separator1);

   Menu_Item.Gtk_New (Quit, "Quit");
   Widget.Add_Accelerator (Gtk_Widget (Quit), "activate",
     The_Accel_Group, GDK_Q, Gdk.Types.Control_Mask, Accel_Visible);
   Cb_Id := Menu_Item_Callback.Connect
     (Quit, "activate", On_Quit_Activate'Access);
   Container.Add (Gtk_Container (File_Menu), Quit);

   Menu_Item.Gtk_New (Edit, "Edit");
   Container.Add (Gtk_Container (Menubar1), Edit);

   Menu.Gtk_New (Edit_Menu);
   Menu_Item.Set_Submenu (Gtk_Menu_Item (Edit), Edit_Menu);

   Menu_Item.Gtk_New (Cut, "Cut");
   Widget.Add_Accelerator (Gtk_Widget (Cut), "activate",
     The_Accel_Group, GDK_X, Gdk.Types.Control_Mask, Accel_Visible);
   Cb_Id := Menu_Item_Callback.Connect
     (Cut, "activate", On_Cut_Activate'Access);
   Container.Add (Gtk_Container (Edit_Menu), Cut);

   Menu_Item.Gtk_New (Copy, "Copy");
   Widget.Add_Accelerator (Gtk_Widget (Copy), "activate",
     The_Accel_Group, GDK_C, Gdk.Types.Control_Mask, Accel_Visible);
   Cb_Id := Menu_Item_Callback.Connect
     (Copy, "activate", On_Copy_Activate'Access);
   Container.Add (Gtk_Container (Edit_Menu), Copy);

   Menu_Item.Gtk_New (Paste, "Paste");
   Widget.Add_Accelerator (Gtk_Widget (Paste), "activate",
     The_Accel_Group, GDK_V, Gdk.Types.Control_Mask, Accel_Visible);
   Cb_Id := Menu_Item_Callback.Connect
     (Paste, "activate", On_Paste_Activate'Access);
   Container.Add (Gtk_Container (Edit_Menu), Paste);

   Menu_Item.Gtk_New (Delete, "Delete");
   Widget.Add_Accelerator (Gtk_Widget (Delete), "activate",
     The_Accel_Group, GDK_Delete, 0, Accel_Visible);
   Cb_Id := Menu_Item_Callback.Connect
     (Delete, "activate", On_Delete_Activate'Access);
   Container.Add (Gtk_Container (Edit_Menu), Delete);

   Menu_Item.Gtk_New (Help, "Help");
   Container.Add (Gtk_Container (Menubar1), Help);

   Menu.Gtk_New (Help_Menu);
   Menu_Item.Set_Submenu (Gtk_Menu_Item (Help), Help_Menu);

   Menu_Item.Gtk_New (About, "About...");
   Cb_Id := Menu_Item_Callback.Connect
     (About, "activate", On_About_Activate'Access);
   Container.Add (Gtk_Container (Help_Menu), About);

   Toolbar.Gtk_New (Toolbar1, Orientation_Horizontal, Toolbar_Both);
   Box.Pack_Start (Vbox1, Toolbar1, False, True, 0);
   Toolbar.Set_Space_Size (Gtk_Toolbar (Toolbar1), 5);
   Toolbar.Set_Tooltips (Gtk_Toolbar (Toolbar1), True);
   Button1 := Toolbar.Append_Item (toolbar1, "New", "", "",
     Create_Pixmap ("new.xpm"));
   Button2 := Toolbar.Append_Item (toolbar1, "Open", "", "",
     Create_Pixmap ("open.xpm"));
   Button3 := Toolbar.Append_Item (toolbar1, "Save", "", "",
     Create_Pixmap ("save.xpm"));

   Cb_Id := Button_Callback.Connect
     (Button1, "clicked", On_New_Button_Clicked'Access);

   Cb_Id := Button_Callback.Connect
     (Button2, "clicked", On_Open_Button_Clicked'Access);

   Cb_Id := Button_Callback.Connect
     (Button3, "clicked", On_Save_Button_Clicked'Access);

   Scrolled_Window.Gtk_New (Scrolledwindow1);
   Box.Pack_Start (Vbox1, Scrolledwindow1, True, True, 0);
   Scrolled_Window.Set_Policy (Gtk_Scrolled_Window (Scrolledwindow1), Policy_Automatic, Policy_Automatic);

   Text.Gtk_New (Text1);
   Cb_Id := Text_Callback.Connect
     (Text1, "changed", On_Text_Changed'Access);
   Text.Set_Editable (Gtk_Text (Text1), True);
   Container.Add (Gtk_Container (Scrolledwindow1), Text1);

   Status_Bar.Gtk_New (Statusbar1);
   Box.Pack_Start (Vbox1, Statusbar1, False, True, 0);

   return Main_Window;
end Create_Main_Window;
