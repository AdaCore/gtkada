with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Pixmap; use Gtk.Pixmap;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Callbacks_Gladeedit; use Callbacks_Gladeedit;
with Main_Window_Pkg.Callbacks; use Main_Window_Pkg.Callbacks;

package body Main_Window_Pkg is

procedure Gtk_New (Main_Window : out Main_Window_Access) is
begin
   Main_Window := new Main_Window_Record;
   Main_Window_Pkg.Initialize (Main_Window);
end Gtk_New;

procedure Initialize (Main_Window : access Main_Window_Record'Class) is
   The_Accel_Group : Gtk_Accel_Group;

begin
   Gtk.Window.Initialize (Main_Window, Window_Toplevel);
   Set_USize (Main_Window, 600, 450);
   Window_Callback.Connect
     (Main_Window, "delete_event", On_Main_Window_Delete_Event'Access);
   Set_Title (Main_Window, "The Editor");
   Set_Policy (Main_Window, True, True, False);
   Set_Position (Main_Window, Win_Pos_None);
   Set_Modal (Main_Window, False);
   Gtk_New_Vbox (Main_Window.Vbox1, False, 0);
   Add (Main_Window, Main_Window.Vbox1);

   Gtk_New (Main_Window.Menubar1);
   Pack_Start (Main_Window.Vbox1, Main_Window.Menubar1, False, True, 0);
   Set_Shadow_Type (Main_Window.Menubar1, Shadow_Out);

   Gtk_New (Main_Window.File, "File");
   Set_Right_Justify (Main_Window.File, False);
   Add (Main_Window.Menubar1, Main_Window.File);

   Gtk_New (Main_Window.File_Menu);
   Set_Submenu (Main_Window.File, Main_Window.File_Menu);

   Gtk_New (Main_Window.New1, "New");
   Gtk_New (The_Accel_Group);
   Add_Accel_Group (Main_Window, The_Accel_Group);
   Add_Accelerator (Main_Window.New1, "activate",
     The_Accel_Group, GDK_N, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Window.New1, "activate",
      Menu_Item_Callback.To_Marshaller (On_New_Activate'Access));
   Set_Right_Justify (Main_Window.New1, False);
   Add (Main_Window.File_Menu, Main_Window.New1);

   Gtk_New (Main_Window.Open, "Open...");
   Add_Accelerator (Main_Window.Open, "activate",
     The_Accel_Group, GDK_O, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Window.Open, "activate",
      Menu_Item_Callback.To_Marshaller (On_Open_Activate'Access));
   Set_Right_Justify (Main_Window.Open, False);
   Add (Main_Window.File_Menu, Main_Window.Open);

   Gtk_New (Main_Window.Save, "Save");
   Add_Accelerator (Main_Window.Save, "activate",
     The_Accel_Group, GDK_S, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Window.Save, "activate",
      Menu_Item_Callback.To_Marshaller (On_Save_Activate'Access));
   Set_Right_Justify (Main_Window.Save, False);
   Add (Main_Window.File_Menu, Main_Window.Save);

   Gtk_New (Main_Window.Save_As, "Save As...");
   Menu_Item_Callback.Connect
     (Main_Window.Save_As, "activate",
      Menu_Item_Callback.To_Marshaller (On_Save_As_Activate'Access));
   Set_Right_Justify (Main_Window.Save_As, False);
   Add (Main_Window.File_Menu, Main_Window.Save_As);

   Gtk_New (Main_Window.Separator1);
   Set_Right_Justify (Main_Window.Separator1, False);
   Add (Main_Window.File_Menu, Main_Window.Separator1);

   Gtk_New (Main_Window.Quit, "Quit");
   Add_Accelerator (Main_Window.Quit, "activate",
     The_Accel_Group, GDK_Q, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Window.Quit, "activate",
      Menu_Item_Callback.To_Marshaller (On_Quit_Activate'Access));
   Set_Right_Justify (Main_Window.Quit, False);
   Add (Main_Window.File_Menu, Main_Window.Quit);

   Gtk_New (Main_Window.Edit, "Edit");
   Set_Right_Justify (Main_Window.Edit, False);
   Add (Main_Window.Menubar1, Main_Window.Edit);

   Gtk_New (Main_Window.Edit_Menu);
   Set_Submenu (Main_Window.Edit, Main_Window.Edit_Menu);

   Gtk_New (Main_Window.Cut, "Cut");
   Add_Accelerator (Main_Window.Cut, "activate",
     The_Accel_Group, GDK_X, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Window.Cut, "activate",
      Menu_Item_Callback.To_Marshaller (On_Cut_Activate'Access));
   Set_Right_Justify (Main_Window.Cut, False);
   Add (Main_Window.Edit_Menu, Main_Window.Cut);

   Gtk_New (Main_Window.Copy, "Copy");
   Add_Accelerator (Main_Window.Copy, "activate",
     The_Accel_Group, GDK_C, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Window.Copy, "activate",
      Menu_Item_Callback.To_Marshaller (On_Copy_Activate'Access));
   Set_Right_Justify (Main_Window.Copy, False);
   Add (Main_Window.Edit_Menu, Main_Window.Copy);

   Gtk_New (Main_Window.Paste, "Paste");
   Add_Accelerator (Main_Window.Paste, "activate",
     The_Accel_Group, GDK_V, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Window.Paste, "activate",
      Menu_Item_Callback.To_Marshaller (On_Paste_Activate'Access));
   Set_Right_Justify (Main_Window.Paste, False);
   Add (Main_Window.Edit_Menu, Main_Window.Paste);

   Gtk_New (Main_Window.Delete, "Delete");
   Add_Accelerator (Main_Window.Delete, "activate",
     The_Accel_Group, GDK_Delete, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Window.Delete, "activate",
      Menu_Item_Callback.To_Marshaller (On_Delete_Activate'Access));
   Set_Right_Justify (Main_Window.Delete, False);
   Add (Main_Window.Edit_Menu, Main_Window.Delete);

   Gtk_New (Main_Window.Help, "Help");
   Set_Right_Justify (Main_Window.Help, False);
   Add (Main_Window.Menubar1, Main_Window.Help);

   Gtk_New (Main_Window.Help_Menu);
   Set_Submenu (Main_Window.Help, Main_Window.Help_Menu);

   Gtk_New (Main_Window.About, "About...");
   Menu_Item_Callback.Connect
     (Main_Window.About, "activate",
      Menu_Item_Callback.To_Marshaller (On_About_Activate'Access));
   Set_Right_Justify (Main_Window.About, False);
   Add (Main_Window.Help_Menu, Main_Window.About);

   Gtk_New (Main_Window.Toolbar1, Orientation_Horizontal, Toolbar_Both);
   Pack_Start (Main_Window.Vbox1, Main_Window.Toolbar1, False, True, 0);
   Set_Space_Size (Main_Window.Toolbar1, 5);
   Set_Space_Style (Main_Window.Toolbar1, Toolbar_Space_Empty);
   Set_Tooltips (Main_Window.Toolbar1, True);
   Set_Button_Relief (Main_Window.Toolbar1, Relief_Normal);
   Main_Window.Button1 := Append_Item (Main_Window.Toolbar1, "New", "", "",
     Create_Pixmap ("new.xpm", Main_Window));
   Main_Window.Button2 := Append_Item (Main_Window.Toolbar1, "Open", "", "",
     Create_Pixmap ("open.xpm", Main_Window));
   Main_Window.Button3 := Append_Item (Main_Window.Toolbar1, "Save", "", "",
     Create_Pixmap ("save.xpm", Main_Window));

   Button_Callback.Connect
     (Main_Window.Button1, "clicked",
      Button_Callback.To_Marshaller (On_New_Button_Clicked'Access));

   Button_Callback.Connect
     (Main_Window.Button2, "clicked",
      Button_Callback.To_Marshaller (On_Open_Button_Clicked'Access));

   Button_Callback.Connect
     (Main_Window.Button3, "clicked",
      Button_Callback.To_Marshaller (On_Save_Button_Clicked'Access));

   Gtk_New (Main_Window.Scrolledwindow1);
   Pack_Start (Main_Window.Vbox1, Main_Window.Scrolledwindow1, True, True, 0);
   Set_Policy (Main_Window.Scrolledwindow1, Policy_Automatic, Policy_Automatic);

   Gtk_New (Main_Window.Text1);
   Text_Callback.Connect
     (Main_Window.Text1, "changed",
      Text_Callback.To_Marshaller (On_Text_Changed'Access));
   Set_Editable (Main_Window.Text1, True);
   Add (Main_Window.Scrolledwindow1, Main_Window.Text1);

   Gtk_New (Main_Window.Statusbar1);
   Pack_Start (Main_Window.Vbox1, Main_Window.Statusbar1, False, True, 0);

end Initialize;

end Main_Window_Pkg;
