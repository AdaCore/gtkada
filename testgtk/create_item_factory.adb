with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Item_Factory; use Gtk.Item_Factory;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Menu_Bar;
with Ada.Text_IO; use Ada.Text_IO;

package body Create_Item_Factory is

   package Factory_Data is new Data_Item (Integer);
   use Factory_Data;

   procedure Ifactory_Cb
     (Callback_Data   : Data_Type_Access;
      Callback_Action : Guint;
      Widget          : Limited_Widget);

   Menu_Items : Gtk_Item_Factory_Entry_Array :=
     (Gtk_New ("/_File", Item_Type => Branch),
      Gtk_New ("/File/tearoff1", "", Ifactory_Cb'Access, Tearoff),
      Gtk_New ("/File/_New", "<control>N", Ifactory_Cb'Access),
      Gtk_New ("/File/_Open", "<control>O", Ifactory_Cb'Access),
      Gtk_New ("/File/_Save", "<control>S", Ifactory_Cb'Access),
      Gtk_New ("/File/Save _As...", Callback => Ifactory_Cb'Access),
      Gtk_New ("/File/sep1", "", Ifactory_Cb'Access, Separator),
      Gtk_New ("/File/_Quit", "<control>Q", Ifactory_Cb'Access),
      Gtk_New ("/_Preferences", Item_Type => Branch),
      Gtk_New ("/_Preferences/_Color", Item_Type => Branch),
      Gtk_New ("/_Preferences/_Color/_Red", "", Ifactory_Cb'Access, Radio_Item),
      Gtk_New ("/_Preferences/_Color/_Green", "", Ifactory_Cb'Access,
               "/Preferences/Color/Red"),
      Gtk_New ("/_Preferences/_Color/_Blue", "", Ifactory_Cb'Access,
               "/Preferences/Color/Red"),
      Gtk_New ("/_Preferences/_Shape", Item_Type => Branch),
      Gtk_New ("/_Preferences/Shape/_Square", "", Ifactory_Cb'Access,
               Radio_Item),
      Gtk_New ("/_Preferences/Shape/_Rectangle", "", Ifactory_Cb'Access,
               "/Preferences/Shape/Square"),
      Gtk_New ("/_Preferences/Shape/_Oval", "", Ifactory_Cb'Access,
               "/Preferences/Shape/Rectangle"),
      Gtk_New ("/_Help", Item_Type => Last_Branch),
      Gtk_New ("/Help/_About", "", Ifactory_Cb'Access));

   procedure Ifactory_Cb
     (Callback_Data   : Data_Type_Access;
      Callback_Action : Guint;
      Widget          : Limited_Widget) is
   begin
      Put_Line ("ItemFactory: activated " &
        Path_From_Widget (To_Widget (Widget)));
   end Ifactory_Cb;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1         : Gtk_Box;
      Accel_Group  : Gtk_Accel_Group;
      Item_Factory : Gtk_Item_Factory;
 
   begin
      --  gtk_signal_connect (GTK_OBJECT (window), "destroy",
      --                      GTK_SIGNAL_FUNC(gtk_widget_destroyed),
      --                      &window);
      --  gtk_signal_connect (GTK_OBJECT (window), "delete-event",
      --                      GTK_SIGNAL_FUNC (gtk_true),
      --                      NULL);
 
      Gtk_New (Accel_Group);
      Gtk_New (Item_Factory, Gtk.Menu_Bar.Get_Type, "<main>", Accel_Group);
      --  gtk_object_set_data_full (GTK_OBJECT (window),
      --                            "<main>",
      --                            item_factory,
      --                            (GtkDestroyNotify) gtk_object_unref);
      Create_Items (Item_Factory, Menu_Items, null);
 
      --  preselect /Preferences/Shape/Oval over the other radios
      Set_Active (Gtk_Check_Menu_Item (Get_Item
        (Item_Factory, "/Preferences/Shape/Oval")), True);
 
      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);
 
      Pack_Start
        (Box1, Get_Widget (Item_Factory, "<main>"), False, False, 0);
 
      Show_All (Frame);
   end Run;

   function Help return String is
   begin
      return "No help available";
   end Help;

end Create_Item_Factory;
