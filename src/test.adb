with Gtk; use Gtk;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk.Container; use Gtk.Container;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Button; use Gtk.Button;
with Gtk.Box;    use Gtk.Box;
with Gtk.Hbox;   use Gtk.Hbox;
with Gtk.Vbox;   use Gtk.Vbox;
with Gtk.Combo;  use Gtk.Combo;
with Ada.Text_IO;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Color_Selection; use Gtk.Color_Selection;
with Interfaces.C.Strings;
with Gtk.Color_Selection_Dialog; use Gtk.Color_Selection_Dialog;

package body Test is

   subtype String7 is String (1 .. 7);
   package String_Cb is new Callback (String7, Gtk.Button.Gtk_Button);
   package Void_Cb   is new Void_Callback (Gtk.Window.Gtk_Window);
   package Void_Cb_Button is new Void_Callback (Gtk.Button.Gtk_Button);
   package ColSel_Cb is new Object_Callback (Gtk_Color_Selection_Dialog);
   use Gtk.Combo.String_List;

   package ICS renames Interfaces.C.Strings;

   Status   : Gtk_Status_Bar;

   procedure Hello (Widget : in out Gtk.Button.Gtk_Button;
                    S      : in     String7);
   procedure Destroy (Object : in out Gtk.Window.Gtk_Window);
   procedure Launch_Dialog (Object : in out Gtk.Button.Gtk_Button);
   procedure Print_Color (Dialog : in out Gtk_Color_Selection_Dialog'Class);

   -----------
   -- Hello --
   -----------

   procedure Hello (Widget : in out Gtk.Button.Gtk_Button;
                    S      : in     String7) is
      Message : Message_Id;
   begin
      Ada.Text_IO.Put_Line ("Hello World  => String was=" & S);
      Message := Push (Status, 1, S);
      Gtk.Destroy (Widget);
   end Hello;

   -----------------
   -- Print_Color --
   -----------------

   procedure Print_Color (Dialog : in out Gtk_Color_Selection_Dialog'Class) is
      Color : Color_Array;
   begin
      Get_Color (Get_Colorsel (Dialog), Color);
      for I in Red .. Opacity loop
         Ada.Text_IO.Put_Line (Color_Index'Image (I)
                               & " => "
                               & Gdouble'Image (Color (I)));
      end loop;
   end Print_Color;

   -------------------
   -- Launch_Dialog --
   -------------------

   procedure Launch_Dialog (Object : in out Gtk.Button.Gtk_Button) is
      Dialog : aliased Gtk_Color_Selection_Dialog;
      Id     : Guint;
   begin
      Gtk_New (Dialog);
      Hide (Get_Help_Button (Dialog));
      Show (Dialog);
      Id := ColSel_Cb.Connect (Get_OK_Button (Dialog), "clicked",
                               Print_Color'Access, Dialog);
--       Id := ColSel_Cb.Connect (Get_Cancel_Button (Dialog), "clicked",
--                                Destroy'Access, Dialog);
   end Launch_Dialog;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Object : in out Gtk.Window.Gtk_Window) is
   begin
      Ada.Text_IO.Put_Line ("Destroy");
      Main_Quit;
   end Destroy;

   ----------
   -- Main --
   ----------

   procedure Main is

      A_Window : aliased Gtk.Window.Gtk_Window;
      A_Box    : Gtk_Hbox;
      A_Button : Gtk.Button.Gtk_Button;
      V_Box    : Gtk_Vbox;
      Id       : Guint;
      Combo    : Gtk_Combo;
      List     : Glist;

   begin
      Init;
      --  Initialize the library (how can we pass the command line arguments ?)

      --  Create the window  (window = gtk_window_new (GTK_WINDOW_TOPLEVEL))
      Gtk_New (A_Window, Window_Toplevel);
      Set_Title (A_Window, "Hello buttons");
      Border_Width (A_Window, 10);
      Id := Void_Cb.Connect (A_Window, "destroy", Destroy'Access);

      --  Create the box to store the buttons
      Gtk_New (V_Box, False, 0);
      Add (A_Window, V_Box);

      Gtk_New (A_Box, False, 0);
      Pack_Start (V_Box, A_Box);

      --  Create the first button
      Gtk_New (A_Button, With_Label => "Button1");
      Id := String_Cb.Connect (A_Button, "clicked",
                               Hello'Access, "Button1");
      Pack_Start (A_Box, A_Button, True, True, 0);
      Show (A_Button);

      --  Create the second button
      Gtk_New (A_Button, With_Label => "Button2");
      Id := String_Cb.Connect (A_Button, "clicked",
                               Hello'Access, "Button2");
      Pack_Start (A_Box, A_Button, True, True, 0);
      Show (A_Button);

      --  Combo Box
      Append (List, ICS.New_String ("item0"));
      Append (List, ICS.New_String ("item1 item1"));
      Append (List, ICS.New_String ("item2 item2 item2"));
      Append (List, ICS.New_String ("item3 item3 item3 item3"));
      Append (List, ICS.New_String ("item4 item4 item4 item4 item4"));
      Append (List, ICS.New_String ("item5 item5 item5 item5 item5 item5"));
      Append (List, ICS.New_String ("item6 item6 item6 item6 item6"));
      Append (List, ICS.New_String ("item7 item7 item7 item7"));
      Append (List, ICS.New_String ("item8 item8 item8"));
      Append (List, ICS.New_String ("item9 item9"));

      Gtk_New (Combo);
      Set_Popdown_Strings (Combo, List);
      Pack_Start (V_Box, Combo, True, True, 5);
      Show (Combo);

      --  Status Bar
      Gtk_New (Status);
      Pack_Start (V_Box, Status, True, True, 10);
      Show (Status);

      --  Launch dialog
      Gtk_New (A_Button, With_Label => "Color Selection");
      Id := Void_Cb_Button.Connect (A_Button, "clicked",
                                    Launch_Dialog'Access);
      Pack_Start (V_Box, A_Button, True, True, 5);
      Show (A_Button);

      --  Show the box
      Show (A_Box);
      Show (V_Box);

      Show (A_Window);

      Gtk.Main;
      --  Call the main loop   (gtk_main)
   end Main;

end Test;
