with Ada.Text_IO;
with Gtk.Box;    use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Color_Selection; use Gtk.Color_Selection;
with Gtk.Color_Selection_Dialog; use Gtk.Color_Selection_Dialog;
with Gtk.Combo;  use Gtk.Combo;
with Gtk.Container; use Gtk.Container;
with Gtk.Curve; use Gtk.Curve;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Gamma_Curve; use Gtk.Gamma_Curve;
with Gtk.Hbox;   use Gtk.Hbox;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Tooltips; use Gtk.Tooltips;
with Gtk.Vbox;   use Gtk.Vbox;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Interfaces.C.Strings;

package body Test is

   subtype String7 is String (1 .. 7);
   package String_Cb is new Callback (String7, Gtk.Button.Gtk_Button);
   package Void_Cb   is new Void_Callback (Gtk.Window.Gtk_Window);
   package Void_Cb_Button is new Void_Callback (Gtk.Button.Gtk_Button);
   package ColSel_Cb is new Object_Callback (Gtk_Color_Selection_Dialog);
   package Gamma_Cb is new Callback (Gtk_Gamma_Curve, Gtk_Button);
   use Gtk.Combo.String_List;

   package ICS renames Interfaces.C.Strings;

   Status   : Gtk_Status_Bar;

   procedure Hello (Widget : in out Gtk.Button.Gtk_Button'Class;
                    S      : in     String7);
   procedure App_Destroy (Object : in out Gtk.Window.Gtk_Window'Class);
   procedure Launch_Dialog (Object : in out Gtk.Button.Gtk_Button'Class);
   procedure Launch_Drawing (Object : in out Gtk.Button.Gtk_Button'Class);
   procedure Launch_Gamma (Object : in out Gtk.Button.Gtk_Button'Class);
   procedure Print_Color (Dialog : in out Gtk_Color_Selection_Dialog'Class);
   procedure Print_Gamma_Vector (Button : in out Gtk_Button'Class;
                                 Gamma  : in Gtk_Gamma_Curve);

   -----------
   -- Hello --
   -----------

   procedure Hello (Widget : in out Gtk.Button.Gtk_Button'Class;
                    S      : in     String7) is
      Message : Message_Id;
   begin
      Ada.Text_IO.Put_Line ("Hello World  => String was=" & S);
      Message := Push (Status, 1, S);
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

   procedure Launch_Dialog (Object : in out Gtk.Button.Gtk_Button'Class) is
      Dialog : Gtk_Color_Selection_Dialog;
      Id     : Guint;
   begin
      Gtk_New (Dialog);
      Hide (Get_Help_Button (Dialog));
      Show (Dialog);
      Id := ColSel_Cb.Connect (Get_OK_Button (Dialog), "clicked",
                               Print_Color'Access, Dialog);
--       Id := ColSel_Cb.Connect (Get_Cancel_Button (Dialog), "clicked",
--                                Gtk.Destroy'Access, Dialog);
   end Launch_Dialog;

   --------------------
   -- Launch_Drawing --
   --------------------

   procedure Launch_Drawing (Object : in out Gtk.Button.Gtk_Button'Class) is
      Drawing  : Gtk_Drawing_Area;
      Dialog : Gtk_Window;
   begin
      Gtk_New (Dialog, Window_Toplevel);
      Gtk_New (Drawing);
      Add (Dialog, Drawing);
      Show (Drawing);
      Show (Dialog);
   end Launch_Drawing;

   -----------------
   -- Lauch_Gamma --
   -----------------

   procedure Launch_Gamma (Object : in out Gtk.Button.Gtk_Button'Class) is
      Gamma  : Gtk_Gamma_Curve;
      Dialog : Gtk_Window;
      Box    : Gtk_Vbox;
      Button : Gtk_Button;
      Id     : Guint;
   begin
      Gtk_New (Dialog, Window_Toplevel);

      Gtk_New (Box, False, 0);

      Gtk_New (Gamma);
      Add (Box, Gamma);
      Show (Gamma);

      Gtk_New (Button, Label => "Print Vector");
      Add (Box, Button);
      Id := Gamma_Cb.Connect (Button, "clicked",
                              Print_Gamma_Vector'Access, Gamma);
      Show (Button);

      Show (Box);
      Add (Dialog, Box);
      Show (Dialog);

      Set_Range (Get_Curve (Gamma), 0.0, 127.0, 0.0, 127.0);
   end Launch_Gamma;

   ------------------------
   -- Print_Gamma_Vector --
   ------------------------

   procedure Print_Gamma_Vector (Button : in out Gtk_Button'Class;
                                 Gamma  : in Gtk_Gamma_Curve)
   is
      Vector : Curve_Vector (10);
   begin
      Get_Vector (Get_Curve (Gamma), Vector);

      Ada.Text_IO.Put_Line ("Gamma vector");
      for I in Vector.Vector'Range loop
         Ada.Text_IO.Put_Line (Gfloat'Image (Vector.Vector (I)));
      end loop;

   end Print_Gamma_Vector;

   -------------
   -- Destroy --
   -------------

   procedure App_Destroy (Object : in out Gtk.Window.Gtk_Window'Class) is
   begin
      Ada.Text_IO.Put_Line ("Destroy");
      Main_Quit;
   end App_Destroy;

   ----------
   -- Main --
   ----------

   procedure Main is

      A_Window  : aliased Gtk.Window.Gtk_Window;
      A_Box     : Gtk_Hbox;
      A_Button  : Gtk.Button.Gtk_Button;
      V_Box     : Gtk_Vbox;
      Id        : Guint;
      Combo     : Gtk_Combo;
      List      : Glist;
      Menu_Bar  : Gtk_Menu_Bar;
      Menu      : Gtk_Menu;
      Root_Item : Gtk_Menu_Item;
      Menu_Item : Gtk_Menu_Item;
      Tooltips  : Gtk_Tooltips;
   begin
      Init;
      --  Initialize the library (how can we pass the command line arguments ?)

      --  Create the window  (window = gtk_window_new (GTK_WINDOW_TOPLEVEL))
      Gtk_New (A_Window, Window_Toplevel);
      Set_Title (A_Window, "Hello buttons");
      Border_Width (A_Window, 10);
      Id := Void_Cb.Connect (A_Window, "destroy", App_Destroy'Access);

      --  Create the box to store the buttons
      Gtk_New (V_Box, False, 0);
      Add (A_Window, V_Box);

      --  Create the menu bar
      Gtk_New (Menu_Bar);
      Pack_Start (V_Box, Menu_Bar, True, True, 5);
      Show (Menu_Bar);

      Gtk_New (Menu);
      Gtk_New (Root_Item, Label => "File");
      Show (Root_Item);

      Gtk_New (Menu_Item, Label => "Save");
      Append (Menu, Menu_Item);
      Show (Menu_Item);

      Gtk_New (Menu_Item, Label => "Load");
      Append (Menu, Menu_Item);
      Show (Menu_Item);

      Set_Submenu (Root_Item, Menu);
      Append (Menu_Bar, Root_Item);

      --  Create the horizontal box
      Gtk_New (A_Box, False, 0);
      Pack_Start (V_Box, A_Box);

      --  Create the first button
      Gtk_New (A_Button, Label => "Button1");
      Id := String_Cb.Connect (A_Button, "clicked",
                               Hello'Access, "Button1");
      Pack_Start (A_Box, A_Button, True, True, 0);
      Show (A_Button);

      --  Create the second button
      Gtk_New (A_Button, Label => "Button2");
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

      --  Create tooltips
      Gtk_New (Tooltips);

      --  Launch dialog
      Gtk_New (A_Button, Label => "Color Selection");
      Id := Void_Cb_Button.Connect (A_Button, "clicked",
                                    Launch_Dialog'Access);
      Pack_Start (V_Box, A_Button, True, True, 5);
      Show (A_Button);
      Set_Tip (Tooltips, A_Button, "Display a Color_Selection_Dialog",
               "private");

      --  Launch gamma curve
      Gtk_New (A_Button, Label => "Gamma Curve");
      Id := Void_Cb_Button.Connect (A_Button, "clicked",
                                    Launch_Gamma'Access);
      Pack_Start (V_Box, A_Button, True, True, 5);
      Show (A_Button);
      Set_Tip (Tooltips, A_Button, "Display a Gamma_Curve",
               "private");

      --  Launch drawing area
      Gtk_New (A_Button, Label => "Drawing Area");
      Id := Void_Cb_Button.Connect (A_Button, "clicked",
                                    Launch_Drawing'Access);
      Pack_Start (V_Box, A_Button, True, True, 5);
      Show (A_Button);
      Set_Tip (Tooltips, A_Button, "Display a Drawing_Area",
               "private");

      --  Show the box
      Show (A_Box);
      Show (V_Box);

      Show (A_Window);

      Gtk.Main;
   end Main;

end Test;
