-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gdk;
with Gdk.Bitmap;          use Gdk.Bitmap;
with Gdk.Color;           use Gdk.Color;
with Gdk.Pixmap;          use Gdk.Pixmap;
with Glib;                use Glib;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Label;           use Gtk.Label;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Option_Menu;     use Gtk.Option_Menu;
with Gtk.Pixmap;          use Gtk.Pixmap;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Separator;       use Gtk.Separator;
with Gtk.Widget;          use Gtk.Widget;
with Gtk;                 use Gtk;
with Common;              use Common;

package body Create_Notebook is

   package Note_Cb is new Handlers.Callback (Gtk_Notebook_Record);
   package Button_Cb is new Handlers.User_Callback
     (Gtk_Check_Button_Record, Gtk_Notebook);
   package Two_Cb is new Handlers.User_Callback
     (Gtk_Notebook_Record, Gtk_Notebook);
   package Frame_Cb is new Handlers.User_Callback
     (Gtk_Check_Button_Record, Gtk_Frame);

   Book_Open        : Gdk_Pixmap;
   Book_Open_Mask   : Gdk_Bitmap;
   Book_Closed      : Gdk_Pixmap;
   Book_Closed_Mask : Gdk_Bitmap;
   Notebook         : Gtk_Notebook;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Toolbar@B is a tabbed dialog that contains any kind"
        & " of widget, like @bGtk_Frame@Bs in this case. Whenever the user"
        & " selects a new tab, a new page is displayed." & ASCII.LF
        & "A callback can be called whenever a new page is selected, through"
        & " the ""page_switch"" signal.";
   end Help;

   --------------
   -- Tab_Fill --
   --------------

   procedure Tab_Fill (Button : access Gtk_Check_Button_Record'Class;
                       Child  : in Gtk_Frame) is
      Expand, Fill : Boolean;
      Typ          : Gtk_Pack_Type;
   begin
      Query_Tab_Label_Packing (Notebook, Child, Expand, Fill, Typ);
      Set_Tab_Label_Packing
        (Notebook, Child, Expand, Is_Active (Button), Typ);
   end Tab_Fill;

   ----------------
   -- Tab_Expand --
   ----------------

   procedure Tab_Expand (Button : access Gtk_Check_Button_Record'Class;
                         Child  : in Gtk_Frame) is
      Expand, Fill : Boolean;
      Typ          : Gtk_Pack_Type;
   begin
      Query_Tab_Label_Packing (Notebook, Child, Expand, Fill, Typ);
      Set_Tab_Label_Packing
        (Notebook, Child, Is_Active (Button), Fill, Typ);
   end Tab_Expand;

   ----------
   -- Hide --
   ----------

   procedure Hide (Widget : access Gtk_Widget_Record'Class) is
   begin
      Gtk.Widget.Hide (Widget);
   end Hide;

   ---------------
   -- Next_Page --
   ---------------

   procedure Next_Page (Notebook : access Gtk_Notebook_Record'Class) is
   begin
      Gtk.Notebook.Next_Page (Notebook);
   end Next_Page;

   ---------------
   -- Prev_Page --
   ---------------

   procedure Prev_Page (Notebook : access Gtk_Notebook_Record'Class) is
   begin
      Gtk.Notebook.Prev_Page (Notebook);
   end Prev_Page;

   --------------
   -- Tab_Pack --
   --------------

   procedure Tab_Pack (Button : access Gtk_Check_Button_Record'Class;
                       Child  : in Gtk_Frame) is
      Expand, Fill : Boolean;
      Typ          : Gtk_Pack_Type;
   begin
      Query_Tab_Label_Packing (Notebook, Child, Expand, Fill, Typ);
      if Is_Active (Button) then
         Set_Tab_Label_Packing
           (Notebook, Child, Expand, Fill, Pack_Start);
      else
         Set_Tab_Label_Packing
           (Notebook, Child, Expand, Fill, Pack_End);
      end if;
   end Tab_Pack;

   ------------------
   -- Create_Pages --
   ------------------

   procedure Create_Pages
     (Notebook  : access Gtk_Notebook_Record'Class;
      The_Start : Gint;
      The_End   : Gint)
   is
      Child     : Gtk_Frame;
      Label_Box : Gtk_Box;
      Menu_Box  : Gtk_Box;
      Pixmap    : Gtk_Pixmap;
      Label     : Gtk_Label;
      Vbox      : Gtk_Box;
      Hbox      : Gtk_Box;
      Check     : Gtk_Check_Button;
      Button    : Gtk_Button;

   begin
      for I in The_Start .. The_End loop
         Gtk_New (Child, "Page " & Gint'Image (I));
         Set_Border_Width (Child, 10);

         Gtk_New_Vbox (Vbox, True, 0);
         Set_Border_Width (Vbox, 10);
         Add (Child, Vbox);

         Gtk_New_Hbox (Hbox, True, 0);
         Pack_Start (Vbox, Hbox, False, True, 5);

         Gtk_New (Check, "Fill tab");
         Pack_Start (Hbox, Check, True, True, 5);
         Set_Active (Check, True);
         Frame_Cb.Connect (Check, "toggled",
                           Frame_Cb.To_Marshaller (Tab_Fill'Access),
                           Child);

         Gtk_New (Check, "Expand tab");
         Pack_Start (Hbox, Check, True, True, 5);
         Set_Active (Check, True);
         Frame_Cb.Connect (Check, "toggled",
                           Frame_Cb.To_Marshaller (Tab_Expand'Access),
                           Child);

         Gtk_New (Check, "Pack end");
         Pack_Start (Hbox, Check, True, True, 5);
         Set_Active (Check, True);
         Frame_Cb.Connect (Check, "toggled",
                           Frame_Cb.To_Marshaller (Tab_Pack'Access),
                           Child);

         Gtk_New (Button, "Hide page");
         Pack_Start (Vbox, Button, True, True, 5);
         Widget_Handler.Object_Connect
           (Button, "clicked",
            Widget_Handler.To_Marshaller (Hide'Access),
            Slot_Object => Child);

         Show_All (Child);
         Gtk_New_Hbox (Label_Box, False, 0);
         Gtk_New (Pixmap, Book_Closed, Book_Closed_Mask);
         Pack_Start (Label_Box, Pixmap, False, True, 0);
         Set_Padding (Pixmap, 3, 1);

         Gtk_New (Label, "Page" & Gint'Image (I));
         Pack_Start (Label_Box, Label, False, True, 0);
         Show_All (Label_Box);

         Gtk_New_Vbox (Menu_Box, False, 0);
         Gtk_New (Pixmap, Book_Closed, Book_Closed_Mask);
         Pack_Start (Menu_Box, Pixmap, False, True, 0);
         Set_Padding (Pixmap, 3, 1);

         Gtk_New (Label, "Page" & Gint'Image (I));
         Pack_Start (Menu_Box, Label, False, True, 0);
         Show_All (Menu_Box);

         Append_Page_Menu (Notebook, Child, Label_Box, Menu_Box);
      end loop;
   end Create_Pages;

   ---------------------
   -- Rotate_Notebook --
   ---------------------

   procedure Rotate_Notebook (Notebook : access Gtk_Notebook_Record'Class) is
   begin
      Set_Tab_Pos (Notebook,
                   Gtk_Position_Type'Val
                   ((Gtk_Position_Type'Pos (Get_Tab_Pos (Notebook)) + 1)
                    mod 4));
   end Rotate_Notebook;

   --------------------
   -- Show_All_Pages --
   --------------------

   procedure Show_All_Pages (Notebook : access Gtk_Notebook_Record'Class) is
   begin
      Show_All (Notebook);
   end Show_All_Pages;

   -----------------------
   -- Standard_Notebook --
   -----------------------

   procedure Standard_Notebook (Notebook : access Gtk_Notebook_Record'Class) is
   begin
      Set_Show_Tabs (Notebook, True);
      Set_Scrollable (Notebook, False);
      if Page_List.Length (Get_Children (Notebook)) = 15 then
         for I in 0 .. 9 loop
            Remove_Page (Notebook, 5);
         end loop;
      end if;
   end Standard_Notebook;

   ---------------------
   -- Notabs_Notebook --
   ---------------------

   procedure Notabs_Notebook (Notebook : access Gtk_Notebook_Record'Class) is
   begin
      Set_Show_Tabs (Notebook, False);
      if Page_List.Length (Get_Children (Notebook)) = 15 then
         for I in 0 .. 9 loop
            Remove_Page (Notebook, 5);
         end loop;
      end if;
   end Notabs_Notebook;

   -------------------------
   -- Scrollable_Notebook --
   -------------------------

   procedure Scrollable_Notebook (Notebook : access Gtk_Notebook_Record'Class)
   is
   begin
      Set_Show_Tabs (Notebook, True);
      Set_Scrollable (Notebook, True);
      if Page_List.Length (Get_Children (Notebook)) = 5 then
         Create_Pages (Notebook, 6, 15);
      end if;
   end Scrollable_Notebook;

   --------------------
   -- Notebook_Popup --
   --------------------

   procedure Notebook_Popup (Button : access Gtk_Check_Button_Record'Class;
                             Notebook : in Gtk_Notebook) is
   begin
      if Is_Active (Button) then
         Popup_Enable (Notebook);
      else
         Popup_Disable (Notebook);
      end if;
   end Notebook_Popup;

   -----------------
   -- Homogeneous --
   -----------------

   procedure Homogeneous (Button : access Gtk_Check_Button_Record'Class;
                          Notebook : in Gtk_Notebook) is
   begin
      Set_Homogeneous_Tabs (Notebook, Is_Active (Button));
   end Homogeneous;

   -----------------
   -- Page_Switch --
   -----------------

   procedure Page_Switch (Notebook : access Gtk_Notebook_Record'Class;
                          Page     : in Gtk_Notebook_Page;
                          Page_Num : in Gtk_Notebook)
   is
      pragma Warnings (Off, Page_Num);
      Old_Page : Gtk_Notebook_Page := Get_Cur_Page (Notebook);
      Pixmap  : Gtk_Pixmap;
   begin
      Pixmap := Gtk_Pixmap
        (Get_Child (Gtk_Box (Get_Tab_Label (Page)), 0));
      Set (Pixmap, Book_Open, Book_Open_Mask);
      Pixmap := Gtk_Pixmap
        (Get_Child (Gtk_Box (Get_Menu_Label (Page)), 0));
      Set (Pixmap, Book_Open, Book_Open_Mask);

      if Gdk.Is_Created (Old_Page) then
         Pixmap := Gtk_Pixmap
           (Get_Child (Gtk_Box (Get_Tab_Label (Old_Page)), 0));
         Set (Pixmap, Book_Closed, Book_Closed_Mask);
         Pixmap := Gtk_Pixmap
           (Get_Child (Gtk_Box (Get_Menu_Label (Old_Page)), 0));
         Set (Pixmap, Book_Closed, Book_Closed_Mask);
      end if;
   end Page_Switch;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1            : Gtk_Box;
      Box2            : Gtk_Box;
      Option_Menu     : Gtk_Option_Menu;
      Menu            : Gtk_Menu;
      Menu_Item       : Gtk_Radio_Menu_Item;
      Group           : Widget_SList.GSlist;
      Button          : Gtk_Check_Button;
      Button2         : Gtk_Button;
      Label           : Gtk_Label;
      Separator       : Gtk_Separator;

   begin
      Set_Label (Frame, "Notebook");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New (Notebook);
      Two_Cb.Connect
        (Notebook, "switch_page",
         Two_Cb.To_Marshaller (Page_Switch'Access), Notebook);
      Set_Tab_Pos (Notebook, Pos_Top);
      Pack_Start (Box1, Notebook, False, False, 0);
      Set_Border_Width (Notebook, 10);
      Realize (Notebook);

      Create_From_Xpm_D (Book_Open,
                         Get_Window (Notebook),
                         Book_Open_Mask, Null_Color,
                         Book_Open_Xpm);
      Create_From_Xpm_D (Book_Closed,
                         Get_Window (Notebook),
                         Book_Closed_Mask, Null_Color,
                         Book_Closed_Xpm);

      Create_Pages (Notebook, 1, 5);

      Gtk_New_Hseparator (Separator);
      Pack_Start (Box1, Separator, False, True, 10);

      Gtk_New_Hbox (Box2, False, 5);
      Pack_Start (Box1, Box2, False, True, 0);

      Gtk_New (Button, "popup menu");
      Pack_Start (Box2, Button, True, False, 0);
      Button_Cb.Connect
        (Button, "clicked",
         Button_Cb.To_Marshaller (Notebook_Popup'Access), Notebook);

      Gtk_New (Button, "homogeneous tabs");
      Pack_Start (Box2, Button, True, False, 0);
      Button_Cb.Connect
        (Button, "clicked",
         Button_Cb.To_Marshaller (Homogeneous'Access), Notebook);

      Gtk_New_Hbox (Box2, False, 5);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, True, 0);

      Gtk_New (Label, "Notebook Style :");
      Pack_Start (Box2, Label, False, True, 0);

      Gtk_New (Option_Menu);
      Gtk_New (Menu);

      Gtk_New (Menu_Item, Group, "Standard");
      Note_Cb.Object_Connect
        (Menu_Item, "activate",
         Note_Cb.To_Marshaller (Standard_Notebook'Access),
         Slot_Object => Notebook);
      Group := Gtk.Radio_Menu_Item.Group (Menu_Item);
      Append (Menu, Menu_Item);
      Show (Menu_Item);

      Gtk_New (Menu_Item, Group, "w/o Tabs");
      Note_Cb.Object_Connect
        (Menu_Item, "activate",
         Note_Cb.To_Marshaller (Notabs_Notebook'Access),
         Slot_Object => Notebook);
      Group := Gtk.Radio_Menu_Item.Group (Menu_Item);
      Append (Menu, Menu_Item);
      Show (Menu_Item);

      Gtk_New (Menu_Item, Group, "Scrollable");
      Note_Cb.Object_Connect
        (Menu_Item, "activate",
         Note_Cb.To_Marshaller (Scrollable_Notebook'Access),
         Slot_Object => Notebook);
      Group := Gtk.Radio_Menu_Item.Group (Menu_Item);
      Append (Menu, Menu_Item);
      Show (Menu_Item);

      Set_Menu (Option_Menu, Menu);
      Pack_Start (Box2, Option_Menu, False, False, 0);

      Gtk_New (Button2, "Show all pages");
      Pack_Start (Box2, Button2, False, True, 0);
      Note_Cb.Object_Connect
        (Button2, "clicked",
         Note_Cb.To_Marshaller (Show_All_Pages'Access),
         Slot_Object => Notebook);

      Gtk_New_Hbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, True, 0);

      Gtk_New (Button2, "next");
      Note_Cb.Object_Connect
        (Button2, "clicked",
         Note_Cb.To_Marshaller (Next_Page'Access),
         Slot_Object => Notebook);
      Pack_Start (Box2, Button2, True, True, 0);

      Gtk_New (Button2, "prev");
      Note_Cb.Object_Connect
        (Button2, "clicked",
         Note_Cb.To_Marshaller (Prev_Page'Access),
         Slot_Object => Notebook);
      Pack_Start (Box2, Button2, True, True, 0);

      Gtk_New (Button2, "rotate");
      Note_Cb.Object_Connect
        (Button2, "clicked",
         Note_Cb.To_Marshaller (Rotate_Notebook'Access),
         Slot_Object => Notebook);
      Pack_Start (Box2, Button2, True, True, 0);

      Show_All (Frame);
   end Run;

end Create_Notebook;
