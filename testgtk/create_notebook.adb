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
with Gdk.Bitmap; use Gdk.Bitmap;
with Gdk.Color;  use Gdk.Color;
with Gdk.Pixmap; use Gdk.Pixmap;
with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Option_Menu; use Gtk.Option_Menu;
with Gtk.Pixmap; use Gtk.Pixmap;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Common; use Common;

package body Create_Notebook is

   package Note_Cb is new Signal.Object_Callback (Gtk_Notebook_Record);
   package Button_Cb is new Signal.Callback
     (Gtk_Check_Button_Record, Gtk_Notebook);
   package Two_Cb is new Signal.Two_Callback
     (Gtk_Notebook_Record, Gtk_Notebook, Gtk_Notebook_Page);
   package Frame_Cb is new Signal.Callback
     (Gtk_Check_Button_Record, Gtk_Frame);

   package ConvertP is new Gdk.Unchecked_Cast
     (Gtk_Pixmap_Record, Gtk_Pixmap);
   package ConvertB is new Gdk.Unchecked_Cast
     (Gtk_Box_Record, Gtk_Box);

   Window           : aliased Gtk_Window;
   Book_Open        : Gdk_Pixmap;
   Book_Open_Mask   : Gdk_Bitmap;
   Book_Closed      : Gdk_Pixmap;
   Book_Closed_Mask : Gdk_Bitmap;
   Notebook         : Gtk_Notebook;

   procedure Tab_Fill (Button : access Gtk_Check_Button_Record;
                       Child  : in Gtk_Frame) is
      Expand, Fill : Boolean;
      Typ          : Gtk_Pack_Type;
   begin
      Query_Tab_Label_Packing (Notebook, Child, Expand, Fill, Typ);
      Set_Tab_Label_Packing
        (Notebook, Child, Expand, Is_Active (Button), Typ);
   end Tab_Fill;

   procedure Tab_Expand (Button : access Gtk_Check_Button_Record;
                         Child  : in Gtk_Frame) is
      Expand, Fill : Boolean;
      Typ          : Gtk_Pack_Type;
   begin
      Query_Tab_Label_Packing (Notebook, Child, Expand, Fill, Typ);
      Set_Tab_Label_Packing
        (Notebook, Child, Is_Active (Button), Fill, Typ);
   end Tab_Expand;

   procedure Tab_Pack (Button : access Gtk_Check_Button_Record;
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

   procedure Create_Pages
     (Notebook  : access Gtk_Notebook_Record'Class;
      The_Start : Gint; The_End : Gint)
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
      Id        : Guint;
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
         Id := Frame_Cb.Connect (Check, "toggled", Tab_Fill'Access, Child);

         Gtk_New (Check, "Expand tab");
         Pack_Start (Hbox, Check, True, True, 5);
         Set_Active (Check, True);
         Id := Frame_Cb.Connect (Check, "toggled", Tab_Expand'Access, Child);

         Gtk_New (Check, "Pack end");
         Pack_Start (Hbox, Check, True, True, 5);
         Set_Active (Check, True);
         Id := Frame_Cb.Connect (Check, "toggled", Tab_Pack'Access, Child);

         Gtk_New (Button, "Hide page");
         Pack_Start (Vbox, Button, True, True, 5);
         Id := Widget_Cb.Connect (Button, "clicked", Hide'Access, Child);

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

   procedure Rotate_Notebook (Notebook : access Gtk_Notebook_Record) is
   begin
      Set_Tab_Pos (Notebook,
                   Gtk_Position_Type'Val
                   ((Gtk_Position_Type'Pos (Get_Tab_Pos (Notebook)) + 1)
                    mod 4));
   end Rotate_Notebook;

   procedure Show_All_Pages (Notebook : access Gtk_Notebook_Record) is
   begin
      Show_All (Notebook);
   end Show_All_Pages;

   procedure Standard_Notebook (Notebook : access Gtk_Notebook_Record) is
   begin
      Set_Show_Tabs (Notebook, True);
      Set_Scrollable (Notebook, False);
      if Page_List.Length (Get_Children (Notebook)) = 15 then
         for I in 0 .. 9 loop
            Remove_Page (Notebook, 5);
         end loop;
      end if;
   end Standard_Notebook;

   procedure Notabs_Notebook (Notebook : access Gtk_Notebook_Record) is
   begin
      Set_Show_Tabs (Notebook, False);
      if Page_List.Length (Get_Children (Notebook)) = 15 then
         for I in 0 .. 9 loop
            Remove_Page (Notebook, 5);
         end loop;
      end if;
   end Notabs_Notebook;

   procedure Scrollable_Notebook (Notebook : access Gtk_Notebook_Record) is
   begin
      Set_Show_Tabs (Notebook, True);
      Set_Scrollable (Notebook, True);
      if Page_List.Length (Get_Children (Notebook)) = 5 then
         Create_Pages (Notebook, 6, 15);
      end if;
   end Scrollable_Notebook;

   procedure Notebook_Popup (Button : access Gtk_Check_Button_Record;
                             Notebook : in Gtk_Notebook) is
   begin
      if Is_Active (Button) then
         Popup_Enable (Notebook);
      else
         Popup_Disable (Notebook);
      end if;
   end Notebook_Popup;

   procedure Homogeneous (Button : access Gtk_Check_Button_Record;
                          Notebook : in Gtk_Notebook) is
   begin
      Set_Homogeneous_Tabs (Notebook, Is_Active (Button));
   end Homogeneous;

   procedure Page_Switch (Notebook : access Gtk_Notebook_Record;
                          Page     : in Gtk_Notebook_Page;
                          Page_Num : in Gtk_Notebook)
   is
      pragma Warnings (Off, Page_Num);
      Old_Page : Gtk_Notebook_Page := Get_Cur_Page (Notebook);
      Pixmap  : Gtk_Pixmap;
   begin
      Pixmap := ConvertP.Convert
        (Get_Child (ConvertB.Convert (Get_Tab_Label (Page)), 0));
      Set (Pixmap, Book_Open, Book_Open_Mask);
      Pixmap := ConvertP.Convert
        (Get_Child (ConvertB.Convert (Get_Menu_Label (Page)), 0));
      Set (Pixmap, Book_Open, Book_Open_Mask);

      if Gdk.Is_Created (Old_Page) then
         Pixmap := ConvertP.Convert
           (Get_Child (ConvertB.Convert (Get_Tab_Label (Old_Page)), 0));
         Set (Pixmap, Book_Closed, Book_Closed_Mask);
         Pixmap := ConvertP.Convert
           (Get_Child (ConvertB.Convert (Get_Menu_Label (Old_Page)), 0));
         Set (Pixmap, Book_Closed, Book_Closed_Mask);
      end if;
   end Page_Switch;

   procedure Run (Widget : access Gtk.Button.Gtk_Button_Record) is
      Id              : Guint;
      Box1            : Gtk_Box;
      Box2            : Gtk_Box;
      Separator       : Gtk_Separator;
      Option_Menu     : Gtk_Option_Menu;
      Menu            : Gtk_Menu;
      Menu_Item       : Gtk_Radio_Menu_Item;
      Group           : Widget_SList.GSlist;
      Button          : Gtk_Check_Button;
      Button2         : Gtk_Button;
      Label           : Gtk_Label;
   begin
      if Window = null then
         Gtk_New (Window, Window_Toplevel);
         Id := Destroy_Cb.Connect
           (Window, "destroy", Destroy_Window'Access, Window'Access);
         Set_Title (Window, "notebook");
         Set_Border_Width (Window, Border_Width => 0);

         Gtk_New_Vbox (Box1, False, 0);
         Add (Window, Box1);

         Gtk_New (Notebook);
         Id := Two_Cb.Connect
           (Notebook, "switch_page", Page_Switch'Access, Notebook);
         Set_Tab_Pos (Notebook, Pos_Top);
         Pack_Start (Box1, Notebook, True, True, 0);
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
         Id := Button_Cb.Connect
           (Button, "clicked", Notebook_Popup'Access, Notebook);

         Gtk_New (Button, "homogeneous tabs");
         Pack_Start (Box2, Button, True, False, 0);
         Id := Button_Cb.Connect
           (Button, "clicked", Homogeneous'Access, Notebook);

         Gtk_New_Hbox (Box2, False, 5);
         Set_Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, False, True, 0);

         Gtk_New (Label, "Notebook Style :");
         Pack_Start (Box2, Label, False, True, 0);

         Gtk_New (Option_Menu);
         Gtk_New (Menu);

         Gtk_New (Menu_Item, Group, "Standard");
         Id := Note_Cb.Connect
           (Menu_Item, "activate", Standard_Notebook'Access, Notebook);
         Group := Gtk.Radio_Menu_Item.Group (Menu_Item);
         Append (Menu, Menu_Item);
         Show (Menu_Item);

         Gtk_New (Menu_Item, Group, "w/o Tabs");
         Id := Note_Cb.Connect
           (Menu_Item, "activate", Notabs_Notebook'Access, Notebook);
         Group := Gtk.Radio_Menu_Item.Group (Menu_Item);
         Append (Menu, Menu_Item);
         Show (Menu_Item);

         Gtk_New (Menu_Item, Group, "Scrollable");
         Id := Note_Cb.Connect
           (Menu_Item, "activate", Scrollable_Notebook'Access, Notebook);
         Group := Gtk.Radio_Menu_Item.Group (Menu_Item);
         Append (Menu, Menu_Item);
         Show (Menu_Item);

         Set_Menu (Option_Menu, Menu);
         Pack_Start (Box2, Option_Menu, False, False, 0);

         Gtk_New (Button2, "Show all pages");
         Pack_Start (Box2, Button2, False, True, 0);
         Id := Note_Cb.Connect
           (Button2, "clicked", Show_All_Pages'Access, Notebook);

         Gtk_New_Hbox (Box2, False, 10);
         Set_Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, False, True, 0);

         Gtk_New (Button2, "next");
         Id := Note_Cb.Connect
           (Button2, "clicked", Next_Page'Access, Notebook);
         Pack_Start (Box2, Button2, True, True, 0);

         Gtk_New (Button2, "prev");
         Id := Note_Cb.Connect
           (Button2, "clicked", Prev_Page'Access, Notebook);
         Pack_Start (Box2, Button2, True, True, 0);

         Gtk_New (Button2, "rotate");
         Id := Note_Cb.Connect
           (Button2, "clicked", Rotate_Notebook'Access, Notebook);
         Pack_Start (Box2, Button2, True, True, 0);

         Gtk_New_Hseparator (Separator);
         Pack_Start (Box1, Separator, Expand => False, Fill => True,
                     Padding => 0);
         Show (Separator);

         Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 0);
         Set_Border_Width (Box2, Border_Width => 10);
         Pack_Start (Box1, Box2, Expand => False, Fill => True, Padding => 0);
         Show (Box2);

         Gtk_New (Button2, Label => "Close");
         Id := Widget_Cb.Connect (Button2, "clicked", Destroy'Access, Window);
         Pack_Start
           (Box2, Button2, Expand => True, Fill => True, Padding => 0);
         Set_Flags (Button2, Can_Default);
         Grab_Default (Button2);

         Show_All (Window);
      else
         Destroy (Window);
      end if;

   end Run;

end Create_Notebook;
