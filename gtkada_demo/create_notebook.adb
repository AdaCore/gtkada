------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Gdk;
with Gdk.Pixbuf;          use Gdk.Pixbuf;
with Glib;                use Glib;
with Gtk.Arguments;       use Gtk.Arguments;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Image;           use Gtk.Image;
with Gtk.Label;           use Gtk.Label;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Separator;       use Gtk.Separator;
with Gtk.Widget;          use Gtk.Widget;
with Gtk;                 use Gtk;
with Common;              use Common;

package body Create_Notebook is

   package Note_Cb is new Handlers.Callback (Gtk_Notebook_Record);
   package Button_Cb is new Handlers.User_Callback
     (Gtk_Check_Button_Record, Gtk_Notebook);
   package Combo_Cb is new Handlers.User_Callback
     (Gtk_Combo_Box_Text_Record, Gtk_Notebook);
   package Notebook_Cb is new Handlers.Callback (Gtk_Notebook_Record);

   Book_Open        : Gdk_Pixbuf;
   Book_Closed      : Gdk_Pixbuf;
   Notebook         : Gtk_Notebook;

   procedure Hide (Widget : access Gtk_Widget_Record'Class);
   --  Hide tabs

   procedure Next_Page (Notebook : access Gtk_Notebook_Record'Class);
   --  Switch to the next page

   procedure Prev_Page (Notebook : access Gtk_Notebook_Record'Class);
   --  Switch to the previous page

   procedure Create_Pages
     (Notebook  : access Gtk_Notebook_Record'Class;
      The_Start : Gint;
      The_End   : Gint);
   --  Create the notebook pages

   procedure Rotate_Notebook
      (Notebook : access Gtk_Notebook_Record'Class);
   --  Rotate the tabs around the notebook

   procedure Show_All_Pages
      (Notebook : access Gtk_Notebook_Record'Class);
   --  Show all pages

   procedure Change_Tabs_Display
     (Combo    : access Gtk_Combo_Box_Text_Record'Class;
      Notebook : Gtk_Notebook);
   --  Change notebook to be displayed without tabs, with scrollable tabs or
   --  with the standard tabs.

   procedure Notebook_Popup
     (Button   : access Gtk_Check_Button_Record'Class;
      Notebook : Gtk_Notebook);
   --  Allow popup window to switch from a page to another

   procedure Set_Tabs_Detachable
     (Button   : access Gtk_Check_Button_Record'Class;
      Notebook : Gtk_Notebook);
   --  Set tabs detachable

   procedure Set_Tabs_Reorderable
     (Button   : access Gtk_Check_Button_Record'Class;
      Notebook : Gtk_Notebook);
   --  Set tabs reorderable

   procedure Page_Switch
     (Notebook : access Gtk_Notebook_Record'Class;
      Params   : Gtk.Arguments.Gtk_Args);
   --  Switch the current page

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

   procedure Next_Page
      (Notebook : access Gtk_Notebook_Record'Class) is
   begin
      Gtk.Notebook.Next_Page (Notebook);
   end Next_Page;

   ---------------
   -- Prev_Page --
   ---------------

   procedure Prev_Page
      (Notebook : access Gtk_Notebook_Record'Class) is
   begin
      Gtk.Notebook.Prev_Page (Notebook);
   end Prev_Page;

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
      Pixmap    : Gtk_Image;
      Label     : Gtk_Label;
      Vbox      : Gtk_Box;
      Hbox      : Gtk_Box;
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

         Gtk_New (Button, "Hide page");
         Pack_Start (Vbox, Button, True, True, 5);
         Widget_Handler.Object_Connect
           (Button, "clicked", Hide'Access, Slot_Object => Child);

         Show_All (Child);
         Gtk_New_Hbox (Label_Box, False, 0);
         Gtk_New (Pixmap, Book_Closed);
         Pack_Start (Label_Box, Pixmap, False, True, 0);
         Set_Padding (Pixmap, 3, 1);

         Gtk_New (Label, "Page" & Gint'Image (I));
         Pack_Start (Label_Box, Label, False, True, 0);
         Show_All (Label_Box);

         Gtk_New_Vbox (Menu_Box, False, 0);
         Gtk_New (Pixmap, Book_Closed);
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

   procedure Rotate_Notebook
      (Notebook : access Gtk_Notebook_Record'Class) is
   begin
      Set_Tab_Pos
        (Notebook,
         Gtk_Position_Type'Val
           ((Gtk_Position_Type'Pos (Get_Tab_Pos (Notebook)) + 1)
            mod 4));
   end Rotate_Notebook;

   --------------------
   -- Show_All_Pages --
   --------------------

   procedure Show_All_Pages
      (Notebook : access Gtk_Notebook_Record'Class) is
   begin
      Show_All (Notebook);
   end Show_All_Pages;

   -------------------------
   -- Change_Tabs_Display --
   -------------------------

   procedure Change_Tabs_Display
     (Combo    : access Gtk_Combo_Box_Text_Record'Class;
      Notebook : Gtk_Notebook)
   is
      Active_Text : constant String := Get_Active_Text (Combo);
   begin
      if Active_Text = "Standard" then
         Set_Show_Tabs (Notebook, True);
         Set_Scrollable (Notebook, False);
         if Get_N_Pages (Notebook) = 15 then
            for I in 0 .. 9 loop
               Remove_Page (Notebook, 5);
            end loop;
         end if;
      elsif Active_Text = "w/o tabs" then
         Set_Show_Tabs (Notebook, False);
         if Get_N_Pages (Notebook) = 15 then
            for I in 0 .. 9 loop
               Remove_Page (Notebook, 5);
            end loop;
         end if;
      elsif Active_Text = "Scrollable" then
         Set_Show_Tabs (Notebook, True);
         Set_Scrollable (Notebook, True);
         if Get_N_Pages (Notebook) = 5 then
            Create_Pages (Notebook, 6, 15);
         end if;
      end if;
   end Change_Tabs_Display;

   --------------------
   -- Notebook_Popup --
   --------------------

   procedure Notebook_Popup
     (Button   : access Gtk_Check_Button_Record'Class;
      Notebook : Gtk_Notebook) is
   begin
      if Get_Active (Button) then
         Popup_Enable (Notebook);
      else
         Popup_Disable (Notebook);
      end if;
   end Notebook_Popup;

   --------------------------
   -- Set_Pages_Detachable --
   --------------------------

   procedure Set_Tabs_Detachable
     (Button   : access Gtk_Check_Button_Record'Class;
      Notebook : Gtk_Notebook) is
   begin
      for N in 0 .. Get_N_Pages (Notebook) - 1 loop
         Set_Tab_Detachable
           (Notebook, Get_Nth_Page (Notebook, N), Get_Active (Button));
      end loop;
   end Set_Tabs_Detachable;

   ---------------------------
   -- Set_Pages_Reorderable --
   ---------------------------

   procedure Set_Tabs_Reorderable
     (Button   : access Gtk_Check_Button_Record'Class;
      Notebook : Gtk_Notebook) is
   begin
      for N in 0 .. Get_N_Pages (Notebook) - 1 loop
         Set_Tab_Reorderable
           (Notebook, Get_Nth_Page (Notebook, N), Get_Active (Button));
      end loop;
   end Set_Tabs_Reorderable;

   -----------------
   -- Page_Switch --
   -----------------

   procedure Page_Switch
     (Notebook : access Gtk_Notebook_Record'Class;
      Params   : Gtk.Arguments.Gtk_Args)
   is
      Old_Page : constant Gint := Get_Current_Page (Notebook);
      Pixmap   : Gtk_Image;
      Page_Num : constant Gint := Gint (To_Guint (Params, 2));
      Widget   : Gtk_Widget;

   begin
      Widget := Get_Nth_Page (Notebook, Page_Num);
      Pixmap := Gtk_Image
        (Get_Child (Gtk_Box (Get_Tab_Label (Notebook, Widget)), 0));
      Pixmap.Set (Book_Open);

      Pixmap := Gtk_Image
        (Get_Child (Gtk_Box (Get_Menu_Label (Notebook, Widget)), 0));
      Pixmap.Set (Book_Open);

      if Old_Page >= 0 then
         Widget := Get_Nth_Page (Notebook, Old_Page);
         Pixmap := Gtk_Image
           (Get_Child (Gtk_Box (Get_Tab_Label (Notebook, Widget)), 0));
         Pixmap.Set (Book_Closed);
         Pixmap := Gtk_Image
           (Get_Child (Gtk_Box (Get_Menu_Label (Notebook, Widget)), 0));
         Pixmap.Set (Book_Closed);
      end if;
   end Page_Switch;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1      : Gtk_Box;
      Box2      : Gtk_Box;
      Combo     : Gtk_Combo_Box_Text;
      Button    : Gtk_Check_Button;
      Button2   : Gtk_Button;
      Label     : Gtk_Label;
      Separator : Gtk_Separator;

   begin
      Set_Label (Frame, "Notebook");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New (Notebook);

      Notebook_Cb.Connect (Notebook, "switch_page", Page_Switch'Access);
      Set_Tab_Pos (Notebook, Pos_Top);
      Pack_Start (Box1, Notebook, False, False, 0);
      Set_Border_Width (Notebook, 10);
      Realize (Notebook);

      Book_Open := Gdk_New_From_Xpm_Data (Book_Open_Xpm);
      Book_Closed := Gdk_New_From_Xpm_Data (Book_Closed_Xpm);

      Create_Pages (Notebook, 1, 5);

      Gtk_New_Hseparator (Separator);
      Pack_Start (Box1, Separator, False, True, 10);

      Gtk_New_Hbox (Box2, False, 5);
      Pack_Start (Box1, Box2, False, True, 0);

      Gtk_New (Button, "popup menu");
      Pack_Start (Box2, Button, True, False, 0);
      Button_Cb.Connect (Button, "clicked", Notebook_Popup'Access, Notebook);

      Gtk_New (Button, "reorderable tabs");
      Pack_Start (Box2, Button, True, False, 0);
      Button_Cb.Connect
        (Button, "clicked", Set_Tabs_Reorderable'Access, Notebook);

      Gtk_New (Button, "detachable tabs");
      Pack_Start (Box2, Button, True, False, 0);
      Button_Cb.Connect
        (Button, "clicked", Set_Tabs_Detachable'Access, Notebook);

      Gtk_New_Hbox (Box2, False, 5);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, True, 0);

      Gtk_New (Label, "Notebook Style :");
      Pack_Start (Box2, Label, False, True, 0);

      Gtk_New (Combo);
      Append_Text (Combo, "Standard");
      Append_Text (Combo, "w/o tabs");
      Append_Text (Combo, "Scrollable");
      Combo_Cb.Connect
        (Combo, "changed", Change_Tabs_Display'Access, Notebook);
      Pack_Start (Box2, Combo, False, False, 0);

      Gtk_New (Button2, "Show all pages");
      Pack_Start (Box2, Button2, False, True, 0);
      Note_Cb.Object_Connect
        (Button2, "clicked", Show_All_Pages'Access, Slot_Object => Notebook);

      Gtk_New_Hbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, True, 0);

      Gtk_New (Button2, "next");
      Note_Cb.Object_Connect
        (Button2, "clicked", Next_Page'Access, Slot_Object => Notebook);
      Pack_Start (Box2, Button2, True, True, 0);

      Gtk_New (Button2, "prev");
      Note_Cb.Object_Connect
        (Button2, "clicked", Prev_Page'Access, Slot_Object => Notebook);
      Pack_Start (Box2, Button2, True, True, 0);

      Gtk_New (Button2, "rotate");
      Note_Cb.Object_Connect
        (Button2, "clicked", Rotate_Notebook'Access, Slot_Object => Notebook);
      Pack_Start (Box2, Button2, True, True, 0);

      Show_All (Frame);
   end Run;

end Create_Notebook;
