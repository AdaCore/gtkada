------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Glib;        use Glib;
with Glib.Object; use Glib.Object;

with Gdk.Display; use Gdk.Display;
with Gdk.Screen;  use Gdk.Screen;

with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Handlers;
with Gtk.Label;        use Gtk.Label;
with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtk.Status_Icon;  use Gtk.Status_Icon;
with Gtk.Stock;        use Gtk.Stock;

with Common; use Common;

package body Create_Status_Icons is

   type Image_Type is (Info, Warning, Error);
   Current_Image : Image_Type := Info;
   --  We use this to keep some state information about the current
   --  icon being displayed in the status area.

   type Icon_Array is array (Gint range <>) of Gtk_Status_Icon;
   type Icon_Array_Access is access Icon_Array;
   Icons : Icon_Array_Access := null;
   --  The icons that we will stick on the display's status icon area.

   package Menu_Item_Handler is
     new Gtk.Handlers.Callback (Gtk_Menu_Item_Record);

   procedure Change_Icon_Cb (Button : access Gtk_Button_Record'Class);
   procedure Popup_Menu_Cb
      (Status_Icon   : access Gtk_Status_Icon_Record'Class;
       Button        : Guint;
       Activate_Time : Guint);
   --  Callback procedures

   package Widget_Popups is new Popup_User_Data (GObject);

   -----------------
   -- Change_Icon --
   -----------------

   procedure Change_Icon is
      Display   : constant Gdk_Display := Get_Default;
      N_Screens : constant Gint := Display.Get_N_Screens;
   begin
      --  Advance to next image
      if Current_Image = Image_Type'Last then
         Current_Image := Image_Type'First;
      else
         Current_Image := Image_Type'Succ (Current_Image);
      end if;

      --  Change all images on all displays.
      for I in 1 .. N_Screens loop
         case Current_Image is
            when Info =>
               Set_From_Stock (Icons (I), Stock_Ok);
               Set_Tooltip_Text (Icons (I), "Some Information ...");
            when Warning =>
               Set_From_Stock (Icons (I), Stock_Dialog_Warning);
               Set_Tooltip_Text (Icons (I), "Some Warning ...");
            when Error =>
               Set_From_Stock (Icons (I), Stock_Dialog_Error);
               Set_Tooltip_Text (Icons (I), "Some Error ...");
         end case;
      end loop;
   end Change_Icon;

   --------------------
   -- Change_Icon_Cb --
   --------------------

   procedure Change_Icon_Cb
      (Button : access Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      Change_Icon;
   end Change_Icon_Cb;

   --------------------
   -- Change_Icon_Cb --
   --------------------

   procedure Change_Icon_Cb
      (Menu_Item : access Gtk_Menu_Item_Record'Class)
   is
      pragma Unreferenced (Menu_Item);
   begin
      Change_Icon;
   end Change_Icon_Cb;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "a @bGtk_Status_Icon@B is used to display an icon in a"
        & " ""system tray.""  The icon can have a tooltip, and the user"
        & " can interact with it by activating it or popping up a context"
        & " menu. Critical information should not solely be displayed in a"
        & " @bGtk_Status_Icon@B, since it may not be visible (e.g. when the"
        & " user doesn't have a notification area on his panel). This can be"
        & " checked with @bIs_Embedded@B.";
   end Help;

   -------------------
   -- Popup_Menu_Cb --
   -------------------

   procedure Popup_Menu_Cb
      (Status_Icon   : access Gtk_Status_Icon_Record'Class;
       Button        : Guint;
       Activate_Time : Guint)
   is
      Menu      : constant Gtk_Menu := Gtk_Menu_New;
      Menu_Item : Gtk_Menu_Item;
   begin
      Menu.Set_Screen (Status_Icon.Get_Screen);

      Gtk_New (Menu_Item, "Change Icon");
      Menu_Item_Handler.Connect (Menu_Item, "activate", Change_Icon_Cb'Access);
      Append (Menu, Menu_Item);
      Show (Menu_Item);

      Widget_Popups.Popup
        (Menu          => Menu,
         Func          => Gtk.Status_Icon.Position_Menu'Access,
         Data          => GObject (Status_Icon),
         Button        => Button,
         Activate_Time => Guint32 (Activate_Time));
   end Popup_Menu_Cb;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1          : Gtk_Box;
      Button1       : Gtk_Button;
      Label1        : Gtk_Label;
      Display       : constant Gdk_Display := Get_Default;
      N_Screens     : constant Gint := Display.Get_N_Screens;

   begin
      if Icons = null then
         Icons := new Icon_Array (1 .. N_Screens);

         --  Reset all of our icons' variable settings.
         for I in 1 .. N_Screens loop
            Icons (I) := Gtk_Status_Icon_New;
            Icons (I).Set_Screen (Get_Screen (Display, I - 1));
            Icons (I).Set_From_Stock (Stock_Ok);
            Icons (I).Set_Tooltip_Text ("Some question...");
            Icons (I).On_Popup_Menu (Popup_Menu_Cb'Access);
         end loop;
      end if;

      Gtk.Frame.Set_Label (Frame, "Status_Icons");

      Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
      Gtk.Frame.Add (Frame, Box1);

      Gtk_New (Label1, "Look for the Status Icon in the system tray.");
      Pack_Start (Box1, Label1, False, False, 10);
      Gtk_New (Label1, "Click on Help for more information.");
      Pack_Start (Box1, Label1, False, False, 10);

      Gtk_New (Button1, "Change Icon (Info/Warning/Error)");
      Button_Handler.Connect (Button1, "clicked", Change_Icon_Cb'Access);
      Pack_Start (Box1, Button1, False, False, 0);

      Show_All (Box1);
   end Run;

end Create_Status_Icons;
