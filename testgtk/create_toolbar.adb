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

with Glib.Error;   use Glib.Error;
with Glib.Object;  use Glib.Object;
with Gdk.Pixbuf;   use Gdk.Pixbuf;
with Gtk.Box;      use Gtk.Box;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.GEntry;   use Gtk.GEntry;
with Gtk.Image;    use Gtk.Image;
with Gtk.Separator_Tool_Item; use Gtk.Separator_Tool_Item;
with Gtk.Tool_Button;         use Gtk.Tool_Button;
with Gtk.Tool_Item; use Gtk.Tool_Item;
with Gtk.Widget;   use Gtk.Widget;
with Gtk;          use Gtk;

package body Create_Toolbar is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Toolbar@B is a set of buttons. Each button is can be"
        & " represented both as a text or an icon, or even both. A tooltip"
        & " is automatically created for each button." & ASCII.LF
        & "The following two signals are defined:" & ASCII.LF
        & "   - ""orientation_changed"": the widget was re-oriented."
        & ASCII.LF
        & "   - ""style_changed"": a new style was selected." & ASCII.LF
        & ASCII.LF
        & "It is worth noting that the toolbar can basically contain any type"
        & " of widget, not only buttons. In this demo, we have added a "
        & " @bGtk_Entry@B widget in the middle.";
   end Help;

   ----------------
   -- New_Pixmap --
   ----------------

   function New_Pixmap (Filename   : String) return Gtk_Widget is
      Image     : Gtk_Image;
      Data      : Gdk_Pixbuf;
      Error     : GError;
   begin
      Gdk_New_From_File (Data, Filename, Error => Error);
      Gtk_New (Image, Data);
      return Gtk_Widget (Image);
   end New_Pixmap;

   --------------------
   -- Set_Horizontal --
   --------------------

   procedure Set_Horizontal (Toolbar : access GObject_Record'Class) is
   begin
      Gtk_Toolbar (Toolbar).Set_Orientation (Orientation_Horizontal);
   end Set_Horizontal;

   ------------------
   -- Set_Vertical --
   ------------------

   procedure Set_Vertical (Toolbar : access GObject_Record'Class) is
   begin
      Gtk_Toolbar (Toolbar).Set_Orientation (Orientation_Vertical);
   end Set_Vertical;

   ---------------
   -- Set_Icons --
   ---------------

   procedure Set_Icons (Toolbar : access GObject_Record'Class) is
   begin
      Gtk_Toolbar (Toolbar).Set_Style (Toolbar_Icons);
   end Set_Icons;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Toolbar : access GObject_Record'Class) is
   begin
      Gtk_Toolbar (Toolbar).Set_Style (Toolbar_Text);
   end Set_Text;

   --------------
   -- Set_Both --
   --------------

   procedure Set_Both (Toolbar : access GObject_Record'Class) is
   begin
      Gtk_Toolbar (Toolbar).Set_Style (Toolbar_Both);
   end Set_Both;

   ------------------
   -- Make_Toolbar --
   ------------------

   procedure Make_Toolbar (Toolbar    : out Gtk_Toolbar;
                           With_Entry : Boolean := False)
   is
      The_Entry : Gtk_Entry;
      Button    : Gtk_Tool_Button;
      Separator : Gtk_Separator_Tool_Item;
      Item      : Gtk_Tool_Item;

   begin
      Gtk_New (Toolbar);
      Set_Orientation (Toolbar, Orientation_Horizontal);
      Set_Style (Toolbar, Toolbar_Both);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Horizontal");
      Button.Set_Tooltip_Text ("Horizontal toolbar layout");
      Insert (Toolbar, Button);
      Button.On_Clicked (Set_Horizontal'Access, Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Vertical");
      Button.Set_Tooltip_Text ("Vertical toolbar layout");
      Insert (Toolbar, Button);
      Button.On_Clicked (Set_Vertical'Access, Toolbar);

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Icons");
      Button.Set_Tooltip_Text ("Only show toolbar icons");
      Insert (Toolbar, Button);
      Button.On_Clicked (Set_Icons'Access, Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Text");
      Button.Set_Tooltip_Text ("Only show toolbar text");
      Insert (Toolbar, Button);
      Button.On_Clicked (Set_Text'Access, Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Both");
      Button.Set_Tooltip_Text ("Show toolbar icons and text");
      Insert (Toolbar, Button);
      Button.On_Clicked (Set_Both'Access, Toolbar);

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      if With_Entry then
         Gtk_New (Item);
         Insert (Toolbar, Item);

         Gtk_New (The_Entry);
         Show (The_Entry);
         Add (Item, The_Entry);

         Gtk_New (Separator);
         Insert (Toolbar, Separator);
      end if;

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      --  Examples on how to insert buttons using named icons.
      --  Those icons are looked up in the icon theme (see Gtk_Icon_Theme)
      --  Valid names correspond to the base file names for files any of
      --  the subdirectories of the icon theme, for instance in
      --    <prefix>/share/icons/*/
      --  The theme will automatically use different variants of the icon,
      --  for instance depending on left-to-right or right-to-left writting,
      --  or based on the color theme.

      Gtk_New (Button);
      Button.Set_Icon_Name ("media-record");
      Insert (Toolbar, Button);

      Gtk_New (Button);
      Button.Set_Icon_Name ("media-playback-pause");
      Insert (Toolbar, Button);

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      Gtk_New (Button);
      Button.Set_Icon_Name ("media-playback-start");
      Insert (Toolbar, Button);

      Gtk_New (Button);
      Button.Set_Icon_Name ("media-playback-stop");
      Insert (Toolbar, Button);

   end Make_Toolbar;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Toolbar : Gtk_Toolbar;
      Box     : Gtk_Box;
   begin
      Set_Label (Frame, "Toolbar");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Frame.Add (Box);

      Make_Toolbar (Toolbar, True);
      Set_Orientation (Toolbar, Orientation_Horizontal);
      Box.Pack_Start (Toolbar, Fill => False, Expand => False);

      Show_All (Frame);
   end Run;

end Create_Toolbar;
