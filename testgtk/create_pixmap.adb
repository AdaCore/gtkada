------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2012, AdaCore                     --
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

with Gdk.Bitmap; use Gdk.Bitmap;
with Gdk.Pixmap; use Gdk.Pixmap;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Label; use Gtk.Label;
with Gtk.Image; use Gtk.Image;
with Gtk.Style; use Gtk.Style;
with Gtk; use Gtk;

package body Create_Pixmap is

   function Help return String is
   begin
      return "This demo simply shows how you can put a pixmap in a"
        & " @bGtk_Button@B simply by putting both in a @bGtk_Box@B, and then"
        & " associating the box with the button.";
   end Help;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1      : Gtk_Box;
      Box2      : Gtk_Box;
      Box3      : Gtk_Box;
      Button    : Gtk_Button;
      Style     : Gtk_Style;
      Pixmap    : Gdk_Pixmap;
      Mask      : Gdk_Bitmap;
      PixmapWid : Gtk_Image;
      Label     : Gtk_Label;

   begin

      Set_Label (Frame, "Pixmap");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, True, True, 0);

      Gtk_New (Button);
      Pack_Start (Box2, Button, False, False, 0);

      Style := Get_Style (Button);
      Create_From_Xpm (Pixmap, Get_Window (Frame), Mask,
                       Get_Bg (Style, State_Normal), "test.xpm");
      Gtk_New (PixmapWid, Pixmap, Mask);

      Gtk_New (Label, "Pixmap" & ASCII.LF & "test");
      Gtk_New_Hbox (Box3, False, 0);
      Set_Border_Width (Box3, 2);
      Add (Box3, PixmapWid);
      Add (Box3, Label);
      Add (Button, Box3);

      Show_All (Frame);
   end Run;

end Create_Pixmap;

