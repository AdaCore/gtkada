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

with Gtk; use Gtk;
with Glib; use Glib;
with Gtk.Box;              use Gtk.Box;
with Gtk.Button;           use Gtk.Button;
with Gtk.Color_Selection;  use Gtk.Color_Selection;
with Gtk.Enums;
with Gtk.Signal;
with Ada.Text_IO;

package body Create_Color_Selection is

   package Color_Sel_Cb is new Signal.Object_Callback
     (Gtk_Color_Selection_Record);
   --  Must be instanciated at library level !

   --------------
   -- Color_Ok --
   --------------

   procedure Color_Ok (Dialog : access Gtk_Color_Selection_Record)
   is
      Color : Color_Array;
   begin
      Get_Color (Dialog, Color);
      for I in Red .. Opacity loop
         Ada.Text_IO.Put_Line (Color_Index'Image (I)
                               & " => "
                               & Gdouble'Image (Color (I)));
      end loop;
      Set_Color (Dialog, Color);
   end Color_Ok;

   ---------
   -- Run --
   ---------

   procedure Run
     (Frame : access Gtk_Frame_Record'Class)
   is
      Cb_Id     : Guint;
      Color     : Gtk_Color_Selection;
      Box, Hbox : Gtk_Box;
      Button    : Gtk_Button;

   begin

      Set_Label (Frame, "Color Selection");
      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Gtk_New (Color);
      Set_Opacity (Color, True);
      Pack_Start (Box, Color, Expand => False, Fill => False);

      Gtk_New_Hbox (Hbox, Homogeneous => True);
      Pack_Start (Box, Hbox, Expand => False, Fill => True);

      Gtk_New (Button, "Print Color Value");
      Pack_Start (Hbox, Button, Expand => False, Fill => True);
      Cb_Id := Color_Sel_Cb.Connect
        (Button, "clicked", Color_Ok'Access, Color);

      Show_All (Frame);
   end Run;

end Create_Color_Selection;

