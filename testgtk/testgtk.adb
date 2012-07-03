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

with Ada.Text_IO;        use Ada.Text_IO;
with Gdk.Display;        use Gdk.Display;
with Gdk.Screen;         use Gdk.Screen;
with Glib;               use Glib;
with Glib.Error;         use Glib.Error;
with Gtk.Css_Provider;   use Gtk.Css_Provider;
with Gtk.Style_Provider; use Gtk.Style_Provider;
with Gtk.Style_Context;
with Gtk.Main;
with Main_Windows;

procedure Testgtk is
   Css     : Gtk_Css_Provider;
   Win     : Main_Windows.Main_Window;
   Display : Gdk_Display;
   Screen  : Gdk_Screen;
   Error   : aliased GError;
begin
   Gtk.Main.Init;

   Gtk_New (Css);
   if not Css.Load_From_Path ("testgtk.css", Error'Access) then
      Put_Line ("Could not load testgtk.css");
      Put_Line (Get_Message (Error));
   else
      Display := Get_Default;
      Screen  := Get_Default_Screen (Display);
      Gtk.Style_Context.Add_Provider_For_Screen
        (Screen, +Css, Priority => Priority_Theme + 1);
   end if;

   Main_Windows.Gtk_New (Win);
   Main_Windows.Show_All (Win);
   Gtk.Main.Main;
end Testgtk;
