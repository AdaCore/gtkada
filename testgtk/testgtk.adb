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

with Ada.Text_IO;        use Ada.Text_IO;
with Gtkada.Bindings;    use Gtkada.Bindings;
with Gtkada.Style;       use Gtkada.Style;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Style_Provider; use Gtk.Style_Provider;
with Gtk.Main;

with Global_Exception_Handler;
with Main_Windows;

procedure Testgtk is
   Win     : Main_Windows.Main_Window;
begin
   Gtk.Main.Init;

   --  For debug purposes only
   Gtkada.Bindings.Set_On_Exception
     (Global_Exception_Handler.On_Exception'Access);

   Load_Css_File
      ("testgtk.css", Ada.Text_IO.Put_Line'Access, Priority_Application);

   Main_Windows.Gtk_New (Win);
   Win.Set_Position (Win_Pos_Center);
   Main_Windows.Show_All (Win);
   Gtk.Main.Main;
end Testgtk;
