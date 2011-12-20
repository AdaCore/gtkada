------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Gtk.Main;
with Gtk.Window; use Gtk.Window;
with Tictactoe; use Tictactoe;

procedure TTT_Test is
   Window : Gtk_Window;
   Tic    : Gtk_Tictactoe;
begin
   Gtk.Main.Init;

   Gtk_New (Window);

   Set_Title (Window, "Aspect Frame");

   Widget_Cb.Connect
     (Window, "destroy", Widget_Cb.To_Marshaller (Quit'Access));

   Set_Border_Width (Window, 10);

   Gtk_New (Tic);
   Add (Window, Tic);
   Show (Tic);

   Tictactoe_Cb.Connect (Tic, "tictactoe",
     Tictactoe_Cb.To_Marshaller (Win'Access));

   Show (Window);

   Gtk.Main.Main;
end TTT_Test;
