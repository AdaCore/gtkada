-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

with Ada.Text_IO;         use Ada.Text_IO;
with Gtk.Box;             use Gtk.Box;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Print_Operation; use Gtk.Print_Operation;
with Gtk.Window;          use Gtk.Window;

package body Create_Print is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This is a very simple demo to demonstrate GTK+'s high-level,"
        & " portable printing API.  On some platforms, Gtk_Print_Operation"
        & " uses the native print dialog.  On platforms which do not provide"
        & " a native print dialog, GTK+ uses its own.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box      : Gtk_Box;
      Print_Op : Gtk_Print_Operation;
      Result   : Gtk_Print_Operation_Result;
   begin
      Set_Label (Frame, "Printing");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Gtk_New (Print_Op);
      Result :=
        Run (Print_Op, Action_Print_Dialog, Gtk_Window (Get_Toplevel (Frame)));
      Put_Line ("Result is " & Result'Img);

      Show_All (Frame);
   end Run;

end Create_Print;
