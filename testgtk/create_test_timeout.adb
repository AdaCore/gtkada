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

with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Label; use Gtk.Label;
with Gtk.Main; use Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with Common; use Common;

package body Create_Test_Timeout is

   package Label_Timeout is new Timeout (Gtk_Label);

   Timeout   : Guint;
   Count  : Integer := 0;

   function Timeout_Test (Label : in Gtk_Label) return Boolean is
   begin
      Count := Count + 1;
      Set_Text (Label, "count:" & Integer'Image (Count));
      return True;
   end Timeout_Test;

   procedure Stop_Timeout (Object : access Gtk_Widget_Record) is
      pragma Warnings (Off, Object);
   begin
      if Timeout /= 0 then
         Timeout_Remove (Timeout);
         Timeout := 0;
         Count := 0;
      end if;
   end Stop_Timeout;

   procedure Destroy_Timeout (Window : access Gtk_Widget_Record) is
   begin
      Stop_Timeout (Window);
   end Destroy_Timeout;

   procedure Start_Timeout (Label : access Gtk_Label_Record) is
   begin
      if Timeout = 0 then
         Timeout := Label_Timeout.Add (100, Timeout_Test'Access,
                                       Gtk_Label (Label));
      end if;
   end Start_Timeout;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Id       : Guint;
      Button   : Gtk_Button;
      Label    : Gtk_Label;
      Box      : Gtk_Box;

   begin
      Set_Label (Frame, "Timeout Test");
      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Add (Frame, Box);

      Gtk_New (Label, "count : 0");
      Set_Padding (Label, 10, 10);
      Pack_Start (Box, Label, False, False, 0);

      Gtk_New (Button, "start");
      Id := Label_Cb.Connect (Button, "clicked", Start_Timeout'Access, Label);
      Set_Flags (Button, Can_Default);
      Pack_Start (Box, Button, False, False, 0);

      Gtk_New (Button, "stop");
      Id := Widget_Cb.Connect
        (Button, "clicked", Destroy_Timeout'Access, Frame);
      Set_Flags (Button, Can_Default);
      Pack_Start (Box, Button, False, False, 0);

      Id := Widget3_Cb.Connect
        (Box, "destroy", Destroy_Timeout'Access);

      Show_All (Frame);
   end Run;

end Create_Test_Timeout;

