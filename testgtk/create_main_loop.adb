-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
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
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Label; use Gtk.Label;
with Gtk.Main; use Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with Common; use Common;

with Ada.Text_IO;

package body Create_Main_Loop is


   Window : aliased Gtk.Dialog.Gtk_Dialog;

   procedure Loop_Destroy (Window : in out Gtk_Widget) is
   begin
      Gtk.Widget.Destroy (Window);
      Main_Quit;
   end Loop_Destroy;

   procedure Loop_Destroy (Window : in out Gtk_Widget;
                           Widget : in out Gtk_Widget_Access) is
   begin
      Loop_Destroy (Window);
      Destroyed (Window, Widget);
   end Loop_Destroy;
 
   procedure Run (Widget : in out Gtk.Button.Gtk_Button) is
      Id     : Guint;
      Label  : Gtk_Label;
      Button : Gtk_Button;
  begin

      if not Is_Created (Window) then
         Gtk_New (Window);
         Id := Widget2_Cb.Connect (Window, "destroy", Loop_Destroy'Access,
                                   Window'Access);
         Set_Title (Window, "test_main_loop");
         Border_Width (Window, Border_Width => 0);

         Gtk_New (Label, "In recursive main loop...");
         Set_Padding (Label, 20, 20);
         Pack_Start (Get_Vbox (Window), Label, True, True, 0);
         Show (Label);

         Gtk_New (Button, "Leave");
         Pack_Start (Get_Action_Area (Window), Button, False, True, 0);
         Id := Widget_Cb.Connect (Button, "clicked", Loop_Destroy'Access, Window);
         Set_Flags (Button, Can_Default);
         Grab_Default (Button);
         Show (Button);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Show (Window);
         Ada.Text_IO.Put_Line ("Create_Mainloop: start");
         Gtk.Main.Main;
         Ada.Text_IO.Put_Line ("Create_Mainloop: done");
      else
         Destroy (Window);
      end if;
   end Run;

end Create_Main_Loop;

