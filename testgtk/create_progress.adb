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
-- Library General Public License for more details.                  --
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
with Gdk.Types; use Gdk.Types;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Label; use Gtk.Label;
with Gtk.Main; use Gtk.Main;
with Gtk.Misc; use Gtk.Misc;
with Gtk.Object; use Gtk.Object;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Tooltips; use Gtk.Tooltips;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Common; use Common;

package body Create_Progress is

   package Time_Cb   is new Gtk.Main.Timeout (Gtk_Progress_Bar);

   Window     : aliased Gtk.Dialog.Gtk_Dialog;
   Timeout_Id : Guint := 0;

   procedure Destroy_Progress (Window : in out Gtk_Widget'Class) is
   begin
      Timeout_Remove (Timeout_Id);
      Timeout_Id := 0;
      Gtk.Widget.Destroy (Window);
   end Destroy_Progress;

   function Progress_Timeout (Pbar : in Gtk_Progress_Bar) return Boolean is
      New_Val : Gfloat;
   begin
      New_Val := Get_Percentage (Pbar);
      if New_Val >= 1.0 then
         New_Val := 0.0;
      end if;
      New_Val := New_Val + 0.02;
      Update (Pbar, New_Val);
      return True;
   end Progress_Timeout;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id       : Guint;
      Tooltips : Gtk_Tooltips;
      Vbox     : Gtk_Box;
      Pbar     : Gtk_Progress_Bar;
      Button   : Gtk_Button;
      Label    : Gtk_Label;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Set_Title (Window, "progress bar");
         Border_Width (Window, Border_Width => 0);

         Gtk_New (Tooltips);

         Gtk_New_Vbox (Vbox, False, 5);
         Border_Width (Vbox, 10);
         Pack_Start (Get_Vbox (Window), Vbox, True, True, 0);
         Show (Vbox);

         Gtk_New (Label, "progress...");
         Set_Alignment (Label, 0.0, 0.5);
         Pack_Start (Vbox, Label, False, True, 0);
         Show (Label);

         Gtk_New (Pbar);
         Set_Events (Pbar, Enter_Notify_Mask + Leave_Notify_Mask);
         Set_Usize (Pbar, 200, 20);
         Pack_Start (Vbox, Pbar, True, True, 0);
         Show (Pbar);

         Set_Tip (Tooltips, Pbar, "Countdown is progressing yet!", "Secret");
         Set_Delay (Tooltips, 0);

         Timeout_Id := Time_Cb.Add (100, Progress_Timeout'Access, Pbar);

         Gtk_New (Button, "close");
         Id := Widget_Cb.Connect (Button, "clicked", Destroy_Progress'Access,
                                  Window);
         Set_Flags (Button, Can_Default);
         Pack_Start (Get_Action_Area (Window), Button, True, True, 0);
         Grab_Default (Button);
         Show (Button);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Progress;

