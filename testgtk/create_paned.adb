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
--         General Public License for more details.                  --
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
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Common; use Common;

package body Create_Paned is

   Window : aliased Gtk.Window.Gtk_Window;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button) is
      VPaned : Gtk_Paned;
      HPaned : Gtk_Paned;
      Frame  : Gtk_Frame;
      Button : Gtk_Button;
      Id     : Guint;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Set_Title (Window, "Panes");
         Border_Width (Window, Border_Width => 0);

         Gtk_New_Vpaned (VPaned);
         Add (Window, VPaned);
         Border_Width (VPaned, 5);
         Show (VPaned);

         Gtk_New_Hpaned (HPaned);
         Add1 (Vpaned, HPaned);

         Gtk_New (Frame);
         Set_Shadow_Type (Frame, Shadow_In);
         Set_Usize (Frame, 60, 60);
         Add1 (HPaned, Frame);
         Show (Frame);

         Gtk_New (Button, "Hi There");
         Add (Frame, Button);
         Show (Button);

         Gtk_New (Frame);
         Set_Shadow_Type (Frame, Shadow_In);
         Set_Usize (Frame, 80, 60);
         Add2 (HPaned, Frame);
         Show (Frame);

         Show (HPaned);

         Gtk_New (Frame);
         Set_Shadow_Type (Frame, Shadow_In);
         Set_Usize (Frame, 60, 80);
         Add2 (VPaned, Frame);
         Show (Frame);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Show (Window);
      else
         Destroy (Window);
      end if;

   end Run;

end Create_Paned;
