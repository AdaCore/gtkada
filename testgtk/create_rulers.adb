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
with Gdk.Types; use Gdk.Types;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Ruler; use Gtk.Ruler;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Table; use Gtk.Table;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Common; use Common;

package body Create_Rulers is

   package Ruler_Cb is new Signal.Object_Callback (Gtk_Ruler);

   Window : aliased Gtk.Window.Gtk_Window;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button) is
      Id        : Guint;
      Ruler     : GTk_Ruler;
      Table     : Gtk_Table;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Set_Title (Window, "Ruler");
         Set_Border_Width (Window, Border_Width => 0);
         Set_Usize (Window, 300, 300);
         Set_Events (Window, Pointer_Motion_Mask + Pointer_Motion_Hint_Mask);

         Gtk_New (Table, 2, 2, False);
         Add (Window, Table);
         Show (Table);

         Gtk_New_Hruler (Ruler);
         Set_Range (Ruler, 5.0, 15.0, 0.0, 20.0);
         Id := C_Unsafe_Connect (Window, "motion_notify_event",
                                 Get_Default_Motion_Notify_Event (Ruler),
                                 Ruler);
         Attach (Table, Ruler, 1, 2, 0, 1, Expand + Enums.Fill, Enums.Fill, 0, 0);
         Show (Ruler);

         Gtk_New_Vruler (Ruler);
         Set_Range (Ruler, 5.0, 15.0, 0.0, 20.0);
         Id := C_Unsafe_Connect (Window, "motion_notify_event",
                                 Get_Default_Motion_Notify_Event (Ruler),
                                 Ruler);
         Attach (Table, Ruler, 0, 1, 1, 2, Enums.Fill, Expand + Enums.Fill, 0, 0);
         Show (Ruler);


      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Show (Window);
      else
         Destroy (Window);
      end if;

   end Run;

end Create_Rulers;

