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
with Gdk.Types; use Gdk.Types;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Object; use Gtk.Object;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Table; use Gtk.Table;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Window; use Gtk.Window;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with Common; use Common;

package body Create_Scrolled is

   Window : aliased Gtk.Dialog.Gtk_Dialog;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button) is
      Id        : Guint;
      Button    : Gtk_Button;
      Table     : Gtk_Table;
      Scrolled  : Gtk_Scrolled_Window;
      Toggle    : Gtk_Toggle_Button;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                  Window'Access);
         Set_Title (Window, "Scrolled Window");
         Border_Width (Window, Border_Width => 0);

         Gtk_New (Scrolled);
         Border_Width (Scrolled, 10);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
         Pack_Start (Get_Vbox (Window), Scrolled, True, True, 0);
         Show (Scrolled);

         Gtk_New (Table, 20, 20, False);
         Set_Row_Spacings (Table, 10);
         Set_Col_Spacings (Table, 10);
         Add (Scrolled, Table);
         Set_Focus_Hadjustment (Table, Get_Hadjustment (Scrolled));
         Set_Focus_Vadjustment (Table, Get_Vadjustment (Scrolled));
         Show (Table);

         for I in 0 .. 19 loop
            for J in 0 .. 19 loop
               Gtk_New (Toggle, "button (" & Integer'Image (I)
                        & "," & Integer'Image (J) & ")");
               Attach_Defaults (Table, Toggle, Gint (I), Gint (I + 1),
                                Gint (J), Gint (J + 1));
               Show (Toggle);
            end loop;
         end loop;

         Gtk_New (Button, "close");
         Id := Widget_Cb.Connect (Button, "clicked", Destroy'Access, Window);
         Set_Flags (Button, Can_Default);
         Pack_Start (Get_Action_Area (Window), Button, True, True, 0);
         Grab_Default (Button);
         Show (Button);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Show (Window);
      else
         Destroy (Window);
      end if;

   end Run;

end Create_Scrolled;

