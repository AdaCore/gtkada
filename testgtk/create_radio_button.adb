-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------

with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Object; use Gtk.Object;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

package body Create_Radio_Button is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Widget2_Cb is new Signal.Callback (Gtk_Widget, Gtk_Widget_Access);

   Window : aliased Gtk.Window.Gtk_Window;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id        : Guint;
      Box1,
        Box2    : Gtk_Box;
      Separator : Gtk_Separator;
      Button    : Gtk_Radio_Button;
      Button2   : Gtk_Button;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Set_Title (Window, "radio buttons");
         Border_Width (Window, Border_Width => 0);

         Gtk_New_Vbox (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Gtk_New (Button, Widget_SList.Null_List, "button1");
         Pack_Start (Box2, Button, True, True, 0);
         Show (Button);

         Gtk_New (Button, Group (Button), "button2");
         Set_State (Button, True);
         Pack_Start (Box2, Button, True, True, 0);
         Show (Button);

         Gtk_New (Button, Group (Button), "button3");
         Pack_Start (Box2, Button, True, True, 0);
         Show (Button);

         Gtk_New_Hseparator (Separator);
         Pack_Start (Box1, Separator, False, True, 0);
         Show (Separator);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, False, True, 0);
         Show (Box2);

         Gtk_New (Button2, "Close");
         Id := Widget_Cb.Connect (Button2, "clicked", Destroy'Access, Window);
         Pack_Start (Box2, Button2, True, True, 0);
         Set_Flags (Button2, Can_Default);
         Grab_Default (Button2);
         Show (Button2);

      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Radio_Button;

