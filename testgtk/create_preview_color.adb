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
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Main; use Gtk.Main;
with Gtk.Preview; use Gtk.Preview;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

package body Create_Preview_Color is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Preview_Idle is new Gtk.Main.Idle (Gtk_Preview);
   package Widget2_Cb is new Signal.Callback (Gtk_Widget, Gtk_Widget_Access);

   Window : aliased Gtk.Window.Gtk_Window;

   Color_Idle : Guint  := 0;
   Count      : Guchar := 1;

   function Color_Idle_Func (Preview : Gtk_Preview) return Boolean is
      Buf : Guchar_Array (0 .. 767);
      K   : Natural;
   begin
      for I in 0 .. Guchar'(255) loop
         K := 0;
         for J in 0 .. Guchar'(255) loop
            Buf (K + 0) := I + Count;
            Buf (K + 1) := 0;
            Buf (K + 2) := J + Count;
            K := K + 3;
         end loop;
         Draw_Row (Preview, Buf, 0, Gint (I), 256);
      end loop;
      Count := Count + 1;
      Draw (Preview);
      return True;
   end Color_Idle_Func;

   procedure Preview_Destroy (Dummy : in out Gtk_Widget'Class;
                              Widget: in out Gtk_Widget_Access) is
      pragma Warnings (Off, Dummy);
   begin
      Idle_Remove (Color_Idle);
      Color_Idle := 0;
      Gtk.Widget.Destroy (Widget.all);
   end Preview_Destroy;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id      : Guint;
      Preview : Gtk_Preview;
      Buf     : Guchar_Array (0 .. 767);
      K       : Natural;
   begin
      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget2_Cb.Connect (Window, "destroy", Preview_Destroy'Access,
                                   Window'Access);
         Set_Title (Window, "test");
         Border_Width (Window, Border_Width => 10);

         Gtk_New (Preview, Preview_Color);
         Size (Preview, 256, 256);
         Add (Window, Preview);
         Show (Preview);

         for I in 0 .. Guchar'(255) loop
            K := 0;
            for J in 0 .. Guchar'(255) loop
               Buf (K + 0) := I;
               Buf (K + 1) := 0;
               Buf (K + 2) := J;
               K := K + 3;
            end loop;
            Draw_Row (Preview, Buf, 0, Gint (I), 256);
         end loop;

         Color_Idle := Preview_Idle.Add (Color_Idle_Func'Access, Preview);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Preview_Color;

