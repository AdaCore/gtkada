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
with Gtk.Curve; use Gtk.Curve;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Gamma_Curve; use Gtk.Gamma_Curve;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

package body Create_Gamma_Curve is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Float_P is new Ada.Numerics.Generic_Elementary_Functions (Gfloat);
   package Widget2_Cb is new Signal.Callback (Gtk_Widget, Gtk_Widget_Access);

   Window : aliased Gtk_Window;
   Count  : Gint := 0;
   Curve  : Gtk_Gamma_Curve;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id    : Guint;
      Max   : Gint := 127 + (Count mod 4) * 128;
      Vec   : Gfloat_Array (1 .. Positive (Max));
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, WIndow_Toplevel);
         Set_Title (Window, "test");
         Border_Width (Window, 10);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Gtk_New (Curve);
         Add (Window, Curve);
         Show (Curve);
      end if;

      if (Count mod 4 /= 3) then
         Ada.Text_IO.Put_Line ("Redrawing the window with "
                               & Gint'Image (Max)
                               & " points");
      end if;
      Set_Range (Get_Curve (Curve), 0.0, Gfloat (Max), 0.0, Gfloat (Max));
      for J in Vec'Range loop
         Vec (J) := (127.0 / Float_P.Sqrt (Gfloat (Max)))
           * Float_P.Sqrt (Gfloat (J));
      end loop;
      Set_Vector (Get_Curve (Curve), Vec);


      if not Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      elsif (Count mod 4 = 3) then
         Gtk.Widget.Destroy (Window);
      end if;

      Count := Count + 1;
   end Run;

end Create_Gamma_Curve;

