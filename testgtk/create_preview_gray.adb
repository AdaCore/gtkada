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
with Gtk.Enums; use Gtk.Enums;
with Gtk.Main; use Gtk.Main;
with Gtk.Preview; use Gtk.Preview;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with Common; use Common;

package body Create_Preview_Gray is

   package Preview_Idle is new Gtk.Main.Idle (Gtk_Preview);

   Gray_Idle : Guint  := 0;
   Count     : Guchar := 1;

   function Gray_Idle_Func (Preview : Gtk_Preview) return Boolean is
      Buf : Guchar_Array (0 .. 255);
   begin
      for I in 0 .. Guchar'(255) loop
         for J in 0 .. (255) loop
            Buf (J) := I + Guchar (J) + Count;
         end loop;
         Draw_Row (Preview, Buf, 0, Gint (I), 256);
      end loop;
      Count := Count + 1;
      Draw (Preview);
      return True;
   end Gray_Idle_Func;

   procedure Preview_Destroy (Dummy  : access Gtk_Widget_Record) is
      pragma Warnings (Off, Dummy);
   begin
      Idle_Remove (Gray_Idle);
      Gray_Idle := 0;
   end Preview_Destroy;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Preview : Gtk_Preview;
      Buf     : Guchar_Array (0 .. 255);
      Id      : Guint;

   begin
      Set_Label (Frame, "Preview Gray");

      Gtk_New (Preview, Preview_Grayscale);
      Id := Widget3_Cb.Connect
        (Preview, "destroy", Preview_Destroy'Access);
      Size (Preview, 256, 256);
      Add (Frame, Preview);

      for I in 0 .. Guchar'(255) loop
         for J in 0 .. 255 loop
            Buf (J) := I + Guchar (J);
         end loop;
         Draw_Row (Preview, Buf, 0, Gint (I), 256);
      end loop;

      Gray_Idle := Preview_Idle.Add (Gray_Idle_Func'Access, Preview);

      Show_All (Frame);
   end Run;

end Create_Preview_Gray;

