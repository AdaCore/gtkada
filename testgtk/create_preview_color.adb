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
with Gtk.Enums; use Gtk.Enums;
with Gtk.Main; use Gtk.Main;
with Gtk.Preview; use Gtk.Preview;
with Gtk.Signal;  use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with Common; use Common;

with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;

with Ada.Text_IO; use Ada.Text_IO;

package body Create_Preview_Color is

   package Preview_Idle is new Gtk.Main.Idle (Gtk_Preview);
   package Map_Cb is new Gtk.Signal.Void_Callback (Gtk_Preview_Record);
   package Expose_Cb is new Gtk.Signal.Two_Callback
     (Gtk_Preview_Record, Integer, Gdk_Event_Expose);

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
      --  Draw (Preview);
      -- gtk_signal_emit (GTK_OBJECT (widget), widget_signals[DRAW], area);
--      Put_Line ("Color_Idle");

--        Allocate (Event, Expose, Get_Window (Preview));
--        Set_Area (Event, Full_Area);
--        Expose_Cb.Emit_By_Name (Preview, "expose_event", Event);
      return True;
   end Color_Idle_Func;

   procedure Map_Handler (Preview : access Gtk_Preview_Record) is
   begin
--      Put_Line ("Map_Handler");
      Color_Idle := Preview_Idle.Add (Color_Idle_Func'Access,
                                      Gtk_Preview (Preview));
   end Map_Handler;

   procedure Preview_Destroy (Dummy : access Gtk_Widget_Record) is
      pragma Warnings (Off, Dummy);
   begin
      if Color_Idle /= 0 then
         Idle_Remove (Color_Idle);
      end if;
      Color_Idle := 0;
   end Preview_Destroy;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Preview : Gtk_Preview;
      Buf     : Guchar_Array (0 .. 767);
      K       : Natural;
      Id      : Guint;

   begin
      Set_Label (Frame, "Preview Color");

      Gtk_New (Preview, Preview_Color);
      Id := Widget3_Cb.Connect
        (Preview, "destroy", Preview_Destroy'Access);

      Size (Preview, 256, 256);
      Add (Frame, Preview);

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

      --  Color_Idle := Preview_Idle.Add (Color_Idle_Func'Access, Preview);
      Id := Map_Cb.Connect (Preview, "map", Map_Handler'Access);

      Show_All (Frame);
   end Run;

end Create_Preview_Color;

