-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

with Cairo;        use Cairo;

with Gdk.Cairo;    use Gdk.Cairo;

with Gdk.Event;    use Gdk.Event;
with Gtk.Main;
with Gtk.Window;   use Gtk.Window;
with Gtk.Handlers; use Gtk.Handlers;

procedure Testcairo is
   Win : Gtk_Window;

   type Test_Type is (Rectangles);

   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Window_Record, Boolean);

   function Expose_Cb (Win : access Gtk_Window_Record'Class;
                       Event : Gdk_Event) return Boolean;

   ---------------
   -- Expose_Cb --
   ---------------

   function Expose_Cb (Win : access Gtk_Window_Record'Class;
                       Event : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      Cr : Cairo_Context;
      D  : Gdouble;

      Test : constant Test_Type := Rectangles;
   begin
      Cr := Create (Get_Window (Win));

      case Test is
         when Rectangles =>
            for J in reverse 1 .. 10 loop
               D := Gdouble (J);
               --  Create a color
               Set_Source_Rgb (Cr, D / 10.0, 0.5 - D / 20.0, 0.0);

               --  Draw a rectangle
               Rectangle (Cr, 0.0, 0.0, D * 10.0, D * 10.0);
               Fill (Cr);
            end loop;
      end case;
      Destroy (Cr);
      return True;
   end Expose_Cb;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Win);

   --  Connect to the "expose" event.

   Event_Cb.Connect (Win, "expose_event",
                     Event_Cb.To_Marshaller (Expose_Cb'Access));

   Show_All (Win);
   Gtk.Main.Main;
end Testcairo;
