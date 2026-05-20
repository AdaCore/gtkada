------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Cairo;               use Cairo;
with Gdk.Event;           use Gdk.Event;
with Gdk.Window;          use Gdk.Window;
with Glib;                use Glib;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Button;          use Gtk.Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Label;           use Gtk.Label;
with Gtk.Layout;          use Gtk.Layout;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;

package body Create_Layout is

   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Layout_Record, Boolean);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Layout@B is a mixture between a @bGtk_Viewport@B and"
        & " a @bGtk_Fixed@B. Its children can be located anywhere, the layout"
        & " does not try to align them in any way. As opposed to a"
        & " @bGtk_Fixed@B, there is no limit to the size of a @bGtk_Layout@B"
        & " and it won't use as much memory as a @bGtk_Fixed@B."
        & ASCII.LF
        & "The area that is currently visible is indicated by two"
        & " @bGtk_Alignment@B widgets. It can thus be put directly into a"
        & " @bGtk_Scrolled_Window@B widget, as is the case in this demo."
        & ASCII.LF
        & "In this demo, the background is painted by a callback on the"
        & " draw event, and thus does not occupy any memory."
        & " The Layout has a size of 1600 by 128000.";
   end Help;

   -------------
   -- On_Draw --
   -------------

   function On_Draw
      (Layout : access Gtk_Layout_Record'Class;
       Cr     : Cairo_Context) return Boolean
   is
      Imin, Imax : Gdouble;
      Jmin, Jmax : Gdouble;
      Xmin, Xmax : Gint;
      Ymin, Ymax : Gint;
      X, Y : Gint;
   begin
      Gdk.Window.Get_Position (Layout.Get_Bin_Window, X, Y);
      Cairo.Translate (Cr, Gdouble (X), Gdouble (Y));

      Clip_Extents (Cr, Imin, Jmin, Imax, Jmax);
      Xmin := Gint (Imin / 10.0);
      Xmax := Gint ((Imax + 9.0) / 10.0);
      Ymin := Gint (Jmin / 10.0);
      Ymax := Gint ((Jmax + 9.0) / 10.0);

      for I in Xmin .. Xmax - 1 loop
         for J in Ymin .. Ymax - 1 loop
            if (I + J) mod 2 /= 0 then
               Rectangle (Cr,
                          X      => Gdouble (10 * I),
                          Y      => Gdouble (10 * J),
                          Width  => Gdouble (1 + I mod 10),
                          Height => Gdouble (1 + J mod 10));
            end if;
         end loop;
      end loop;

      Set_Source_Rgb (Cr, Red => 0.0, Green => 0.0, Blue => 0.0);
      Cairo.Fill (Cr);

      return True;
   end On_Draw;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Scrolled : Gtk_Scrolled_Window;
      Layout   : Gtk_Layout;
      Button   : Gtk_Button;
      Label    : Gtk_Label;
   begin
      Set_Label (Frame, "Layout");
      Gtk_New (Scrolled);
      Set_Shadow_Type (Scrolled, Shadow_In);
      Add (Frame, Scrolled);
      Set_Placement (Scrolled, Corner_Top_Right);

      Gtk_New (Layout);
      Set_Events (Layout, Exposure_Mask);
      Add (Scrolled, Layout);

      Set_Step_Increment (Get_Hadjustment (Layout), 10.0);
      Set_Step_Increment (Get_Vadjustment (Layout), 10.0);

      Event_Cb.Connect (Layout, Signal_Draw,
                        Event_Cb.To_Marshaller (On_Draw'Access));
      Set_Size (Layout, 1600, 128000);

      for I in 0 .. Gint'(15) loop
         for J in 0 .. Gint'(15) loop
            if (I + J) mod 2 /= 0 then
               Gtk_New (Button, "Button " & Gint'Image (I)
                        & " " & Gint'Image (J));
               Put (Layout, Button, J * 100, I * 100);
            else
               Gtk_New (Label, "Label " & Gint'Image (I)
                        & " " & Gint'Image (J));
               Put (Layout, Label, J * 100, I * 100);
            end if;
         end loop;
      end loop;

      for I in 16 .. Gint'(1279) loop
         if I mod 2 /= 0 then
            Gtk_New (Button, "Button " & Gint'Image (I) & " 0");
            Put (Layout, Button, 0, I * 100);
         else
            Gtk_New (Label, "Label " & Gint'Image (I) & " 0");
            Put (Layout, Label, 0, I * 100);
         end if;
      end loop;

      Show_All (Frame);
   end Run;

end Create_Layout;
