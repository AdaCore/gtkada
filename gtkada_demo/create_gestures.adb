------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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

with Ada.Numerics;      use Ada.Numerics;
with Cairo;             use Cairo;
with Gdk.Event;         use Gdk.Event;
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Gtk.Box;           use Gtk.Box;
with Gtk.Drawing_Area;  use Gtk.Drawing_Area;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Frame;         use Gtk.Frame;
with Gtk.Label;         use Gtk.Label;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Gesture;             use Gtk.Gesture;
with Gtk.Gesture_Long_Press;  use Gtk.Gesture_Long_Press;
with Gtk.Gesture_Zoom;        use Gtk.Gesture_Zoom;

package body Create_Gestures is

   type My_Drawing_Area_Record is new Gtk_Drawing_Area_Record with record
      Long_Press_X : Gdouble := Gdouble'First;
      Long_Press_Y : Gdouble := Gdouble'First;

      Zoom_Scale   : Gdouble := 1.0;
   end record;
   type My_Drawing_Area is access all My_Drawing_Area_Record'Class;

   procedure On_Long_Press
      (Area : access GObject_Record'Class;
       X, Y : Gdouble);
   procedure On_Long_Press_End
      (Area : access GObject_Record'Class;
       Sequence : Gdk_Event_Sequence);
   procedure On_Scale_Changed
      (Area : access GObject_Record'Class;
       Scale : Gdouble);

   function On_Draw
      (Area : access Gtk_Widget_Record'Class;
       Cr   : Cairo.Cairo_Context) return Boolean;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "";
   end Help;

   ----------------------
   -- On_Scale_Changed --
   ----------------------

   procedure On_Scale_Changed
      (Area : access GObject_Record'Class;
       Scale : Gdouble)
   is
      A : constant My_Drawing_Area := My_Drawing_Area (Area);
   begin
      A.Zoom_Scale := Scale;
      A.Queue_Draw;
   end On_Scale_Changed;

   -------------------
   -- On_Long_Press --
   -------------------

   procedure On_Long_Press
      (Area : access GObject_Record'Class;
       X, Y : Gdouble)
   is
      A : constant My_Drawing_Area := My_Drawing_Area (Area);
   begin
      A.Long_Press_X := X;
      A.Long_Press_Y := Y;
      A.Queue_Draw;
   end On_Long_Press;

   -----------------------
   -- On_Long_Press_End --
   -----------------------

   procedure On_Long_Press_End
      (Area : access GObject_Record'Class;
       Sequence : Gdk_Event_Sequence)
   is
      pragma Unreferenced (Sequence);
      A : constant My_Drawing_Area := My_Drawing_Area (Area);
   begin
      A.Long_Press_X := Gdouble'First;
      A.Queue_Draw;
   end On_Long_Press_End;

   -------------
   -- On_Draw --
   -------------

   function On_Draw
      (Area : access Gtk_Widget_Record'Class;
       Cr   : Cairo.Cairo_Context) return Boolean
   is
      A : constant My_Drawing_Area := My_Drawing_Area (Area);
      Alloc : Gtk_Allocation;
   begin
      Area.Get_Allocation (Alloc);

      Cairo.Scale (Cr, A.Zoom_Scale, A.Zoom_Scale);

      Cairo.Save (Cr);
      Cairo.Rectangle
         (Cr, 5.0, 5.0, Gdouble (Alloc.Width) - 10.0,
          Gdouble (Alloc.Height) - 10.0);
      Cairo.Set_Source_Rgba (Cr, 1.0, 0.0, 0.0, 0.8);
      Cairo.Set_Line_Width (Cr, 2.0);
      Cairo.Stroke (Cr);
      Cairo.Restore (Cr);

      Cairo.Save (Cr);
      Cairo.Rectangle (Cr, 50.0, 50.0, 50.0, 50.0);
      Cairo.Set_Source_Rgba (Cr, 1.0, 0.0, 0.0, 0.8);
      Cairo.Stroke (Cr);
      Cairo.Restore (Cr);

      if A.Long_Press_X /= Gdouble'First then
         Cairo.Save (Cr);
         Cairo.Arc
            (Cr, A.Long_Press_X, A.Long_Press_Y,
             Radius => 20.0, Angle1 => 0.0, Angle2 => 2.0 * Pi);
         Cairo.Set_Source_Rgba (Cr, 0.0, 0.0, 1.0, 0.8);
         Cairo.Stroke (Cr);
         Cairo.Restore (Cr);
      end if;

      return True;
   end On_Draw;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Area     : My_Drawing_Area;
      Box      : Gtk_Box;
      Label    : Gtk_Label;
   begin
      Frame.Set_Label ("Gestures");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Frame.Add (Box);

      Gtk_New
         (Label,
         "Try various gestures below: long press and zoom (on some hardware)");
      Box.Pack_Start (Label, Expand => False, Fill => True);

      Area := new My_Drawing_Area_Record;
      Gtk.Drawing_Area.Initialize (Area);
      Box.Pack_Start (Area, Expand => True, Fill => True);
      Area.On_Draw (On_Draw'Access);

      declare
         G : Gtk_Gesture_Long_Press;
      begin
         Gtk_New (G, Area);
         G.On_Pressed (On_Long_Press'Access, Area);
         G.On_End (On_Long_Press_End'Access, Area);
         G.Set_Propagation_Phase (Phase_Bubble);
         --  Weak_Ref (Area, Glib.Object.Unref, G);
      end;

      declare
         G : Gtk_Gesture_Zoom;
      begin
         Gtk_New (G, Area);
         G.On_Scale_Changed (On_Scale_Changed'Access, Area);
         G.Set_Propagation_Phase (Phase_Bubble);
      end;

      Frame.Show_All;
   end Run;

end Create_Gestures;
