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

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Gdk.RGBA;                 use Gdk.RGBA;
with Gdk.Types;                use Gdk.Types;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Label;                use Gtk.Label;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Canvas_View;       use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Gtkada.Style;             use Gtkada.Style;

package body Create_Canvas_View_Animate is

   Width  : constant Gdouble := 30.0;
   Height : constant Gdouble := 30.0;

   type My_View_Record is new Canvas_View_Record with record
      Item1 : access Abstract_Item_Record'Class;
      Item2 : access Abstract_Item_Record'Class;
   end record;
   type My_View is access all My_View_Record'Class;

   function On_Item_Event
     (View  : not null access GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  Called when the canvas reports an event

   function On_Item_Event_Zoom is new On_Item_Event_Zoom_Generic
     (Modifier => Mod1_Mask,
      Factor   => 2.0,
      Duration => 0.4,
      Easing   => Easing_Linear'Access);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "The @bCanvas_View@B widget provides an animation framework"
        & " which can be used when moving or resizing items, scaling the"
        & " view, changing styles,..." & ASCII.LF
        & "In this demo, click with left or right mouse button to move items"
        & " with animation, or alt-wheel to zoom the canvas with animation.";
   end Help;

   -------------------
   -- On_Item_Event --
   -------------------

   function On_Item_Event
      (View  : not null access GObject_Record'Class;
       Event : Event_Details_Access)
     return Boolean
   is
      Self : constant My_View := My_View (View);
      It   : Abstract_Item;
   begin
      if Event.Event_Type = Button_Press then
         if Event.Button = 1 then
            It := Abstract_Item (Self.Item1);
         else
            It := Abstract_Item (Self.Item2);
         end if;

         Animate_Position
            (It,
             (Event.M_Point.X - Width / 2.0,
              Event.M_Point.Y - Height / 2.0),
             Duration => 1.6,
             Easing   => Easing_Out_Elastic'Access).Start (Self);
      end if;
      return False;  --  fallback to other handlers
   end On_Item_Event;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box           : Gtk_Box;
      Canvas        : constant My_View := new My_View_Record;
      Model         : List_Canvas_Model;
      Link          : Canvas_Link;
      Scrolled      : Gtk_Scrolled_Window;
      Label         : Gtk_Label;
   begin
      Gtk_New (Model);

      Gtkada.Canvas_View.Initialize (Canvas);
      Canvas.On_Item_Event (On_Item_Event'Access);
      Canvas.On_Item_Event (On_Item_Event_Zoom'Access);

      Canvas.Item1 := Gtk_New_Rect
         (Gtk_New (Stroke => Black_RGBA), Width => Width, Height => Height);
      Canvas.Item1.Set_Position ((0.0, 0.0));
      Model.Add (Canvas.Item1);

      Canvas.Item2 := Gtk_New_Rect
         (Gtk_New (Stroke => (0.4, 0.4, 0.4, 1.0)),
          Width => Width, Height => Height);
      Canvas.Item2.Set_Position ((300.0, 300.0));
      Model.Add (Canvas.Item2);

      Link := Gtk_New
         (Canvas.Item1, Canvas.Item2, Gtk_New (Stroke => Black_RGBA));
      Model.Add (Link);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Frame.Add (Box);

      Gtk_New
         (Label,
          "Click (left or right) in canvas to move the item, with animation");
      Box.Pack_Start (Label, Expand => False, Fill => False);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Always, Policy_Always);
      Box.Pack_Start (Scrolled, Expand => True, Fill => True);

      Canvas.Set_Model (Model);
      Unref (Model);
      Scrolled.Add (Canvas);

      Frame.Show_All;
   end Run;

end Create_Canvas_View_Animate;
