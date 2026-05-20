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

with Ada.Tags;                 use Ada.Tags;
with Cairo;                    use Cairo;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Gdk.RGBA;                 use Gdk.RGBA;
with Gdk.Types;                use Gdk.Types;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Canvas_View;       use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Gtkada.Style;             use Gtkada.Style;
with Pango.Font;               use Pango.Font;

package body Create_Canvas_View_Events is

   type Demo_View_Record is new Canvas_View_Record with record
      Console : Text_Item;
      --  A text item that reports the events from the view.
   end record;
   type Demo_View is access all Demo_View_Record'Class;
   overriding procedure Draw_Internal
     (Self    : not null access Demo_View_Record;
      Context : Draw_Context;
      Area    : Model_Rectangle);

   function On_Item_Event
     (View  : not null access GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  Called when the canvas reports an event

   function On_Item_Event_Zoom is new On_Item_Event_Zoom_Generic
     (Modifier => Mod1_Mask);
   function On_Item_Event_Key_Navigate
     is new On_Item_Event_Key_Navigate_Generic (Modifier => Mod1_Mask);
   function On_Item_Event_Key_Scrolls is new On_Item_Event_Key_Scrolls_Generic
     (Modifier => 0);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo illustrates the @binteractive@B features of the"
        & " Canvas_View widget."
        & ASCII.LF
        & "By default, this widget provides very limited handling events."
        & " It knows how to scroll when inserted in a Gtk_Scrolled_Window,"
        & " including reacting to mouse wheels and touchpad events for the"
        & " scrolling."
        & ASCII.LF
        & "But if you want more advanced behavior (@bselection@B of items,"
        & " @bmoving items@B with the mouse or keyboard, @bzooming@B in or"
        & " out, ...) you will need to connect to the signals emitted by"
        & " the view, in particular ""@bitem_event@B""."
        & ASCII.LF
        & "This demo shows the following capabilities:" & ASCII.LF
        & "  - @bdragging items@B with the mouse. Items will tend to @bsnap@B"
        & " to the grid when they get close enough to it, but they are"
        & " not constrained to grid coordinates. Snapping is disabled if you"
        & " press shift during the move. Items will also snap on @bsmart"
        & " guides@B, which are lines matching various points of interest"
        & " from the other items. This provides a convenient way to align"
        & " items."
        & ASCII.LF
        & "  - @boverlap@B avoidance: when an item is moved on top of another"
        & " item, the latter is moved aside."
        & ASCII.LF
        & "  - @bscrolling@B by dragging the background with the mouse."
        & ASCII.LF
        & "  - @bzooming@B with alt-mouse wheel." & ASCII.LF
        & "  - @bkeys@B can be used to move the selected items, or scroll the"
        & " view when no item is selected." & ASCII.LF
        & "  - @bkeys@B can also be used to navigate between items, by"
        & " pressing @balt@B at the same time as the keyboard arrows.";
   end Help;

   -------------------
   -- Draw_Internal --
   -------------------

   overriding procedure Draw_Internal
     (Self    : not null access Demo_View_Record;
      Context : Draw_Context;
      Area    : Model_Rectangle)
   is
      P : Cairo_Pattern;
      B : constant Model_Rectangle := Self.Get_Visible_Area;
   begin
      P := Pattern_Create_Linear
        (X0  => B.X,
         Y0  => B.Y,
         X1  => B.X + B.Width,
         Y1  => B.Y + B.Height);
      Pattern_Add_Color_Stop_Rgb (P, 0.0, 0.76, 0.9, 0.4);
      Pattern_Add_Color_Stop_Rgb (P, 0.7, 0.76, 0.9, 0.4);
      Pattern_Add_Color_Stop_Rgb (P, 1.0, 0.0,  0.0, 0.0);

      Draw_Grid_Lines
        (Self    => Self,
         Style   => Gtk_New (Stroke => (0.8, 0.8, 0.8, 0.8),
                             Fill => P),
         Context => Context,
         Area    => Area);

      Pattern_Destroy (P);

      Canvas_View_Record (Self.all).Draw_Internal (Context, Area);
   end Draw_Internal;

   -------------------
   -- On_Item_Event --
   -------------------

   function On_Item_Event
      (View  : not null access GObject_Record'Class;
       Event : Event_Details_Access)
     return Boolean
   is
      Self : constant Demo_View := Demo_View (View);
      Base_Text : constant String :=
        Canvas_Event_Type'Image (Event.Event_Type)
        & " (button" & Guint'Image (Event.Button) & ")";
   begin
      if Event.Toplevel_Item /= null then
         Self.Console.Set_Text
           (Base_Text & " on "
            & External_Tag (Event.Toplevel_Item'Tag));
      else
         Self.Console.Set_Text (Base_Text);
      end if;

      --  layout changed, since the size of the console has changed
      Self.Model.Refresh_Layout;

      return False;  --  fallback to other handlers
   end On_Item_Event;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      S : constant Gdouble := 0.86602540378443864676;  --  sqrt(3) / 2
      Canvas        : Demo_View;
      Model         : List_Canvas_Model;
      Scrolled      : Gtk_Scrolled_Window;
      Black, Font   : Drawing_Style;

      L1, L2, L3    : Canvas_Link;
      It1, It3      : Rect_Item;
      It2           : Polyline_Item;
      Ellipse       : Ellipse_Item;

      L             : Gdouble;
   begin
      Gtk_New (Model);
      Model.Set_Selection_Mode (Selection_Multiple);

      Canvas := new Demo_View_Record;
      Gtkada.Canvas_View.Initialize (Canvas);
      Canvas.Set_Grid_Size (30.0);
      Canvas.Avoid_Overlap (True);
      Canvas.Set_Snap (Snap_To_Grid   => True,
                       Snap_To_Guides => True);
      --  Connect this one first so that all events are traced
      Canvas.On_Item_Event (On_Item_Event'Access);

      --  Order is irrelevant here.
      Canvas.On_Item_Event (On_Item_Event_Select'Access);
      Canvas.On_Item_Event (On_Item_Event_Move_Item'Access);
      Canvas.On_Item_Event (On_Item_Event_Scroll_Background'Access);
      Canvas.On_Item_Event (On_Item_Event_Key_Navigate'Access);
      Canvas.On_Item_Event (On_Item_Event_Key_Scrolls'Access);
      Canvas.On_Item_Event (On_Item_Event_Zoom'Access);

      Font := Gtk_New (Stroke => Null_RGBA,
                       Font   => (Name   => From_String ("sans 8"),
                                  others => <>));
      Canvas.Console := Gtk_New_Text (Font, "");
      Canvas.Console.Set_Position ((0.0, 0.0));
      Model.Add (Canvas.Console);

      Black := Gtk_New (Fill => Create_Rgba_Pattern ((1.0, 0.0, 0.0, 1.0)));
      It1 := Gtk_New_Rect (Black, 80.0, 80.0);
      It1.Set_Position ((50.0, 50.0));
      Model.Add (It1);

      L := 30.0;
      It2 := Gtk_New_Polyline
        (Gtk_New (Fill => Create_Rgba_Pattern ((1.0, 0.0, 0.0, 1.0)),
                  Shadow => (Color => (0.0, 0.0, 0.0, 0.1), others => <>)),
         ((2.0 * L, L * S),
          (1.5 * L, L * S * 2.0),
          (0.5 * L, L * S * 2.0),
          (0.0,     L * S),
          (0.5 * L, 0.0),
          (1.5 * L, 0.0)),
         Close => True);
      It2.Set_Position ((250.0, 250.0));
      Model.Add (It2);

      It3 := Gtk_New_Rect
        (Gtk_New (Fill => Create_Rgba_Pattern ((0.0, 1.0, 0.0, 1.0)),
                  Shadow => (Color => (0.0, 0.0, 0.0, 0.1), others => <>)),
         50.0, 80.0);
      It3.Set_Position ((50.0, 220.0));
      Model.Add (It3);

      L1 := Gtk_New (From => It1, To => It2, Style => Gtk_New,
                     Routing => Straight);
      Model.Add (L1);

      L2 := Gtk_New (From => It2, To => It3, Style => Gtk_New,
                     Routing => Straight);
      Model.Add (L2);

      L3 := Gtk_New (From => L1, To => L2,
                     Style => Gtk_New (Dashes => (4.0, 4.0)),
                     Routing => Straight);
      Model.Add (L3);

      It3 := Gtk_New_Rect (Black, 50.0, 80.0);
      It3.Set_Position ((650.0, 400.0));
      Model.Add (It3);

      Ellipse := Gtk_New_Ellipse
        (Gtk_New (Fill => Create_Rgba_Pattern ((1.0, 0.0, 0.0, 1.0)),
                  Shadow => (Color => (0.0, 0.0, 0.0, 0.1), others => <>)),
         Width => 70.0, Height => 40.0);
      Ellipse.Set_Position ((200.0, 30.0));
      Model.Add (Ellipse);

      --  Create the view once the model is populated, to avoid a refresh
      --  every time a new item is added.

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Always, Policy_Always);
      Frame.Add (Scrolled);

      Canvas.Set_Model (Model);
      Unref (Model);
      Scrolled.Add (Canvas);

      Frame.Show_All;
   end Run;

end Create_Canvas_View_Events;
