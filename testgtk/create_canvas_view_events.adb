------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2014, AdaCore                          --
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

with Ada.Tags;            use Ada.Tags;
with Cairo;               use Cairo;
with Glib;                use Glib;
with Glib.Object;         use Glib.Object;
with Gdk.RGBA;            use Gdk.RGBA;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtkada.Canvas_View;  use Gtkada.Canvas_View;
with Gtkada.Style;        use Gtkada.Style;
with Pango.Font;          use Pango.Font;

package body Create_Canvas_View_Events is

   type Demo_View_Record is new Canvas_View_Record with record
      Console : Text_Item;
      --  A text item that reports the events from the view.
   end record;
   type Demo_View is access all Demo_View_Record'Class;

   procedure On_Item_Event
      (View  : not null access GObject_Record'Class;
       Event : Event_Details_Access);
   --  Called when the canvas reports an event

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
        & " the view, in particular ""@bitem_event@B"".";
   end Help;

   -------------------
   -- On_Item_Event --
   -------------------

   procedure On_Item_Event
      (View  : not null access GObject_Record'Class;
       Event : Event_Details_Access)
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

      if Event.Event_Type = Button_Press then
         if Event.Toplevel_Item /= null then
            --  Enable moving the item
            Event.Allowed_Drag_Area := Drag_Anywhere;
         else
            --  Enable scrolling by dragging the background. However, there is
            --  no point showing an area where there is no item, so we limit
            --  the scrolling.
            Event.Allowed_Drag_Area :=
              Self.Model.Bounding_Box (Margin => View_Margin / Self.Get_Scale);
         end if;
      end if;

      Self.Model.Refresh_Layout;
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

      L1, L2        : Canvas_Link;
      It1, It3      : Rect_Item;
      It2           : Polyline_Item;

      L             : Gdouble;
   begin
      Gtk_New (Model);

      Canvas := new Demo_View_Record;
      Gtkada.Canvas_View.Initialize (Canvas);
      Canvas.On_Item_Event (On_Item_Event'Access);

      Font := Gtk_New (Stroke => Null_RGBA,
                       Font   => (Name   => From_String ("sans 8"),
                                  others => <>));
      Canvas.Console := Gtk_New_Text (Font, "");
      Canvas.Console.Set_Position ((0.0, 0.0));
      Model.Add (Canvas.Console);

      Black := Gtk_New (Fill => Create_Rgba_Pattern ((1.0, 0.0, 0.0, 0.5)));
      It1 := Gtk_New_Rect (Black, 80.0, 80.0);
      It1.Set_Position ((50.0, 50.0));
      Model.Add (It1);

      L := 30.0;
      It2 := Gtk_New_Polyline
        (Black,
         ((2.0 * L, L * S),
          (1.5 * L, L * S * 2.0),
          (0.5 * L, L * S * 2.0),
          (0.0,     L * S),
          (0.5 * L, 0.0),
          (1.5 * L, 0.0)),
         Close => True);
      It2.Set_Position ((250.0, 250.0));
      Model.Add (It2);

      It3 := Gtk_New_Rect (Black, 40.0, 80.0);
      It3.Set_Position ((50.0, 220.0));
      Model.Add (It3);

      L1 := Gtk_New (From => It1, To => It2, Style => Gtk_New);
      Model.Add (L1);

      L2 := Gtk_New (From => It2, To => It3, Style => Gtk_New);
      Model.Add (L2);

      It3 := Gtk_New_Rect (Black, 40.0, 80.0);
      It3.Set_Position ((650.0, 400.0));
      Model.Add (It3);

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
