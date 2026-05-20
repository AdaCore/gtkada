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

with Gdk.RGBA;                  use Gdk.RGBA;
with Gdk.Types;                 use Gdk.Types;
with Glib;                      use Glib;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtkada.Canvas_View;        use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Models; use Gtkada.Canvas_View.Models;
with Gtkada.Canvas_View.Views;  use Gtkada.Canvas_View.Views;
with Gtkada.Style;              use Gtkada.Style;

package body Create_Canvas_View_Rtrees is

   function On_Item_Event_Zoom is new On_Item_Event_Zoom_Generic
      (Modifier => Mod1_Mask);

   package List_Rtrees is new Rtree_Models (List_Canvas_Model_Record);
   use List_Rtrees;
   --  Rtree models based on list models

   Items_Count : constant := 30_000;
   Per_Row     : constant := 400;
   Rows        : constant := Items_Count / Per_Row + 1;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "Multiple data models can be used to store the items that must"
        & " be displayed in a canvas." & ASCII.LF
        & "The simplest model is a @bList_Canvas_Model@B which provides a"
        & " very simple implementation, which is reasonably for small graphs"
        & " and can be extended easily without having to override all its"
        & " primitives." & ASCII.LF
        & "But if you needs to show thousands of items (this demo"
        & " displays@b" & Integer'Image (Items_Count) & "@B items and@b"
        & Integer'Image (2 * Items_Count - Per_Row - Rows) & "@B links)"
        & " you will need to wrap that simple model with a @bRtree_Model@B,"
        & " which provides a much more efficient implementation for some of"
        & " the queries, like finding the list of items in a given region of"
        & " the screen.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Size          : constant Gdouble := 20.0;
      Margin        : constant Gdouble := 20.0;

      Items         : array (0 .. Per_Row - 1, 0 .. Rows) of Rect_Item;
      X, Y          : Integer;

      Canvas        : Canvas_View;
      Model         : List_Rtrees.Rtree_Model;
      --  Model         : List_Canvas_Model;
      Scrolled      : Gtk_Scrolled_Window;
      Filled        : Drawing_Style;
      Rect          : Rect_Item;
      Link          : Canvas_Link;
      Link_Style    : Drawing_Style;

   begin
      Gtk_New (Model);
      Model.Set_Selection_Mode (Selection_Single);

      Canvas := new Canvas_View_Record;
      Gtkada.Canvas_View.Initialize (Canvas);
      Canvas.On_Item_Event (On_Item_Event_Select'Access);
      Canvas.On_Item_Event (On_Item_Event_Move_Item'Access);
      Canvas.On_Item_Event (On_Item_Event_Scroll_Background'Access);
      Canvas.On_Item_Event (On_Item_Event_Zoom'Access);

      Link_Style := Gtk_New (Stroke => Black_RGBA);

      for Num in 0 .. Items_Count loop
         X := Num mod Per_Row;
         Y := Num / Per_Row;

         --  Creating one style per item is not very efficient, it is
         --  better to share styles whenever possible.

         Filled := Gtk_New
           (Stroke => Black_RGBA,
            Fill => Create_Rgba_Pattern
               ((0.0,
                 1.0 / Gdouble (Per_Row) * Gdouble (X),
                 1.0 / Gdouble (Rows) * Gdouble (Y),
                 0.8)));

         Rect := Gtk_New_Rect (Filled, Size, Size);
         Rect.Set_Position
            (((Size + Margin) * Gdouble (X),
              (Size + Margin) * Gdouble (Y)));
         Model.Add (Rect);

         Items (X, Y) := Rect;
         if X > 0 then
            Link := Gtk_New (Items (X - 1, Y), Rect, Link_Style,
                             Routing => Curve);
            Model.Add (Link);
         end if;
         if Y > 0 then
            Link := Gtk_New (Items (X, Y - 1), Rect, Link_Style,
                             Routing => Curve);
            Model.Add (Link);
         end if;
      end loop;

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Scrolled);

      Canvas.Set_Model (Model);
      Unref (Model);
      Scrolled.Add (Canvas);

      Frame.Show_All;
   end Run;

end Create_Canvas_View_Rtrees;
