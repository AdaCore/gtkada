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

with Cairo;                    use Cairo;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Gdk.Types;                use Gdk.Types;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Paned;                use Gtk.Paned;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Canvas_View;       use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Gtkada.Style;             use Gtkada.Style;

package body Create_Canvas_View_Minimap is

   function On_Item_Event_Zoom is new On_Item_Event_Zoom_Generic
     (Modifier => Mod1_Mask);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      S : constant Gdouble := 0.86602540378443864676;  --  sqrt(3) / 2
      Canvas        : Canvas_View;
      Model         : List_Canvas_Model;
      Scrolled      : Gtk_Scrolled_Window;
      Black         : Drawing_Style;
      Paned         : Gtk_Paned;
      Minimap       : Minimap_View;
      Minimap_Frame : Gtk_Frame;

      L1, L2, L3    : Canvas_Link;
      It1, It3      : Rect_Item;
      It2           : Polyline_Item;

      L             : Gdouble;
   begin
      Gtk_New (Model);

      Canvas := new Canvas_View_Record;
      Gtkada.Canvas_View.Initialize (Canvas);
      Canvas.Set_Grid_Size (30.0);
      Canvas.Set_Snap (Snap_To_Grid   => True,
                       Snap_To_Guides => True);

      --  Order is irrelevant here.
      Canvas.On_Item_Event (On_Item_Event_Select'Access);
      Canvas.On_Item_Event (On_Item_Event_Move_Item'Access);
      Canvas.On_Item_Event (On_Item_Event_Scroll_Background'Access);
      Canvas.On_Item_Event (On_Item_Event_Zoom'Access);

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

      It3 := Gtk_New_Rect (Black, 50.0, 80.0);
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

      --  Create the view once the model is populated, to avoid a refresh
      --  every time a new item is added.

      Gtk_New (Paned, Orientation_Vertical);
      Paned.Set_Position (100);
      Frame.Add (Paned);

      Gtk_New (Minimap_Frame);
      Paned.Add1 (Minimap_Frame);

      Gtk_New (Minimap);
      Minimap.Monitor (Canvas);
      Minimap_Frame.Add (Minimap);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Always, Policy_Always);
      Paned.Add2 (Scrolled);

      Canvas.Set_Model (Model);
      Unref (Model);
      Scrolled.Add (Canvas);

      Frame.Show_All;
   end Run;

end Create_Canvas_View_Minimap;
