------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2014, AdaCore                     --
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

--  Various support utilities for the grid and smart guides in the canvas

with Glib.Object;

package Gtkada.Canvas_View.Views is

   ----------------------------
   -- Drawing the background --
   ----------------------------
   --  Various subprograms that draw the background of a view.
   --  By default, a view only displays a white background, but you can
   --  override the Draw_Internal primitive and call one of the following
   --  subprograms if you want to draw alternate backgrounds.
   --
   --  You could also use an image as the background, by creating a
   --  cairo pattern:
   --     Surf    : Cairo_Surface := Cairo.Png.Create_From_Png ("file.png");
   --     Pattern : Cairo_Pattern := Cairo.Pattern.Create_For_Surface (Surf);
   --     Cairo.Pattern.Set_Extend (Pattern, Cairo_Extend_Repeat);
   --     Destroy (Surf);
   --  and then drawing that pattern.
   --     Set_Source (Context.Cr, Pattern);
   --     Paint (Context.Cr);
   --  With that code, the image will be scrolled when the canvas is scrolled.
   --  If you do not want to scroll it, you need to set the identity matrix as
   --  the transformation matrix.
   --
   --  Using a custom background color can be done with:
   --     Set_Source_Rgb (Context.Cr, Red, Green, Blue);
   --     Paint (Context.Cr);

   procedure Draw_Grid_Lines
     (Self    : not null access Canvas_View_Record'Class;
      Style   : Gtkada.Style.Drawing_Style;
      Context : Draw_Context;
      Area    : Model_Rectangle);
   --  Draw a grid with lines in the background.
   --  The size of the grid can be set with Gtkada.Canvas_View.Set_Grid_Size

   procedure Draw_Grid_Dots
     (Self    : not null access Canvas_View_Record'Class;
      Style   : Gtkada.Style.Drawing_Style;
      Context : Draw_Context;
      Area    : Model_Rectangle);
   --  Draw a grid with dots in the background

   ---------------
   -- Callbacks --
   ---------------
   --  These procedures contain a number of example callbacks for "item_event"
   --  which enable various behaviors. Depending on your application, one of
   --  these might be useful as is, or a starting point for your own callback

   function On_Item_Event_Move_Item
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  Add this to the list of callbacks for "item_event" to enable dragging
   --  items with the mouse.

   function On_Item_Event_Scroll_Background
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  Add this to the list of callbacks for "item_event" to enable scrolling
   --  the canvas by dragging the background. Scrolling is limited to the area
   --  that actually contains items.

   generic
      Modifier : Gdk.Types.Gdk_Modifier_Type := Mod1_Mask;
      Factor   : Gdouble := 1.1;
   function On_Item_Event_Zoom_Generic
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  Add this to the list of callbacks for "item_event" to enable zooming in
   --  or out with the mouse wheel and a keyboard modifier like ctrl, alt,...
   --  (since the mouse wheel on its own is used for vertical scrolling by
   --  gtk+, and for horizontal scrolling when used with shift).

   --------------
   -- Snapping --
   --------------

   function Do_Snap_Grid
     (Self        : not null access Canvas_View_Record'Class;
      Snap_Margin : Model_Coordinate;
      Pos         : Model_Coordinate;
      Size        : Model_Coordinate) return Model_Coordinate;
   --  Snap the Pos coordinate to the canvas grid.
   --  Size is the size of the item along that coordinate, since the item
   --  could be snap either on its left (resp. top) or right (resp. bottom)

end Gtkada.Canvas_View.Views;
