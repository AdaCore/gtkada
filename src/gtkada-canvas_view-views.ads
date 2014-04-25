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
   --  If shift is pressed, no snapping on the grid or smart guides occurs.

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

   -------------
   -- Minimap --
   -------------

   type Minimap_View_Record is new Canvas_View_Record with private;
   type Minimap_View is access all Minimap_View_Record'Class;
   --  A special canvas view that monitors another view and displays the same
   --  contents, but at a scale such that the whole model is visible (and the
   --  area visible in the monitored view is drawn as a rectangle).

   Default_Current_Region_Style : constant Gtkada.Style.Drawing_Style :=
     Gtkada.Style.Gtk_New
       (Stroke     => (0.0, 0.0, 0.0, 1.0),
        Fill       => Gtkada.Style.Create_Rgba_Pattern ((0.9, 0.9, 0.9, 0.2)),
        Line_Width => 2.0);

   procedure Gtk_New
     (Self  : out Minimap_View;
      Style : Gtkada.Style.Drawing_Style := Default_Current_Region_Style);
   procedure Initialize
     (Self  : not null access Minimap_View_Record'Class;
      Style : Gtkada.Style.Drawing_Style := Default_Current_Region_Style);
   --  Create a new minimap, which does not monitor any view yet.
   --  The style is used to highlight the region currently visible in the
   --  monitored view.

   procedure Monitor
     (Self : not null access Minimap_View_Record;
      View : access Canvas_View_Record'Class := null);
   --  Start monitoring a specific view.
   --  Any change in the viewport or the model of that view will be reflected
   --  in the display of Self.

   overriding procedure Draw_Internal
     (Self    : not null access Minimap_View_Record;
      Context : Draw_Context;
      Area    : Model_Rectangle);

   --------------
   -- Snapping --
   --------------
   --  These functions are mostly for the internal implementation of the view.

   function Snap_To_Grid
     (Self        : not null access Canvas_View_Record'Class;
      Pos         : Model_Coordinate;
      Size        : Model_Coordinate) return Model_Coordinate;
   --  Snap the Pos coordinate to the canvas grid.
   --  Size is the size of the item along that coordinate, since the item
   --  could be snap either on its left (resp. top) or right (resp. bottom)

   procedure Prepare_Smart_Guides
     (Self : not null access Canvas_View_Record'Class);
   --  Prepare data for the smart guides, before we start a drag operation.

   procedure Free_Smart_Guides
     (Self : not null access Canvas_View_Record'Class);
   --  Free the memory used for the smart guidss

   function Snap_To_Smart_Guides
     (Self       : not null access Canvas_View_Record'Class;
      Pos        : Model_Coordinate;
      Size       : Model_Coordinate;
      Horizontal : Boolean) return Model_Coordinate;
   --  Snap the Pos coordinate to the smart guides.
   --  This also computes which smart guides should be made visible

   procedure Draw_Visible_Smart_Guides
     (Self     : not null access Canvas_View_Record'Class;
      Context  : Draw_Context;
      For_Item : not null access Abstract_Item_Record'Class);
   --  Draw the visible smart guides, as computed by Snap_To_Smart_Guides;

   -------------------------
   -- Continous scrolling --
   -------------------------
   --  These functions are mostly for the internal implementation of the view.

   procedure Cancel_Continuous_Scrolling
     (Self    : not null access Canvas_View_Record'Class);

private
   type Minimap_View_Record is new Canvas_View_Record with record
      Monitored           : Canvas_View;
      Viewport_Changed_Id : Gtk.Handlers.Handler_Id;
      Area_Style          : Gtkada.Style.Drawing_Style;

      Drag_Pos_X, Drag_Pos_Y : Gdouble;
   end record;
end Gtkada.Canvas_View.Views;
