-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

--  <description>
--  This package provides an interactive canvas, on which the user can put
--  items, move them with the mouse, etc. The items can be connected together,
--  and the connections remain active while the items are moved.
--
--  It also supports scrolling if put in a Gtk_Scrolled_Window.
--
--  All items put in this canvas must inherit from the type Canvas_Item_Record.
--  However, it is your responsability, as a programmer, to provide drawing
--  routines. In fact, all these items should draw in a pixmap, which is then
--  copied automatically to the screen whenever the canvas needs to redraw
--  itself.
--
--  The items can also react to mouse events: mouse clicks are transmitted to
--  the item if the mouse did not move more than a given amount of pixels.
--  To decide what their reaction should be, you should override the
--  On_Button_Click subprogram.
--
--  This canvas is not intended for cases where you want to put hundreds of
--  items on the screen. For instance, it does not provide any smart
--  double-buffering, and thus you would get some flicker if there are too
--  many items.
--  </description>

with Gdk.Event;
with Gdk.Font;
with Gdk.GC;
with Gdk.Pixmap;
with Gdk.Window;
with Glib;
with Gtk.Drawing_Area;
with Gdk.Rectangle;
with Gtk.Adjustment;
with Gtk.Main;

package Gtkada.Canvas is

   type Interactive_Canvas_Record is new
     Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type Interactive_Canvas is access all Interactive_Canvas_Record'Class;
   --  A canvas on which items are put.
   --  Each item can be moved interactively by the user, and links can be
   --  drawn automatically from an item to another.
   --  This widget can be inserted directly in a scrolled window to provide
   --  support for scrolling.

   type Canvas_Item_Record is abstract tagged private;
   type Canvas_Item is access all Canvas_Item_Record'Class;
   --  An item that can be put on the canvas.
   --  This is an abstract type, as it does not provide any default drawing
   --  routine. You must override the abstract Draw subprogram.

   type Canvas_Link_Record is tagged private;
   type Canvas_Link is access all Canvas_Link_Record'Class;
   --  A link between two items in the canvas.
   --  The implementation provided in this package provides links that can
   --  be either straight links or curved links.
   --  This type is provided as a tagged type so that you can associated your
   --  own user data with it.

   -------------------
   -- Customization --
   -------------------

   Default_Annotation_Font   : constant String := "Helvetica";
   --  Font used when displaying link annotation.
   --  This is the name of a postscript font, as defined in Gtk.Extra.PsFont.

   Default_Annotation_Height : constant := 8;
   --  Default Height use for the annotation font.

   Default_Grid_Size         : constant := 15;
   --  Number of pixels between two dots on the grid.
   --  This is used for both horizontal and vertical orientation.

   Default_Arc_Link_Offset   : constant := 25;
   --  Distance between two parallel arcs for two links. This is not the exact
   --  distance, and it only used to compute the control points for the bezier
   --  curves.

   Default_Arrow_Angle       : constant := 30;
   --  Half angle for the arrows in degres

   Default_Arrow_Length      : constant := 6;
   --  Length of the arrows in pixels.

   Default_Motion_Threshold  : constant := 4;
   --  Mimimum motion the mouse must have before we start moving the selected
   --  item. If the mouse has moved less than that amount of pixels in any
   --  direction, then the mouse click is considered as being a selection
   --  only and is transfered to the item itself.

   ----------------
   -- Enum types --
   ----------------

   type Arrow_Type is
     (No_Arrow,
      --  the link does not have an arrow

      Start_Arrow,
      --  the link has an arrow at its beginning

      End_Arrow,
      --  the link has an arrow at the end

      Both_Arrow
      --  the link has an arrow on both sides
     );
   --  Indicate whether the links have an arrow or not.

   -----------------------
   -- Creating a canvas --
   -----------------------

   procedure Gtk_New (Canvas : out Interactive_Canvas);
   --  Create a new empty Canvas.
   --  The canvas includes a grid in its background.

   procedure Initialize (Canvas : access Interactive_Canvas_Record'Class);
   --  Internal function used to initialize the canvas.

   procedure Configure
     (Canvas : access Interactive_Canvas_Record;
      Grid_Size         : Glib.Guint := Default_Grid_Size;
      Annotation_Font   : String := Default_Annotation_Font;
      Annotation_Height : Glib.Gint := Default_Annotation_Height;
      Arc_Link_Offset   : Glib.Gint := Default_Arc_Link_Offset;
      Arrow_Angle       : Glib.Gint := Default_Arrow_Angle;
      Arrow_Length      : Glib.Gint := Default_Arrow_Length;
      Motion_Threshold  : Glib.Gint := Default_Motion_Threshold);
   --  Change the parameters for the canvas.
   --  A Grid_Size of 0 means than no grid should be drawn in the background of
   --  canvas. Note that in that case you can never activate Align_On_Grid.

   procedure Align_On_Grid
     (Canvas : access Interactive_Canvas_Record;
      Align  : Boolean := True);
   --  Choose whether the items should be aligned on the grid when moved.
   --  Existing items are not moved even if you set this parameter to True,
   --  this will only take effect the next time the items are moved.

   function Get_Align_On_Grid
     (Canvas : access Interactive_Canvas_Record) return Boolean;
   --  Return True if items are currently aligned on grid.

   procedure Move_To
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Glib.Gint := Glib.Gint'First);
   --  Move the item in the canvas.
   --  Item is assumed to be already in the canvas.
   --  If at least one of X or Y has the default value, then the item
   --  is placed automatically in a free area of the canvas.
   --  Its new position depends on whether it has links to other existing
   --  items (in which case it is placed to the right of it), or not (in which
   --  case it is placed at the bottom of the leftmost column).

   procedure Put
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Glib.Gint := Glib.Gint'First);
   --  Add a new item to the canvas.
   --  The item is added at a specific location.
   --  If at least one of X or Y has the default value, then the item
   --  is placed automatically in a free area of the canvas.
   --  Its new position depends on whether it has links to other existing
   --  items (in which case it is placed to the right of it), or not (in which
   --  case it is placed at the bottom of the leftmost column).
   --  Note also that the current size of the item is used to compute the new
   --  location, so it should depend on the current zoom.

   procedure Remove
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class);
   --  Remove an item and all the links to and from it from the canvas.
   --  The item itself is not freed, but the links are.
   --  Nothing is done if the item is not part of the canvas.

   procedure Item_Updated
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class);
   --  This should be called when Item has changed the contents of its
   --  pixmap, and thus the Canvas should be updated.

   procedure Refresh_Canvas (Canvas : access Interactive_Canvas_Record);
   --  Redraw the whole canvas (both in the double buffer and on the screen).

   type Item_Processor is access function
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class) return Boolean;

   procedure For_Each_Item
     (Canvas  : access Interactive_Canvas_Record;
      Execute : Item_Processor);
   --  Execute an action on each of the items contained in the canvas.
   --  If Execute returns False, we stop traversing the list of children.
   --  It is safe to remove the items in Item_Processor.

   function Has_Link
     (Canvas   : access Interactive_Canvas_Record;
      From, To : access Canvas_Item_Record'Class;
      Name     : String := "") return Boolean;
   --  Test whether there is a link from From to To, with the same name.
   --  If Name is the empty string "", then no check is done on the name,
   --  and True if returned if there is any link between the two items.

   procedure Raise_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class);
   --  Raise the item so that it is displayed on top of all the others
   --  The canvas is refreshed as needed to reflect the change.
   --  Nothing happens if Item is not part of the canvas.

   procedure Lower_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class);
   --  Lower the item so that it is displayed below all the others.
   --  The canvas is refreshed as needed to reflect the change.
   --  Nothing happens if Item is not part of the canvas.

   function Is_On_Top
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) return Boolean;
   --  Return True if Item is displayed on top of all the others in the canvas.

   procedure Show_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class);
   --  Scroll the canvas so that Item is visible.

   procedure Zoom
     (Canvas : access Interactive_Canvas_Record;
      Percent : Glib.Guint := 100;
      Steps   : Glib.Guint := 1);
   --  Zoom in or out in the canvas. All items are resized, and their contents
   --  is then random.
   --  You must either redraw them systematically afterwards, or connect to the
   --  "zoomed" signal which is emitted automatically by this subprogram.
   --
   --  Steps is the number of successive zooms that will be done to provide
   --  smooth scrolling.
   --
   --  Note that one possible use for this function is to refresh the canvas
   --  and emit the "zoomed" signal, which might redraw all the items. This can
   --  be accomplished by keeping the default 100 value for Percent.

   function Get_Zoom
     (Canvas : access Interactive_Canvas_Record) return Glib.Guint;
   --  Return the current zoom level

   function To_Canvas_Coordinates
     (Canvas : access Interactive_Canvas_Record'Class;
      X      : Glib.Gint) return Glib.Gint;
   --  Scale the scalar X depending by the zoom level (map from world
   --  coordinates to canvas coordinates)

   function To_World_Coordinates
     (Canvas : access Interactive_Canvas_Record'Class;
      X      : Glib.Gint) return Glib.Gint;
   --  Scale the scalar X depending by the zoom level (map from canvas
   --  coordinates to world coordinates)

   -----------
   -- Links --
   -----------

   procedure Configure
     (Link   : access Canvas_Link_Record;
      Src    : access Canvas_Item_Record'Class;
      Dest   : access Canvas_Item_Record'Class;
      Arrow  : in Arrow_Type := End_Arrow;
      Descr  : in String := "");
   --  Configure a link.
   --  The link is an oriented bound between two items on the canvas.
   --  If Descr is not the empty string, it will be displayed in the middle
   --  of the link, and should indicate what the link means.
   --  Arrow indicates whether some arrows should be printed as well.

   function Get_Src (Link : access Canvas_Link_Record) return Canvas_Item;
   --  Return the source item for the link

   function Get_Dest (Link : access Canvas_Link_Record) return Canvas_Item;
   --  Return the destination item for the link

   function Get_Descr (Link : access Canvas_Link_Record) return String;
   --  Return the description for the link, or "" if there is none

   procedure Set_Src_Pos
     (Link : access Canvas_Link_Record; X_Pos, Y_Pos : Glib.Gfloat := 0.5);
   --  Set the position of the link's attachment in its source item.
   --  X_Pos and Y_Pos should be given between 0 and 100 (from left to right
   --  or top to bottom).

   procedure Set_Dest_Pos
     (Link : access Canvas_Link_Record; X_Pos, Y_Pos : Glib.Gfloat := 0.5);
   --  Same as Set_Src_Pos for the destination item

   procedure Add_Link
     (Canvas : access Interactive_Canvas_Record;
      Link   : access Canvas_Link_Record'Class);
   --  Add a link to the canvas. The link must have been created first.

   procedure Add_Link
     (Canvas : access Interactive_Canvas_Record;
      Src    : access Canvas_Item_Record'Class;
      Dest   : access Canvas_Item_Record'Class;
      Arrow  : in Arrow_Type := End_Arrow;
      Descr  : in String := "");
   --  Create and add a link between two items.
   --  Simpler procedure to add a standard link.
   --  This takes care of memory allocation, as well as adding the link to
   --  the canvas.

   procedure Remove_Link
     (Canvas : access Interactive_Canvas_Record;
      Link   : access Canvas_Link_Record'Class);
   --  Remove a link from the canvas.
   --  It also destroys the link itself, and free the memory allocated to it.
   --  Nothing is done if Link does not belong to canvas.

   type Link_Processor is access function
     (Canvas : access Interactive_Canvas_Record'Class;
      Link   : access Canvas_Link_Record'Class) return Boolean;

   procedure For_Each_Link
     (Canvas  : access Interactive_Canvas_Record;
      Execute : Link_Processor);
   --  Execute an action on each of the links contained in the canvas.
   --  If Execute returns False, we stop traversing the list of links.
   --  It is safe to remove the link from the list in Link_Processor.

   procedure Destroy (Link : access Canvas_Link_Record);
   --  Method called every time a link is destroyed. You should override this
   --  if you define your own link types.
   --  Note that Link should first be removed from the canvas, but this is
   --  your responsability to do so.
   --  This shouldn't free the link itself, only its fields.

   ---------------
   -- Selection --
   ---------------

   procedure Clear_Selection (Canvas : access Interactive_Canvas_Record);
   --  Clear the list of currently selected items.

   procedure Add_To_Selection
     (Canvas : access Interactive_Canvas_Record;
      Item : access Canvas_Item_Record'Class);
   --  Add Item to the selection.  This is only meaningful during a drag
   --  operation (ie during a button press and the matching button
   --  release). Item will be moved at the same time that the selection is
   --  moved.
   --  Item is not added again if it is already in the selection.
   --  This function can be called from the Button_Click subprogram to force
   --  moving items.

   procedure Remove_From_Selection
     (Canvas : access Interactive_Canvas_Record;
      Item : access Canvas_Item_Record'Class);
   --  Remove Item from the selection.

   ------------------------
   -- Items manipulation --
   ------------------------

   procedure Set_Screen_Size
     (Item   : access Canvas_Item_Record;
      Width  : Glib.Gint;
      Height : Glib.Gint);
   --  Set the size that the items occupies on the screen. You must call this
   --  subprogram every time the zoom level changes, since Width and Height
   --  must reflect the current size of the item in the current zoom level,
   --  not an absolute size that is automatically scaled.

   procedure Draw
     (Item   : access Canvas_Item_Record;
      Canvas : access Interactive_Canvas_Record'Class;
      Dest   : Gdk.Pixmap.Gdk_Pixmap;
      Xdest, Ydest : Glib.Gint) is abstract;
   --  This subprogram, that must be overridden, should draw the item on
   --  the pixmap Dest, at the specific location (At_X, At_Y).

   procedure Destroy (Item : access Canvas_Item_Record);
   --  Free the memory occupied by the item (not the item itself). You should
   --  override this function if you define your own widget type.

   procedure On_Button_Click
     (Item   : access Canvas_Item_Record;
      Event  : Gdk.Event.Gdk_Event_Button);
   --  Function called whenever the item was clicked on.
   --  Note that this function is not called when the item is moved, and thus
   --  is only called when the click was short.
   --  If it returns True, the canvas it redrawn afterwards (in case the item
   --  has changed for instance).
   --  This procedure is called is always called, except when clicking with the
   --  first button on an item. It is called otherwise for button releases and
   --  double-clicks.
   --  The coordinates (X, Y) in the Event are relative to the top-left corner
   --  of Item.

   function Get_Coord
     (Item : access Canvas_Item_Record) return Gdk.Rectangle.Gdk_Rectangle;
   --  Return the coordinates and size of the item.

   procedure Set_Visibility
     (Item    : access Canvas_Item_Record;
      Visible : Boolean);
   --  Set the visibility status of the item.
   --  The canvas is not refreshed (this is your responsibility to do it after
   --  you have finished doing all the modifications).

   function Is_Visible (Item : access Canvas_Item_Record) return Boolean;
   --  Return True if the item is currently visible.

   --------------------
   -- Buffered items --
   --------------------

   type Buffered_Item_Record is new Canvas_Item_Record with private;
   type Buffered_Item is access all Buffered_Item_Record'Class;
   --  A widget that has a double-buffer associated. You should use this one
   --  when drawing items can take a long time. You need to update the contents
   --  of the pixmap when the item is created and every time the canvas is
   --  zoomed (connect to the "zoomed" signal).

   procedure Draw
     (Item : access Buffered_Item_Record;
      Canvas : access Interactive_Canvas_Record'Class;
      Dest : Gdk.Pixmap.Gdk_Pixmap;
      Xdest, Ydest : Glib.Gint);
   --  Draw the item's double-buffer onto Dest.

   procedure Destroy (Item : access Buffered_Item_Record);
   --  Free the double-buffer allocated for the item

   procedure Set_Screen_Size_And_Pixmap
     (Item   : access Buffered_Item_Record;
      Win    : Gdk.Window.Gdk_Window;
      Width, Height  : Glib.Gint);
   --  Sets the size used on the screen by item, and reallocate the pixmap
   --  if needed

   function Pixmap (Item : access Buffered_Item_Record)
      return Gdk.Pixmap.Gdk_Pixmap;
   --  Return the double-buffer that must be updated every time the canvas
   --  is scrolled.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "background_click"
   --  procedure Handler (Canvas : access Interactive_Canvas_Record'Class;
   --                     Event  : Gdk.Event.Gdk_Event);
   --
   --  Called every time the user clicks in the background (ie not on an item,
   --  or On_Button_Click would be called).
   --  This is called both on Button_Release and Button_Press events.
   --  The coordinates (X, Y) in the Event are relative to the top-left corner
   --  of Canvas.
   --
   --  - "item_selected"
   --  procedure Handler (Canvas : access Interactive_Canvas_Record'Class;
   --                     Item   : Canvas_Item);
   --
   --  Called when the user has clicked on an item to select it, ie before any
   --  drag even has occured. This is a good time to add other items to the
   --  selection if you need.
   --
   --  - "zoomed"
   --  procedure Handler (Canvas : access Interactive_Canvas_Record'Class);
   --
   --  Emitted when the canvas has been zoomed in or out. You must resize the
   --  items as needed, and possibily refresh their internal double-pixmap.
   --  However, you do not need to redraw them on the screen, this will be
   --  handled by separate calls to Draw.
   --
   --  </signals>

private

   type String_Access is access String;

   type Canvas_Link_List_Record;
   type Canvas_Link_List is access Canvas_Link_List_Record;
   type Canvas_Link_List_Record is record
      Link : Canvas_Link;
      Next : Canvas_Link_List;
   end record;

   type Canvas_Link_Record is tagged record
      Src    : Canvas_Item;
      Dest   : Canvas_Item;
      Descr  : String_Access;

      Src_X_Pos  : Glib.Gfloat := 0.5;
      Src_Y_Pos  : Glib.Gfloat := 0.5;
      Dest_X_Pos : Glib.Gfloat := 0.5;
      Dest_Y_Pos : Glib.Gfloat := 0.5;
      --  Position of the link's attachment in each of the src and dest items.

      Arrow  : Arrow_Type;

      Sibling : Canvas_Link;
      --  Pointer to the next link that connects the same two items.
   end record;

   type Canvas_Item_List_Record;
   type Canvas_Item_List is access Canvas_Item_List_Record;
   type Canvas_Item_List_Record is record
      Item : Canvas_Item;
      Next : Canvas_Item_List;
   end record;

   type Item_Selection_List_Record;
   type Item_Selection_List is access Item_Selection_List_Record;
   type Item_Selection_List_Record is record
      Item : Canvas_Item;
      X, Y : Glib.Gint;
      Next : Item_Selection_List;
   end record;
   --  A list of items, but this also memorizes the position of the items (used
   --  for selected widgets while they are being moved, so that we do not
   --  change the coordinates of the item itself until the mouse is released
   --  (and thus provide correct scrolling).

   type Interactive_Canvas_Record is new
      Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
   record
      Links             : Canvas_Link_List  := null;
      Children          : Canvas_Item_List := null;

      Selection         : Item_Selection_List := null;
      --  List of currently selected items that will be moved when the mouse
      --  is dragged

      Last_X_Event      : Glib.Gint;
      Last_Y_Event      : Glib.Gint;
      --  Last position where the mouse was pressed or moved in the canvas.

      Mouse_Has_Moved   : Boolean;
      --  True if mouse has moved while the button was clicked. This is used
      --  to distinguish between item motion and item selection.

      Grid_Size         : Glib.Guint := Default_Grid_Size;
      --  The current number of pixels between each dot of the grid. If this
      --  is strictly below 2, the grid is not drawn. (0) means that the
      --  user has deactivated the grid completely.

      Annotation_Font   : String_Access;
      Annotation_Height : Glib.Gint := Default_Annotation_Height;
      Arc_Link_Offset   : Glib.Gint := Default_Arc_Link_Offset;
      Arrow_Angle       : Float;
      Arrow_Length      : Glib.Gint := Default_Arrow_Length;
      Motion_Threshold  : Glib.Gint := Default_Motion_Threshold;
      Align_On_Grid     : Boolean := False;

      --  The following variables are initialized as soon as a Gdk_Window
      --  has been created for the canvas, in the Realized subprograms.

      Clear_GC : Gdk.GC.Gdk_GC := Gdk.GC.Null_GC;
      Black_GC : Gdk.GC.Gdk_GC := Gdk.GC.Null_GC;
      Anim_GC  : Gdk.GC.Gdk_GC := Gdk.GC.Null_GC;
      Font     : Gdk.Font.Gdk_Font := Gdk.Font.Null_Font;

      Double_Buffer : Gdk.Pixmap.Gdk_Pixmap;

      Hadj, Vadj : Gtk.Adjustment.Gtk_Adjustment;
      Scrolling_Timeout_Id : Gtk.Main.Timeout_Handler_Id := 0;
      Dashed_Line_Visible : Boolean := False;

      Zoom : Glib.Guint := 100;
      --  Zoom level in percent (100% is normal size)

      Target_Zoom : Glib.Guint := 100;
      Zoom_Step : Glib.Gint;
      --  Variables used while smooth-scrolling the canvas
   end record;

   type Canvas_Item_Record is abstract tagged record
      Coord   : Gdk.Rectangle.Gdk_Rectangle;
      Visible : Boolean := True;
   end record;

   type Buffered_Item_Record is new Canvas_Item_Record with record
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
   end record;

   pragma Inline (To_Canvas_Coordinates);
   pragma Inline (To_World_Coordinates);
end Gtkada.Canvas;
