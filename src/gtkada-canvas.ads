-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
with Gdk.Pixbuf;
with Gdk.Pixmap;
with Gdk.Window;
with Glib;
with Glib.Graphs;
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

   type Canvas_Item_Record is abstract new Glib.Graphs.Vertex with private;
   type Canvas_Item is access all Canvas_Item_Record'Class;
   --  An item that can be put on the canvas.
   --  This is an abstract type, as it does not provide any default drawing
   --  routine. You must override the abstract Draw subprogram.

   type Canvas_Link_Record is new Glib.Graphs.Edge with private;
   type Canvas_Link is access all Canvas_Link_Record'Class;
   type Canvas_Link_Access is access all Canvas_Link_Record;
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

   procedure Gtk_New
     (Canvas : out Interactive_Canvas; Auto_Layout : Boolean := True);
   --  Create a new empty Canvas.
   --  The canvas includes a grid in its background.
   --  If Auto_Layout is True, then the items are automatically positioned as
   --  they are put in the canvas, if no coordinates are specified.

   procedure Initialize
     (Canvas : access Interactive_Canvas_Record'Class;
      Auto_Layout : Boolean := True);
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

   procedure Set_Orthogonal_Links
     (Canvas : access Interactive_Canvas_Record;
      Orthogonal : Boolean);
   --  If Orthogonal is True, then all the links will be drawn only with
   --  vertical and horizontal lines. This is not applied for the second or
   --  more link between two items.

   function Get_Orthogonal_Links
     (Canvas : access Interactive_Canvas_Record) return Boolean;
   --  Return True if the links are only drawn horizontally and vertically.

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
   --  If you leave both coordinates X and Y to their default value, then the
   --  item's location will be automatically computed when you layout the
   --  canvas (it is your responsability to call Layout).
   --  Note that (X, Y) is the coordinates for a zoom level 100%. Conversion to
   --  other zoom levels is fully automatic

   procedure Set_Items
     (Canvas : access Interactive_Canvas_Record;
      Items  : Glib.Graphs.Graph);
   --  Set the items and links to display in the canvas from Items.
   --  All items previously in the canvas are removed, and replaced by the
   --  vertices in Items.
   --  Note that the vertices in Items must be in Canvas_Item_Record'Class, and
   --  the links must be in Canvas_Link_Record'Class.
   --  If you do not have an automatic layout set up in Canvas, you need to set
   --  the coordinates of all the vertices by calling Move_To separately.
   --
   --  You mustn't destroy items yourself, this is done automatically when the
   --  canvas is destroyed.

   procedure Put
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Glib.Gint := Glib.Gint'First);
   --  Add a new item to the canvas.
   --  The item is added at a specific location.
   --  If you leave both X and Y to their default value, the item's location
   --  will be computed automatically when you call Layout on the canvas,
   --  unless Auto_Layout has been set, in which case the position will be
   --  computed immediately.
   --  Note that (X, Y) is the coordinates for a zoom level 100%. Conversion to
   --  other zoom levels is fully automatic

   function Item_At_Coordinates
     (Canvas : access Interactive_Canvas_Record;
      X, Y : Glib.Gint) return Canvas_Item;
   --  Return the item on top, at coordinates (X, Y).
   --  null is returned if there is no such item.

   function Item_At_Coordinates
     (Canvas : access Interactive_Canvas_Record; Event : Gdk.Event.Gdk_Event)
      return Canvas_Item;
   --  Same as above, but using the coordinates of the event, taking into
   --  account the current zoom level and current scrolling

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
   --
   --  ??? Should we remove, and replace with the standard iterators from
   --  ??? Graphs

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
   --  Zoom in or out in the canvas.
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
   --  coordinates to canvas coordinates).
   --
   --  ??? Need an extra parameter to convert coordinates instead of distances.

   function To_World_Coordinates
     (Canvas : access Interactive_Canvas_Record'Class;
      X      : Glib.Gint) return Glib.Gint;
   --  Scale the scalar X depending by the zoom level (map from canvas
   --  coordinates to world coordinates)

   ---------------------
   -- Layout of items --
   ---------------------

   type Layout_Algorithm is access procedure
     (Canvas : access Interactive_Canvas_Record'Class;
      Graph : Glib.Graphs.Graph;
      Force : Boolean;
      Vertical_Layout : Boolean);
   --  A general layout algorithm. It should compute the position of all the
   --  vertices of the graph, and set them directly in the graph itself.
   --  NoteL all the vertices in the graph are of time Canvad_Item_Record'Class
   --  and you should use that to set the coordinates through a call to
   --  Move_To.
   --  if Force is False, then only the item at location (Gint'First,
   --  Gint'First) should be moved.
   --
   --  This function doesn't need to align items, this is done automatically by
   --  the canvas if necessary.

   procedure Set_Layout_Algorithm
     (Canvas    : access Interactive_Canvas_Record;
      Algorithm : Layout_Algorithm);
   --  Set the layout algorithm to use to compute the position of the items.
   --  Algorithm mustn't be null.

   procedure Default_Layout_Algorithm
     (Canvas : access Interactive_Canvas_Record'Class;
      Graph : Glib.Graphs.Graph;
      Force : Boolean;
      Vertical_Layout : Boolean);
   --  The default algorithm used in the canvas.
   --  Basically, items are put next to each other, unless there is a link
   --  between two items. In that case, the second item is put below the first,
   --  as space allows.

   procedure Set_Auto_Layout
     (Canvas : access Interactive_Canvas_Record;
      Auto_Layout : Boolean);
   --  If Auto_Layout is true, then the items will be automatically layed out
   --  when inserted in the canvas, if no coordinates are specified.

   procedure Layout
     (Canvas : access Interactive_Canvas_Record;
      Force  : Boolean := False;
      Vertical_Layout : Boolean := False);
   --  Recompute the layout of the canvas.
   --  If Force is False, only the items that don't already have a location are
   --  moved.

   -----------
   -- Links --
   -----------

   procedure Configure
     (Link   : access Canvas_Link_Record;
      Arrow  : in Arrow_Type := End_Arrow;
      Descr  : in String := "");
   --  Configure a link.
   --  The link is an oriented bound between two items on the canvas.
   --  If Descr is not the empty string, it will be displayed in the middle
   --  of the link, and should indicate what the link means.
   --  Arrow indicates whether some arrows should be printed as well.

   function Get_Descr (Link : access Canvas_Link_Record) return String;
   --  Return the description for the link, or "" if there is none

   function Get_Arrow_Type
     (Link : access Canvas_Link_Record) return Arrow_Type;
   --  Return the location of the arrows on Link

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
      Link   : access Canvas_Link_Record'Class;
      Src    : access Canvas_Item_Record'Class;
      Dest   : access Canvas_Item_Record'Class;
      Arrow  : in Arrow_Type := End_Arrow;
      Descr  : in String := "");
   --  Add Link in the canvas. This connects the two items Src and Dest.
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
      Execute : Link_Processor;
      From, To : Canvas_Item := null);
   --  Execute an action on each of the links contained in the canvas.
   --  If Execute returns False, we stop traversing the list of links.
   --  It is safe to remove the link from the list in Link_Processor.
   --
   --  (From, To) can be used to limit what links are looked for.
   --
   --  ??? Would be nicer to give direct access to the Graph iterators

   procedure Update_Links
     (Canvas : access Interactive_Canvas_Record'Class;
      Win    : Gdk.Window.Gdk_Window;
      From_Item : Canvas_Item := null);
   --  Draw all the links to and from From_Item (or all the links in the canvas
   --  if From_Item is null).

   procedure Draw_Link
     (Canvas      : access Interactive_Canvas_Record'Class;
      Link        : access Canvas_Link_Record;
      Window      : Gdk.Window.Gdk_Window;
      Invert_Mode : Boolean;
      GC          : Gdk.GC.Gdk_GC;
      Edge_Number : Glib.Gint);
   --  Redraw the link on the canvas.
   --  Note that this is a primitive procedure of Link, not of Canvas, and thus
   --  can easily be overrided for specific links. The default version draws
   --  either straight or arc links (the latter when there are multiple links
   --  between two given items).
   --  This function shouldn't be called if one of the two ends of the link is
   --  invisible.
   --
   --  Window is the window in which the links should be drawn (they are
   --  sometimes drawn into the double-buffer, and sometimes directly on the
   --  screen, while items are moved).
   --
   --  GC is a possible graphic context that could be used to draw the
   --  link. You shouldn't destroy it or modify its attributes. However, you
   --  can use any other graphic context specific to your application, for
   --  instance if you want to draw the link in various colors or shapes. The
   --  graphic context you use must be in Invert mode (see Gdk.GC.Set_Function)
   --  if and only if Invert_Mode is true, so that when items are moved on the
   --  canvas, the links properly follow the items they are attached to.
   --
   --  Edge_Number indicates the index of link in the list of links that join
   --  the same source to the same destination. It should be used so that two
   --  links do not overlap (for instance, the default is to draw the first
   --  link straight, and the others as arcs).

   procedure Destroy (Link : in out Canvas_Link_Record);
   --  Method called every time a link is destroyed. You should override this
   --  if you define your own link types.
   --  Note that the link might already have been removed from the canvas
   --  when this subprogram is called.
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
   --  Set the size of the item in world coordinates (ie for a zoom level
   --  100%).

   procedure Draw
     (Item   : access Canvas_Item_Record;
      Canvas : access Interactive_Canvas_Record'Class;
      Dest   : Gdk.Pixmap.Gdk_Pixmap;
      Xdest, Ydest : Glib.Gint) is abstract;
   --  This subprogram, that must be overridden, should draw the item on
   --  the pixmap Dest, at the specific location (Xdest, Ydest). The item must
   --  also be drawn at the appropriate zoom level.

   procedure Destroy (Item : in out Canvas_Item_Record);
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
   --  This procedure is never called for events that are used to move the
   --  item in the canvas.
   --  The coordinates (X, Y) in the Event are relative to the top-left corner
   --  of Item.

   function Get_Coord
     (Item : access Canvas_Item_Record) return Gdk.Rectangle.Gdk_Rectangle;
   --  Return the coordinates and size of the item, in world coordinates (ie
   --  for a zoom level 100%).

   procedure Set_Visibility
     (Item    : access Canvas_Item_Record;
      Visible : Boolean);
   --  Set the visibility status of the item.
   --  The canvas is not refreshed (this is your responsibility to do it after
   --  you have finished doing all the modifications).

   function Is_Visible (Item : access Canvas_Item_Record) return Boolean;
   --  Return True if the item is currently visible.

   function Is_From_Auto_Layout
     (Item : access Canvas_Item_Record) return Boolean;
   --  Return True if the current location of the item is the result from the
   --  auto layout algorithm.
   --  False is returned if the item was moved manually by the user.

   --------------------
   -- Buffered items --
   --------------------

   type Buffered_Item_Record is new Canvas_Item_Record with private;
   type Buffered_Item is access all Buffered_Item_Record'Class;
   --  A widget that has a double-buffer associated. You should use this one
   --  when drawing items can take a long time, or you do not want to handle
   --  the zoom yourself.
   --  You only need to update the contents of the double pixmap when the
   --  contents of the item changes, since all the drawing and zooming is
   --  taken care of automatically.

   procedure Draw
     (Item : access Buffered_Item_Record;
      Canvas : access Interactive_Canvas_Record'Class;
      Dest : Gdk.Pixmap.Gdk_Pixmap;
      Xdest, Ydest : Glib.Gint);
   --  Draw the item's double-buffer onto Dest.

   procedure Destroy (Item : in out Buffered_Item_Record);
   --  Free the double-buffer allocated for the item

   procedure Set_Screen_Size_And_Pixmap
     (Item   : access Buffered_Item_Record;
      Win    : Gdk.Window.Gdk_Window;
      Width, Height  : Glib.Gint);
   --  Changes the size of the item. Width and Height should be world sizes,
   --  for a zoom level 100%, and should not depend on the current zoom level.

   function Pixmap (Item : access Buffered_Item_Record)
      return Gdk.Pixmap.Gdk_Pixmap;
   --  Return the double-buffer

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
   --  Emitted when the canvas has been zoomed in or out. You do not need to
   --  redraw the items yourself, since this will be handled by calls to Draw
   --
   --  - "draw_links"
   --  procedure Handler (Canvas : access Interactive_Canvas_Record'Class;
   --                     window : Gdk.Window.Gdk_Window);
   --
   --  Emitted when the links must be drawn in Window, before redrawing all the
   --  items.
   --
   --  </signals>

private

   type String_Access is access String;

   type Canvas_Link_Record is new Glib.Graphs.Edge with record
      Descr  : String_Access;
      Arrow  : Arrow_Type := End_Arrow;

      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf := Gdk.Pixbuf.Null_Pixbuf;
      --  The pixmap in which the text is displayed. This is required to
      --  properly implement zooming through pixmaps. The text is drawn at zoom
      --  level 100%.

      Src_X_Pos  : Glib.Gfloat := 0.5;
      Src_Y_Pos  : Glib.Gfloat := 0.5;
      Dest_X_Pos : Glib.Gfloat := 0.5;
      Dest_Y_Pos : Glib.Gfloat := 0.5;
      --  Position of the link's attachment in each of the src and dest items.
   end record;

   type Item_Selection_List_Record;
   type Item_Selection_List is access all Item_Selection_List_Record;
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
     Gtk.Drawing_Area.Gtk_Drawing_Area_Record
   with record
      Children          : Glib.Graphs.Graph;

      Layout            : Layout_Algorithm := Default_Layout_Algorithm'Access;
      Auto_Layout       : Boolean := True;
      --  The algorithm to use when laying out items on the canvas.

      Selection         : Item_Selection_List := null;
      --  List of currently selected items that will be moved when the mouse
      --  is dragged

      Last_X_Event      : Glib.Gint;
      Last_Y_Event      : Glib.Gint;
      --  Last position where the mouse was pressed or moved in the canvas.

      Mouse_Has_Moved   : Boolean;
      --  True if mouse has moved while the button was clicked. This is used
      --  to distinguish between item motion and item selection.

      Event_Press       : Gdk.Event.Gdk_Event;
      --  Save the event that was sent when the item was clicked on. This
      --  event will be sent to the application if the item was not moved.

      Grid_Size         : Glib.Guint := Default_Grid_Size;
      --  The current number of pixels between each dot of the grid. If this
      --  is strictly below 2, the grid is not drawn.

      Arc_Link_Offset   : Glib.Gint := Default_Arc_Link_Offset;
      Arrow_Angle       : Float;
      Arrow_Length      : Glib.Gint := Default_Arrow_Length;
      Motion_Threshold  : Glib.Gint := Default_Motion_Threshold;
      Align_On_Grid     : Boolean := False;

      --  The following variables are initialized as soon as a Gdk_Window
      --  has been created for the canvas, in the Realized subprograms.

      Clear_GC        : Gdk.GC.Gdk_GC := Gdk.GC.Null_GC;
      Black_GC        : Gdk.GC.Gdk_GC := Gdk.GC.Null_GC;
      Anim_GC         : Gdk.GC.Gdk_GC := Gdk.GC.Null_GC;
      Annotation_Font : Gdk.Font.Gdk_Font := Gdk.Font.Null_Font;

      Hadj, Vadj : Gtk.Adjustment.Gtk_Adjustment;
      Scrolling_Timeout_Id : Gtk.Main.Timeout_Handler_Id := 0;
      Dashed_Line_Visible : Boolean := False;

      Orthogonal_Links : Boolean := False;
      --  True if the links should be orthogonal

      Zoom : Glib.Guint := 100;
      --  Zoom level in percent (100% is normal size)

      Target_Zoom : Glib.Guint := 100;
      Zoom_Step : Glib.Gint;
      --  Variables used while smooth-scrolling the canvas
   end record;

   type Canvas_Item_Record is abstract new Glib.Graphs.Vertex with record
      Coord   : Gdk.Rectangle.Gdk_Rectangle := (0, 0, 10, 10);
      Visible : Boolean := True;

      From_Auto_Layout : Boolean := True;
      --  True if the item's current location is the result of the automatic
      --  layout algorithm.
   end record;

   type Buffered_Item_Record is new Canvas_Item_Record with record
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
   end record;

   pragma Inline (To_Canvas_Coordinates);
   pragma Inline (To_World_Coordinates);
   pragma Inline (Get_Arrow_Type);
   pragma Inline (Pixmap);
end Gtkada.Canvas;

