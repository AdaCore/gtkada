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
with Gtk.Viewport;
with Gdk.Rectangle;

package Gtkada.Canvas is

   type Interactive_Canvas_Record is new Gtk.Viewport.Gtk_Viewport_Record
     with private;
   type Interactive_Canvas is access all Interactive_Canvas_Record'Class;
   --  A canvas on which items are put.
   --  Each item can be moved interactively by the user, and links can be
   --  drawn automatically from an item to another.

   type Canvas_Item_Record is abstract tagged private;
   type Canvas_Item is access all Canvas_Item_Record'Class;
   --  An item that can be put on the canvas.
   --  This is an abstract type, as it does not provide any default drawing
   --  routine. Instead, the end-user should extend this type and implement
   --  a subprogram to draw on the pixmap returned by the Pixmap subprogram.

   -------------------
   -- Customization --
   -------------------

   Default_Annotation_Font   : constant String := "Helvetica";
   --  Font used when displaying link annotation.
   --  This is the name of a postscript font, as defined in Gtk.Extra.PsFont.

   Default_Annotation_Height : constant := 7;
   --  Default Height use for the annotation font.

   Default_Grid_Size         : constant := 15;
   --  Number of pixels between two dots on the grid.
   --  This is used for both horizontal and vertical orientation.

   Default_Arc_Link_Offset   : constant := 25;
   --  Distance between two parallel arcs for two links.

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

   type Arrow_Type is (No_Arrow,
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

   procedure Align_On_Grid (Canvas : access Interactive_Canvas_Record;
                            Align  : Boolean := True);
   --  Choose whether the items should be aligned on the grid when moved.
   --  Existing items are not moved even if you set this parameter to True,
   --  this will only take effect the next time the items are moved.

   function Get_Align_On_Grid (Canvas : access Interactive_Canvas_Record)
                              return Boolean;
   --  Return True if items are currently aligned on grid.

   procedure Add_Link (Canvas : access Interactive_Canvas_Record;
                       Src    : access Canvas_Item_Record'Class;
                       Dest   : access Canvas_Item_Record'Class;
                       Arrow  : in Arrow_Type := End_Arrow;
                       Descr  : in String := "");
   --  Add an oriented link between two items.
   --  If Descr is not the empty string, it will be displayed in the middle
   --  of the link, and should indicate what the link means.
   --  This package automatically chose whether the link should be a straight
   --  line or an arc, so as to avoid overloading links.
   --  An arrow is drawn at the end of the link, on the border of Dest.

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

   procedure Put (Canvas : access Interactive_Canvas_Record;
                  Item   : access Canvas_Item_Record'Class;
                  X, Y   : Glib.Gint := Glib.Gint'First);
   --  Add a new item to the canvas.
   --  The item is added at a specific location.
   --  If at least one of X or Y has the default value, then the item
   --  is placed automatically in a free area of the canvas.
   --  Its new position depends on whether it has links to other existing
   --  items (in which case it is placed to the right of it), or not (in which
   --  case it is placed at the bottom of the leftmost column).

   procedure Remove (Canvas : access Interactive_Canvas_Record;
                     Item   : access Canvas_Item_Record'Class);
   --  Remove an item and all the links to and from it from the canvas.
   --  The item itself is not freed.
   --  Nothing is done if the item is not part of the canvas.

   procedure Item_Updated (Canvas : access Interactive_Canvas_Record;
                           Item   : access Canvas_Item_Record'Class);
   --  This should be called when Item has changed the contents of its
   --  pixmap, and thus the Canvas should be updated.

   procedure Item_Resized (Canvas : access Interactive_Canvas_Record;
                           Item   : access Canvas_Item_Record'Class);
   --  This should be called when Item has been resized, and the canvas
   --  should be updated (ie the arrow should be redrawn for instance).
   --  However, Item should resize its pixmap and redraw itself before
   --  calling this procedure.

   procedure Refresh_Canvas (Canvas : access Interactive_Canvas_Record);
   --  Redraw the whole canvas (both in the double buffer and on the screen).

   type Item_Processor is access
     function (Canvas : access Interactive_Canvas_Record'Class;
               Item   : access Canvas_Item_Record'Class)
              return Boolean;
   procedure For_Each_Item (Canvas  : access Interactive_Canvas_Record;
                            Execute : Item_Processor);
   --  Execute an action on each of the items contained in the canvas.
   --  If Execute returns False, we stop traversing the list of children.

   function Has_Link (Canvas   : access Interactive_Canvas_Record;
                      From, To : access Canvas_Item_Record'Class;
                      Name     : String)
                     return Boolean;
   --  Test whether there is a link from From to To, with the same name.

   procedure Raise_Item (Canvas : access Interactive_Canvas_Record;
                         Item   : access Canvas_Item_Record'Class);
   --  Raise the item so that it is displayed on top of all the others
   --  The canvas is refreshed as needed to reflect the change.
   --  Nothing happens if Item is not part of the canvas.

   procedure Lower_Item (Canvas : access Interactive_Canvas_Record;
                         Item   : access Canvas_Item_Record'Class);
   --  Lower the item so that it is displayed below all the others.
   --  The canvas is refreshed as needed to reflect the change.
   --  Nothing happens if Item is not part of the canvas.

   function Is_On_Top (Canvas : access Interactive_Canvas_Record;
                       Item   : access Canvas_Item_Record'Class)
                      return Boolean;
   --  Return True if Item is displayed on top of all the others in the canvas.

   procedure Show_Item (Canvas : access Interactive_Canvas_Record;
                        Item   : access Canvas_Item_Record'Class);
   --  Scroll the canvas so that Item is visible.

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

   procedure Remove_From_Selection
     (Canvas : access Interactive_Canvas_Record;
      Item : access Canvas_Item_Record'Class);
   --  Remove Item from the selection.

   ------------------------
   -- Items manipulation --
   ------------------------

   procedure Initialize (Item   : access Canvas_Item_Record'Class;
                         Win    : Gdk.Window.Gdk_Window;
                         Width  : Glib.Gint;
                         Height : Glib.Gint);
   --  Function used to initialize the private data of the item.
   --  Each child of Canvas_Item should call this function, so as to create
   --  the canvas and register the size.

   function Pixmap (Item : access Canvas_Item_Record'Class)
                   return Gdk.Pixmap.Gdk_Pixmap;
   --  Return the pixmap on which the contents of the Item should be drawn.
   --  Drawing is left to the end-user.

   procedure On_Button_Click (Item   : access Canvas_Item_Record;
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

   function Get_Coord (Item : access Canvas_Item_Record)
                      return Gdk.Rectangle.Gdk_Rectangle;
   --  Return the coordinates and size of the item.

   procedure Set_Visibility
     (Item    : access Canvas_Item_Record;
      Visible : Boolean);
   --  Set the visibility status of the item.
   --  The canvas is not refreshed (this is your responsibility to do it after
   --  you have finished doing all the modifications).

   function Is_Visible (Item : access Canvas_Item_Record) return Boolean;
   --  Return True if the item is currently visible.

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
   --  </signals>

private

   type String_Access is access String;

   type Link_Side is (Straight, Right, Left);
   --  The type of each link.
   --  For Straight, the link is drawn as a straight line between the two
   --  items. The other two cases indicates curve links, to a specific side.

   type Link;
   type Link_Access is access Link;
   type Link is
      record
         Src    : Canvas_Item;
         Dest   : Canvas_Item;
         Descr  : String_Access;

         Arrow  : Arrow_Type;

         Side   : Link_Side;
         Offset : Glib.Gint;
         --  How the link is drawn.
         --  Offset is used, along with Side, to calculate the "equation" of
         --  the arc. Basically, Offset * Grid_Size is the distance in the
         --  middle of the link between where a straight link would be and
         --  where the arc is.

         Next   : Link_Access;
      end record;

   type Canvas_Item_List_Record;
   type Canvas_Item_List is access Canvas_Item_List_Record;
   type Canvas_Item_List_Record is
      record
         Item : Canvas_Item;
         Next : Canvas_Item_List;
      end record;

   type Interactive_Canvas_Record is new Gtk.Viewport.Gtk_Viewport_Record with
      record
         Links             : Link_Access := null;
         Children          : Canvas_Item_List := null;

         Selection         : Canvas_Item_List := null;
         --  List of currently selected items that will be moved when the mouse
         --  is dragged

         Xmax, Ymax        : Glib.Gint := 0;
         --  Maximal coordinates in the canvas.

         Last_X_Event      : Glib.Gint;
         Last_Y_Event      : Glib.Gint;
         --  Last position where the mouse was pressed or moved in the canvas.

         Mouse_Has_Moved   : Boolean;
         --  True if mouse has moved while the button was clicked. This is used
         --  to distinguish between item motion and item selection.

         Drawing_Area      : Gtk.Drawing_Area.Gtk_Drawing_Area;
         Double_Pixmap     : Gdk.Pixmap.Gdk_Pixmap;

         Grid_Size         : Glib.Guint := Default_Grid_Size;
         Annotation_Font   : String_Access;
         Annotation_Height : Glib.Gint := Default_Annotation_Height;
         Arc_Link_Offset   : Float := Float (Default_Arc_Link_Offset);
         Arrow_Angle       : Float;
         Arrow_Length      : Float := Float (Default_Arrow_Length);
         Motion_Threshold  : Glib.Gint := Default_Motion_Threshold;
         Align_On_Grid     : Boolean := False;

         --  The following variables are initialized as soon as a Gdk_Window
         --  has been created for the canvas, in the Realized subprograms.

         Clear_GC : Gdk.GC.Gdk_GC := Gdk.GC.Null_GC;
         Black_GC : Gdk.GC.Gdk_GC := Gdk.GC.Null_GC;
         Anim_GC  : Gdk.GC.Gdk_GC := Gdk.GC.Null_GC;
         Font     : Gdk.Font.Gdk_Font := Gdk.Font.Null_Font;
      end record;

   type Canvas_Item_Record is abstract tagged
      record
         Coord     : Gdk.Rectangle.Gdk_Rectangle;
         Pixmap    : Gdk.Pixmap.Gdk_Pixmap;
         Visible   : Boolean := True;
      end record;

   pragma Inline (Pixmap);
end Gtkada.Canvas;
