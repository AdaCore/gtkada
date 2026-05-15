------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Ada.Calendar;   use Ada.Calendar;
with Glib.Object;
with GNAT.Calendar;  use GNAT.Calendar;
with Gtk.Enums;

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
   --  The size of the grid can be set with Gtkada.Canvas_View.Set_Grid_Size.
   --  This also sets the background color from the style's fill pattern.

   procedure Draw_Grid_Dots
     (Self    : not null access Canvas_View_Record'Class;
      Style   : Gtkada.Style.Drawing_Style;
      Context : Draw_Context;
      Area    : Model_Rectangle);
   --  Draw a grid with dots in the background
   --  This also sets the background color from the style's fill pattern.

   ------------
   -- Easing --
   ------------
   --  These functions are used to compute the intermediate values during an
   --  animation. They can be used to provide special effects like starting
   --  slow, finish slow, or even bounding when reaching the end.
   --
   --  see http://www.robertpenner.com/easing
   --  and http://api.jqueryui.com/easings/

   type Animation_Progress is new Duration range 0.0 .. 1.0;
   type Animation_Value is record
      Start, Finish : Gdouble;
      Duration      : Standard.Duration;
   end record;
   --  Describes one value to be animated, giving its initial and final values,
   --  as well as the duration that the total animation should take.

   type Easing_Function is access function
     (Value    : Animation_Value;
      Progress : Animation_Progress) return Gdouble;
   --  A function that is responsible for computing the current value of
   --  a property, given the initial and final values, and the current
   --  progress. It returns the current value of the property.

   function Easing_Linear
     (Value : Animation_Value; Progress : Animation_Progress) return Gdouble;
   --  The current value is on the straight line from Start to Finish.
   --  Progresses at a constant pace.

   function Easing_In_Out_Cubic
     (Value : Animation_Value; Progress : Animation_Progress) return Gdouble;
   --  Rate of change starts slow, increases to linear in the middle, and
   --  slows done in the end.

   function Easing_In_Cubic
     (Value : Animation_Value; Progress : Animation_Progress) return Gdouble;
   --  Starts slow, and then speeds up till the end.

   function Easing_Out_Cubic
     (Value : Animation_Value; Progress : Animation_Progress) return Gdouble;
   --  Starts normally, then slows down near the end

   function Easing_Out_Elastic
     (Value : Animation_Value; Progress : Animation_Progress) return Gdouble;
   --  Will move past the finish, then slightly back towards the start, and so
   --  on.

   function Easing_Out_Bounce
     (Value : Animation_Value; Progress : Animation_Progress) return Gdouble;
   --  Will reach the finish value early, then bounce back towards the start,
   --  a few times. Does not go over the finish value.

   ---------------
   -- Callbacks --
   ---------------
   --  These procedures contain a number of example callbacks for "item_event"
   --  which enable various behaviors. Depending on your application, one of
   --  these might be useful as is, or a starting point for your own callback

   function On_Item_Event_Move_Item
     (View  : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  Add this to the list of callbacks for "item_event" to enable dragging
   --  items with the mouse.
   --  If shift is pressed, no snapping on the grid or smart guides occurs.
   --  You can call Avoid_Overlap below if you want over items to be moved
   --  aside to avoid overlap.

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
      Duration : Standard.Duration := 0.0;
      Easing   : Easing_Function := Easing_In_Out_Cubic'Access;
   function On_Item_Event_Zoom_Generic
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  Add this to the list of callbacks for "item_event" to enable zooming in
   --  or out with the mouse wheel and a keyboard modifier like ctrl, alt,...
   --  (since the mouse wheel on its own is used for vertical scrolling by
   --  gtk+, and for horizontal scrolling when used with shift).
   --  If a duration other than 0.0 is provided, the scaling is animated.

   function On_Item_Event_Zoom
     (View     : not null access Glib.Object.GObject_Record'Class;
      Event    : Event_Details_Access;
      Modifier : Gdk.Types.Gdk_Modifier_Type;
      Factor   : Gdouble;
      Duration : Standard.Duration;
      Easing   : Easing_Function)
      return Boolean;
   --  This function does zooming. It is called by On_Item_Event_Zoom_Generic
   --  and usually it is not necesarry to call it directly.

   function On_Item_Event_Select
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  When an item is clicked, it is added to the selection (or replaces the
   --  selection, depending on the modifiers).
   --  This callback should be connected first (before any of the others above)

   generic
      Modifier     : Gdk.Types.Gdk_Modifier_Type := 0;
      Ignore_Links : Boolean := True;
   function On_Item_Event_Key_Navigate_Generic
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  Add this to the list of callbacks for "item_event" so that arrow keys
   --  move the selection to another item.

   generic
      Modifier : Gdk.Types.Gdk_Modifier_Type := Mod1_Mask;
   function On_Item_Event_Key_Scrolls_Generic
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  Add this to the list of callbacks for "item_event" so that arrow keys
   --  scroll the view when no item is selected, or moves the selected items.

   function On_Item_Event_Edit
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  Add this to the list of callbacks for "item_event" so that double
   --  clicking on an item that supports it starts editing it.
   --  This editing is by default only supported for Text_Item, but you can
   --  override the Edit_Widget method for other items if you want to support
   --  in-place editing for them too.

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

   ----------------
   -- Navigation --
   ----------------

   function Move_To_Item
     (Self         : not null access Canvas_View_Record'Class;
      Item         : not null access Abstract_Item_Record'Class;
      Dir          : Gtk.Enums.Gtk_Direction_Type;
      Ignore_Links : Boolean := True)
      return Abstract_Item;
   --  Search for the next item in the given direction

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
     (Self : not null access Canvas_View_Record'Class);
   --  Stops the continuous scrolling (that occurs while dragging items outside
   --  of the visible area)

   --------------------
   -- Inline editing --
   --------------------

   procedure Start_Inline_Editing
     (Self : not null access Canvas_View_Record'Class;
      Item : not null access Abstract_Item_Record'Class);
   --  If Item is editable, overlap a widget on top of it to allow its editing.
   --  The widget is created via the Item.Edit_Widget method.
   --  Returns True if such a widget could be displayed, False if editing could
   --  not take place.

   function Inline_Editing_In_Progress
     (Self : not null access Canvas_View_Record'Class)
     return Boolean;
   --  Whether any inline editing is taking place

   procedure Cancel_Inline_Editing
     (Self    : not null access Canvas_View_Record'Class);
   --  Destroys any inline editing widget that might be set

   ---------------
   -- Animation --
   ---------------
   --  The following subprograms provide a light-weight animation framework.
   --  Rather than do your own animation through the use of gtk's idle or
   --  timeout callbacks, it is more efficient to use this framework which will
   --  register a single callback and avoid monopolizing the CPU for too long
   --  each time.
   --  To move an item from its current position to another with animation,
   --  use something like:
   --      Animate (View, Animate_Position (Item, (100.0, 100.0)));

   type Animation_Status is mod 2 ** 16;
   Needs_Refresh_Links_From_Item : constant Animation_Status := 2 ** 0;
   --  Whether we need to recompute the layout of links to and from the
   --  animated item.

   Needs_Refresh_All_Links : constant Animation_Status := 2 ** 1;
   --  Whether we need to recompute the layout of all links

   Needs_Refresh_Layout : constant Animation_Status := 2 ** 2;
   --  Whether we need to recompute the layout of the whole model (items and
   --  links).

   type Animator is abstract tagged private;
   type Animator_Access is access all Animator'Class;

   procedure Destroy (Self : in out Animator) is null;
   --  Called when the animator has finished running

   function Is_Unique_For_Item
     (Self : not null access Animator) return Boolean;
   --  If True,  single animator of this type can be active for a given item.
   --  As a result, when you call Animate for this animator, any other
   --  registered similar animator for the same item is removed from the queue
   --  (and not completed).

   procedure Setup
     (Self     : in out Animator;
      Duration : Standard.Duration;
      Easing   : not null Easing_Function := Easing_In_Out_Cubic'Access;
      View     : access Canvas_View_Record'Class := null;
      Item     : access Abstract_Item_Record'Class := null);
   --  Initialize internal fields. This is only needed when you are writing
   --  your own animators.

   function Execute
     (Self     : not null access Animator;
      Progress : Animation_Progress) return Animation_Status is abstract;
   --  Performs one iteration of the animation.
   --  For instance, this could be moving a specific item slightly closer to
   --  its goal, or zooming the view a bit more.

   procedure Start
     (Self : access Animator'Class;
      View : not null access Canvas_View_Record'Class);
   --  Adds the animator to the animation queue.
   --  The animator will be destroyed automatically (and memory reclaimed) when
   --  it finishes its execution.
   --  It is valid to pass a null animator (nothing happens in this case)

   procedure Terminate_Animation
     (Self : not null access Canvas_View_Record'Class);
   --  Terminate the animation queue:
   --  All animators are completed (i.e. for instance items are moved to their
   --  final position,...)

   procedure Terminate_Animation_For_Item
     (Self : not null access Canvas_View_Record'Class;
      Item : access Abstract_Item_Record'Class := null);
   --  Terminate the animation for a specific item (or for the view itself when
   --  Item is null).

   ---------------
   -- Animators --
   ---------------
   --  Various prebuilt animators.

   function Animate_Position
     (Item           : not null access Abstract_Item_Record'Class;
      Final_Position : Gtkada.Style.Point;
      Duration       : Standard.Duration := 0.4;
      Easing         : Easing_Function := Easing_In_Out_Cubic'Access)
      return Animator_Access;
   --  Moves an item from one position to another.
   --  Returns null if the item is already at the right position

   function Animate_Scale
     (View           : not null access Canvas_View_Record'Class;
      Final_Scale    : Gdouble;
      Preserve       : Model_Point := No_Point;
      Duration       : Standard.Duration := 0.4;
      Easing         : Easing_Function := Easing_In_Out_Cubic'Access)
      return Animator_Access;
   --  Changes the scale of the view progressively

   function Animate_Scroll
     (View           : not null access Canvas_View_Record'Class;
      Final_Topleft  : Model_Point;
      Duration       : Standard.Duration := 0.8;
      Easing         : Easing_Function := Easing_In_Out_Cubic'Access)
      return Animator_Access;
   --  Scroll the canvas until the top-left corner reaches the given coordinate

   --------------
   -- Overlaps --
   --------------
   --  The following subprograms can be used to avoid overlap of items.

   type Move_Direction is
     (Left, Right, Up, Down, Horizontal, Vertical, Any);
   type Specific_Direction is new Move_Direction range Left .. Down;
   --  In which direction items should be moved to make space for other items.

   procedure Reserve_Space
     (Self        : not null access Canvas_View_Record'Class;
      Rect        : Model_Rectangle;
      Direction   : Move_Direction := Any;
      Do_Not_Move : Item_Sets.Set := Item_Sets.Empty_Set;
      Duration    : Standard.Duration := 0.0;
      Easing      : Easing_Function := Easing_In_Out_Cubic'Access);
   --  Move aside all items that intersect with the rectangle, so that the
   --  latter ends up being an empty area.
   --  The direction constraints what is allowed. By default, the items are
   --  moved in the direction of the minimal distance. Items can also end up
   --  pushing other items in turn if they need some extra space.
   --  Duration can be specified to animate items to their new position.

   procedure Insert_And_Layout_Items
     (Self                 : not null access Canvas_View_Record'Class;
      Ref                  : not null access Abstract_Item_Record'Class;
      Items                : Items_Lists.List;
      Direction            : Specific_Direction;
      Space_Between_Items  : Gdouble := 10.0;
      Space_Between_Layers : Gdouble := 20.0;
      Duration             : Standard.Duration := 0.0);
   --  Insert several items in the view, with the following behavior:
   --    * If Ref itself currenty has No_Position, it is moved to a position
   --      to below all other items currently in the canvas (if Direction is
   --      Left or Right) or to the right of all other items.
   --
   --    * the other items will be displayed to one side of Ref, after one
   --      below the other (if Direction is Left or Right), or one next to the
   --      other. Their current position is ignored.
   --
   --  Any item currently in those position will be moved aside via a call to
   --  Reserve_Space.
   --  This procedure can be used to avoid recomputing the whole layout of the
   --  view, and perhaps preserve whatever changes the user has made to the
   --  model.
   --
   --  Direction is the position of the items in Items compared to Ref.

private
   type Minimap_View_Record is new Canvas_View_Record with record
      Monitored           : Canvas_View;
      Viewport_Changed_Id : Gtk.Handlers.Handler_Id;
      Area_Style          : Gtkada.Style.Drawing_Style;

      Drag_Pos_X, Drag_Pos_Y : Gdouble;
   end record;

   type Animator is abstract tagged record
      Start    : Ada.Calendar.Time := GNAT.Calendar.No_Time;
      Duration : Standard.Duration;
      Easing   : Easing_Function;

      Item     : access Abstract_Item_Record'Class;
      View     : access Canvas_View_Record'Class;
      --  Set only when animating a specific item or view
   end record;

end Gtkada.Canvas_View.Views;
