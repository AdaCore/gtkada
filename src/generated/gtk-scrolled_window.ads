------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  GtkScrolledWindow is a container that accepts a single child widget, makes
--  that child scrollable using either internally added scrollbars or
--  externally associated adjustments, and optionally draws a frame around the
--  child.
--
--  Widgets with native scrolling support, i.e. those whose classes implement
--  the Gtk.Scrollable.Gtk_Scrollable interface, are added directly. For other
--  types of widget, the class Gtk.Viewport.Gtk_Viewport acts as an adaptor,
--  giving scrollability to other widgets. GtkScrolledWindow's implementation
--  of Gtk.Container.Add intelligently accounts for whether or not the added
--  child is a Gtk.Scrollable.Gtk_Scrollable. If it isn't,
--  Gtk.Scrolled_Window.Gtk_Scrolled_Window wraps the child in a
--  Gtk.Viewport.Gtk_Viewport and adds that for you. Therefore, you can just
--  add any child widget and not worry about the details.
--
--  If Gtk.Container.Add has added a Gtk.Viewport.Gtk_Viewport for you, you
--  can remove both your added child widget from the Gtk.Viewport.Gtk_Viewport,
--  and the Gtk.Viewport.Gtk_Viewport from the GtkScrolledWindow, like this:
--
--  |[<!-- language="C" --> GtkWidget *scrolled_window =
--  gtk_scrolled_window_new (NULL, NULL); GtkWidget *child_widget =
--  gtk_button_new ();
--
--  // GtkButton is not a GtkScrollable, so GtkScrolledWindow will
--  automatically // add a GtkViewport. gtk_container_add (GTK_CONTAINER
--  (scrolled_window), child_widget);
--
--  // Either of these will result in child_widget being unparented:
--  gtk_container_remove (GTK_CONTAINER (scrolled_window), child_widget); // or
--  gtk_container_remove (GTK_CONTAINER (scrolled_window), gtk_bin_get_child
--  (GTK_BIN (scrolled_window))); ]|
--
--  Unless Gtk.Scrolled_Window.Gtk_Scrolled_Window:policy is GTK_POLICY_NEVER
--  or GTK_POLICY_EXTERNAL, GtkScrolledWindow adds internal
--  Gtk.Scrollbar.Gtk_Scrollbar widgets around its child. The scroll position
--  of the child, and if applicable the scrollbars, is controlled by the
--  Gtk.Scrolled_Window.Gtk_Scrolled_Window:hadjustment and
--  Gtk.Scrolled_Window.Gtk_Scrolled_Window:vadjustment that are associated
--  with the GtkScrolledWindow. See the docs on Gtk.Scrollbar.Gtk_Scrollbar for
--  the details, but note that the "step_increment" and "page_increment" fields
--  are only effective if the policy causes scrollbars to be present.
--
--  If a GtkScrolledWindow doesn't behave quite as you would like, or doesn't
--  have exactly the right layout, it's very possible to set up your own
--  scrolling with Gtk.Scrollbar.Gtk_Scrollbar and for example a
--  Gtk.Grid.Gtk_Grid.
--
--  # Touch support
--
--  GtkScrolledWindow has built-in support for touch devices. When a
--  touchscreen is used, swiping will move the scrolled window, and will expose
--  'kinetic' behavior. This can be turned off with the
--  Gtk.Scrolled_Window.Gtk_Scrolled_Window:kinetic-scrolling property if it is
--  undesired.
--
--  GtkScrolledWindow also displays visual 'overshoot' indication when the
--  content is pulled beyond the end, and this situation can be captured with
--  the Gtk.Scrolled_Window.Gtk_Scrolled_Window::edge-overshot signal.
--
--  If no mouse device is present, the scrollbars will overlayed as narrow,
--  auto-hiding indicators over the content. If traditional scrollbars are
--  desired although no mouse is present, this behaviour can be turned off with
--  the Gtk.Scrolled_Window.Gtk_Scrolled_Window:overlay-scrolling property.
--
--  # CSS nodes
--
--  GtkScrolledWindow has a main CSS node with name scrolledwindow.
--
--  It uses subnodes with names overshoot and undershoot to draw the overflow
--  and underflow indications. These nodes get the .left, .right, .top or
--  .bottom style class added depending on where the indication is drawn.
--
--  GtkScrolledWindow also sets the positional style classes (.left, .right,
--  .top, .bottom) and style classes related to overlay scrolling
--  (.overlay-indicator, .dragging, .hovering) on its scrollbars.
--
--  If both scrollbars are visible, the area where they meet is drawn with a
--  subnode named junction.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Scrollbar;   use Gtk.Scrollbar;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Scrolled_Window is

   type Gtk_Scrolled_Window_Record is new Gtk_Bin_Record with null record;
   type Gtk_Scrolled_Window is access all Gtk_Scrolled_Window_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Scrolled_Window : out Gtk_Scrolled_Window;
       Hadjustment     : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment     : Gtk.Adjustment.Gtk_Adjustment := null);
   procedure Initialize
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record'Class;
       Hadjustment     : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment     : Gtk.Adjustment.Gtk_Adjustment := null);
   --  Creates a new scrolled window.
   --  The two arguments are the scrolled window's adjustments; these will be
   --  shared with the scrollbars and the child widget to keep the bars in sync
   --  with the child. Usually you want to pass null for the adjustments, which
   --  will cause the scrolled window to create them for you.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "hadjustment": horizontal adjustment
   --  "vadjustment": vertical adjustment

   function Gtk_Scrolled_Window_New
      (Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null)
       return Gtk_Scrolled_Window;
   --  Creates a new scrolled window.
   --  The two arguments are the scrolled window's adjustments; these will be
   --  shared with the scrollbars and the child widget to keep the bars in sync
   --  with the child. Usually you want to pass null for the adjustments, which
   --  will cause the scrolled window to create them for you.
   --  "hadjustment": horizontal adjustment
   --  "vadjustment": vertical adjustment

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_scrolled_window_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_With_Viewport
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Child           : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   pragma Obsolescent (Add_With_Viewport);
   --  Used to add children without native scrolling capabilities. This is
   --  simply a convenience function; it is equivalent to adding the
   --  unscrollable child to a viewport, then adding the viewport to the
   --  scrolled window. If a child has native scrolling, use Gtk.Container.Add
   --  instead of this function.
   --  The viewport scrolls the child by moving its Gdk.Gdk_Window, and takes
   --  the size of the child to be the size of its toplevel Gdk.Gdk_Window.
   --  This will be very wrong for most widgets that support native scrolling;
   --  for example, if you add a widget such as Gtk.Tree_View.Gtk_Tree_View
   --  with a viewport, the whole widget will scroll, including the column
   --  headings. Thus, widgets with native scrolling support should not be used
   --  with the Gtk.Viewport.Gtk_Viewport proxy.
   --  A widget supports scrolling natively if it implements the
   --  Gtk.Scrollable.Gtk_Scrollable interface.
   --  Deprecated since 3.8, 1
   --  "child": the widget you want to scroll

   function Get_Capture_Button_Press
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean;
   --  Return whether button presses are captured during kinetic scrolling.
   --  See Gtk.Scrolled_Window.Set_Capture_Button_Press.
   --  Since: gtk+ 3.4

   procedure Set_Capture_Button_Press
      (Scrolled_Window      : not null access Gtk_Scrolled_Window_Record;
       Capture_Button_Press : Boolean);
   --  Changes the behaviour of Scrolled_Window with regard to the initial
   --  event that possibly starts kinetic scrolling. When Capture_Button_Press
   --  is set to True, the event is captured by the scrolled window, and then
   --  later replayed if it is meant to go to the child widget.
   --  This should be enabled if any child widgets perform non-reversible
   --  actions on Gtk.Widget.Gtk_Widget::button-press-event. If they don't, and
   --  handle additionally handle Gtk.Widget.Gtk_Widget::grab-broken-event, it
   --  might be better to set Capture_Button_Press to False.
   --  This setting only has an effect if kinetic scrolling is enabled.
   --  Since: gtk+ 3.4
   --  "capture_button_press": True to capture button presses

   function Get_Hadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Returns the horizontal scrollbar's adjustment, used to connect the
   --  horizontal scrollbar to the child widget's horizontal scroll
   --  functionality.

   procedure Set_Hadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Hadjustment     : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the Gtk.Adjustment.Gtk_Adjustment for the horizontal scrollbar.
   --  "hadjustment": the Gtk.Adjustment.Gtk_Adjustment to use, or null to
   --  create a new one

   function Get_Hscrollbar
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Scrollbar.Gtk_Scrollbar;
   --  Returns the horizontal scrollbar of Scrolled_Window.
   --  Since: gtk+ 2.8

   function Get_Kinetic_Scrolling
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean;
   --  Returns the specified kinetic scrolling behavior.
   --  Since: gtk+ 3.4

   procedure Set_Kinetic_Scrolling
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Kinetic_Scrolling : Boolean);
   --  Turns kinetic scrolling on or off. Kinetic scrolling only applies to
   --  devices with source GDK_SOURCE_TOUCHSCREEN.
   --  Since: gtk+ 3.4
   --  "kinetic_scrolling": True to enable kinetic scrolling

   function Get_Max_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Glib.Gint;
   --  Returns the maximum content height set.
   --  Since: gtk+ 3.22

   procedure Set_Max_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Height          : Glib.Gint);
   --  Sets the maximum height that Scrolled_Window should keep visible. The
   --  Scrolled_Window will grow up to this height before it starts scrolling
   --  the content.
   --  It is a programming error to set the maximum content height to a value
   --  smaller than Gtk.Scrolled_Window.Gtk_Scrolled_Window:min-content-height.
   --  Since: gtk+ 3.22
   --  "height": the maximum content height

   function Get_Max_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Glib.Gint;
   --  Returns the maximum content width set.
   --  Since: gtk+ 3.22

   procedure Set_Max_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Width           : Glib.Gint);
   --  Sets the maximum width that Scrolled_Window should keep visible. The
   --  Scrolled_Window will grow up to this width before it starts scrolling
   --  the content.
   --  It is a programming error to set the maximum content width to a value
   --  smaller than Gtk.Scrolled_Window.Gtk_Scrolled_Window:min-content-width.
   --  Since: gtk+ 3.22
   --  "width": the maximum content width

   function Get_Min_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Glib.Gint;
   --  Gets the minimal content height of Scrolled_Window, or -1 if not set.
   --  Since: gtk+ 3.0

   procedure Set_Min_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Height          : Glib.Gint);
   --  Sets the minimum height that Scrolled_Window should keep visible. Note
   --  that this can and (usually will) be smaller than the minimum size of the
   --  content.
   --  It is a programming error to set the minimum content height to a value
   --  greater than Gtk.Scrolled_Window.Gtk_Scrolled_Window:max-content-height.
   --  Since: gtk+ 3.0
   --  "height": the minimal content height

   function Get_Min_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Glib.Gint;
   --  Gets the minimum content width of Scrolled_Window, or -1 if not set.
   --  Since: gtk+ 3.0

   procedure Set_Min_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Width           : Glib.Gint);
   --  Sets the minimum width that Scrolled_Window should keep visible. Note
   --  that this can and (usually will) be smaller than the minimum size of the
   --  content.
   --  It is a programming error to set the minimum content width to a value
   --  greater than Gtk.Scrolled_Window.Gtk_Scrolled_Window:max-content-width.
   --  Since: gtk+ 3.0
   --  "width": the minimal content width

   function Get_Overlay_Scrolling
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean;
   --  Returns whether overlay scrolling is enabled for this scrolled window.
   --  Since: gtk+ 3.16

   procedure Set_Overlay_Scrolling
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Overlay_Scrolling : Boolean);
   --  Enables or disables overlay scrolling for this scrolled window.
   --  Since: gtk+ 3.16
   --  "overlay_scrolling": whether to enable overlay scrolling

   function Get_Placement
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Enums.Gtk_Corner_Type;
   --  Gets the placement of the contents with respect to the scrollbars for
   --  the scrolled window. See Gtk.Scrolled_Window.Set_Placement.

   procedure Set_Placement
      (Scrolled_Window  : not null access Gtk_Scrolled_Window_Record;
       Window_Placement : Gtk.Enums.Gtk_Corner_Type);
   --  Sets the placement of the contents with respect to the scrollbars for
   --  the scrolled window.
   --  The default is Gtk.Enums.Corner_Top_Left, meaning the child is in the
   --  top left, with the scrollbars underneath and to the right. Other values
   --  in Gtk.Enums.Gtk_Corner_Type are Gtk.Enums.Corner_Top_Right,
   --  Gtk.Enums.Corner_Bottom_Left, and Gtk.Enums.Corner_Bottom_Right.
   --  See also Gtk.Scrolled_Window.Get_Placement and
   --  Gtk.Scrolled_Window.Unset_Placement.
   --  "window_placement": position of the child window

   procedure Get_Policy
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Hscrollbar_Policy : out Gtk.Enums.Gtk_Policy_Type;
       Vscrollbar_Policy : out Gtk.Enums.Gtk_Policy_Type);
   --  Retrieves the current policy values for the horizontal and vertical
   --  scrollbars. See Gtk.Scrolled_Window.Set_Policy.
   --  "hscrollbar_policy": location to store the policy for the horizontal
   --  scrollbar, or null
   --  "vscrollbar_policy": location to store the policy for the vertical
   --  scrollbar, or null

   procedure Set_Policy
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Hscrollbar_Policy : Gtk.Enums.Gtk_Policy_Type;
       Vscrollbar_Policy : Gtk.Enums.Gtk_Policy_Type);
   --  Sets the scrollbar policy for the horizontal and vertical scrollbars.
   --  The policy determines when the scrollbar should appear; it is a value
   --  from the Gtk.Enums.Gtk_Policy_Type enumeration. If
   --  Gtk.Enums.Policy_Always, the scrollbar is always present; if
   --  Gtk.Enums.Policy_Never, the scrollbar is never present; if
   --  Gtk.Enums.Policy_Automatic, the scrollbar is present only if needed
   --  (that is, if the slider part of the bar would be smaller than the trough
   --  — the display is larger than the page size).
   --  "hscrollbar_policy": policy for horizontal bar
   --  "vscrollbar_policy": policy for vertical bar

   function Get_Propagate_Natural_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean;
   --  Reports whether the natural height of the child will be calculated and
   --  propagated through the scrolled window's requested natural height.
   --  Since: gtk+ 3.22

   procedure Set_Propagate_Natural_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Propagate       : Boolean);
   --  Sets whether the natural height of the child should be calculated and
   --  propagated through the scrolled window's requested natural height.
   --  Since: gtk+ 3.22
   --  "propagate": whether to propagate natural height

   function Get_Propagate_Natural_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean;
   --  Reports whether the natural width of the child will be calculated and
   --  propagated through the scrolled window's requested natural width.
   --  Since: gtk+ 3.22

   procedure Set_Propagate_Natural_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Propagate       : Boolean);
   --  Sets whether the natural width of the child should be calculated and
   --  propagated through the scrolled window's requested natural width.
   --  Since: gtk+ 3.22
   --  "propagate": whether to propagate natural width

   function Get_Shadow_Type
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Enums.Gtk_Shadow_Type;
   --  Gets the shadow type of the scrolled window. See
   --  Gtk.Scrolled_Window.Set_Shadow_Type.

   procedure Set_Shadow_Type
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       The_Type        : Gtk.Enums.Gtk_Shadow_Type);
   --  Changes the type of shadow drawn around the contents of
   --  Scrolled_Window.
   --  "type": kind of shadow to draw around scrolled window contents

   function Get_Vadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Returns the vertical scrollbar's adjustment, used to connect the
   --  vertical scrollbar to the child widget's vertical scroll functionality.

   procedure Set_Vadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Vadjustment     : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the Gtk.Adjustment.Gtk_Adjustment for the vertical scrollbar.
   --  "vadjustment": the Gtk.Adjustment.Gtk_Adjustment to use, or null to
   --  create a new one

   function Get_Vscrollbar
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Scrollbar.Gtk_Scrollbar;
   --  Returns the vertical scrollbar of Scrolled_Window.
   --  Since: gtk+ 2.8

   procedure Unset_Placement
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record);
   --  Unsets the placement of the contents with respect to the scrollbars for
   --  the scrolled window. If no window placement is set for a scrolled
   --  window, it defaults to Gtk.Enums.Corner_Top_Left.
   --  See also Gtk.Scrolled_Window.Set_Placement and
   --  Gtk.Scrolled_Window.Get_Placement.
   --  Since: gtk+ 2.10

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Hadjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment

   Hscrollbar_Policy_Property : constant Gtk.Enums.Property_Gtk_Policy_Type;

   Kinetic_Scrolling_Property : constant Glib.Properties.Property_Boolean;
   --  Whether kinetic scrolling is enabled or not. Kinetic scrolling only
   --  applies to devices with source GDK_SOURCE_TOUCHSCREEN.

   Max_Content_Height_Property : constant Glib.Properties.Property_Int;
   --  The maximum content height of Scrolled_Window, or -1 if not set.

   Max_Content_Width_Property : constant Glib.Properties.Property_Int;
   --  The maximum content width of Scrolled_Window, or -1 if not set.

   Min_Content_Height_Property : constant Glib.Properties.Property_Int;
   --  The minimum content height of Scrolled_Window, or -1 if not set.

   Min_Content_Width_Property : constant Glib.Properties.Property_Int;
   --  The minimum content width of Scrolled_Window, or -1 if not set.

   Overlay_Scrolling_Property : constant Glib.Properties.Property_Boolean;
   --  Whether overlay scrolling is enabled or not. If it is, the scrollbars
   --  are only added as traditional widgets when a mouse is present.
   --  Otherwise, they are overlayed on top of the content, as narrow
   --  indicators.
   --
   --  Note that overlay scrolling can also be globally disabled, with the
   --  Gtk.Settings.Gtk_Settings::gtk-overlay-scrolling setting.

   Propagate_Natural_Height_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the natural height of the child should be calculated and
   --  propagated through the scrolled window's requested natural height.
   --
   --  This is useful in cases where an attempt should be made to allocate
   --  exactly enough space for the natural size of the child.

   Propagate_Natural_Width_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the natural width of the child should be calculated and
   --  propagated through the scrolled window's requested natural width.
   --
   --  This is useful in cases where an attempt should be made to allocate
   --  exactly enough space for the natural size of the child.

   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type;

   Vadjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment

   Vscrollbar_Policy_Property : constant Gtk.Enums.Property_Gtk_Policy_Type;

   Window_Placement_Property : constant Gtk.Enums.Property_Gtk_Corner_Type;

   Window_Placement_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether "window-placement" should be used to determine the location of
   --  the contents with respect to the scrollbars.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Scrolled_Window_Gtk_Position_Type_Void is not null access procedure
     (Self : access Gtk_Scrolled_Window_Record'Class;
      Pos  : Gtk.Enums.Gtk_Position_Type);

   type Cb_GObject_Gtk_Position_Type_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Pos  : Gtk.Enums.Gtk_Position_Type);

   Signal_Edge_Overshot : constant Glib.Signal_Name := "edge-overshot";
   procedure On_Edge_Overshot
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_Gtk_Scrolled_Window_Gtk_Position_Type_Void;
       After : Boolean := False);
   procedure On_Edge_Overshot
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_GObject_Gtk_Position_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::edge-overshot signal is emitted whenever user initiated scrolling
   --  makes the scrolled window firmly surpass (i.e. with some edge
   --  resistance) the lower or upper limits defined by the adjustment in that
   --  orientation.
   --
   --  A similar behavior without edge resistance is provided by the
   --  Gtk.Scrolled_Window.Gtk_Scrolled_Window::edge-reached signal.
   --
   --  Note: The Pos argument is LTR/RTL aware, so callers should be aware too
   --  if intending to provide behavior on horizontal edges.

   Signal_Edge_Reached : constant Glib.Signal_Name := "edge-reached";
   procedure On_Edge_Reached
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_Gtk_Scrolled_Window_Gtk_Position_Type_Void;
       After : Boolean := False);
   procedure On_Edge_Reached
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_GObject_Gtk_Position_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::edge-reached signal is emitted whenever user-initiated scrolling
   --  makes the scrolled window exactly reach the lower or upper limits
   --  defined by the adjustment in that orientation.
   --
   --  A similar behavior with edge resistance is provided by the
   --  Gtk.Scrolled_Window.Gtk_Scrolled_Window::edge-overshot signal.
   --
   --  Note: The Pos argument is LTR/RTL aware, so callers should be aware too
   --  if intending to provide behavior on horizontal edges.

   type Cb_Gtk_Scrolled_Window_Gtk_Direction_Type_Void is not null access procedure
     (Self           : access Gtk_Scrolled_Window_Record'Class;
      Direction_Type : Gtk.Enums.Gtk_Direction_Type);

   type Cb_GObject_Gtk_Direction_Type_Void is not null access procedure
     (Self           : access Glib.Object.GObject_Record'Class;
      Direction_Type : Gtk.Enums.Gtk_Direction_Type);

   Signal_Move_Focus_Out : constant Glib.Signal_Name := "move-focus-out";
   procedure On_Move_Focus_Out
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_Gtk_Scrolled_Window_Gtk_Direction_Type_Void;
       After : Boolean := False);
   procedure On_Move_Focus_Out
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::move-focus-out signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when focus is moved away from the scrolled window by
   --  a keybinding. The Gtk.Widget.Gtk_Widget::move-focus signal is emitted
   --  with Direction_Type on this scrolled window's toplevel parent in the
   --  container hierarchy. The default bindings for this signal are `Ctrl +
   --  Tab` to move forward and `Ctrl + Shift + Tab` to move backward.

   type Cb_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean is not null access function
     (Self       : access Gtk_Scrolled_Window_Record'Class;
      Scroll     : Gtk.Enums.Gtk_Scroll_Type;
      Horizontal : Boolean) return Boolean;

   type Cb_GObject_Gtk_Scroll_Type_Boolean_Boolean is not null access function
     (Self       : access Glib.Object.GObject_Record'Class;
      Scroll     : Gtk.Enums.Gtk_Scroll_Type;
      Horizontal : Boolean) return Boolean;

   Signal_Scroll_Child : constant Glib.Signal_Name := "scroll-child";
   procedure On_Scroll_Child
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean;
       After : Boolean := False);
   procedure On_Scroll_Child
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::scroll-child signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when a keybinding that scrolls is pressed. The
   --  horizontal or vertical adjustment is updated which triggers a signal
   --  that the scrolled window's child may listen to and scroll itself.
   -- 
   --  Callback parameters:
   --    --  "scroll": a Gtk.Enums.Gtk_Scroll_Type describing how much to scroll
   --    --  "horizontal": whether the keybinding scrolls the child horizontally or
   --    --  not

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Scrolled_Window_Record, Gtk_Scrolled_Window);
   function "+"
     (Widget : access Gtk_Scrolled_Window_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Scrolled_Window
   renames Implements_Gtk_Buildable.To_Object;

private
   Window_Placement_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("window-placement-set");
   Window_Placement_Property : constant Gtk.Enums.Property_Gtk_Corner_Type :=
     Gtk.Enums.Build ("window-placement");
   Vscrollbar_Policy_Property : constant Gtk.Enums.Property_Gtk_Policy_Type :=
     Gtk.Enums.Build ("vscrollbar-policy");
   Vadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("vadjustment");
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type :=
     Gtk.Enums.Build ("shadow-type");
   Propagate_Natural_Width_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("propagate-natural-width");
   Propagate_Natural_Height_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("propagate-natural-height");
   Overlay_Scrolling_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("overlay-scrolling");
   Min_Content_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("min-content-width");
   Min_Content_Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("min-content-height");
   Max_Content_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-content-width");
   Max_Content_Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-content-height");
   Kinetic_Scrolling_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("kinetic-scrolling");
   Hscrollbar_Policy_Property : constant Gtk.Enums.Property_Gtk_Policy_Type :=
     Gtk.Enums.Build ("hscrollbar-policy");
   Hadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("hadjustment");
end Gtk.Scrolled_Window;
