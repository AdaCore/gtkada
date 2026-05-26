------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Makes its child scrollable.
--
--  <picture> <source srcset="scrolledwindow-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example
--  GtkScrolledWindow" src="scrolledwindow.png"> </picture>
--  It does so using either internally added scrollbars or externally
--  associated adjustments, and optionally draws a frame around the child.
--
--  Widgets with native scrolling support, i.e. those whose classes implement
--  the [ifaceGtk.Scrollable] interface, are added directly. For other types of
--  widget, the class [classGtk.Viewport] acts as an adaptor, giving
--  scrollability to other widgets. [methodGtk.ScrolledWindow.set_child]
--  intelligently accounts for whether or not the added child is a
--  `GtkScrollable`. If it isn't, then it wraps the child in a `GtkViewport`.
--  Therefore, you can just add any child widget and not worry about the
--  details.
--
--  If [methodGtk.ScrolledWindow.set_child] has added a `GtkViewport` for you,
--  it will be automatically removed when you unset the child. Unless
--  [propertyGtk.ScrolledWindow:hscrollbar-policy] and
--  [propertyGtk.ScrolledWindow:vscrollbar-policy] are Gtk.Enums.Policy_Never
--  or Gtk.Enums.Policy_External, `GtkScrolledWindow` adds internal
--  `GtkScrollbar` widgets around its child. The scroll position of the child,
--  and if applicable the scrollbars, is controlled by the
--  [propertyGtk.ScrolledWindow:hadjustment] and
--  [propertyGtk.ScrolledWindow:vadjustment] that are associated with the
--  `GtkScrolledWindow`. See the docs on [classGtk.Scrollbar] for the details,
--  but note that the "step_increment" and "page_increment" fields are only
--  effective if the policy causes scrollbars to be present.
--
--  If a `GtkScrolledWindow` doesn't behave quite as you would like, or
--  doesn't have exactly the right layout, it's very possible to set up your
--  own scrolling with `GtkScrollbar` and for example a `GtkGrid`.
--
--  # Touch support
--
--  `GtkScrolledWindow` has built-in support for touch devices. When a
--  touchscreen is used, swiping will move the scrolled window, and will expose
--  'kinetic' behavior. This can be turned off with the
--  [propertyGtk.ScrolledWindow:kinetic-scrolling] property if it is undesired.
--
--  `GtkScrolledWindow` also displays visual 'overshoot' indication when the
--  content is pulled beyond the end, and this situation can be captured with
--  the [signalGtk.ScrolledWindow::edge-overshot] signal.
--
--  If no mouse device is present, the scrollbars will overlaid as narrow,
--  auto-hiding indicators over the content. If traditional scrollbars are
--  desired although no mouse is present, this behaviour can be turned off with
--  the [propertyGtk.ScrolledWindow:overlay-scrolling] property.
--
--  # Shortcuts and Gestures
--
--  The following signals have default keybindings:
--
--  - [signalGtk.ScrolledWindow::scroll-child]
--
--  # CSS nodes
--
--  `GtkScrolledWindow` has a main CSS node with name scrolledwindow. It gets
--  a .frame style class added when [propertyGtk.ScrolledWindow:has-frame] is
--  True.
--
--  It uses subnodes with names overshoot and undershoot to draw the overflow
--  and underflow indications. These nodes get the .left, .right, .top or
--  .bottom style class added depending on where the indication is drawn.
--
--  `GtkScrolledWindow` also sets the positional style classes (.left, .right,
--  .top, .bottom) and style classes related to overlay scrolling
--  (.overlay-indicator, .dragging, .hovering) on its scrollbars.
--
--  If both scrollbars are visible, the area where they meet is drawn with a
--  subnode named junction.
--
--  # Accessibility
--
--  Until GTK 4.10, `GtkScrolledWindow` used the
--  [enumGtk.AccessibleRole.group] role.
--
--  Starting from GTK 4.12, `GtkScrolledWindow` uses the
--  [enumGtk.AccessibleRole.generic] role.
--
--  <group>Layout containers</group>
--  <gtkada_demo>create_scrolled.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Scrolled_Window is

   type Gtk_Scrolled_Window_Record is new Gtk_Widget_Record with null record;
   type Gtk_Scrolled_Window is access all Gtk_Scrolled_Window_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Scrolled_Window : out Gtk_Scrolled_Window);
   procedure Initialize
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record'Class);
   --  Creates a new scrolled window.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Scrolled_Window_New return Gtk_Scrolled_Window;
   --  Creates a new scrolled window.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_scrolled_window_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Scrolled_Window.
   --  If the scrolled window automatically added a [classGtk.Viewport], this
   --  function will return the viewport widget, and you can retrieve its child
   --  using [methodGtk.Viewport.get_child].
   --  @return the child widget of Scrolled_Window

   procedure Set_Child
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Child           : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Scrolled_Window.
   --  If Child does not implement the [ifaceGtk.Scrollable] interface, the
   --  scrolled window will add Child to a [classGtk.Viewport] instance and
   --  then add the viewport as its child widget.
   --  @param Child the child widget

   function Get_Hadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Returns the horizontal scrollbar's adjustment.
   --  This is the adjustment used to connect the horizontal scrollbar to the
   --  child widget's horizontal scroll functionality.
   --  @return the horizontal `GtkAdjustment`

   procedure Set_Hadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Hadjustment     : Gtk.Adjustment.Gtk_Adjustment := null);
   --  Sets the `GtkAdjustment` for the horizontal scrollbar.
   --  @param Hadjustment the `GtkAdjustment` to use, or null to create a new
   --  one

   function Get_Has_Frame
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean;
   --  Gets whether the scrolled window draws a frame.
   --  @return True if the Scrolled_Window has a frame

   procedure Set_Has_Frame
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Has_Frame       : Boolean);
   --  Changes the frame drawn around the contents of Scrolled_Window.
   --  @param Has_Frame whether to draw a frame around scrolled window
   --  contents

   function Get_Hscrollbar
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the horizontal scrollbar of Scrolled_Window.
   --  @return the horizontal scrollbar of the scrolled window.

   function Get_Kinetic_Scrolling
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean;
   --  Returns the specified kinetic scrolling behavior.
   --  @return the scrolling behavior flags.

   procedure Set_Kinetic_Scrolling
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Kinetic_Scrolling : Boolean);
   --  Turns kinetic scrolling on or off.
   --  Kinetic scrolling only applies to devices with source
   --  Gdk.Device.Gdk_Source_Touchscreen.
   --  @param Kinetic_Scrolling True to enable kinetic scrolling

   function Get_Max_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Glib.Gint;
   --  Returns the maximum content height set.
   --  @return the maximum content height, or -1

   procedure Set_Max_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Height          : Glib.Gint);
   --  Sets the maximum height that Scrolled_Window should keep visible.
   --  The Scrolled_Window will grow up to this height before it starts
   --  scrolling the content.
   --  It is a programming error to set the maximum content height to a value
   --  smaller than [propertyGtk.ScrolledWindow:min-content-height].
   --  @param Height the maximum content height

   function Get_Max_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Glib.Gint;
   --  Returns the maximum content width set.
   --  @return the maximum content width, or -1

   procedure Set_Max_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Width           : Glib.Gint);
   --  Sets the maximum width that Scrolled_Window should keep visible.
   --  The Scrolled_Window will grow up to this width before it starts
   --  scrolling the content.
   --  It is a programming error to set the maximum content width to a value
   --  smaller than [propertyGtk.ScrolledWindow:min-content-width].
   --  @param Width the maximum content width

   function Get_Min_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Glib.Gint;
   --  Gets the minimal content height of Scrolled_Window.
   --  @return the minimal content height

   procedure Set_Min_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Height          : Glib.Gint);
   --  Sets the minimum height that Scrolled_Window should keep visible.
   --  Note that this can and (usually will) be smaller than the minimum size
   --  of the content.
   --  It is a programming error to set the minimum content height to a value
   --  greater than [propertyGtk.ScrolledWindow:max-content-height].
   --  @param Height the minimal content height

   function Get_Min_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Glib.Gint;
   --  Gets the minimum content width of Scrolled_Window.
   --  @return the minimum content width

   procedure Set_Min_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Width           : Glib.Gint);
   --  Sets the minimum width that Scrolled_Window should keep visible.
   --  Note that this can and (usually will) be smaller than the minimum size
   --  of the content.
   --  It is a programming error to set the minimum content width to a value
   --  greater than [propertyGtk.ScrolledWindow:max-content-width].
   --  @param Width the minimal content width

   function Get_Overlay_Scrolling
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean;
   --  Returns whether overlay scrolling is enabled for this scrolled window.
   --  @return True if overlay scrolling is enabled

   procedure Set_Overlay_Scrolling
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Overlay_Scrolling : Boolean);
   --  Enables or disables overlay scrolling for this scrolled window.
   --  @param Overlay_Scrolling whether to enable overlay scrolling

   function Get_Placement
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Enums.Gtk_Corner_Type;
   --  Gets the placement of the contents with respect to the scrollbars.
   --  @return the current placement value.

   procedure Set_Placement
      (Scrolled_Window  : not null access Gtk_Scrolled_Window_Record;
       Window_Placement : Gtk.Enums.Gtk_Corner_Type);
   --  Sets the placement of the contents with respect to the scrollbars for
   --  the scrolled window.
   --  The default is Gtk.Enums.Corner_Top_Left, meaning the child is in the
   --  top left, with the scrollbars underneath and to the right. Other values
   --  in [enumGtk.CornerType] are Gtk.Enums.Corner_Top_Right,
   --  Gtk.Enums.Corner_Bottom_Left, and Gtk.Enums.Corner_Bottom_Right.
   --  See also [methodGtk.ScrolledWindow.get_placement] and
   --  [methodGtk.ScrolledWindow.unset_placement].
   --  @param Window_Placement position of the child window

   procedure Get_Policy
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Hscrollbar_Policy : out Gtk.Enums.Gtk_Policy_Type;
       Vscrollbar_Policy : out Gtk.Enums.Gtk_Policy_Type);
   --  Retrieves the current policy values for the horizontal and vertical
   --  scrollbars.
   --  See [methodGtk.ScrolledWindow.set_policy].
   --  @param Hscrollbar_Policy location to store the policy for the
   --  horizontal scrollbar
   --  @param Vscrollbar_Policy location to store the policy for the vertical
   --  scrollbar

   procedure Set_Policy
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Hscrollbar_Policy : Gtk.Enums.Gtk_Policy_Type;
       Vscrollbar_Policy : Gtk.Enums.Gtk_Policy_Type);
   --  Sets the scrollbar policy for the horizontal and vertical scrollbars.
   --  The policy determines when the scrollbar should appear; it is a value
   --  from the [enumGtk.PolicyType] enumeration. If Gtk.Enums.Policy_Always,
   --  the scrollbar is always present; if Gtk.Enums.Policy_Never, the
   --  scrollbar is never present; if Gtk.Enums.Policy_Automatic, the scrollbar
   --  is present only if needed (that is, if the slider part of the bar would
   --  be smaller than the trough — the display is larger than the page size).
   --  @param Hscrollbar_Policy policy for horizontal bar
   --  @param Vscrollbar_Policy policy for vertical bar

   function Get_Propagate_Natural_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean;
   --  Reports whether the natural height of the child will be calculated and
   --  propagated through the scrolled window's requested natural height.
   --  @return whether natural height propagation is enabled.

   procedure Set_Propagate_Natural_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Propagate       : Boolean);
   --  Sets whether the natural height of the child should be calculated and
   --  propagated through the scrolled window's requested natural height.
   --  @param Propagate whether to propagate natural height

   function Get_Propagate_Natural_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean;
   --  Reports whether the natural width of the child will be calculated and
   --  propagated through the scrolled window's requested natural width.
   --  @return whether natural width propagation is enabled.

   procedure Set_Propagate_Natural_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Propagate       : Boolean);
   --  Sets whether the natural width of the child should be calculated and
   --  propagated through the scrolled window's requested natural width.
   --  @param Propagate whether to propagate natural width

   function Get_Vadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Returns the vertical scrollbar's adjustment.
   --  This is the adjustment used to connect the vertical scrollbar to the
   --  child widget's vertical scroll functionality.
   --  @return the vertical `GtkAdjustment`

   procedure Set_Vadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Vadjustment     : Gtk.Adjustment.Gtk_Adjustment := null);
   --  Sets the `GtkAdjustment` for the vertical scrollbar.
   --  @param Vadjustment the `GtkAdjustment` to use, or null to create a new
   --  one

   function Get_Vscrollbar
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the vertical scrollbar of Scrolled_Window.
   --  @return the vertical scrollbar of the scrolled window.

   procedure Unset_Placement
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record);
   --  Unsets the placement of the contents with respect to the scrollbars.
   --  If no window placement is set for a scrolled window, it defaults to
   --  Gtk.Enums.Corner_Top_Left.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Scrolled_Window_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Scrolled_Window_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Scrolled_Window_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Scrolled_Window_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Scrolled_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Scrolled_Window_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Scrolled_Window_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Scrolled_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Scrolled_Window_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Scrolled_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.
   --
   --  When setting this property, if the child widget does not implement
   --  [ifaceGtk.Scrollable], the scrolled window will add the child to a
   --  [classGtk.Viewport] and then set the viewport as the child.

   Hadjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  The `GtkAdjustment` for the horizontal position.

   Has_Frame_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to draw a frame around the contents.

   Hscrollbar_Policy_Property : constant Gtk.Enums.Property_Gtk_Policy_Type;
   --  When the horizontal scrollbar is displayed.
   --
   --  Use [methodGtk.ScrolledWindow.set_policy] to set this property.

   Kinetic_Scrolling_Property : constant Glib.Properties.Property_Boolean;
   --  Whether kinetic scrolling is enabled or not.
   --
   --  Kinetic scrolling only applies to devices with source
   --  Gdk.Device.Gdk_Source_Touchscreen.

   Max_Content_Height_Property : constant Glib.Properties.Property_Int;
   --  The maximum content height of Scrolled_Window.

   Max_Content_Width_Property : constant Glib.Properties.Property_Int;
   --  The maximum content width of Scrolled_Window.

   Min_Content_Height_Property : constant Glib.Properties.Property_Int;
   --  The minimum content height of Scrolled_Window.

   Min_Content_Width_Property : constant Glib.Properties.Property_Int;
   --  The minimum content width of Scrolled_Window.

   Overlay_Scrolling_Property : constant Glib.Properties.Property_Boolean;
   --  Whether overlay scrolling is enabled or not.
   --
   --  If it is, the scrollbars are only added as traditional widgets when a
   --  mouse is present. Otherwise, they are overlaid on top of the content, as
   --  narrow indicators.
   --
   --  Note that overlay scrolling can also be globally disabled, with the
   --  [propertyGtk.Settings:gtk-overlay-scrolling] setting.

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

   Vadjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  The `GtkAdjustment` for the vertical position.

   Vscrollbar_Policy_Property : constant Gtk.Enums.Property_Gtk_Policy_Type;
   --  When the vertical scrollbar is displayed.
   --
   --  Use [methodGtk.ScrolledWindow.set_policy] to set this property.

   Window_Placement_Property : constant Gtk.Enums.Property_Gtk_Corner_Type;
   --  Where the contents are located with respect to the scrollbars.

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
   --  Emitted whenever user initiated scrolling makes the scrolled window
   --  firmly surpass the limits defined by the adjustment in that orientation.
   --
   --  A similar behavior without edge resistance is provided by the
   --  [signalGtk.ScrolledWindow::edge-reached] signal.
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
   --  Emitted whenever user-initiated scrolling makes the scrolled window
   --  exactly reach the lower or upper limits defined by the adjustment in
   --  that orientation.
   --
   --  A similar behavior with edge resistance is provided by the
   --  [signalGtk.ScrolledWindow::edge-overshot] signal.
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
   --  Emitted when focus is moved away from the scrolled window by a
   --  keybinding.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default bindings for this signal are <kbd>Ctrl</kbd>+<kbd>Tab</kbd>
   --  to move forward and <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Tab</kbd>` to
   --  move backward.

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
   --  Emitted when a keybinding that scrolls is pressed.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The horizontal or vertical adjustment is updated which triggers a
   --  signal that the scrolled window's child may listen to and scroll itself.
   -- 
   --  Callback parameters:
   --    --  @param Scroll a `GtkScrollType` describing how much to scroll
   --    --  @param Horizontal whether the keybinding scrolls the child horizontally
   --    --  or not

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Scrolled_Window_Record, Gtk_Scrolled_Window);
   function "+"
     (Widget : access Gtk_Scrolled_Window_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Scrolled_Window
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Scrolled_Window_Record, Gtk_Scrolled_Window);
   function "+"
     (Widget : access Gtk_Scrolled_Window_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Scrolled_Window
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Window_Placement_Property : constant Gtk.Enums.Property_Gtk_Corner_Type :=
     Gtk.Enums.Build ("window-placement");
   Vscrollbar_Policy_Property : constant Gtk.Enums.Property_Gtk_Policy_Type :=
     Gtk.Enums.Build ("vscrollbar-policy");
   Vadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("vadjustment");
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
   Has_Frame_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-frame");
   Hadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("hadjustment");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Scrolled_Window;
