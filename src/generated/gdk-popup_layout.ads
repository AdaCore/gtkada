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

--  Contains information that is necessary position a [ifaceGdk.Popup]
--  relative to its parent.
--
--  The positioning requires a negotiation with the windowing system, since it
--  depends on external constraints, such as the position of the parent
--  surface, and the screen dimensions.
--
--  The basic ingredients are a rectangle on the parent surface, and the
--  anchor on both that rectangle and the popup. The anchors specify a side or
--  corner to place next to each other.
--
--  ![Popup anchors](popup-anchors.png)
--
--  For cases where placing the anchors next to each other would make the
--  popup extend offscreen, the layout includes some hints for how to resolve
--  this problem. The hints may suggest to flip the anchor position to the
--  other side, or to 'slide' the popup along a side, or to resize it.
--
--  ![Flipping popups](popup-flip.png)
--
--  ![Sliding popups](popup-slide.png)
--
--  These hints may be combined.
--
--  Ultimatively, it is up to the windowing system to determine the position
--  and size of the popup. You can learn about the result by calling
--  [methodGdk.Popup.get_position_x], [methodGdk.Popup.get_position_y],
--  [methodGdk.Popup.get_rect_anchor] and [methodGdk.Popup.get_surface_anchor]
--  after the popup has been presented. This can be used to adjust the
--  rendering. For example, [GtkPopover](../gtk4/class.Popover.html) changes
--  its arrow position accordingly. But you have to be careful avoid changing
--  the size of the popover, or it has to be presented again.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Enums;     use Gdk.Enums;
with Gdk.Rectangle; use Gdk.Rectangle;
with Glib;          use Glib;
with System;

package Gdk.Popup_Layout is

   type Gdk_Popup_Layout is new Glib.C_Boxed with null record;
   Null_Gdk_Popup_Layout : constant Gdk_Popup_Layout;

   function From_Object (Object : System.Address) return Gdk_Popup_Layout;
   function From_Object_Free (B : access Gdk_Popup_Layout'Class) return Gdk_Popup_Layout;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New
      (Self           : out Gdk_Popup_Layout;
       Anchor_Rect    : Gdk.Rectangle.Gdk_Rectangle;
       Rect_Anchor    : Gdk.Enums.Gdk_Gravity;
       Surface_Anchor : Gdk.Enums.Gdk_Gravity);
   --  Create a popup layout description.
   --  Used together with [methodGdk.Popup.present] to describe how a popup
   --  surface should be placed and behave on-screen.
   --  Anchor_Rect is relative to the top-left corner of the surface's parent.
   --  Rect_Anchor and Surface_Anchor determine anchor points on Anchor_Rect
   --  and surface to pin together.
   --  The position of Anchor_Rect's anchor point can optionally be offset
   --  using [methodGdk.PopupLayout.set_offset], which is equivalent to
   --  offsetting the position of surface.
   --  @param Anchor_Rect the anchor rectangle to align Surface with
   --  @param Rect_Anchor the point on Anchor_Rect to align with Surface's
   --  anchor point
   --  @param Surface_Anchor the point on Surface to align with Rect's anchor
   --  point

   function Gdk_Popup_Layout_New
      (Anchor_Rect    : Gdk.Rectangle.Gdk_Rectangle;
       Rect_Anchor    : Gdk.Enums.Gdk_Gravity;
       Surface_Anchor : Gdk.Enums.Gdk_Gravity) return Gdk_Popup_Layout;
   --  Create a popup layout description.
   --  Used together with [methodGdk.Popup.present] to describe how a popup
   --  surface should be placed and behave on-screen.
   --  Anchor_Rect is relative to the top-left corner of the surface's parent.
   --  Rect_Anchor and Surface_Anchor determine anchor points on Anchor_Rect
   --  and surface to pin together.
   --  The position of Anchor_Rect's anchor point can optionally be offset
   --  using [methodGdk.PopupLayout.set_offset], which is equivalent to
   --  offsetting the position of surface.
   --  @param Anchor_Rect the anchor rectangle to align Surface with
   --  @param Rect_Anchor the point on Anchor_Rect to align with Surface's
   --  anchor point
   --  @param Surface_Anchor the point on Surface to align with Rect's anchor
   --  point

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_popup_layout_get_type");

   -------------
   -- Methods --
   -------------

   function Copy (Self : Gdk_Popup_Layout) return Gdk_Popup_Layout;
   --  Makes a copy of Layout.
   --  @return a copy of Layout.

   function Equal
      (Self  : Gdk_Popup_Layout;
       Other : Gdk_Popup_Layout) return Boolean;
   --  Check whether Layout and Other has identical layout properties.
   --  @param Other another popup layout
   --  @return true if Layout and Other have identical layout properties,
   --  otherwise false.

   function Get_Anchor_Hints
      (Self : Gdk_Popup_Layout) return Gdk.Enums.Gdk_Anchor_Hints;
   --  Get the anchor hints.
   --  @return the anchor hints

   procedure Set_Anchor_Hints
      (Self         : Gdk_Popup_Layout;
       Anchor_Hints : Gdk.Enums.Gdk_Anchor_Hints);
   --  Set new anchor hints.
   --  The set Anchor_Hints determines how Surface will be moved if the anchor
   --  points cause it to move off-screen. For example, `GDK_ANCHOR_FLIP_X`
   --  will replace `GDK_GRAVITY_NORTH_WEST` with `GDK_GRAVITY_NORTH_EAST` and
   --  vice versa if Surface extends beyond the left or right edges of the
   --  monitor.
   --  @param Anchor_Hints the new anchor hints

   procedure Get_Offset
      (Self : Gdk_Popup_Layout;
       Dx   : out Glib.Gint;
       Dy   : out Glib.Gint);
   --  Retrieves the offset for the anchor rectangle.
   --  @param Dx return location for the delta X coordinate
   --  @param Dy return location for the delta Y coordinate

   procedure Set_Offset
      (Self : Gdk_Popup_Layout;
       Dx   : Glib.Gint;
       Dy   : Glib.Gint);
   --  Offset the position of the anchor rectangle with the given delta.
   --  @param Dx x delta to offset the anchor rectangle with
   --  @param Dy y delta to offset the anchor rectangle with

   function Get_Rect_Anchor
      (Self : Gdk_Popup_Layout) return Gdk.Enums.Gdk_Gravity;
   --  Returns the anchor position on the anchor rectangle.
   --  @return the anchor on the anchor rectangle.

   procedure Set_Rect_Anchor
      (Self   : Gdk_Popup_Layout;
       Anchor : Gdk.Enums.Gdk_Gravity);
   --  Set the anchor on the anchor rectangle.
   --  @param Anchor the new rect anchor

   procedure Get_Shadow_Width
      (Self   : Gdk_Popup_Layout;
       Left   : out Glib.Gint;
       Right  : out Glib.Gint;
       Top    : out Glib.Gint;
       Bottom : out Glib.Gint);
   --  Obtains the shadow widths of this layout.
   --  Since: gtk+ 4.2
   --  @param Left return location for the left shadow width
   --  @param Right return location for the right shadow width
   --  @param Top return location for the top shadow width
   --  @param Bottom return location for the bottom shadow width

   procedure Set_Shadow_Width
      (Self   : Gdk_Popup_Layout;
       Left   : Glib.Gint;
       Right  : Glib.Gint;
       Top    : Glib.Gint;
       Bottom : Glib.Gint);
   --  Sets the shadow width of the popup.
   --  The shadow width corresponds to the part of the computed surface size
   --  that would consist of the shadow margin surrounding the window, would
   --  there be any.
   --  Since: gtk+ 4.2
   --  @param Left width of the left part of the shadow
   --  @param Right width of the right part of the shadow
   --  @param Top height of the top part of the shadow
   --  @param Bottom height of the bottom part of the shadow

   function Get_Surface_Anchor
      (Self : Gdk_Popup_Layout) return Gdk.Enums.Gdk_Gravity;
   --  Returns the anchor position on the popup surface.
   --  @return the anchor on the popup surface.

   procedure Set_Surface_Anchor
      (Self   : Gdk_Popup_Layout;
       Anchor : Gdk.Enums.Gdk_Gravity);
   --  Set the anchor on the popup surface.
   --  @param Anchor the new popup surface anchor

   function Ref (Self : Gdk_Popup_Layout) return Gdk_Popup_Layout;
   --  Increases the reference count of Value.
   --  @return the same Layout

   procedure Unref (Self : Gdk_Popup_Layout);
   --  Decreases the reference count of Value.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Anchor_Rect
     (Self : Gdk_Popup_Layout) return access constant Gdk.Rectangle.Gdk_Rectangle;
   --  Get the anchor rectangle.

   procedure Set_Anchor_Rect
     (Self        : Gdk_Popup_Layout;
      Anchor_Rect : not null access Gdk.Rectangle.Gdk_Rectangle);
   --  Set the anchor rectangle.
   --  @param Anchor_Rect the new anchor rectangle

private
   Null_Gdk_Popup_Layout : constant Gdk_Popup_Layout :=
      (Glib.C_Boxed with null record);

end Gdk.Popup_Layout;
