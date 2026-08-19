
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


pragma Warnings (Off, "*is already use-visible*");
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Gdk.Enums is

   type Gdk_Gravity is (
      Gdk_Gravity_North_West,
      Gdk_Gravity_North,
      Gdk_Gravity_North_East,
      Gdk_Gravity_West,
      Gdk_Gravity_Center,
      Gdk_Gravity_East,
      Gdk_Gravity_South_West,
      Gdk_Gravity_South,
      Gdk_Gravity_South_East,
      Gdk_Gravity_Static);
   pragma Convention (C, Gdk_Gravity);
   --  Defines the reference point of a surface and is used in
   --  `GdkPopupLayout`.

   for Gdk_Gravity use (
      Gdk_Gravity_North_West => 1,
      Gdk_Gravity_North => 2,
      Gdk_Gravity_North_East => 3,
      Gdk_Gravity_West => 4,
      Gdk_Gravity_Center => 5,
      Gdk_Gravity_East => 6,
      Gdk_Gravity_South_West => 7,
      Gdk_Gravity_South => 8,
      Gdk_Gravity_South_East => 9,
      Gdk_Gravity_Static => 10);

   type Gdk_Anchor_Hints is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Anchor_Hints);
   --  Positioning hints for aligning a surface relative to a rectangle.
   --
   --  These hints determine how the surface should be positioned in the case
   --  that the surface would fall off-screen if placed in its ideal position.
   --
   --  For example, Gdk.Enums.Gdk_Anchor_Flip_X will replace
   --  Gdk.Enums.Gdk_Gravity_North_West with Gdk.Enums.Gdk_Gravity_North_East
   --  and vice versa if the surface extends beyond the left or right edges of
   --  the monitor.
   --
   --  If Gdk.Enums.Gdk_Anchor_Slide_X is set, the surface can be shifted
   --  horizontally to fit on-screen. If Gdk.Enums.Gdk_Anchor_Resize_X is set,
   --  the surface can be shrunken horizontally to fit.
   --
   --  In general, when multiple flags are set, flipping should take
   --  precedence over sliding, which should take precedence over resizing.

   Gdk_Anchor_Flip_X : constant Gdk_Anchor_Hints := 1;
   Gdk_Anchor_Flip_Y : constant Gdk_Anchor_Hints := 2;
   Gdk_Anchor_Slide_X : constant Gdk_Anchor_Hints := 4;
   Gdk_Anchor_Slide_Y : constant Gdk_Anchor_Hints := 8;
   Gdk_Anchor_Resize_X : constant Gdk_Anchor_Hints := 16;
   Gdk_Anchor_Resize_Y : constant Gdk_Anchor_Hints := 32;
   Gdk_Anchor_Flip : constant Gdk_Anchor_Hints := 3;
   Gdk_Anchor_Slide : constant Gdk_Anchor_Hints := 12;
   Gdk_Anchor_Resize : constant Gdk_Anchor_Hints := 48;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Gravity_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Gravity);
   type Property_Gdk_Gravity is new Gdk_Gravity_Properties.Property;

   package Gdk_Anchor_Hints_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Anchor_Hints);
   type Property_Gdk_Anchor_Hints is new Gdk_Anchor_Hints_Properties.Property;

end Gdk.Enums;
