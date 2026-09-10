
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
with Glib;
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

   type Gdk_Modifier_Type is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Modifier_Type);
   --  Flags to indicate the state of modifier keys and mouse buttons in
   --  events.
   --
   --  Typical modifier keys are Shift, Control, Meta, Super, Hyper, Alt,
   --  Compose, Apple, CapsLock or ShiftLock.
   --
   --  Note that GDK may add internal values to events which include values
   --  outside of this enumeration. Your code should preserve and ignore them.
   --  You can use GDK_MODIFIER_MASK to remove all private values.

   Gdk_No_Modifier_Mask : constant Gdk_Modifier_Type := 0;
   Gdk_Shift_Mask : constant Gdk_Modifier_Type := 1;
   Gdk_Lock_Mask : constant Gdk_Modifier_Type := 2;
   Gdk_Control_Mask : constant Gdk_Modifier_Type := 4;
   Gdk_Alt_Mask : constant Gdk_Modifier_Type := 8;
   Gdk_Button1_Mask : constant Gdk_Modifier_Type := 256;
   Gdk_Button2_Mask : constant Gdk_Modifier_Type := 512;
   Gdk_Button3_Mask : constant Gdk_Modifier_Type := 1024;
   Gdk_Button4_Mask : constant Gdk_Modifier_Type := 2048;
   Gdk_Button5_Mask : constant Gdk_Modifier_Type := 4096;
   Gdk_Super_Mask : constant Gdk_Modifier_Type := 67108864;
   Gdk_Hyper_Mask : constant Gdk_Modifier_Type := 134217728;
   Gdk_Meta_Mask : constant Gdk_Modifier_Type := 268435456;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Gravity_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Gravity);
   type Property_Gdk_Gravity is new Gdk_Gravity_Properties.Property;

   package Gdk_Anchor_Hints_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Anchor_Hints);
   type Property_Gdk_Anchor_Hints is new Gdk_Anchor_Hints_Properties.Property;

   package Gdk_Modifier_Type_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Modifier_Type);
   type Property_Gdk_Modifier_Type is new Gdk_Modifier_Type_Properties.Property;

   ----------------------
   -- GtkAda additions --
   ----------------------

   Gdk_Modifier_Mask : constant Gdk_Modifier_Type := 16#1C00_1F0F#;
   --  A mask covering all entries in Gdk_Modifier_Type. GDK may set bits
   --  outside of this mask on events, so mask them out before comparing.

   function Gdk_Modifier_Type_Get_Type return Glib.GType;
   pragma Import (C, Gdk_Modifier_Type_Get_Type, "gdk_modifier_type_get_type");
   --  The GType of the Gdk_Modifier_Type flags class, for use with
   --  Glib.Properties.Creation.Flags_Class_From_Type.

end Gdk.Enums;
