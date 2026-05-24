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

--  Provides information to interpret colors and pixels in a variety of ways.
--
--  They are also known as [*color
--  spaces*](https://en.wikipedia.org/wiki/Color_space).
--
--  Crucially, GTK knows how to convert colors from one color state to
--  another.
--
--  `GdkColorState` objects are immutable and therefore threadsafe.

pragma Warnings (Off, "*is already use-visible*");
with Glib;   use Glib;
with System;

package Gdk.Color_State is

   type Gdk_Color_State is new Glib.C_Boxed with null record;
   Null_Gdk_Color_State : constant Gdk_Color_State;

   function From_Object (Object : System.Address) return Gdk_Color_State;
   function From_Object_Free (B : access Gdk_Color_State'Class) return Gdk_Color_State;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_color_state_get_type");

   -------------
   -- Methods --
   -------------

   function Create_Cicp_Params
      (Self : Gdk_Color_State) return Gdk.Gdk_Cicp_Params;
   --  Create a [classGdk.CicpParams] representing the colorstate.
   --  It is not guaranteed that every `GdkColorState` can be represented with
   --  Cicp parameters. If that is the case, this function returns `NULL`.
   --  Since: gtk+ 4.16
   --  @return A new [classGdk.CicpParams]

   function Equal
      (Self  : Gdk_Color_State;
       Other : Gdk_Color_State) return Boolean;
   --  Compares two `GdkColorStates` for equality.
   --  Note that this function is not guaranteed to be perfect and two objects
   --  describing the same color state may compare not equal. However,
   --  different color states will never compare equal.
   --  Since: gtk+ 4.16
   --  @param Other another `GdkColorStatee`
   --  @return True if the two color states compare equal

   function Equivalent
      (Self  : Gdk_Color_State;
       Other : Gdk_Color_State) return Boolean;
   --  Compares two `GdkColorStates` for equivalence.
   --  Two objects that represent the same color state should be equivalent,
   --  even though they may not be equal in the sense of
   --  [methodGdk.ColorState.equal].
   --  Since: gtk+ 4.20
   --  @param Other another `GdkColorStatee`
   --  @return True if the two color states are equivalent

   function Ref (Self : Gdk_Color_State) return Gdk_Color_State;
   --  Increase the reference count of Self.
   --  Since: gtk+ 4.16
   --  @return the object that was passed in

   procedure Unref (Self : Gdk_Color_State);
   --  Decrease the reference count of Self.
   --  Unless Self is static, it will be freed when the reference count
   --  reaches zero.
   --  Since: gtk+ 4.16

   ---------------
   -- Functions --
   ---------------

   function Get_Oklab return Gdk_Color_State;
   --  Returns the color state object representing the oklab color space.
   --  This is a perceptually uniform color state.
   --  Since: gtk+ 4.18
   --  @return the color state object for oklab

   function Get_Oklch return Gdk_Color_State;
   --  Returns the color state object representing the oklch color space.
   --  This is the polar variant of oklab, in which the hue is encoded as a
   --  polar coordinate.
   --  Since: gtk+ 4.18
   --  @return the color state object for oklch

   function Get_Rec2100_Linear return Gdk_Color_State;
   --  Returns the color state object representing the linear rec2100 color
   --  space.
   --  This color state uses the primaries defined by BT.2020-2 and BT.2100-0
   --  and a linear transfer function.
   --  It is equivalent to the [Cicp](class.CicpParams.html) tuple 9/8/0/1.
   --  See e.g. [the CSS HDR
   --  Module](https://drafts.csswg.org/css-color-hdr/valdef-color-rec2100-linear)
   --  for details about this colorstate.
   --  Since: gtk+ 4.16
   --  @return the color state object for linearized rec2100

   function Get_Rec2100_Pq return Gdk_Color_State;
   --  Returns the color state object representing the rec2100-pq color space.
   --  This color state uses the primaries defined by BT.2020-2 and BT.2100-0
   --  and the transfer function defined by SMPTE ST 2084 and BT.2100-2.
   --  It is equivalent to the [Cicp](class.CicpParams.html) tuple 9/16/0/1.
   --  See e.g. [the CSS HDR
   --  Module](https://drafts.csswg.org/css-color-hdr/valdef-color-rec2100-pq)
   --  for details about this colorstate.
   --  Since: gtk+ 4.16
   --  @return the color state object for rec2100-pq

   function Get_Srgb return Gdk_Color_State;
   --  Returns the color state object representing the sRGB color space.
   --  This color state uses the primaries defined by BT.709-6 and the
   --  transfer function defined by IEC 61966-2-1.
   --  It is equivalent to the [Cicp](class.CicpParams.html) tuple 1/13/0/1.
   --  See e.g. [the CSS Color
   --  Module](https://www.w3.org/TR/css-color-4/predefined-sRGB) for details
   --  about this colorstate.
   --  Since: gtk+ 4.16
   --  @return the color state object for sRGB

   function Get_Srgb_Linear return Gdk_Color_State;
   --  Returns the color state object representing the linearized sRGB color
   --  space.
   --  This color state uses the primaries defined by BT.709-6 and a linear
   --  transfer function.
   --  It is equivalent to the [Cicp](class.CicpParams.html) tuple 1/8/0/1.
   --  See e.g. [the CSS Color
   --  Module](https://www.w3.org/TR/css-color-4/predefined-sRGB-linear) for
   --  details about this colorstate.
   --  Since: gtk+ 4.16
   --  @return the color state object for linearized sRGB

private

   Null_Gdk_Color_State : constant Gdk_Color_State := (Glib.C_Boxed with null record);

end Gdk.Color_State;
