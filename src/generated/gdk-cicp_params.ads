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

--  Contains the parameters that define a colorstate with cicp parameters.
--
--  Cicp parameters are specified in the ITU-T H.273
--  [specification](https://www.itu.int/rec/T-REC-H.273/en).
--
--  See the documentation of individual properties for supported values.
--
--  The 'unspecified' value (2) is not treated in any special way, and must be
--  replaced by a different value before creating a color state.
--
--  `GdkCicpParams` can be used as a builder object to construct a color state
--  from Cicp data with [methodGdk.CicpParams.build_color_state]. The function
--  will return an error if the given parameters are not supported.
--
--  You can obtain a `GdkCicpParams` object from a color state with
--  [methodGdk.ColorState.create_cicp_params]. This can be used to create a
--  variant of a color state, by changing just one of the cicp parameters, or
--  just to obtain information about the color state.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;

package Gdk.Cicp_Params is

   type Gdk_Cicp_Params_Record is new GObject_Record with null record;
   subtype Gdk_Cicp_Params is Gdk.Gdk_Cicp_Params;

   type Gdk_Cicp_Range is (
      Gdk_Cicp_Range_Narrow,
      Gdk_Cicp_Range_Full);
   pragma Convention (C, Gdk_Cicp_Range);
   --  The values of this enumeration describe whether image data uses the
   --  full range of 8-bit values.
   --
   --  In digital broadcasting, it is common to reserve the lowest and highest
   --  values. Typically the allowed values for the narrow range are 16-235 for
   --  Y and 16-240 for u,v (when dealing with YUV data).

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Cicp_Range_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Cicp_Range);
   type Property_Gdk_Cicp_Range is new Gdk_Cicp_Range_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New (Self : out Gdk_Cicp_Params);
   --  Creates a new `GdkCicpParams` object.
   --  The initial values of the properties are the values for "undefined" and
   --  need to be set before a color state object can be built.
   --  Since: gtk+ 4.16

   procedure Initialize
      (Self : not null access Gdk_Cicp_Params_Record'Class);
   --  Creates a new `GdkCicpParams` object.
   --  The initial values of the properties are the values for "undefined" and
   --  need to be set before a color state object can be built.
   --  Since: gtk+ 4.16
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gdk_Cicp_Params_New return Gdk_Cicp_Params;
   --  Creates a new `GdkCicpParams` object.
   --  The initial values of the properties are the values for "undefined" and
   --  need to be set before a color state object can be built.
   --  Since: gtk+ 4.16

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_cicp_params_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Color_Primaries
      (Self : not null access Gdk_Cicp_Params_Record) return Guint;
   --  Returns the value of the color-primaries property of Self.
   --  Since: gtk+ 4.16
   --  @return the color-primaries value

   procedure Set_Color_Primaries
      (Self            : not null access Gdk_Cicp_Params_Record;
       Color_Primaries : Guint);
   --  Sets the color-primaries property of Self.
   --  Since: gtk+ 4.16
   --  @param Color_Primaries the new color primaries value

   function Get_Matrix_Coefficients
      (Self : not null access Gdk_Cicp_Params_Record) return Guint;
   --  Gets the matrix-coefficients property of Self.
   --  Since: gtk+ 4.16
   --  @return the matrix-coefficients value

   procedure Set_Matrix_Coefficients
      (Self                : not null access Gdk_Cicp_Params_Record;
       Matrix_Coefficients : Guint);
   --  Self a `GdkCicpParams` Sets the matrix-coefficients property of Self.
   --  Since: gtk+ 4.16
   --  @param Matrix_Coefficients the new matrix-coefficients value

   function Get_Range
      (Self : not null access Gdk_Cicp_Params_Record) return Gdk_Cicp_Range;
   --  Gets the range property of Self.
   --  Since: gtk+ 4.16
   --  @return the range value

   procedure Set_Range
      (Self   : not null access Gdk_Cicp_Params_Record;
       GRange : Gdk_Cicp_Range);
   --  Sets the range property of Self
   --  Since: gtk+ 4.16
   --  @param GRange the range value

   function Get_Transfer_Function
      (Self : not null access Gdk_Cicp_Params_Record) return Guint;
   --  Gets the transfer-function property of Self.
   --  Since: gtk+ 4.16
   --  @return the transfer-function value

   procedure Set_Transfer_Function
      (Self              : not null access Gdk_Cicp_Params_Record;
       Transfer_Function : Guint);
   --  Sets the transfer-function property of Self.
   --  Since: gtk+ 4.16
   --  @param Transfer_Function the new transfer-function value

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Color_Primaries_Property : constant Glib.Properties.Property_Uint;
   --  The color primaries to use.
   --
   --  Supported values:
   --
   --  - 1: BT.709 / sRGB - 2: unspecified - 5: PAL - 6,7: BT.601 / NTSC - 9:
   --  BT.2020 - 12: Display P3

   GRange_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Cicp_Range
   --  Whether the data is using the full range of values.
   --
   --  The range of the data.

   Matrix_Coefficients_Property : constant Glib.Properties.Property_Uint;
   --  The matrix coefficients (for YUV to RGB conversion).
   --
   --  Supported values:
   --
   --  - 0: RGB - 1: BT.709 - 2: unspecified - 5,6: BT.601 - 9: BT.2020

   Transfer_Function_Property : constant Glib.Properties.Property_Uint;
   --  The transfer function to use.
   --
   --  Supported values:
   --
   --  - 1,6,14,15: BT.709, BT.601, BT.2020 - 2: unspecified - 4: gamma 2.2 -
   --  5: gamma 2.8 - 8: linear - 13: sRGB - 16: BT.2100 PQ - 18: BT.2100 HLG

private
   Transfer_Function_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("transfer-function");
   Matrix_Coefficients_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("matrix-coefficients");
   GRange_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("range");
   Color_Primaries_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("color-primaries");
end Gdk.Cicp_Params;
