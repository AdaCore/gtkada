-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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
--  The Gtk_Curve widget allows the user to edit a curve covering a range of
--  values. It is typically used to fine-tune color balances in graphics
--  applications like the Gimp.
--
--  The Gtk_Curve widget has 3 modes of operation: spline, linear and free.
--  In spline mode the user places points on the curve which are automatically
--  connected together into a smooth curve. In linear mode the user places
--  points on the curve which are connected by straight lines. In free mode the
--  user can draw the points of the curve freely, and they are not connected at
--  all.
--  </description>
--  <c_version>1.3.11</c_version>

with Gtk.Drawing_Area;
with Gtk.Enums; use Gtk.Enums;
with Glib.Generic_Properties; use Glib.Generic_Properties;
pragma Elaborate_All (Glib.Generic_Properties);
with Glib.Properties;

package Gtk.Curve is

   type Gtk_Curve_Record is new
     Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type Gtk_Curve is access all Gtk_Curve_Record'Class;

   procedure Gtk_New (Curve : out Gtk_Curve);
   --  Create a new Curve.

   procedure Initialize (Curve : access Gtk_Curve_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Curve.

   procedure Reset (Curve : access Gtk_Curve_Record);
   --  Reset the curve.
   --  Reset to a straight line from the minimum x & y values to the maximum
   --  x & y values (i.e. from the bottom-left to the top-right corners).
   --  The curve type is not changed.

   procedure Set_Gamma (Curve : access Gtk_Curve_Record; Gamma : Gfloat);
   --  Recompute the entire curve using the given gamma value.
   --  A gamma value of 1.0 results in a straight line. Values greater than 1.0
   --  result in a curve above the straight line. Values less than 1.0 result
   --  in a curve below the straight line. The curve type is changed to
   --  Curve_Type_Free.

   procedure Set_Range
     (Curve : access Gtk_Curve_Record;
      Min_X : Gfloat;
      Max_X : Gfloat;
      Min_Y : Gfloat;
      Max_Y : Gfloat);
   --  Set the minimum and maximum x & y values of the curve.
   --  The curve is also reset with a call to Reset.

   procedure Set_Vector
     (Curve  : access Gtk_Curve_Record;
      Vector : Gfloat_Array);
   --  Set the vector of points on the curve.
   --  The curve type is set to Curve_Type_Free.

   procedure Get_Vector
     (Curve  : access Gtk_Curve_Record;
      Vector : out Gfloat_Array);
   --  Return a vector of points representing the curve.

   procedure Set_Curve_Type
     (Curve      : access Gtk_Curve_Record;
      Curve_Type : Gtk_Curve_Type);
   --  Set the type of the curve.
   --  The curve will remain unchanged except when changing from a free curve
   --  to a linear or spline curve, in which case the curve will be changed as
   --  little as possible.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Curve_Type_Property
   --    Type:  Gtk_Curve_Type
   --    Flags: read-write
   --    Descr: Is this curve linear, spline interpolated, or free-form
   --    See also: Set_Curve_Type
   --
   --  - Name:  Min_X_Property
   --    Type:  Gfloat
   --    Flags: read-write
   --    Descr: Minimum possible value for X
   --    See also: Set_Range
   --
   --  - Name:  Min_Y_Property
   --    Type:  Gfloat
   --    Flags: read-write
   --    Descr: Minimum possible value for Y
   --    See also: Set_Range
   --
   --  - Name:  Max_X_Property
   --    Type:  Gfloat
   --    Flags: read-write
   --    Descr: Maximum possible value for X
   --    See also: Set_Range
   --
   --  - Name:  Max_Y_Property
   --    Type:  Gfloat
   --    Flags: read-write
   --    Descr: Maximum possible value for Y
   --    See also: Set_Range
   --
   --  </properties>

   package Curve_Type_Properties is new Generic_Internal_Discrete_Property
     (Gtk_Curve_Type);
   type Property_Gtk_Curve_Type   is new Curve_Type_Properties.Property;

   Curve_Type_Property : constant Property_Gtk_Curve_Type;
   Min_X_Property      : constant Glib.Properties.Property_Float;
   Max_X_Property      : constant Glib.Properties.Property_Float;
   Min_Y_Property      : constant Glib.Properties.Property_Float;
   Max_Y_Property      : constant Glib.Properties.Property_Float;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --   - "curve-type-changed"
   --     procedure Handler (Curve : access Gtk_Curve_Record'Class);
   --
   --  Emitted when the curve type has been changed. The curve type can be
   --  changed explicitly with a call to Set_Curve_Type. It is also changed as
   --  a side-effect of calling Reset or Set_Gamma.
   --  </signals>

private
   type Gtk_Curve_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
     with null record;

   Curve_Type_Property : constant Property_Gtk_Curve_Type :=
     Build ("curve_type");
   Min_X_Property      : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("min_x");
   Max_X_Property      : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("max_x");
   Min_Y_Property      : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("min_y");
   Max_Y_Property      : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("max_y");

   pragma Import (C, Get_Type, "gtk_curve_get_type");
end Gtk.Curve;
