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
--  The Gtk.Level_Bar.Gtk_Level_Bar is a bar widget that can be used as a
--  level indicator. Typical use cases are displaying the strength of a
--  password, or showing the charge level of a battery.
--
--  Use Gtk.Level_Bar.Set_Value to set the current value, and
--  Gtk.Level_Bar.Add_Offset_Value to set the value offsets at which the bar
--  will be considered in a different state. GTK will add a few offsets by
--  default on the level bar: GTK_LEVEL_BAR_OFFSET_LOW,
--  GTK_LEVEL_BAR_OFFSET_HIGH and GTK_LEVEL_BAR_OFFSET_FULL, with values 0.25,
--  0.75 and 1.0 respectively.
--
--  Note that it is your responsibility to update preexisting offsets when
--  changing the minimum or maximum value. GTK+ will simply clamp them to the
--  new range.
--
--  ## Adding a custom offset on the bar
--
--  |[<!-- language="C" -->
--
--  static GtkWidget * create_level_bar (void) { GtkWidget *widget;
--  GtkLevelBar *bar;
--
--  widget = gtk_level_bar_new (); bar = GTK_LEVEL_BAR (widget);
--
--  // This changes the value of the default low offset
--
--  gtk_level_bar_add_offset_value (bar, GTK_LEVEL_BAR_OFFSET_LOW, 0.10);
--
--  // This adds a new offset to the bar; the application will // be able to
--  change its color CSS like this: // // levelbar block.my-offset { //
--  background-color: magenta; // border-style: solid; // border-color: black;
--  // border-style: 1px; // }
--
--  gtk_level_bar_add_offset_value (bar, "my-offset", 0.60);
--
--  return widget; } ]|
--
--  The default interval of values is between zero and one, but it's possible
--  to modify the interval using Gtk.Level_Bar.Set_Min_Value and
--  Gtk.Level_Bar.Set_Max_Value. The value will be always drawn in proportion
--  to the admissible interval, i.e. a value of 15 with a specified interval
--  between 10 and 20 is equivalent to a value of 0.5 with an interval between
--  0 and 1. When GTK_LEVEL_BAR_MODE_DISCRETE is used, the bar level is
--  rendered as a finite number of separated blocks instead of a single one.
--  The number of blocks that will be rendered is equal to the number of units
--  specified by the admissible interval.
--
--  For instance, to build a bar rendered with five blocks, it's sufficient to
--  set the minimum value to 0 and the maximum value to 5 after changing the
--  indicator mode to discrete.
--
--  GtkLevelBar was introduced in GTK+ 3.6.
--
--  # GtkLevelBar as GtkBuildable
--
--  The GtkLevelBar implementation of the GtkBuildable interface supports a
--  custom <offsets> element, which can contain any number of <offset>
--  elements, each of which must have name and value attributes.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> levelbar[.discrete] ╰── trough ├──
--  block.filled.level-name ┊ ├── block.empty ┊ ]|
--
--  GtkLevelBar has a main CSS node with name levelbar and one of the style
--  classes .discrete or .continuous and a subnode with name trough. Below the
--  trough node are a number of nodes with name block and style class .filled
--  or .empty. In continuous mode, there is exactly one node of each, in
--  discrete mode, the number of filled and unfilled nodes corresponds to
--  blocks that are drawn. The block.filled nodes also get a style class
--  .level-name corresponding to the level for the current value.
--
--  In horizontal orientation, the nodes are always arranged from left to
--  right, regardless of text direction.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Level_Bar is

   type Gtk_Level_Bar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Level_Bar is access all Gtk_Level_Bar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Level_Bar);
   procedure Initialize (Self : not null access Gtk_Level_Bar_Record'Class);
   --  Creates a new Gtk.Level_Bar.Gtk_Level_Bar.
   --  Since: gtk+ 3.6
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Level_Bar_New return Gtk_Level_Bar;
   --  Creates a new Gtk.Level_Bar.Gtk_Level_Bar.
   --  Since: gtk+ 3.6

   procedure Gtk_New_For_Interval
      (Self      : out Gtk_Level_Bar;
       Min_Value : Gdouble;
       Max_Value : Gdouble);
   procedure Initialize_For_Interval
      (Self      : not null access Gtk_Level_Bar_Record'Class;
       Min_Value : Gdouble;
       Max_Value : Gdouble);
   --  Utility constructor that creates a new Gtk.Level_Bar.Gtk_Level_Bar for
   --  the specified interval.
   --  Since: gtk+ 3.6
   --  Initialize_For_Interval does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "min_value": a positive value
   --  "max_value": a positive value

   function Gtk_Level_Bar_New_For_Interval
      (Min_Value : Gdouble;
       Max_Value : Gdouble) return Gtk_Level_Bar;
   --  Utility constructor that creates a new Gtk.Level_Bar.Gtk_Level_Bar for
   --  the specified interval.
   --  Since: gtk+ 3.6
   --  "min_value": a positive value
   --  "max_value": a positive value

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_level_bar_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Offset_Value
      (Self  : not null access Gtk_Level_Bar_Record;
       Name  : UTF8_String;
       Value : Gdouble);
   --  Adds a new offset marker on Self at the position specified by Value.
   --  When the bar value is in the interval topped by Value (or between Value
   --  and Gtk.Level_Bar.Gtk_Level_Bar:max-value in case the offset is the last
   --  one on the bar) a style class named `level-`Name will be applied when
   --  rendering the level bar fill. If another offset marker named Name
   --  exists, its value will be replaced by Value.
   --  Since: gtk+ 3.6
   --  "name": the name of the new offset
   --  "value": the value for the new offset

   function Get_Inverted
      (Self : not null access Gtk_Level_Bar_Record) return Boolean;
   --  Return the value of the Gtk.Level_Bar.Gtk_Level_Bar:inverted property.
   --  Since: gtk+ 3.8

   procedure Set_Inverted
      (Self     : not null access Gtk_Level_Bar_Record;
       Inverted : Boolean);
   --  Sets the value of the Gtk.Level_Bar.Gtk_Level_Bar:inverted property.
   --  Since: gtk+ 3.8
   --  "inverted": True to invert the level bar

   function Get_Max_Value
      (Self : not null access Gtk_Level_Bar_Record) return Gdouble;
   --  Returns the value of the Gtk.Level_Bar.Gtk_Level_Bar:max-value
   --  property.
   --  Since: gtk+ 3.6

   procedure Set_Max_Value
      (Self  : not null access Gtk_Level_Bar_Record;
       Value : Gdouble);
   --  Sets the value of the Gtk.Level_Bar.Gtk_Level_Bar:max-value property.
   --  You probably want to update preexisting level offsets after calling
   --  this function.
   --  Since: gtk+ 3.6
   --  "value": a positive value

   function Get_Min_Value
      (Self : not null access Gtk_Level_Bar_Record) return Gdouble;
   --  Returns the value of the Gtk.Level_Bar.Gtk_Level_Bar:min-value
   --  property.
   --  Since: gtk+ 3.6

   procedure Set_Min_Value
      (Self  : not null access Gtk_Level_Bar_Record;
       Value : Gdouble);
   --  Sets the value of the Gtk.Level_Bar.Gtk_Level_Bar:min-value property.
   --  You probably want to update preexisting level offsets after calling
   --  this function.
   --  Since: gtk+ 3.6
   --  "value": a positive value

   function Get_Mode
      (Self : not null access Gtk_Level_Bar_Record)
       return Gtk.Enums.Gtk_Level_Bar_Mode;
   --  Returns the value of the Gtk.Level_Bar.Gtk_Level_Bar:mode property.
   --  Since: gtk+ 3.6

   procedure Set_Mode
      (Self : not null access Gtk_Level_Bar_Record;
       Mode : Gtk.Enums.Gtk_Level_Bar_Mode);
   --  Sets the value of the Gtk.Level_Bar.Gtk_Level_Bar:mode property.
   --  Since: gtk+ 3.6
   --  "mode": a Gtk.Enums.Gtk_Level_Bar_Mode

   function Get_Offset_Value
      (Self  : not null access Gtk_Level_Bar_Record;
       Name  : UTF8_String := "";
       Value : access Gdouble) return Boolean;
   --  Fetches the value specified for the offset marker Name in Self,
   --  returning True in case an offset named Name was found.
   --  Since: gtk+ 3.6
   --  "name": the name of an offset in the bar
   --  "value": location where to store the value

   function Get_Value
      (Self : not null access Gtk_Level_Bar_Record) return Gdouble;
   --  Returns the value of the Gtk.Level_Bar.Gtk_Level_Bar:value property.
   --  Since: gtk+ 3.6

   procedure Set_Value
      (Self  : not null access Gtk_Level_Bar_Record;
       Value : Gdouble);
   --  Sets the value of the Gtk.Level_Bar.Gtk_Level_Bar:value property.
   --  Since: gtk+ 3.6
   --  "value": a value in the interval between
   --  Gtk.Level_Bar.Gtk_Level_Bar:min-value and
   --  Gtk.Level_Bar.Gtk_Level_Bar:max-value

   procedure Remove_Offset_Value
      (Self : not null access Gtk_Level_Bar_Record;
       Name : UTF8_String := "");
   --  Removes an offset marker previously added with
   --  Gtk.Level_Bar.Add_Offset_Value.
   --  Since: gtk+ 3.6
   --  "name": the name of an offset in the bar

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Level_Bar_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Level_Bar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Inverted_Property : constant Glib.Properties.Property_Boolean;
   --  Level bars normally grow from top to bottom or left to right. Inverted
   --  level bars grow in the opposite direction.

   Max_Value_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The Gtk.Level_Bar.Gtk_Level_Bar:max-value property determaxes the
   --  maximum value of the interval that can be displayed by the bar.

   Min_Value_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The Gtk.Level_Bar.Gtk_Level_Bar:min-value property determines the
   --  minimum value of the interval that can be displayed by the bar.

   Mode_Property : constant Gtk.Enums.Property_Gtk_Level_Bar_Mode;
   --  The Gtk.Level_Bar.Gtk_Level_Bar:mode property determines the way
   --  Gtk.Level_Bar.Gtk_Level_Bar interprets the value properties to draw the
   --  level fill area. Specifically, when the value is
   --  GTK_LEVEL_BAR_MODE_CONTINUOUS, Gtk.Level_Bar.Gtk_Level_Bar will draw a
   --  single block representing the current value in that area; when the value
   --  is GTK_LEVEL_BAR_MODE_DISCRETE, the widget will draw a succession of
   --  separate blocks filling the draw area, with the number of blocks being
   --  equal to the units separating the integral roundings of
   --  Gtk.Level_Bar.Gtk_Level_Bar:min-value and
   --  Gtk.Level_Bar.Gtk_Level_Bar:max-value.

   Value_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The Gtk.Level_Bar.Gtk_Level_Bar:value property determines the currently
   --  filled value of the level bar.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Level_Bar_UTF8_String_Void is not null access procedure
     (Self : access Gtk_Level_Bar_Record'Class;
      Name : UTF8_String);

   type Cb_GObject_UTF8_String_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Name : UTF8_String);

   Signal_Offset_Changed : constant Glib.Signal_Name := "offset-changed";
   procedure On_Offset_Changed
      (Self  : not null access Gtk_Level_Bar_Record;
       Call  : Cb_Gtk_Level_Bar_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Offset_Changed
      (Self  : not null access Gtk_Level_Bar_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when an offset specified on the bar changes value as an effect
   --  to Gtk.Level_Bar.Add_Offset_Value being called.
   --
   --  The signal supports detailed connections; you can connect to the
   --  detailed signal "changed::x" in order to only receive callbacks when the
   --  value of offset "x" changes.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Level_Bar_Record, Gtk_Level_Bar);
   function "+"
     (Widget : access Gtk_Level_Bar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Level_Bar
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Level_Bar_Record, Gtk_Level_Bar);
   function "+"
     (Widget : access Gtk_Level_Bar_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Level_Bar
   renames Implements_Gtk_Orientable.To_Object;

private
   Value_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("value");
   Mode_Property : constant Gtk.Enums.Property_Gtk_Level_Bar_Mode :=
     Gtk.Enums.Build ("mode");
   Min_Value_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("min-value");
   Max_Value_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("max-value");
   Inverted_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inverted");
end Gtk.Level_Bar;
