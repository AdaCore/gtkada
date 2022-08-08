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
--  The Gtk.Adjustment.Gtk_Adjustment object represents a value which has an
--  associated lower and upper bound, together with step and page increments,
--  and a page size. It is used within several GTK+ widgets, including
--  Gtk.Spin_Button.Gtk_Spin_Button, Gtk.Viewport.Gtk_Viewport, and
--  Gtk.GRange.Gtk_Range (which is a base class for Gtk.Scrollbar.Gtk_Scrollbar
--  and Gtk.Scale.Gtk_Scale).
--
--  The Gtk.Adjustment.Gtk_Adjustment object does not update the value itself.
--  Instead it is left up to the owner of the Gtk.Adjustment.Gtk_Adjustment to
--  control the value.
--
--  </description>
--  <description>
--  The meaning of the most important fields can be explained on the following
--  figure (imagine this is a scrollbar):
--
--  [-------|=================|-------------------]
--
--  lower value value + page_size upper
--
--  </description>
--  <group>Scrolling</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gtk.Adjustment is

   type Gtk_Adjustment_Record is new GObject_Record with null record;
   type Gtk_Adjustment is access all Gtk_Adjustment_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Adjustment     : out Gtk_Adjustment;
       Value          : Gdouble;
       Lower          : Gdouble;
       Upper          : Gdouble;
       Step_Increment : Gdouble;
       Page_Increment : Gdouble;
       Page_Size      : Gdouble := 0.0);
   procedure Initialize
      (Adjustment     : not null access Gtk_Adjustment_Record'Class;
       Value          : Gdouble;
       Lower          : Gdouble;
       Upper          : Gdouble;
       Step_Increment : Gdouble;
       Page_Increment : Gdouble;
       Page_Size      : Gdouble := 0.0);
   --  Create a new adjustment. Value is the initial value of the adjustment.
   --  It must be in the range (Lower .. Upper) and the adjustment's value will
   --  never be outside this range. Step_Increment is the value used to make
   --  minor adjustments, such as when the user clicks on the arrows of a
   --  scrollbar. Page_Increment is used to make major adjustments, such as
   --  when the user clicks in the through on a scrollbar. Page_Size is
   --  deprecated, use the default value.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "value": the initial value
   --  "lower": the minimum value
   --  "upper": the maximum value
   --  "step_increment": the step increment
   --  "page_increment": the page increment
   --  "page_size": the page size

   function Gtk_Adjustment_New
      (Value          : Gdouble;
       Lower          : Gdouble;
       Upper          : Gdouble;
       Step_Increment : Gdouble;
       Page_Increment : Gdouble;
       Page_Size      : Gdouble := 0.0) return Gtk_Adjustment;
   --  Create a new adjustment. Value is the initial value of the adjustment.
   --  It must be in the range (Lower .. Upper) and the adjustment's value will
   --  never be outside this range. Step_Increment is the value used to make
   --  minor adjustments, such as when the user clicks on the arrows of a
   --  scrollbar. Page_Increment is used to make major adjustments, such as
   --  when the user clicks in the through on a scrollbar. Page_Size is
   --  deprecated, use the default value.
   --  "value": the initial value
   --  "lower": the minimum value
   --  "upper": the maximum value
   --  "step_increment": the step increment
   --  "page_increment": the page increment
   --  "page_size": the page size

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_adjustment_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed (Adjustment : not null access Gtk_Adjustment_Record);
   pragma Obsolescent (Changed);
   --  Emits a Gtk.Adjustment.Gtk_Adjustment::changed signal from the
   --  Gtk.Adjustment.Gtk_Adjustment. This is typically called by the owner of
   --  the Gtk.Adjustment.Gtk_Adjustment after it has changed any of the
   --  Gtk.Adjustment.Gtk_Adjustment properties other than the value.
   --  Deprecated since 3.18, 1

   procedure Clamp_Page
      (Adjustment : not null access Gtk_Adjustment_Record;
       Lower      : Gdouble;
       Upper      : Gdouble);
   --  Update the Adjustment value to ensure that the range between Lower and
   --  Upper is in the current page (i.e. between value and value + page_size).
   --  If the range is larger than the page size, then only the start of it
   --  will be in the current page. A "value_changed" signal will be emitted if
   --  the value is changed.
   --  "lower": the lower value
   --  "upper": the upper value

   procedure Configure
      (Adjustment     : not null access Gtk_Adjustment_Record;
       Value          : Gdouble;
       Lower          : Gdouble;
       Upper          : Gdouble;
       Step_Increment : Gdouble;
       Page_Increment : Gdouble;
       Page_Size      : Gdouble);
   --  Sets all properties of the adjustment at once.
   --  Use this function to avoid multiple emissions of the
   --  Gtk.Adjustment.Gtk_Adjustment::changed signal. See
   --  Gtk.Adjustment.Set_Lower for an alternative way of compressing multiple
   --  emissions of Gtk.Adjustment.Gtk_Adjustment::changed into one.
   --  Since: gtk+ 2.14
   --  "value": the new value
   --  "lower": the new minimum value
   --  "upper": the new maximum value
   --  "step_increment": the new step increment
   --  "page_increment": the new page increment
   --  "page_size": the new page size

   function Get_Lower
      (Adjustment : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Retrieves the minimum value of the adjustment.
   --  Since: gtk+ 2.14

   procedure Set_Lower
      (Adjustment : not null access Gtk_Adjustment_Record;
       Lower      : Gdouble);
   --  Sets the minimum value of the adjustment.
   --  When setting multiple adjustment properties via their individual
   --  setters, multiple Gtk.Adjustment.Gtk_Adjustment::changed signals will be
   --  emitted. However, since the emission of the
   --  Gtk.Adjustment.Gtk_Adjustment::changed signal is tied to the emission of
   --  the Glib.Object.GObject::notify signals of the changed properties, it's
   --  possible to compress the Gtk.Adjustment.Gtk_Adjustment::changed signals
   --  into one by calling g_object_freeze_notify and g_object_thaw_notify
   --  around the calls to the individual setters.
   --  Alternatively, using a single g_object_set for all the properties to
   --  change, or using Gtk.Adjustment.Configure has the same effect of
   --  compressing Gtk.Adjustment.Gtk_Adjustment::changed emissions.
   --  Since: gtk+ 2.14
   --  "lower": the new minimum value

   function Get_Minimum_Increment
      (Adjustment : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Gets the smaller of step increment and page increment.
   --  Since: gtk+ 3.2

   function Get_Page_Increment
      (Adjustment : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Retrieves the page increment of the adjustment.
   --  Since: gtk+ 2.14

   procedure Set_Page_Increment
      (Adjustment     : not null access Gtk_Adjustment_Record;
       Page_Increment : Gdouble);
   --  Sets the page increment of the adjustment.
   --  See Gtk.Adjustment.Set_Lower about how to compress multiple emissions
   --  of the Gtk.Adjustment.Gtk_Adjustment::changed signal when setting
   --  multiple adjustment properties.
   --  Since: gtk+ 2.14
   --  "page_increment": the new page increment

   function Get_Page_Size
      (Adjustment : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Retrieves the page size of the adjustment.
   --  Since: gtk+ 2.14

   procedure Set_Page_Size
      (Adjustment : not null access Gtk_Adjustment_Record;
       Page_Size  : Gdouble);
   --  Sets the page size of the adjustment.
   --  See Gtk.Adjustment.Set_Lower about how to compress multiple emissions
   --  of the GtkAdjustment::changed signal when setting multiple adjustment
   --  properties.
   --  Since: gtk+ 2.14
   --  "page_size": the new page size

   function Get_Step_Increment
      (Adjustment : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Retrieves the step increment of the adjustment.
   --  Since: gtk+ 2.14

   procedure Set_Step_Increment
      (Adjustment     : not null access Gtk_Adjustment_Record;
       Step_Increment : Gdouble);
   --  Sets the step increment of the adjustment.
   --  See Gtk.Adjustment.Set_Lower about how to compress multiple emissions
   --  of the Gtk.Adjustment.Gtk_Adjustment::changed signal when setting
   --  multiple adjustment properties.
   --  Since: gtk+ 2.14
   --  "step_increment": the new step increment

   function Get_Upper
      (Adjustment : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Retrieves the maximum value of the adjustment.
   --  Since: gtk+ 2.14

   procedure Set_Upper
      (Adjustment : not null access Gtk_Adjustment_Record;
       Upper      : Gdouble);
   --  Sets the maximum value of the adjustment.
   --  Note that values will be restricted by `upper - page-size` if the
   --  page-size property is nonzero.
   --  See Gtk.Adjustment.Set_Lower about how to compress multiple emissions
   --  of the Gtk.Adjustment.Gtk_Adjustment::changed signal when setting
   --  multiple adjustment properties.
   --  Since: gtk+ 2.14
   --  "upper": the new maximum value

   function Get_Value
      (Adjustment : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Gets the current value of the adjustment. See Gtk.Adjustment.Set_Value.

   procedure Set_Value
      (Adjustment : not null access Gtk_Adjustment_Record;
       Value      : Gdouble);
   --  Sets the Gtk.Adjustment.Gtk_Adjustment value. The value is clamped to
   --  lie between Gtk.Adjustment.Gtk_Adjustment:lower and
   --  Gtk.Adjustment.Gtk_Adjustment:upper.
   --  Note that for adjustments which are used in a
   --  Gtk.Scrollbar.Gtk_Scrollbar, the effective range of allowed values goes
   --  from Gtk.Adjustment.Gtk_Adjustment:lower to
   --  Gtk.Adjustment.Gtk_Adjustment:upper -
   --  Gtk.Adjustment.Gtk_Adjustment:page-size.
   --  "value": the new value

   procedure Value_Changed
      (Adjustment : not null access Gtk_Adjustment_Record);
   pragma Obsolescent (Value_Changed);
   --  Emits a Gtk.Adjustment.Gtk_Adjustment::value-changed signal from the
   --  Gtk.Adjustment.Gtk_Adjustment. This is typically called by the owner of
   --  the Gtk.Adjustment.Gtk_Adjustment after it has changed the
   --  Gtk.Adjustment.Gtk_Adjustment:value property.
   --  Deprecated since 3.18, 1

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Lower_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The minimum value of the adjustment.

   Page_Increment_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The page increment of the adjustment.

   Page_Size_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The page size of the adjustment. Note that the page-size is irrelevant
   --  and should be set to zero if the adjustment is used for a simple scalar
   --  value, e.g. in a Gtk.Spin_Button.Gtk_Spin_Button.

   Step_Increment_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The step increment of the adjustment.

   Upper_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The maximum value of the adjustment. Note that values will be
   --  restricted by `upper - page-size` if the page-size property is nonzero.

   Value_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The value of the adjustment.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Adjustment_Void is not null access procedure (Self : access Gtk_Adjustment_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Adjustment_Record;
       Call  : Cb_Gtk_Adjustment_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Adjustment_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when one or more of the Gtk.Adjustment.Gtk_Adjustment
   --  properties have been changed, other than the
   --  Gtk.Adjustment.Gtk_Adjustment:value property.

   Signal_Value_Changed : constant Glib.Signal_Name := "value-changed";
   procedure On_Value_Changed
      (Self  : not null access Gtk_Adjustment_Record;
       Call  : Cb_Gtk_Adjustment_Void;
       After : Boolean := False);
   procedure On_Value_Changed
      (Self  : not null access Gtk_Adjustment_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the Gtk.Adjustment.Gtk_Adjustment:value property has been
   --  changed.

private
   Value_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("value");
   Upper_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("upper");
   Step_Increment_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("step-increment");
   Page_Size_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("page-size");
   Page_Increment_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("page-increment");
   Lower_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("lower");
end Gtk.Adjustment;
