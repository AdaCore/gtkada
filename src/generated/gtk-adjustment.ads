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

--  A model for a numeric value.
--
--  The `GtkAdjustment` has an associated lower and upper bound. It also
--  contains step and page increments, and a page size.
--
--  Adjustments are used within several GTK widgets, including
--  [classGtk.SpinButton], [classGtk.Viewport], [classGtk.Scrollbar] and
--  [classGtk.Scale].
--
--  The `GtkAdjustment` object does not update the value itself. Instead it is
--  left up to the owner of the `GtkAdjustment` to control the value.
--
--  The meaning of the most important fields can be explained on the following
--  figure (imagine this is a scrollbar):
--
--  [-------|=================|-------------------]
--
--  lower value value + page_size upper
--
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
      (Self           : out Gtk_Adjustment;
       Value          : Gdouble;
       Lower          : Gdouble;
       Upper          : Gdouble;
       Step_Increment : Gdouble;
       Page_Increment : Gdouble;
       Page_Size      : Gdouble := 0.0);
   procedure Initialize
      (Self           : not null access Gtk_Adjustment_Record'Class;
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
   --  when the user clicks in the through on a scrollbar.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Value the initial value
   --  @param Lower the minimum value
   --  @param Upper the maximum value
   --  @param Step_Increment the step increment
   --  @param Page_Increment the page increment
   --  @param Page_Size the page size

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
   --  when the user clicks in the through on a scrollbar.
   --  @param Value the initial value
   --  @param Lower the minimum value
   --  @param Upper the maximum value
   --  @param Step_Increment the step increment
   --  @param Page_Increment the page increment
   --  @param Page_Size the page size

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_adjustment_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clamp_Page
      (Self  : not null access Gtk_Adjustment_Record;
       Lower : Gdouble;
       Upper : Gdouble);
   --  Updates the value of the adjustment to ensure that the given range is
   --  contained in the current page.
   --  The current page goes from `value` to `value` + `page-size`. If the
   --  range is larger than the page size, then only the start of it will be in
   --  the current page.
   --  A [signalGtk.Adjustment::value-changed] signal will be emitted if the
   --  value is changed.
   --  @param Lower the lower value
   --  @param Upper the upper value

   procedure Configure
      (Self           : not null access Gtk_Adjustment_Record;
       Value          : Gdouble;
       Lower          : Gdouble;
       Upper          : Gdouble;
       Step_Increment : Gdouble;
       Page_Increment : Gdouble;
       Page_Size      : Gdouble);
   --  Sets all properties of the adjustment at once.
   --  Use this function to avoid multiple emissions of the
   --  [signalGtk.Adjustment::changed] signal. See
   --  [methodGtk.Adjustment.set_lower] for an alternative way of compressing
   --  multiple emissions of [signalGtk.Adjustment::changed] into one.
   --  @param Value the new value
   --  @param Lower the new minimum value
   --  @param Upper the new maximum value
   --  @param Step_Increment the new step increment
   --  @param Page_Increment the new page increment
   --  @param Page_Size the new page size

   function Get_Lower
      (Self : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Retrieves the minimum value of the adjustment.
   --  @return the minimum value

   procedure Set_Lower
      (Self  : not null access Gtk_Adjustment_Record;
       Lower : Gdouble);
   --  Sets the minimum value of the adjustment.
   --  When setting multiple adjustment properties via their individual
   --  setters, multiple [signalGtk.Adjustment::changed] signals will be
   --  emitted. However, since the emission of the
   --  [signalGtk.Adjustment::changed] signal is tied to the emission of the
   --  ::notify signals of the changed properties, it's possible to compress
   --  the [signalGtk.Adjustment::changed] signals into one by calling
   --  g_object_freeze_notify and g_object_thaw_notify around the calls to the
   --  individual setters.
   --  Alternatively, using a single g_object_set for all the properties to
   --  change, or using [methodGtk.Adjustment.configure] has the same effect.
   --  @param Lower the new minimum value

   function Get_Minimum_Increment
      (Self : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Gets the smaller of step increment and page increment.
   --  @return the minimum increment

   function Get_Page_Increment
      (Self : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Retrieves the page increment of the adjustment.
   --  @return the page increment

   procedure Set_Page_Increment
      (Self           : not null access Gtk_Adjustment_Record;
       Page_Increment : Gdouble);
   --  Sets the page increment of the adjustment.
   --  See [methodGtk.Adjustment.set_lower] about how to compress multiple
   --  emissions of the [signalGtk.Adjustment::changed] signal when setting
   --  multiple adjustment properties.
   --  @param Page_Increment the new page increment

   function Get_Page_Size
      (Self : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Retrieves the page size of the adjustment.
   --  @return the page size

   procedure Set_Page_Size
      (Self      : not null access Gtk_Adjustment_Record;
       Page_Size : Gdouble);
   --  Sets the page size of the adjustment.
   --  See [methodGtk.Adjustment.set_lower] about how to compress multiple
   --  emissions of the [signalGtk.Adjustment::changed] signal when setting
   --  multiple adjustment properties.
   --  @param Page_Size the new page size

   function Get_Step_Increment
      (Self : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Retrieves the step increment of the adjustment.
   --  @return the step increment

   procedure Set_Step_Increment
      (Self           : not null access Gtk_Adjustment_Record;
       Step_Increment : Gdouble);
   --  Sets the step increment of the adjustment.
   --  See [methodGtk.Adjustment.set_lower] about how to compress multiple
   --  emissions of the [signalGtk.Adjustment::changed] signal when setting
   --  multiple adjustment properties.
   --  @param Step_Increment the new step increment

   function Get_Upper
      (Self : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Retrieves the maximum value of the adjustment.
   --  @return the maximum value

   procedure Set_Upper
      (Self  : not null access Gtk_Adjustment_Record;
       Upper : Gdouble);
   --  Sets the maximum value of the adjustment.
   --  Note that values will be restricted by `upper - page-size` if the
   --  page-size property is nonzero.
   --  See [methodGtk.Adjustment.set_lower] about how to compress multiple
   --  emissions of the [signalGtk.Adjustment::changed] signal when setting
   --  multiple adjustment properties.
   --  @param Upper the new maximum value

   function Get_Value
      (Self : not null access Gtk_Adjustment_Record) return Gdouble;
   --  Gets the current value of the adjustment.
   --  @return the current value

   procedure Set_Value
      (Self  : not null access Gtk_Adjustment_Record;
       Value : Gdouble);
   --  Sets the `GtkAdjustment` value.
   --  The value is clamped to lie between [propertyGtk.Adjustment:lower] and
   --  [propertyGtk.Adjustment:upper].
   --  Note that for adjustments which are used in a `GtkScrollbar`, the
   --  effective range of allowed values goes from
   --  [propertyGtk.Adjustment:lower] to [propertyGtk.Adjustment:upper] -
   --  [propertyGtk.Adjustment:page-size].
   --  @param Value the new value

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
   --  The page size of the adjustment.
   --
   --  Note that the page-size is irrelevant and should be set to zero if the
   --  adjustment is used for a simple scalar value, e.g. in a `GtkSpinButton`.

   Step_Increment_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The step increment of the adjustment.

   Upper_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The maximum value of the adjustment.
   --
   --  Note that values will be restricted by `upper - page-size` if the
   --  page-size property is nonzero.

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
   --  Emitted when one or more of the `GtkAdjustment` properties have been
   --  changed.
   --
   --  Note that the [propertyGtk.Adjustment:value] property is covered by the
   --  [signalGtk.Adjustment::value-changed] signal.

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
   --  Emitted when the value has been changed.

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
