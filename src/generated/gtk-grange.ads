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
--  Gtk.GRange.Gtk_Range is the common base class for widgets which visualize
--  an adjustment, e.g Gtk.Scale.Gtk_Scale or Gtk.Scrollbar.Gtk_Scrollbar.
--
--  Apart from signals for monitoring the parameters of the adjustment,
--  Gtk.GRange.Gtk_Range provides properties and methods for influencing the
--  sensitivity of the "steppers". It also provides properties and methods for
--  setting a "fill level" on range widgets. See Gtk.GRange.Set_Fill_Level.
--
--  </description>
--  <screenshot>gtk-range</screenshot>
--  <testgtk>create_range.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Rectangle;   use Gdk.Rectangle;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.GRange is

   type Gtk_Range_Record is new Gtk_Widget_Record with null record;
   type Gtk_Range is access all Gtk_Range_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_range_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Adjustment
      (The_Range : not null access Gtk_Range_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Get the Gtk.Adjustment.Gtk_Adjustment which is the "model" object for
   --  Gtk.GRange.Gtk_Range. See Gtk.GRange.Set_Adjustment for details. The
   --  return value does not have a reference added, so should not be
   --  unreferenced.

   procedure Set_Adjustment
      (The_Range  : not null access Gtk_Range_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the adjustment to be used as the "model" object for this range
   --  widget. The adjustment indicates the current range value, the minimum
   --  and maximum range values, the step/page increments used for keybindings
   --  and scrolling, and the page size. The page size is normally 0 for
   --  Gtk.Scale.Gtk_Scale and nonzero for Gtk.Scrollbar.Gtk_Scrollbar, and
   --  indicates the size of the visible area of the widget being scrolled. The
   --  page size affects the size of the scrollbar slider.
   --  "adjustment": a Gtk.Adjustment.Gtk_Adjustment

   function Get_Fill_Level
      (The_Range : not null access Gtk_Range_Record) return Gdouble;
   --  Gets the current position of the fill level indicator.
   --  Since: gtk+ 2.12

   procedure Set_Fill_Level
      (The_Range  : not null access Gtk_Range_Record;
       Fill_Level : Gdouble);
   --  Set the new position of the fill level indicator.
   --  The "fill level" is probably best described by its most prominent use
   --  case, which is an indicator for the amount of pre-buffering in a
   --  streaming media player. In that use case, the value of the range would
   --  indicate the current play position, and the fill level would be the
   --  position up to which the file/stream has been downloaded.
   --  This amount of prebuffering can be displayed on the range's trough and
   --  is themeable separately from the trough. To enable fill level display,
   --  use Gtk.GRange.Set_Show_Fill_Level. The range defaults to not showing
   --  the fill level.
   --  Additionally, it's possible to restrict the range's slider position to
   --  values which are smaller than the fill level. This is controller by
   --  Gtk.GRange.Set_Restrict_To_Fill_Level and is by default enabled.
   --  Since: gtk+ 2.12
   --  "fill_level": the new position of the fill level indicator

   function Get_Flippable
      (The_Range : not null access Gtk_Range_Record) return Boolean;
   --  Gets the value set by Gtk.GRange.Set_Flippable.
   --  Since: gtk+ 2.18

   procedure Set_Flippable
      (The_Range : not null access Gtk_Range_Record;
       Flippable : Boolean);
   --  If a range is flippable, it will switch its direction if it is
   --  horizontal and its direction is Gtk.Enums.Text_Dir_Rtl.
   --  See Gtk.Widget.Get_Direction.
   --  Since: gtk+ 2.18
   --  "flippable": True to make the range flippable

   function Get_Inverted
      (The_Range : not null access Gtk_Range_Record) return Boolean;
   --  Gets the value set by Gtk.GRange.Set_Inverted.

   procedure Set_Inverted
      (The_Range : not null access Gtk_Range_Record;
       Setting   : Boolean);
   --  Ranges normally move from lower to higher values as the slider moves
   --  from top to bottom or left to right. Inverted ranges have higher values
   --  at the top or on the right rather than on the bottom or left.
   --  "setting": True to invert the range

   function Get_Lower_Stepper_Sensitivity
      (The_Range : not null access Gtk_Range_Record)
       return Gtk.Enums.Gtk_Sensitivity_Type;
   --  Gets the sensitivity policy for the stepper that points to the 'lower'
   --  end of the GtkRange's adjustment.
   --  Since: gtk+ 2.10

   procedure Set_Lower_Stepper_Sensitivity
      (The_Range   : not null access Gtk_Range_Record;
       Sensitivity : Gtk.Enums.Gtk_Sensitivity_Type);
   --  Sets the sensitivity policy for the stepper that points to the 'lower'
   --  end of the GtkRange's adjustment.
   --  Since: gtk+ 2.10
   --  "sensitivity": the lower stepper's sensitivity policy.

   function Get_Min_Slider_Size
      (The_Range : not null access Gtk_Range_Record) return Glib.Gint;
   pragma Obsolescent (Get_Min_Slider_Size);
   --  This function is useful mainly for Gtk.GRange.Gtk_Range subclasses.
   --  See Gtk.GRange.Set_Min_Slider_Size.
   --  Since: gtk+ 2.20
   --  Deprecated since 3.20, 1

   procedure Set_Min_Slider_Size
      (The_Range : not null access Gtk_Range_Record;
       Min_Size  : Glib.Gint);
   pragma Obsolescent (Set_Min_Slider_Size);
   --  Sets the minimum size of the range's slider.
   --  This function is useful mainly for Gtk.GRange.Gtk_Range subclasses.
   --  Since: gtk+ 2.20
   --  Deprecated since 3.20, 1
   --  "min_size": The slider's minimum size

   procedure Get_Range_Rect
      (The_Range  : not null access Gtk_Range_Record;
       Range_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   --  This function returns the area that contains the range's trough and its
   --  steppers, in widget->window coordinates.
   --  This function is useful mainly for Gtk.GRange.Gtk_Range subclasses.
   --  Since: gtk+ 2.20
   --  "range_rect": return location for the range rectangle

   function Get_Restrict_To_Fill_Level
      (The_Range : not null access Gtk_Range_Record) return Boolean;
   --  Gets whether the range is restricted to the fill level.
   --  Since: gtk+ 2.12

   procedure Set_Restrict_To_Fill_Level
      (The_Range              : not null access Gtk_Range_Record;
       Restrict_To_Fill_Level : Boolean);
   --  Sets whether the slider is restricted to the fill level. See
   --  Gtk.GRange.Set_Fill_Level for a general description of the fill level
   --  concept.
   --  Since: gtk+ 2.12
   --  "restrict_to_fill_level": Whether the fill level restricts slider
   --  movement.

   function Get_Round_Digits
      (The_Range : not null access Gtk_Range_Record) return Glib.Gint;
   --  Gets the number of digits to round the value to when it changes. See
   --  Gtk.GRange.Gtk_Range::change-value.
   --  Since: gtk+ 2.24

   procedure Set_Round_Digits
      (The_Range    : not null access Gtk_Range_Record;
       Round_Digits : Glib.Gint);
   --  Sets the number of digits to round the value to when it changes. See
   --  Gtk.GRange.Gtk_Range::change-value.
   --  Since: gtk+ 2.24
   --  "round_digits": the precision in digits, or -1

   function Get_Show_Fill_Level
      (The_Range : not null access Gtk_Range_Record) return Boolean;
   --  Gets whether the range displays the fill level graphically.
   --  Since: gtk+ 2.12

   procedure Set_Show_Fill_Level
      (The_Range       : not null access Gtk_Range_Record;
       Show_Fill_Level : Boolean);
   --  Sets whether a graphical fill level is show on the trough. See
   --  Gtk.GRange.Set_Fill_Level for a general description of the fill level
   --  concept.
   --  Since: gtk+ 2.12
   --  "show_fill_level": Whether a fill level indicator graphics is shown.

   procedure Get_Slider_Range
      (The_Range    : not null access Gtk_Range_Record;
       Slider_Start : out Glib.Gint;
       Slider_End   : out Glib.Gint);
   --  This function returns sliders range along the long dimension, in
   --  widget->window coordinates.
   --  This function is useful mainly for Gtk.GRange.Gtk_Range subclasses.
   --  Since: gtk+ 2.20
   --  "slider_start": return location for the slider's start, or null
   --  "slider_end": return location for the slider's end, or null

   function Get_Slider_Size_Fixed
      (The_Range : not null access Gtk_Range_Record) return Boolean;
   --  This function is useful mainly for Gtk.GRange.Gtk_Range subclasses.
   --  See Gtk.GRange.Set_Slider_Size_Fixed.
   --  Since: gtk+ 2.20

   procedure Set_Slider_Size_Fixed
      (The_Range  : not null access Gtk_Range_Record;
       Size_Fixed : Boolean);
   --  Sets whether the range's slider has a fixed size, or a size that
   --  depends on its adjustment's page size.
   --  This function is useful mainly for Gtk.GRange.Gtk_Range subclasses.
   --  Since: gtk+ 2.20
   --  "size_fixed": True to make the slider size constant

   function Get_Upper_Stepper_Sensitivity
      (The_Range : not null access Gtk_Range_Record)
       return Gtk.Enums.Gtk_Sensitivity_Type;
   --  Gets the sensitivity policy for the stepper that points to the 'upper'
   --  end of the GtkRange's adjustment.
   --  Since: gtk+ 2.10

   procedure Set_Upper_Stepper_Sensitivity
      (The_Range   : not null access Gtk_Range_Record;
       Sensitivity : Gtk.Enums.Gtk_Sensitivity_Type);
   --  Sets the sensitivity policy for the stepper that points to the 'upper'
   --  end of the GtkRange's adjustment.
   --  Since: gtk+ 2.10
   --  "sensitivity": the upper stepper's sensitivity policy.

   function Get_Value
      (The_Range : not null access Gtk_Range_Record) return Gdouble;
   --  Gets the current value of the range.

   procedure Set_Value
      (The_Range : not null access Gtk_Range_Record;
       Value     : Gdouble);
   --  Sets the current value of the range; if the value is outside the
   --  minimum or maximum range values, it will be clamped to fit inside them.
   --  The range emits the Gtk.GRange.Gtk_Range::value-changed signal if the
   --  value changes.
   --  "value": new value of the range

   procedure Set_Increments
      (The_Range : not null access Gtk_Range_Record;
       Step      : Gdouble;
       Page      : Gdouble);
   --  Sets the step and page sizes for the range. The step size is used when
   --  the user clicks the Gtk.Scrollbar.Gtk_Scrollbar arrows or moves
   --  Gtk.Scale.Gtk_Scale via arrow keys. The page size is used for example
   --  when moving via Page Up or Page Down keys.
   --  "step": step size
   --  "page": page size

   procedure Set_Range
      (The_Range : not null access Gtk_Range_Record;
       Min       : Gdouble;
       Max       : Gdouble);
   --  Sets the allowable values in the Gtk.GRange.Gtk_Range, and clamps the
   --  range value to be between Min and Max. (If the range has a non-zero page
   --  size, it is clamped between Min and Max - page-size.)
   --  "min": minimum range value
   --  "max": maximum range value

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Range_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Range_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Adjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment

   Fill_Level_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The fill level (e.g. prebuffering of a network stream). See
   --  Gtk.GRange.Set_Fill_Level.

   Inverted_Property : constant Glib.Properties.Property_Boolean;

   Lower_Stepper_Sensitivity_Property : constant Gtk.Enums.Property_Gtk_Sensitivity_Type;

   Restrict_To_Fill_Level_Property : constant Glib.Properties.Property_Boolean;
   --  The restrict-to-fill-level property controls whether slider movement is
   --  restricted to an upper boundary set by the fill level. See
   --  Gtk.GRange.Set_Restrict_To_Fill_Level.

   Round_Digits_Property : constant Glib.Properties.Property_Int;
   --  The number of digits to round the value to when it changes, or -1. See
   --  Gtk.GRange.Gtk_Range::change-value.

   Show_Fill_Level_Property : constant Glib.Properties.Property_Boolean;
   --  The show-fill-level property controls whether fill level indicator
   --  graphics are displayed on the trough. See
   --  Gtk.GRange.Set_Show_Fill_Level.

   Upper_Stepper_Sensitivity_Property : constant Gtk.Enums.Property_Gtk_Sensitivity_Type;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Range_Gdouble_Void is not null access procedure
     (Self  : access Gtk_Range_Record'Class;
      Value : Gdouble);

   type Cb_GObject_Gdouble_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Value : Gdouble);

   Signal_Adjust_Bounds : constant Glib.Signal_Name := "adjust-bounds";
   procedure On_Adjust_Bounds
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_Gtk_Range_Gdouble_Void;
       After : Boolean := False);
   procedure On_Adjust_Bounds
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_GObject_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted before clamping a value, to give the application a chance to
   --  adjust the bounds.

   type Cb_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean is not null access function
     (Self   : access Gtk_Range_Record'Class;
      Scroll : Gtk.Enums.Gtk_Scroll_Type;
      Value  : Gdouble) return Boolean;

   type Cb_GObject_Gtk_Scroll_Type_Gdouble_Boolean is not null access function
     (Self   : access Glib.Object.GObject_Record'Class;
      Scroll : Gtk.Enums.Gtk_Scroll_Type;
      Value  : Gdouble) return Boolean;

   Signal_Change_Value : constant Glib.Signal_Name := "change-value";
   procedure On_Change_Value
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean;
       After : Boolean := False);
   procedure On_Change_Value
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Gdouble_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The Gtk.GRange.Gtk_Range::change-value signal is emitted when a scroll
   --  action is performed on a range. It allows an application to determine
   --  the type of scroll event that occurred and the resultant new value. The
   --  application can handle the event itself and return True to prevent
   --  further processing. Or, by returning False, it can pass the event to
   --  other handlers until the default GTK+ handler is reached.
   --
   --  The value parameter is unrounded. An application that overrides the
   --  GtkRange::change-value signal is responsible for clamping the value to
   --  the desired number of decimal digits; the default GTK+ handler clamps
   --  the value based on Gtk.GRange.Gtk_Range:round-digits.
   -- 
   --  Callback parameters:
   --    --  "scroll": the type of scroll action that was performed
   --    --  "value": the new value resulting from the scroll action
   --    --  Returns True to prevent other handlers from being invoked for
   --     the signal, False to propagate the signal further

   type Cb_Gtk_Range_Gtk_Scroll_Type_Void is not null access procedure
     (Self : access Gtk_Range_Record'Class;
      Step : Gtk.Enums.Gtk_Scroll_Type);

   type Cb_GObject_Gtk_Scroll_Type_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Step : Gtk.Enums.Gtk_Scroll_Type);

   Signal_Move_Slider : constant Glib.Signal_Name := "move-slider";
   procedure On_Move_Slider
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_Gtk_Range_Gtk_Scroll_Type_Void;
       After : Boolean := False);
   procedure On_Move_Slider
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Virtual function that moves the slider. Used for keybindings.

   type Cb_Gtk_Range_Void is not null access procedure (Self : access Gtk_Range_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Value_Changed : constant Glib.Signal_Name := "value-changed";
   procedure On_Value_Changed
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_Gtk_Range_Void;
       After : Boolean := False);
   procedure On_Value_Changed
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the range value changes.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Range_Record, Gtk_Range);
   function "+"
     (Widget : access Gtk_Range_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Range
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Range_Record, Gtk_Range);
   function "+"
     (Widget : access Gtk_Range_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Range
   renames Implements_Gtk_Orientable.To_Object;

private
   Upper_Stepper_Sensitivity_Property : constant Gtk.Enums.Property_Gtk_Sensitivity_Type :=
     Gtk.Enums.Build ("upper-stepper-sensitivity");
   Show_Fill_Level_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-fill-level");
   Round_Digits_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("round-digits");
   Restrict_To_Fill_Level_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("restrict-to-fill-level");
   Lower_Stepper_Sensitivity_Property : constant Gtk.Enums.Property_Gtk_Sensitivity_Type :=
     Gtk.Enums.Build ("lower-stepper-sensitivity");
   Inverted_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inverted");
   Fill_Level_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("fill-level");
   Adjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("adjustment");
end Gtk.GRange;
