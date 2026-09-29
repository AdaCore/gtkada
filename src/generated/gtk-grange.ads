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

--  Base class for widgets which visualize an adjustment.
--
--  Widgets that are derived from `GtkRange` include [classGtk.Scale] and
--  [classGtk.Scrollbar].
--
--  Apart from signals for monitoring the parameters of the adjustment,
--  `GtkRange` provides properties and methods for setting a "fill level" on
--  range widgets. See [methodGtk.Range.set_fill_level].
--
--  # Shortcuts and Gestures
--
--  The `GtkRange` slider is draggable. Holding the <kbd>Shift</kbd> key while
--  dragging, or initiating the drag with a long-press will enable the
--  fine-tuning mode.
--
--  <group>Numeric/Text Data Entry</group>
--  <gtkada_demo>create_scale.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Rectangle;         use Gdk.Rectangle;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Orientable;        use Gtk.Orientable;
with Gtk.Widget;            use Gtk.Widget;

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
      (Self : not null access Gtk_Range_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Get the adjustment which is the "model" object for `GtkRange`.
   --  @return a `GtkAdjustment`. Has transfer-ownership='none'.

   procedure Set_Adjustment
      (Self       : not null access Gtk_Range_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the adjustment to be used as the "model" object for the `GtkRange`
   --  The adjustment indicates the current range value, the minimum and
   --  maximum range values, the step/page increments used for keybindings and
   --  scrolling, and the page size.
   --  The page size is normally 0 for `GtkScale` and nonzero for
   --  `GtkScrollbar`, and indicates the size of the visible area of the widget
   --  being scrolled. The page size affects the size of the scrollbar slider.
   --  @param Adjustment a `GtkAdjustment`

   function Get_Fill_Level
      (Self : not null access Gtk_Range_Record) return Gdouble;
   --  Gets the current position of the fill level indicator.
   --  @return The current fill level

   procedure Set_Fill_Level
      (Self       : not null access Gtk_Range_Record;
       Fill_Level : Gdouble);
   --  Set the new position of the fill level indicator.
   --  The "fill level" is probably best described by its most prominent use
   --  case, which is an indicator for the amount of pre-buffering in a
   --  streaming media player. In that use case, the value of the range would
   --  indicate the current play position, and the fill level would be the
   --  position up to which the file/stream has been downloaded.
   --  This amount of prebuffering can be displayed on the range's trough and
   --  is themeable separately from the trough. To enable fill level display,
   --  use [methodGtk.Range.set_show_fill_level]. The range defaults to not
   --  showing the fill level.
   --  Additionally, it's possible to restrict the range's slider position to
   --  values which are smaller than the fill level. This is controlled by
   --  [methodGtk.Range.set_restrict_to_fill_level] and is by default enabled.
   --  @param Fill_Level the new position of the fill level indicator

   function Get_Flippable
      (Self : not null access Gtk_Range_Record) return Boolean;
   --  Gets whether the `GtkRange` respects text direction.
   --  See [methodGtk.Range.set_flippable].
   --  @return True if the range is flippable

   procedure Set_Flippable
      (Self      : not null access Gtk_Range_Record;
       Flippable : Boolean);
   --  Sets whether the `GtkRange` respects text direction.
   --  If a range is flippable, it will switch its direction if it is
   --  horizontal and its direction is Gtk.Enums.Text_Dir_Rtl.
   --  See [methodGtk.Widget.get_direction].
   --  @param Flippable True to make the range flippable

   function Get_Inverted
      (Self : not null access Gtk_Range_Record) return Boolean;
   --  Gets whether the range is inverted.
   --  See [methodGtk.Range.set_inverted].
   --  @return True if the range is inverted

   procedure Set_Inverted
      (Self    : not null access Gtk_Range_Record;
       Setting : Boolean);
   --  Sets whether to invert the range.
   --  Ranges normally move from lower to higher values as the slider moves
   --  from top to bottom or left to right. Inverted ranges have higher values
   --  at the top or on the right rather than on the bottom or left.
   --  @param Setting True to invert the range

   procedure Get_Range_Rect
      (Self       : not null access Gtk_Range_Record;
       Range_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   --  This function returns the area that contains the range's trough, in
   --  coordinates relative to Range's origin.
   --  This function is useful mainly for `GtkRange` subclasses.
   --  @param Range_Rect return location for the range rectangle

   function Get_Restrict_To_Fill_Level
      (Self : not null access Gtk_Range_Record) return Boolean;
   --  Gets whether the range is restricted to the fill level.
   --  @return True if Range is restricted to the fill level.

   procedure Set_Restrict_To_Fill_Level
      (Self                   : not null access Gtk_Range_Record;
       Restrict_To_Fill_Level : Boolean);
   --  Sets whether the slider is restricted to the fill level.
   --  See [methodGtk.Range.set_fill_level] for a general description of the
   --  fill level concept.
   --  @param Restrict_To_Fill_Level Whether the fill level restricts slider
   --  movement.

   function Get_Round_Digits
      (Self : not null access Gtk_Range_Record) return Glib.Gint;
   --  Gets the number of digits to round the value to when it changes.
   --  See [signalGtk.Range::change-value].
   --  @return the number of digits to round to

   procedure Set_Round_Digits
      (Self         : not null access Gtk_Range_Record;
       Round_Digits : Glib.Gint);
   --  Sets the number of digits to round the value to when it changes.
   --  See [signalGtk.Range::change-value].
   --  @param Round_Digits the precision in digits, or -1

   function Get_Show_Fill_Level
      (Self : not null access Gtk_Range_Record) return Boolean;
   --  Gets whether the range displays the fill level graphically.
   --  @return True if Range shows the fill level.

   procedure Set_Show_Fill_Level
      (Self            : not null access Gtk_Range_Record;
       Show_Fill_Level : Boolean);
   --  Sets whether a graphical fill level is show on the trough.
   --  See [methodGtk.Range.set_fill_level] for a general description of the
   --  fill level concept.
   --  @param Show_Fill_Level Whether a fill level indicator graphics is
   --  shown.

   procedure Get_Slider_Range
      (Self         : not null access Gtk_Range_Record;
       Slider_Start : out Glib.Gint;
       Slider_End   : out Glib.Gint);
   --  This function returns sliders range along the long dimension, in
   --  widget->window coordinates.
   --  This function is useful mainly for `GtkRange` subclasses.
   --  @param Slider_Start return location for the slider's start
   --  @param Slider_End return location for the slider's end

   function Get_Slider_Size_Fixed
      (Self : not null access Gtk_Range_Record) return Boolean;
   --  This function is useful mainly for `GtkRange` subclasses.
   --  See [methodGtk.Range.set_slider_size_fixed].
   --  @return whether the range's slider has a fixed size.

   procedure Set_Slider_Size_Fixed
      (Self       : not null access Gtk_Range_Record;
       Size_Fixed : Boolean);
   --  Sets whether the range's slider has a fixed size, or a size that
   --  depends on its adjustment's page size.
   --  This function is useful mainly for `GtkRange` subclasses.
   --  @param Size_Fixed True to make the slider size constant

   function Get_Value
      (Self : not null access Gtk_Range_Record) return Gdouble;
   --  Gets the current value of the range.
   --  @return current value of the range.

   procedure Set_Value
      (Self  : not null access Gtk_Range_Record;
       Value : Gdouble);
   --  Sets the current value of the range.
   --  If the value is outside the minimum or maximum range values, it will be
   --  clamped to fit inside them. The range emits the
   --  [signalGtk.Range::value-changed] signal if the value changes.
   --  @param Value new value of the range

   procedure Set_Increments
      (Self : not null access Gtk_Range_Record;
       Step : Gdouble;
       Page : Gdouble);
   --  Sets the step and page sizes for the range.
   --  The step size is used when the user clicks the `GtkScrollbar` arrows or
   --  moves a `GtkScale` via arrow keys. The page size is used for example
   --  when moving via Page Up or Page Down keys.
   --  @param Step step size
   --  @param Page page size

   procedure Set_Range
      (Self : not null access Gtk_Range_Record;
       Min  : Gdouble;
       Max  : Gdouble);
   --  Sets the allowable values in the `GtkRange`.
   --  The range value is clamped to be between Min and Max. (If the range has
   --  a non-zero page size, it is clamped between Min and Max - page-size.)
   --  @param Min minimum range value
   --  @param Max maximum range value

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Range_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Range_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Range_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Range_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Range_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Range_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Range_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Range_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Range_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Range_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Range_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Range_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Range_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Range_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Range_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

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
   --  The adjustment that is controlled by the range.

   Fill_Level_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The fill level (e.g. prebuffering of a network stream).

   Inverted_Property : constant Glib.Properties.Property_Boolean;
   --  If True, the direction in which the slider moves is inverted.

   Restrict_To_Fill_Level_Property : constant Glib.Properties.Property_Boolean;
   --  Controls whether slider movement is restricted to an upper boundary set
   --  by the fill level.

   Round_Digits_Property : constant Glib.Properties.Property_Int;
   --  The number of digits to round the value to when it changes.
   --
   --  See [signalGtk.Range::change-value].

   Show_Fill_Level_Property : constant Glib.Properties.Property_Boolean;
   --  Controls whether fill level indicator graphics are displayed on the
   --  trough.

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
   --  Emitted when a scroll action is performed on a range.
   --
   --  It allows an application to determine the type of scroll event that
   --  occurred and the resultant new value. The application can handle the
   --  event itself and return True to prevent further processing. Or, by
   --  returning False, it can pass the event to other handlers until the
   --  default GTK handler is reached.
   --
   --  The value parameter is unrounded. An application that overrides the
   --  ::change-value signal is responsible for clamping the value to the
   --  desired number of decimal digits; the default GTK handler clamps the
   --  value based on [propertyGtk.Range:round-digits].
   -- 
   --  Callback parameters:
   --    --  @param Scroll the type of scroll action that was performed
   --    --  @param Value the new value resulting from the scroll action

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
   --  Virtual function that moves the slider.
   --
   --  Used for keybindings.

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
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Orientable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Range_Record, Gtk_Range);
   function "+"
     (Widget : access Gtk_Range_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Range
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Range_Record, Gtk_Range);
   function "+"
     (Widget : access Gtk_Range_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Range
   renames Implements_Gtk_Constraint_Target.To_Object;

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
   Show_Fill_Level_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-fill-level");
   Round_Digits_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("round-digits");
   Restrict_To_Fill_Level_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("restrict-to-fill-level");
   Inverted_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inverted");
   Fill_Level_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("fill-level");
   Adjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("adjustment");
end Gtk.GRange;
