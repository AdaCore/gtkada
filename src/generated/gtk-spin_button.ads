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

--  Allows to enter or change numeric values.
--
--  <picture> <source srcset="spinbutton-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkSpinButton"
--  src="spinbutton.png"> </picture>
--  Rather than having to directly type a number into a `GtkEntry`,
--  `GtkSpinButton` allows the user to click on one of two arrows to increment
--  or decrement the displayed value. A value can still be typed in, with the
--  bonus that it can be checked to ensure it is in a given range.
--
--  The main properties of a `GtkSpinButton` are through an adjustment. See
--  the [classGtk.Adjustment] documentation for more details about an
--  adjustment's properties.
--
--  Note that `GtkSpinButton` will by default make its entry large enough to
--  accommodate the lower and upper bounds of the adjustment. If this is not
--  desired, the automatic sizing can be turned off by explicitly setting
--  [propertyGtk.Editable:width-chars] to a value != -1.
--
--  ## Using a GtkSpinButton to get an integer
--
--  ```c // Provides a function to retrieve an integer value from a
--  GtkSpinButton // and creates a spin button to model percentage values.
--
--  int grab_int_value (GtkSpinButton *button, gpointer user_data) { return
--  gtk_spin_button_get_value_as_int (button); }
--
--  void create_integer_spin_button (void) {
--
--  GtkWidget *window, *button; GtkAdjustment *adjustment;
--
--  adjustment = gtk_adjustment_new (50.0, 0.0, 100.0, 1.0, 5.0, 0.0);
--
--  window = gtk_window_new ();
--
--  // creates the spinbutton, with no decimal places button =
--  gtk_spin_button_new (adjustment, 1.0, 0); gtk_window_set_child (GTK_WINDOW
--  (window), button);
--
--  gtk_window_present (GTK_WINDOW (window)); } ```
--
--  ## Using a GtkSpinButton to get a floating point value
--
--  ```c // Provides a function to retrieve a floating point value from a //
--  GtkSpinButton, and creates a high precision spin button.
--
--  float grab_float_value (GtkSpinButton *button, gpointer user_data) {
--  return gtk_spin_button_get_value (button); }
--
--  void create_floating_spin_button (void) { GtkWidget *window, *button;
--  GtkAdjustment *adjustment;
--
--  adjustment = gtk_adjustment_new (2.500, 0.0, 5.0, 0.001, 0.1, 0.0);
--
--  window = gtk_window_new ();
--
--  // creates the spinbutton, with three decimal places button =
--  gtk_spin_button_new (adjustment, 0.001, 3); gtk_window_set_child
--  (GTK_WINDOW (window), button);
--
--  gtk_window_present (GTK_WINDOW (window)); } ```
--
--  # Shortcuts and Gestures
--
--  The following signals have default keybindings:
--
--  - [signalGtk.SpinButton::change-value]
--
--  # CSS nodes
--
--  ``` spinbutton.horizontal ├── text │ ├── undershoot.left │ ╰──
--  undershoot.right ├── button.down ╰── button.up ```
--
--  ``` spinbutton.vertical ├── button.up ├── text │ ├── undershoot.left │ ╰──
--  undershoot.right ╰── button.down ```
--
--  `GtkSpinButton`s main CSS node has the name spinbutton. It creates
--  subnodes for the entry and the two buttons, with these names. The button
--  nodes have the style classes .up and .down. The `GtkText` subnodes (if
--  present) are put below the text node. The orientation of the spin button is
--  reflected in the .vertical or .horizontal style class on the main node.
--
--  # Accessibility
--
--  `GtkSpinButton` uses the [enumGtk.AccessibleRole.spin_button] role.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;               use Gdk.Event;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Adjustment;          use Gtk.Adjustment;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Cell_Editable;       use Gtk.Cell_Editable;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Editable;            use Gtk.Editable;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Orientable;          use Gtk.Orientable;
with Gtk.Widget;              use Gtk.Widget;
with Interfaces.C;            use Interfaces.C;

package Gtk.Spin_Button is

   type Gtk_Spin_Button_Record is new Gtk_Widget_Record with null record;
   type Gtk_Spin_Button is access all Gtk_Spin_Button_Record'Class;

   type Gtk_Spin_Type is (
      Spin_Step_Forward,
      Spin_Step_Backward,
      Spin_Page_Forward,
      Spin_Page_Backward,
      Spin_Home,
      Spin_End,
      Spin_User_Defined);
   pragma Convention (C, Gtk_Spin_Type);
   --  The values of the GtkSpinType enumeration are used to specify the
   --  change to make in Gtk.Spin_Button.Spin.

   type Gtk_Spin_Button_Update_Policy is (
      Update_Always,
      Update_If_Valid);
   pragma Convention (C, Gtk_Spin_Button_Update_Policy);
   --  Determines whether the spin button displays values outside the
   --  adjustment bounds.
   --
   --  See [methodGtk.SpinButton.set_update_policy].

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Spin_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Spin_Type);
   type Property_Gtk_Spin_Type is new Gtk_Spin_Type_Properties.Property;

   package Gtk_Spin_Button_Update_Policy_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Spin_Button_Update_Policy);
   type Property_Gtk_Spin_Button_Update_Policy is new Gtk_Spin_Button_Update_Policy_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gtk_Spin_Button;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
       Climb_Rate : Gdouble;
       The_Digits : Guint := 0);
   procedure Initialize
      (Self       : not null access Gtk_Spin_Button_Record'Class;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
       Climb_Rate : Gdouble;
       The_Digits : Guint := 0);
   --  Creates a new `GtkSpinButton`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Adjustment the `GtkAdjustment` that this spin button should use
   --  @param Climb_Rate specifies by how much the rate of change in the value
   --  will accelerate if you continue to hold down an up/down button or arrow
   --  key
   --  @param The_Digits the number of decimal places to display

   function Gtk_Spin_Button_New
      (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
       Climb_Rate : Gdouble;
       The_Digits : Guint := 0) return Gtk_Spin_Button;
   --  Creates a new `GtkSpinButton`.
   --  @param Adjustment the `GtkAdjustment` that this spin button should use
   --  @param Climb_Rate specifies by how much the rate of change in the value
   --  will accelerate if you continue to hold down an up/down button or arrow
   --  key
   --  @param The_Digits the number of decimal places to display

   procedure Gtk_New_With_Range
      (Self : out Gtk_Spin_Button;
       Min  : Gdouble;
       Max  : Gdouble;
       Step : Gdouble);
   procedure Initialize_With_Range
      (Self : not null access Gtk_Spin_Button_Record'Class;
       Min  : Gdouble;
       Max  : Gdouble;
       Step : Gdouble);
   --  Creates a new `GtkSpinButton` with the given properties.
   --  This is a convenience constructor that allows creation of a numeric
   --  `GtkSpinButton` without manually creating an adjustment. The value is
   --  initially set to the minimum value and a page increment of 10 * Step is
   --  the default. The precision of the spin button is equivalent to the
   --  precision of Step.
   --  Note that the way in which the precision is derived works best if Step
   --  is a power of ten. If the resulting precision is not suitable for your
   --  needs, use [methodGtk.SpinButton.set_digits] to correct it.
   --  Initialize_With_Range does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Min Minimum allowable value
   --  @param Max Maximum allowable value
   --  @param Step Increment added or subtracted by spinning the widget

   function Gtk_Spin_Button_New_With_Range
      (Min  : Gdouble;
       Max  : Gdouble;
       Step : Gdouble) return Gtk_Spin_Button;
   --  Creates a new `GtkSpinButton` with the given properties.
   --  This is a convenience constructor that allows creation of a numeric
   --  `GtkSpinButton` without manually creating an adjustment. The value is
   --  initially set to the minimum value and a page increment of 10 * Step is
   --  the default. The precision of the spin button is equivalent to the
   --  precision of Step.
   --  Note that the way in which the precision is derived works best if Step
   --  is a power of ten. If the resulting precision is not suitable for your
   --  needs, use [methodGtk.SpinButton.set_digits] to correct it.
   --  @param Min Minimum allowable value
   --  @param Max Maximum allowable value
   --  @param Step Increment added or subtracted by spinning the widget

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_spin_button_get_type");

   -------------
   -- Methods --
   -------------

   procedure Configure
      (Self       : not null access Gtk_Spin_Button_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
       Climb_Rate : Gdouble;
       The_Digits : Guint);
   --  Changes the properties of an existing spin button.
   --  The adjustment, climb rate, and number of decimal places are updated
   --  accordingly.
   --  @param Adjustment a `GtkAdjustment` to replace the spin button's
   --  existing adjustment, or null to leave its current adjustment unchanged
   --  @param Climb_Rate the new climb rate
   --  @param The_Digits the number of decimal places to display in the spin
   --  button

   function Get_Activates_Default
      (Self : not null access Gtk_Spin_Button_Record) return Boolean;
   --  Retrieves the value set by
   --  [methodGtk.SpinButton.set_activates_default].
   --  Since: gtk+ 4.14
   --  @return True if the spin button will activate the default widget

   procedure Set_Activates_Default
      (Self              : not null access Gtk_Spin_Button_Record;
       Activates_Default : Boolean);
   --  Sets whether activating the spin button will activate the default
   --  widget for the window containing the spin button.
   --  See [signalGtk.SpinButton::activate] for what counts as activation.
   --  Since: gtk+ 4.14
   --  @param Activates_Default True to activate window's default widget on
   --  activation

   function Get_Adjustment
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Get the adjustment associated with a `GtkSpinButton`.
   --  @return the `GtkAdjustment` of Spin_Button
   --  Return has transfer-ownership='none'

   procedure Set_Adjustment
      (Self       : not null access Gtk_Spin_Button_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Replaces the `GtkAdjustment` associated with Spin_Button.
   --  @param Adjustment a `GtkAdjustment` to replace the existing adjustment

   function Get_Climb_Rate
      (Self : not null access Gtk_Spin_Button_Record) return Gdouble;
   --  Returns the acceleration rate for repeated changes.
   --  @return the acceleration rate

   procedure Set_Climb_Rate
      (Self       : not null access Gtk_Spin_Button_Record;
       Climb_Rate : Gdouble);
   --  Sets the acceleration rate for repeated changes when you hold down a
   --  button or key.
   --  @param Climb_Rate the rate of acceleration, must be >= 0

   function Get_Digits
      (Self : not null access Gtk_Spin_Button_Record) return Guint;
   --  Fetches the precision of Spin_Button.
   --  @return the current precision

   procedure Set_Digits
      (Self       : not null access Gtk_Spin_Button_Record;
       The_Digits : Guint);
   --  Set the precision to be displayed by Spin_Button.
   --  Up to 20 digit precision is allowed.
   --  @param The_Digits the number of digits after the decimal point to be
   --  displayed for the spin button's value

   procedure Get_Increments
      (Self : not null access Gtk_Spin_Button_Record;
       Step : out Gdouble;
       Page : out Gdouble);
   --  Gets the current step and page the increments used by Spin_Button.
   --  See [methodGtk.SpinButton.set_increments].
   --  @param Step location to store step increment
   --  @param Page location to store page increment

   procedure Set_Increments
      (Self : not null access Gtk_Spin_Button_Record;
       Step : Gdouble;
       Page : Gdouble);
   --  Sets the step and page increments for spin_button.
   --  This affects how quickly the value changes when the spin button's
   --  arrows are activated.
   --  @param Step increment applied for a button 1 press.
   --  @param Page increment applied for a button 2 press.

   function Get_Numeric
      (Self : not null access Gtk_Spin_Button_Record) return Boolean;
   --  Returns whether non-numeric text can be typed into the spin button.
   --  @return True if only numeric text can be entered

   procedure Set_Numeric
      (Self    : not null access Gtk_Spin_Button_Record;
       Numeric : Boolean);
   --  Sets the flag that determines if non-numeric text can be typed into the
   --  spin button.
   --  @param Numeric flag indicating if only numeric entry is allowed

   procedure Get_Range
      (Self : not null access Gtk_Spin_Button_Record;
       Min  : out Gdouble;
       Max  : out Gdouble);
   --  Gets the range allowed for Spin_Button.
   --  See [methodGtk.SpinButton.set_range].
   --  @param Min location to store minimum allowed value
   --  @param Max location to store maximum allowed value

   procedure Set_Range
      (Self : not null access Gtk_Spin_Button_Record;
       Min  : Gdouble;
       Max  : Gdouble);
   --  Sets the minimum and maximum allowable values for Spin_Button.
   --  If the current value is outside this range, it will be adjusted to fit
   --  within the range, otherwise it will remain unchanged.
   --  @param Min minimum allowable value
   --  @param Max maximum allowable value

   function Get_Snap_To_Ticks
      (Self : not null access Gtk_Spin_Button_Record) return Boolean;
   --  Returns whether the values are corrected to the nearest step.
   --  @return True if values are snapped to the nearest step

   procedure Set_Snap_To_Ticks
      (Self          : not null access Gtk_Spin_Button_Record;
       Snap_To_Ticks : Boolean);
   --  Sets the policy as to whether values are corrected to the nearest step
   --  increment when a spin button is activated after providing an invalid
   --  value.
   --  @param Snap_To_Ticks a flag indicating if invalid values should be
   --  corrected

   function Get_Update_Policy
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk_Spin_Button_Update_Policy;
   --  Gets the update behavior of a spin button.
   --  See [methodGtk.SpinButton.set_update_policy].
   --  @return the current update policy

   procedure Set_Update_Policy
      (Self   : not null access Gtk_Spin_Button_Record;
       Policy : Gtk_Spin_Button_Update_Policy);
   --  Sets the update behavior of a spin button.
   --  This determines whether the spin button is always updated or only when
   --  a valid value is set.
   --  @param Policy a `GtkSpinButtonUpdatePolicy` value

   function Get_Value
      (Self : not null access Gtk_Spin_Button_Record) return Gdouble;
   --  Get the value in the Spin_Button.
   --  @return the value of Spin_Button

   procedure Set_Value
      (Self  : not null access Gtk_Spin_Button_Record;
       Value : Gdouble);
   --  Sets the value of Spin_Button.
   --  @param Value the new value

   function Get_Value_As_Int
      (Self : not null access Gtk_Spin_Button_Record) return Glib.Gint;
   --  Get the value Spin_Button represented as an integer.
   --  @return the value of Spin_Button

   function Get_Wrap
      (Self : not null access Gtk_Spin_Button_Record) return Boolean;
   --  Returns whether the spin button's value wraps around to the opposite
   --  limit when the upper or lower limit of the range is exceeded.
   --  @return True if the spin button wraps around

   procedure Set_Wrap
      (Self : not null access Gtk_Spin_Button_Record;
       Wrap : Boolean);
   --  Sets the flag that determines if a spin button value wraps around to
   --  the opposite limit when the upper or lower limit of the range is
   --  exceeded.
   --  @param Wrap a flag indicating if wrapping behavior is performed

   procedure Spin
      (Self      : not null access Gtk_Spin_Button_Record;
       Direction : Gtk_Spin_Type;
       Increment : Gdouble);
   --  Increment or decrement a spin button's value in a specified direction
   --  by a specified amount.
   --  @param Direction a `GtkSpinType` indicating the direction to spin
   --  @param Increment step increment to apply in the specified direction

   procedure Update (Self : not null access Gtk_Spin_Button_Record);
   --  Manually force an update of the spin button.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Spin_Button_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Spin_Button_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Spin_Button_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Spin_Button_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Spin_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Spin_Button_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Spin_Button_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Spin_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Spin_Button_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Spin_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   procedure Editing_Done
      (Cell_Editable : not null access Gtk_Spin_Button_Record);
   pragma Obsolescent (Editing_Done);

   procedure Remove_Widget
      (Cell_Editable : not null access Gtk_Spin_Button_Record);
   pragma Obsolescent (Remove_Widget);

   procedure Start_Editing
      (Cell_Editable : not null access Gtk_Spin_Button_Record;
       Event         : Gdk.Event.Gdk_Event);

   function Delegate_Get_Accessible_Platform_State
      (Self  : not null access Gtk_Spin_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Delete_Selection
      (Self : not null access Gtk_Spin_Button_Record);

   procedure Delete_Text
      (Self      : not null access Gtk_Spin_Button_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);

   procedure Finish_Delegate (Self : not null access Gtk_Spin_Button_Record);

   function Get_Alignment
      (Self : not null access Gtk_Spin_Button_Record)
       return Interfaces.C.C_float;

   procedure Set_Alignment
      (Self   : not null access Gtk_Spin_Button_Record;
       Xalign : Interfaces.C.C_float);

   function Get_Chars
      (Self      : not null access Gtk_Spin_Button_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String;

   function Get_Delegate
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk.Editable.Gtk_Editable;

   function Get_Editable
      (Self : not null access Gtk_Spin_Button_Record) return Boolean;

   procedure Set_Editable
      (Self        : not null access Gtk_Spin_Button_Record;
       Is_Editable : Boolean);

   function Get_Enable_Undo
      (Self : not null access Gtk_Spin_Button_Record) return Boolean;

   procedure Set_Enable_Undo
      (Self        : not null access Gtk_Spin_Button_Record;
       Enable_Undo : Boolean);

   function Get_Max_Width_Chars
      (Self : not null access Gtk_Spin_Button_Record) return Glib.Gint;

   procedure Set_Max_Width_Chars
      (Self    : not null access Gtk_Spin_Button_Record;
       N_Chars : Glib.Gint);

   function Get_Position
      (Self : not null access Gtk_Spin_Button_Record) return Glib.Gint;

   procedure Set_Position
      (Self     : not null access Gtk_Spin_Button_Record;
       Position : Glib.Gint);

   procedure Get_Selection_Bounds
      (Self          : not null access Gtk_Spin_Button_Record;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean);

   function Get_Text
      (Self : not null access Gtk_Spin_Button_Record) return UTF8_String;

   procedure Set_Text
      (Self : not null access Gtk_Spin_Button_Record;
       Text : UTF8_String);

   function Get_Width_Chars
      (Self : not null access Gtk_Spin_Button_Record) return Glib.Gint;

   procedure Set_Width_Chars
      (Self    : not null access Gtk_Spin_Button_Record;
       N_Chars : Glib.Gint);

   procedure Init_Delegate (Self : not null access Gtk_Spin_Button_Record);

   procedure Insert_Text
      (Self     : not null access Gtk_Spin_Button_Record;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Position : in out Glib.Gint);

   procedure Select_Region
      (Self      : not null access Gtk_Spin_Button_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);

   function Get_Orientation
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Spin_Button_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Activates_Default_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to activate the default widget when the spin button is
   --  activated.
   --
   --  See [signalGtk.SpinButton::activate] for what counts as activation.

   Adjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  The adjustment that holds the value of the spin button.

   Climb_Rate_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The acceleration rate when you hold down a button or key.

   Numeric_Property : constant Glib.Properties.Property_Boolean;
   --  Whether non-numeric characters should be ignored.

   Snap_To_Ticks_Property : constant Glib.Properties.Property_Boolean;
   --  Whether erroneous values are automatically changed to the spin buttons
   --  nearest step increment.

   The_Digits_Property : constant Glib.Properties.Property_Uint;
   --  The number of decimal places to display.

   Update_Policy_Property : constant Gtk.Spin_Button.Property_Gtk_Spin_Button_Update_Policy;
   --  Type: Gtk_Spin_Button_Update_Policy
   --  Whether the spin button should update always, or only when the value is
   --  acceptable.

   Value_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The current value.

   Wrap_Property : constant Glib.Properties.Property_Boolean;
   --  Whether a spin button should wrap upon reaching its limits.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Spin_Button_Void is not null access procedure
     (Self : access Gtk_Spin_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_Gtk_Spin_Button_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the spin button is activated.
   --
   --  The keybindings for this signal are all forms of the <kbd>Enter</kbd>
   --  key.
   --
   --  If the <kbd>Enter</kbd> key results in the value being committed to the
   --  spin button, then activation does not occur until <kbd>Enter</kbd> is
   --  pressed again.

   type Cb_Gtk_Spin_Button_Gtk_Scroll_Type_Void is not null access procedure
     (Self   : access Gtk_Spin_Button_Record'Class;
      Scroll : Gtk.Enums.Gtk_Scroll_Type);

   type Cb_GObject_Gtk_Scroll_Type_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Scroll : Gtk.Enums.Gtk_Scroll_Type);

   Signal_Change_Value : constant Glib.Signal_Name := "change-value";
   procedure On_Change_Value
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_Gtk_Spin_Button_Gtk_Scroll_Type_Void;
       After : Boolean := False);
   procedure On_Change_Value
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user initiates a value change.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control the cursor
   --  programmatically.
   --
   --  The default bindings for this signal are Up/Down and PageUp/PageDown.

   type Cb_Gtk_Spin_Button_Gdouble_Gint is not null access function
     (Self      : access Gtk_Spin_Button_Record'Class;
      New_Value : out Glib.Gdouble) return Glib.Gint;

   type Cb_GObject_Gdouble_Gint is not null access function
     (Self      : access Glib.Object.GObject_Record'Class;
      New_Value : out Glib.Gdouble) return Glib.Gint;

   Signal_Input : constant Glib.Signal_Name := "input";
   procedure On_Input
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_Gtk_Spin_Button_Gdouble_Gint;
       After : Boolean := False);
   procedure On_Input
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_GObject_Gdouble_Gint;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to convert the users input into a double value.
   --
   --  The signal handler is expected to use [methodGtk.Editable.get_text] to
   --  retrieve the text of the spinbutton and set New_Value to the new value.
   --
   --  The default conversion uses g_strtod.
   -- 
   --  Callback parameters:
   --    --  @param New_Value return location for the new value

   type Cb_Gtk_Spin_Button_Boolean is not null access function
     (Self : access Gtk_Spin_Button_Record'Class) return Boolean;

   type Cb_GObject_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Boolean;

   Signal_Output : constant Glib.Signal_Name := "output";
   procedure On_Output
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_Gtk_Spin_Button_Boolean;
       After : Boolean := False);
   procedure On_Output
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to tweak the formatting of the value for display.
   --
   --  ```c // show leading zeros static gboolean on_output (GtkSpinButton
   --  *spin, gpointer data) { char *text; int value;
   --
   --  value = gtk_spin_button_get_value_as_int (spin); text = g_strdup_printf
   --  ("%02d", value); gtk_editable_set_text (GTK_EDITABLE (spin), text):
   --  g_free (text);
   --
   --  return TRUE; } ```
   -- 
   --  Callback parameters:

   Signal_Value_Changed : constant Glib.Signal_Name := "value-changed";
   procedure On_Value_Changed
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_Gtk_Spin_Button_Void;
       After : Boolean := False);
   procedure On_Value_Changed
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the value is changed.
   --
   --  Also see the [signalGtk.SpinButton::output] signal.

   Signal_Wrapped : constant Glib.Signal_Name := "wrapped";
   procedure On_Wrapped
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_Gtk_Spin_Button_Void;
       After : Boolean := False);
   procedure On_Wrapped
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted right after the spinbutton wraps from its maximum to its
   --  minimum value or vice-versa.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.CellEditable"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Editable"
   --
   --  - "Gtk.Orientable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Spin_Button_Record, Gtk_Spin_Button);
   function "+"
     (Widget : access Gtk_Spin_Button_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Spin_Button
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Spin_Button_Record, Gtk_Spin_Button);
   function "+"
     (Widget : access Gtk_Spin_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Spin_Button
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Cell_Editable is new Glib.Types.Implements
     (Gtk.Cell_Editable.Gtk_Cell_Editable, Gtk_Spin_Button_Record, Gtk_Spin_Button);
   function "+"
     (Widget : access Gtk_Spin_Button_Record'Class)
   return Gtk.Cell_Editable.Gtk_Cell_Editable
   renames Implements_Gtk_Cell_Editable.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Editable.Gtk_Cell_Editable)
   return Gtk_Spin_Button
   renames Implements_Gtk_Cell_Editable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Spin_Button_Record, Gtk_Spin_Button);
   function "+"
     (Widget : access Gtk_Spin_Button_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Spin_Button
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Editable is new Glib.Types.Implements
     (Gtk.Editable.Gtk_Editable, Gtk_Spin_Button_Record, Gtk_Spin_Button);
   function "+"
     (Widget : access Gtk_Spin_Button_Record'Class)
   return Gtk.Editable.Gtk_Editable
   renames Implements_Gtk_Editable.To_Interface;
   function "-"
     (Interf : Gtk.Editable.Gtk_Editable)
   return Gtk_Spin_Button
   renames Implements_Gtk_Editable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Spin_Button_Record, Gtk_Spin_Button);
   function "+"
     (Widget : access Gtk_Spin_Button_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Spin_Button
   renames Implements_Gtk_Orientable.To_Object;

private
   Wrap_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap");
   Value_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("value");
   Update_Policy_Property : constant Gtk.Spin_Button.Property_Gtk_Spin_Button_Update_Policy :=
     Gtk.Spin_Button.Build ("update-policy");
   The_Digits_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("digits");
   Snap_To_Ticks_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("snap-to-ticks");
   Numeric_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("numeric");
   Climb_Rate_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("climb-rate");
   Adjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("adjustment");
   Activates_Default_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activates-default");
end Gtk.Spin_Button;
