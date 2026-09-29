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

--  Allows to select a numeric value with a slider control.
--
--  <picture> <source srcset="scales-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkScale" src="scales.png"> </picture>
--  To use it, you'll probably want to investigate the methods on its base
--  class, [classGtk.Range], in addition to the methods for `GtkScale` itself.
--  To set the value of a scale, you would normally use
--  [methodGtk.Range.set_value]. To detect changes to the value, you would
--  normally use the [signalGtk.Range::value-changed] signal.
--
--  Note that using the same upper and lower bounds for the `GtkScale`
--  (through the `GtkRange` methods) will hide the slider itself. This is
--  useful for applications that want to show an undeterminate value on the
--  scale, without changing the layout of the application (such as movie or
--  music players).
--
--  # GtkScale as GtkBuildable
--
--  `GtkScale` supports a custom `<marks>` element, which can contain multiple
--  `<mark\>` elements. The "value" and "position" attributes have the same
--  meaning as [methodGtk.Scale.add_mark] parameters of the same name. If the
--  element is not empty, its content is taken as the markup to show at the
--  mark. It can be translated with the usual "translatable" and "context"
--  attributes.
--
--  # Shortcuts and Gestures
--
--  `GtkPopoverMenu` supports the following keyboard shortcuts:
--
--  - Arrow keys, <kbd>+</kbd> and <kbd>-</kbd> will increment or decrement by
--  step, or by page when combined with <kbd>Ctrl</kbd>. - <kbd>PgUp</kbd> and
--  <kbd>PgDn</kbd> will increment or decrement by page. - <kbd>Home</kbd> and
--  <kbd>End</kbd> will set the minimum or maximum value.
--
--  # CSS nodes
--
--  ``` scale[.fine-tune][.marks-before][.marks-after] ├──
--  [value][.top][.right][.bottom][.left] ├── marks.top │ ├── mark │ ┊ ├──
--  [label] │ ┊ ╰── indicator ┊ ┊ │ ╰── mark ├── marks.bottom │ ├── mark │ ┊
--  ├── indicator │ ┊ ╰── [label] ┊ ┊ │ ╰── mark ╰── trough ├── [fill] ├──
--  [highlight] ╰── slider ```
--
--  `GtkScale` has a main CSS node with name scale and a subnode for its
--  contents, with subnodes named trough and slider.
--
--  The main node gets the style class .fine-tune added when the scale is in
--  'fine-tuning' mode.
--
--  If the scale has an origin (see [methodGtk.Scale.set_has_origin]), there
--  is a subnode with name highlight below the trough node that is used for
--  rendering the highlighted part of the trough.
--
--  If the scale is showing a fill level (see
--  [methodGtk.Range.set_show_fill_level]), there is a subnode with name fill
--  below the trough node that is used for rendering the filled in part of the
--  trough.
--
--  If marks are present, there is a marks subnode before or after the trough
--  node, below which each mark gets a node with name mark. The marks nodes get
--  either the .top or .bottom style class.
--
--  The mark node has a subnode named indicator. If the mark has text, it also
--  has a subnode named label. When the mark is either above or left of the
--  scale, the label subnode is the first when present. Otherwise, the
--  indicator subnode is the first.
--
--  The main CSS node gets the 'marks-before' and/or 'marks-after' style
--  classes added depending on what marks are present.
--
--  If the scale is displaying the value (see [propertyGtk.Scale:draw-value]),
--  there is subnode with name value. This node will get the .top or .bottom
--  style classes similar to the marks node.
--
--  # Accessibility
--
--  `GtkScale` uses the [enumGtk.AccessibleRole.slider] role.
--
--  <group>Numeric/Text Data Entry</group>
--  <gtkada_demo>create_scale.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.GRange;            use Gtk.GRange;
with Gtk.Orientable;        use Gtk.Orientable;
with Pango.Layout;          use Pango.Layout;

package Gtk.Scale is

   type Gtk_Scale_Record is new Gtk_Range_Record with null record;
   type Gtk_Scale is access all Gtk_Scale_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Scale_Format_Value_Func is access function
     (Scale : not null access Gtk_Scale_Record'Class;
      Value : Gdouble) return UTF8_String;
   --  Function that formats the value of a scale.
   --  See [methodGtk.Scale.set_format_value_func].
   --  @param Scale The `GtkScale`
   --  @param Value The numeric value to format
   --  @return A newly allocated string describing a textual representation of
   --  the given numerical value.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self        : out Gtk_Scale;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   procedure Initialize
      (Self        : not null access Gtk_Scale_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Creates a new `GtkScale`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Orientation the scale's orientation.
   --  @param Adjustment the [classGtk.Adjustment] which sets the range of the
   --  scale, or null to create a new adjustment.

   function Gtk_Scale_New
      (Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
       return Gtk_Scale;
   --  Creates a new `GtkScale`.
   --  @param Orientation the scale's orientation.
   --  @param Adjustment the [classGtk.Adjustment] which sets the range of the
   --  scale, or null to create a new adjustment.

   procedure Gtk_New_With_Range
      (Self        : out Gtk_Scale;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Min         : Gdouble;
       Max         : Gdouble;
       Step        : Gdouble);
   procedure Initialize_With_Range
      (Self        : not null access Gtk_Scale_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Min         : Gdouble;
       Max         : Gdouble;
       Step        : Gdouble);
   --  Creates a new scale widget with a range from Min to Max.
   --  The returns scale will have the given orientation and will let the user
   --  input a number between Min and Max (including Min and Max) with the
   --  increment Step. Step must be nonzero; it's the distance the slider moves
   --  when using the arrow keys to adjust the scale value.
   --  Note that the way in which the precision is derived works best if Step
   --  is a power of ten. If the resulting precision is not suitable for your
   --  needs, use [methodGtk.Scale.set_digits] to correct it.
   --  Initialize_With_Range does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Orientation the scale's orientation.
   --  @param Min minimum value
   --  @param Max maximum value
   --  @param Step step increment (tick size) used with keyboard shortcuts

   function Gtk_Scale_New_With_Range
      (Orientation : Gtk.Enums.Gtk_Orientation;
       Min         : Gdouble;
       Max         : Gdouble;
       Step        : Gdouble) return Gtk_Scale;
   --  Creates a new scale widget with a range from Min to Max.
   --  The returns scale will have the given orientation and will let the user
   --  input a number between Min and Max (including Min and Max) with the
   --  increment Step. Step must be nonzero; it's the distance the slider moves
   --  when using the arrow keys to adjust the scale value.
   --  Note that the way in which the precision is derived works best if Step
   --  is a power of ten. If the resulting precision is not suitable for your
   --  needs, use [methodGtk.Scale.set_digits] to correct it.
   --  @param Orientation the scale's orientation.
   --  @param Min minimum value
   --  @param Max maximum value
   --  @param Step step increment (tick size) used with keyboard shortcuts

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_scale_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Mark
      (Self     : not null access Gtk_Scale_Record;
       Value    : Gdouble;
       Position : Gtk.Enums.Gtk_Position_Type;
       Markup   : UTF8_String := "");
   --  Adds a mark at Value.
   --  A mark is indicated visually by drawing a tick mark next to the scale,
   --  and GTK makes it easy for the user to position the scale exactly at the
   --  marks value.
   --  If Markup is not null, text is shown next to the tick mark.
   --  To remove marks from a scale, use [methodGtk.Scale.clear_marks].
   --  @param Value the value at which the mark is placed, must be between the
   --  lower and upper limits of the scales' adjustment
   --  @param Position where to draw the mark. For a horizontal scale,
   --  Gtk.Enums.Pos_Top and Gtk.Enums.Pos_Left are drawn above the scale,
   --  anything else below. For a vertical scale, Gtk.Enums.Pos_Left and
   --  Gtk.Enums.Pos_Top are drawn to the left of the scale, anything else to
   --  the right.
   --  @param Markup Text to be shown at the mark, using Pango markup

   procedure Clear_Marks (Self : not null access Gtk_Scale_Record);
   --  Removes any marks that have been added.

   function Get_Digits
      (Self : not null access Gtk_Scale_Record) return Glib.Gint;
   --  Gets the number of decimal places that are displayed in the value.
   --  @return the number of decimal places that are displayed

   procedure Set_Digits
      (Self       : not null access Gtk_Scale_Record;
       The_Digits : Glib.Gint);
   --  Sets the number of decimal places that are displayed in the value.
   --  Also causes the value of the adjustment to be rounded to this number of
   --  digits, so the retrieved value matches the displayed one, if
   --  [propertyGtk.Scale:draw-value] is True when the value changes. If you
   --  want to enforce rounding the value when [propertyGtk.Scale:draw-value]
   --  is False, you can set [propertyGtk.Range:round-digits] instead.
   --  Note that rounding to a small number of digits can interfere with the
   --  smooth autoscrolling that is built into `GtkScale`. As an alternative,
   --  you can use [methodGtk.Scale.set_format_value_func] to format the
   --  displayed value yourself.
   --  @param The_Digits the number of decimal places to display, e.g. use 1
   --  to display 1.0, 2 to display 1.00, etc

   function Get_Draw_Value
      (Self : not null access Gtk_Scale_Record) return Boolean;
   --  Returns whether the current value is displayed as a string next to the
   --  slider.
   --  @return whether the current value is displayed as a string

   procedure Set_Draw_Value
      (Self       : not null access Gtk_Scale_Record;
       Draw_Value : Boolean);
   --  Specifies whether the current value is displayed as a string next to
   --  the slider.
   --  @param Draw_Value True to draw the value

   function Get_Has_Origin
      (Self : not null access Gtk_Scale_Record) return Boolean;
   --  Returns whether the scale has an origin.
   --  @return True if the scale has an origin.

   procedure Set_Has_Origin
      (Self       : not null access Gtk_Scale_Record;
       Has_Origin : Boolean);
   --  Sets whether the scale has an origin.
   --  If [propertyGtk.Scale:has-origin] is set to True (the default), the
   --  scale will highlight the part of the trough between the origin (bottom
   --  or left side) and the current value.
   --  @param Has_Origin True if the scale has an origin

   function Get_Layout
      (Self : not null access Gtk_Scale_Record)
       return Pango.Layout.Pango_Layout;
   --  Gets the `PangoLayout` used to display the scale.
   --  The returned object is owned by the scale so does not need to be freed
   --  by the caller.
   --  @return the [classPango.Layout] for this scale, or null if the
   --  [propertyGtk.Scale:draw-value] property is False. Has
   --  transfer-ownership='none'.

   procedure Get_Layout_Offsets
      (Self : not null access Gtk_Scale_Record;
       X    : out Glib.Gint;
       Y    : out Glib.Gint);
   --  Obtains the coordinates where the scale will draw the `PangoLayout`
   --  representing the text in the scale.
   --  Remember when using the `PangoLayout` function you need to convert to
   --  and from pixels using `PANGO_PIXELS` or `PANGO_SCALE`.
   --  If the [propertyGtk.Scale:draw-value] property is False, the return
   --  values are undefined.
   --  @param X location to store X offset of layout
   --  @param Y location to store Y offset of layout

   function Get_Value_Pos
      (Self : not null access Gtk_Scale_Record)
       return Gtk.Enums.Gtk_Position_Type;
   --  Gets the position in which the current value is displayed.
   --  @return the position in which the current value is displayed

   procedure Set_Value_Pos
      (Self : not null access Gtk_Scale_Record;
       Pos  : Gtk.Enums.Gtk_Position_Type);
   --  Sets the position in which the current value is displayed.
   --  @param Pos the position in which the current value is displayed

   procedure Set_Format_Value_Func
      (Self : not null access Gtk_Scale_Record;
       Func : Gtk_Scale_Format_Value_Func);
   --  Func allows you to change how the scale value is displayed.
   --  The given function will return an allocated string representing Value.
   --  That string will then be used to display the scale's value.
   --  If NULL is passed as Func, the value will be displayed on its own,
   --  rounded according to the value of the [propertyGtk.Scale:digits]
   --  property.
   --  @param Func function that formats the value

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Format_Value_Func_User_Data is

      type Gtk_Scale_Format_Value_Func is access function
        (Scale     : not null access Gtk.Scale.Gtk_Scale_Record'Class;
         Value     : Gdouble;
         User_Data : User_Data_Type) return UTF8_String;
      --  Function that formats the value of a scale.
      --  See [methodGtk.Scale.set_format_value_func].
      --  @param Scale The `GtkScale`
      --  @param Value The numeric value to format
      --  @param User_Data user data
      --  @return A newly allocated string describing a textual representation of
      --  the given numerical value.

      procedure Set_Format_Value_Func
         (Self      : not null access Gtk.Scale.Gtk_Scale_Record'Class;
          Func      : Gtk_Scale_Format_Value_Func;
          User_Data : User_Data_Type);
      --  Func allows you to change how the scale value is displayed.
      --  The given function will return an allocated string representing
      --  Value. That string will then be used to display the scale's value.
      --  If NULL is passed as Func, the value will be displayed on its own,
      --  rounded according to the value of the [propertyGtk.Scale:digits]
      --  property.
      --  @param Func function that formats the value
      --  @param User_Data user data to pass to Func

   end Set_Format_Value_Func_User_Data;

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Scale_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Scale_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Scale_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Scale_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Scale_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Scale_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Scale_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Scale_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Scale_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Scale_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Scale_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Scale_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Scale_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Scale_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Scale_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Orientation
      (Self : not null access Gtk_Scale_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Scale_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Draw_Value_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the current value is displayed as a string next to the slider.

   Has_Origin_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the scale has an origin.

   The_Digits_Property : constant Glib.Properties.Property_Int;
   --  The number of decimal places that are displayed in the value.

   Value_Pos_Property : constant Gtk.Enums.Property_Gtk_Position_Type;
   --  The position in which the current value is displayed.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Scale_Record, Gtk_Scale);
   function "+"
     (Widget : access Gtk_Scale_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Scale
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Scale_Record, Gtk_Scale);
   function "+"
     (Widget : access Gtk_Scale_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Scale
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Scale_Record, Gtk_Scale);
   function "+"
     (Widget : access Gtk_Scale_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Scale
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Scale_Record, Gtk_Scale);
   function "+"
     (Widget : access Gtk_Scale_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Scale
   renames Implements_Gtk_Orientable.To_Object;

private
   Value_Pos_Property : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("value-pos");
   The_Digits_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("digits");
   Has_Origin_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-origin");
   Draw_Value_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("draw-value");
end Gtk.Scale;
