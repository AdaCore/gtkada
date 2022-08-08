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
--  A GtkScale is a slider control used to select a numeric value. To use it,
--  you'll probably want to investigate the methods on its base class,
--  Gtk.GRange.Gtk_Range, in addition to the methods for GtkScale itself. To
--  set the value of a scale, you would normally use Gtk.GRange.Set_Value. To
--  detect changes to the value, you would normally use the
--  Gtk.GRange.Gtk_Range::value-changed signal.
--
--  Note that using the same upper and lower bounds for the
--  Gtk.Scale.Gtk_Scale (through the Gtk.GRange.Gtk_Range methods) will hide
--  the slider itself. This is useful for applications that want to show an
--  undeterminate value on the scale, without changing the layout of the
--  application (such as movie or music players).
--
--  # GtkScale as GtkBuildable
--
--  GtkScale supports a custom <marks> element, which can contain multiple
--  <mark> elements. The "value" and "position" attributes have the same
--  meaning as Gtk.Scale.Add_Mark parameters of the same name. If the element
--  is not empty, its content is taken as the markup to show at the mark. It
--  can be translated with the usual "translatable" and "context" attributes.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> scale[.fine-tune][.marks-before][.marks-after]
--  ├── marks.top │ ├── mark │ ┊ ├── [label] │ ┊ ╰── indicator ┊ ┊ │ ╰── mark
--  ├── [value] ├── contents │ ╰── trough │ ├── slider │ ├── [highlight] │ ╰──
--  [fill] ╰── marks.bottom ├── mark ┊ ├── indicator ┊ ╰── [label] ╰── mark ]|
--
--  GtkScale has a main CSS node with name scale and a subnode for its
--  contents, with subnodes named trough and slider.
--
--  The main node gets the style class .fine-tune added when the scale is in
--  'fine-tuning' mode.
--
--  If the scale has an origin (see Gtk.Scale.Set_Has_Origin), there is a
--  subnode with name highlight below the trough node that is used for
--  rendering the highlighted part of the trough.
--
--  If the scale is showing a fill level (see Gtk.GRange.Set_Show_Fill_Level),
--  there is a subnode with name fill below the trough node that is used for
--  rendering the filled in part of the trough.
--
--  If marks are present, there is a marks subnode before or after the
--  contents node, below which each mark gets a node with name mark. The marks
--  nodes get either the .top or .bottom style class.
--
--  The mark node has a subnode named indicator. If the mark has text, it also
--  has a subnode named label. When the mark is either above or left of the
--  scale, the label subnode is the first when present. Otherwise, the
--  indicator subnode is the first.
--
--  The main CSS node gets the 'marks-before' and/or 'marks-after' style
--  classes added depending on what marks are present.
--
--  If the scale is displaying the value (see Gtk.Scale.Gtk_Scale:draw-value),
--  there is subnode with name value.
--
--  </description>
--  <screenshot>gtk-scale.png</screenshot>
--  <group>Numeric/Text Data Entry</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.GRange;      use Gtk.GRange;
with Gtk.Orientable;  use Gtk.Orientable;
with Pango.Layout;    use Pango.Layout;

package Gtk.Scale is

   type Gtk_Scale_Record is new Gtk_Range_Record with null record;
   type Gtk_Scale is access all Gtk_Scale_Record'Class;

   subtype Gtk_Hscale_Record is Gtk_Scale_Record;
   subtype Gtk_Hscale is Gtk_Scale;

   subtype Gtk_Vscale_Record is Gtk_Scale_Record;
   subtype Gtk_Vscale is Gtk_Scale;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Scale       : out Gtk_Scale;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   procedure Initialize
      (Scale       : not null access Gtk_Scale_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Creates a new Gtk.Scale.Gtk_Scale.
   --  Since: gtk+ 3.0
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "orientation": the scale's orientation.
   --  "adjustment": the Gtk.Adjustment.Gtk_Adjustment which sets the range of
   --  the scale, or null to create a new adjustment.

   function Gtk_Scale_New
      (Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
       return Gtk_Scale;
   --  Creates a new Gtk.Scale.Gtk_Scale.
   --  Since: gtk+ 3.0
   --  "orientation": the scale's orientation.
   --  "adjustment": the Gtk.Adjustment.Gtk_Adjustment which sets the range of
   --  the scale, or null to create a new adjustment.

   procedure Gtk_New_With_Range
      (Scale       : out Gtk_Scale;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Min         : Gdouble;
       Max         : Gdouble;
       Step        : Gdouble);
   procedure Initialize_With_Range
      (Scale       : not null access Gtk_Scale_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Min         : Gdouble;
       Max         : Gdouble;
       Step        : Gdouble);
   --  Creates a new scale widget with the given orientation that lets the
   --  user input a number between Min and Max (including Min and Max) with the
   --  increment Step. Step must be nonzero; it's the distance the slider moves
   --  when using the arrow keys to adjust the scale value.
   --  Note that the way in which the precision is derived works best if Step
   --  is a power of ten. If the resulting precision is not suitable for your
   --  needs, use Gtk.Scale.Set_Digits to correct it.
   --  Since: gtk+ 3.0
   --  Initialize_With_Range does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "orientation": the scale's orientation.
   --  "min": minimum value
   --  "max": maximum value
   --  "step": step increment (tick size) used with keyboard shortcuts

   function Gtk_Scale_New_With_Range
      (Orientation : Gtk.Enums.Gtk_Orientation;
       Min         : Gdouble;
       Max         : Gdouble;
       Step        : Gdouble) return Gtk_Scale;
   --  Creates a new scale widget with the given orientation that lets the
   --  user input a number between Min and Max (including Min and Max) with the
   --  increment Step. Step must be nonzero; it's the distance the slider moves
   --  when using the arrow keys to adjust the scale value.
   --  Note that the way in which the precision is derived works best if Step
   --  is a power of ten. If the resulting precision is not suitable for your
   --  needs, use Gtk.Scale.Set_Digits to correct it.
   --  Since: gtk+ 3.0
   --  "orientation": the scale's orientation.
   --  "min": minimum value
   --  "max": maximum value
   --  "step": step increment (tick size) used with keyboard shortcuts

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_scale_get_type");

   procedure Gtk_New_Hscale
      (Scale      : out Gtk_Hscale;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment := null);
   procedure Initialize_Hscale
      (Scale      : not null access Gtk_Hscale_Record'Class;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment := null);
   --  Creates a new Gtk.Scale.Gtk_Hscale.
   --  Initialize_Hscale does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  "adjustment": the Gtk.Adjustment.Gtk_Adjustment which sets the range of
   --  the scale.

   procedure Gtk_New_Hscale
      (Scale : out Gtk_Hscale;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble);
   procedure Initialize_Hscale
      (Scale : not null access Gtk_Hscale_Record'Class;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble);
   --  Creates a new horizontal scale widget that lets the user input a number
   --  between Min and Max (including Min and Max) with the increment Step.
   --  Step must be nonzero; it's the distance the slider moves when using the
   --  arrow keys to adjust the scale value.
   --  Note that the way in which the precision is derived works best if Step
   --  is a power of ten. If the resulting precision is not suitable for your
   --  needs, use Gtk.Scale.Set_Digits to correct it.
   --  Initialize_Hscale does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  "min": minimum value
   --  "max": maximum value
   --  "step": step increment (tick size) used with keyboard shortcuts

   function Gtk_Hscale_New
      (Adjustment : Gtk.Adjustment.Gtk_Adjustment := null) return Gtk_Hscale;
   --  Creates a new Gtk.Scale.Gtk_Hscale.
   --  "adjustment": the Gtk.Adjustment.Gtk_Adjustment which sets the range of
   --  the scale.

   function Gtk_Hscale_New_With_Range
      (Min  : Gdouble;
       Max  : Gdouble;
       Step : Gdouble) return Gtk_Hscale;
   --  Creates a new horizontal scale widget that lets the user input a number
   --  between Min and Max (including Min and Max) with the increment Step.
   --  Step must be nonzero; it's the distance the slider moves when using the
   --  arrow keys to adjust the scale value.
   --  Note that the way in which the precision is derived works best if Step
   --  is a power of ten. If the resulting precision is not suitable for your
   --  needs, use Gtk.Scale.Set_Digits to correct it.
   --  "min": minimum value
   --  "max": maximum value
   --  "step": step increment (tick size) used with keyboard shortcuts

   function Hscale_Get_Type return Glib.GType;
   pragma Import (C, Hscale_Get_Type, "gtk_hscale_get_type");

   procedure Gtk_New_Vscale
      (Scale      : out Gtk_Vscale;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment := null);
   procedure Initialize_Vscale
      (Scale      : not null access Gtk_Vscale_Record'Class;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment := null);
   --  Creates a new Gtk.Scale.Gtk_Vscale.
   --  Initialize_Vscale does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  "adjustment": the Gtk.Adjustment.Gtk_Adjustment which sets the range of
   --  the scale.

   procedure Gtk_New_Vscale
      (Scale : out Gtk_Vscale;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble);
   procedure Initialize_Vscale
      (Scale : not null access Gtk_Vscale_Record'Class;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble);
   --  Creates a new vertical scale widget that lets the user input a number
   --  between Min and Max (including Min and Max) with the increment Step.
   --  Step must be nonzero; it's the distance the slider moves when using the
   --  arrow keys to adjust the scale value.
   --  Note that the way in which the precision is derived works best if Step
   --  is a power of ten. If the resulting precision is not suitable for your
   --  needs, use Gtk.Scale.Set_Digits to correct it.
   --  Initialize_Vscale does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  "min": minimum value
   --  "max": maximum value
   --  "step": step increment (tick size) used with keyboard shortcuts

   function Gtk_Vscale_New
      (Adjustment : Gtk.Adjustment.Gtk_Adjustment := null) return Gtk_Vscale;
   --  Creates a new Gtk.Scale.Gtk_Vscale.
   --  "adjustment": the Gtk.Adjustment.Gtk_Adjustment which sets the range of
   --  the scale.

   function Gtk_Vscale_New_With_Range
      (Min  : Gdouble;
       Max  : Gdouble;
       Step : Gdouble) return Gtk_Vscale;
   --  Creates a new vertical scale widget that lets the user input a number
   --  between Min and Max (including Min and Max) with the increment Step.
   --  Step must be nonzero; it's the distance the slider moves when using the
   --  arrow keys to adjust the scale value.
   --  Note that the way in which the precision is derived works best if Step
   --  is a power of ten. If the resulting precision is not suitable for your
   --  needs, use Gtk.Scale.Set_Digits to correct it.
   --  "min": minimum value
   --  "max": maximum value
   --  "step": step increment (tick size) used with keyboard shortcuts

   function Vscale_Get_Type return Glib.GType;
   pragma Import (C, Vscale_Get_Type, "gtk_vscale_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Mark
      (Scale    : not null access Gtk_Scale_Record;
       Value    : Gdouble;
       Position : Gtk.Enums.Gtk_Position_Type;
       Markup   : UTF8_String := "");
   --  Adds a mark at Value.
   --  A mark is indicated visually by drawing a tick mark next to the scale,
   --  and GTK+ makes it easy for the user to position the scale exactly at the
   --  marks value.
   --  If Markup is not null, text is shown next to the tick mark.
   --  To remove marks from a scale, use Gtk.Scale.Clear_Marks.
   --  Since: gtk+ 2.16
   --  "value": the value at which the mark is placed, must be between the
   --  lower and upper limits of the scales' adjustment
   --  "position": where to draw the mark. For a horizontal scale, GTK_POS_TOP
   --  and Gtk.Enums.Pos_Left are drawn above the scale, anything else below.
   --  For a vertical scale, GTK_POS_LEFT and Gtk.Enums.Pos_Top are drawn to
   --  the left of the scale, anything else to the right.
   --  "markup": Text to be shown at the mark, using [Pango
   --  markup][PangoMarkupFormat], or null

   procedure Clear_Marks (Scale : not null access Gtk_Scale_Record);
   --  Removes any marks that have been added with Gtk.Scale.Add_Mark.
   --  Since: gtk+ 2.16

   function Get_Digits
      (Scale : not null access Gtk_Scale_Record) return Glib.Gint;
   --  Gets the number of decimal places that are displayed in the value.

   procedure Set_Digits
      (Scale      : not null access Gtk_Scale_Record;
       The_Digits : Glib.Gint);
   --  Sets the number of decimal places that are displayed in the value. Also
   --  causes the value of the adjustment to be rounded to this number of
   --  digits, so the retrieved value matches the displayed one, if
   --  Gtk.Scale.Gtk_Scale:draw-value is True when the value changes. If you
   --  want to enforce rounding the value when Gtk.Scale.Gtk_Scale:draw-value
   --  is False, you can set Gtk.GRange.Gtk_Range:round-digits instead.
   --  Note that rounding to a small number of digits can interfere with the
   --  smooth autoscrolling that is built into Gtk.Scale.Gtk_Scale. As an
   --  alternative, you can use the Gtk.Scale.Gtk_Scale::format-value signal to
   --  format the displayed value yourself.
   --  "digits": the number of decimal places to display, e.g. use 1 to
   --  display 1.0, 2 to display 1.00, etc

   function Get_Draw_Value
      (Scale : not null access Gtk_Scale_Record) return Boolean;
   --  Returns whether the current value is displayed as a string next to the
   --  slider.

   procedure Set_Draw_Value
      (Scale      : not null access Gtk_Scale_Record;
       Draw_Value : Boolean);
   --  Specifies whether the current value is displayed as a string next to
   --  the slider.
   --  "draw_value": True to draw the value

   function Get_Has_Origin
      (Scale : not null access Gtk_Scale_Record) return Boolean;
   --  Returns whether the scale has an origin.
   --  Since: gtk+ 3.4

   procedure Set_Has_Origin
      (Scale      : not null access Gtk_Scale_Record;
       Has_Origin : Boolean);
   --  If Gtk.Scale.Gtk_Scale:has-origin is set to True (the default), the
   --  scale will highlight the part of the trough between the origin (bottom
   --  or left side) and the current value.
   --  Since: gtk+ 3.4
   --  "has_origin": True if the scale has an origin

   function Get_Layout
      (Scale : not null access Gtk_Scale_Record)
       return Pango.Layout.Pango_Layout;
   --  Gets the Pango.Layout.Pango_Layout used to display the scale. The
   --  returned object is owned by the scale so does not need to be freed by
   --  the caller.
   --  Since: gtk+ 2.4

   procedure Get_Layout_Offsets
      (Scale : not null access Gtk_Scale_Record;
       X     : out Glib.Gint;
       Y     : out Glib.Gint);
   --  Obtains the coordinates where the scale will draw the
   --  Pango.Layout.Pango_Layout representing the text in the scale. Remember
   --  when using the Pango.Layout.Pango_Layout function you need to convert to
   --  and from pixels using PANGO_PIXELS or PANGO_SCALE.
   --  If the Gtk.Scale.Gtk_Scale:draw-value property is False, the return
   --  values are undefined.
   --  Since: gtk+ 2.4
   --  "x": location to store X offset of layout, or null
   --  "y": location to store Y offset of layout, or null

   function Get_Value_Pos
      (Scale : not null access Gtk_Scale_Record)
       return Gtk.Enums.Gtk_Position_Type;
   --  Gets the position in which the current value is displayed.

   procedure Set_Value_Pos
      (Scale : not null access Gtk_Scale_Record;
       Pos   : Gtk.Enums.Gtk_Position_Type);
   --  Sets the position in which the current value is displayed.
   --  "pos": the position in which the current value is displayed

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

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

   Has_Origin_Property : constant Glib.Properties.Property_Boolean;

   The_Digits_Property : constant Glib.Properties.Property_Int;

   Value_Pos_Property : constant Gtk.Enums.Property_Gtk_Position_Type;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Scale_Gdouble_UTF8_String is not null access function
     (Self  : access Gtk_Scale_Record'Class;
      Value : Gdouble) return UTF8_String;

   type Cb_GObject_Gdouble_UTF8_String is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Value : Gdouble) return UTF8_String;

   Signal_Format_Value : constant Glib.Signal_Name := "format-value";
   procedure On_Format_Value
      (Self  : not null access Gtk_Scale_Record;
       Call  : Cb_Gtk_Scale_Gdouble_UTF8_String;
       After : Boolean := False);
   procedure On_Format_Value
      (Self  : not null access Gtk_Scale_Record;
       Call  : Cb_GObject_Gdouble_UTF8_String;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Signal which allows you to change how the scale value is displayed.
   --  Connect a signal handler which returns an allocated string representing
   --  Value. That string will then be used to display the scale's value.
   --
   --  If no user-provided handlers are installed, the value will be displayed
   --  on its own, rounded according to the value of the
   --  Gtk.Scale.Gtk_Scale:digits property.
   --
   --  Here's an example signal handler which displays a value 1.0 as with
   --  "-->1.0<--". |[<!-- language="C" --> static gchar* format_value_callback
   --  (GtkScale *scale, gdouble value) { return g_strdup_printf
   --  ("-->\%0.*g<--", gtk_scale_get_digits (scale), value); } ]|
   -- 
   --  Callback parameters:
   --    --  "value": the value to format
   --    --  Returns allocated string representing Value

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

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
