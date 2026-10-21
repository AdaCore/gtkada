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

--  Provides a button which pops up a scale widget.
--
--  This kind of widget is commonly used for volume controls in multimedia
--  applications, and GTK provides a [classGtk.VolumeButton] subclass that is
--  tailored for this use case.
--
--  # Shortcuts and Gestures
--
--  The following signals have default keybindings:
--
--  - [signalGtk.ScaleButton::popup]
--
--  # CSS nodes
--
--  ``` scalebutton.scale ╰── button.toggle ╰── <icon> ```
--
--  `GtkScaleButton` has a single CSS node with name scalebutton and `.scale`
--  style class, and contains a `button` node with a `.toggle` style class.
--
--  <group>Numeric/Text Data Entry</group>
--  <gtkada_demo>create_range.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;          use GNAT.Strings;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Accessible_Range;  use Gtk.Accessible_Range;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Orientable;        use Gtk.Orientable;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Scale_Button is

   type Gtk_Scale_Button_Record is new Gtk_Widget_Record with null record;
   type Gtk_Scale_Button is access all Gtk_Scale_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self  : out Gtk_Scale_Button;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble;
       Icons : GNAT.Strings.String_List);
   procedure Initialize
      (Self  : not null access Gtk_Scale_Button_Record'Class;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble;
       Icons : GNAT.Strings.String_List);
   --  Creates a `GtkScaleButton`.
   --  The new scale button has a range between Min and Max, with a stepping
   --  of Step.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Min the minimum value of the scale (usually 0)
   --  @param Max the maximum value of the scale (usually 100)
   --  @param Step the stepping of value when a scroll-wheel event, or up/down
   --  arrow event occurs (usually 2)
   --  @param Icons a null-terminated array of icon names, or null if you want
   --  to set the list later with Gtk.Scale_Button.Set_Icons

   function Gtk_Scale_Button_New
      (Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble;
       Icons : GNAT.Strings.String_List) return Gtk_Scale_Button;
   --  Creates a `GtkScaleButton`.
   --  The new scale button has a range between Min and Max, with a stepping
   --  of Step.
   --  @param Min the minimum value of the scale (usually 0)
   --  @param Max the maximum value of the scale (usually 100)
   --  @param Step the stepping of value when a scroll-wheel event, or up/down
   --  arrow event occurs (usually 2)
   --  @param Icons a null-terminated array of icon names, or null if you want
   --  to set the list later with Gtk.Scale_Button.Set_Icons

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_scale_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active
      (Self : not null access Gtk_Scale_Button_Record) return Boolean;
   --  Queries a `GtkScaleButton` and returns its current state.
   --  Returns True if the scale button is pressed in and False if it is
   --  raised.
   --  Since: gtk+ 4.10
   --  @return whether the button is pressed

   function Get_Adjustment
      (Self : not null access Gtk_Scale_Button_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Gets the `GtkAdjustment` associated with the `GtkScaleButton`'s scale.
   --  See [methodGtk.Range.get_adjustment] for details.
   --  @return the adjustment associated with the scale. Has
   --  transfer-ownership='none'.

   procedure Set_Adjustment
      (Self       : not null access Gtk_Scale_Button_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the `GtkAdjustment` to be used as a model for the
   --  `GtkScaleButton`'s scale.
   --  See [methodGtk.Range.set_adjustment] for details.
   --  @param Adjustment a `GtkAdjustment`

   function Get_Has_Frame
      (Self : not null access Gtk_Scale_Button_Record) return Boolean;
   --  Returns whether the button has a frame.
   --  Since: gtk+ 4.14
   --  @return True if the button has a frame

   procedure Set_Has_Frame
      (Self      : not null access Gtk_Scale_Button_Record;
       Has_Frame : Boolean);
   --  Sets the style of the button.
   --  Since: gtk+ 4.14
   --  @param Has_Frame whether the button should have a visible frame

   function Get_Minus_Button
      (Self : not null access Gtk_Scale_Button_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the minus button of the `GtkScaleButton`.
   --  @return the minus button of the `GtkScaleButton`. Has
   --  transfer-ownership='none'.

   function Get_Plus_Button
      (Self : not null access Gtk_Scale_Button_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the plus button of the `GtkScaleButton.`
   --  @return the plus button of the `GtkScaleButton`. Has
   --  transfer-ownership='none'.

   function Get_Popup
      (Self : not null access Gtk_Scale_Button_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the popup of the `GtkScaleButton`.
   --  @return the popup of the `GtkScaleButton`. Has
   --  transfer-ownership='none'.

   function Get_Value
      (Self : not null access Gtk_Scale_Button_Record) return Gdouble;
   --  Gets the current value of the scale button.
   --  @return current value of the scale button

   procedure Set_Value
      (Self  : not null access Gtk_Scale_Button_Record;
       Value : Gdouble);
   --  Sets the current value of the scale.
   --  If the value is outside the minimum or maximum range values, it will be
   --  clamped to fit inside them.
   --  The scale button emits the [signalGtk.ScaleButton::value-changed]
   --  signal if the value changes.
   --  @param Value new value of the scale button

   procedure Set_Icons
      (Self  : not null access Gtk_Scale_Button_Record;
       Icons : GNAT.Strings.String_List);
   --  Sets the icons to be used by the scale button.
   --  @param Icons a null-terminated array of icon names

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Scale_Button_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Scale_Button_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Scale_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Scale_Button_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Scale_Button_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Scale_Button_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Scale_Button_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Scale_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Scale_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Scale_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Scale_Button_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Scale_Button_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Scale_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Scale_Button_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Scale_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Orientation
      (Self : not null access Gtk_Scale_Button_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Scale_Button_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Property : constant Glib.Properties.Property_Boolean;
   --  If the scale button should be pressed in.

   Adjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  The `GtkAdjustment` that is used as the model.

   Has_Frame_Property : constant Glib.Properties.Property_Boolean;
   --  If the scale button has a frame.

   Value_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The value of the scale.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Scale_Button_Void is not null access procedure
     (Self : access Gtk_Scale_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Popdown : constant Glib.Signal_Name := "popdown";
   procedure On_Popdown
      (Self  : not null access Gtk_Scale_Button_Record;
       Call  : Cb_Gtk_Scale_Button_Void;
       After : Boolean := False);
   procedure On_Popdown
      (Self  : not null access Gtk_Scale_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to dismiss the popup.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default binding for this signal is <kbd>Escape</kbd>.

   Signal_Popup : constant Glib.Signal_Name := "popup";
   procedure On_Popup
      (Self  : not null access Gtk_Scale_Button_Record;
       Call  : Cb_Gtk_Scale_Button_Void;
       After : Boolean := False);
   procedure On_Popup
      (Self  : not null access Gtk_Scale_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to popup the scale widget.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default bindings for this signal are <kbd>Space</kbd>,
   --  <kbd>Enter</kbd> and <kbd>Return</kbd>.

   type Cb_Gtk_Scale_Button_Gdouble_Void is not null access procedure
     (Self  : access Gtk_Scale_Button_Record'Class;
      Value : Gdouble);

   type Cb_GObject_Gdouble_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Value : Gdouble);

   Signal_Value_Changed : constant Glib.Signal_Name := "value-changed";
   procedure On_Value_Changed
      (Self  : not null access Gtk_Scale_Button_Record;
       Call  : Cb_Gtk_Scale_Button_Gdouble_Void;
       After : Boolean := False);
   procedure On_Value_Changed
      (Self  : not null access Gtk_Scale_Button_Record;
       Call  : Cb_GObject_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the value field has changed.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.AccessibleRange"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Orientable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Scale_Button_Record, Gtk_Scale_Button);
   function "+"
     (Widget : access Gtk_Scale_Button_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Scale_Button
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Accessible_Range is new Glib.Types.Implements
     (Gtk.Accessible_Range.Gtk_Accessible_Range, Gtk_Scale_Button_Record, Gtk_Scale_Button);
   function "+"
     (Widget : access Gtk_Scale_Button_Record'Class)
   return Gtk.Accessible_Range.Gtk_Accessible_Range
   renames Implements_Gtk_Accessible_Range.To_Interface;
   function "-"
     (Interf : Gtk.Accessible_Range.Gtk_Accessible_Range)
   return Gtk_Scale_Button
   renames Implements_Gtk_Accessible_Range.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Scale_Button_Record, Gtk_Scale_Button);
   function "+"
     (Widget : access Gtk_Scale_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Scale_Button
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Scale_Button_Record, Gtk_Scale_Button);
   function "+"
     (Widget : access Gtk_Scale_Button_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Scale_Button
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Scale_Button_Record, Gtk_Scale_Button);
   function "+"
     (Widget : access Gtk_Scale_Button_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Scale_Button
   renames Implements_Gtk_Orientable.To_Object;

private
   Value_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("value");
   Has_Frame_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-frame");
   Adjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("adjustment");
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Scale_Button;
