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

--  Places a label next to an indicator.
--
--  <picture> <source srcset="check-button-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="Example GtkCheckButtons"
--  src="check-button.png"> </picture>
--  A `GtkCheckButton` is created by calling either [ctorGtk.CheckButton.new]
--  or [ctorGtk.CheckButton.new_with_label].
--
--  The state of a `GtkCheckButton` can be set specifically using
--  [methodGtk.CheckButton.set_active], and retrieved using
--  [methodGtk.CheckButton.get_active].
--
--  # Inconsistent state
--
--  In addition to "on" and "off", check buttons can be an "in between" state
--  that is neither on nor off. This can be used e.g. when the user has
--  selected a range of elements (such as some text or spreadsheet cells) that
--  are affected by a check button, and the current values in that range are
--  inconsistent.
--
--  To set a `GtkCheckButton` to inconsistent state, use
--  [methodGtk.CheckButton.set_inconsistent].
--
--  # Grouping
--
--  Check buttons can be grouped together, to form mutually exclusive groups -
--  only one of the buttons can be toggled at a time, and toggling another one
--  will switch the currently toggled one off.
--
--  Grouped check buttons use a different indicator, and are commonly referred
--  to as *radio buttons*.
--
--  <picture> <source srcset="radio-button-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="Example GtkRadioButtons"
--  src="radio-button.png"> </picture>
--  To add a `GtkCheckButton` to a group, use
--  [methodGtk.CheckButton.set_group].
--
--  When the code must keep track of the state of a group of radio buttons, it
--  is recommended to keep track of such state through a stateful `GAction`
--  with a target for each button. Using the `toggled` signals to keep track of
--  the group changes and state is discouraged.
--
--  # Shortcuts and Gestures
--
--  `GtkCheckButton` supports the following keyboard shortcuts:
--
--  - <kbd>␣</kbd> or <kbd>Enter</kbd> activates the button.
--
--  # CSS nodes
--
--  ``` checkbutton[.text-button][.grouped] ├── check ╰── [label] ```
--
--  A `GtkCheckButton` has a main node with name checkbutton. If the
--  [propertyGtk.CheckButton:label] or [propertyGtk.CheckButton:child]
--  properties are set, it contains a child widget. The indicator node is named
--  check when no group is set, and radio if the checkbutton is grouped
--  together with other checkbuttons.
--
--  # Accessibility
--
--  `GtkCheckButton` uses the [enumGtk.AccessibleRole.checkbox] role.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Check_Button is

   type Gtk_Check_Button_Record is new Gtk_Widget_Record with null record;
   type Gtk_Check_Button is access all Gtk_Check_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Check_Button);
   procedure Initialize
      (Self : not null access Gtk_Check_Button_Record'Class);
   --  Creates a new `GtkCheckButton`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Check_Button_New return Gtk_Check_Button;
   --  Creates a new `GtkCheckButton`.

   procedure Gtk_New_With_Label
      (Self  : out Gtk_Check_Button;
       Label : UTF8_String := "");
   procedure Initialize_With_Label
      (Self  : not null access Gtk_Check_Button_Record'Class;
       Label : UTF8_String := "");
   --  Creates a new `GtkCheckButton` with the given text.
   --  Initialize_With_Label does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Label the text for the check button.

   function Gtk_Check_Button_New_With_Label
      (Label : UTF8_String := "") return Gtk_Check_Button;
   --  Creates a new `GtkCheckButton` with the given text.
   --  @param Label the text for the check button.

   procedure Gtk_New_With_Mnemonic
      (Self  : out Gtk_Check_Button;
       Label : UTF8_String := "");
   procedure Initialize_With_Mnemonic
      (Self  : not null access Gtk_Check_Button_Record'Class;
       Label : UTF8_String := "");
   --  Creates a new `GtkCheckButton` with the given text and a mnemonic.
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Label The text of the button, with an underscore in front of the
   --  mnemonic character

   function Gtk_Check_Button_New_With_Mnemonic
      (Label : UTF8_String := "") return Gtk_Check_Button;
   --  Creates a new `GtkCheckButton` with the given text and a mnemonic.
   --  @param Label The text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_check_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active
      (Self : not null access Gtk_Check_Button_Record) return Boolean;
   --  Returns whether the check button is active.
   --  @return whether the check button is active

   procedure Set_Active
      (Self    : not null access Gtk_Check_Button_Record;
       Setting : Boolean);
   --  Changes the check buttons active state.
   --  @param Setting the new value to set

   function Get_Child
      (Self : not null access Gtk_Check_Button_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Button or `NULL` if
   --  [propertyCheckbutton:label] is set.
   --  Since: gtk+ 4.8
   --  @return the child widget of Button

   procedure Set_Child
      (Self  : not null access Gtk_Check_Button_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Button.
   --  Note that by using this API, you take full responsibility for setting
   --  up the proper accessibility label and description information for
   --  Button. Most likely, you'll either set the accessibility label or
   --  description for Button explicitly, or you'll set a labelled-by or
   --  described-by relations from Child to Button.
   --  Since: gtk+ 4.8
   --  @param Child the child widget

   function Get_Inconsistent
      (Self : not null access Gtk_Check_Button_Record) return Boolean;
   --  Returns whether the check button is in an inconsistent state.
   --  @return True if Check_Button is currently in an inconsistent state

   procedure Set_Inconsistent
      (Self         : not null access Gtk_Check_Button_Record;
       Inconsistent : Boolean);
   --  Sets the `GtkCheckButton` to inconsistent state.
   --  You should turn off the inconsistent state again if the user checks the
   --  check button. This has to be done manually.
   --  @param Inconsistent True if state is inconsistent

   function Get_Label
      (Self : not null access Gtk_Check_Button_Record) return UTF8_String;
   --  Returns the label of the check button or `NULL` if
   --  [propertyCheckbutton:child] is set.
   --  @return The label Self shows next to the indicator. If no label is
   --  shown, null will be returned.

   procedure Set_Label
      (Self  : not null access Gtk_Check_Button_Record;
       Label : UTF8_String := "");
   --  Sets the text of Self.
   --  If [propertyGtk.CheckButton:use-underline] is True, an underscore in
   --  Label is interpreted as mnemonic indicator, see
   --  [methodGtk.CheckButton.set_use_underline] for details on this behavior.
   --  @param Label The text shown next to the indicator, or null to show no
   --  text

   function Get_Use_Underline
      (Self : not null access Gtk_Check_Button_Record) return Boolean;
   --  Returns whether underlines in the label indicate mnemonics.
   --  @return The value of the [propertyGtk.CheckButton:use-underline]
   --  property. See [methodGtk.CheckButton.set_use_underline] for details on
   --  how to set a new value.

   procedure Set_Use_Underline
      (Self    : not null access Gtk_Check_Button_Record;
       Setting : Boolean);
   --  Sets whether underlines in the label indicate mnemonics.
   --  If Setting is True, an underscore character in Self's label indicates a
   --  mnemonic accelerator key. This behavior is similar to
   --  [propertyGtk.Label:use-underline].
   --  @param Setting the new value to set

   procedure Set_Group
      (Self  : not null access Gtk_Check_Button_Record;
       Group : access Gtk_Check_Button_Record'Class);
   --  Adds Self to the group of Group.
   --  In a group of multiple check buttons, only one button can be active at
   --  a time. The behavior of a checkbutton in a group is also commonly known
   --  as a *radio button*.
   --  Setting the group of a check button also changes the css name of the
   --  indicator widget's CSS node to 'radio'.
   --  Setting up groups in a cycle leads to undefined behavior.
   --  Note that the same effect can be achieved via the [ifaceGtk.Actionable]
   --  API, by using the same action with parameter type and state type 's' for
   --  all buttons in the group, and giving each button its own target value.
   --  @param Group another `GtkCheckButton` to form a group with

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Check_Button_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Check_Button_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Check_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Check_Button_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Check_Button_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Check_Button_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Check_Button_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Check_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Check_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Check_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Check_Button_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Check_Button_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Check_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Check_Button_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Check_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Property : constant Glib.Properties.Property_Boolean;
   --  If the check button is active.
   --
   --  Setting `active` to True will add the `:checked:` state to both the
   --  check button and the indicator CSS node.

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Group_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk_Check_Button
   --  Flags: write
   --  The check button whose group this widget belongs to.

   Inconsistent_Property : constant Glib.Properties.Property_Boolean;
   --  If the check button is in an "in between" state.
   --
   --  The inconsistent state only affects visual appearance, not the
   --  semantics of the button.

   Label_Property : constant Glib.Properties.Property_String;
   --  Text of the label inside the check button, if it contains a label
   --  widget.

   Use_Underline_Property : constant Glib.Properties.Property_Boolean;
   --  If set, an underline in the text indicates that the following character
   --  is to be used as mnemonic.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Check_Button_Void is not null access procedure
     (Self : access Gtk_Check_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Check_Button_Record;
       Call  : Cb_Gtk_Check_Button_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Check_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to when the check button is activated.
   --
   --  The `::activate` signal on `GtkCheckButton` is an action signal and
   --  emitting it causes the button to animate press then release.
   --
   --  Applications should never connect to this signal, but use the
   --  [signalGtk.CheckButton::toggled] signal.
   --
   --  The default bindings for this signal are all forms of the <kbd>␣</kbd>
   --  and <kbd>Enter</kbd> keys.

   Signal_Toggled : constant Glib.Signal_Name := "toggled";
   procedure On_Toggled
      (Self  : not null access Gtk_Check_Button_Record;
       Call  : Cb_Gtk_Check_Button_Void;
       After : Boolean := False);
   procedure On_Toggled
      (Self  : not null access Gtk_Check_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the buttons's [propertyGtk.CheckButton:active] property
   --  changes.

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

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Check_Button_Record, Gtk_Check_Button);
   function "+"
     (Widget : access Gtk_Check_Button_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Check_Button
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Check_Button_Record, Gtk_Check_Button);
   function "+"
     (Widget : access Gtk_Check_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Check_Button
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Check_Button_Record, Gtk_Check_Button);
   function "+"
     (Widget : access Gtk_Check_Button_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Check_Button
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Inconsistent_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inconsistent");
   Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("group");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Check_Button;
