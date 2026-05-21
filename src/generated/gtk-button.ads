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

--  Calls a callback function when the button is clicked.
--
--  <picture> <source srcset="button-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkButton" src="button.png"> </picture>
--  The `GtkButton` widget can hold any valid child widget. That is, it can
--  hold almost any other standard `GtkWidget`. The most commonly used child is
--  the `GtkLabel`.
--
--  # Shortcuts and Gestures
--
--  The following signals have default keybindings:
--
--  - [signalGtk.Button::activate]
--
--  # CSS nodes
--
--  `GtkButton` has a single CSS node with name button. The node will get the
--  style classes .image-button or .text-button, if the content is just an
--  image or label, respectively. It may also receive the .flat style class.
--  When activating a button via the keyboard, the button will temporarily gain
--  the .keyboard-activating style class.
--
--  Other style classes that are commonly used with `GtkButton` include
--  .suggested-action and .destructive-action. In special cases, buttons can be
--  made round by adding the .circular style class.
--
--  Button-like widgets like [classGtk.ToggleButton], [classGtk.MenuButton],
--  [classGtk.VolumeButton], [classGtk.LockButton], [classGtk.ColorButton] or
--  [classGtk.FontButton] use style classes such as .toggle, .popup, .scale,
--  .lock, .color on the button node to differentiate themselves from a plain
--  `GtkButton`.
--
--  # Accessibility
--
--  `GtkButton` uses the [enumGtk.AccessibleRole.button] role.
--
--  <screenshot>gtk-button</screenshot>
--  <group>Buttons and Toggles</group>
--  <gtkada_demo>create_buttons.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Button is

   type Gtk_Button_Record is new Gtk_Widget_Record with null record;
   type Gtk_Button is access all Gtk_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_From_Icon_Name
      (Button    : out Gtk_Button;
       Icon_Name : UTF8_String);
   procedure Initialize_From_Icon_Name
      (Button    : not null access Gtk_Button_Record'Class;
       Icon_Name : UTF8_String);
   --  Creates a new button containing an icon from the current icon theme.
   --  If the icon name isn't known, a "broken image" icon will be displayed
   --  instead. If the current icon theme is changed, the icon will be updated
   --  appropriately.
   --  Initialize_From_Icon_Name does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  @param Icon_Name an icon name

   function Gtk_Button_New_From_Icon_Name
      (Icon_Name : UTF8_String) return Gtk_Button;
   --  Creates a new button containing an icon from the current icon theme.
   --  If the icon name isn't known, a "broken image" icon will be displayed
   --  instead. If the current icon theme is changed, the icon will be updated
   --  appropriately.
   --  @param Icon_Name an icon name

   procedure Gtk_New (Button : out Gtk_Button; Label : UTF8_String := "");
   procedure Initialize
      (Button : not null access Gtk_Button_Record'Class;
       Label  : UTF8_String := "");
   --  Creates a `GtkButton` widget with a `GtkLabel` child.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Label The text you want the `GtkLabel` to hold

   function Gtk_Button_New_With_Label
      (Label : UTF8_String := "") return Gtk_Button;
   --  Creates a `GtkButton` widget with a `GtkLabel` child.
   --  @param Label The text you want the `GtkLabel` to hold

   procedure Gtk_New_With_Mnemonic
      (Button : out Gtk_Button;
       Label  : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Button : not null access Gtk_Button_Record'Class;
       Label  : UTF8_String);
   --  Creates a new `GtkButton` containing a label.
   --  If characters in Label are preceded by an underscore, they are
   --  underlined. If you need a literal underscore character in a label, use
   --  "__" (two underscores). The first underlined character represents a
   --  keyboard accelerator called a mnemonic. Pressing <kbd>Alt</kbd> and that
   --  key activates the button.
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Label The text of the button, with an underscore in front of the
   --  mnemonic character

   function Gtk_Button_New_With_Mnemonic
      (Label : UTF8_String) return Gtk_Button;
   --  Creates a new `GtkButton` containing a label.
   --  If characters in Label are preceded by an underscore, they are
   --  underlined. If you need a literal underscore character in a label, use
   --  "__" (two underscores). The first underlined character represents a
   --  keyboard accelerator called a mnemonic. Pressing <kbd>Alt</kbd> and that
   --  key activates the button.
   --  @param Label The text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Can_Shrink
      (Button : not null access Gtk_Button_Record) return Boolean;
   --  Retrieves whether the button can be smaller than the natural size of
   --  its contents.
   --  Since: gtk+ 4.12
   --  @return true if the button can shrink, and false otherwise

   procedure Set_Can_Shrink
      (Button     : not null access Gtk_Button_Record;
       Can_Shrink : Boolean);
   --  Sets whether the button size can be smaller than the natural size of
   --  its contents.
   --  For text buttons, setting Can_Shrink to true will ellipsize the label.
   --  For icons and custom children, this function has no effect.
   --  Since: gtk+ 4.12
   --  @param Can_Shrink whether the button can shrink

   function Get_Child
      (Button : not null access Gtk_Button_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Button.
   --  @return the child widget of Button

   procedure Set_Child
      (Button : not null access Gtk_Button_Record;
       Child  : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Button.
   --  Note that by using this API, you take full responsibility for setting
   --  up the proper accessibility label and description information for
   --  Button. Most likely, you'll either set the accessibility label or
   --  description for Button explicitly, or you'll set a labelled-by or
   --  described-by relations from Child to Button.
   --  @param Child the child widget

   function Get_Has_Frame
      (Button : not null access Gtk_Button_Record) return Boolean;
   --  Returns whether the button has a frame.
   --  @return True if the button has a frame

   procedure Set_Has_Frame
      (Button    : not null access Gtk_Button_Record;
       Has_Frame : Boolean);
   --  Sets the style of the button.
   --  Buttons can have a flat appearance or have a frame drawn around them.
   --  @param Has_Frame whether the button should have a visible frame

   function Get_Icon_Name
      (Button : not null access Gtk_Button_Record) return UTF8_String;
   --  Returns the icon name of the button.
   --  If the icon name has not been set with [methodGtk.Button.set_icon_name]
   --  the return value will be null. This will be the case if you create an
   --  empty button with [ctorGtk.Button.new] to use as a container.
   --  @return The icon name set via [methodGtk.Button.set_icon_name]

   procedure Set_Icon_Name
      (Button    : not null access Gtk_Button_Record;
       Icon_Name : UTF8_String);
   --  Adds a `GtkImage` with the given icon name as a child.
   --  If Button already contains a child widget, that child widget will be
   --  removed and replaced with the image.
   --  @param Icon_Name An icon name

   function Get_Label
      (Button : not null access Gtk_Button_Record) return UTF8_String;
   --  Fetches the text from the label of the button.
   --  If the label text has not been set with [methodGtk.Button.set_label]
   --  the return value will be null. This will be the case if you create an
   --  empty button with [ctorGtk.Button.new] to use as a container.
   --  @return The text of the label widget. This string is owned by the
   --  widget and must not be modified or freed.

   procedure Set_Label
      (Button : not null access Gtk_Button_Record;
       Label  : UTF8_String);
   --  Sets the text of the label of the button to Label.
   --  This will also clear any previously set labels.
   --  @param Label a string

   function Get_Use_Underline
      (Button : not null access Gtk_Button_Record) return Boolean;
   --  gets whether underlines are interpreted as mnemonics.
   --  See [methodGtk.Button.set_use_underline].
   --  @return True if an embedded underline in the button label indicates the
   --  mnemonic accelerator keys.

   procedure Set_Use_Underline
      (Button        : not null access Gtk_Button_Record;
       Use_Underline : Boolean);
   --  Sets whether to use underlines as mnemonics.
   --  If true, an underline in the text of the button label indicates the
   --  next character should be used for the mnemonic accelerator key.
   --  @param Use_Underline True if underlines in the text indicate mnemonics

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Announce
      (Self     : not null access Gtk_Button_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Button_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Button_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Button_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Button_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Button_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Button_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Button_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Button_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Can_Shrink_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the size of the button can be made smaller than the natural
   --  size of its contents.
   --
   --  For text buttons, setting this property will allow ellipsizing the
   --  label.
   --
   --  If the contents of a button are an icon or a custom widget, setting
   --  this property has no effect.

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Has_Frame_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the button has a frame.

   Icon_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the icon used to automatically populate the button.

   Label_Property : constant Glib.Properties.Property_String;
   --  Text of the label inside the button, if the button contains a label
   --  widget.

   Use_Underline_Property : constant Glib.Properties.Property_Boolean;
   --  If set, an underline in the text indicates that the following character
   --  is to be used as mnemonic.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Button_Void is not null access procedure (Self : access Gtk_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Button_Record;
       Call  : Cb_Gtk_Button_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to animate press then release.
   --
   --  This is an action signal. Applications should never connect to this
   --  signal, but use the [signalGtk.Button::clicked] signal.
   --
   --  The default bindings for this signal are all forms of the <kbd>␣</kbd>
   --  and <kbd>Enter</kbd> keys.

   Signal_Clicked : constant Glib.Signal_Name := "clicked";
   procedure On_Clicked
      (Self  : not null access Gtk_Button_Record;
       Call  : Cb_Gtk_Button_Void;
       After : Boolean := False);
   procedure On_Clicked
      (Self  : not null access Gtk_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the button has been activated (pressed and released).

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Accessible"
   --
   --  - "ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Button_Record, Gtk_Button);
   function "+"
     (Widget : access Gtk_Button_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Button
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Button_Record, Gtk_Button);
   function "+"
     (Widget : access Gtk_Button_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Button
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Has_Frame_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-frame");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
   Can_Shrink_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("can-shrink");
end Gtk.Button;
