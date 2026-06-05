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

--  Shows a button which remains "pressed-in" when clicked.
--
--  <picture> <source srcset="toggle-button-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="Example GtkToggleButtons"
--  src="toggle-button.png"> </picture>
--  Clicking again will cause the toggle button to return to its normal state.
--
--  A toggle button is created by calling either [ctorGtk.ToggleButton.new] or
--  [ctorGtk.ToggleButton.new_with_label]. If using the former, it is advisable
--  to pack a widget, (such as a `GtkLabel` and/or a `GtkImage`), into the
--  toggle button's container. (See [classGtk.Button] for more information).
--
--  The state of a `GtkToggleButton` can be set specifically using
--  [methodGtk.ToggleButton.set_active], and retrieved using
--  [methodGtk.ToggleButton.get_active].
--
--  ## Grouping
--
--  Toggle buttons can be grouped together, to form mutually exclusive groups
--  - only one of the buttons can be toggled at a time, and toggling another
--  one will switch the currently toggled one off.
--
--  To add a `GtkToggleButton` to a group, use
--  [methodGtk.ToggleButton.set_group].
--
--  ## CSS nodes
--
--  `GtkToggleButton` has a single CSS node with name button. To differentiate
--  it from a plain `GtkButton`, it gets the `.toggle` style class.
--
--  ## Accessibility
--
--  `GtkToggleButton` uses the [enumGtk.AccessibleRole.toggle_button] role.
--
--  ## Creating two `GtkToggleButton` widgets.
--
--  ```c static void output_state (GtkToggleButton *source, gpointer
--  user_data) { g_print ("Toggle button "%s" is active: %s",
--  gtk_button_get_label (GTK_BUTTON (source)), gtk_toggle_button_get_active
--  (source) ? "Yes" : "No"); }
--
--  static void make_toggles (void) { GtkWidget *window, *toggle1, *toggle2;
--  GtkWidget *box; const char *text;
--
--  window = gtk_window_new (); box = gtk_box_new (GTK_ORIENTATION_VERTICAL,
--  12);
--
--  text = "Hi, I'm toggle button one"; toggle1 =
--  gtk_toggle_button_new_with_label (text);
--
--  g_signal_connect (toggle1, "toggled", G_CALLBACK (output_state), NULL);
--  gtk_box_append (GTK_BOX (box), toggle1);
--
--  text = "Hi, I'm toggle button two"; toggle2 =
--  gtk_toggle_button_new_with_label (text); g_signal_connect (toggle2,
--  "toggled", G_CALLBACK (output_state), NULL); gtk_box_append (GTK_BOX (box),
--  toggle2);
--
--  gtk_window_set_child (GTK_WINDOW (window), box); gtk_window_present
--  (GTK_WINDOW (window)); } ```

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Button;            use Gtk.Button;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;

package Gtk.Toggle_Button is

   type Gtk_Toggle_Button_Record is new Gtk_Button_Record with null record;
   type Gtk_Toggle_Button is access all Gtk_Toggle_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Toggle_Button);
   procedure Initialize
      (Self : not null access Gtk_Toggle_Button_Record'Class);
   --  Creates a new toggle button.
   --  A widget should be packed into the button, as in [ctorGtk.Button.new].
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Toggle_Button_New return Gtk_Toggle_Button;
   --  Creates a new toggle button.
   --  A widget should be packed into the button, as in [ctorGtk.Button.new].

   procedure Gtk_New_With_Label
      (Self  : out Gtk_Toggle_Button;
       Label : UTF8_String);
   procedure Initialize_With_Label
      (Self  : not null access Gtk_Toggle_Button_Record'Class;
       Label : UTF8_String);
   --  Creates a new toggle button with a text label.
   --  Initialize_With_Label does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Label a string containing the message to be placed in the toggle
   --  button.

   function Gtk_Toggle_Button_New_With_Label
      (Label : UTF8_String) return Gtk_Toggle_Button;
   --  Creates a new toggle button with a text label.
   --  @param Label a string containing the message to be placed in the toggle
   --  button.

   procedure Gtk_New_With_Mnemonic
      (Self  : out Gtk_Toggle_Button;
       Label : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Self  : not null access Gtk_Toggle_Button_Record'Class;
       Label : UTF8_String);
   --  Creates a new `GtkToggleButton` containing a label.
   --  The label will be created using [ctorGtk.Label.new_with_mnemonic], so
   --  underscores in Label indicate the mnemonic for the button.
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Label the text of the button, with an underscore in front of the
   --  mnemonic character

   function Gtk_Toggle_Button_New_With_Mnemonic
      (Label : UTF8_String) return Gtk_Toggle_Button;
   --  Creates a new `GtkToggleButton` containing a label.
   --  The label will be created using [ctorGtk.Label.new_with_mnemonic], so
   --  underscores in Label indicate the mnemonic for the button.
   --  @param Label the text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_toggle_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active
      (Self : not null access Gtk_Toggle_Button_Record) return Boolean;
   --  Queries a `GtkToggleButton` and returns its current state.
   --  Returns True if the toggle button is pressed in and False if it is
   --  raised.
   --  @return whether the button is pressed

   procedure Set_Active
      (Self      : not null access Gtk_Toggle_Button_Record;
       Is_Active : Boolean);
   --  Sets the status of the toggle button.
   --  Set to True if you want the `GtkToggleButton` to be "pressed in", and
   --  False to raise it.
   --  If the status of the button changes, this action causes the
   --  [signalGtk.ToggleButton::toggled] signal to be emitted.
   --  @param Is_Active True or False.

   procedure Set_Group
      (Self  : not null access Gtk_Toggle_Button_Record;
       Group : access Gtk_Toggle_Button_Record'Class);
   --  Adds Self to the group of Group.
   --  In a group of multiple toggle buttons, only one button can be active at
   --  a time.
   --  Setting up groups in a cycle leads to undefined behavior.
   --  Note that the same effect can be achieved via the [ifaceGtk.Actionable]
   --  API, by using the same action with parameter type and state type 's' for
   --  all buttons in the group, and giving each button its own target value.
   --  @param Group another `GtkToggleButton` to form a group with

   procedure Toggled (Self : not null access Gtk_Toggle_Button_Record);
   pragma Obsolescent (Toggled);
   --  Emits the ::toggled signal on the `GtkToggleButton`.
   --  Deprecated since 4.10, 1

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Toggle_Button_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Toggle_Button_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Toggle_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Toggle_Button_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Toggle_Button_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Toggle_Button_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Toggle_Button_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Toggle_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Toggle_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Toggle_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Toggle_Button_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Toggle_Button_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Toggle_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Toggle_Button_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Toggle_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Property : constant Glib.Properties.Property_Boolean;
   --  If the toggle button should be pressed in.

   Group_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk_Toggle_Button
   --  Flags: write
   --  The toggle button whose group this widget belongs to.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Toggle_Button_Void is not null access procedure
     (Self : access Gtk_Toggle_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Toggled : constant Glib.Signal_Name := "toggled";
   procedure On_Toggled
      (Self  : not null access Gtk_Toggle_Button_Record;
       Call  : Cb_Gtk_Toggle_Button_Void;
       After : Boolean := False);
   procedure On_Toggled
      (Self  : not null access Gtk_Toggle_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever the `GtkToggleButton`'s state is changed.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Toggle_Button_Record, Gtk_Toggle_Button);
   function "+"
     (Widget : access Gtk_Toggle_Button_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Toggle_Button
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Toggle_Button_Record, Gtk_Toggle_Button);
   function "+"
     (Widget : access Gtk_Toggle_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Toggle_Button
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Toggle_Button_Record, Gtk_Toggle_Button);
   function "+"
     (Widget : access Gtk_Toggle_Button_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Toggle_Button
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("group");
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Toggle_Button;
