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
--  A Gtk.Toggle_Button.Gtk_Toggle_Button is a Gtk.Button.Gtk_Button which
--  will remain "pressed-in" when clicked. Clicking again will cause the toggle
--  button to return to its normal state.
--
--  A toggle button is created by calling either gtk_toggle_button_new or
--  Gtk.Toggle_Button.Gtk_New. If using the former, it is advisable to pack a
--  widget, (such as a Gtk.Label.Gtk_Label and/or a Gtk.Image.Gtk_Image), into
--  the toggle button's container. (See Gtk.Button.Gtk_Button for more
--  information).
--
--  The state of a Gtk.Toggle_Button.Gtk_Toggle_Button can be set specifically
--  using Gtk.Toggle_Button.Set_Active, and retrieved using
--  Gtk.Toggle_Button.Get_Active.
--
--  To simply switch the state of a toggle button, use
--  Gtk.Toggle_Button.Toggled.
--
--  # CSS nodes
--
--  GtkToggleButton has a single CSS node with name button. To differentiate
--  it from a plain Gtk.Button.Gtk_Button, it gets the .toggle style class.
--
--  ## Creating two Gtk.Toggle_Button.Gtk_Toggle_Button widgets.
--
--  |[<!-- language="C" --> static void output_state (GtkToggleButton *source,
--  gpointer user_data) { printf ("Active: %d\n", gtk_toggle_button_get_active
--  (source)); }
--
--  void make_toggles (void) { GtkWidget *window, *toggle1, *toggle2;
--  GtkWidget *box; const char *text;
--
--  window = gtk_window_new (GTK_WINDOW_TOPLEVEL); box = gtk_box_new
--  (GTK_ORIENTATION_VERTICAL, 12);
--
--  text = "Hi, I'm a toggle button."; toggle1 =
--  gtk_toggle_button_new_with_label (text);
--
--  // Makes this toggle button invisible gtk_toggle_button_set_mode
--  (GTK_TOGGLE_BUTTON (toggle1), TRUE);
--
--  g_signal_connect (toggle1, "toggled", G_CALLBACK (output_state), NULL);
--  gtk_container_add (GTK_CONTAINER (box), toggle1);
--
--  text = "Hi, I'm a toggle button."; toggle2 =
--  gtk_toggle_button_new_with_label (text); gtk_toggle_button_set_mode
--  (GTK_TOGGLE_BUTTON (toggle2), FALSE); g_signal_connect (toggle2, "toggled",
--  G_CALLBACK (output_state), NULL); gtk_container_add (GTK_CONTAINER (box),
--  toggle2);
--
--  gtk_container_add (GTK_CONTAINER (window), box); gtk_widget_show_all
--  (window); } ]|
--
--  </description>
--  <description>
--  You should consider using a Gtk_Check_Button instead, since it looks nicer
--  and provides more visual clues that the button can be toggled.
--
--  </description>
--  <screenshot>gtk-toggle_button</screenshot>
--  <group>Buttons and Toggles</group>
--  <testgtk>create_toggle_buttons.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Glib.Variant;    use Glib.Variant;
with Gtk.Action;      use Gtk.Action;
with Gtk.Actionable;  use Gtk.Actionable;
with Gtk.Activatable; use Gtk.Activatable;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Button;      use Gtk.Button;

package Gtk.Toggle_Button is

   type Gtk_Toggle_Button_Record is new Gtk_Button_Record with null record;
   type Gtk_Toggle_Button is access all Gtk_Toggle_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Toggle_Button : out Gtk_Toggle_Button;
       Label         : UTF8_String := "");
   procedure Initialize
      (Toggle_Button : not null access Gtk_Toggle_Button_Record'Class;
       Label         : UTF8_String := "");
   --  Initialize a button. If Label is "", then no label is created inside
   --  the button and you will have to provide your own child through a call to
   --  Gtk.Container.Add. This is the recommended way to put a pixmap inside a
   --  toggle button.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "label": a string containing the message to be placed in the toggle
   --  button.

   function Gtk_Toggle_Button_New_With_Label
      (Label : UTF8_String := "") return Gtk_Toggle_Button;
   --  Initialize a button. If Label is "", then no label is created inside
   --  the button and you will have to provide your own child through a call to
   --  Gtk.Container.Add. This is the recommended way to put a pixmap inside a
   --  toggle button.
   --  "label": a string containing the message to be placed in the toggle
   --  button.

   procedure Gtk_New_With_Mnemonic
      (Toggle_Button : out Gtk_Toggle_Button;
       Label         : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Toggle_Button : not null access Gtk_Toggle_Button_Record'Class;
       Label         : UTF8_String);
   --  Creates a new Gtk.Toggle_Button.Gtk_Toggle_Button containing a label.
   --  The label will be created using Gtk.Label.Gtk_New_With_Mnemonic, so
   --  underscores in Label indicate the mnemonic for the button.
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "label": the text of the button, with an underscore in front of the
   --  mnemonic character

   function Gtk_Toggle_Button_New_With_Mnemonic
      (Label : UTF8_String) return Gtk_Toggle_Button;
   --  Creates a new Gtk.Toggle_Button.Gtk_Toggle_Button containing a label.
   --  The label will be created using Gtk.Label.Gtk_New_With_Mnemonic, so
   --  underscores in Label indicate the mnemonic for the button.
   --  "label": the text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_toggle_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active
      (Toggle_Button : not null access Gtk_Toggle_Button_Record)
       return Boolean;
   --  Queries a Gtk.Toggle_Button.Gtk_Toggle_Button and returns its current
   --  state. Returns True if the toggle button is pressed in and False if it
   --  is raised.

   procedure Set_Active
      (Toggle_Button : not null access Gtk_Toggle_Button_Record;
       Is_Active     : Boolean);
   --  Change the state of the button. When Is_Active is True, the button is
   --  drawn as a pressed button
   --  "is_active": True or False.

   function Get_Inconsistent
      (Toggle_Button : not null access Gtk_Toggle_Button_Record)
       return Boolean;
   --  Gets the value set by Gtk.Toggle_Button.Set_Inconsistent.

   procedure Set_Inconsistent
      (Toggle_Button : not null access Gtk_Toggle_Button_Record;
       Setting       : Boolean := True);
   --  If the user has selected a range of elements (such as some text or
   --  spreadsheet cells) that are affected by a toggle button, and the current
   --  values in that range are inconsistent, you may want to display the
   --  toggle in an "in between" state. This function turns on "in between"
   --  display. Normally you would turn off the inconsistent state again if the
   --  user toggles the toggle button. This has to be done manually,
   --  Gtk.Toggle_Button.Set_Inconsistent only affects visual appearance, it
   --  doesn't affect the semantics of the button.
   --  "setting": True if state is inconsistent

   function Get_Mode
      (Toggle_Button : not null access Gtk_Toggle_Button_Record)
       return Boolean;
   --  Retrieves whether the button is displayed as a separate indicator and
   --  label. See Gtk.Toggle_Button.Set_Mode.

   procedure Set_Mode
      (Toggle_Button  : not null access Gtk_Toggle_Button_Record;
       Draw_Indicator : Boolean);
   --  Sets whether the button is displayed as a separate indicator and label.
   --  You can call this function on a checkbutton or a radiobutton with
   --  Draw_Indicator = False to make the button look like a normal button.
   --  This can be used to create linked strip of buttons that work like a
   --  Gtk.Stack_Switcher.Gtk_Stack_Switcher.
   --  This function only affects instances of classes like
   --  Gtk.Check_Button.Gtk_Check_Button and Gtk.Radio_Button.Gtk_Radio_Button
   --  that derive from Gtk.Toggle_Button.Gtk_Toggle_Button, not instances of
   --  Gtk.Toggle_Button.Gtk_Toggle_Button itself.
   --  "draw_indicator": if True, draw the button as a separate indicator and
   --  label; if False, draw the button like a normal button

   procedure Toggled
      (Toggle_Button : not null access Gtk_Toggle_Button_Record);
   --  Emits the Gtk.Toggle_Button.Gtk_Toggle_Button::toggled signal on the
   --  Gtk.Toggle_Button.Gtk_Toggle_Button. There is no good reason for an
   --  application ever to call this function.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Toggle_Button_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Toggle_Button_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Toggle_Button_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Toggle_Button_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Toggle_Button_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Toggle_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Toggle_Button_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Toggle_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Toggle_Button_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Toggle_Button_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Toggle_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Property : constant Glib.Properties.Property_Boolean;

   Draw_Indicator_Property : constant Glib.Properties.Property_Boolean;

   Inconsistent_Property : constant Glib.Properties.Property_Boolean;

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
   --  Should be connected if you wish to perform an action whenever the
   --  Gtk.Toggle_Button.Gtk_Toggle_Button's state is changed.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Actionable"
   --
   --  - "Activatable"
   --
   --  - "Buildable"

   package Implements_Gtk_Actionable is new Glib.Types.Implements
     (Gtk.Actionable.Gtk_Actionable, Gtk_Toggle_Button_Record, Gtk_Toggle_Button);
   function "+"
     (Widget : access Gtk_Toggle_Button_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Toggle_Button
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Toggle_Button_Record, Gtk_Toggle_Button);
   function "+"
     (Widget : access Gtk_Toggle_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Toggle_Button
   renames Implements_Gtk_Activatable.To_Object;

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

private
   Inconsistent_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inconsistent");
   Draw_Indicator_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("draw-indicator");
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Toggle_Button;
