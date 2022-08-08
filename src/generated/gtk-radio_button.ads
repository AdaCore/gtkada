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
--  A single radio button performs the same basic function as a
--  Gtk.Check_Button.Gtk_Check_Button, as its position in the object hierarchy
--  reflects. It is only when multiple radio buttons are grouped together that
--  they become a different user interface component in their own right.
--
--  Every radio button is a member of some group of radio buttons. When one is
--  selected, all other radio buttons in the same group are deselected. A
--  Gtk.Radio_Button.Gtk_Radio_Button is one way of giving the user a choice
--  from many options.
--
--  Radio button widgets are created with gtk_radio_button_new, passing null
--  as the argument if this is the first radio button in a group. In subsequent
--  calls, the group you wish to add this button to should be passed as an
--  argument. Optionally, Gtk.Radio_Button.Gtk_New can be used if you want a
--  text label on the radio button.
--
--  Alternatively, when adding widgets to an existing group of radio buttons,
--  use gtk_radio_button_new_from_widget with a
--  Gtk.Radio_Button.Gtk_Radio_Button that already has a group assigned to it.
--  The convenience function Gtk.Radio_Button.Gtk_New is also provided.
--
--  To retrieve the group a Gtk.Radio_Button.Gtk_Radio_Button is assigned to,
--  use Gtk.Radio_Button.Get_Group.
--
--  To remove a Gtk.Radio_Button.Gtk_Radio_Button from one group and make it
--  part of a new one, use Gtk.Radio_Button.Set_Group.
--
--  The group list does not need to be freed, as each
--  Gtk.Radio_Button.Gtk_Radio_Button will remove itself and its list item when
--  it is destroyed.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> radiobutton ├── radio ╰── <child> ]|
--
--  A GtkRadioButton with indicator (see Gtk.Toggle_Button.Set_Mode) has a
--  main CSS node with name radiobutton and a subnode with name radio.
--
--  |[<!-- language="plain" --> button.radio ├── radio ╰── <child> ]|
--
--  A GtkRadioButton without indicator changes the name of its main node to
--  button and adds a .radio style class to it. The subnode is invisible in
--  this case.
--
--  ## How to create a group of two radio buttons.
--
--  |[<!-- language="C" --> void create_radio_buttons (void) {
--
--  GtkWidget *window, *radio1, *radio2, *box, *entry; window = gtk_window_new
--  (GTK_WINDOW_TOPLEVEL); box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 2);
--  gtk_box_set_homogeneous (GTK_BOX (box), TRUE);
--
--  // Create a radio button with a GtkEntry widget radio1 =
--  gtk_radio_button_new (NULL); entry = gtk_entry_new (); gtk_container_add
--  (GTK_CONTAINER (radio1), entry);
--
--  // Create a radio button with a label radio2 =
--  gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (radio1),
--  "I'm the second radio button.");
--
--  // Pack them into a box, then show all the widgets gtk_box_pack_start
--  (GTK_BOX (box), radio1); gtk_box_pack_start (GTK_BOX (box), radio2);
--  gtk_container_add (GTK_CONTAINER (window), box); gtk_widget_show_all
--  (window); return; } ]|
--
--  When an unselected button in the group is clicked the clicked button
--  receives the Gtk.Toggle_Button.Gtk_Toggle_Button::toggled signal, as does
--  the previously selected button. Inside the
--  Gtk.Toggle_Button.Gtk_Toggle_Button::toggled handler,
--  Gtk.Toggle_Button.Get_Active can be used to determine if the button has
--  been selected or deselected.
--
--  </description>
--  <screenshot>gtk-radio_button</screenshot>
--  <group>Buttons and Toggles</group>
--  <testgtk>create_radio_button.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Properties;  use Glib.Properties;
with Glib.Types;       use Glib.Types;
with Glib.Variant;     use Glib.Variant;
with Gtk.Action;       use Gtk.Action;
with Gtk.Actionable;   use Gtk.Actionable;
with Gtk.Activatable;  use Gtk.Activatable;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Widget;       use Gtk.Widget;

package Gtk.Radio_Button is

   type Gtk_Radio_Button_Record is new Gtk_Check_Button_Record with null record;
   type Gtk_Radio_Button is access all Gtk_Radio_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Radio_Button : out Gtk_Radio_Button;
       Group        : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label        : UTF8_String := "");
   procedure Initialize
      (Radio_Button : not null access Gtk_Radio_Button_Record'Class;
       Group        : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label        : UTF8_String := "");
   --  Creates or initializes a new radio button, belonging to Group. If Label
   --  is left as the empty string, then the button will not have any child and
   --  you are free to put any thing you want in it, including a pixmap. To
   --  initialize the group (when creating the first button), leave Group to
   --  the Null_List. You can later get the new group that is created with a
   --  call to the Get_Group subprogram below.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "group": an existing radio button group, or null if you are creating a
   --  new group.
   --  "label": the text label to display next to the radio button.

   function Gtk_Radio_Button_New_With_Label
      (Group : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label : UTF8_String := "") return Gtk_Radio_Button;
   --  Creates or initializes a new radio button, belonging to Group. If Label
   --  is left as the empty string, then the button will not have any child and
   --  you are free to put any thing you want in it, including a pixmap. To
   --  initialize the group (when creating the first button), leave Group to
   --  the Null_List. You can later get the new group that is created with a
   --  call to the Get_Group subprogram below.
   --  "group": an existing radio button group, or null if you are creating a
   --  new group.
   --  "label": the text label to display next to the radio button.

   procedure Gtk_New
      (Radio_Button : out Gtk_Radio_Button;
       Group        : access Gtk_Radio_Button_Record'Class;
       Label        : UTF8_String := "");
   procedure Initialize
      (Radio_Button : not null access Gtk_Radio_Button_Record'Class;
       Group        : access Gtk_Radio_Button_Record'Class;
       Label        : UTF8_String := "");
   --  Creates a new Gtk.Radio_Button.Gtk_Radio_Button with a text label,
   --  adding it to the same group as Radio_Group_Member.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "Group": widget to get radio group from or null
   --  "label": a text string to display next to the radio button.

   function Gtk_Radio_Button_New_With_Label_From_Widget
      (Group : access Gtk_Radio_Button_Record'Class;
       Label : UTF8_String := "") return Gtk_Radio_Button;
   --  Creates a new Gtk.Radio_Button.Gtk_Radio_Button with a text label,
   --  adding it to the same group as Radio_Group_Member.
   --  "Group": widget to get radio group from or null
   --  "label": a text string to display next to the radio button.

   procedure Gtk_New_With_Mnemonic
      (Radio_Button : out Gtk_Radio_Button;
       Group        : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label        : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Radio_Button : not null access Gtk_Radio_Button_Record'Class;
       Group        : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label        : UTF8_String);
   --  Creates a new Gtk.Radio_Button.Gtk_Radio_Button containing a label,
   --  adding it to the same group as Group. The label will be created using
   --  Gtk.Label.Gtk_New_With_Mnemonic, so underscores in Label indicate the
   --  mnemonic for the button.
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "group": the radio button group, or null
   --  "label": the text of the button, with an underscore in front of the
   --  mnemonic character

   procedure Gtk_New_With_Mnemonic
      (Radio_Button : out Gtk_Radio_Button;
       Group        : access Gtk_Radio_Button_Record'Class;
       Label        : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Radio_Button : not null access Gtk_Radio_Button_Record'Class;
       Group        : access Gtk_Radio_Button_Record'Class;
       Label        : UTF8_String);
   --  Creates a new Gtk.Radio_Button.Gtk_Radio_Button containing a label. The
   --  label will be created using Gtk.Label.Gtk_New_With_Mnemonic, so
   --  underscores in Label indicate the mnemonic for the button.
   --  To initialize a new group (when creating the first button), you should
   --  pass it null or a button that has not been created with Gtk_New, as in
   --  the example below.
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "Group": widget to get radio group from or null
   --  "label": the text of the button, with an underscore in front of the
   --  mnemonic character

   function Gtk_Radio_Button_New_With_Mnemonic
      (Group : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label : UTF8_String) return Gtk_Radio_Button;
   --  Creates a new Gtk.Radio_Button.Gtk_Radio_Button containing a label,
   --  adding it to the same group as Group. The label will be created using
   --  Gtk.Label.Gtk_New_With_Mnemonic, so underscores in Label indicate the
   --  mnemonic for the button.
   --  "group": the radio button group, or null
   --  "label": the text of the button, with an underscore in front of the
   --  mnemonic character

   function Gtk_Radio_Button_New_With_Mnemonic_From_Widget
      (Group : access Gtk_Radio_Button_Record'Class;
       Label : UTF8_String) return Gtk_Radio_Button;
   --  Creates a new Gtk.Radio_Button.Gtk_Radio_Button containing a label. The
   --  label will be created using Gtk.Label.Gtk_New_With_Mnemonic, so
   --  underscores in Label indicate the mnemonic for the button.
   --  To initialize a new group (when creating the first button), you should
   --  pass it null or a button that has not been created with Gtk_New, as in
   --  the example below.
   --  "Group": widget to get radio group from or null
   --  "label": the text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_radio_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Group
      (Radio_Button : not null access Gtk_Radio_Button_Record)
       return Gtk.Widget.Widget_SList.GSlist;
   --  Retrieves the group assigned to a radio button.

   procedure Set_Group
      (Radio_Button : not null access Gtk_Radio_Button_Record;
       Group        : Gtk.Widget.Widget_SList.GSlist);
   --  Sets a Gtk.Radio_Button.Gtk_Radio_Button's group. It should be noted
   --  that this does not change the layout of your interface in any way, so if
   --  you are changing the group, it is likely you will need to re-arrange the
   --  user interface to reflect these changes.
   --  "group": an existing radio button group, such as one returned from
   --  Gtk.Radio_Button.Get_Group, or null.

   procedure Join_Group
      (Radio_Button : not null access Gtk_Radio_Button_Record;
       Group_Source : access Gtk_Radio_Button_Record'Class);
   --  Joins a Gtk.Radio_Button.Gtk_Radio_Button object to the group of
   --  another Gtk.Radio_Button.Gtk_Radio_Button object
   --  Use this in language bindings instead of the Gtk.Radio_Button.Get_Group
   --  and Gtk.Radio_Button.Set_Group methods
   --  A common way to set up a group of radio buttons is the following:
   --  |[<!-- language="C" --> GtkRadioButton *radio_button; GtkRadioButton
   --  *last_button;
   --  while (some_condition) { radio_button = gtk_radio_button_new (NULL);
   --  gtk_radio_button_join_group (radio_button, last_button); last_button =
   --  radio_button; } ]|
   --  Since: gtk+ 3.0
   --  "group_source": a radio button object whos group we are joining, or
   --  null to remove the radio button from its group

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Radio_Button_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Radio_Button_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Radio_Button_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Radio_Button_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Radio_Button_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Radio_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Radio_Button_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Radio_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Radio_Button_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Radio_Button_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Radio_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Group_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk_Radio_Button
   --  Flags: write
   --  Sets a new group for a radio button.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Radio_Button_Void is not null access procedure
     (Self : access Gtk_Radio_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Group_Changed : constant Glib.Signal_Name := "group-changed";
   procedure On_Group_Changed
      (Self  : not null access Gtk_Radio_Button_Record;
       Call  : Cb_Gtk_Radio_Button_Void;
       After : Boolean := False);
   procedure On_Group_Changed
      (Self  : not null access Gtk_Radio_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the group of radio buttons that a radio button belongs to
   --  changes. This is emitted when a radio button switches from being alone
   --  to being part of a group of 2 or more buttons, or vice-versa, and when a
   --  button is moved from one group of 2 or more buttons to a different one,
   --  but not when the composition of the group that a button belongs to
   --  changes.

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
     (Gtk.Actionable.Gtk_Actionable, Gtk_Radio_Button_Record, Gtk_Radio_Button);
   function "+"
     (Widget : access Gtk_Radio_Button_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Radio_Button
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Radio_Button_Record, Gtk_Radio_Button);
   function "+"
     (Widget : access Gtk_Radio_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Radio_Button
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Radio_Button_Record, Gtk_Radio_Button);
   function "+"
     (Widget : access Gtk_Radio_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Radio_Button
   renames Implements_Gtk_Buildable.To_Object;

private
   Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("group");
end Gtk.Radio_Button;
