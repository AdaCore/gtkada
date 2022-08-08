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
--  A Gtk.Check_Button.Gtk_Check_Button places a discrete
--  Gtk.Toggle_Button.Gtk_Toggle_Button next to a widget, (usually a
--  Gtk.Label.Gtk_Label). See the section on
--  Gtk.Toggle_Button.Gtk_Toggle_Button widgets for more information about
--  toggle/check buttons.
--
--  The important signal ( Gtk.Toggle_Button.Gtk_Toggle_Button::toggled ) is
--  also inherited from Gtk.Toggle_Button.Gtk_Toggle_Button.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> checkbutton ├── check ╰── <child> ]|
--
--  A GtkCheckButton with indicator (see Gtk.Toggle_Button.Set_Mode) has a
--  main CSS node with name checkbutton and a subnode with name check.
--
--  |[<!-- language="plain" --> button.check ├── check ╰── <child> ]|
--
--  A GtkCheckButton without indicator changes the name of its main node to
--  button and adds a .check style class to it. The subnode is invisible in
--  this case.
--
--  </description>
--  <screenshot>gtk-check_button</screenshot>
--  <group>Buttons and Toggles</group>
--  <testgtk>create_check_buttons.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Types;        use Glib.Types;
with Glib.Variant;      use Glib.Variant;
with Gtk.Action;        use Gtk.Action;
with Gtk.Actionable;    use Gtk.Actionable;
with Gtk.Activatable;   use Gtk.Activatable;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;

package Gtk.Check_Button is

   type Gtk_Check_Button_Record is new Gtk_Toggle_Button_Record with null record;
   type Gtk_Check_Button is access all Gtk_Check_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Check_Button : out Gtk_Check_Button;
       Label        : UTF8_String := "");
   procedure Initialize
      (Check_Button : not null access Gtk_Check_Button_Record'Class;
       Label        : UTF8_String := "");
   --  Create a check button. if Label is null, then no widget is associated
   --  with the button, and any widget can be added to the button (with
   --  Gtk.Container.Add).
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "label": the text for the check button.

   function Gtk_Check_Button_New_With_Label
      (Label : UTF8_String := "") return Gtk_Check_Button;
   --  Create a check button. if Label is null, then no widget is associated
   --  with the button, and any widget can be added to the button (with
   --  Gtk.Container.Add).
   --  "label": the text for the check button.

   procedure Gtk_New_With_Mnemonic
      (Check_Button : out Gtk_Check_Button;
       Label        : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Check_Button : not null access Gtk_Check_Button_Record'Class;
       Label        : UTF8_String);
   --  Creates a new Gtk.Check_Button.Gtk_Check_Button containing a label. The
   --  label will be created using Gtk.Label.Gtk_New_With_Mnemonic, so
   --  underscores in Label indicate the mnemonic for the check button.
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "label": The text of the button, with an underscore in front of the
   --  mnemonic character

   function Gtk_Check_Button_New_With_Mnemonic
      (Label : UTF8_String) return Gtk_Check_Button;
   --  Creates a new Gtk.Check_Button.Gtk_Check_Button containing a label. The
   --  label will be created using Gtk.Label.Gtk_New_With_Mnemonic, so
   --  underscores in Label indicate the mnemonic for the check button.
   --  "label": The text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_check_button_get_type");

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Check_Button_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Check_Button_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Check_Button_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Check_Button_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Check_Button_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Check_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Check_Button_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Check_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Check_Button_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Check_Button_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Check_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

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
     (Gtk.Actionable.Gtk_Actionable, Gtk_Check_Button_Record, Gtk_Check_Button);
   function "+"
     (Widget : access Gtk_Check_Button_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Check_Button
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Check_Button_Record, Gtk_Check_Button);
   function "+"
     (Widget : access Gtk_Check_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Check_Button
   renames Implements_Gtk_Activatable.To_Object;

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

end Gtk.Check_Button;
