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
--  A Gtk.Check_Menu_Item.Gtk_Check_Menu_Item is a menu item that maintains
--  the state of a boolean value in addition to a Gtk.Menu_Item.Gtk_Menu_Item
--  usual role in activating application code.
--
--  A check box indicating the state of the boolean value is displayed at the
--  left side of the Gtk.Menu_Item.Gtk_Menu_Item. Activating the
--  Gtk.Menu_Item.Gtk_Menu_Item toggles the value.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> menuitem ├── check.left ╰── <child> ]|
--
--  GtkCheckMenuItem has a main CSS node with name menuitem, and a subnode
--  with name check, which gets the .left or .right style class.
--
--  </description>

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
with Gtk.Menu_Item;   use Gtk.Menu_Item;

package Gtk.Check_Menu_Item is

   type Gtk_Check_Menu_Item_Record is new Gtk_Menu_Item_Record with null record;
   type Gtk_Check_Menu_Item is access all Gtk_Check_Menu_Item_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Check_Menu_Item : out Gtk_Check_Menu_Item;
       Label           : UTF8_String := "");
   procedure Initialize
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record'Class;
       Label           : UTF8_String := "");
   --  Creates a new Gtk.Check_Menu_Item.Gtk_Check_Menu_Item with a label.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "label": the string to use for the label.

   function Gtk_Check_Menu_Item_New_With_Label
      (Label : UTF8_String := "") return Gtk_Check_Menu_Item;
   --  Creates a new Gtk.Check_Menu_Item.Gtk_Check_Menu_Item with a label.
   --  "label": the string to use for the label.

   procedure Gtk_New_With_Mnemonic
      (Check_Menu_Item : out Gtk_Check_Menu_Item;
       Label           : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record'Class;
       Label           : UTF8_String);
   --  Creates a new Gtk.Check_Menu_Item.Gtk_Check_Menu_Item containing a
   --  label. The label will be created using Gtk.Label.Gtk_New_With_Mnemonic,
   --  so underscores in Label indicate the mnemonic for the menu item.
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "label": The text of the button, with an underscore in front of the
   --  character

   function Gtk_Check_Menu_Item_New_With_Mnemonic
      (Label : UTF8_String) return Gtk_Check_Menu_Item;
   --  Creates a new Gtk.Check_Menu_Item.Gtk_Check_Menu_Item containing a
   --  label. The label will be created using Gtk.Label.Gtk_New_With_Mnemonic,
   --  so underscores in Label indicate the mnemonic for the menu item.
   --  "label": The text of the button, with an underscore in front of the
   --  character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_check_menu_item_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record)
       return Boolean;
   --  Returns whether the check menu item is active. See
   --  gtk_check_menu_item_set_active ().

   procedure Set_Active
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record;
       Is_Active       : Boolean);
   --  Sets the active state of the menu item's check box.
   --  "is_active": boolean value indicating whether the check box is active.

   function Get_Draw_As_Radio
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record)
       return Boolean;
   --  Returns whether Check_Menu_Item looks like a
   --  Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item
   --  Since: gtk+ 2.4

   procedure Set_Draw_As_Radio
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record;
       Draw_As_Radio   : Boolean);
   --  Sets whether Check_Menu_Item is drawn like a
   --  Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item
   --  Since: gtk+ 2.4
   --  "draw_as_radio": whether Check_Menu_Item is drawn like a
   --  Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item

   function Get_Inconsistent
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record)
       return Boolean;
   --  Retrieves the value set by Gtk.Check_Menu_Item.Set_Inconsistent.

   procedure Set_Inconsistent
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record;
       Setting         : Boolean);
   --  If the user has selected a range of elements (such as some text or
   --  spreadsheet cells) that are affected by a boolean setting, and the
   --  current values in that range are inconsistent, you may want to display
   --  the check in an "in between" state. This function turns on "in between"
   --  display. Normally you would turn off the inconsistent state again if the
   --  user explicitly selects a setting. This has to be done manually,
   --  Gtk.Check_Menu_Item.Set_Inconsistent only affects visual appearance, it
   --  doesn't affect the semantics of the widget.
   --  "setting": True to display an "inconsistent" third state check

   procedure Toggled
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record);
   --  Emits the Gtk.Check_Menu_Item.Gtk_Check_Menu_Item::toggled signal.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Check_Menu_Item_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Check_Menu_Item_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Check_Menu_Item_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Check_Menu_Item_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Check_Menu_Item_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Check_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Check_Menu_Item_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Check_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Check_Menu_Item_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Check_Menu_Item_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Check_Menu_Item_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Property : constant Glib.Properties.Property_Boolean;

   Draw_As_Radio_Property : constant Glib.Properties.Property_Boolean;

   Inconsistent_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Check_Menu_Item_Void is not null access procedure
     (Self : access Gtk_Check_Menu_Item_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Toggled : constant Glib.Signal_Name := "toggled";
   procedure On_Toggled
      (Self  : not null access Gtk_Check_Menu_Item_Record;
       Call  : Cb_Gtk_Check_Menu_Item_Void;
       After : Boolean := False);
   procedure On_Toggled
      (Self  : not null access Gtk_Check_Menu_Item_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the state of the check box is changed.
   --
   --  A signal handler can use Gtk.Check_Menu_Item.Get_Active to discover the
   --  new state.

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
     (Gtk.Actionable.Gtk_Actionable, Gtk_Check_Menu_Item_Record, Gtk_Check_Menu_Item);
   function "+"
     (Widget : access Gtk_Check_Menu_Item_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Check_Menu_Item
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Check_Menu_Item_Record, Gtk_Check_Menu_Item);
   function "+"
     (Widget : access Gtk_Check_Menu_Item_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Check_Menu_Item
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Check_Menu_Item_Record, Gtk_Check_Menu_Item);
   function "+"
     (Widget : access Gtk_Check_Menu_Item_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Check_Menu_Item
   renames Implements_Gtk_Buildable.To_Object;

private
   Inconsistent_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inconsistent");
   Draw_As_Radio_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("draw-as-radio");
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Check_Menu_Item;
