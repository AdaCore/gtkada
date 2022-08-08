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
--  A Gtk.Radio_Tool_Button.Gtk_Radio_Tool_Button is a
--  Gtk.Tool_Item.Gtk_Tool_Item that contains a radio button, that is, a button
--  that is part of a group of toggle buttons where only one button can be
--  active at a time.
--
--  Use Gtk.Radio_Tool_Button.Gtk_New to create a new GtkRadioToolButton. Use
--  gtk_radio_tool_button_new_from_widget to create a new GtkRadioToolButton
--  that is part of the same group as an existing GtkRadioToolButton.
--
--  # CSS nodes
--
--  GtkRadioToolButton has a single CSS node with name toolbutton.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                   use Glib;
with Glib.Properties;        use Glib.Properties;
with Glib.Types;             use Glib.Types;
with Glib.Variant;           use Glib.Variant;
with Gtk.Action;             use Gtk.Action;
with Gtk.Actionable;         use Gtk.Actionable;
with Gtk.Activatable;        use Gtk.Activatable;
with Gtk.Buildable;          use Gtk.Buildable;
with Gtk.Toggle_Tool_Button; use Gtk.Toggle_Tool_Button;
with Gtk.Widget;             use Gtk.Widget;

package Gtk.Radio_Tool_Button is

   type Gtk_Radio_Tool_Button_Record is new Gtk_Toggle_Tool_Button_Record with null record;
   type Gtk_Radio_Tool_Button is access all Gtk_Radio_Tool_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self  : out Gtk_Radio_Tool_Button;
       Group : Gtk.Widget.Widget_SList.GSlist);
   procedure Initialize
      (Self  : not null access Gtk_Radio_Tool_Button_Record'Class;
       Group : Gtk.Widget.Widget_SList.GSlist);
   --  Creates a new Gtk.Radio_Tool_Button.Gtk_Radio_Tool_Button, adding it to
   --  Group.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "group": An existing radio button group, or null if you are creating a
   --  new group

   function Gtk_Radio_Tool_Button_New
      (Group : Gtk.Widget.Widget_SList.GSlist) return Gtk_Radio_Tool_Button;
   --  Creates a new Gtk.Radio_Tool_Button.Gtk_Radio_Tool_Button, adding it to
   --  Group.
   --  Since: gtk+ 2.4
   --  "group": An existing radio button group, or null if you are creating a
   --  new group

   procedure Gtk_New_From_Stock
      (Self     : out Gtk_Radio_Tool_Button;
       Group    : Gtk.Widget.Widget_SList.GSlist;
       Stock_Id : UTF8_String);
   procedure Initialize_From_Stock
      (Self     : not null access Gtk_Radio_Tool_Button_Record'Class;
       Group    : Gtk.Widget.Widget_SList.GSlist;
       Stock_Id : UTF8_String);
   --  Creates a new Gtk.Radio_Tool_Button.Gtk_Radio_Tool_Button, adding it to
   --  Group. The new Gtk.Radio_Tool_Button.Gtk_Radio_Tool_Button will contain
   --  an icon and label from the stock item indicated by Stock_Id.
   --  Since: gtk+ 2.4
   --  Initialize_From_Stock does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "group": an existing radio button group, or null if you are creating a
   --  new group
   --  "stock_id": the name of a stock item

   function Gtk_Radio_Tool_Button_New_From_Stock
      (Group    : Gtk.Widget.Widget_SList.GSlist;
       Stock_Id : UTF8_String) return Gtk_Radio_Tool_Button;
   --  Creates a new Gtk.Radio_Tool_Button.Gtk_Radio_Tool_Button, adding it to
   --  Group. The new Gtk.Radio_Tool_Button.Gtk_Radio_Tool_Button will contain
   --  an icon and label from the stock item indicated by Stock_Id.
   --  Since: gtk+ 2.4
   --  "group": an existing radio button group, or null if you are creating a
   --  new group
   --  "stock_id": the name of a stock item

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_radio_tool_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Group
      (Self : not null access Gtk_Radio_Tool_Button_Record)
       return Gtk.Widget.Widget_SList.GSlist;
   --  Returns the radio button group Button belongs to.
   --  Since: gtk+ 2.4

   procedure Set_Group
      (Self  : not null access Gtk_Radio_Tool_Button_Record;
       Group : Gtk.Widget.Widget_SList.GSlist);
   --  Adds Button to Group, removing it from the group it belonged to before.
   --  Since: gtk+ 2.4
   --  "group": an existing radio button group, or null

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Radio_Tool_Button_Record)
       return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Radio_Tool_Button_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Radio_Tool_Button_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Radio_Tool_Button_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Radio_Tool_Button_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Radio_Tool_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Radio_Tool_Button_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Radio_Tool_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Radio_Tool_Button_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Radio_Tool_Button_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Radio_Tool_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Group_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk_Radio_Tool_Button
   --  Flags: write
   --  Sets a new group for a radio tool button.

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
     (Gtk.Actionable.Gtk_Actionable, Gtk_Radio_Tool_Button_Record, Gtk_Radio_Tool_Button);
   function "+"
     (Widget : access Gtk_Radio_Tool_Button_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Radio_Tool_Button
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Radio_Tool_Button_Record, Gtk_Radio_Tool_Button);
   function "+"
     (Widget : access Gtk_Radio_Tool_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Radio_Tool_Button
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Radio_Tool_Button_Record, Gtk_Radio_Tool_Button);
   function "+"
     (Widget : access Gtk_Radio_Tool_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Radio_Tool_Button
   renames Implements_Gtk_Buildable.To_Object;

private
   Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("group");
end Gtk.Radio_Tool_Button;
