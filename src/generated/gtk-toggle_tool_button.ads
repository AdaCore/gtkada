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
--  A Gtk.Toggle_Tool_Button.Gtk_Toggle_Tool_Button is a
--  Gtk.Tool_Item.Gtk_Tool_Item that contains a toggle button.
--
--  Use Gtk.Toggle_Tool_Button.Gtk_New to create a new GtkToggleToolButton.
--
--  # CSS nodes
--
--  GtkToggleToolButton has a single CSS node with name togglebutton.
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
with Gtk.Tool_Button; use Gtk.Tool_Button;

package Gtk.Toggle_Tool_Button is

   type Gtk_Toggle_Tool_Button_Record is new Gtk_Tool_Button_Record with null record;
   type Gtk_Toggle_Tool_Button is access all Gtk_Toggle_Tool_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Button : out Gtk_Toggle_Tool_Button);
   procedure Initialize
      (Button : not null access Gtk_Toggle_Tool_Button_Record'Class);
   --  Returns a new Gtk.Toggle_Tool_Button.Gtk_Toggle_Tool_Button
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Toggle_Tool_Button_New return Gtk_Toggle_Tool_Button;
   --  Returns a new Gtk.Toggle_Tool_Button.Gtk_Toggle_Tool_Button
   --  Since: gtk+ 2.4

   procedure Gtk_New_From_Stock
      (Button   : out Gtk_Toggle_Tool_Button;
       Stock_Id : UTF8_String);
   procedure Initialize_From_Stock
      (Button   : not null access Gtk_Toggle_Tool_Button_Record'Class;
       Stock_Id : UTF8_String);
   --  Creates a new Gtk.Toggle_Tool_Button.Gtk_Toggle_Tool_Button containing
   --  the image and text from a stock item. Some stock ids have preprocessor
   --  macros like GTK_STOCK_OK and GTK_STOCK_APPLY.
   --  It is an error if Stock_Id is not a name of a stock item.
   --  Since: gtk+ 2.4
   --  Initialize_From_Stock does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "stock_id": the name of the stock item

   function Gtk_Toggle_Tool_Button_New_From_Stock
      (Stock_Id : UTF8_String) return Gtk_Toggle_Tool_Button;
   --  Creates a new Gtk.Toggle_Tool_Button.Gtk_Toggle_Tool_Button containing
   --  the image and text from a stock item. Some stock ids have preprocessor
   --  macros like GTK_STOCK_OK and GTK_STOCK_APPLY.
   --  It is an error if Stock_Id is not a name of a stock item.
   --  Since: gtk+ 2.4
   --  "stock_id": the name of the stock item

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_toggle_tool_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active
      (Button : not null access Gtk_Toggle_Tool_Button_Record)
       return Boolean;
   --  Queries a Gtk.Toggle_Tool_Button.Gtk_Toggle_Tool_Button and returns its
   --  current state. Returns True if the toggle button is pressed in and False
   --  if it is raised.
   --  Since: gtk+ 2.4

   procedure Set_Active
      (Button    : not null access Gtk_Toggle_Tool_Button_Record;
       Is_Active : Boolean);
   --  Sets the status of the toggle tool button. Set to True if you want the
   --  GtkToggleButton to be "pressed in", and False to raise it. This action
   --  causes the toggled signal to be emitted.
   --  Since: gtk+ 2.4
   --  "is_active": whether Button should be active

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Toggle_Tool_Button_Record)
       return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Toggle_Tool_Button_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Toggle_Tool_Button_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Toggle_Tool_Button_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Toggle_Tool_Button_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Toggle_Tool_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Toggle_Tool_Button_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Toggle_Tool_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Toggle_Tool_Button_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Toggle_Tool_Button_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Toggle_Tool_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Property : constant Glib.Properties.Property_Boolean;
   --  If the toggle tool button should be pressed in.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Toggle_Tool_Button_Void is not null access procedure
     (Self : access Gtk_Toggle_Tool_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Toggled : constant Glib.Signal_Name := "toggled";
   procedure On_Toggled
      (Self  : not null access Gtk_Toggle_Tool_Button_Record;
       Call  : Cb_Gtk_Toggle_Tool_Button_Void;
       After : Boolean := False);
   procedure On_Toggled
      (Self  : not null access Gtk_Toggle_Tool_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever the toggle tool button changes state.

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
     (Gtk.Actionable.Gtk_Actionable, Gtk_Toggle_Tool_Button_Record, Gtk_Toggle_Tool_Button);
   function "+"
     (Widget : access Gtk_Toggle_Tool_Button_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Toggle_Tool_Button
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Toggle_Tool_Button_Record, Gtk_Toggle_Tool_Button);
   function "+"
     (Widget : access Gtk_Toggle_Tool_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Toggle_Tool_Button
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Toggle_Tool_Button_Record, Gtk_Toggle_Tool_Button);
   function "+"
     (Widget : access Gtk_Toggle_Tool_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Toggle_Tool_Button
   renames Implements_Gtk_Buildable.To_Object;

private
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Toggle_Tool_Button;
