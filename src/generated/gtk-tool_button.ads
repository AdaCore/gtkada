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
--  Gtk_Tool_Buttons are Gtk_Tool_Items containing buttons.
--
--  Use Gtk.Tool_Button.Gtk_New to create a new
--  Gtk.Tool_Button.Gtk_Tool_Button.
--
--  The label of a Gtk.Tool_Button.Gtk_Tool_Button is determined by the
--  properties Gtk.Tool_Button.Gtk_Tool_Button:label-widget,
--  Gtk.Tool_Button.Gtk_Tool_Button:label, and
--  Gtk.Tool_Button.Gtk_Tool_Button:stock-id. If
--  Gtk.Tool_Button.Gtk_Tool_Button:label-widget is non-null, then that widget
--  is used as the label. Otherwise, if Gtk.Tool_Button.Gtk_Tool_Button:label
--  is non-null, that string is used as the label. Otherwise, if
--  Gtk.Tool_Button.Gtk_Tool_Button:stock-id is non-null, the label is
--  determined by the stock item. Otherwise, the button does not have a label.
--
--  The icon of a Gtk.Tool_Button.Gtk_Tool_Button is determined by the
--  properties Gtk.Tool_Button.Gtk_Tool_Button:icon-widget and
--  Gtk.Tool_Button.Gtk_Tool_Button:stock-id. If
--  Gtk.Tool_Button.Gtk_Tool_Button:icon-widget is non-null, then that widget
--  is used as the icon. Otherwise, if Gtk.Tool_Button.Gtk_Tool_Button:stock-id
--  is non-null, the icon is determined by the stock item. Otherwise, the
--  button does not have a icon.
--
--  # CSS nodes
--
--  GtkToolButton has a single CSS node with name toolbutton.
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
with Gtk.Tool_Item;   use Gtk.Tool_Item;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Tool_Button is

   type Gtk_Tool_Button_Record is new Gtk_Tool_Item_Record with null record;
   type Gtk_Tool_Button is access all Gtk_Tool_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Button      : out Gtk_Tool_Button;
       Icon_Widget : Gtk.Widget.Gtk_Widget := null;
       Label       : UTF8_String := "");
   procedure Initialize
      (Button      : not null access Gtk_Tool_Button_Record'Class;
       Icon_Widget : Gtk.Widget.Gtk_Widget := null;
       Label       : UTF8_String := "");
   --  Creates a new Gtk.Tool_Button.Gtk_Tool_Button using Icon_Widget as
   --  contents and Label as label.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "icon_widget": a widget that will be used as the button contents, or
   --  null
   --  "label": a string that will be used as label, or null

   function Gtk_Tool_Button_New
      (Icon_Widget : Gtk.Widget.Gtk_Widget := null;
       Label       : UTF8_String := "") return Gtk_Tool_Button;
   --  Creates a new Gtk.Tool_Button.Gtk_Tool_Button using Icon_Widget as
   --  contents and Label as label.
   --  Since: gtk+ 2.4
   --  "icon_widget": a widget that will be used as the button contents, or
   --  null
   --  "label": a string that will be used as label, or null

   procedure Gtk_New_From_Stock
      (Button   : out Gtk_Tool_Button;
       Stock_Id : UTF8_String);
   procedure Initialize_From_Stock
      (Button   : not null access Gtk_Tool_Button_Record'Class;
       Stock_Id : UTF8_String);
   --  Creates a new Gtk.Tool_Button.Gtk_Tool_Button containing the image and
   --  text from a stock item. Some stock ids have preprocessor macros like
   --  GTK_STOCK_OK and GTK_STOCK_APPLY.
   --  It is an error if Stock_Id is not a name of a stock item.
   --  Since: gtk+ 2.4
   --  Initialize_From_Stock does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "stock_id": the name of the stock item

   function Gtk_Tool_Button_New_From_Stock
      (Stock_Id : UTF8_String) return Gtk_Tool_Button;
   --  Creates a new Gtk.Tool_Button.Gtk_Tool_Button containing the image and
   --  text from a stock item. Some stock ids have preprocessor macros like
   --  GTK_STOCK_OK and GTK_STOCK_APPLY.
   --  It is an error if Stock_Id is not a name of a stock item.
   --  Since: gtk+ 2.4
   --  "stock_id": the name of the stock item

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tool_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Icon_Name
      (Button : not null access Gtk_Tool_Button_Record) return UTF8_String;
   --  Returns the name of the themed icon for the tool button, see
   --  Gtk.Tool_Button.Set_Icon_Name.
   --  Since: gtk+ 2.8

   procedure Set_Icon_Name
      (Button    : not null access Gtk_Tool_Button_Record;
       Icon_Name : UTF8_String := "");
   --  Sets the icon for the tool button from a named themed icon. See the
   --  docs for Gtk.Icon_Theme.Gtk_Icon_Theme for more details. The
   --  Gtk.Tool_Button.Gtk_Tool_Button:icon-name property only has an effect if
   --  not overridden by non-null Gtk.Tool_Button.Gtk_Tool_Button:label-widget,
   --  Gtk.Tool_Button.Gtk_Tool_Button:icon-widget and
   --  Gtk.Tool_Button.Gtk_Tool_Button:stock-id properties.
   --  Since: gtk+ 2.8
   --  "icon_name": the name of the themed icon

   function Get_Icon_Widget
      (Button : not null access Gtk_Tool_Button_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Return the widget used as icon widget on Button. See
   --  Gtk.Tool_Button.Set_Icon_Widget.
   --  Since: gtk+ 2.4

   procedure Set_Icon_Widget
      (Button      : not null access Gtk_Tool_Button_Record;
       Icon_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets Icon as the widget used as icon on Button. If Icon_Widget is null
   --  the icon is determined by the Gtk.Tool_Button.Gtk_Tool_Button:stock-id
   --  property. If the Gtk.Tool_Button.Gtk_Tool_Button:stock-id property is
   --  also null, Button will not have an icon.
   --  Since: gtk+ 2.4
   --  "icon_widget": the widget used as icon, or null

   function Get_Label
      (Button : not null access Gtk_Tool_Button_Record) return UTF8_String;
   --  Returns the label used by the tool button, or null if the tool button
   --  doesn't have a label. or uses a the label from a stock item. The
   --  returned string is owned by GTK+, and must not be modified or freed.
   --  Since: gtk+ 2.4

   procedure Set_Label
      (Button : not null access Gtk_Tool_Button_Record;
       Label  : UTF8_String := "");
   --  Sets Label as the label used for the tool button. The
   --  Gtk.Tool_Button.Gtk_Tool_Button:label property only has an effect if not
   --  overridden by a non-null Gtk.Tool_Button.Gtk_Tool_Button:label-widget
   --  property. If both the Gtk.Tool_Button.Gtk_Tool_Button:label-widget and
   --  Gtk.Tool_Button.Gtk_Tool_Button:label properties are null, the label is
   --  determined by the Gtk.Tool_Button.Gtk_Tool_Button:stock-id property. If
   --  the Gtk.Tool_Button.Gtk_Tool_Button:stock-id property is also null,
   --  Button will not have a label.
   --  Since: gtk+ 2.4
   --  "label": a string that will be used as label, or null.

   function Get_Label_Widget
      (Button : not null access Gtk_Tool_Button_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the widget used as label on Button. See
   --  Gtk.Tool_Button.Set_Label_Widget.
   --  Since: gtk+ 2.4

   procedure Set_Label_Widget
      (Button       : not null access Gtk_Tool_Button_Record;
       Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets Label_Widget as the widget that will be used as the label for
   --  Button. If Label_Widget is null the
   --  Gtk.Tool_Button.Gtk_Tool_Button:label property is used as label. If
   --  Gtk.Tool_Button.Gtk_Tool_Button:label is also null, the label in the
   --  stock item determined by the Gtk.Tool_Button.Gtk_Tool_Button:stock-id
   --  property is used as label. If Gtk.Tool_Button.Gtk_Tool_Button:stock-id
   --  is also null, Button does not have a label.
   --  Since: gtk+ 2.4
   --  "label_widget": the widget used as label, or null

   function Get_Stock_Id
      (Button : not null access Gtk_Tool_Button_Record) return UTF8_String;
   pragma Obsolescent (Get_Stock_Id);
   --  Returns the name of the stock item. See Gtk.Tool_Button.Set_Stock_Id.
   --  The returned string is owned by GTK+ and must not be freed or modifed.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   procedure Set_Stock_Id
      (Button   : not null access Gtk_Tool_Button_Record;
       Stock_Id : UTF8_String := "");
   pragma Obsolescent (Set_Stock_Id);
   --  Sets the name of the stock item. See
   --  Gtk.Tool_Button.Gtk_New_From_Stock. The stock_id property only has an
   --  effect if not overridden by non-null
   --  Gtk.Tool_Button.Gtk_Tool_Button:label-widget and
   --  Gtk.Tool_Button.Gtk_Tool_Button:icon-widget properties.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "stock_id": a name of a stock item, or null

   function Get_Use_Underline
      (Button : not null access Gtk_Tool_Button_Record) return Boolean;
   --  Returns whether underscores in the label property are used as mnemonics
   --  on menu items on the overflow menu. See
   --  Gtk.Tool_Button.Set_Use_Underline.
   --  Since: gtk+ 2.4

   procedure Set_Use_Underline
      (Button        : not null access Gtk_Tool_Button_Record;
       Use_Underline : Boolean);
   --  If set, an underline in the label property indicates that the next
   --  character should be used for the mnemonic accelerator key in the
   --  overflow menu. For example, if the label property is "_Open" and
   --  Use_Underline is True, the label on the tool button will be "Open" and
   --  the item on the overflow menu will have an underlined "O".
   --  Labels shown on tool buttons never have mnemonics on them; this
   --  property only affects the menu item on the overflow menu.
   --  Since: gtk+ 2.4
   --  "use_underline": whether the button label has the form "_Open"

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Tool_Button_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Tool_Button_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Tool_Button_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Tool_Button_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Tool_Button_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Tool_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Tool_Button_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Tool_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Tool_Button_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Tool_Button_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Tool_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Icon_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the themed icon displayed on the item. This property only
   --  has an effect if not overridden by
   --  Gtk.Tool_Button.Gtk_Tool_Button:label-widget,
   --  Gtk.Tool_Button.Gtk_Tool_Button:icon-widget or
   --  Gtk.Tool_Button.Gtk_Tool_Button:stock-id properties.

   Icon_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Label_Property : constant Glib.Properties.Property_String;

   Label_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Stock_Id_Property : constant Glib.Properties.Property_String;

   Use_Underline_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Tool_Button_Void is not null access procedure
     (Self : access Gtk_Tool_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Clicked : constant Glib.Signal_Name := "clicked";
   procedure On_Clicked
      (Self  : not null access Gtk_Tool_Button_Record;
       Call  : Cb_Gtk_Tool_Button_Void;
       After : Boolean := False);
   procedure On_Clicked
      (Self  : not null access Gtk_Tool_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the tool button is clicked with the mouse
   --  or activated with the keyboard.

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
     (Gtk.Actionable.Gtk_Actionable, Gtk_Tool_Button_Record, Gtk_Tool_Button);
   function "+"
     (Widget : access Gtk_Tool_Button_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Tool_Button
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Tool_Button_Record, Gtk_Tool_Button);
   function "+"
     (Widget : access Gtk_Tool_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Tool_Button
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Tool_Button_Record, Gtk_Tool_Button);
   function "+"
     (Widget : access Gtk_Tool_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Tool_Button
   renames Implements_Gtk_Buildable.To_Object;

private
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
   Stock_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock-id");
   Label_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("label-widget");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Icon_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("icon-widget");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
end Gtk.Tool_Button;
