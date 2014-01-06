------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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
--  A GtkImageMenuItem is a menu item which has an icon next to the text
--  label.
--
--  Note that the user can disable display of menu icons, so make sure to
--  still fill in the text label.
--
--  </description>
pragma Ada_2005;

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Glib.Variant;    use Glib.Variant;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Action;      use Gtk.Action;
with Gtk.Actionable;  use Gtk.Actionable;
with Gtk.Activatable; use Gtk.Activatable;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Menu_Item;   use Gtk.Menu_Item;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Image_Menu_Item is

   type Gtk_Image_Menu_Item_Record is new Gtk_Menu_Item_Record with null record;
   type Gtk_Image_Menu_Item is access all Gtk_Image_Menu_Item_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Image_Menu_Item);
   procedure Initialize
      (Self : not null access Gtk_Image_Menu_Item_Record'Class);
   --  Creates a new Gtk.Image_Menu_Item.Gtk_Image_Menu_Item with an empty
   --  label.

   function Gtk_Image_Menu_Item_New return Gtk_Image_Menu_Item;
   --  Creates a new Gtk.Image_Menu_Item.Gtk_Image_Menu_Item with an empty
   --  label.

   procedure Gtk_New_From_Stock
      (Self        : out Gtk_Image_Menu_Item;
       Stock_Id    : UTF8_String;
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   procedure Initialize_From_Stock
      (Self        : not null access Gtk_Image_Menu_Item_Record'Class;
       Stock_Id    : UTF8_String;
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   --  Creates a new Gtk.Image_Menu_Item.Gtk_Image_Menu_Item containing the
   --  image and text from a stock item. Some stock ids have preprocessor
   --  macros like GTK_STOCK_OK and GTK_STOCK_APPLY.
   --  If you want this menu item to have changeable accelerators, then pass
   --  in null for accel_group. Next call Gtk.Menu_Item.Set_Accel_Path with an
   --  appropriate path for the menu item, use Gtk.Stock.Lookup to look up the
   --  standard accelerator for the stock item, and if one is found, call
   --  Gtk.Accel_Map.Add_Entry to register it.
   --  "stock_id": the name of the stock item.
   --  "accel_group": the Gtk.Accel_Group.Gtk_Accel_Group to add the menu
   --  items accelerator to, or null.

   function Gtk_Image_Menu_Item_New_From_Stock
      (Stock_Id    : UTF8_String;
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
       return Gtk_Image_Menu_Item;
   --  Creates a new Gtk.Image_Menu_Item.Gtk_Image_Menu_Item containing the
   --  image and text from a stock item. Some stock ids have preprocessor
   --  macros like GTK_STOCK_OK and GTK_STOCK_APPLY.
   --  If you want this menu item to have changeable accelerators, then pass
   --  in null for accel_group. Next call Gtk.Menu_Item.Set_Accel_Path with an
   --  appropriate path for the menu item, use Gtk.Stock.Lookup to look up the
   --  standard accelerator for the stock item, and if one is found, call
   --  Gtk.Accel_Map.Add_Entry to register it.
   --  "stock_id": the name of the stock item.
   --  "accel_group": the Gtk.Accel_Group.Gtk_Accel_Group to add the menu
   --  items accelerator to, or null.

   procedure Gtk_New (Self : out Gtk_Image_Menu_Item; Label : UTF8_String);
   procedure Initialize
      (Self  : not null access Gtk_Image_Menu_Item_Record'Class;
       Label : UTF8_String);
   --  Creates a new Gtk.Image_Menu_Item.Gtk_Image_Menu_Item containing a
   --  label.
   --  "label": the text of the menu item.

   function Gtk_Image_Menu_Item_New_With_Label
      (Label : UTF8_String) return Gtk_Image_Menu_Item;
   --  Creates a new Gtk.Image_Menu_Item.Gtk_Image_Menu_Item containing a
   --  label.
   --  "label": the text of the menu item.

   procedure Gtk_New_With_Mnemonic
      (Self  : out Gtk_Image_Menu_Item;
       Label : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Self  : not null access Gtk_Image_Menu_Item_Record'Class;
       Label : UTF8_String);
   --  Creates a new Gtk.Image_Menu_Item.Gtk_Image_Menu_Item containing a
   --  label. The label will be created using Gtk.Label.Gtk_New_With_Mnemonic,
   --  so underscores in Label indicate the mnemonic for the menu item.
   --  "label": the text of the menu item, with an underscore in front of the
   --  mnemonic character

   function Gtk_Image_Menu_Item_New_With_Mnemonic
      (Label : UTF8_String) return Gtk_Image_Menu_Item;
   --  Creates a new Gtk.Image_Menu_Item.Gtk_Image_Menu_Item containing a
   --  label. The label will be created using Gtk.Label.Gtk_New_With_Mnemonic,
   --  so underscores in Label indicate the mnemonic for the menu item.
   --  "label": the text of the menu item, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_image_menu_item_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Always_Show_Image
      (Self : not null access Gtk_Image_Menu_Item_Record) return Boolean;
   --  Returns whether the menu item will ignore the
   --  Gtk.Settings.Gtk_Settings:gtk-menu-images setting and always show the
   --  image, if available.
   --  Since: gtk+ 2.16

   procedure Set_Always_Show_Image
      (Self        : not null access Gtk_Image_Menu_Item_Record;
       Always_Show : Boolean);
   --  If True, the menu item will ignore the
   --  Gtk.Settings.Gtk_Settings:gtk-menu-images setting and always show the
   --  image, if available.
   --  Use this property if the menuitem would be useless or hard to use
   --  without the image.
   --  Since: gtk+ 2.16
   --  "always_show": True if the menuitem should always show the image

   function Get_Image
      (Self : not null access Gtk_Image_Menu_Item_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the widget that is currently set as the image of Image_Menu_Item.
   --  See Gtk.Image_Menu_Item.Set_Image.

   procedure Set_Image
      (Self  : not null access Gtk_Image_Menu_Item_Record;
       Image : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the image of Image_Menu_Item to the given widget. Note that it
   --  depends on the show-menu-images setting whether the image will be
   --  displayed or not.
   --  "image": a widget to set as the image for the menu item.

   function Get_Use_Stock
      (Self : not null access Gtk_Image_Menu_Item_Record) return Boolean;
   --  Checks whether the label set in the menuitem is used as a stock id to
   --  select the stock item for the item.
   --  Since: gtk+ 2.16

   procedure Set_Use_Stock
      (Self      : not null access Gtk_Image_Menu_Item_Record;
       Use_Stock : Boolean);
   --  If True, the label set in the menuitem is used as a stock id to select
   --  the stock item for the item.
   --  Since: gtk+ 2.16
   --  "use_stock": True if the menuitem should use a stock item

   procedure Set_Accel_Group
      (Self        : not null access Gtk_Image_Menu_Item_Record;
       Accel_Group : not null access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   --  Specifies an Accel_Group to add the menu items accelerator to (this
   --  only applies to stock items so a stock item must already be set, make
   --  sure to call Gtk.Image_Menu_Item.Set_Use_Stock and
   --  Gtk.Menu_Item.Set_Label with a valid stock item first).
   --  If you want this menu item to have changeable accelerators then you
   --  shouldnt need this (see Gtk.Image_Menu_Item.Gtk_New_From_Stock).
   --  Since: gtk+ 2.16
   --  "accel_group": the Gtk.Accel_Group.Gtk_Accel_Group

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Image_Menu_Item_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Image_Menu_Item_Record;
       Action_Name : UTF8_String);

   function Get_Action_Target_Value
      (Self : not null access Gtk_Image_Menu_Item_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Image_Menu_Item_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Image_Menu_Item_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Image_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Related_Action
      (Self : not null access Gtk_Image_Menu_Item_Record)
       return Gtk.Action.Gtk_Action;

   procedure Set_Related_Action
      (Self   : not null access Gtk_Image_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Image_Menu_Item_Record) return Boolean;

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Image_Menu_Item_Record;
       Use_Appearance : Boolean);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Image_Menu_Item_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accel_Group_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Accel_Group.Gtk_Accel_Group
   --  Flags: write
   --  The Accel Group to use for stock accelerator keys

   Always_Show_Image_Property : constant Glib.Properties.Property_Boolean;
   --  If True, the menu item will ignore the
   --  Gtk.Settings.Gtk_Settings:gtk-menu-images setting and always show the
   --  image, if available.
   --
   --  Use this property if the menuitem would be useless or hard to use
   --  without the image.

   Image_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Use_Stock_Property : constant Glib.Properties.Property_Boolean;
   --  If True, the label set in the menuitem is used as a stock id to select
   --  the stock item for the item.

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
     (Gtk.Actionable.Gtk_Actionable, Gtk_Image_Menu_Item_Record, Gtk_Image_Menu_Item);
   function "+"
     (Widget : access Gtk_Image_Menu_Item_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Image_Menu_Item
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Image_Menu_Item_Record, Gtk_Image_Menu_Item);
   function "+"
     (Widget : access Gtk_Image_Menu_Item_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Image_Menu_Item
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Image_Menu_Item_Record, Gtk_Image_Menu_Item);
   function "+"
     (Widget : access Gtk_Image_Menu_Item_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Image_Menu_Item
   renames Implements_Gtk_Buildable.To_Object;

private
   Use_Stock_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-stock");
   Image_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("image");
   Always_Show_Image_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("always-show-image");
   Accel_Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("accel-group");
end Gtk.Image_Menu_Item;
