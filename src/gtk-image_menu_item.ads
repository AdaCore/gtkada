-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2006 AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  This widget works like a normal menu_item, but you can insert a
--  arbitrary widget (most often a pixmap widget), which is displayed
--  at the left side. The advantage is that indentation is handled the
--  same way as GtkAda does (i.e if you create a menu with a
--  Gtk_Check_Menu_Item, all normal menu_items are automatically indented by
--  GtkAda - so if you use a normal menu_item to display pixmaps at the left
--  side, the pixmaps will be indented, which is not what you want. This widget
--  solves the problem).
--  </description>
--  <c_version>2.8.17</c_version>

with Glib.Properties;
with Gtk.Accel_Group;
with Gtk.Menu_Item;
with Gtk.Widget;

package Gtk.Image_Menu_Item is

   type Gtk_Image_Menu_Item_Record is new
     Gtk.Menu_Item.Gtk_Menu_Item_Record with private;
   type Gtk_Image_Menu_Item is access all Gtk_Image_Menu_Item_Record'Class;

   procedure Gtk_New
     (Widget : out Gtk_Image_Menu_Item;
      Label  : UTF8_String);
   --  Create a new Gtk_Image_Menu_Item.
   --  If label is non null, set the label of the menu item.

   procedure Gtk_New_From_Stock
     (Widget   : out Gtk_Image_Menu_Item;
      Stock_Id : String);
   --  Create a new Gtk_Image_Menu_Item from a stock item.

   procedure Gtk_New
     (Widget      : out Gtk_Image_Menu_Item;
      Stock_Id    : String;
      Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group);
   --  Create a new Gtk_Image_Menu_Item with a label.
   --  If label contains an underscore, a mnemonic is created accordingly.

   procedure Gtk_New_With_Mnemonic
     (Widget : out Gtk_Image_Menu_Item;
      Label  : UTF8_String);
   --  Create a new Gtk_Image_Menu_Item with a label.
   --  If label contains an underscore, a mnemonic is created accordingly.

   procedure Initialize
     (Widget : access Gtk_Image_Menu_Item_Record'Class;
      Label  : UTF8_String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize
     (Widget      : access Gtk_Image_Menu_Item_Record'Class;
      Stock_Id    : String;
      Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group);
   --  ditto.

   procedure Initialize_With_Mnemonic
     (Widget : access Gtk_Image_Menu_Item_Record'Class;
      Label  : UTF8_String);
   --  ditto.

   procedure Initialize_From_Stock
     (Widget   : access Gtk_Image_Menu_Item_Record'Class;
      Stock_Id : String);
   --  ditto.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Set_Image
     (Menu_Item : access Gtk_Image_Menu_Item_Record;
      Image     : access Gtk.Widget.Gtk_Widget_Record'Class);

   function Get_Image
     (Menu_Item : access Gtk_Image_Menu_Item_Record)
      return Gtk.Widget.Gtk_Widget;

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Image_Property
   --  Type:  Object
   --  Descr: Child widget to appear next to the menu text
   --
   --  </properties>

   Image_Property : constant Glib.Properties.Property_Object;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gtk_Image_Menu_Item_Record is new
     Gtk.Menu_Item.Gtk_Menu_Item_Record with null record;

   Image_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("image");

   pragma Import (C, Get_Type, "gtk_image_menu_item_get_type");
end Gtk.Image_Menu_Item;
