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
--  A Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item is a
--  Gtk.Tool_Item.Gtk_Tool_Item that separates groups of other Gtk_Tool_Items.
--  Depending on the theme, a Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item
--  will often look like a vertical line on horizontally docked toolbars.
--
--  If the Gtk.Toolbar.Gtk_Toolbar child property "expand" is True and the
--  property Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item:draw is False, a
--  Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item will act as a "spring" that
--  forces other items to the ends of the toolbar.
--
--  Use Gtk.Separator_Tool_Item.Gtk_New to create a new
--  Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item.
--
--  # CSS nodes
--
--  GtkSeparatorToolItem has a single CSS node with name separator.
--
--  </description>
--  <group>Menus and Toolbars</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Action;      use Gtk.Action;
with Gtk.Activatable; use Gtk.Activatable;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Tool_Item;   use Gtk.Tool_Item;

package Gtk.Separator_Tool_Item is

   type Gtk_Separator_Tool_Item_Record is new Gtk_Tool_Item_Record with null record;
   type Gtk_Separator_Tool_Item is access all Gtk_Separator_Tool_Item_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Item : out Gtk_Separator_Tool_Item);
   procedure Initialize
      (Item : not null access Gtk_Separator_Tool_Item_Record'Class);
   --  Create a new Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Separator_Tool_Item_New return Gtk_Separator_Tool_Item;
   --  Create a new Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item
   --  Since: gtk+ 2.4

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_separator_tool_item_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Draw
      (Item : not null access Gtk_Separator_Tool_Item_Record) return Boolean;
   --  Returns whether Item is drawn as a line, or just blank. See
   --  Gtk.Separator_Tool_Item.Set_Draw.
   --  Since: gtk+ 2.4

   procedure Set_Draw
      (Item : not null access Gtk_Separator_Tool_Item_Record;
       Draw : Boolean);
   --  Whether Item is drawn as a vertical line, or just blank. Setting this
   --  to False along with Gtk.Tool_Item.Set_Expand is useful to create an item
   --  that forces following items to the end of the toolbar.
   --  Since: gtk+ 2.4
   --  "draw": whether Item is drawn as a vertical line

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Separator_Tool_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Separator_Tool_Item_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Separator_Tool_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Separator_Tool_Item_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Separator_Tool_Item_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Separator_Tool_Item_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Draw_Property : constant Glib.Properties.Property_Boolean;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Activatable"
   --
   --  - "Buildable"

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Separator_Tool_Item_Record, Gtk_Separator_Tool_Item);
   function "+"
     (Widget : access Gtk_Separator_Tool_Item_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Separator_Tool_Item
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Separator_Tool_Item_Record, Gtk_Separator_Tool_Item);
   function "+"
     (Widget : access Gtk_Separator_Tool_Item_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Separator_Tool_Item
   renames Implements_Gtk_Buildable.To_Object;

private
   Draw_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("draw");
end Gtk.Separator_Tool_Item;
