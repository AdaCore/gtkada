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
--  A Gtk.Tearoff_Menu_Item.Gtk_Tearoff_Menu_Item is a special
--  Gtk.Menu_Item.Gtk_Menu_Item which is used to tear off and reattach its
--  menu.
--
--  When its menu is shown normally, the
--  Gtk.Tearoff_Menu_Item.Gtk_Tearoff_Menu_Item is drawn as a dotted line
--  indicating that the menu can be torn off. Activating it causes its menu to
--  be torn off and displayed in its own window as a tearoff menu.
--
--  When its menu is shown as a tearoff menu, the
--  Gtk.Tearoff_Menu_Item.Gtk_Tearoff_Menu_Item is drawn as a dotted line which
--  has a left pointing arrow graphic indicating that the tearoff menu can be
--  reattached. Activating it will erase the tearoff menu window.
--
--  > Gtk.Tearoff_Menu_Item.Gtk_Tearoff_Menu_Item is deprecated and should not
--  be used in newly > written code. Menus are not meant to be torn around.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Types;      use Glib.Types;
with Glib.Variant;    use Glib.Variant;
with Gtk.Action;      use Gtk.Action;
with Gtk.Actionable;  use Gtk.Actionable;
with Gtk.Activatable; use Gtk.Activatable;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Menu_Item;   use Gtk.Menu_Item;

package Gtk.Tearoff_Menu_Item is

   type Gtk_Tearoff_Menu_Item_Record is new Gtk_Menu_Item_Record with null record;
   type Gtk_Tearoff_Menu_Item is access all Gtk_Tearoff_Menu_Item_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Menu_Item : out Gtk_Tearoff_Menu_Item);
   procedure Initialize
      (Menu_Item : not null access Gtk_Tearoff_Menu_Item_Record'Class);
   --  Creates a new Gtk.Tearoff_Menu_Item.Gtk_Tearoff_Menu_Item.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Tearoff_Menu_Item_New return Gtk_Tearoff_Menu_Item;
   --  Creates a new Gtk.Tearoff_Menu_Item.Gtk_Tearoff_Menu_Item.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tearoff_menu_item_get_type");

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Tearoff_Menu_Item_Record)
       return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Tearoff_Menu_Item_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Tearoff_Menu_Item_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Tearoff_Menu_Item_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Tearoff_Menu_Item_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Tearoff_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Tearoff_Menu_Item_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Tearoff_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Tearoff_Menu_Item_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Tearoff_Menu_Item_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Tearoff_Menu_Item_Record;
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
     (Gtk.Actionable.Gtk_Actionable, Gtk_Tearoff_Menu_Item_Record, Gtk_Tearoff_Menu_Item);
   function "+"
     (Widget : access Gtk_Tearoff_Menu_Item_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Tearoff_Menu_Item
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Tearoff_Menu_Item_Record, Gtk_Tearoff_Menu_Item);
   function "+"
     (Widget : access Gtk_Tearoff_Menu_Item_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Tearoff_Menu_Item
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Tearoff_Menu_Item_Record, Gtk_Tearoff_Menu_Item);
   function "+"
     (Widget : access Gtk_Tearoff_Menu_Item_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Tearoff_Menu_Item
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Tearoff_Menu_Item;
