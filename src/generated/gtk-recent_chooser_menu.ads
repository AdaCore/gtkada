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
--  Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu is a widget suitable for
--  displaying recently used files inside a menu. It can be used to set a
--  sub-menu of a Gtk.Menu_Item.Gtk_Menu_Item using Gtk.Menu_Item.Set_Submenu,
--  or as the menu of a Gtk.Menu_Tool_Button.Gtk_Menu_Tool_Button.
--
--  Note that Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu does not have
--  any methods of its own. Instead, you should use the functions that work on
--  a Gtk.Recent_Chooser.Gtk_Recent_Chooser.
--
--  Note also that Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu does not
--  support multiple filters, as it has no way to let the user choose between
--  them as the Gtk.Recent_Chooser_Widget.Gtk_Recent_Chooser_Widget and
--  Gtk.Recent_Chooser_Dialog.Gtk_Recent_Chooser_Dialog widgets do. Thus using
--  Gtk.Recent_Chooser.Add_Filter on a
--  Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu widget will yield the same
--  effects as using Gtk.Recent_Chooser.Set_Filter, replacing any currently set
--  filter with the supplied filter; Gtk.Recent_Chooser.Remove_Filter will
--  remove any currently set Gtk.Recent_Filter.Gtk_Recent_Filter object and
--  will unset the current filter; Gtk.Recent_Chooser.List_Filters will return
--  a list containing a single Gtk.Recent_Filter.Gtk_Recent_Filter object.
--
--  Recently used files are supported since GTK+ 2.10.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Properties;    use Glib.Properties;
with Glib.Types;         use Glib.Types;
with Gtk.Action;         use Gtk.Action;
with Gtk.Activatable;    use Gtk.Activatable;
with Gtk.Buildable;      use Gtk.Buildable;
with Gtk.Menu;           use Gtk.Menu;
with Gtk.Recent_Chooser; use Gtk.Recent_Chooser;
with Gtk.Recent_Filter;  use Gtk.Recent_Filter;
with Gtk.Recent_Info;    use Gtk.Recent_Info;
with Gtk.Recent_Manager; use Gtk.Recent_Manager;

package Gtk.Recent_Chooser_Menu is

   type Gtk_Recent_Chooser_Menu_Record is new Gtk_Menu_Record with null record;
   type Gtk_Recent_Chooser_Menu is access all Gtk_Recent_Chooser_Menu_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Recent_Sort_Func is access function
     (A : Gtk.Recent_Info.Gtk_Recent_Info;
      B : Gtk.Recent_Info.Gtk_Recent_Info) return Glib.Gint;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Recent_Chooser_Menu);
   procedure Initialize
      (Self : not null access Gtk_Recent_Chooser_Menu_Record'Class);
   --  Creates a new Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu widget.
   --  This kind of widget shows the list of recently used resources as a
   --  menu, each item as a menu item. Each item inside the menu might have an
   --  icon, representing its MIME type, and a number, for mnemonic access.
   --  This widget implements the Gtk.Recent_Chooser.Gtk_Recent_Chooser
   --  interface.
   --  This widget creates its own Gtk.Recent_Manager.Gtk_Recent_Manager
   --  object. See the Gtk.Recent_Chooser_Menu.Gtk_New_For_Manager function to
   --  know how to create a Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu
   --  widget bound to another Gtk.Recent_Manager.Gtk_Recent_Manager object.
   --  Since: gtk+ 2.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Recent_Chooser_Menu_New return Gtk_Recent_Chooser_Menu;
   --  Creates a new Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu widget.
   --  This kind of widget shows the list of recently used resources as a
   --  menu, each item as a menu item. Each item inside the menu might have an
   --  icon, representing its MIME type, and a number, for mnemonic access.
   --  This widget implements the Gtk.Recent_Chooser.Gtk_Recent_Chooser
   --  interface.
   --  This widget creates its own Gtk.Recent_Manager.Gtk_Recent_Manager
   --  object. See the Gtk.Recent_Chooser_Menu.Gtk_New_For_Manager function to
   --  know how to create a Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu
   --  widget bound to another Gtk.Recent_Manager.Gtk_Recent_Manager object.
   --  Since: gtk+ 2.10

   procedure Gtk_New_For_Manager
      (Self    : out Gtk_Recent_Chooser_Menu;
       Manager : not null access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class);
   procedure Initialize_For_Manager
      (Self    : not null access Gtk_Recent_Chooser_Menu_Record'Class;
       Manager : not null access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class);
   --  Creates a new Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu widget
   --  using Manager as the underlying recently used resources manager.
   --  This is useful if you have implemented your own recent manager, or if
   --  you have a customized instance of a
   --  Gtk.Recent_Manager.Gtk_Recent_Manager object or if you wish to share a
   --  common Gtk.Recent_Manager.Gtk_Recent_Manager object among multiple
   --  Gtk.Recent_Chooser.Gtk_Recent_Chooser widgets.
   --  Since: gtk+ 2.10
   --  Initialize_For_Manager does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "manager": a Gtk.Recent_Manager.Gtk_Recent_Manager

   function Gtk_Recent_Chooser_Menu_New_For_Manager
      (Manager : not null access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class)
       return Gtk_Recent_Chooser_Menu;
   --  Creates a new Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu widget
   --  using Manager as the underlying recently used resources manager.
   --  This is useful if you have implemented your own recent manager, or if
   --  you have a customized instance of a
   --  Gtk.Recent_Manager.Gtk_Recent_Manager object or if you wish to share a
   --  common Gtk.Recent_Manager.Gtk_Recent_Manager object among multiple
   --  Gtk.Recent_Chooser.Gtk_Recent_Chooser widgets.
   --  Since: gtk+ 2.10
   --  "manager": a Gtk.Recent_Manager.Gtk_Recent_Manager

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_recent_chooser_menu_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Show_Numbers
      (Self : not null access Gtk_Recent_Chooser_Menu_Record) return Boolean;
   --  Returns the value set by Gtk.Recent_Chooser_Menu.Set_Show_Numbers.
   --  Since: gtk+ 2.10

   procedure Set_Show_Numbers
      (Self         : not null access Gtk_Recent_Chooser_Menu_Record;
       Show_Numbers : Boolean);
   --  Sets whether a number should be added to the items of Menu. The numbers
   --  are shown to provide a unique character for a mnemonic to be used inside
   --  ten menu item's label. Only the first the items get a number to avoid
   --  clashes.
   --  Since: gtk+ 2.10
   --  "show_numbers": whether to show numbers

   procedure Set_Sort_Func
      (Chooser      : not null access Gtk_Recent_Chooser_Menu_Record;
       Sort_Func    : Gtk_Recent_Sort_Func;
       Data_Destroy : Glib.G_Destroy_Notify_Address);
   --  Sets the comparison function used when sorting to be Sort_Func. If the
   --  Chooser has the sort type set to GTK_RECENT_SORT_CUSTOM then the chooser
   --  will sort using this function.
   --  To the comparison function will be passed two
   --  Gtk.Recent_Info.Gtk_Recent_Info structs and Sort_Data; Sort_Func should
   --  return a positive integer if the first item comes before the second,
   --  zero if the two items are equal and a negative integer if the first item
   --  comes after the second.
   --  Since: gtk+ 2.10
   --  "sort_func": the comparison function
   --  "data_destroy": destroy notifier for Sort_Data, or null

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Sort_Func_User_Data is

      type Gtk_Recent_Sort_Func is access function
        (A         : Gtk.Recent_Info.Gtk_Recent_Info;
         B         : Gtk.Recent_Info.Gtk_Recent_Info;
         User_Data : User_Data_Type) return Glib.Gint;

      procedure Set_Sort_Func
         (Chooser      : not null access Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu_Record'Class;
          Sort_Func    : Gtk_Recent_Sort_Func;
          Sort_Data    : User_Data_Type;
          Data_Destroy : Glib.G_Destroy_Notify_Address);
      --  Sets the comparison function used when sorting to be Sort_Func. If
      --  the Chooser has the sort type set to GTK_RECENT_SORT_CUSTOM then the
      --  chooser will sort using this function.
      --  To the comparison function will be passed two
      --  Gtk.Recent_Info.Gtk_Recent_Info structs and Sort_Data; Sort_Func
      --  should return a positive integer if the first item comes before the
      --  second, zero if the two items are equal and a negative integer if the
      --  first item comes after the second.
      --  Since: gtk+ 2.10
      --  "sort_func": the comparison function
      --  "sort_data": user data to pass to Sort_Func, or null
      --  "data_destroy": destroy notifier for Sort_Data, or null

   end Set_Sort_Func_User_Data;

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Recent_Chooser_Menu_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Recent_Chooser_Menu_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Recent_Chooser_Menu_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Recent_Chooser_Menu_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Recent_Chooser_Menu_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Recent_Chooser_Menu_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   procedure Add_Filter
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);

   function Get_Current_Item
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Gtk.Recent_Info.Gtk_Recent_Info;

   function Get_Current_Uri
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return UTF8_String;

   function Set_Current_Uri
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       URI     : UTF8_String) return Boolean;

   function Get_Filter
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Gtk.Recent_Filter.Gtk_Recent_Filter;

   procedure Set_Filter
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       Filter  : access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);

   function Get_Items
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Gtk.Recent_Manager.Gtk_Recent_Info_List.Glist;

   function Get_Limit
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Glib.Gint;

   procedure Set_Limit
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       Limit   : Glib.Gint);

   function Get_Local_Only
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Boolean;

   procedure Set_Local_Only
      (Chooser    : not null access Gtk_Recent_Chooser_Menu_Record;
       Local_Only : Boolean);

   function Get_Select_Multiple
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Boolean;

   procedure Set_Select_Multiple
      (Chooser         : not null access Gtk_Recent_Chooser_Menu_Record;
       Select_Multiple : Boolean);

   function Get_Show_Icons
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Boolean;

   procedure Set_Show_Icons
      (Chooser    : not null access Gtk_Recent_Chooser_Menu_Record;
       Show_Icons : Boolean);

   function Get_Show_Not_Found
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Boolean;

   procedure Set_Show_Not_Found
      (Chooser        : not null access Gtk_Recent_Chooser_Menu_Record;
       Show_Not_Found : Boolean);

   function Get_Show_Private
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Boolean;

   procedure Set_Show_Private
      (Chooser      : not null access Gtk_Recent_Chooser_Menu_Record;
       Show_Private : Boolean);

   function Get_Show_Tips
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Boolean;

   procedure Set_Show_Tips
      (Chooser   : not null access Gtk_Recent_Chooser_Menu_Record;
       Show_Tips : Boolean);

   function Get_Sort_Type
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Gtk.Recent_Chooser.Gtk_Recent_Sort_Type;

   procedure Set_Sort_Type
      (Chooser   : not null access Gtk_Recent_Chooser_Menu_Record;
       Sort_Type : Gtk.Recent_Chooser.Gtk_Recent_Sort_Type);

   function List_Filters
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Gtk.Recent_Filter.Gtk_Recent_Filter_List.GSlist;

   procedure Remove_Filter
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);

   procedure Select_All
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record);

   function Select_Uri
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       URI     : UTF8_String) return Boolean;

   procedure Unselect_All
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record);

   procedure Unselect_Uri
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       URI     : UTF8_String);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Show_Numbers_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the first ten items in the menu should be prepended by a number
   --  acting as a unique mnemonic.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Activatable"
   --
   --  - "Buildable"
   --
   --  - "RecentChooser"

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Recent_Chooser_Menu_Record, Gtk_Recent_Chooser_Menu);
   function "+"
     (Widget : access Gtk_Recent_Chooser_Menu_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Recent_Chooser_Menu
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Recent_Chooser_Menu_Record, Gtk_Recent_Chooser_Menu);
   function "+"
     (Widget : access Gtk_Recent_Chooser_Menu_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Recent_Chooser_Menu
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Recent_Chooser is new Glib.Types.Implements
     (Gtk.Recent_Chooser.Gtk_Recent_Chooser, Gtk_Recent_Chooser_Menu_Record, Gtk_Recent_Chooser_Menu);
   function "+"
     (Widget : access Gtk_Recent_Chooser_Menu_Record'Class)
   return Gtk.Recent_Chooser.Gtk_Recent_Chooser
   renames Implements_Gtk_Recent_Chooser.To_Interface;
   function "-"
     (Interf : Gtk.Recent_Chooser.Gtk_Recent_Chooser)
   return Gtk_Recent_Chooser_Menu
   renames Implements_Gtk_Recent_Chooser.To_Object;

private
   Show_Numbers_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-numbers");
end Gtk.Recent_Chooser_Menu;
