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
--  A Gtk.Recent_Action.Gtk_Recent_Action represents a list of recently used
--  files, which can be shown by widgets such as
--  Gtk.Recent_Chooser_Dialog.Gtk_Recent_Chooser_Dialog or
--  Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu.
--
--  To construct a submenu showing recently used files, use a
--  Gtk.Recent_Action.Gtk_Recent_Action as the action for a <menuitem>. To
--  construct a menu toolbutton showing the recently used files in the popup
--  menu, use a Gtk.Recent_Action.Gtk_Recent_Action as the action for a
--  <toolitem> element.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Properties;    use Glib.Properties;
with Glib.Types;         use Glib.Types;
with Gtk.Action;         use Gtk.Action;
with Gtk.Buildable;      use Gtk.Buildable;
with Gtk.Recent_Chooser; use Gtk.Recent_Chooser;
with Gtk.Recent_Filter;  use Gtk.Recent_Filter;
with Gtk.Recent_Info;    use Gtk.Recent_Info;
with Gtk.Recent_Manager; use Gtk.Recent_Manager;

package Gtk.Recent_Action is

   type Gtk_Recent_Action_Record is new Gtk_Action_Record with null record;
   type Gtk_Recent_Action is access all Gtk_Recent_Action_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Recent_Sort_Func is access function
     (A : Gtk.Recent_Info.Gtk_Recent_Info;
      B : Gtk.Recent_Info.Gtk_Recent_Info) return Glib.Gint;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Widget   : out Gtk_Recent_Action;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "");
   procedure Initialize
      (Widget   : not null access Gtk_Recent_Action_Record'Class;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "");
   --  Creates a new Gtk.Recent_Action.Gtk_Recent_Action object. To add the
   --  action to a Gtk.Action_Group.Gtk_Action_Group and set the accelerator
   --  for the action, call Gtk.Action_Group.Add_Action_With_Accel.
   --  Since: gtk+ 2.12
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "name": a unique name for the action
   --  "label": the label displayed in menu items and on buttons, or null
   --  "tooltip": a tooltip for the action, or null
   --  "stock_id": the stock icon to display in widgets representing the
   --  action, or null

   function Gtk_Recent_Action_New
      (Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "") return Gtk_Recent_Action;
   --  Creates a new Gtk.Recent_Action.Gtk_Recent_Action object. To add the
   --  action to a Gtk.Action_Group.Gtk_Action_Group and set the accelerator
   --  for the action, call Gtk.Action_Group.Add_Action_With_Accel.
   --  Since: gtk+ 2.12
   --  "name": a unique name for the action
   --  "label": the label displayed in menu items and on buttons, or null
   --  "tooltip": a tooltip for the action, or null
   --  "stock_id": the stock icon to display in widgets representing the
   --  action, or null

   procedure Gtk_New_For_Manager
      (Widget   : out Gtk_Recent_Action;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Manager  : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class := Gtk.Recent_Manager.Get_Default);
   procedure Initialize_For_Manager
      (Widget   : not null access Gtk_Recent_Action_Record'Class;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Manager  : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class := Gtk.Recent_Manager.Get_Default);
   --  Creates a new Gtk.Recent_Action.Gtk_Recent_Action object. To add the
   --  action to a Gtk.Action_Group.Gtk_Action_Group and set the accelerator
   --  for the action, call Gtk.Action_Group.Add_Action_With_Accel.
   --  Since: gtk+ 2.12
   --  Initialize_For_Manager does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "name": a unique name for the action
   --  "label": the label displayed in menu items and on buttons, or null
   --  "tooltip": a tooltip for the action, or null
   --  "stock_id": the stock icon to display in widgets representing the
   --  action, or null
   --  "manager": a Gtk.Recent_Manager.Gtk_Recent_Manager, or null for using
   --  the default Gtk.Recent_Manager.Gtk_Recent_Manager

   function Gtk_Recent_Action_New_For_Manager
      (Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Manager  : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class := Gtk.Recent_Manager.Get_Default)
       return Gtk_Recent_Action;
   --  Creates a new Gtk.Recent_Action.Gtk_Recent_Action object. To add the
   --  action to a Gtk.Action_Group.Gtk_Action_Group and set the accelerator
   --  for the action, call Gtk.Action_Group.Add_Action_With_Accel.
   --  Since: gtk+ 2.12
   --  "name": a unique name for the action
   --  "label": the label displayed in menu items and on buttons, or null
   --  "tooltip": a tooltip for the action, or null
   --  "stock_id": the stock icon to display in widgets representing the
   --  action, or null
   --  "manager": a Gtk.Recent_Manager.Gtk_Recent_Manager, or null for using
   --  the default Gtk.Recent_Manager.Gtk_Recent_Manager

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_recent_action_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Show_Numbers
      (Widget : not null access Gtk_Recent_Action_Record) return Boolean;
   pragma Obsolescent (Get_Show_Numbers);
   --  Returns the value set by Gtk.Recent_Chooser_Menu.Set_Show_Numbers.
   --  Since: gtk+ 2.12
   --  Deprecated since 3.10, 1

   procedure Set_Show_Numbers
      (Widget       : not null access Gtk_Recent_Action_Record;
       Show_Numbers : Boolean);
   pragma Obsolescent (Set_Show_Numbers);
   --  Sets whether a number should be added to the items shown by the widgets
   --  representing Action. The numbers are shown to provide a unique character
   --  for a mnemonic to be used inside the menu item's label. Only the first
   --  ten items get a number to avoid clashes.
   --  Since: gtk+ 2.12
   --  Deprecated since 3.10, 1
   --  "show_numbers": True if the shown items should be numbered

   procedure Set_Sort_Func
      (Chooser      : not null access Gtk_Recent_Action_Record;
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
         (Chooser      : not null access Gtk.Recent_Action.Gtk_Recent_Action_Record'Class;
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

   procedure Add_Filter
      (Chooser : not null access Gtk_Recent_Action_Record;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);

   function Get_Current_Item
      (Chooser : not null access Gtk_Recent_Action_Record)
       return Gtk.Recent_Info.Gtk_Recent_Info;

   function Get_Current_Uri
      (Chooser : not null access Gtk_Recent_Action_Record)
       return UTF8_String;

   function Set_Current_Uri
      (Chooser : not null access Gtk_Recent_Action_Record;
       URI     : UTF8_String) return Boolean;

   function Get_Filter
      (Chooser : not null access Gtk_Recent_Action_Record)
       return Gtk.Recent_Filter.Gtk_Recent_Filter;

   procedure Set_Filter
      (Chooser : not null access Gtk_Recent_Action_Record;
       Filter  : access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);

   function Get_Items
      (Chooser : not null access Gtk_Recent_Action_Record)
       return Gtk.Recent_Manager.Gtk_Recent_Info_List.Glist;

   function Get_Limit
      (Chooser : not null access Gtk_Recent_Action_Record) return Glib.Gint;

   procedure Set_Limit
      (Chooser : not null access Gtk_Recent_Action_Record;
       Limit   : Glib.Gint);

   function Get_Local_Only
      (Chooser : not null access Gtk_Recent_Action_Record) return Boolean;

   procedure Set_Local_Only
      (Chooser    : not null access Gtk_Recent_Action_Record;
       Local_Only : Boolean);

   function Get_Select_Multiple
      (Chooser : not null access Gtk_Recent_Action_Record) return Boolean;

   procedure Set_Select_Multiple
      (Chooser         : not null access Gtk_Recent_Action_Record;
       Select_Multiple : Boolean);

   function Get_Show_Icons
      (Chooser : not null access Gtk_Recent_Action_Record) return Boolean;

   procedure Set_Show_Icons
      (Chooser    : not null access Gtk_Recent_Action_Record;
       Show_Icons : Boolean);

   function Get_Show_Not_Found
      (Chooser : not null access Gtk_Recent_Action_Record) return Boolean;

   procedure Set_Show_Not_Found
      (Chooser        : not null access Gtk_Recent_Action_Record;
       Show_Not_Found : Boolean);

   function Get_Show_Private
      (Chooser : not null access Gtk_Recent_Action_Record) return Boolean;

   procedure Set_Show_Private
      (Chooser      : not null access Gtk_Recent_Action_Record;
       Show_Private : Boolean);

   function Get_Show_Tips
      (Chooser : not null access Gtk_Recent_Action_Record) return Boolean;

   procedure Set_Show_Tips
      (Chooser   : not null access Gtk_Recent_Action_Record;
       Show_Tips : Boolean);

   function Get_Sort_Type
      (Chooser : not null access Gtk_Recent_Action_Record)
       return Gtk.Recent_Chooser.Gtk_Recent_Sort_Type;

   procedure Set_Sort_Type
      (Chooser   : not null access Gtk_Recent_Action_Record;
       Sort_Type : Gtk.Recent_Chooser.Gtk_Recent_Sort_Type);

   function List_Filters
      (Chooser : not null access Gtk_Recent_Action_Record)
       return Gtk.Recent_Filter.Gtk_Recent_Filter_List.GSlist;

   procedure Remove_Filter
      (Chooser : not null access Gtk_Recent_Action_Record;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);

   procedure Select_All (Chooser : not null access Gtk_Recent_Action_Record);

   function Select_Uri
      (Chooser : not null access Gtk_Recent_Action_Record;
       URI     : UTF8_String) return Boolean;

   procedure Unselect_All
      (Chooser : not null access Gtk_Recent_Action_Record);

   procedure Unselect_Uri
      (Chooser : not null access Gtk_Recent_Action_Record;
       URI     : UTF8_String);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Show_Numbers_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the items should be displayed with a number.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "RecentChooser"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Recent_Action_Record, Gtk_Recent_Action);
   function "+"
     (Widget : access Gtk_Recent_Action_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Recent_Action
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Recent_Chooser is new Glib.Types.Implements
     (Gtk.Recent_Chooser.Gtk_Recent_Chooser, Gtk_Recent_Action_Record, Gtk_Recent_Action);
   function "+"
     (Widget : access Gtk_Recent_Action_Record'Class)
   return Gtk.Recent_Chooser.Gtk_Recent_Chooser
   renames Implements_Gtk_Recent_Chooser.To_Interface;
   function "-"
     (Interf : Gtk.Recent_Chooser.Gtk_Recent_Chooser)
   return Gtk_Recent_Action
   renames Implements_Gtk_Recent_Chooser.To_Object;

private
   Show_Numbers_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-numbers");
end Gtk.Recent_Action;
