------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

pragma Ada_05;
--  <description>
--  Gtk.Recent_Chooser.Gtk_Recent_Chooser is an interface that can be
--  implemented by widgets displaying the list of recently used files. In GTK+,
--  the main objects that implement this interface are
--  Gtk.Recent_Chooser_Widget.Gtk_Recent_Chooser_Widget,
--  Gtk.Recent_Chooser_Dialog.Gtk_Recent_Chooser_Dialog and
--  Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu.
--
--  Recently used files are supported since GTK+ 2.10.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Recent_Filter;       use Gtk.Recent_Filter;
with Gtk.Recent_Info;         use Gtk.Recent_Info;
with Gtk.Recent_Manager;      use Gtk.Recent_Manager;

package Gtk.Recent_Chooser is

   type Gtk_Recent_Chooser is new Glib.Types.GType_Interface;

   type Gtk_Recent_Sort_Type is (
      Recent_Sort_None,
      Recent_Sort_Mru,
      Recent_Sort_Lru,
      Recent_Sort_Custom);
   pragma Convention (C, Gtk_Recent_Sort_Type);
   --  Used to specify the sorting method to be applyed to the recently used
   --  resource list.

   type Gtk_Recent_Sort_Func is access function
     (A : Gtk.Recent_Info.Gtk_Recent_Info;
      B : Gtk.Recent_Info.Gtk_Recent_Info) return Gint;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Recent_Sort_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Recent_Sort_Type);
   type Property_Gtk_Recent_Sort_Type is new Gtk_Recent_Sort_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_recent_chooser_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Filter
      (Chooser : Gtk_Recent_Chooser;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class)
      ;
   --  Adds Filter to the list of Gtk.Recent_Filter.Gtk_Recent_Filter objects
   --  held by Chooser.
   --  If no previous filter objects were defined, this function will call
   --  Gtk.Recent_Chooser.Set_Filter.
   --  Since: gtk+ 2.10
   --  "filter": a Gtk.Recent_Filter.Gtk_Recent_Filter

   function Get_Current_Item
      (Chooser : Gtk_Recent_Chooser) return Gtk.Recent_Info.Gtk_Recent_Info;
   --  Gets the Gtk.Recent_Info.Gtk_Recent_Info currently selected by Chooser.
   --  when you have finished using it.
   --  Since: gtk+ 2.10

   function Get_Current_Uri
      (Chooser : Gtk_Recent_Chooser) return UTF8_String;
   function Set_Current_Uri
      (Chooser : Gtk_Recent_Chooser;
       URI     : UTF8_String) return Boolean;
   --  Sets Uri as the current URI for Chooser.
   --  Since: gtk+ 2.10
   --  "uri": a URI

   function Get_Filter
      (Chooser : Gtk_Recent_Chooser)
       return Gtk.Recent_Filter.Gtk_Recent_Filter;
   procedure Set_Filter
      (Chooser : Gtk_Recent_Chooser;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class)
      ;
   --  Sets Filter as the current Gtk.Recent_Filter.Gtk_Recent_Filter object
   --  used by Chooser to affect the displayed recently used resources.
   --  Since: gtk+ 2.10
   --  "filter": a Gtk.Recent_Filter.Gtk_Recent_Filter

   function Get_Items
      (Chooser : Gtk_Recent_Chooser)
       return Gtk.Recent_Manager.Gtk_Recent_Info_List.Glist;
   --  Gets the list of recently used resources in form of
   --  Gtk.Recent_Info.Gtk_Recent_Info objects.
   --  The return value of this function is affected by the "sort-type" and
   --  "limit" properties of Chooser.
   --  list of Gtk.Recent_Info.Gtk_Recent_Info objects. You should use
   --  Gtk.Recent_Info.Unref on every item of the list, and then free the list
   --  itself using g_list_free.
   --  Since: gtk+ 2.10

   function Get_Limit (Chooser : Gtk_Recent_Chooser) return Gint;
   pragma Import (C, Get_Limit, "gtk_recent_chooser_get_limit");
   procedure Set_Limit (Chooser : Gtk_Recent_Chooser; Limit : Gint);
   pragma Import (C, Set_Limit, "gtk_recent_chooser_set_limit");
   --  Sets the number of items that should be returned by
   --  Gtk.Recent_Chooser.Get_Items and gtk_recent_chooser_get_uris.
   --  Since: gtk+ 2.10
   --  "limit": a positive integer, or -1 for all items

   function Get_Local_Only (Chooser : Gtk_Recent_Chooser) return Boolean;
   procedure Set_Local_Only
      (Chooser    : Gtk_Recent_Chooser;
       Local_Only : Boolean);
   --  Sets whether only local resources, that is resources using the file://
   --  URI scheme, should be shown in the recently used resources selector. If
   --  Local_Only is True (the default) then the shown resources are guaranteed
   --  to be accessible through the operating system native file system.
   --  Since: gtk+ 2.10
   --  "local_only": True if only local files can be shown

   function Get_Select_Multiple
      (Chooser : Gtk_Recent_Chooser) return Boolean;
   procedure Set_Select_Multiple
      (Chooser         : Gtk_Recent_Chooser;
       Select_Multiple : Boolean);
   --  Sets whether Chooser can select multiple items.
   --  Since: gtk+ 2.10
   --  "select_multiple": True if Chooser can select more than one item

   function Get_Show_Icons (Chooser : Gtk_Recent_Chooser) return Boolean;
   procedure Set_Show_Icons
      (Chooser    : Gtk_Recent_Chooser;
       Show_Icons : Boolean);
   --  Sets whether Chooser should show an icon near the resource when
   --  displaying it.
   --  Since: gtk+ 2.10
   --  "show_icons": whether to show an icon near the resource

   function Get_Show_Not_Found (Chooser : Gtk_Recent_Chooser) return Boolean;
   procedure Set_Show_Not_Found
      (Chooser        : Gtk_Recent_Chooser;
       Show_Not_Found : Boolean);
   --  Sets whether Chooser should display the recently used resources that it
   --  didn't find. This only applies to local resources.
   --  Since: gtk+ 2.10
   --  "show_not_found": whether to show the local items we didn't find

   function Get_Show_Private (Chooser : Gtk_Recent_Chooser) return Boolean;
   procedure Set_Show_Private
      (Chooser      : Gtk_Recent_Chooser;
       Show_Private : Boolean);
   --  Whether to show recently used resources marked registered as private.
   --  Since: gtk+ 2.10
   --  "show_private": True to show private items, False otherwise

   function Get_Show_Tips (Chooser : Gtk_Recent_Chooser) return Boolean;
   procedure Set_Show_Tips
      (Chooser   : Gtk_Recent_Chooser;
       Show_Tips : Boolean);
   --  Sets whether to show a tooltips containing the full path of each
   --  recently used resource in a Gtk.Recent_Chooser.Gtk_Recent_Chooser
   --  widget.
   --  Since: gtk+ 2.10
   --  "show_tips": True if tooltips should be shown

   function Get_Sort_Type
      (Chooser : Gtk_Recent_Chooser) return Gtk_Recent_Sort_Type;
   pragma Import (C, Get_Sort_Type, "gtk_recent_chooser_get_sort_type");
   procedure Set_Sort_Type
      (Chooser   : Gtk_Recent_Chooser;
       Sort_Type : Gtk_Recent_Sort_Type);
   pragma Import (C, Set_Sort_Type, "gtk_recent_chooser_set_sort_type");
   --  Changes the sorting order of the recently used resources list displayed
   --  by Chooser.
   --  Since: gtk+ 2.10
   --  "sort_type": sort order that the chooser should use

   function List_Filters
      (Chooser : Gtk_Recent_Chooser)
       return Gtk.Recent_Filter.Gtk_Recent_Filter_List.GSlist;
   --  Gets the Gtk.Recent_Filter.Gtk_Recent_Filter objects held by Chooser.
   --  of Gtk.Recent_Filter.Gtk_Recent_Filter objects. You should just free the
   --  returned list using g_slist_free.
   --  Since: gtk+ 2.10

   procedure Remove_Filter
      (Chooser : Gtk_Recent_Chooser;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class)
      ;
   --  Removes Filter from the list of Gtk.Recent_Filter.Gtk_Recent_Filter
   --  objects held by Chooser.
   --  Since: gtk+ 2.10
   --  "filter": a Gtk.Recent_Filter.Gtk_Recent_Filter

   procedure Select_All (Chooser : Gtk_Recent_Chooser);
   pragma Import (C, Select_All, "gtk_recent_chooser_select_all");
   --  Selects all the items inside Chooser, if the Chooser supports multiple
   --  selection.
   --  Since: gtk+ 2.10

   function Select_Uri
      (Chooser : Gtk_Recent_Chooser;
       URI     : UTF8_String) return Boolean;
   --  Selects Uri inside Chooser.
   --  Since: gtk+ 2.10
   --  "uri": a URI

   procedure Set_Sort_Func
      (Chooser      : Gtk_Recent_Chooser;
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
         User_Data : User_Data_Type) return Gint;

      procedure Set_Sort_Func
         (Chooser      : Gtk.Recent_Chooser.Gtk_Recent_Chooser;
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

   procedure Unselect_All (Chooser : Gtk_Recent_Chooser);
   pragma Import (C, Unselect_All, "gtk_recent_chooser_unselect_all");
   --  Unselects all the items inside Chooser.
   --  Since: gtk+ 2.10

   procedure Unselect_Uri (Chooser : Gtk_Recent_Chooser; URI : UTF8_String);
   --  Unselects Uri inside Chooser.
   --  Since: gtk+ 2.10
   --  "uri": a URI

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Filter_Property
   --  Type: Gtk.Recent_Filter.Gtk_Recent_Filter
   --  Flags: read-write
   --  The Gtk.Recent_Filter.Gtk_Recent_Filter object to be used when
   --  displaying the recently used resources.
   --
   --  Name: Limit_Property
   --  Type: Gint
   --  Flags: read-write
   --  The maximum number of recently used resources to be displayed, or -1 to
   --  display all items. By default, the GtkSetting:gtk-recent-files-limit
   --  setting is respected: you can override that limit on a particular
   --  instance of Gtk.Recent_Chooser.Gtk_Recent_Chooser by setting this
   --  property.
   --
   --  Name: Local_Only_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether this Gtk.Recent_Chooser.Gtk_Recent_Chooser should display only
   --  local (file:) resources.
   --
   --  Name: Recent_Manager_Property
   --  Type: Gtk.Recent_Manager.Gtk_Recent_Manager
   --  Flags: write
   --  The Gtk.Recent_Manager.Gtk_Recent_Manager instance used by the
   --  Gtk.Recent_Chooser.Gtk_Recent_Chooser to display the list of recently
   --  used resources.
   --
   --  Name: Select_Multiple_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Allow the user to select multiple resources.
   --
   --  Name: Show_Icons_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether this Gtk.Recent_Chooser.Gtk_Recent_Chooser should display an
   --  icon near the item.
   --
   --  Name: Show_Not_Found_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether this Gtk.Recent_Chooser.Gtk_Recent_Chooser should display the
   --  recently used resources even if not present anymore. Setting this to
   --  False will perform a potentially expensive check on every local resource
   --  (every remote resource will always be displayed).
   --
   --  Name: Show_Private_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Show_Tips_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether this Gtk.Recent_Chooser.Gtk_Recent_Chooser should display a
   --  tooltip containing the full path of the recently used resources.
   --
   --  Name: Sort_Type_Property
   --  Type: Gtk_Recent_Sort_Type
   --  Flags: read-write
   --  Sorting order to be used when displaying the recently used resources.

   Filter_Property : constant Glib.Properties.Property_Object;
   Limit_Property : constant Glib.Properties.Property_Int;
   Local_Only_Property : constant Glib.Properties.Property_Boolean;
   Recent_Manager_Property : constant Glib.Properties.Property_Object;
   Select_Multiple_Property : constant Glib.Properties.Property_Boolean;
   Show_Icons_Property : constant Glib.Properties.Property_Boolean;
   Show_Not_Found_Property : constant Glib.Properties.Property_Boolean;
   Show_Private_Property : constant Glib.Properties.Property_Boolean;
   Show_Tips_Property : constant Glib.Properties.Property_Boolean;
   Sort_Type_Property : constant Gtk.Recent_Chooser.Property_Gtk_Recent_Sort_Type;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "item-activated"
   --     procedure Handler (Self : access Gtk_Recent_Chooser);
   --  This signal is emitted when the user "activates" a recent item in the
   --  recent chooser. This can happen by double-clicking on an item in the
   --  recently used resources list, or by pressing <keycap>Enter</keycap>.
   --
   --  "selection-changed"
   --     procedure Handler (Self : access Gtk_Recent_Chooser);
   --  This signal is emitted when there is a change in the set of selected
   --  recently used resources. This can happen when a user modifies the
   --  selection with the mouse or the keyboard, or when explicitely calling
   --  functions to change the selection.

   Signal_Item_Activated : constant Glib.Signal_Name := "item-activated";
   Signal_Selection_Changed : constant Glib.Signal_Name := "selection-changed";

private
   Filter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("filter");
   Limit_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("limit");
   Local_Only_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("local-only");
   Recent_Manager_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("recent-manager");
   Select_Multiple_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("select-multiple");
   Show_Icons_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-icons");
   Show_Not_Found_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-not-found");
   Show_Private_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-private");
   Show_Tips_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-tips");
   Sort_Type_Property : constant Gtk.Recent_Chooser.Property_Gtk_Recent_Sort_Type :=
     Gtk.Recent_Chooser.Build ("sort-type");
end Gtk.Recent_Chooser;
