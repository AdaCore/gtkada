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
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Recent_Filter;       use Gtk.Recent_Filter;
with Gtk.Recent_Info;         use Gtk.Recent_Info;
with Gtk.Recent_Manager;      use Gtk.Recent_Manager;

package Gtk.Recent_Chooser is

   type Gtk_Recent_Chooser is new Glib.Types.GType_Interface;
   Null_Gtk_Recent_Chooser : constant Gtk_Recent_Chooser;

   type Gtk_Recent_Sort_Type is (
      Recent_Sort_None,
      Recent_Sort_Mru,
      Recent_Sort_Lru,
      Recent_Sort_Custom);
   pragma Convention (C, Gtk_Recent_Sort_Type);
   --  Used to specify the sorting method to be applyed to the recently used
   --  resource list.

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Recent_Sort_Func is access function
     (A : Gtk.Recent_Info.Gtk_Recent_Info;
      B : Gtk.Recent_Info.Gtk_Recent_Info) return Glib.Gint;

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
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);
   --  Adds Filter to the list of Gtk.Recent_Filter.Gtk_Recent_Filter objects
   --  held by Chooser.
   --  If no previous filter objects were defined, this function will call
   --  Gtk.Recent_Chooser.Set_Filter.
   --  Since: gtk+ 2.10
   --  "filter": a Gtk.Recent_Filter.Gtk_Recent_Filter

   function Get_Current_Item
      (Chooser : Gtk_Recent_Chooser) return Gtk.Recent_Info.Gtk_Recent_Info;
   --  Gets the Gtk.Recent_Info.Gtk_Recent_Info currently selected by Chooser.
   --  Since: gtk+ 2.10

   function Get_Current_Uri
      (Chooser : Gtk_Recent_Chooser) return UTF8_String;
   --  Gets the URI currently selected by Chooser.
   --  Since: gtk+ 2.10

   function Set_Current_Uri
      (Chooser : Gtk_Recent_Chooser;
       URI     : UTF8_String) return Boolean;
   --  Sets Uri as the current URI for Chooser.
   --  Since: gtk+ 2.10
   --  "uri": a URI

   function Get_Filter
      (Chooser : Gtk_Recent_Chooser)
       return Gtk.Recent_Filter.Gtk_Recent_Filter;
   --  Gets the Gtk.Recent_Filter.Gtk_Recent_Filter object currently used by
   --  Chooser to affect the display of the recently used resources.
   --  Since: gtk+ 2.10

   procedure Set_Filter
      (Chooser : Gtk_Recent_Chooser;
       Filter  : access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);
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
   --  Since: gtk+ 2.10

   function Get_Limit (Chooser : Gtk_Recent_Chooser) return Glib.Gint;
   pragma Import (C, Get_Limit, "gtk_recent_chooser_get_limit");
   --  Gets the number of items returned by Gtk.Recent_Chooser.Get_Items and
   --  gtk_recent_chooser_get_uris.
   --  Since: gtk+ 2.10

   procedure Set_Limit (Chooser : Gtk_Recent_Chooser; Limit : Glib.Gint);
   pragma Import (C, Set_Limit, "gtk_recent_chooser_set_limit");
   --  Sets the number of items that should be returned by
   --  Gtk.Recent_Chooser.Get_Items and gtk_recent_chooser_get_uris.
   --  Since: gtk+ 2.10
   --  "limit": a positive integer, or -1 for all items

   function Get_Local_Only (Chooser : Gtk_Recent_Chooser) return Boolean;
   --  Gets whether only local resources should be shown in the recently used
   --  resources selector. See Gtk.Recent_Chooser.Set_Local_Only
   --  Since: gtk+ 2.10

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
   --  Gets whether Chooser can select multiple items.
   --  Since: gtk+ 2.10

   procedure Set_Select_Multiple
      (Chooser         : Gtk_Recent_Chooser;
       Select_Multiple : Boolean);
   --  Sets whether Chooser can select multiple items.
   --  Since: gtk+ 2.10
   --  "select_multiple": True if Chooser can select more than one item

   function Get_Show_Icons (Chooser : Gtk_Recent_Chooser) return Boolean;
   --  Retrieves whether Chooser should show an icon near the resource.
   --  Since: gtk+ 2.10

   procedure Set_Show_Icons
      (Chooser    : Gtk_Recent_Chooser;
       Show_Icons : Boolean);
   --  Sets whether Chooser should show an icon near the resource when
   --  displaying it.
   --  Since: gtk+ 2.10
   --  "show_icons": whether to show an icon near the resource

   function Get_Show_Not_Found (Chooser : Gtk_Recent_Chooser) return Boolean;
   --  Retrieves whether Chooser should show the recently used resources that
   --  were not found.
   --  Since: gtk+ 2.10

   procedure Set_Show_Not_Found
      (Chooser        : Gtk_Recent_Chooser;
       Show_Not_Found : Boolean);
   --  Sets whether Chooser should display the recently used resources that it
   --  didn't find. This only applies to local resources.
   --  Since: gtk+ 2.10
   --  "show_not_found": whether to show the local items we didn't find

   function Get_Show_Private (Chooser : Gtk_Recent_Chooser) return Boolean;
   --  Returns whether Chooser should display recently used resources
   --  registered as private.
   --  Since: gtk+ 2.10

   procedure Set_Show_Private
      (Chooser      : Gtk_Recent_Chooser;
       Show_Private : Boolean);
   --  Whether to show recently used resources marked registered as private.
   --  Since: gtk+ 2.10
   --  "show_private": True to show private items, False otherwise

   function Get_Show_Tips (Chooser : Gtk_Recent_Chooser) return Boolean;
   --  Gets whether Chooser should display tooltips containing the full path
   --  of a recently user resource.
   --  Since: gtk+ 2.10

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
   --  Gets the value set by Gtk.Recent_Chooser.Set_Sort_Type.
   --  Since: gtk+ 2.10

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
   --  Since: gtk+ 2.10

   procedure Remove_Filter
      (Chooser : Gtk_Recent_Chooser;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);
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
         User_Data : User_Data_Type) return Glib.Gint;

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

   Filter_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Recent_Filter.Gtk_Recent_Filter
   --  The Gtk.Recent_Filter.Gtk_Recent_Filter object to be used when
   --  displaying the recently used resources.

   Limit_Property : constant Glib.Properties.Property_Int;
   --  The maximum number of recently used resources to be displayed, or -1 to
   --  display all items.

   Local_Only_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this Gtk.Recent_Chooser.Gtk_Recent_Chooser should display only
   --  local (file:) resources.

   Recent_Manager_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Recent_Manager.Gtk_Recent_Manager
   --  Flags: write
   --  The Gtk.Recent_Manager.Gtk_Recent_Manager instance used by the
   --  Gtk.Recent_Chooser.Gtk_Recent_Chooser to display the list of recently
   --  used resources.

   Select_Multiple_Property : constant Glib.Properties.Property_Boolean;
   --  Allow the user to select multiple resources.

   Show_Icons_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this Gtk.Recent_Chooser.Gtk_Recent_Chooser should display an
   --  icon near the item.

   Show_Not_Found_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this Gtk.Recent_Chooser.Gtk_Recent_Chooser should display the
   --  recently used resources even if not present anymore. Setting this to
   --  False will perform a potentially expensive check on every local resource
   --  (every remote resource will always be displayed).

   Show_Private_Property : constant Glib.Properties.Property_Boolean;

   Show_Tips_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this Gtk.Recent_Chooser.Gtk_Recent_Chooser should display a
   --  tooltip containing the full path of the recently used resources.

   Sort_Type_Property : constant Gtk.Recent_Chooser.Property_Gtk_Recent_Sort_Type;
   --  Type: Gtk_Recent_Sort_Type
   --  Sorting order to be used when displaying the recently used resources.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Recent_Chooser_Void is not null access procedure (Self : Gtk_Recent_Chooser);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Item_Activated : constant Glib.Signal_Name := "item-activated";
   procedure On_Item_Activated
      (Self  : Gtk_Recent_Chooser;
       Call  : Cb_Gtk_Recent_Chooser_Void;
       After : Boolean := False);
   procedure On_Item_Activated
      (Self  : Gtk_Recent_Chooser;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the user "activates" a recent item in the
   --  recent chooser. This can happen by double-clicking on an item in the
   --  recently used resources list, or by pressing `Enter`.

   Signal_Selection_Changed : constant Glib.Signal_Name := "selection-changed";
   procedure On_Selection_Changed
      (Self  : Gtk_Recent_Chooser;
       Call  : Cb_Gtk_Recent_Chooser_Void;
       After : Boolean := False);
   procedure On_Selection_Changed
      (Self  : Gtk_Recent_Chooser;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when there is a change in the set of selected
   --  recently used resources. This can happen when a user modifies the
   --  selection with the mouse or the keyboard, or when explicitly calling
   --  functions to change the selection.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Recent_Chooser"

   function "+" (W : Gtk_Recent_Chooser) return Gtk_Recent_Chooser;
   pragma Inline ("+");

private
   Sort_Type_Property : constant Gtk.Recent_Chooser.Property_Gtk_Recent_Sort_Type :=
     Gtk.Recent_Chooser.Build ("sort-type");
   Show_Tips_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-tips");
   Show_Private_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-private");
   Show_Not_Found_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-not-found");
   Show_Icons_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-icons");
   Select_Multiple_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("select-multiple");
   Recent_Manager_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("recent-manager");
   Local_Only_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("local-only");
   Limit_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("limit");
   Filter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("filter");

Null_Gtk_Recent_Chooser : constant Gtk_Recent_Chooser :=
   Gtk_Recent_Chooser (Glib.Types.Null_Interface);
end Gtk.Recent_Chooser;
