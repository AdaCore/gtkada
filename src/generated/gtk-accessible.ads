------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  An interface for describing UI elements for Assistive Technologies.
--
--  Every accessible implementation has:
--
--  - a "role", represented by a value of the [enumGtk.AccessibleRole]
--  enumeration - "attributes", represented by a set of
--  [enumGtk.AccessibleState], [enumGtk.AccessibleProperty] and
--  [enumGtk.AccessibleRelation] values
--
--  The role cannot be changed after instantiating a `GtkAccessible`
--  implementation.
--
--  The attributes are updated every time a UI element's state changes in a
--  way that should be reflected by assistive technologies. For instance, if a
--  `GtkWidget` visibility changes, the Gtk.Accessible.Accessible_State_Hidden
--  state will also change to reflect the [propertyGtk.Widget:visible]
--  property.
--
--  Every accessible implementation is part of a tree of accessible objects.
--  Normally, this tree corresponds to the widget tree, but can be customized
--  by reimplementing the [vfuncGtk.Accessible.get_accessible_parent],
--  [vfuncGtk.Accessible.get_first_accessible_child] and
--  [vfuncGtk.Accessible.get_next_accessible_sibling] virtual functions.
--
--  Note that you can not create a top-level accessible object as of now,
--  which means that you must always have a parent accessible object.
--
--  Also note that when an accessible object does not correspond to a widget,
--  and it has children, whose implementation you don't control, it is
--  necessary to ensure the correct shape of the a11y tree by calling
--  [methodGtk.Accessible.set_accessible_parent] and updating the sibling by
--  [methodGtk.Accessible.update_next_accessible_sibling].

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Types;              use Glib.Types;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtkada.Bindings;         use Gtkada.Bindings;
with Gtkada.Types;            use Gtkada.Types;

package Gtk.Accessible is

   type Gtk_Accessible is new Glib.Types.GType_Interface;
   Null_Gtk_Accessible : constant Gtk_Accessible;

   type Gtk_Accessible_Announcement_Priority is (
      Accessible_Announcement_Priority_Low,
      Accessible_Announcement_Priority_Medium,
      Accessible_Announcement_Priority_High);
   pragma Convention (C, Gtk_Accessible_Announcement_Priority);
   --  The priority of an accessibility announcement.

   type Gtk_Accessible_Platform_State is (
      Accessible_Platform_State_Focusable,
      Accessible_Platform_State_Focused,
      Accessible_Platform_State_Active);
   pragma Convention (C, Gtk_Accessible_Platform_State);
   --  The various platform states which can be queried using
   --  [methodGtk.Accessible.get_platform_state].

   type Gtk_Accessible_Property is (
      Accessible_Property_Autocomplete,
      Accessible_Property_Description,
      Accessible_Property_Has_Popup,
      Accessible_Property_Key_Shortcuts,
      Accessible_Property_Label,
      Accessible_Property_Level,
      Accessible_Property_Modal,
      Accessible_Property_Multi_Line,
      Accessible_Property_Multi_Selectable,
      Accessible_Property_Orientation,
      Accessible_Property_Placeholder,
      Accessible_Property_Read_Only,
      Accessible_Property_Required,
      Accessible_Property_Role_Description,
      Accessible_Property_Sort,
      Accessible_Property_Value_Max,
      Accessible_Property_Value_Min,
      Accessible_Property_Value_Now,
      Accessible_Property_Value_Text,
      Accessible_Property_Help_Text);
   pragma Convention (C, Gtk_Accessible_Property);
   --  The possible accessible properties of a [ifaceAccessible].

   type Gtk_Accessible_Relation is (
      Accessible_Relation_Active_Descendant,
      Accessible_Relation_Col_Count,
      Accessible_Relation_Col_Index,
      Accessible_Relation_Col_Index_Text,
      Accessible_Relation_Col_Span,
      Accessible_Relation_Controls,
      Accessible_Relation_Described_By,
      Accessible_Relation_Details,
      Accessible_Relation_Error_Message,
      Accessible_Relation_Flow_To,
      Accessible_Relation_Labelled_By,
      Accessible_Relation_Owns,
      Accessible_Relation_Pos_In_Set,
      Accessible_Relation_Row_Count,
      Accessible_Relation_Row_Index,
      Accessible_Relation_Row_Index_Text,
      Accessible_Relation_Row_Span,
      Accessible_Relation_Set_Size,
      Accessible_Relation_Label_For,
      Accessible_Relation_Description_For,
      Accessible_Relation_Controlled_By,
      Accessible_Relation_Details_For,
      Accessible_Relation_Error_Message_For,
      Accessible_Relation_Flow_From);
   pragma Convention (C, Gtk_Accessible_Relation);
   --  The possible accessible relations of a [ifaceAccessible].
   --
   --  Accessible relations can be references to other widgets, integers or
   --  strings.

   type Gtk_Accessible_State is (
      Accessible_State_Busy,
      Accessible_State_Checked,
      Accessible_State_Disabled,
      Accessible_State_Expanded,
      Accessible_State_Hidden,
      Accessible_State_Invalid,
      Accessible_State_Pressed,
      Accessible_State_Selected,
      Accessible_State_Visited);
   pragma Convention (C, Gtk_Accessible_State);
   --  The possible accessible states of a [ifaceAccessible].

   type Gtk_Accessible_Role is (
      Accessible_Role_Alert,
      Accessible_Role_Alert_Dialog,
      Accessible_Role_Banner,
      Accessible_Role_Button,
      Accessible_Role_Caption,
      Accessible_Role_Cell,
      Accessible_Role_Checkbox,
      Accessible_Role_Column_Header,
      Accessible_Role_Combo_Box,
      Accessible_Role_Command,
      Accessible_Role_Composite,
      Accessible_Role_Dialog,
      Accessible_Role_Document,
      Accessible_Role_Feed,
      Accessible_Role_Form,
      Accessible_Role_Generic,
      Accessible_Role_Grid,
      Accessible_Role_Grid_Cell,
      Accessible_Role_Group,
      Accessible_Role_Heading,
      Accessible_Role_Img,
      Accessible_Role_Input,
      Accessible_Role_Label,
      Accessible_Role_Landmark,
      Accessible_Role_Legend,
      Accessible_Role_Link,
      Accessible_Role_List,
      Accessible_Role_List_Box,
      Accessible_Role_List_Item,
      Accessible_Role_Log,
      Accessible_Role_Main,
      Accessible_Role_Marquee,
      Accessible_Role_Math,
      Accessible_Role_Meter,
      Accessible_Role_Menu,
      Accessible_Role_Menu_Bar,
      Accessible_Role_Menu_Item,
      Accessible_Role_Menu_Item_Checkbox,
      Accessible_Role_Menu_Item_Radio,
      Accessible_Role_Navigation,
      Accessible_Role_None,
      Accessible_Role_Note,
      Accessible_Role_Option,
      Accessible_Role_Presentation,
      Accessible_Role_Progress_Bar,
      Accessible_Role_Radio,
      Accessible_Role_Radio_Group,
      Accessible_Role_Range,
      Accessible_Role_Region,
      Accessible_Role_Row,
      Accessible_Role_Row_Group,
      Accessible_Role_Row_Header,
      Accessible_Role_Scrollbar,
      Accessible_Role_Search,
      Accessible_Role_Search_Box,
      Accessible_Role_Section,
      Accessible_Role_Section_Head,
      Accessible_Role_Select,
      Accessible_Role_Separator,
      Accessible_Role_Slider,
      Accessible_Role_Spin_Button,
      Accessible_Role_Status,
      Accessible_Role_Structure,
      Accessible_Role_Switch,
      Accessible_Role_Tab,
      Accessible_Role_Table,
      Accessible_Role_Tab_List,
      Accessible_Role_Tab_Panel,
      Accessible_Role_Text_Box,
      Accessible_Role_Time,
      Accessible_Role_Timer,
      Accessible_Role_Toolbar,
      Accessible_Role_Tooltip,
      Accessible_Role_Tree,
      Accessible_Role_Tree_Grid,
      Accessible_Role_Tree_Item,
      Accessible_Role_Widget,
      Accessible_Role_Window,
      Accessible_Role_Toggle_Button,
      Accessible_Role_Application,
      Accessible_Role_Paragraph,
      Accessible_Role_Block_Quote,
      Accessible_Role_Article,
      Accessible_Role_Comment,
      Accessible_Role_Terminal);
   pragma Convention (C, Gtk_Accessible_Role);
   --  The accessible role for a [ifaceAccessible] implementation.
   --
   --  Abstract roles are only used as part of the ontology; application
   --  developers must not use abstract roles in their code.
   --  The priority of an accessibility announcement.
   --  The various platform states which can be queried using
   --  [methodGtk.Accessible.get_platform_state].
   --  The possible accessible properties of a [ifaceAccessible].
   --  The possible accessible relations of a [ifaceAccessible].
   --
   --  Accessible relations can be references to other widgets, integers or
   --  strings.
   --  The possible accessible states of a [ifaceAccessible].
   --  The accessible role for a [ifaceAccessible] implementation.
   --
   --  Abstract roles are only used as part of the ontology; application
   --  developers must not use abstract roles in their code.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Accessible_Announcement_Priority_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Accessible_Announcement_Priority);
   type Property_Gtk_Accessible_Announcement_Priority is new Gtk_Accessible_Announcement_Priority_Properties.Property;

   package Gtk_Accessible_Platform_State_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Accessible_Platform_State);
   type Property_Gtk_Accessible_Platform_State is new Gtk_Accessible_Platform_State_Properties.Property;

   package Gtk_Accessible_Property_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Accessible_Property);
   type Property_Gtk_Accessible_Property is new Gtk_Accessible_Property_Properties.Property;

   package Gtk_Accessible_Relation_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Accessible_Relation);
   type Property_Gtk_Accessible_Relation is new Gtk_Accessible_Relation_Properties.Property;

   package Gtk_Accessible_State_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Accessible_State);
   type Property_Gtk_Accessible_State is new Gtk_Accessible_State_Properties.Property;

   package Gtk_Accessible_Role_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Accessible_Role);
   type Property_Gtk_Accessible_Role is new Gtk_Accessible_Role_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_accessible_get_type");

   -------------
   -- Methods --
   -------------

   procedure Announce
      (Self     : Gtk_Accessible;
       Message  : UTF8_String;
       Priority : Gtk_Accessible_Announcement_Priority);
   --  Requests the user's screen reader to announce the given message.
   --  This kind of notification is useful for messages that either have only
   --  a visual representation or that are not exposed visually at all, e.g. a
   --  notification about a successful operation.
   --  Also, by using this API, you can ensure that the message does not
   --  interrupts the user's current screen reader output.
   --  Since: gtk+ 4.14
   --  @param Message the string to announce
   --  @param Priority the priority of the announcement

   function Get_Accessible_Id (Self : Gtk_Accessible) return UTF8_String;
   --  Retrieves the accessible identifier for the accessible object.
   --  This functionality can be overridden by `GtkAccessible`
   --  implementations.
   --  It is left to the accessible implementation to define the scope and
   --  uniqueness of the identifier.
   --  Since: gtk+ 4.22
   --  @return the accessible identifier

   function Get_Accessible_Parent
      (Self : Gtk_Accessible) return Gtk_Accessible;
   pragma Import (C, Get_Accessible_Parent, "gtk_accessible_get_accessible_parent");
   --  Retrieves the accessible parent for an accessible object.
   --  This function returns `NULL` for top level widgets.
   --  Since: gtk+ 4.10
   --  @return the accessible parent

   procedure Set_Accessible_Parent
      (Self         : Gtk_Accessible;
       Parent       : Gtk_Accessible;
       Next_Sibling : Gtk_Accessible);
   pragma Import (C, Set_Accessible_Parent, "gtk_accessible_set_accessible_parent");
   --  Sets the parent and sibling of an accessible object.
   --  This function is meant to be used by accessible implementations that
   --  are not part of the widget hierarchy, and but act as a logical bridge
   --  between widgets. For instance, if a widget creates an object that holds
   --  metadata for each child, and you want that object to implement the
   --  `GtkAccessible` interface, you will use this function to ensure that the
   --  parent of each child widget is the metadata object, and the parent of
   --  each metadata object is the container widget.
   --  Since: gtk+ 4.10
   --  @param Parent the parent accessible object
   --  @param Next_Sibling the sibling accessible object

   function Get_Accessible_Role
      (Self : Gtk_Accessible) return Gtk_Accessible_Role;
   pragma Import (C, Get_Accessible_Role, "gtk_accessible_get_accessible_role");
   --  Retrieves the accessible role of an accessible object.
   --  @return the accessible role

   function Get_At_Context
      (Self : Gtk_Accessible) return Gtk.Atcontext.Gtk_Atcontext;
   --  Retrieves the implementation for the given accessible object.
   --  Since: gtk+ 4.10
   --  @return the accessible implementation object

   function Get_Bounds
      (Self   : Gtk_Accessible;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;
   --  Queries the coordinates and dimensions of this accessible
   --  This functionality can be overridden by `GtkAccessible`
   --  implementations, e.g. to get the bounds from an ignored child widget.
   --  Since: gtk+ 4.10
   --  @param X the x coordinate of the top left corner of the accessible
   --  @param Y the y coordinate of the top left corner of the widget
   --  @param Width the width of the accessible object
   --  @param Height the height of the accessible object
   --  @return true if the bounds are valid, and false otherwise

   function Get_First_Accessible_Child
      (Self : Gtk_Accessible) return Gtk_Accessible;
   pragma Import (C, Get_First_Accessible_Child, "gtk_accessible_get_first_accessible_child");
   --  Retrieves the first accessible child of an accessible object.
   --  Since: gtk+ 4.10
   --  @return the first accessible child

   function Get_Next_Accessible_Sibling
      (Self : Gtk_Accessible) return Gtk_Accessible;
   pragma Import (C, Get_Next_Accessible_Sibling, "gtk_accessible_get_next_accessible_sibling");
   --  Retrieves the next accessible sibling of an accessible object
   --  Since: gtk+ 4.10
   --  @return the next accessible sibling

   function Get_Platform_State
      (Self  : Gtk_Accessible;
       State : Gtk_Accessible_Platform_State) return Boolean;
   --  Queries a platform state, such as focus.
   --  This functionality can be overridden by `GtkAccessible`
   --  implementations, e.g. to get platform state from an ignored child
   --  widget, as is the case for `GtkText` wrappers.
   --  Since: gtk+ 4.10
   --  @param State platform state to query
   --  @return the value of state for the accessible

   procedure Reset_Property
      (Self     : Gtk_Accessible;
       Property : Gtk_Accessible_Property);
   pragma Import (C, Reset_Property, "gtk_accessible_reset_property");
   --  Resets the accessible property to its default value.
   --  @param Property the accessible property

   procedure Reset_Relation
      (Self     : Gtk_Accessible;
       Relation : Gtk_Accessible_Relation);
   pragma Import (C, Reset_Relation, "gtk_accessible_reset_relation");
   --  Resets the accessible relation to its default value.
   --  @param Relation the accessible relation

   procedure Reset_State
      (Self  : Gtk_Accessible;
       State : Gtk_Accessible_State);
   pragma Import (C, Reset_State, "gtk_accessible_reset_state");
   --  Resets the accessible state to its default value.
   --  @param State the accessible state

   procedure Update_Next_Accessible_Sibling
      (Self        : Gtk_Accessible;
       New_Sibling : Gtk_Accessible);
   pragma Import (C, Update_Next_Accessible_Sibling, "gtk_accessible_update_next_accessible_sibling");
   --  Updates the next accessible sibling.
   --  That might be useful when a new child of a custom accessible is
   --  created, and it needs to be linked to a previous child.
   --  Since: gtk+ 4.10
   --  @param New_Sibling the new next accessible sibling to set

   procedure Update_Platform_State
      (Self  : Gtk_Accessible;
       State : Gtk_Accessible_Platform_State);
   pragma Import (C, Update_Platform_State, "gtk_accessible_update_platform_state");
   --  Informs ATs that the platform state has changed.
   --  This function should be used by `GtkAccessible` implementations that
   --  have a platform state but are not widgets. Widgets handle platform
   --  states automatically.
   --  Since: gtk+ 4.18
   --  @param State the platform state to update

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accessible_Role_Property : constant Gtk.Accessible.Property_Gtk_Accessible_Role;
   --  Type: Gtk_Accessible_Role
   --  The accessible role of the given `GtkAccessible` implementation.
   --
   --  The accessible role cannot be changed once set.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Accessible"

   function "+" (W : Gtk_Accessible) return Gtk_Accessible;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Get_Accessible_Id is access function (Self : Gtk_Accessible) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Accessible_Id);
   --  Retrieves the accessible identifier for the accessible object.
   --  This functionality can be overridden by `GtkAccessible`
   --  implementations.
   --  It is left to the accessible implementation to define the scope and
   --  uniqueness of the identifier.
   --  Since: gtk+ 4.22
   --  @return the accessible identifier

   type Virtual_Get_Accessible_Parent is access function (Self : Gtk_Accessible) return Gtk_Accessible;
   pragma Convention (C, Virtual_Get_Accessible_Parent);
   --  Retrieves the accessible parent for an accessible object.
   --  This function returns `NULL` for top level widgets.
   --  Since: gtk+ 4.10
   --  @return the accessible parent

   type Virtual_Get_At_Context is access function (Self : Gtk_Accessible) return System.Address;
   pragma Convention (C, Virtual_Get_At_Context);
   --  Retrieves the implementation for the given accessible object.
   --  Since: gtk+ 4.10
   --  @return the accessible implementation object

   type Virtual_Get_Bounds is access function
     (Self   : Gtk_Accessible;
      X      : access Glib.Gint;
      Y      : access Glib.Gint;
      Width  : access Glib.Gint;
      Height : access Glib.Gint) return Glib.Gboolean;
   pragma Convention (C, Virtual_Get_Bounds);
   --  Queries the coordinates and dimensions of this accessible
   --  This functionality can be overridden by `GtkAccessible`
   --  implementations, e.g. to get the bounds from an ignored child widget.
   --  Since: gtk+ 4.10
   --  @param X the x coordinate of the top left corner of the accessible
   --  @param Y the y coordinate of the top left corner of the widget
   --  @param Width the width of the accessible object
   --  @param Height the height of the accessible object
   --  @return true if the bounds are valid, and false otherwise

   type Virtual_Get_First_Accessible_Child is access function (Self : Gtk_Accessible) return Gtk_Accessible;
   pragma Convention (C, Virtual_Get_First_Accessible_Child);
   --  Retrieves the first accessible child of an accessible object.
   --  Since: gtk+ 4.10
   --  @return the first accessible child

   type Virtual_Get_Next_Accessible_Sibling is access function (Self : Gtk_Accessible) return Gtk_Accessible;
   pragma Convention (C, Virtual_Get_Next_Accessible_Sibling);
   --  Retrieves the next accessible sibling of an accessible object
   --  Since: gtk+ 4.10
   --  @return the next accessible sibling

   type Virtual_Get_Platform_State is access function
     (Self  : Gtk_Accessible;
      State : Gtk_Accessible_Platform_State) return Glib.Gboolean;
   pragma Convention (C, Virtual_Get_Platform_State);
   --  Queries a platform state, such as focus.
   --  This functionality can be overridden by `GtkAccessible`
   --  implementations, e.g. to get platform state from an ignored child
   --  widget, as is the case for `GtkText` wrappers.
   --  Since: gtk+ 4.10
   --  @param State platform state to query
   --  @return the value of state for the accessible

   subtype Accessible_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Get_Accessible_Id
     (Self    : Accessible_Interface_Descr;
      Handler : Virtual_Get_Accessible_Id);
   pragma Import (C, Set_Get_Accessible_Id, "gtkada_Accessible_set_get_accessible_id");

   procedure Set_Get_Accessible_Parent
     (Self    : Accessible_Interface_Descr;
      Handler : Virtual_Get_Accessible_Parent);
   pragma Import (C, Set_Get_Accessible_Parent, "gtkada_Accessible_set_get_accessible_parent");

   procedure Set_Get_At_Context
     (Self    : Accessible_Interface_Descr;
      Handler : Virtual_Get_At_Context);
   pragma Import (C, Set_Get_At_Context, "gtkada_Accessible_set_get_at_context");

   procedure Set_Get_Bounds
     (Self    : Accessible_Interface_Descr;
      Handler : Virtual_Get_Bounds);
   pragma Import (C, Set_Get_Bounds, "gtkada_Accessible_set_get_bounds");

   procedure Set_Get_First_Accessible_Child
     (Self    : Accessible_Interface_Descr;
      Handler : Virtual_Get_First_Accessible_Child);
   pragma Import (C, Set_Get_First_Accessible_Child, "gtkada_Accessible_set_get_first_accessible_child");

   procedure Set_Get_Next_Accessible_Sibling
     (Self    : Accessible_Interface_Descr;
      Handler : Virtual_Get_Next_Accessible_Sibling);
   pragma Import (C, Set_Get_Next_Accessible_Sibling, "gtkada_Accessible_set_get_next_accessible_sibling");

   procedure Set_Get_Platform_State
     (Self    : Accessible_Interface_Descr;
      Handler : Virtual_Get_Platform_State);
   pragma Import (C, Set_Get_Platform_State, "gtkada_Accessible_set_get_platform_state");
   --  See Glib.Object.Add_Interface

private
   Accessible_Role_Property : constant Gtk.Accessible.Property_Gtk_Accessible_Role :=
     Gtk.Accessible.Build ("accessible-role");

   Null_Gtk_Accessible : constant Gtk_Accessible :=
      Gtk_Accessible (Glib.Types.Null_Interface);
end Gtk.Accessible;
