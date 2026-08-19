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

--  Allows the user to choose an item from a list of options.
--
--  <picture> <source srcset="drop-down-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkDropDown"
--  src="drop-down.png"> </picture>
--  The `GtkDropDown` displays the [selected][propertyGtk.DropDown:selected]
--  choice.
--
--  The options are given to `GtkDropDown` in the form of `GListModel` and how
--  the individual options are represented is determined by a
--  [classGtk.ListItemFactory]. The default factory displays simple strings,
--  and adds a checkmark to the selected item in the popup.
--
--  To set your own factory, use [methodGtk.DropDown.set_factory]. It is
--  possible to use a separate factory for the items in the popup, with
--  [methodGtk.DropDown.set_list_factory].
--
--  `GtkDropDown` knows how to obtain strings from the items in a
--  [classGtk.StringList]; for other models, you have to provide an expression
--  to find the strings via [methodGtk.DropDown.set_expression].
--
--  `GtkDropDown` can optionally allow search in the popup, which is useful if
--  the list of options is long. To enable the search entry, use
--  [methodGtk.DropDown.set_enable_search].
--
--  Here is a UI definition example for `GtkDropDown` with a simple model:
--
--  ```xml <object class="GtkDropDown"> <property name="model"> <object
--  class="GtkStringList"> <items> <item translatable="yes">Factory</item>
--  <item translatable="yes">Home</item> <item translatable="yes">Subway</item>
--  </items> </object> </property> </object> ```
--
--  If a `GtkDropDown` is created in this manner, or with
--  [ctorGtk.DropDown.new_from_strings], for instance, the object returned from
--  [methodGtk.DropDown.get_selected_item] will be a [classGtk.StringObject].
--
--  To learn more about the list widget framework, see the
--  [overview](section-list-widget.html).
--
--  ## CSS nodes
--
--  `GtkDropDown` has a single CSS node with name dropdown, with the button
--  and popover nodes as children.
--
--  ## Accessibility
--
--  `GtkDropDown` uses the [enumGtk.AccessibleRole.combo_box] role.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;          use GNAT.Strings;
with Glib;                  use Glib;
with Glib.List_Model;       use Glib.List_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Expression;        use Gtk.Expression;
with Gtk.List_Item_Factory; use Gtk.List_Item_Factory;
with Gtk.String_Filter;     use Gtk.String_Filter;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Drop_Down is

   type Gtk_Drop_Down_Record is new Gtk_Widget_Record with null record;
   type Gtk_Drop_Down is access all Gtk_Drop_Down_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gtk_Drop_Down;
       Model      : Glib.List_Model.Glist_Model;
       Expression : Gtk.Expression.Gtk_Expression);
   procedure Initialize
      (Self       : not null access Gtk_Drop_Down_Record'Class;
       Model      : Glib.List_Model.Glist_Model;
       Expression : Gtk.Expression.Gtk_Expression);
   --  Creates a new `GtkDropDown`.
   --  You may want to call [methodGtk.DropDown.set_factory] to set up a way
   --  to map its items to widgets.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Model the model to use
   --  @param Expression the expression to use

   function Gtk_Drop_Down_New
      (Model      : Glib.List_Model.Glist_Model;
       Expression : Gtk.Expression.Gtk_Expression) return Gtk_Drop_Down;
   --  Creates a new `GtkDropDown`.
   --  You may want to call [methodGtk.DropDown.set_factory] to set up a way
   --  to map its items to widgets.
   --  @param Model the model to use
   --  @param Expression the expression to use

   procedure Gtk_New_From_Strings
      (Self    : out Gtk_Drop_Down;
       Strings : GNAT.Strings.String_List);
   procedure Initialize_From_Strings
      (Self    : not null access Gtk_Drop_Down_Record'Class;
       Strings : GNAT.Strings.String_List);
   --  Creates a new `GtkDropDown` that is populated with the strings.
   --  Initialize_From_Strings does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Strings The strings to put in the dropdown

   function Gtk_Drop_Down_New_From_Strings
      (Strings : GNAT.Strings.String_List) return Gtk_Drop_Down;
   --  Creates a new `GtkDropDown` that is populated with the strings.
   --  @param Strings The strings to put in the dropdown

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_drop_down_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Enable_Search
      (Self : not null access Gtk_Drop_Down_Record) return Boolean;
   --  Returns whether search is enabled.
   --  @return True if the popup includes a search entry

   procedure Set_Enable_Search
      (Self          : not null access Gtk_Drop_Down_Record;
       Enable_Search : Boolean);
   --  Sets whether a search entry will be shown in the popup that allows to
   --  search for items in the list.
   --  Note that [propertyGtk.DropDown:expression] must be set for search to
   --  work.
   --  @param Enable_Search whether to enable search

   function Get_Expression
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.Expression.Gtk_Expression;
   --  Gets the expression set that is used to obtain strings from items.
   --  See [methodGtk.DropDown.set_expression].
   --  @return a `GtkExpression`
   --  Return has transfer-ownership='none'

   procedure Set_Expression
      (Self       : not null access Gtk_Drop_Down_Record;
       Expression : Gtk.Expression.Gtk_Expression);
   --  Sets the expression that gets evaluated to obtain strings from items.
   --  This is used for search in the popup. The expression must have a value
   --  type of G_TYPE_STRING.
   --  @param Expression a `GtkExpression`

   function Get_Factory
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory;
   --  Gets the factory that's currently used to populate list items.
   --  The factory returned by this function is always used for the item in
   --  the button. It is also used for items in the popup if
   --  [propertyGtk.DropDown:list-factory] is not set.
   --  @return The factory in use
   --  Return has transfer-ownership='none'

   procedure Set_Factory
      (Self    : not null access Gtk_Drop_Down_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   --  Sets the `GtkListItemFactory` to use for populating list items.
   --  @param Factory the factory to use

   function Get_Header_Factory
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory;
   --  Gets the factory that's currently used to create header widgets for the
   --  popup.
   --  Since: gtk+ 4.12
   --  @return The factory in use
   --  Return has transfer-ownership='none'

   procedure Set_Header_Factory
      (Self    : not null access Gtk_Drop_Down_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   --  Sets the `GtkListItemFactory` to use for creating header widgets for
   --  the popup.
   --  Since: gtk+ 4.12
   --  @param Factory the factory to use

   function Get_List_Factory
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory;
   --  Gets the factory that's currently used to populate list items in the
   --  popup.
   --  @return The factory in use
   --  Return has transfer-ownership='none'

   procedure Set_List_Factory
      (Self    : not null access Gtk_Drop_Down_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   --  Sets the `GtkListItemFactory` to use for populating list items in the
   --  popup.
   --  @param Factory the factory to use

   function Get_Model
      (Self : not null access Gtk_Drop_Down_Record)
       return Glib.List_Model.Glist_Model;
   --  Gets the model that provides the displayed items.
   --  @return The model in use

   procedure Set_Model
      (Self  : not null access Gtk_Drop_Down_Record;
       Model : Glib.List_Model.Glist_Model);
   --  Sets the `GListModel` to use.
   --  @param Model the model to use

   function Get_Search_Match_Mode
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.String_Filter.Gtk_String_Filter_Match_Mode;
   --  Returns the match mode that the search filter is using.
   --  Since: gtk+ 4.12
   --  @return the match mode of the search filter

   procedure Set_Search_Match_Mode
      (Self              : not null access Gtk_Drop_Down_Record;
       Search_Match_Mode : Gtk.String_Filter.Gtk_String_Filter_Match_Mode);
   --  Sets the match mode for the search filter.
   --  Since: gtk+ 4.12
   --  @param Search_Match_Mode the new match mode

   function Get_Selected
      (Self : not null access Gtk_Drop_Down_Record) return Guint;
   --  Gets the position of the selected item.
   --  @return the position of the selected item, or GTK_INVALID_LIST_POSITION
   --  if no item is selected

   procedure Set_Selected
      (Self     : not null access Gtk_Drop_Down_Record;
       Position : Guint);
   --  Selects the item at the given position.
   --  @param Position the position of the item to select, or
   --  GTK_INVALID_LIST_POSITION

   function Get_Selected_Item
      (Self : not null access Gtk_Drop_Down_Record)
       return Glib.Object.GObject;
   --  Gets the selected item. If no item is selected, null is returned.
   --  Return has transfer-ownership='none'

   function Get_Show_Arrow
      (Self : not null access Gtk_Drop_Down_Record) return Boolean;
   --  Returns whether to show an arrow within the widget.
   --  Since: gtk+ 4.6
   --  @return True if an arrow will be shown.

   procedure Set_Show_Arrow
      (Self       : not null access Gtk_Drop_Down_Record;
       Show_Arrow : Boolean);
   --  Sets whether an arrow will be displayed within the widget.
   --  Since: gtk+ 4.6
   --  @param Show_Arrow whether to show an arrow within the widget

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Drop_Down_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Drop_Down_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Drop_Down_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Drop_Down_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Drop_Down_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Drop_Down_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Drop_Down_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Drop_Down_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Drop_Down_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Drop_Down_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Enable_Search_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to show a search entry in the popup.
   --
   --  Note that search requires [propertyGtk.DropDown:expression] to be set.

   Factory_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.List_Item_Factory.Gtk_List_Item_Factory
   --  Factory for populating list items.

   Header_Factory_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.List_Item_Factory.Gtk_List_Item_Factory
   --  The factory for creating header widgets for the popup.

   List_Factory_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.List_Item_Factory.Gtk_List_Item_Factory
   --  The factory for populating list items in the popup.
   --
   --  If this is not set, [propertyGtk.DropDown:factory] is used.

   Search_Match_Mode_Property : constant Gtk.String_Filter.Property_Gtk_String_Filter_Match_Mode;
   --  Type: Gtk.String_Filter.Gtk_String_Filter_Match_Mode
   --  The match mode for the search filter.

   Selected_Property : constant Glib.Properties.Property_Uint;
   --  The position of the selected item.
   --
   --  If no item is selected, the property has the value
   --  GTK_INVALID_LIST_POSITION.

   Selected_Item_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Object.GObject
   --  The selected item.

   Show_Arrow_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to show an arrow within the GtkDropDown widget.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Drop_Down_Void is not null access procedure (Self : access Gtk_Drop_Down_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Drop_Down_Record;
       Call  : Cb_Gtk_Drop_Down_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Drop_Down_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to when the drop down is activated.
   --
   --  The `::activate` signal on `GtkDropDown` is an action signal and
   --  emitting it causes the drop down to pop up its dropdown.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Drop_Down_Record, Gtk_Drop_Down);
   function "+"
     (Widget : access Gtk_Drop_Down_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Drop_Down
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Drop_Down_Record, Gtk_Drop_Down);
   function "+"
     (Widget : access Gtk_Drop_Down_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Drop_Down
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Drop_Down_Record, Gtk_Drop_Down);
   function "+"
     (Widget : access Gtk_Drop_Down_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Drop_Down
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Show_Arrow_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-arrow");
   Selected_Item_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("selected-item");
   Selected_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("selected");
   Search_Match_Mode_Property : constant Gtk.String_Filter.Property_Gtk_String_Filter_Match_Mode :=
     Gtk.String_Filter.Build ("search-match-mode");
   List_Factory_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("list-factory");
   Header_Factory_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("header-factory");
   Factory_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("factory");
   Enable_Search_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-search");
end Gtk.Drop_Down;
