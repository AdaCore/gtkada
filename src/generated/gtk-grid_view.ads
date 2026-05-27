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

--  Presents a large dynamic grid of items.
--
--  `GtkGridView` uses its factory to generate one child widget for each
--  visible item and shows them in a grid. The orientation of the grid view
--  determines if the grid reflows vertically or horizontally.
--
--  `GtkGridView` allows the user to select items according to the selection
--  characteristics of the model. For models that allow multiple selected
--  items, it is possible to turn on _rubberband selection_, using
--  [propertyGtk.GridView:enable-rubberband].
--
--  To learn more about the list widget framework, see the
--  [overview](section-list-widget.html).
--
--  # Actions
--
--  `GtkGridView` defines a set of built-in actions:
--
--  - `list.activate-item` activates the item at given position by emitting
--  the the [signalGtk.GridView::activate] signal.
--
--  # CSS nodes
--
--  ``` gridview ├── child[.activatable] │ ├── child[.activatable] │ ┊ ╰──
--  [rubberband] ```
--
--  `GtkGridView` uses a single CSS node with name `gridview`. Each child uses
--  a single CSS node with name `child`. If the
--  [propertyGtk.ListItem:activatable] property is set, the corresponding row
--  will have the `.activatable` style class. For rubberband selection, a
--  subnode with name `rubberband` is used.
--
--  # Accessibility
--
--  `GtkGridView` uses the [enumGtk.AccessibleRole.grid] role, and the items
--  use the [enumGtk.AccessibleRole.grid_cell] role.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.List_Base;         use Gtk.List_Base;
with Gtk.List_Item_Factory; use Gtk.List_Item_Factory;
with Gtk.Orientable;        use Gtk.Orientable;
with Gtk.Scroll_Info;       use Gtk.Scroll_Info;
with Gtk.Selection_Model;   use Gtk.Selection_Model;

package Gtk.Grid_View is

   type Gtk_Grid_View_Record is new Gtk_List_Base_Record with null record;
   type Gtk_Grid_View is access all Gtk_Grid_View_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self    : out Gtk_Grid_View;
       Model   : Gtk.Selection_Model.Gtk_Selection_Model;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   procedure Initialize
      (Self    : not null access Gtk_Grid_View_Record'Class;
       Model   : Gtk.Selection_Model.Gtk_Selection_Model;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   --  Creates a new `GtkGridView` that uses the given Factory for mapping
   --  items to widgets.
   --  The function takes ownership of the arguments, so you can write code
   --  like ```c grid_view = gtk_grid_view_new (create_model (),
   --  gtk_builder_list_item_factory_new_from_resource ("/resource.ui")); ```
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Model the model to use
   --  @param Factory The factory to populate items with

   function Gtk_Grid_View_New
      (Model   : Gtk.Selection_Model.Gtk_Selection_Model;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
       return Gtk_Grid_View;
   --  Creates a new `GtkGridView` that uses the given Factory for mapping
   --  items to widgets.
   --  The function takes ownership of the arguments, so you can write code
   --  like ```c grid_view = gtk_grid_view_new (create_model (),
   --  gtk_builder_list_item_factory_new_from_resource ("/resource.ui")); ```
   --  @param Model the model to use
   --  @param Factory The factory to populate items with

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_grid_view_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Enable_Rubberband
      (Self : not null access Gtk_Grid_View_Record) return Boolean;
   --  Returns whether rows can be selected by dragging with the mouse.
   --  @return True if rubberband selection is enabled

   procedure Set_Enable_Rubberband
      (Self              : not null access Gtk_Grid_View_Record;
       Enable_Rubberband : Boolean);
   --  Sets whether selections can be changed by dragging with the mouse.
   --  @param Enable_Rubberband True to enable rubberband selection

   function Get_Factory
      (Self : not null access Gtk_Grid_View_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory;
   --  Gets the factory that's currently used to populate list items.
   --  @return The factory in use

   procedure Set_Factory
      (Self    : not null access Gtk_Grid_View_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   --  Sets the `GtkListItemFactory` to use for populating list items.
   --  @param Factory the factory to use

   function Get_Max_Columns
      (Self : not null access Gtk_Grid_View_Record) return Guint;
   --  Gets the maximum number of columns that the grid will use.
   --  @return The maximum number of columns

   procedure Set_Max_Columns
      (Self        : not null access Gtk_Grid_View_Record;
       Max_Columns : Guint);
   --  Sets the maximum number of columns to use.
   --  This number must be at least 1.
   --  If Max_Columns is smaller than the minimum set via
   --  [methodGtk.GridView.set_min_columns], that value is used instead.
   --  @param Max_Columns The maximum number of columns

   function Get_Min_Columns
      (Self : not null access Gtk_Grid_View_Record) return Guint;
   --  Gets the minimum number of columns that the grid will use.
   --  @return The minimum number of columns

   procedure Set_Min_Columns
      (Self        : not null access Gtk_Grid_View_Record;
       Min_Columns : Guint);
   --  Sets the minimum number of columns to use.
   --  This number must be at least 1.
   --  If Min_Columns is smaller than the minimum set via
   --  [methodGtk.GridView.set_max_columns], that value is ignored.
   --  @param Min_Columns The minimum number of columns

   function Get_Model
      (Self : not null access Gtk_Grid_View_Record)
       return Gtk.Selection_Model.Gtk_Selection_Model;
   --  Gets the model that's currently used to read the items displayed.
   --  @return The model in use

   procedure Set_Model
      (Self  : not null access Gtk_Grid_View_Record;
       Model : Gtk.Selection_Model.Gtk_Selection_Model);
   --  Sets the model to use.
   --  This must be a [ifaceGtk.SelectionModel].
   --  @param Model the model to use

   function Get_Single_Click_Activate
      (Self : not null access Gtk_Grid_View_Record) return Boolean;
   --  Returns whether items will be activated on single click and selected on
   --  hover.
   --  @return True if items are activated on single click

   procedure Set_Single_Click_Activate
      (Self                  : not null access Gtk_Grid_View_Record;
       Single_Click_Activate : Boolean);
   --  Sets whether items should be activated on single click and selected on
   --  hover.
   --  @param Single_Click_Activate True to activate items on single click

   function Get_Tab_Behavior
      (Self : not null access Gtk_Grid_View_Record)
       return Gtk.Enums.Gtk_List_Tab_Behavior;
   --  Gets the behavior set for the <kbd>Tab</kbd> key.
   --  Since: gtk+ 4.12
   --  @return The behavior of the <kbd>Tab</kbd> key

   procedure Set_Tab_Behavior
      (Self         : not null access Gtk_Grid_View_Record;
       Tab_Behavior : Gtk.Enums.Gtk_List_Tab_Behavior);
   --  Sets the behavior of the <kbd>Tab</kbd> and
   --  <kbd>Shift</kbd>+<kbd>Tab</kbd> keys.
   --  Since: gtk+ 4.12
   --  @param Tab_Behavior The desired tab behavior

   procedure Scroll_To
      (Self   : not null access Gtk_Grid_View_Record;
       Pos    : Guint;
       Flags  : Gtk.Enums.Gtk_List_Scroll_Flags;
       Scroll : Gtk.Scroll_Info.Gtk_Scroll_Info);
   --  Scrolls to the item at the given position and performs the actions
   --  specified in Flags.
   --  This function works no matter if the gridview is shown or focused. If
   --  it isn't, then the changes will take effect once that happens.
   --  Since: gtk+ 4.12
   --  @param Pos position of the item. Must be less than the number of items
   --  in the view.
   --  @param Flags actions to perform
   --  @param Scroll details of how to perform the scroll operation or null to
   --  scroll into view

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Grid_View_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Grid_View_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Grid_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Grid_View_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Grid_View_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Grid_View_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Grid_View_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Grid_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Grid_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Grid_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Grid_View_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Grid_View_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Grid_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Grid_View_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Grid_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Orientation
      (Self : not null access Gtk_Grid_View_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Grid_View_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Enable_Rubberband_Property : constant Glib.Properties.Property_Boolean;
   --  Allow rubberband selection.

   Factory_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.List_Item_Factory.Gtk_List_Item_Factory
   --  Factory for populating list items.
   --
   --  The factory must be for configuring [classGtk.ListItem] objects.

   Max_Columns_Property : constant Glib.Properties.Property_Uint;
   --  Maximum number of columns per row.
   --
   --  If this number is smaller than [propertyGtk.GridView:min-columns], that
   --  value is used instead.

   Min_Columns_Property : constant Glib.Properties.Property_Uint;
   --  Minimum number of columns per row.

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Selection_Model.Gtk_Selection_Model
   --  Model for the items displayed.

   Single_Click_Activate_Property : constant Glib.Properties.Property_Boolean;
   --  Activate rows on single click and select them on hover.

   Tab_Behavior_Property : constant Gtk.Enums.Property_Gtk_List_Tab_Behavior;
   --  Behavior of the <kbd>Tab</kbd> key

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Grid_View_Guint_Void is not null access procedure
     (Self     : access Gtk_Grid_View_Record'Class;
      Position : Guint);

   type Cb_GObject_Guint_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Position : Guint);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Grid_View_Record;
       Call  : Cb_Gtk_Grid_View_Guint_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Grid_View_Record;
       Call  : Cb_GObject_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a cell has been activated by the user, usually via
   --  activating the GtkGridView|list.activate-item action.
   --
   --  This allows for a convenient way to handle activation in a gridview.
   --  See [propertyGtk.ListItem:activatable] for details on how to use this
   --  signal.

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
   --
   --  - "Gtk.Orientable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Grid_View_Record, Gtk_Grid_View);
   function "+"
     (Widget : access Gtk_Grid_View_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Grid_View
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Grid_View_Record, Gtk_Grid_View);
   function "+"
     (Widget : access Gtk_Grid_View_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Grid_View
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Grid_View_Record, Gtk_Grid_View);
   function "+"
     (Widget : access Gtk_Grid_View_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Grid_View
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Grid_View_Record, Gtk_Grid_View);
   function "+"
     (Widget : access Gtk_Grid_View_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Grid_View
   renames Implements_Gtk_Orientable.To_Object;

private
   Tab_Behavior_Property : constant Gtk.Enums.Property_Gtk_List_Tab_Behavior :=
     Gtk.Enums.Build ("tab-behavior");
   Single_Click_Activate_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("single-click-activate");
   Model_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("model");
   Min_Columns_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("min-columns");
   Max_Columns_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("max-columns");
   Factory_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("factory");
   Enable_Rubberband_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-rubberband");
end Gtk.Grid_View;
