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

--  Presents a large dynamic list of items.
--
--  `GtkListView` uses its factory to generate one row widget for each visible
--  item and shows them in a linear display, either vertically or horizontally.
--
--  The [propertyGtk.ListView:show-separators] property offers a simple way to
--  display separators between the rows.
--
--  `GtkListView` allows the user to select items according to the selection
--  characteristics of the model. For models that allow multiple selected
--  items, it is possible to turn on _rubberband selection_, using
--  [propertyGtk.ListView:enable-rubberband].
--
--  If you need multiple columns with headers, see [classGtk.ColumnView].
--
--  To learn more about the list widget framework, see the
--  [overview](section-list-widget.html).
--
--  An example of using `GtkListView`: ```c static void setup_listitem_cb
--  (GtkListItemFactory *factory, GtkListItem *list_item) { GtkWidget *image;
--
--  image = gtk_image_new (); gtk_image_set_icon_size (GTK_IMAGE (image),
--  GTK_ICON_SIZE_LARGE); gtk_list_item_set_child (list_item, image); }
--
--  static void bind_listitem_cb (GtkListItemFactory *factory, GtkListItem
--  *list_item) { GtkWidget *image; GAppInfo *app_info;
--
--  image = gtk_list_item_get_child (list_item); app_info =
--  gtk_list_item_get_item (list_item); gtk_image_set_from_gicon (GTK_IMAGE
--  (image), g_app_info_get_icon (app_info)); }
--
--  static void activate_cb (GtkListView *list, guint position, gpointer
--  unused) { GAppInfo *app_info;
--
--  app_info = g_list_model_get_item (G_LIST_MODEL (gtk_list_view_get_model
--  (list)), position); g_app_info_launch (app_info, NULL, NULL, NULL);
--  g_object_unref (app_info); }
--
--  ...
--
--  model = create_application_list ();
--
--  factory = gtk_signal_list_item_factory_new (); g_signal_connect (factory,
--  "setup", G_CALLBACK (setup_listitem_cb), NULL); g_signal_connect (factory,
--  "bind", G_CALLBACK (bind_listitem_cb), NULL);
--
--  list = gtk_list_view_new (GTK_SELECTION_MODEL (gtk_single_selection_new
--  (model)), factory);
--
--  g_signal_connect (list, "activate", G_CALLBACK (activate_cb), NULL);
--
--  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (sw), list); ```
--
--  # Actions
--
--  `GtkListView` defines a set of built-in actions:
--
--  - `list.activate-item` activates the item at given position by emitting
--  the [signalGtk.ListView::activate] signal.
--
--  # CSS nodes
--
--  ``` listview[.separators][.rich-list][.navigation-sidebar][.data-table]
--  ├── row[.activatable] │ ├── row[.activatable] │ ┊ ╰── [rubberband] ```
--
--  `GtkListView` uses a single CSS node named `listview`. It may carry the
--  `.separators` style class, when [propertyGtk.ListView:show-separators]
--  property is set. Each child widget uses a single CSS node named `row`. If
--  the [propertyGtk.ListItem:activatable] property is set, the corresponding
--  row will have the `.activatable` style class. For rubberband selection, a
--  node with name `rubberband` is used.
--
--  The main listview node may also carry style classes to select the style of
--  [list presentation](section-list-widget.htmllist-styles): .rich-list,
--  .navigation-sidebar or .data-table.
--
--  # Accessibility
--
--  `GtkListView` uses the [enumGtk.AccessibleRole.list] role, and the list
--  items use the [enumGtk.AccessibleRole.list_item] role.

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

package Gtk.List_View is

   type Gtk_List_View_Record is new Gtk_List_Base_Record with null record;
   type Gtk_List_View is access all Gtk_List_View_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self    : out Gtk_List_View;
       Model   : Gtk.Selection_Model.Gtk_Selection_Model;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   procedure Initialize
      (Self    : not null access Gtk_List_View_Record'Class;
       Model   : Gtk.Selection_Model.Gtk_Selection_Model;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   --  Creates a new `GtkListView` that uses the given Factory for mapping
   --  items to widgets.
   --  The function takes ownership of the arguments, so you can write code
   --  like ```c list_view = gtk_list_view_new (create_model (),
   --  gtk_builder_list_item_factory_new_from_resource ("/resource.ui")); ```
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Model the model to use
   --  @param Factory The factory to populate items with

   function Gtk_List_View_New
      (Model   : Gtk.Selection_Model.Gtk_Selection_Model;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
       return Gtk_List_View;
   --  Creates a new `GtkListView` that uses the given Factory for mapping
   --  items to widgets.
   --  The function takes ownership of the arguments, so you can write code
   --  like ```c list_view = gtk_list_view_new (create_model (),
   --  gtk_builder_list_item_factory_new_from_resource ("/resource.ui")); ```
   --  @param Model the model to use
   --  @param Factory The factory to populate items with

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_list_view_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Enable_Rubberband
      (Self : not null access Gtk_List_View_Record) return Boolean;
   --  Returns whether rows can be selected by dragging with the mouse.
   --  @return true if rubberband selection is enabled

   procedure Set_Enable_Rubberband
      (Self              : not null access Gtk_List_View_Record;
       Enable_Rubberband : Boolean);
   --  Sets whether selections can be changed by dragging with the mouse.
   --  @param Enable_Rubberband whether to enable rubberband selection

   function Get_Factory
      (Self : not null access Gtk_List_View_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory;
   --  Gets the factory that's currently used to populate list items.
   --  @return The factory in use
   --  Return has transfer-ownership='none'

   procedure Set_Factory
      (Self    : not null access Gtk_List_View_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   --  Sets the `GtkListItemFactory` to use for populating list items.
   --  @param Factory the factory to use

   function Get_Header_Factory
      (Self : not null access Gtk_List_View_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory;
   --  Gets the factory that's currently used to populate section headers.
   --  Since: gtk+ 4.12
   --  @return The factory in use
   --  Return has transfer-ownership='none'

   procedure Set_Header_Factory
      (Self    : not null access Gtk_List_View_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class);
   --  Sets the `GtkListItemFactory` to use for populating the
   --  [classGtk.ListHeader] objects used in section headers.
   --  If this factory is set to `NULL`, the list will not show section
   --  headers.
   --  Since: gtk+ 4.12
   --  @param Factory the factory to use

   function Get_Model
      (Self : not null access Gtk_List_View_Record)
       return Gtk.Selection_Model.Gtk_Selection_Model;
   --  Gets the model that's currently used to read the items displayed.
   --  @return The model in use

   procedure Set_Model
      (Self  : not null access Gtk_List_View_Record;
       Model : Gtk.Selection_Model.Gtk_Selection_Model);
   --  Sets the model to use.
   --  This must be a [ifaceGtk.SelectionModel] to use.
   --  @param Model the model to use

   function Get_Show_Separators
      (Self : not null access Gtk_List_View_Record) return Boolean;
   --  Returns whether the listview should show separators between rows.
   --  @return true if the listview shows separators

   procedure Set_Show_Separators
      (Self            : not null access Gtk_List_View_Record;
       Show_Separators : Boolean);
   --  Sets whether the listview should show separators between rows.
   --  @param Show_Separators whether to show separators

   function Get_Single_Click_Activate
      (Self : not null access Gtk_List_View_Record) return Boolean;
   --  Returns whether rows will be activated on single click and selected on
   --  hover.
   --  @return true if rows are activated on single click

   procedure Set_Single_Click_Activate
      (Self                  : not null access Gtk_List_View_Record;
       Single_Click_Activate : Boolean);
   --  Sets whether rows should be activated on single click and selected on
   --  hover.
   --  @param Single_Click_Activate whether to activate items on single click

   function Get_Tab_Behavior
      (Self : not null access Gtk_List_View_Record)
       return Gtk.Enums.Gtk_List_Tab_Behavior;
   --  Gets the behavior set for the <kbd>Tab</kbd> key.
   --  Since: gtk+ 4.12
   --  @return The behavior of the <kbd>Tab</kbd> key

   procedure Set_Tab_Behavior
      (Self         : not null access Gtk_List_View_Record;
       Tab_Behavior : Gtk.Enums.Gtk_List_Tab_Behavior);
   --  Sets the <kbd>Tab</kbd> key behavior.
   --  This influences how the <kbd>Tab</kbd> and
   --  <kbd>Shift</kbd>+<kbd>Tab</kbd> keys move the focus in the listview.
   --  Since: gtk+ 4.12
   --  @param Tab_Behavior The desired tab behavior

   procedure Scroll_To
      (Self   : not null access Gtk_List_View_Record;
       Pos    : Guint;
       Flags  : Gtk.Enums.Gtk_List_Scroll_Flags;
       Scroll : Gtk.Scroll_Info.Gtk_Scroll_Info);
   --  Scrolls to the item at the given position and performs the actions
   --  specified in Flags.
   --  This function works no matter if the listview is shown or focused. If
   --  it isn't, then the changes will take effect once that happens.
   --  Since: gtk+ 4.12
   --  Parameter Scroll has transfer-ownership='full'
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
      (Self     : not null access Gtk_List_View_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_List_View_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_List_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_List_View_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_List_View_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_List_View_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_List_View_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_List_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_List_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_List_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_List_View_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_List_View_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_List_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_List_View_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_List_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Orientation
      (Self : not null access Gtk_List_View_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_List_View_Record;
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

   Header_Factory_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.List_Item_Factory.Gtk_List_Item_Factory
   --  Factory for creating header widgets.
   --
   --  The factory must be for configuring [classGtk.ListHeader] objects.

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Selection_Model.Gtk_Selection_Model
   --  Model for the items displayed.

   Show_Separators_Property : constant Glib.Properties.Property_Boolean;
   --  Show separators between rows.

   Single_Click_Activate_Property : constant Glib.Properties.Property_Boolean;
   --  Activate rows on single click and select them on hover.

   Tab_Behavior_Property : constant Gtk.Enums.Property_Gtk_List_Tab_Behavior;
   --  Behavior of the <kbd>Tab</kbd> key

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_List_View_Guint_Void is not null access procedure
     (Self     : access Gtk_List_View_Record'Class;
      Position : Guint);

   type Cb_GObject_Guint_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Position : Guint);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_List_View_Record;
       Call  : Cb_Gtk_List_View_Guint_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_List_View_Record;
       Call  : Cb_GObject_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a row has been activated by the user.
   --
   --  Activation usually happens via the list.activate-item action of the
   --  `GtkListView`.
   --
   --  This allows for a convenient way to handle activation in a listview.
   --  See [methodGtk.ListItem.set_activatable] for details on how to use this
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
     (Gtk.Accessible.Gtk_Accessible, Gtk_List_View_Record, Gtk_List_View);
   function "+"
     (Widget : access Gtk_List_View_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_List_View
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_List_View_Record, Gtk_List_View);
   function "+"
     (Widget : access Gtk_List_View_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_List_View
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_List_View_Record, Gtk_List_View);
   function "+"
     (Widget : access Gtk_List_View_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_List_View
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_List_View_Record, Gtk_List_View);
   function "+"
     (Widget : access Gtk_List_View_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_List_View
   renames Implements_Gtk_Orientable.To_Object;

private
   Tab_Behavior_Property : constant Gtk.Enums.Property_Gtk_List_Tab_Behavior :=
     Gtk.Enums.Build ("tab-behavior");
   Single_Click_Activate_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("single-click-activate");
   Show_Separators_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-separators");
   Model_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("model");
   Header_Factory_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("header-factory");
   Factory_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("factory");
   Enable_Rubberband_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-rubberband");
end Gtk.List_View;
