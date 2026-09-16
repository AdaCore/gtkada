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

--  Provides an expander for a tree-like list.
--
--  It is typically placed as a bottommost child into a `GtkListView` to allow
--  users to expand and collapse children in a list with a
--  [classGtk.TreeListModel]. `GtkTreeExpander` provides the common UI
--  elements, gestures and keybindings for this purpose.
--
--  On top of this, the "listitem.expand", "listitem.collapse" and
--  "listitem.toggle-expand" actions are provided to allow adding custom UI for
--  managing expanded state.
--
--  It is important to mention that you want to set the
--  [propertyGtk.ListItem:focusable] property to FALSE when using this widget,
--  as you want the keyboard focus to be in the treexpander, and not inside the
--  list to make use of the keybindings.
--
--  The `GtkTreeListModel` must be set to not be passthrough. Then it will
--  provide [classGtk.TreeListRow] items which can be set via
--  [methodGtk.TreeExpander.set_list_row] on the expander. The expander will
--  then watch that row item automatically. [methodGtk.TreeExpander.set_child]
--  sets the widget that displays the actual row contents.
--
--  `GtkTreeExpander` can be modified with properties such as
--  [propertyGtk.TreeExpander:indent-for-icon],
--  [propertyGtk.TreeExpander:indent-for-depth], and
--  [propertyGtk.TreeExpander:hide-expander] to achieve a different appearance.
--  This can even be done to influence individual rows, for example by binding
--  the [propertyGtk.TreeExpander:hide-expander] property to the item count of
--  the model of the treelistrow, to hide the expander for rows without
--  children, even if the row is expandable.
--
--  ## Shortcuts and Gestures
--
--  `GtkTreeExpander` supports the following keyboard shortcuts:
--
--  - <kbd>+</kbd> or <kbd>*</kbd> expands the expander. - <kbd>-</kbd> or
--  <kbd>/</kbd> collapses the expander. - Left and right arrow keys, when
--  combined with <kbd>Shift</kbd> or <kbd>Ctrl</kbd>+<kbd>Shift</kbd>, will
--  expand or collapse, depending on the locale's text direction. -
--  <kbd>Ctrl</kbd>+<kbd>␣</kbd> toggles the expander state.
--
--  The row can also expand on drag gestures.
--
--  ## Actions
--
--  `GtkTreeExpander` defines a set of built-in actions:
--
--  - `listitem.expand` expands the expander if it can be expanded. -
--  `listitem.collapse` collapses the expander. - `listitem.toggle-expand`
--  tries to expand the expander if it was collapsed or collapses it if it was
--  expanded.
--
--  ## CSS nodes
--
--  ``` treeexpander ├── [indent]* ├── [expander] ╰── <child> ```
--
--  `GtkTreeExpander` has zero or one CSS nodes with the name "expander" that
--  should display the expander icon. The node will be `:checked` when it is
--  expanded. If the node is not expandable, an "indent" node will be displayed
--  instead.
--
--  For every level of depth, another "indent" node is prepended.
--
--  ## Accessibility
--
--  Until GTK 4.10, `GtkTreeExpander` used the [enumGtk.AccessibleRole.group]
--  role.
--
--  Since GTK 4.12, `GtkTreeExpander` uses the [enumGtk.AccessibleRole.button]
--  role. Toggling it will change the `GTK_ACCESSIBLE_STATE_EXPANDED` state.
--
--  <group>Trees and Lists</group>
--  <gtkada_demo>create_column_view.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Tree_List_Row;     use Gtk.Tree_List_Row;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Tree_Expander is

   type Gtk_Tree_Expander_Record is new Gtk_Widget_Record with null record;
   type Gtk_Tree_Expander is access all Gtk_Tree_Expander_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Tree_Expander);
   procedure Initialize
      (Self : not null access Gtk_Tree_Expander_Record'Class);
   --  Creates a new `GtkTreeExpander`
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Tree_Expander_New return Gtk_Tree_Expander;
   --  Creates a new `GtkTreeExpander`

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_expander_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : not null access Gtk_Tree_Expander_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget displayed by Self.
   --  @return The child displayed by Self. Has transfer-ownership='none'.

   procedure Set_Child
      (Self  : not null access Gtk_Tree_Expander_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the content widget to display.
   --  @param Child a `GtkWidget`

   function Get_Hide_Expander
      (Self : not null access Gtk_Tree_Expander_Record) return Boolean;
   --  Gets whether the TreeExpander should be hidden in a GtkTreeListRow.
   --  Since: gtk+ 4.10
   --  @return TRUE if the expander icon should be hidden. Otherwise FALSE.

   procedure Set_Hide_Expander
      (Self          : not null access Gtk_Tree_Expander_Record;
       Hide_Expander : Boolean);
   --  Sets whether the expander icon should be visible in a GtkTreeListRow.
   --  Since: gtk+ 4.10
   --  @param Hide_Expander TRUE if the expander should be hidden. Otherwise
   --  FALSE.

   function Get_Indent_For_Depth
      (Self : not null access Gtk_Tree_Expander_Record) return Boolean;
   --  TreeExpander indents each level of depth with an additional indent.
   --  Since: gtk+ 4.10
   --  @return TRUE if the child should be indented . Otherwise FALSE.

   procedure Set_Indent_For_Depth
      (Self             : not null access Gtk_Tree_Expander_Record;
       Indent_For_Depth : Boolean);
   --  Sets if the TreeExpander should indent the child according to its
   --  depth.
   --  Since: gtk+ 4.10
   --  @param Indent_For_Depth TRUE if the child should be indented. Otherwise
   --  FALSE.

   function Get_Indent_For_Icon
      (Self : not null access Gtk_Tree_Expander_Record) return Boolean;
   --  TreeExpander indents the child by the width of an expander-icon if it
   --  is not expandable.
   --  Since: gtk+ 4.6
   --  @return TRUE if the child should be indented when not expandable.
   --  Otherwise FALSE.

   procedure Set_Indent_For_Icon
      (Self            : not null access Gtk_Tree_Expander_Record;
       Indent_For_Icon : Boolean);
   --  Sets if the TreeExpander should indent the child by the width of an
   --  expander-icon when it is not expandable.
   --  Since: gtk+ 4.6
   --  @param Indent_For_Icon TRUE if the child should be indented without
   --  expander. Otherwise FALSE.

   function Get_Item
      (Self : not null access Gtk_Tree_Expander_Record)
       return Glib.Object.GObject;
   --  Forwards the item set on the `GtkTreeListRow` that Self is managing.
   --  This call is essentially equivalent to calling:
   --  ```c gtk_tree_list_row_get_item (gtk_tree_expander_get_list_row
   --  (Self)); ```
   --  @return The item of the row this expander manages

   function Get_List_Row
      (Self : not null access Gtk_Tree_Expander_Record)
       return Gtk.Tree_List_Row.Gtk_Tree_List_Row;
   --  Gets the list row managed by Self.
   --  @return The list row displayed by Self. Has transfer-ownership='none'.

   procedure Set_List_Row
      (Self     : not null access Gtk_Tree_Expander_Record;
       List_Row : access Gtk.Tree_List_Row.Gtk_Tree_List_Row_Record'Class);
   --  Sets the tree list row that this expander should manage.
   --  @param List_Row a `GtkTreeListRow`

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Tree_Expander_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Tree_Expander_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Tree_Expander_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Tree_Expander_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Tree_Expander_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Tree_Expander_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Tree_Expander_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Tree_Expander_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Tree_Expander_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Tree_Expander_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Tree_Expander_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Tree_Expander_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Tree_Expander_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Tree_Expander_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Tree_Expander_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget with the actual contents.

   Hide_Expander_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the expander icon should be hidden in a GtkTreeListRow. Note
   --  that this property simply hides the icon. The actions and keybinding
   --  (i.e. collapse and expand) are not affected by this property.
   --
   --  A common use for this property would be to bind to the number of
   --  children in a GtkTreeListRow's model in order to hide the expander when
   --  a row has no children.

   Indent_For_Depth_Property : constant Glib.Properties.Property_Boolean;
   --  TreeExpander indents the child according to its depth.

   Indent_For_Icon_Property : constant Glib.Properties.Property_Boolean;
   --  TreeExpander indents the child by the width of an expander-icon if it
   --  is not expandable.

   Item_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Object.GObject
   --  The item held by this expander's row.

   List_Row_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Tree_List_Row.Gtk_Tree_List_Row
   --  The list row to track for expander state.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Tree_Expander_Record, Gtk_Tree_Expander);
   function "+"
     (Widget : access Gtk_Tree_Expander_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Tree_Expander
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Tree_Expander_Record, Gtk_Tree_Expander);
   function "+"
     (Widget : access Gtk_Tree_Expander_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Tree_Expander
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Tree_Expander_Record, Gtk_Tree_Expander);
   function "+"
     (Widget : access Gtk_Tree_Expander_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Tree_Expander
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   List_Row_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("list-row");
   Item_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("item");
   Indent_For_Icon_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("indent-for-icon");
   Indent_For_Depth_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("indent-for-depth");
   Hide_Expander_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hide-expander");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Tree_Expander;
