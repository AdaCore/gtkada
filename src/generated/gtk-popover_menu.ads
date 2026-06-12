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

--  A subclass of `GtkPopover` that implements menu behavior.
--
--  <picture> <source srcset="menu-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkPopoverMenu" src="menu.png"> </picture>
--  `GtkPopoverMenu` treats its children like menus and allows switching
--  between them. It can open submenus as traditional, nested submenus, or in a
--  more touch-friendly sliding fashion. The property
--  [propertyGtk.PopoverMenu:flags] controls this appearance.
--
--  `GtkPopoverMenu` is meant to be used primarily with menu models, using
--  [ctorGtk.PopoverMenu.new_from_model]. If you need to put other widgets such
--  as a `GtkSpinButton` or a `GtkSwitch` into a popover, you can use
--  [methodGtk.PopoverMenu.add_child].
--
--  For more dialog-like behavior, use a plain `GtkPopover`.
--
--  ## Menu models
--
--  The XML format understood by `GtkBuilder` for `GMenuModel` consists of a
--  toplevel `<menu>` element, which contains one or more `<item>` elements.
--  Each `<item>` element contains `<attribute>` and `<link>` elements with a
--  mandatory name attribute. `<link>` elements have the same content model as
--  `<menu>`. Instead of `<link name="submenu">` or `<link name="section">`,
--  you can use `<submenu>` or `<section>` elements.
--
--  ```xml <menu id='app-menu'> <section> <item> <attribute name='label'
--  translatable='yes'>_New Window</attribute> <attribute
--  name='action'>app.new</attribute> </item> <item> <attribute name='label'
--  translatable='yes'>_About Sunny</attribute> <attribute
--  name='action'>app.about</attribute> </item> <item> <attribute name='label'
--  translatable='yes'>_Quit</attribute> <attribute
--  name='action'>app.quit</attribute> </item> </section> </menu> ```
--
--  Attribute values can be translated using gettext, like other `GtkBuilder`
--  content. `<attribute>` elements can be marked for translation with a
--  `translatable="yes"` attribute. It is also possible to specify message
--  context and translator comments, using the context and comments attributes.
--  To make use of this, the `GtkBuilder` must have been given the gettext
--  domain to use.
--
--  The following attributes are used when constructing menu items:
--
--  - "label": a user-visible string to display - "use-markup": whether the
--  text in the menu item includes [Pango
--  markup](https://docs.gtk.org/Pango/pango_markup.html) - "action": the
--  prefixed name of the action to trigger - "target": the parameter to use
--  when activating the action - "icon" and "verb-icon": names of icons that
--  may be displayed - "submenu-action": name of an action that may be used to
--  track whether a submenu is open - "hidden-when": a string used to determine
--  when the item will be hidden. Possible values include "action-disabled",
--  "action-missing", "macos-menubar". This is mainly useful for exported
--  menus, see [methodGtk.Application.set_menubar]. - "custom": a string used
--  to match against the ID of a custom child added with
--  [methodGtk.PopoverMenu.add_child], [methodGtk.PopoverMenuBar.add_child], or
--  in the ui file with `<child type="ID">`.
--
--  The following attributes are used when constructing sections:
--
--  - "label": a user-visible string to use as section heading -
--  "display-hint": a string used to determine special formatting for the
--  section. Possible values include "horizontal-buttons", "circular-buttons"
--  and "inline-buttons". They all indicate that section should be displayed as
--  a horizontal row of buttons. - "text-direction": a string used to determine
--  the `GtkTextDirection` to use when "display-hint" is set to
--  "horizontal-buttons". Possible values include "rtl", "ltr", and "none".
--
--  The following attributes are used when constructing submenus:
--
--  - "label": a user-visible string to display - "icon": icon name to display
--  - "gtk-macos-special": (macOS only, ignored by others) Add special meaning
--  to a menu in the macOS menu bar. See [Using GTK on Apple macOS](osx.html).
--
--  Menu items will also show accelerators, which are usually associated with
--  actions via [methodGtk.Application.set_accels_for_action],
--  [methodWidgetclass.add_binding_action] or
--  [methodGtk.ShortcutController.add_shortcut].
--
--  # Shortcuts and Gestures
--
--  `GtkPopoverMenu` supports the following keyboard shortcuts:
--
--  - <kbd>Space</kbd> activates the default widget.
--
--  # CSS Nodes
--
--  `GtkPopoverMenu` is just a subclass of `GtkPopover` that adds custom
--  content to it, therefore it has the same CSS nodes. It is one of the cases
--  that add a `.menu` style class to the main `popover` node.
--
--  Menu items have nodes with name `button` and class `.model`. If a section
--  display-hint is set, the section gets a node `box` with class `horizontal`
--  plus a class with the same text as the display hint. Note that said box may
--  not be the direct ancestor of the item `button`s. Thus, for example, to
--  style items in an `inline-buttons` section, select `.inline-buttons
--  button.model`. Other things that may be of interest to style in menus
--  include `label` nodes.
--
--  # Accessibility
--
--  `GtkPopoverMenu` uses the [enumGtk.AccessibleRole.menu] role, and its
--  items use the [enumGtk.AccessibleRole.menu_item],
--  [enumGtk.AccessibleRole.checkbox] or
--  [enumGtk.AccessibleRole.menu_item_radio] roles, depending on the action
--  they are connected to.
--
--  <group>Menus and Toolbars</group>
--  <gtkada_demo>create_menu.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Gdk;                     use Gdk;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Menu_Model;         use Glib.Menu_Model;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Native;              use Gtk.Native;
with Gtk.Popover;             use Gtk.Popover;
with Gtk.Shortcut_Manager;    use Gtk.Shortcut_Manager;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Popover_Menu is

   type Gtk_Popover_Menu_Record is new Gtk_Popover_Record with null record;
   type Gtk_Popover_Menu is access all Gtk_Popover_Menu_Record'Class;

   type Gtk_Popover_Menu_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Popover_Menu_Flags);
   --  Flags that affect how [classGtk.PopoverMenu] widgets built from a
   --  [classGio.MenuModel] are created and displayed.

   Popover_Menu_Sliding : constant Gtk_Popover_Menu_Flags := 0;
   Popover_Menu_Nested : constant Gtk_Popover_Menu_Flags := 1;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Popover_Menu_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Popover_Menu_Flags);
   type Property_Gtk_Popover_Menu_Flags is new Gtk_Popover_Menu_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_From_Model
      (Self  : out Gtk_Popover_Menu;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   procedure Initialize_From_Model
      (Self  : not null access Gtk_Popover_Menu_Record'Class;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Creates a `GtkPopoverMenu` and populates it according to Model.
   --  The created buttons are connected to actions found in the
   --  `GtkApplicationWindow` to which the popover belongs - typically by means
   --  of being attached to a widget that is contained within the
   --  `GtkApplicationWindow`s widget hierarchy.
   --  Actions can also be added using [methodGtk.Widget.insert_action_group]
   --  on the menus attach widget or on any of its parent widgets.
   --  This function creates menus with sliding submenus. See
   --  [ctorGtk.PopoverMenu.new_from_model_full] for a way to control this.
   --  Initialize_From_Model does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Model a `GMenuModel`

   function Gtk_Popover_Menu_New_From_Model
      (Model : access Glib.Menu_Model.Gmenu_Model_Record'Class)
       return Gtk_Popover_Menu;
   --  Creates a `GtkPopoverMenu` and populates it according to Model.
   --  The created buttons are connected to actions found in the
   --  `GtkApplicationWindow` to which the popover belongs - typically by means
   --  of being attached to a widget that is contained within the
   --  `GtkApplicationWindow`s widget hierarchy.
   --  Actions can also be added using [methodGtk.Widget.insert_action_group]
   --  on the menus attach widget or on any of its parent widgets.
   --  This function creates menus with sliding submenus. See
   --  [ctorGtk.PopoverMenu.new_from_model_full] for a way to control this.
   --  @param Model a `GMenuModel`

   procedure Gtk_New_From_Model_Full
      (Self  : out Gtk_Popover_Menu;
       Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Flags : Gtk_Popover_Menu_Flags);
   procedure Initialize_From_Model_Full
      (Self  : not null access Gtk_Popover_Menu_Record'Class;
       Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Flags : Gtk_Popover_Menu_Flags);
   --  Creates a `GtkPopoverMenu` and populates it according to Model.
   --  The created buttons are connected to actions found in the action groups
   --  that are accessible from the parent widget. This includes the
   --  `GtkApplicationWindow` to which the popover belongs. Actions can also be
   --  added using [methodGtk.Widget.insert_action_group] on the parent widget
   --  or on any of its parent widgets.
   --  Initialize_From_Model_Full does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  @param Model a `GMenuModel`
   --  @param Flags flags that affect how the menu is created

   function Gtk_Popover_Menu_New_From_Model_Full
      (Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Flags : Gtk_Popover_Menu_Flags) return Gtk_Popover_Menu;
   --  Creates a `GtkPopoverMenu` and populates it according to Model.
   --  The created buttons are connected to actions found in the action groups
   --  that are accessible from the parent widget. This includes the
   --  `GtkApplicationWindow` to which the popover belongs. Actions can also be
   --  added using [methodGtk.Widget.insert_action_group] on the parent widget
   --  or on any of its parent widgets.
   --  @param Model a `GMenuModel`
   --  @param Flags flags that affect how the menu is created

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_popover_menu_get_type");

   -------------
   -- Methods --
   -------------

   function Add_Child
      (Self  : not null access Gtk_Popover_Menu_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Id    : UTF8_String) return Boolean;
   --  Adds a custom widget to a generated menu.
   --  For this to work, the menu model of Popover must have an item with a
   --  `custom` attribute that matches Id.
   --  @param Child the `GtkWidget` to add
   --  @param Id the ID to insert Child at
   --  @return True if Id was found and the widget added

   function Get_Flags
      (Self : not null access Gtk_Popover_Menu_Record)
       return Gtk_Popover_Menu_Flags;
   --  Returns the flags that Popover uses to create/display a menu from its
   --  model.
   --  Since: gtk+ 4.14
   --  @return the `GtkPopoverMenuFlags`

   procedure Set_Flags
      (Self  : not null access Gtk_Popover_Menu_Record;
       Flags : Gtk_Popover_Menu_Flags);
   --  Sets the flags that Popover uses to create/display a menu from its
   --  model.
   --  If a model is set and the flags change, contents are rebuilt, so if
   --  setting properties individually, set flags before model to avoid a
   --  redundant rebuild.
   --  Since: gtk+ 4.14
   --  @param Flags a set of `GtkPopoverMenuFlags`

   function Get_Menu_Model
      (Self : not null access Gtk_Popover_Menu_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Returns the menu model used to populate the popover.
   --  @return the menu model of Popover

   procedure Set_Menu_Model
      (Self  : not null access Gtk_Popover_Menu_Record;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets a new menu model on Popover.
   --  The existing contents of Popover are removed, and the Popover is
   --  populated with new contents according to Model.
   --  @param Model a `GMenuModel`

   function Remove_Child
      (Self  : not null access Gtk_Popover_Menu_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Removes a widget that has previously been added with
   --  [methodGtk.PopoverMenu.add_child]
   --  @param Child the `GtkWidget` to remove
   --  @return True if the widget was removed

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Popover_Menu_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Popover_Menu_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Popover_Menu_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Popover_Menu_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Popover_Menu_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Popover_Menu_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Popover_Menu_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Popover_Menu_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Popover_Menu_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Popover_Menu_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Popover_Menu_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Popover_Menu_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Popover_Menu_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Popover_Menu_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Popover_Menu_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Surface
      (Self : not null access Gtk_Popover_Menu_Record)
       return Gdk.Gdk_Surface;

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Popover_Menu_Record;
       X    : out Gdouble;
       Y    : out Gdouble);

   procedure Realize (Self : not null access Gtk_Popover_Menu_Record);

   procedure Unrealize (Self : not null access Gtk_Popover_Menu_Record);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Flags_Property : constant Gtk.Popover_Menu.Property_Gtk_Popover_Menu_Flags;
   --  Type: Gtk_Popover_Menu_Flags
   --  The flags that Popover uses to create/display a menu from its model.
   --
   --  If a model is set and the flags change, contents are rebuilt, so if
   --  setting properties individually, set flags before model to avoid a
   --  redundant rebuild.

   Menu_Model_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model
   --  The model from which the menu is made.

   Visible_Submenu_Property : constant Glib.Properties.Property_String;
   --  The name of the visible submenu.

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
   --  - "Gtk.Native"
   --
   --  - "Gtk.ShortcutManager"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Popover_Menu_Record, Gtk_Popover_Menu);
   function "+"
     (Widget : access Gtk_Popover_Menu_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Popover_Menu
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Popover_Menu_Record, Gtk_Popover_Menu);
   function "+"
     (Widget : access Gtk_Popover_Menu_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Popover_Menu
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Popover_Menu_Record, Gtk_Popover_Menu);
   function "+"
     (Widget : access Gtk_Popover_Menu_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Popover_Menu
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Native is new Glib.Types.Implements
     (Gtk.Native.Gtk_Native, Gtk_Popover_Menu_Record, Gtk_Popover_Menu);
   function "+"
     (Widget : access Gtk_Popover_Menu_Record'Class)
   return Gtk.Native.Gtk_Native
   renames Implements_Gtk_Native.To_Interface;
   function "-"
     (Interf : Gtk.Native.Gtk_Native)
   return Gtk_Popover_Menu
   renames Implements_Gtk_Native.To_Object;

   package Implements_Gtk_Shortcut_Manager is new Glib.Types.Implements
     (Gtk.Shortcut_Manager.Gtk_Shortcut_Manager, Gtk_Popover_Menu_Record, Gtk_Popover_Menu);
   function "+"
     (Widget : access Gtk_Popover_Menu_Record'Class)
   return Gtk.Shortcut_Manager.Gtk_Shortcut_Manager
   renames Implements_Gtk_Shortcut_Manager.To_Interface;
   function "-"
     (Interf : Gtk.Shortcut_Manager.Gtk_Shortcut_Manager)
   return Gtk_Popover_Menu
   renames Implements_Gtk_Shortcut_Manager.To_Object;

private
   Visible_Submenu_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("visible-submenu");
   Menu_Model_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("menu-model");
   Flags_Property : constant Gtk.Popover_Menu.Property_Gtk_Popover_Menu_Flags :=
     Gtk.Popover_Menu.Build ("flags");
end Gtk.Popover_Menu;
