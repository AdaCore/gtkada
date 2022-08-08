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
--  > GtkUIManager is deprecated since GTK+ 3.10. To construct user interfaces
--  > from XML definitions, you should use Gtk.Builder.Gtk_Builder,
--  Glib.Menu_Model.Gmenu_Model, et al. To > work with actions, use
--  Glib.Action.Gaction, Gtk.Actionable.Gtk_Actionable et al. These newer
--  classes > support richer functionality and integration with various desktop
--  shells. > It should be possible to migrate most/all functionality from
--  GtkUIManager.
--
--  A Gtk.UI_Manager.Gtk_UI_Manager constructs a user interface (menus and
--  toolbars) from one or more UI definitions, which reference actions from one
--  or more action groups.
--
--  # UI Definitions # {XML-UI}
--
--  The UI definitions are specified in an XML format which can be roughly
--  described by the following DTD.
--
--  > Do not confuse the GtkUIManager UI Definitions described here with > the
--  similarly named [GtkBuilder UI Definitions][BUILDER-UI].
--
--  |[ <!ELEMENT ui (menubar|toolbar|popup|accelerator)* > <!ELEMENT menubar
--  (menuitem|separator|placeholder|menu)* > <!ELEMENT menu
--  (menuitem|separator|placeholder|menu)* > <!ELEMENT popup
--  (menuitem|separator|placeholder|menu)* > <!ELEMENT toolbar
--  (toolitem|separator|placeholder)* > <!ELEMENT placeholder
--  (menuitem|toolitem|separator|placeholder|menu)* > <!ELEMENT menuitem EMPTY
--  > <!ELEMENT toolitem (menu?) > <!ELEMENT separator EMPTY > <!ELEMENT
--  accelerator EMPTY > <!ATTLIST menubar name IMPLIED action IMPLIED >
--  <!ATTLIST toolbar name IMPLIED action IMPLIED > <!ATTLIST popup name
--  IMPLIED action IMPLIED accelerators (true|false) IMPLIED > <!ATTLIST
--  placeholder name IMPLIED action IMPLIED > <!ATTLIST separator name IMPLIED
--  action IMPLIED expand (true|false) IMPLIED > <!ATTLIST menu name IMPLIED
--  action REQUIRED position (top|bot) IMPLIED > <!ATTLIST menuitem name
--  IMPLIED action REQUIRED position (top|bot) IMPLIED always-show-image
--  (true|false) IMPLIED > <!ATTLIST toolitem name IMPLIED action REQUIRED
--  position (top|bot) IMPLIED > <!ATTLIST accelerator name IMPLIED action
--  REQUIRED > ]|
--
--  There are some additional restrictions beyond those specified in the DTD,
--  e.g. every toolitem must have a toolbar in its anchestry and every menuitem
--  must have a menubar or popup in its anchestry. Since a GMarkup_Parser is
--  used to parse the UI description, it must not only be valid XML, but valid
--  markup.
--
--  If a name is not specified, it defaults to the action. If an action is not
--  specified either, the element name is used. The name and action attributes
--  must not contain "/" characters after parsing (since that would mess up
--  path lookup) and must be usable as XML attributes when enclosed in
--  doublequotes, thus they must not """ characters or references to the &quot;
--  entity.
--
--  # A UI definition #
--
--  |[ <ui> <menubar> <menu name="FileMenu" action="FileMenuAction"> <menuitem
--  name="New" action="New2Action" /> <placeholder name="FileMenuAdditions" />
--  </menu> <menu name="JustifyMenu" action="JustifyMenuAction"> <menuitem
--  name="Left" action="justify-left"/> <menuitem name="Centre"
--  action="justify-center"/> <menuitem name="Right" action="justify-right"/>
--  <menuitem name="Fill" action="justify-fill"/> </menu> </menubar> <toolbar
--  action="toolbar1"> <placeholder name="JustifyToolItems"> <separator/>
--  <toolitem name="Left" action="justify-left"/> <toolitem name="Centre"
--  action="justify-center"/> <toolitem name="Right" action="justify-right"/>
--  <toolitem name="Fill" action="justify-fill"/> <separator/> </placeholder>
--  </toolbar> </ui> ]|
--
--  The constructed widget hierarchy is very similar to the element tree of
--  the XML, with the exception that placeholders are merged into their
--  parents. The correspondence of XML elements to widgets should be almost
--  obvious:
--
--  - menubar
--
--  a Gtk.Menu_Bar.Gtk_Menu_Bar
--
--  - toolbar
--
--  a Gtk.Toolbar.Gtk_Toolbar
--
--  - popup
--
--  a toplevel Gtk.Menu.Gtk_Menu
--
--  - menu
--
--  a Gtk.Menu.Gtk_Menu attached to a menuitem
--
--  - menuitem
--
--  a Gtk.Menu_Item.Gtk_Menu_Item subclass, the exact type depends on the
--  action
--
--  - toolitem
--
--  a Gtk.Tool_Item.Gtk_Tool_Item subclass, the exact type depends on the
--  action. Note that toolitem elements may contain a menu element, but only if
--  their associated action specifies a
--  Gtk.Menu_Tool_Button.Gtk_Menu_Tool_Button as proxy.
--
--  - separator
--
--  a Gtk.Separator_Menu_Item.Gtk_Separator_Menu_Item or
--  Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item
--
--  - accelerator
--
--  a keyboard accelerator
--
--  The "position" attribute determines where a constructed widget is
--  positioned wrt. to its siblings in the partially constructed tree. If it is
--  "top", the widget is prepended, otherwise it is appended.
--
--  # UI Merging # {UI-Merging}
--
--  The most remarkable feature of Gtk.UI_Manager.Gtk_UI_Manager is that it
--  can overlay a set of menuitems and toolitems over another one, and demerge
--  them later.
--
--  Merging is done based on the names of the XML elements. Each element is
--  identified by a path which consists of the names of its anchestors,
--  separated by slashes. For example, the menuitem named "Left" in the example
--  above has the path `/ui/menubar/JustifyMenu/Left` and the toolitem with the
--  same name has path `/ui/toolbar1/JustifyToolItems/Left`.
--
--  # Accelerators #
--
--  Every action has an accelerator path. Accelerators are installed together
--  with menuitem proxies, but they can also be explicitly added with
--  <accelerator> elements in the UI definition. This makes it possible to have
--  accelerators for actions even if they have no visible proxies.
--
--  # Smart Separators # {Smart-Separators}
--
--  The separators created by Gtk.UI_Manager.Gtk_UI_Manager are "smart", i.e.
--  they do not show up in the UI unless they end up between two visible menu
--  or tool items. Separators which are located at the very beginning or end of
--  the menu or toolbar containing them, or multiple separators next to each
--  other, are hidden. This is a useful feature, since the merging of UI
--  elements from multiple sources can make it hard or impossible to determine
--  in advance whether a separator will end up in such an unfortunate position.
--
--  For separators in toolbars, you can set `expand="true"` to turn them from
--  a small, visible separator to an expanding, invisible one. Toolitems
--  following an expanding separator are effectively right-aligned.
--
--  # Empty Menus
--
--  Submenus pose similar problems to separators inconnection with merging. It
--  is impossible to know in advance whether they will end up empty after
--  merging. Gtk.UI_Manager.Gtk_UI_Manager offers two ways to treat empty
--  submenus:
--
--  - make them disappear by hiding the menu item they're attached to
--
--  - add an insensitive "Empty" item
--
--  The behaviour is chosen based on the "hide_if_empty" property of the
--  action to which the submenu is associated.
--
--  # GtkUIManager as GtkBuildable #
--  {Gtk.UI_Manager.Gtk_UI_Manager-BUILDER-UI}
--
--  The GtkUIManager implementation of the GtkBuildable interface accepts
--  GtkActionGroup objects as <child> elements in UI definitions.
--
--  A GtkUIManager UI definition as described above can be embedded in an
--  GtkUIManager <object> element in a GtkBuilder UI definition.
--
--  The widgets that are constructed by a GtkUIManager can be embedded in
--  other parts of the constructed user interface with the help of the
--  "constructor" attribute. See the example below.
--
--  ## An embedded GtkUIManager UI definition
--
--  |[ <object class="GtkUIManager" id="uiman"> <child> <object
--  class="GtkActionGroup" id="actiongroup"> <child> <object class="GtkAction"
--  id="file"> <property name="label">_File</property> </object> </child>
--  </object> </child> <ui> <menubar name="menubar1"> <menu action="file">
--  </menu> </menubar> </ui> </object> <object class="GtkWindow"
--  id="main-window"> <child> <object class="GtkMenuBar" id="menubar1"
--  constructor="uiman"/> </child> </object> ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Error;              use Glib.Error;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accel_Group;         use Gtk.Accel_Group;
with Gtk.Action;              use Gtk.Action;
with Gtk.Action_Group;        use Gtk.Action_Group;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.UI_Manager is

   type Gtk_UI_Manager_Record is new GObject_Record with null record;
   type Gtk_UI_Manager is access all Gtk_UI_Manager_Record'Class;

   type Manager_Item_Type is mod 2 ** Integer'Size;
   pragma Convention (C, Manager_Item_Type);
   --  These enumeration values are used by Gtk.UI_Manager.Add_UI to determine
   --  what UI element to create.

   Manager_Auto : constant Manager_Item_Type := 0;
   Manager_Menubar : constant Manager_Item_Type := 1;
   Manager_Menu : constant Manager_Item_Type := 2;
   Manager_Toolbar : constant Manager_Item_Type := 4;
   Manager_Placeholder : constant Manager_Item_Type := 8;
   Manager_Popup : constant Manager_Item_Type := 16;
   Manager_Menuitem : constant Manager_Item_Type := 32;
   Manager_Toolitem : constant Manager_Item_Type := 64;
   Manager_Separator : constant Manager_Item_Type := 128;
   Manager_Accelerator : constant Manager_Item_Type := 256;
   Manager_Popup_With_Accels : constant Manager_Item_Type := 512;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Manager_Item_Type_Properties is
      new Generic_Internal_Discrete_Property (Manager_Item_Type);
   type Property_Manager_Item_Type is new Manager_Item_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_UI_Manager);
   procedure Initialize (Self : not null access Gtk_UI_Manager_Record'Class);
   --  Creates a new ui manager object.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_UI_Manager_New return Gtk_UI_Manager;
   --  Creates a new ui manager object.
   --  Since: gtk+ 2.4

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_ui_manager_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_UI
      (Self     : not null access Gtk_UI_Manager_Record;
       Merge_Id : Guint;
       Path     : UTF8_String;
       Name     : UTF8_String;
       Action   : UTF8_String := "";
       The_Type : Manager_Item_Type := Manager_Auto;
       Top      : Boolean := False);
   pragma Obsolescent (Add_UI);
   --  Adds a UI element to the current contents of Manager.
   --  If Type is Gtk.UI_Manager.Manager_Auto, GTK+ inserts a menuitem,
   --  toolitem or separator if such an element can be inserted at the place
   --  determined by Path. Otherwise Type must indicate an element that can be
   --  inserted at the place determined by Path.
   --  If Path points to a menuitem or toolitem, the new element will be
   --  inserted before or after this item, depending on Top.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "merge_id": the merge id for the merged UI, see
   --  Gtk.UI_Manager.New_Merge_Id
   --  "path": a path
   --  "name": the name for the added UI element
   --  "action": the name of the action to be proxied, or null to add a
   --  separator
   --  "type": the type of UI element to add.
   --  "top": if True, the UI element is added before its siblings, otherwise
   --  it is added after its siblings.

   function Add_UI_From_File
      (Self     : not null access Gtk_UI_Manager_Record;
       Filename : UTF8_String) return Guint;
   pragma Obsolescent (Add_UI_From_File);
   --  Parses a file containing a [UI definition][XML-UI] and merges it with
   --  the current contents of Manager.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "filename": the name of the file to parse

   function Add_UI_From_Resource
      (Self          : not null access Gtk_UI_Manager_Record;
       Resource_Path : UTF8_String) return Guint;
   pragma Obsolescent (Add_UI_From_Resource);
   --  Parses a resource file containing a [UI definition][XML-UI] and merges
   --  it with the current contents of Manager.
   --  Since: gtk+ 3.4
   --  Deprecated since 3.10, 1
   --  "resource_path": the resource path of the file to parse

   function Add_UI_From_String
      (Self   : not null access Gtk_UI_Manager_Record;
       Buffer : UTF8_String;
       Error  : access Glib.Error.GError) return Guint;
   pragma Obsolescent (Add_UI_From_String);
   --  Parses a string containing a [UI definition][XML-UI] and merges it with
   --  the current contents of Manager. An enclosing <ui> element is added if
   --  it is missing.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "buffer": the string to parse

   procedure Ensure_Update (Self : not null access Gtk_UI_Manager_Record);
   pragma Obsolescent (Ensure_Update);
   --  Makes sure that all pending updates to the UI have been completed.
   --  This may occasionally be necessary, since Gtk.UI_Manager.Gtk_UI_Manager
   --  updates the UI in an idle function. A typical example where this
   --  function is useful is to enforce that the menubar and toolbar have been
   --  added to the main window before showing it: |[<!-- language="C" -->
   --  gtk_container_add (GTK_CONTAINER (window), vbox); g_signal_connect
   --  (merge, "add-widget", G_CALLBACK (add_widget), vbox);
   --  gtk_ui_manager_add_ui_from_file (merge, "my-menus");
   --  gtk_ui_manager_add_ui_from_file (merge, "my-toolbars");
   --  gtk_ui_manager_ensure_update (merge); gtk_widget_show (window); ]|
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   function Get_Accel_Group
      (Self : not null access Gtk_UI_Manager_Record)
       return Gtk.Accel_Group.Gtk_Accel_Group;
   pragma Obsolescent (Get_Accel_Group);
   --  Returns the Gtk.Accel_Group.Gtk_Accel_Group associated with Manager.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   function Get_Action
      (Self : not null access Gtk_UI_Manager_Record;
       Path : UTF8_String) return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Action);
   --  Looks up an action by following a path. See Gtk.UI_Manager.Get_Widget
   --  for more information about paths.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "path": a path

   function Get_Action_Groups
      (Self : not null access Gtk_UI_Manager_Record)
       return Glib.Object.Object_Simple_List.Glist;
   pragma Obsolescent (Get_Action_Groups);
   --  Returns the list of action groups associated with Manager.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   function Get_Add_Tearoffs
      (Self : not null access Gtk_UI_Manager_Record) return Boolean;
   pragma Obsolescent (Get_Add_Tearoffs);
   --  Returns whether menus generated by this Gtk.UI_Manager.Gtk_UI_Manager
   --  will have tearoff menu items.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.4, 1

   procedure Set_Add_Tearoffs
      (Self         : not null access Gtk_UI_Manager_Record;
       Add_Tearoffs : Boolean);
   pragma Obsolescent (Set_Add_Tearoffs);
   --  Sets the "add_tearoffs" property, which controls whether menus
   --  generated by this Gtk.UI_Manager.Gtk_UI_Manager will have tearoff menu
   --  items.
   --  Note that this only affects regular menus. Generated popup menus never
   --  have tearoff menu items.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.4, 1
   --  "add_tearoffs": whether tearoff menu items are added

   function Get_Toplevels
      (Self  : not null access Gtk_UI_Manager_Record;
       Types : Manager_Item_Type) return Gtk.Widget.Widget_SList.GSlist;
   pragma Obsolescent (Get_Toplevels);
   --  Obtains a list of all toplevel widgets of the requested types.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "types": specifies the types of toplevel widgets to include. Allowed
   --  types are GTK_UI_MANAGER_MENUBAR, GTK_UI_MANAGER_TOOLBAR and
   --  GTK_UI_MANAGER_POPUP.

   function Get_Ui
      (Self : not null access Gtk_UI_Manager_Record) return UTF8_String;
   pragma Obsolescent (Get_Ui);
   --  Creates a [UI definition][XML-UI] of the merged UI.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   function Get_Widget
      (Self : not null access Gtk_UI_Manager_Record;
       Path : UTF8_String) return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Widget);
   --  Looks up a widget by following a path. The path consists of the names
   --  specified in the XML description of the UI. separated by "/". Elements
   --  which don't have a name or action attribute in the XML (e.g. <popup>)
   --  can be addressed by their XML element name (e.g. "popup"). The root
   --  element ("/ui") can be omitted in the path.
   --  Note that the widget found by following a path that ends in a <menu>;
   --  element is the menuitem to which the menu is attached, not the menu it
   --  manages.
   --  Also note that the widgets constructed by a ui manager are not tied to
   --  the lifecycle of the ui manager. If you add the widgets returned by this
   --  function to some container or explicitly ref them, they will survive the
   --  destruction of the ui manager.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "path": a path

   procedure Insert_Action_Group
      (Self         : not null access Gtk_UI_Manager_Record;
       Action_Group : not null access Gtk.Action_Group.Gtk_Action_Group_Record'Class;
       Pos          : Glib.Gint);
   pragma Obsolescent (Insert_Action_Group);
   --  Inserts an action group into the list of action groups associated with
   --  Manager. Actions in earlier groups hide actions with the same name in
   --  later groups.
   --  If Pos is larger than the number of action groups in Manager, or
   --  negative, Action_Group will be inserted at the end of the internal list.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "action_group": the action group to be inserted
   --  "pos": the position at which the group will be inserted.

   function New_Merge_Id
      (Self : not null access Gtk_UI_Manager_Record) return Guint;
   pragma Obsolescent (New_Merge_Id);
   --  Returns an unused merge id, suitable for use with
   --  Gtk.UI_Manager.Add_UI.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   procedure Remove_Action_Group
      (Self         : not null access Gtk_UI_Manager_Record;
       Action_Group : not null access Gtk.Action_Group.Gtk_Action_Group_Record'Class);
   pragma Obsolescent (Remove_Action_Group);
   --  Removes an action group from the list of action groups associated with
   --  Manager.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "action_group": the action group to be removed

   procedure Remove_UI
      (Self     : not null access Gtk_UI_Manager_Record;
       Merge_Id : Guint);
   pragma Obsolescent (Remove_UI);
   --  Unmerges the part of Manager's content identified by Merge_Id.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "merge_id": a merge id as returned by Gtk.UI_Manager.Add_UI_From_String

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Add_Tearoffs_Property : constant Glib.Properties.Property_Boolean;
   --  The "add-tearoffs" property controls whether generated menus have
   --  tearoff menu items.
   --
   --  Note that this only affects regular menus. Generated popup menus never
   --  have tearoff menu items.

   Ui_Property : constant Glib.Properties.Property_String;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_UI_Manager_Void is not null access procedure (Self : access Gtk_UI_Manager_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Actions_Changed : constant Glib.Signal_Name := "actions-changed";
   procedure On_Actions_Changed
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_Gtk_UI_Manager_Void;
       After : Boolean := False);
   procedure On_Actions_Changed
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::actions-changed signal is emitted whenever the set of actions
   --  changes.

   type Cb_Gtk_UI_Manager_Gtk_Widget_Void is not null access procedure
     (Self   : access Gtk_UI_Manager_Record'Class;
      Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   type Cb_GObject_Gtk_Widget_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   Signal_Add_Widget : constant Glib.Signal_Name := "add-widget";
   procedure On_Add_Widget
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_Gtk_UI_Manager_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Add_Widget
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::add-widget signal is emitted for each generated menubar and
   --  toolbar. It is not emitted for generated popup menus, which can be
   --  obtained by Gtk.UI_Manager.Get_Widget.

   type Cb_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void is not null access procedure
     (Self   : access Gtk_UI_Manager_Record'Class;
      Action : not null access Gtk.Action.Gtk_Action_Record'Class;
      Proxy  : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   type Cb_GObject_Gtk_Action_Gtk_Widget_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Action : not null access Gtk.Action.Gtk_Action_Record'Class;
      Proxy  : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   Signal_Connect_Proxy : constant Glib.Signal_Name := "connect-proxy";
   procedure On_Connect_Proxy
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Connect_Proxy
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_GObject_Gtk_Action_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::connect-proxy signal is emitted after connecting a proxy to an
   --  action in the group.
   --
   --  This is intended for simple customizations for which a custom action
   --  class would be too clumsy, e.g. showing tooltips for menuitems in the
   --  statusbar.
   -- 
   --  Callback parameters:
   --    --  "action": the action
   --    --  "proxy": the proxy

   Signal_Disconnect_Proxy : constant Glib.Signal_Name := "disconnect-proxy";
   procedure On_Disconnect_Proxy
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Disconnect_Proxy
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_GObject_Gtk_Action_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::disconnect-proxy signal is emitted after disconnecting a proxy
   --  from an action in the group.
   -- 
   --  Callback parameters:
   --    --  "action": the action
   --    --  "proxy": the proxy

   type Cb_Gtk_UI_Manager_Gtk_Action_Void is not null access procedure
     (Self   : access Gtk_UI_Manager_Record'Class;
      Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   type Cb_GObject_Gtk_Action_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   Signal_Post_Activate : constant Glib.Signal_Name := "post-activate";
   procedure On_Post_Activate
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_Gtk_UI_Manager_Gtk_Action_Void;
       After : Boolean := False);
   procedure On_Post_Activate
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_GObject_Gtk_Action_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::post-activate signal is emitted just after the Action is
   --  activated.
   --
   --  This is intended for applications to get notification just after any
   --  action is activated.

   Signal_Pre_Activate : constant Glib.Signal_Name := "pre-activate";
   procedure On_Pre_Activate
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_Gtk_UI_Manager_Gtk_Action_Void;
       After : Boolean := False);
   procedure On_Pre_Activate
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_GObject_Gtk_Action_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::pre-activate signal is emitted just before the Action is
   --  activated.
   --
   --  This is intended for applications to get notification just before any
   --  action is activated.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_UI_Manager_Record, Gtk_UI_Manager);
   function "+"
     (Widget : access Gtk_UI_Manager_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_UI_Manager
   renames Implements_Gtk_Buildable.To_Object;

private
   Ui_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("ui");
   Add_Tearoffs_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("add-tearoffs");
end Gtk.UI_Manager;
