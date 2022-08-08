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
--  Gtk.Application.Gtk_Application is a class that handles many important
--  aspects of a GTK+ application in a convenient fashion, without enforcing a
--  one-size-fits-all application model.
--
--  Currently, GtkApplication handles GTK+ initialization, application
--  uniqueness, session management, provides some basic scriptability and
--  desktop shell integration by exporting actions and menus and manages a list
--  of toplevel windows whose life-cycle is automatically tied to the
--  life-cycle of your application.
--
--  While GtkApplication works fine with plain Gtk_Windows, it is recommended
--  to use it together with Gtk.Application_Window.Gtk_Application_Window.
--
--  When GDK threads are enabled, GtkApplication will acquire the GDK lock
--  when invoking actions that arrive from other processes. The GDK lock is not
--  touched for local action invocations. In order to have actions invoked in a
--  predictable context it is therefore recommended that the GDK lock be held
--  while invoking actions locally with Glib.Action_Group.Activate_Action. The
--  same applies to actions associated with
--  Gtk.Application_Window.Gtk_Application_Window and to the "activate" and
--  "open" Glib.Application.Gapplication methods.
--
--  ## Automatic resources ## {automatic-resources}
--
--  Gtk.Application.Gtk_Application will automatically load menus from the
--  Gtk.Builder.Gtk_Builder resource located at "gtk/menus.ui", relative to the
--  application's resource base path (see
--  Glib.Application.Set_Resource_Base_Path). The menu with the ID "app-menu"
--  is taken as the application's app menu and the menu with the ID "menubar"
--  is taken as the application's menubar. Additional menus (most interesting
--  submenus) can be named and accessed via Gtk.Application.Get_Menu_By_Id
--  which allows for dynamic population of a part of the menu structure.
--
--  If the resources "gtk/menus-appmenu.ui" or "gtk/menus-traditional.ui" are
--  present then these files will be used in preference, depending on the value
--  of Gtk.Application.Prefers_App_Menu. If the resource "gtk/menus-common.ui"
--  is present it will be loaded as well. This is useful for storing items that
--  are referenced from both "gtk/menus-appmenu.ui" and
--  "gtk/menus-traditional.ui".
--
--  It is also possible to provide the menus manually using
--  Gtk.Application.Set_App_Menu and Gtk.Application.Set_Menubar.
--
--  Gtk.Application.Gtk_Application will also automatically setup an icon
--  search path for the default icon theme by appending "icons" to the resource
--  base path. This allows your application to easily store its icons as
--  resources. See Gtk.Icon_Theme.Add_Resource_Path for more information.
--
--  If there is a resource located at "gtk/help-overlay.ui" which defines a
--  Gtk.Shortcuts_Window.Gtk_Shortcuts_Window with ID "help_overlay" then
--  GtkApplication associates an instance of this shortcuts window with each
--  Gtk.Application_Window.Gtk_Application_Window and sets up keyboard
--  accelerators (Control-F1 and Control-?) to open it. To create a menu item
--  that displays the shortcuts window, associate the item with the action
--  win.show-help-overlay.
--
--  ## A simple application ## {gtkapplication}
--
--  [A simple
--  example](https://git.gnome.org/browse/gtk+/tree/examples/bp/bloatpad.c)
--
--  GtkApplication optionally registers with a session manager of the users
--  session (if you set the Gtk.Application.Gtk_Application:register-session
--  property) and offers various functionality related to the session
--  life-cycle.
--
--  An application can block various ways to end the session with the
--  Gtk.Application.Inhibit function. Typical use cases for this kind of
--  inhibiting are long-running, uninterruptible operations, such as burning a
--  CD or performing a disk backup. The session manager may not honor the
--  inhibitor, but it can be expected to inform the user about the negative
--  consequences of ending the session while inhibitors are present.
--
--  ## See Also ## {seealso} [HowDoI: Using
--  GtkApplication](https://wiki.gnome.org/HowDoI/GtkApplication), [Getting
--  Started with GTK+:
--  Basics](https://developer.gnome.org/gtk3/stable/gtk-getting-started.htmlid-1.2.3.3)
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Glib;                    use Glib;
with Glib.Action;             use Glib.Action;
with Glib.Action_Group;       use Glib.Action_Group;
with Glib.Action_Map;         use Glib.Action_Map;
with Glib.Application;        use Glib.Application;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Menu;               use Glib.Menu;
with Glib.Menu_Model;         use Glib.Menu_Model;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Glib.Variant;            use Glib.Variant;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;

package Gtk.Application is

   type Gtk_Application_Record is new Gapplication_Record with null record;
   type Gtk_Application is access all Gtk_Application_Record'Class;

   type Gtk_Application_Inhibit_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Application_Inhibit_Flags);
   --  Types of user actions that may be blocked by Gtk.Application.Inhibit.

   Application_Inhibit_Logout : constant Gtk_Application_Inhibit_Flags := 1;
   Application_Inhibit_Switch : constant Gtk_Application_Inhibit_Flags := 2;
   Application_Inhibit_Suspend : constant Gtk_Application_Inhibit_Flags := 4;
   Application_Inhibit_Idle : constant Gtk_Application_Inhibit_Flags := 8;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Application_Inhibit_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Application_Inhibit_Flags);
   type Property_Gtk_Application_Inhibit_Flags is new Gtk_Application_Inhibit_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self           : out Gtk_Application;
       Application_Id : UTF8_String := "";
       Flags          : Glib.Application.GApplication_Flags);
   procedure Initialize
      (Self           : not null access Gtk_Application_Record'Class;
       Application_Id : UTF8_String := "";
       Flags          : Glib.Application.GApplication_Flags);
   --  Creates a new Gtk.Application.Gtk_Application instance.
   --  When using Gtk.Application.Gtk_Application, it is not necessary to call
   --  gtk_init manually. It is called as soon as the application gets
   --  registered as the primary instance.
   --  Concretely, gtk_init is called in the default handler for the
   --  Glib.Application.Gapplication::startup signal. Therefore,
   --  Gtk.Application.Gtk_Application subclasses should chain up in their
   --  Glib.Application.Gapplication::startup handler before using any GTK+
   --  API.
   --  Note that commandline arguments are not passed to gtk_init. All GTK+
   --  functionality that is available via commandline arguments can also be
   --  achieved by setting suitable environment variables such as `G_DEBUG`, so
   --  this should not be a big problem. If you absolutely must support GTK+
   --  commandline arguments, you can explicitly call gtk_init before creating
   --  the application instance.
   --  If non-null, the application ID must be valid. See
   --  Glib.Application.Id_Is_Valid.
   --  If no application ID is given then some features (most notably
   --  application uniqueness) will be disabled. A null application ID is only
   --  allowed with GTK+ 3.6 or later.
   --  Since: gtk+ 3.0
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "application_id": The application ID.
   --  "flags": the application flags

   function Gtk_Application_New
      (Application_Id : UTF8_String := "";
       Flags          : Glib.Application.GApplication_Flags)
       return Gtk_Application;
   --  Creates a new Gtk.Application.Gtk_Application instance.
   --  When using Gtk.Application.Gtk_Application, it is not necessary to call
   --  gtk_init manually. It is called as soon as the application gets
   --  registered as the primary instance.
   --  Concretely, gtk_init is called in the default handler for the
   --  Glib.Application.Gapplication::startup signal. Therefore,
   --  Gtk.Application.Gtk_Application subclasses should chain up in their
   --  Glib.Application.Gapplication::startup handler before using any GTK+
   --  API.
   --  Note that commandline arguments are not passed to gtk_init. All GTK+
   --  functionality that is available via commandline arguments can also be
   --  achieved by setting suitable environment variables such as `G_DEBUG`, so
   --  this should not be a big problem. If you absolutely must support GTK+
   --  commandline arguments, you can explicitly call gtk_init before creating
   --  the application instance.
   --  If non-null, the application ID must be valid. See
   --  Glib.Application.Id_Is_Valid.
   --  If no application ID is given then some features (most notably
   --  application uniqueness) will be disabled. A null application ID is only
   --  allowed with GTK+ 3.6 or later.
   --  Since: gtk+ 3.0
   --  "application_id": The application ID.
   --  "flags": the application flags

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_application_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Accelerator
      (Self        : not null access Gtk_Application_Record;
       Accelerator : UTF8_String;
       Action_Name : UTF8_String;
       Parameter   : Glib.Variant.Gvariant);
   pragma Obsolescent (Add_Accelerator);
   --  Installs an accelerator that will cause the named action to be
   --  activated when the key combination specificed by Accelerator is pressed.
   --  Accelerator must be a string that can be parsed by
   --  Gtk.Accel_Group.Accelerator_Parse, e.g. "<Primary>q" or
   --  "<Control><Alt>p".
   --  Action_Name must be the name of an action as it would be used in the
   --  app menu, i.e. actions that have been added to the application are
   --  referred to with an "app." prefix, and window-specific actions with a
   --  "win." prefix.
   --  GtkApplication also extracts accelerators out of "accel" attributes in
   --  the GMenu_Models passed to Gtk.Application.Set_App_Menu and
   --  Gtk.Application.Set_Menubar, which is usually more convenient than
   --  calling this function for each accelerator.
   --  Since: gtk+ 3.4
   --  Deprecated since 3.14, 1
   --  "accelerator": accelerator string
   --  "action_name": the name of the action to activate
   --  "parameter": parameter to pass when activating the action, or null if
   --  the action does not accept an activation parameter

   procedure Add_Window
      (Self   : not null access Gtk_Application_Record;
       Window : not null access Gtk.Window.Gtk_Window_Record'Class);
   --  Adds a window to Application.
   --  This call can only happen after the Application has started; typically,
   --  you should add new application windows in response to the emission of
   --  the Glib.Application.Gapplication::activate signal.
   --  This call is equivalent to setting the
   --  Gtk.Window.Gtk_Window:application property of Window to Application.
   --  Normally, the connection between the application and the window will
   --  remain until the window is destroyed, but you can explicitly remove it
   --  with Gtk.Application.Remove_Window.
   --  GTK+ will keep the Application running as long as it has any windows.
   --  Since: gtk+ 3.0
   --  "window": a Gtk.Window.Gtk_Window

   function Get_Accels_For_Action
      (Self                 : not null access Gtk_Application_Record;
       Detailed_Action_Name : UTF8_String) return GNAT.Strings.String_List;
   --  Gets the accelerators that are currently associated with the given
   --  action.
   --  Since: gtk+ 3.12
   --  "detailed_action_name": a detailed action name, specifying an action
   --  and target to obtain accelerators for

   procedure Set_Accels_For_Action
      (Self                 : not null access Gtk_Application_Record;
       Detailed_Action_Name : UTF8_String;
       Accels               : GNAT.Strings.String_List);
   --  Sets zero or more keyboard accelerators that will trigger the given
   --  action. The first item in Accels will be the primary accelerator, which
   --  may be displayed in the UI.
   --  To remove all accelerators for an action, use an empty, zero-terminated
   --  array for Accels.
   --  For the Detailed_Action_Name, see g_action_parse_detailed_name and
   --  Glib.Action.Print_Detailed_Name.
   --  Since: gtk+ 3.12
   --  "detailed_action_name": a detailed action name, specifying an action
   --  and target to associate accelerators with
   --  "accels": a list of accelerators in the format understood by
   --  Gtk.Accel_Group.Accelerator_Parse

   function Get_Actions_For_Accel
      (Self  : not null access Gtk_Application_Record;
       Accel : UTF8_String) return GNAT.Strings.String_List;
   --  Returns the list of actions (possibly empty) that Accel maps to. Each
   --  item in the list is a detailed action name in the usual form.
   --  This might be useful to discover if an accel already exists in order to
   --  prevent installation of a conflicting accelerator (from an accelerator
   --  editor or a plugin system, for example). Note that having more than one
   --  action per accelerator may not be a bad thing and might make sense in
   --  cases where the actions never appear in the same context.
   --  In case there are no actions for a given accelerator, an empty array is
   --  returned. null is never returned.
   --  It is a programmer error to pass an invalid accelerator string. If you
   --  are unsure, check it with Gtk.Accel_Group.Accelerator_Parse first.
   --  Since: gtk+ 3.14
   --  "accel": an accelerator that can be parsed by
   --  Gtk.Accel_Group.Accelerator_Parse

   function Get_Active_Window
      (Self : not null access Gtk_Application_Record)
       return Gtk.Window.Gtk_Window;
   --  Gets the "active" window for the application.
   --  The active window is the one that was most recently focused (within the
   --  application). This window may not have the focus at the moment if
   --  another application has it — this is just the most recently-focused
   --  window within this application.
   --  Since: gtk+ 3.6

   function Get_App_Menu
      (Self : not null access Gtk_Application_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Returns the menu model that has been set with
   --  Gtk.Application.Set_App_Menu.
   --  Since: gtk+ 3.4

   procedure Set_App_Menu
      (Self     : not null access Gtk_Application_Record;
       App_Menu : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets or unsets the application menu for Application.
   --  This can only be done in the primary instance of the application, after
   --  it has been registered. Glib.Application.Gapplication::startup is a good
   --  place to call this.
   --  The application menu is a single menu containing items that typically
   --  impact the application as a whole, rather than acting on a specific
   --  window or document. For example, you would expect to see "Preferences"
   --  or "Quit" in an application menu, but not "Save" or "Print".
   --  If supported, the application menu will be rendered by the desktop
   --  environment.
   --  Use the base Glib.Action_Map.Gaction_Map interface to add actions, to
   --  respond to the user selecting these menu items.
   --  Since: gtk+ 3.4
   --  "app_menu": a Glib.Menu_Model.Gmenu_Model, or null

   function Get_Menu_By_Id
      (Self : not null access Gtk_Application_Record;
       Id   : UTF8_String) return Glib.Menu.Gmenu;
   --  Gets a menu from automatically loaded resources. See [Automatic
   --  resources][automatic-resources] for more information.
   --  Since: gtk+ 3.14
   --  "id": the id of the menu to look up

   function Get_Menubar
      (Self : not null access Gtk_Application_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Returns the menu model that has been set with
   --  Gtk.Application.Set_Menubar.
   --  Since: gtk+ 3.4

   procedure Set_Menubar
      (Self    : not null access Gtk_Application_Record;
       Menubar : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets or unsets the menubar for windows of Application.
   --  This is a menubar in the traditional sense.
   --  This can only be done in the primary instance of the application, after
   --  it has been registered. Glib.Application.Gapplication::startup is a good
   --  place to call this.
   --  Depending on the desktop environment, this may appear at the top of
   --  each window, or at the top of the screen. In some environments, if both
   --  the application menu and the menubar are set, the application menu will
   --  be presented as if it were the first item of the menubar. Other
   --  environments treat the two as completely separate — for example, the
   --  application menu may be rendered by the desktop shell while the menubar
   --  (if set) remains in each individual window.
   --  Use the base Glib.Action_Map.Gaction_Map interface to add actions, to
   --  respond to the user selecting these menu items.
   --  Since: gtk+ 3.4
   --  "menubar": a Glib.Menu_Model.Gmenu_Model, or null

   function Get_Window_By_Id
      (Self : not null access Gtk_Application_Record;
       Id   : Guint) return Gtk.Window.Gtk_Window;
   --  Returns the Gtk.Application_Window.Gtk_Application_Window with the
   --  given ID.
   --  The ID of a Gtk.Application_Window.Gtk_Application_Window can be
   --  retrieved with Gtk.Application_Window.Get_Id.
   --  Since: gtk+ 3.6
   --  "id": an identifier number

   function Get_Windows
      (Self : not null access Gtk_Application_Record)
       return Gtk.Widget.Widget_List.Glist;
   --  Gets a list of the Gtk_Windows associated with Application.
   --  The list is sorted by most recently focused window, such that the first
   --  element is the currently focused window. (Useful for choosing a parent
   --  for a transient window.)
   --  The list that is returned should not be modified in any way. It will
   --  only remain valid until the next focus change or window creation or
   --  deletion.
   --  Since: gtk+ 3.0

   function Inhibit
      (Self   : not null access Gtk_Application_Record;
       Window : access Gtk.Window.Gtk_Window_Record'Class;
       Flags  : Gtk_Application_Inhibit_Flags;
       Reason : UTF8_String := "") return Guint;
   --  Inform the session manager that certain types of actions should be
   --  inhibited. This is not guaranteed to work on all platforms and for all
   --  types of actions.
   --  Applications should invoke this method when they begin an operation
   --  that should not be interrupted, such as creating a CD or DVD. The types
   --  of actions that may be blocked are specified by the Flags parameter.
   --  When the application completes the operation it should call
   --  Gtk.Application.Uninhibit to remove the inhibitor. Note that an
   --  application can have multiple inhibitors, and all of them must be
   --  individually removed. Inhibitors are also cleared when the application
   --  exits.
   --  Applications should not expect that they will always be able to block
   --  the action. In most cases, users will be given the option to force the
   --  action to take place.
   --  Reasons should be short and to the point.
   --  If Window is given, the session manager may point the user to this
   --  window to find out more about why the action is inhibited.
   --  Since: gtk+ 3.4
   --  "window": a Gtk.Window.Gtk_Window, or null
   --  "flags": what types of actions should be inhibited
   --  "reason": a short, human-readable string that explains why these
   --  operations are inhibited

   function Is_Inhibited
      (Self  : not null access Gtk_Application_Record;
       Flags : Gtk_Application_Inhibit_Flags) return Boolean;
   --  Determines if any of the actions specified in Flags are currently
   --  inhibited (possibly by another application).
   --  Note that this information may not be available (for example when the
   --  application is running in a sandbox).
   --  Since: gtk+ 3.4
   --  "flags": what types of actions should be queried

   function List_Action_Descriptions
      (Self : not null access Gtk_Application_Record)
       return GNAT.Strings.String_List;
   --  Lists the detailed action names which have associated accelerators. See
   --  Gtk.Application.Set_Accels_For_Action.
   --  Since: gtk+ 3.12

   function Prefers_App_Menu
      (Self : not null access Gtk_Application_Record) return Boolean;
   --  Determines if the desktop environment in which the application is
   --  running would prefer an application menu be shown.
   --  If this function returns True then the application should call
   --  Gtk.Application.Set_App_Menu with the contents of an application menu,
   --  which will be shown by the desktop environment. If it returns False then
   --  you should consider using an alternate approach, such as a menubar.
   --  The value returned by this function is purely advisory and you are free
   --  to ignore it. If you call Gtk.Application.Set_App_Menu even if the
   --  desktop environment doesn't support app menus, then a fallback will be
   --  provided.
   --  Applications are similarly free not to set an app menu even if the
   --  desktop environment wants to show one. In that case, a fallback will
   --  also be created by the desktop environment (GNOME, for example, uses a
   --  menu with only a "Quit" item in it).
   --  The value returned by this function never changes. Once it returns a
   --  particular value, it is guaranteed to always return the same value.
   --  You may only call this function after the application has been
   --  registered and after the base startup handler has run. You're most
   --  likely to want to use this from your own startup handler. It may also
   --  make sense to consult this function while constructing UI (in activate,
   --  open or an action activation handler) in order to determine if you
   --  should show a gear menu or not.
   --  This function will return False on Mac OS and a default app menu will
   --  be created automatically with the "usual" contents of that menu typical
   --  to most Mac OS applications. If you call Gtk.Application.Set_App_Menu
   --  anyway, then this menu will be replaced with your own.
   --  Since: gtk+ 3.14

   procedure Remove_Accelerator
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String;
       Parameter   : Glib.Variant.Gvariant);
   pragma Obsolescent (Remove_Accelerator);
   --  Removes an accelerator that has been previously added with
   --  Gtk.Application.Add_Accelerator.
   --  Since: gtk+ 3.4
   --  Deprecated since 3.14, 1
   --  "action_name": the name of the action to activate
   --  "parameter": parameter to pass when activating the action, or null if
   --  the action does not accept an activation parameter

   procedure Remove_Window
      (Self   : not null access Gtk_Application_Record;
       Window : not null access Gtk.Window.Gtk_Window_Record'Class);
   --  Remove a window from Application.
   --  If Window belongs to Application then this call is equivalent to
   --  setting the Gtk.Window.Gtk_Window:application property of Window to
   --  null.
   --  The application may stop running as a result of a call to this
   --  function.
   --  Since: gtk+ 3.0
   --  "window": a Gtk.Window.Gtk_Window

   procedure Uninhibit
      (Self   : not null access Gtk_Application_Record;
       Cookie : Guint);
   --  Removes an inhibitor that has been established with
   --  Gtk.Application.Inhibit. Inhibitors are also cleared when the
   --  application exits.
   --  Since: gtk+ 3.4
   --  "cookie": a cookie that was returned by Gtk.Application.Inhibit

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Action_Added
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String);

   procedure Action_Enabled_Changed
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String;
       Enabled     : Boolean);

   procedure Action_Removed
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String);

   procedure Action_State_Changed
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String;
       State       : Glib.Variant.Gvariant);

   procedure Activate_Action
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String;
       Parameter   : Glib.Variant.Gvariant);

   procedure Change_Action_State
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String;
       Value       : Glib.Variant.Gvariant);

   function Get_Action_Enabled
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String) return Boolean;

   function Get_Action_Parameter_Type
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type;

   function Get_Action_State
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant;

   function Get_Action_State_Hint
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant;

   function Get_Action_State_Type
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type;

   function Has_Action
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String) return Boolean;

   function List_Actions
      (Self : not null access Gtk_Application_Record)
       return GNAT.Strings.String_List;

   function Query_Action
      (Self           : not null access Gtk_Application_Record;
       Action_Name    : UTF8_String;
       Enabled        : access Boolean;
       Parameter_Type : access Glib.Variant.Gvariant_Type;
       State_Type     : access Glib.Variant.Gvariant_Type;
       State_Hint     : access Glib.Variant.Gvariant;
       State          : access Glib.Variant.Gvariant) return Boolean;

   procedure Add_Action
      (Self   : not null access Gtk_Application_Record;
       Action : Glib.Action.Gaction);

   procedure Add_Action_Entries
      (Self      : not null access Gtk_Application_Record;
       Entries   : GAction_Entry_Array;
       User_Data : System.Address := System.Null_Address);

   function Lookup_Action
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String) return Glib.Action.Gaction;

   procedure Remove_Action
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Window_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Window.Gtk_Window

   App_Menu_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model

   Menubar_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model

   Register_Session_Property : constant Glib.Properties.Property_Boolean;
   --  Set this property to True to register with the session manager.

   Screensaver_Active_Property : constant Glib.Properties.Property_Boolean;
   --  This property is True if GTK+ believes that the screensaver is
   --  currently active. GTK+ only tracks session state (including this) when
   --  Gtk.Application.Gtk_Application::register-session is set to True.
   --
   --  Tracking the screensaver state is supported on Linux.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Application_Void is not null access procedure
     (Self : access Gtk_Application_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Query_End : constant Glib.Signal_Name := "query-end";
   procedure On_Query_End
      (Self  : not null access Gtk_Application_Record;
       Call  : Cb_Gtk_Application_Void;
       After : Boolean := False);
   procedure On_Query_End
      (Self  : not null access Gtk_Application_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the session manager is about to end the session, only if
   --  Gtk.Application.Gtk_Application::register-session is True. Applications
   --  can connect to this signal and call Gtk.Application.Inhibit with
   --  Gtk.Application.Application_Inhibit_Logout to delay the end of the
   --  session until state has been saved.

   type Cb_Gtk_Application_Gtk_Window_Void is not null access procedure
     (Self   : access Gtk_Application_Record'Class;
      Window : not null access Gtk.Window.Gtk_Window_Record'Class);

   type Cb_GObject_Gtk_Window_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Window : not null access Gtk.Window.Gtk_Window_Record'Class);

   Signal_Window_Added : constant Glib.Signal_Name := "window-added";
   procedure On_Window_Added
      (Self  : not null access Gtk_Application_Record;
       Call  : Cb_Gtk_Application_Gtk_Window_Void;
       After : Boolean := False);
   procedure On_Window_Added
      (Self  : not null access Gtk_Application_Record;
       Call  : Cb_GObject_Gtk_Window_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a Gtk.Window.Gtk_Window is added to Application through
   --  Gtk.Application.Add_Window.

   Signal_Window_Removed : constant Glib.Signal_Name := "window-removed";
   procedure On_Window_Removed
      (Self  : not null access Gtk_Application_Record;
       Call  : Cb_Gtk_Application_Gtk_Window_Void;
       After : Boolean := False);
   procedure On_Window_Removed
      (Self  : not null access Gtk_Application_Record;
       Call  : Cb_GObject_Gtk_Window_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a Gtk.Window.Gtk_Window is removed from Application,
   --  either as a side-effect of being destroyed or explicitly through
   --  Gtk.Application.Remove_Window.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ActionGroup"
   --
   --  - "Gio.ActionMap"

   package Implements_Gaction_Group is new Glib.Types.Implements
     (Glib.Action_Group.Gaction_Group, Gtk_Application_Record, Gtk_Application);
   function "+"
     (Widget : access Gtk_Application_Record'Class)
   return Glib.Action_Group.Gaction_Group
   renames Implements_Gaction_Group.To_Interface;
   function "-"
     (Interf : Glib.Action_Group.Gaction_Group)
   return Gtk_Application
   renames Implements_Gaction_Group.To_Object;

   package Implements_Gaction_Map is new Glib.Types.Implements
     (Glib.Action_Map.Gaction_Map, Gtk_Application_Record, Gtk_Application);
   function "+"
     (Widget : access Gtk_Application_Record'Class)
   return Glib.Action_Map.Gaction_Map
   renames Implements_Gaction_Map.To_Interface;
   function "-"
     (Interf : Glib.Action_Map.Gaction_Map)
   return Gtk_Application
   renames Implements_Gaction_Map.To_Object;

private
   Screensaver_Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("screensaver-active");
   Register_Session_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("register-session");
   Menubar_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("menubar");
   App_Menu_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("app-menu");
   Active_Window_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("active-window");
end Gtk.Application;
