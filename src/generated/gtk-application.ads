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

--  A high-level API for writing applications.
--
--  `GtkApplication` supports many aspects of writing a GTK application in a
--  convenient fashion, without enforcing a one-size-fits-all model.
--
--  Currently, it handles GTK initialization, application uniqueness, session
--  management, provides some basic scriptability and desktop shell integration
--  by exporting actions and menus and manages a list of toplevel windows whose
--  life-cycle is automatically tied to the life-cycle of your application.
--
--  While `GtkApplication` works fine with plain [classGtk.Window]s, it is
--  recommended to use it together with [classGtk.ApplicationWindow].
--
--  ## Initialization
--
--  A typical `GtkApplication` will create a window in its
--  [signalGio.Application::activate], [signalGio.Application::open] or
--  [signalGio.Application::command-line] handlers. Note that all of these
--  signals may be emitted multiple times, so handlers must be careful to take
--  existing windows into account.
--
--  A typical ::activate handler should look like this:
--
--  ``` static void activate (GApplication *gapp) { GtkApplication *app =
--  GTK_APPLICATION (gapp); GtkWindow *window;
--
--  window = gtk_application_get_active_window (app); if (!window) window =
--  create_window (app);
--
--  gtk_window_present (window); } ```
--
--  ## Automatic resources
--
--  `GtkApplication` will automatically load menus from the `GtkBuilder`
--  resource located at "gtk/menus.ui", relative to the application's resource
--  base path (see [methodGio.Application.set_resource_base_path]). The menu
--  with the ID "menubar" is taken as the application's menubar. Additional
--  menus (most interesting submenus) can be named and accessed via
--  [methodGtk.Application.get_menu_by_id] which allows for dynamic population
--  of a part of the menu structure.
--
--  Note that automatic resource loading uses the resource base path that is
--  set at construction time and will not work if the resource base path is
--  changed at a later time.
--
--  It is also possible to provide the menubar manually using
--  [methodGtk.Application.set_menubar].
--
--  `GtkApplication` will also automatically setup an icon search path for the
--  default icon theme by appending "icons" to the resource base path. This
--  allows your application to easily store its icons as resources. See
--  [methodGtk.IconTheme.add_resource_path] for more information.
--
--  If there is a resource located at `gtk/help-overlay.ui` which defines a
--  [classGtk.ShortcutsWindow] with ID `help_overlay` then `GtkApplication`
--  associates an instance of this shortcuts window with each
--  [classGtk.ApplicationWindow] and sets up the keyboard accelerator
--  <kbd>Control</kbd>+<kbd>?</kbd> to open it. To create a menu item that
--  displays the shortcuts window, associate the item with the action
--  `win.show-help-overlay`.
--
--  `GtkApplication` will also automatically set the application id as the
--  default window icon. Use [funcGtk.Window.set_default_icon_name] or
--  [propertyGtk.Window:icon-name] to override that behavior.
--
--  # Inhibiting
--
--  An application can block various ways to end the session with the
--  [methodGtk.Application.inhibit] function. Typical use cases for this kind
--  of inhibiting are long-running, uninterruptible operations, such as burning
--  a CD or performing a disk backup. The session manager may not honor the
--  inhibitor, but it can be expected to inform the user about the negative
--  consequences of ending the session while inhibitors are present.
--
--  ## A simple application
--
--  [A simple
--  example](https://gitlab.gnome.org/GNOME/gtk/tree/main/examples/bp/bloatpad.c)
--  is available in the GTK source code repository
--
--  ## See Also
--
--  - [Using
--  GtkApplication](https://developer.gnome.org/documentation/tutorials/application.html)
--  - [Getting Started with GTK: Basics](getting_started.htmlbasics)

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

   pragma Elaborate_Body;

   type Gtk_Application_Record is new Gapplication_Record with null record;
   type Gtk_Application is access all Gtk_Application_Record'Class;

   type Gtk_Application_Inhibit_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Application_Inhibit_Flags);
   --  Types of user actions that may be blocked by `GtkApplication`.
   --
   --  See [methodGtk.Application.inhibit].

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
   --  Creates a new application instance.
   --  When using `GtkApplication`, it is not necessary to call [funcGtk.init]
   --  manually. It is called as soon as the application gets registered as the
   --  primary instance.
   --  Concretely, [funcGtk.init] is called in the default handler for the
   --  `GApplication::startup` signal. Therefore, `GtkApplication` subclasses
   --  should always chain up in their [vfuncGio.Application.startup] handler
   --  before using any GTK API.
   --  Note that commandline arguments are not passed to [funcGtk.init].
   --  If `application_id` is not `NULL`, then it must be valid. See
   --  [funcGio.Application.id_is_valid].
   --  If no application ID is given then some features (most notably
   --  application uniqueness) will be disabled.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Application_Id The application ID
   --  @param Flags the application flags

   function Gtk_Application_New
      (Application_Id : UTF8_String := "";
       Flags          : Glib.Application.GApplication_Flags)
       return Gtk_Application;
   --  Creates a new application instance.
   --  When using `GtkApplication`, it is not necessary to call [funcGtk.init]
   --  manually. It is called as soon as the application gets registered as the
   --  primary instance.
   --  Concretely, [funcGtk.init] is called in the default handler for the
   --  `GApplication::startup` signal. Therefore, `GtkApplication` subclasses
   --  should always chain up in their [vfuncGio.Application.startup] handler
   --  before using any GTK API.
   --  Note that commandline arguments are not passed to [funcGtk.init].
   --  If `application_id` is not `NULL`, then it must be valid. See
   --  [funcGio.Application.id_is_valid].
   --  If no application ID is given then some features (most notably
   --  application uniqueness) will be disabled.
   --  @param Application_Id The application ID
   --  @param Flags the application flags

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_application_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Window
      (Self   : not null access Gtk_Application_Record;
       Window : not null access Gtk.Window.Gtk_Window_Record'Class);
   --  Adds a window to the application.
   --  This call can only happen after the application has started; typically,
   --  you should add new application windows in response to the emission of
   --  the [signalGio.Application::activate] signal.
   --  This call is equivalent to setting the [propertyGtk.Window:application]
   --  property of the window to Application.
   --  Normally, the connection between the application and the window will
   --  remain until the window is destroyed, but you can explicitly remove it
   --  with [methodGtk.Application.remove_window].
   --  GTK will keep the application running as long as it has any windows.
   --  @param Window a window

   function Get_Accels_For_Action
      (Self                 : not null access Gtk_Application_Record;
       Detailed_Action_Name : UTF8_String) return GNAT.Strings.String_List;
   --  Gets the accelerators that are currently associated with the given
   --  action.
   --  @param Detailed_Action_Name a detailed action name, specifying an
   --  action and target to obtain accelerators for
   --  @return accelerators for Detailed_Action_Name

   procedure Set_Accels_For_Action
      (Self                 : not null access Gtk_Application_Record;
       Detailed_Action_Name : UTF8_String;
       Accels               : GNAT.Strings.String_List);
   --  Sets zero or more keyboard accelerators that will trigger the given
   --  action.
   --  The first item in Accels will be the primary accelerator, which may be
   --  displayed in the UI.
   --  To remove all accelerators for an action, use an empty, zero-terminated
   --  array for Accels.
   --  For the Detailed_Action_Name, see [funcGio.Action.parse_detailed_name]
   --  and [Gio.Action.print_detailed_name].
   --  @param Detailed_Action_Name a detailed action name, specifying an
   --  action and target to associate accelerators with
   --  @param Accels a list of accelerators in the format understood by
   --  [funcGtk.accelerator_parse]

   function Get_Actions_For_Accel
      (Self  : not null access Gtk_Application_Record;
       Accel : UTF8_String) return GNAT.Strings.String_List;
   --  Returns the list of actions (possibly empty) that the accelerator maps
   --  to.
   --  Each item in the list is a detailed action name in the usual form.
   --  This might be useful to discover if an accel already exists in order to
   --  prevent installation of a conflicting accelerator (from an accelerator
   --  editor or a plugin system, for example). Note that having more than one
   --  action per accelerator may not be a bad thing and might make sense in
   --  cases where the actions never appear in the same context.
   --  In case there are no actions for a given accelerator, an empty array is
   --  returned. `NULL` is never returned.
   --  It is a programmer error to pass an invalid accelerator string.
   --  If you are unsure, check it with [funcGtk.accelerator_parse] first.
   --  @param Accel an accelerator that can be parsed by
   --  [funcGtk.accelerator_parse]
   --  @return actions for Accel

   function Get_Active_Window
      (Self : not null access Gtk_Application_Record)
       return Gtk.Window.Gtk_Window;
   --  Gets the "active" window for the application.
   --  The active window is the one that was most recently focused (within the
   --  application). This window may not have the focus at the moment if
   --  another application has it — this is just the most recently-focused
   --  window within this application.
   --  @return the active window

   function Get_Menu_By_Id
      (Self : not null access Gtk_Application_Record;
       Id   : UTF8_String) return Glib.Menu.Gmenu;
   --  Gets a menu from automatically loaded resources.
   --  See [the section on Automatic
   --  resources](class.Application.htmlautomatic-resources) for more
   --  information.
   --  @param Id the ID of the menu to look up
   --  @return Gets the menu with the given ID from the automatically loaded
   --  resources

   function Get_Menubar
      (Self : not null access Gtk_Application_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Returns the menu model for the menu bar of the application.
   --  @return the menubar for windows of the application

   procedure Set_Menubar
      (Self    : not null access Gtk_Application_Record;
       Menubar : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets or unsets the menubar for windows of the application.
   --  This is a menubar in the traditional sense.
   --  This can only be done in the primary instance of the application, after
   --  it has been registered. [vfuncGio.Application.startup] is a good place
   --  to call this.
   --  Depending on the desktop environment, this may appear at the top of
   --  each window, or at the top of the screen. In some environments, if both
   --  the application menu and the menubar are set, the application menu will
   --  be presented as if it were the first item of the menubar. Other
   --  environments treat the two as completely separate — for example, the
   --  application menu may be rendered by the desktop shell while the menubar
   --  (if set) remains in each individual window.
   --  Use the base `GActionMap` interface to add actions, to respond to the
   --  user selecting these menu items.
   --  @param Menubar a menu model

   function Get_Window_By_Id
      (Self : not null access Gtk_Application_Record;
       Id   : Guint) return Gtk.Window.Gtk_Window;
   --  Returns the window with the given ID.
   --  The ID of a `GtkApplicationWindow` can be retrieved with
   --  [methodGtk.ApplicationWindow.get_id].
   --  @param Id an identifier number
   --  @return the window for the given ID

   function Get_Windows
      (Self : not null access Gtk_Application_Record)
       return Gtk.Widget.Widget_List.Glist;
   --  Gets a list of the window associated with the application.
   --  The list is sorted by most recently focused window, such that the first
   --  element is the currently focused window. (Useful for choosing a parent
   --  for a transient window.)
   --  The list that is returned should not be modified in any way. It will
   --  only remain valid until the next focus change or window creation or
   --  deletion.

   function Inhibit
      (Self   : not null access Gtk_Application_Record;
       Window : access Gtk.Window.Gtk_Window_Record'Class;
       Flags  : Gtk_Application_Inhibit_Flags;
       Reason : UTF8_String := "") return Guint;
   --  Informs the session manager that certain types of actions should be
   --  inhibited.
   --  This is not guaranteed to work on all platforms and for all types of
   --  actions.
   --  Applications should invoke this method when they begin an operation
   --  that should not be interrupted, such as creating a CD or DVD. The types
   --  of actions that may be blocked are specified by the Flags parameter.
   --  When the application completes the operation it should call
   --  [methodGtk.Application.uninhibit] to remove the inhibitor. Note that an
   --  application can have multiple inhibitors, and all of them must be
   --  individually removed. Inhibitors are also cleared when the application
   --  exits.
   --  Applications should not expect that they will always be able to block
   --  the action. In most cases, users will be given the option to force the
   --  action to take place.
   --  The Reason message should be short and to the point.
   --  If a window is given, the session manager may point the user to this
   --  window to find out more about why the action is inhibited.
   --  The cookie that is returned by this function should be used as an
   --  argument to [methodGtk.Application.uninhibit] in order to remove the
   --  request.
   --  @param Window a window
   --  @param Flags what types of actions should be inhibited
   --  @param Reason a short, human-readable string that explains why these
   --  operations are inhibited
   --  @return A non-zero cookie that is used to uniquely identify this, or 0
   --  if the platform does not support inhibiting or the request failed for
   --  some reason

   function List_Action_Descriptions
      (Self : not null access Gtk_Application_Record)
       return GNAT.Strings.String_List;
   --  Lists the detailed action names which have associated accelerators.
   --  See [methodGtk.Application.set_accels_for_action].
   --  @return the detailed action names

   procedure Remove_Window
      (Self   : not null access Gtk_Application_Record;
       Window : not null access Gtk.Window.Gtk_Window_Record'Class);
   --  Remove a window from the application.
   --  If the window belongs to the application then this call is equivalent
   --  to setting the [propertyGtk.Window:application] property of the window
   --  to `NULL`.
   --  The application may stop running as a result of a call to this
   --  function, if the window was the last window of the application.
   --  @param Window a window

   procedure Uninhibit
      (Self   : not null access Gtk_Application_Record;
       Cookie : Guint);
   --  Removes an inhibitor that has been previously established.
   --  See [methodGtk.Application.inhibit].
   --  Inhibitors are also cleared when the application exits.
   --  @param Cookie a cookie that was returned by
   --  [methodGtk.Application.inhibit]

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
   --  The currently focused window of the application.

   Menubar_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model
   --  The menu model to be used for the application's menu bar.

   Register_Session_Property : constant Glib.Properties.Property_Boolean;
   --  Set this property to true to register with the session manager.
   --
   --  This will make GTK track the session state (such as the
   --  [propertyGtk.Application:screensaver-active] property).

   Screensaver_Active_Property : constant Glib.Properties.Property_Boolean;
   --  This property is true if GTK believes that the screensaver is currently
   --  active.
   --
   --  Tracking the screensaver state is currently only supported on Linux.

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
   --  Emitted when the session manager is about to end the session.
   --
   --  Applications can connect to this signal and call
   --  [methodGtk.Application.inhibit] with
   --  [flagsGtk.ApplicationInhibitFlags.logout] to delay the end of the
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
   --  Emitted when a window is added to an application.
   --
   --  See [methodGtk.Application.add_window].

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
   --  Emitted when a window is removed from an application.
   --
   --  This can happen as a side-effect of the window being destroyed or
   --  explicitly through [methodGtk.Application.remove_window].

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
   Active_Window_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("active-window");
end Gtk.Application;
