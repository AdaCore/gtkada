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
--  A Glib.Application.Gapplication is the foundation of an application. It
--  wraps some low-level platform-specific services and is intended to act as
--  the foundation for higher-level application classes such as
--  Gtk.Application.Gtk_Application or Mx_Application. In general, you should
--  not use this class outside of a higher level framework.
--
--  GApplication provides convenient life cycle management by maintaining a
--  "use count" for the primary application instance. The use count can be
--  changed using Glib.Application.Hold and Glib.Application.Release. If it
--  drops to zero, the application exits. Higher-level classes such as
--  Gtk.Application.Gtk_Application employ the use count to ensure that the
--  application stays alive as long as it has any opened windows.
--
--  Another feature that GApplication (optionally) provides is process
--  uniqueness. Applications can make use of this functionality by providing a
--  unique application ID. If given, only one application with this ID can be
--  running at a time per session. The session concept is platform-dependent,
--  but corresponds roughly to a graphical desktop login. When your application
--  is launched again, its arguments are passed through platform communication
--  to the already running program. The already running instance of the program
--  is called the "primary instance"; for non-unique applications this is
--  always the current instance. On Linux, the D-Bus session bus is used for
--  communication.
--
--  The use of Glib.Application.Gapplication differs from some other
--  commonly-used uniqueness libraries (such as libunique) in important ways.
--  The application is not expected to manually register itself and check if it
--  is the primary instance. Instead, the main function of a
--  Glib.Application.Gapplication should do very little more than instantiating
--  the application instance, possibly connecting signal handlers, then calling
--  Glib.Application.Run. All checks for uniqueness are done internally. If the
--  application is the primary instance then the startup signal is emitted and
--  the mainloop runs. If the application is not the primary instance then a
--  signal is sent to the primary instance and Glib.Application.Run promptly
--  returns. See the code examples below.
--
--  If used, the expected form of an application identifier is the same as
--  that of of a [D-Bus well-known bus
--  name](https://dbus.freedesktop.org/doc/dbus-specification.htmlmessage-protocol-names-bus).
--  Examples include: `com.example.MyApp`,
--  `org.example.internal_apps.Calculator`, `org._7_zip.Archiver`. For details
--  on valid application identifiers, see Glib.Application.Id_Is_Valid.
--
--  On Linux, the application identifier is claimed as a well-known bus name
--  on the user's session bus. This means that the uniqueness of your
--  application is scoped to the current session. It also means that your
--  application may provide additional services (through registration of other
--  object paths) at that bus name. The registration of these object paths
--  should be done with the shared GDBus session bus. Note that due to the
--  internal architecture of GDBus, method calls can be dispatched at any time
--  (even if a main loop is not running). For this reason, you must ensure that
--  any object paths that you wish to register are registered before
--  Glib.Application.Gapplication attempts to acquire the bus name of your
--  application (which happens in g_application_register). Unfortunately, this
--  means that you cannot use Glib.Application.Get_Is_Remote to decide if you
--  want to register object paths.
--
--  GApplication also implements the Glib.Action_Group.Gaction_Group and
--  Glib.Action_Map.Gaction_Map interfaces and lets you easily export actions
--  by adding them with Glib.Action_Map.Add_Action. When invoking an action by
--  calling Glib.Action_Group.Activate_Action on the application, it is always
--  invoked in the primary instance. The actions are also exported on the
--  session bus, and GIO provides the Gdbus.Action_Group.Gdbus_Action_Group
--  wrapper to conveniently access them remotely. GIO provides a
--  Gdbus.Menu_Model.Gdbus_Menu_Model wrapper for remote access to exported
--  GMenu_Models.
--
--  There is a number of different entry points into a GApplication:
--
--  - via 'Activate' (i.e. just starting the application)
--
--  - via 'Open' (i.e. opening some files)
--
--  - by handling a command-line
--
--  - via activating an action
--
--  The Glib.Application.Gapplication::startup signal lets you handle the
--  application initialization for all of these in a single place.
--
--  Regardless of which of these entry points is used to start the
--  application, GApplication passes some ‘platform data' from the launching
--  instance to the primary instance, in the form of a Glib.Variant.Gvariant
--  dictionary mapping strings to variants. To use platform data, override the
--  Before_Emit or After_Emit virtual functions in your
--  Glib.Application.Gapplication subclass. When dealing with
--  Glib.Application.Gapplication_Command_Line objects, the platform data is
--  directly available via Glib.Application.Get_Cwd,
--  Glib.Application.Get_Environ and Glib.Application.Get_Platform_Data.
--
--  As the name indicates, the platform data may vary depending on the
--  operating system, but it always includes the current directory (key "cwd"),
--  and optionally the environment (ie the set of environment variables and
--  their values) of the calling process (key "environ"). The environment is
--  only added to the platform data if the
--  Glib.Application.G_Application_Send_Environment flag is set.
--  Glib.Application.Gapplication subclasses can add their own platform data by
--  overriding the Add_Platform_Data virtual function. For instance,
--  Gtk.Application.Gtk_Application adds startup notification data in this way.
--
--  To parse commandline arguments you may handle the
--  Glib.Application.Gapplication::command-line signal or override the
--  local_command_line vfunc, to parse them in either the primary instance or
--  the local instance, respectively.
--
--  For an example of opening files with a GApplication, see
--  [gapplication-example-open.c](https://git.gnome.org/browse/glib/tree/gio/tests/gapplication-example-open.c).
--
--  For an example of using actions with GApplication, see
--  [gapplication-example-actions.c](https://git.gnome.org/browse/glib/tree/gio/tests/gapplication-example-actions.c).
--
--  For an example of using extra D-Bus hooks with GApplication, see
--  [gapplication-example-dbushooks.c](https://git.gnome.org/browse/glib/tree/gio/tests/gapplication-example-dbushooks.c).
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Glib.Action;             use Glib.Action;
with Glib.Action_Group;       use Glib.Action_Group;
with Glib.Action_Map;         use Glib.Action_Map;
with Glib.Cancellable;        use Glib.Cancellable;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Notification;       use Glib.Notification;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Glib.Variant;            use Glib.Variant;
with Gtkada.Bindings;         use Gtkada.Bindings;

package Glib.Application is

   type Gapplication_Record is new GObject_Record with null record;
   type Gapplication is access all Gapplication_Record'Class;

   type GApplication_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GApplication_Flags);
   --  Flags used to define the behaviour of a Glib.Application.Gapplication.

   G_Application_Flags_None : constant GApplication_Flags := 0;
   G_Application_Is_Service : constant GApplication_Flags := 1;
   G_Application_Is_Launcher : constant GApplication_Flags := 2;
   G_Application_Handles_Open : constant GApplication_Flags := 4;
   G_Application_Handles_Command_Line : constant GApplication_Flags := 8;
   G_Application_Send_Environment : constant GApplication_Flags := 16;
   G_Application_Non_Unique : constant GApplication_Flags := 32;
   G_Application_Can_Override_App_Id : constant GApplication_Flags := 64;
   G_Application_Allow_Replacement : constant GApplication_Flags := 128;
   G_Application_Replace : constant GApplication_Flags := 256;

   type Gapplication_Command_Line_Record is new GObject_Record with null record;
   type Gapplication_Command_Line is access all Gapplication_Command_Line_Record'Class;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GApplication_Flags_Properties is
      new Generic_Internal_Discrete_Property (GApplication_Flags);
   type Property_GApplication_Flags is new GApplication_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New
      (Self           : out Gapplication;
       Application_Id : UTF8_String := "";
       Flags          : GApplication_Flags);
   --  Creates a new Glib.Application.Gapplication instance.
   --  If non-null, the application id must be valid. See
   --  Glib.Application.Id_Is_Valid.
   --  If no application ID is given then some features of
   --  Glib.Application.Gapplication (most notably application uniqueness) will
   --  be disabled.
   --  "application_id": the application id
   --  "flags": the application flags

   procedure Initialize
      (Self           : not null access Gapplication_Record'Class;
       Application_Id : UTF8_String := "";
       Flags          : GApplication_Flags);
   --  Creates a new Glib.Application.Gapplication instance.
   --  If non-null, the application id must be valid. See
   --  Glib.Application.Id_Is_Valid.
   --  If no application ID is given then some features of
   --  Glib.Application.Gapplication (most notably application uniqueness) will
   --  be disabled.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "application_id": the application id
   --  "flags": the application flags

   function Gapplication_New
      (Application_Id : UTF8_String := "";
       Flags          : GApplication_Flags) return Gapplication;
   --  Creates a new Glib.Application.Gapplication instance.
   --  If non-null, the application id must be valid. See
   --  Glib.Application.Id_Is_Valid.
   --  If no application ID is given then some features of
   --  Glib.Application.Gapplication (most notably application uniqueness) will
   --  be disabled.
   --  "application_id": the application id
   --  "flags": the application flags

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_application_get_type");

   function Get_Type_Command_Line return Glib.GType;
   pragma Import (C, Get_Type_Command_Line, "g_application_command_line_get_type");

   -------------
   -- Methods --
   -------------

   procedure Activate (Self : not null access Gapplication_Record);
   --  Activates the application.
   --  In essence, this results in the Glib.Application.Gapplication::activate
   --  signal being emitted in the primary instance.
   --  The application must be registered before calling this function.
   --  Since: gtk+ 2.28

   procedure Bind_Busy_Property
      (Self     : not null access Gapplication_Record;
       Object   : System.Address;
       Property : UTF8_String);
   --  Marks Application as busy (see Glib.Application.Mark_Busy) while
   --  Property on Object is True.
   --  The binding holds a reference to Application while it is active, but
   --  not to Object. Instead, the binding is destroyed when Object is
   --  finalized.
   --  Since: gtk+ 2.44
   --  "object": a Glib.Object.GObject
   --  "property": the name of a boolean property of Object

   function Get_Application_Id
      (Self : not null access Gapplication_Record) return UTF8_String;
   --  Gets the unique identifier for Application.
   --  Since: gtk+ 2.28

   procedure Set_Application_Id
      (Self           : not null access Gapplication_Record;
       Application_Id : UTF8_String := "");
   --  Sets the unique identifier for Application.
   --  The application id can only be modified if Application has not yet been
   --  registered.
   --  If non-null, the application id must be valid. See
   --  Glib.Application.Id_Is_Valid.
   --  Since: gtk+ 2.28
   --  "application_id": the identifier for Application

   function Get_Dbus_Object_Path
      (Self : not null access Gapplication_Record) return UTF8_String;
   --  Gets the D-Bus object path being used by the application, or null.
   --  If Glib.Application.Gapplication is using its D-Bus backend then this
   --  function will return the D-Bus object path that
   --  Glib.Application.Gapplication is using. If the application is the
   --  primary instance then there is an object published at this path. If the
   --  application is not the primary instance then the result of this function
   --  is undefined.
   --  If Glib.Application.Gapplication is not using D-Bus then this function
   --  will return null. This includes the situation where the D-Bus backend
   --  would normally be in use but we were unable to connect to the bus.
   --  This function must not be called before the application has been
   --  registered. See Glib.Application.Get_Is_Registered.
   --  Since: gtk+ 2.34

   function Get_Flags
      (Self : not null access Gapplication_Record) return GApplication_Flags;
   --  Gets the flags for Application.
   --  See Glib.Application.GApplication_Flags.
   --  Since: gtk+ 2.28

   procedure Set_Flags
      (Self  : not null access Gapplication_Record;
       Flags : GApplication_Flags);
   --  Sets the flags for Application.
   --  The flags can only be modified if Application has not yet been
   --  registered.
   --  See Glib.Application.GApplication_Flags.
   --  Since: gtk+ 2.28
   --  "flags": the flags for Application

   function Get_Inactivity_Timeout
      (Self : not null access Gapplication_Record) return Guint;
   --  Gets the current inactivity timeout for the application.
   --  This is the amount of time (in milliseconds) after the last call to
   --  Glib.Application.Release before the application stops running.
   --  Since: gtk+ 2.28

   procedure Set_Inactivity_Timeout
      (Self               : not null access Gapplication_Record;
       Inactivity_Timeout : Guint);
   --  Sets the current inactivity timeout for the application.
   --  This is the amount of time (in milliseconds) after the last call to
   --  Glib.Application.Release before the application stops running.
   --  This call has no side effects of its own. The value set here is only
   --  used for next time Glib.Application.Release drops the use count to zero.
   --  Any timeouts currently in progress are not impacted.
   --  Since: gtk+ 2.28
   --  "inactivity_timeout": the timeout, in milliseconds

   function Get_Is_Busy
      (Self : not null access Gapplication_Record) return Boolean;
   --  Gets the application's current busy state, as set through
   --  Glib.Application.Mark_Busy or Glib.Application.Bind_Busy_Property.
   --  Since: gtk+ 2.44

   function Get_Is_Registered
      (Self : not null access Gapplication_Record) return Boolean;
   --  Checks if Application is registered.
   --  An application is registered if g_application_register has been
   --  successfully called.
   --  Since: gtk+ 2.28

   function Get_Is_Remote
      (Self : not null access Gapplication_Record) return Boolean;
   --  Checks if Application is remote.
   --  If Application is remote then it means that another instance of
   --  application already exists (the 'primary' instance). Calls to perform
   --  actions on Application will result in the actions being performed by the
   --  primary instance.
   --  The value of this property cannot be accessed before
   --  g_application_register has been called. See
   --  Glib.Application.Get_Is_Registered.
   --  Since: gtk+ 2.28

   function Get_Is_Remote
      (Self : not null access Gapplication_Command_Line_Record)
       return Boolean;
   --  Determines if Cmdline represents a remote invocation.
   --  Since: gtk+ 2.28

   function Get_Resource_Base_Path
      (Self : not null access Gapplication_Record) return UTF8_String;
   --  Gets the resource base path of Application.
   --  See Glib.Application.Set_Resource_Base_Path for more information.
   --  Since: gtk+ 2.42

   procedure Set_Resource_Base_Path
      (Self          : not null access Gapplication_Record;
       Resource_Path : UTF8_String := "");
   --  Sets (or unsets) the base resource path of Application.
   --  The path is used to automatically load various [application
   --  resources][gresource] such as menu layouts and action descriptions. The
   --  various types of resources will be found at fixed names relative to the
   --  given base path.
   --  By default, the resource base path is determined from the application
   --  ID by prefixing '/' and replacing each '.' with '/'. This is done at the
   --  time that the Glib.Application.Gapplication object is constructed.
   --  Changes to the application ID after that point will not have an impact
   --  on the resource base path.
   --  As an example, if the application has an ID of "org.example.app" then
   --  the default resource base path will be "/org/example/app". If this is a
   --  Gtk.Application.Gtk_Application (and you have not manually changed the
   --  path) then Gtk will then search for the menus of the application at
   --  "/org/example/app/gtk/menus.ui".
   --  See Gresource.Gresource for more information about adding resources to
   --  your application.
   --  You can disable automatic resource loading functionality by setting the
   --  path to null.
   --  Changing the resource base path once the application is running is not
   --  recommended. The point at which the resource path is consulted for
   --  forming paths for various purposes is unspecified. When writing a
   --  sub-class of Glib.Application.Gapplication you should either set the
   --  Glib.Application.Gapplication:resource-base-path property at
   --  construction time, or call this function during the instance
   --  initialization. Alternatively, you can call this function in the
   --  GApplication_Class.startup virtual function, before chaining up to the
   --  parent implementation.
   --  Since: gtk+ 2.42
   --  "resource_path": the resource path to use

   procedure Hold (Self : not null access Gapplication_Record);
   --  Increases the use count of Application.
   --  Use this function to indicate that the application has a reason to
   --  continue to run. For example, Glib.Application.Hold is called by GTK+
   --  when a toplevel window is on the screen.
   --  To cancel the hold, call Glib.Application.Release.

   procedure Mark_Busy (Self : not null access Gapplication_Record);
   --  Increases the busy count of Application.
   --  Use this function to indicate that the application is busy, for
   --  instance while a long running operation is pending.
   --  The busy state will be exposed to other processes, so a session shell
   --  will use that information to indicate the state to the user (e.g. with a
   --  spinner).
   --  To cancel the busy indication, use Glib.Application.Unmark_Busy.
   --  Since: gtk+ 2.38

   procedure Quit (Self : not null access Gapplication_Record);
   --  Immediately quits the application.
   --  Upon return to the mainloop, Glib.Application.Run will return, calling
   --  only the 'shutdown' function before doing so.
   --  The hold count is ignored. Take care if your code has called
   --  Glib.Application.Hold on the application and is therefore still
   --  expecting it to exist. (Note that you may have called
   --  Glib.Application.Hold indirectly, for example through
   --  Gtk.Application.Add_Window.)
   --  The result of calling Glib.Application.Run again after it returns is
   --  unspecified.
   --  Since: gtk+ 2.32

   procedure Release (Self : not null access Gapplication_Record);
   --  Decrease the use count of Application.
   --  When the use count reaches zero, the application will stop running.
   --  Never call this function except to cancel the effect of a previous call
   --  to Glib.Application.Hold.

   function Run
      (Self : not null access Gapplication_Record;
       Argc : Glib.Gint;
       Argv : GNAT.Strings.String_List) return Glib.Gint;
   --  Runs the application.
   --  This function is intended to be run from main and its return value is
   --  intended to be returned by main. Although you are expected to pass the
   --  Argc, Argv parameters from main to this function, it is possible to pass
   --  null if Argv is not available or commandline handling is not required.
   --  Note that on Windows, Argc and Argv are ignored, and
   --  g_win32_get_command_line is called internally (for proper support of
   --  Unicode commandline arguments).
   --  Glib.Application.Gapplication will attempt to parse the commandline
   --  arguments. You can add commandline flags to the list of recognised
   --  options by way of g_application_add_main_option_entries. After this, the
   --  Glib.Application.Gapplication::handle-local-options signal is emitted,
   --  from which the application can inspect the values of its GOption_Entrys.
   --  Glib.Application.Gapplication::handle-local-options is a good place to
   --  handle options such as `--version`, where an immediate reply from the
   --  local process is desired (instead of communicating with an
   --  already-running instance). A
   --  Glib.Application.Gapplication::handle-local-options handler can stop
   --  further processing by returning a non-negative value, which then becomes
   --  the exit status of the process.
   --  What happens next depends on the flags: if
   --  Glib.Application.G_Application_Handles_Command_Line was specified then
   --  the remaining commandline arguments are sent to the primary instance,
   --  where a Glib.Application.Gapplication::command-line signal is emitted.
   --  Otherwise, the remaining commandline arguments are assumed to be a list
   --  of files. If there are no files listed, the application is activated via
   --  the Glib.Application.Gapplication::activate signal. If there are one or
   --  more files, and Glib.Application.G_Application_Handles_Open was
   --  specified then the files are opened via the
   --  Glib.Application.Gapplication::open signal.
   --  If you are interested in doing more complicated local handling of the
   --  commandline then you should implement your own
   --  Glib.Application.Gapplication subclass and override local_command_line.
   --  In this case, you most likely want to return True from your
   --  local_command_line implementation to suppress the default handling. See
   --  [gapplication-example-cmdline2.c][gapplication-example-cmdline2] for an
   --  example.
   --  If, after the above is done, the use count of the application is zero
   --  then the exit status is returned immediately. If the use count is
   --  non-zero then the default main context is iterated until the use count
   --  falls to zero, at which point 0 is returned.
   --  If the Glib.Application.G_Application_Is_Service flag is set, then the
   --  service will run for as much as 10 seconds with a use count of zero
   --  while waiting for the message that caused the activation to arrive.
   --  After that, if the use count falls to zero the application will exit
   --  immediately, except in the case that
   --  Glib.Application.Set_Inactivity_Timeout is in use.
   --  This function sets the prgname (g_set_prgname), if not already set, to
   --  the basename of argv[0].
   --  Much like g_main_loop_run, this function will acquire the main context
   --  for the duration that the application is running.
   --  Since 2.40, applications that are not explicitly flagged as services or
   --  launchers (ie: neither Glib.Application.G_Application_Is_Service or
   --  Glib.Application.G_Application_Is_Launcher are given as flags) will
   --  check (from the default handler for local_command_line) if
   --  "--gapplication-service" was given in the command line. If this flag is
   --  present then normal commandline processing is interrupted and the
   --  Glib.Application.G_Application_Is_Service flag is set. This provides a
   --  "compromise" solution whereby running an application directly from the
   --  commandline will invoke it in the normal way (which can be useful for
   --  debugging) while still allowing applications to be D-Bus activated in
   --  service mode. The D-Bus service file should invoke the executable with
   --  "--gapplication-service" as the sole commandline argument. This approach
   --  is suitable for use by most graphical applications but should not be
   --  used from applications like editors that need precise control over when
   --  processes invoked via the commandline will exit and what their exit
   --  status will be.
   --  Since: gtk+ 2.28
   --  "argc": the argc from main (or 0 if Argv is null)
   --  "argv": the argv from main, or null

   procedure Send_Notification
      (Self         : not null access Gapplication_Record;
       Id           : UTF8_String := "";
       Notification : not null access Glib.Notification.Gnotification_Record'Class);
   --  Sends a notification on behalf of Application to the desktop shell.
   --  There is no guarantee that the notification is displayed immediately, or
   --  even at all.
   --  Notifications may persist after the application exits. It will be
   --  D-Bus-activated when the notification or one of its actions is
   --  activated.
   --  Modifying Notification after this call has no effect. However, the
   --  object can be reused for a later call to this function.
   --  Id may be any string that uniquely identifies the event for the
   --  application. It does not need to be in any special format. For example,
   --  "new-message" might be appropriate for a notification about new
   --  messages.
   --  If a previous notification was sent with the same Id, it will be
   --  replaced with Notification and shown again as if it was a new
   --  notification. This works even for notifications sent from a previous
   --  execution of the application, as long as Id is the same string.
   --  Id may be null, but it is impossible to replace or withdraw
   --  notifications without an id.
   --  If Notification is no longer relevant, it can be withdrawn with
   --  Glib.Application.Withdraw_Notification.
   --  Since: gtk+ 2.40
   --  "id": id of the notification, or null
   --  "notification": the Glib.Notification.Gnotification to send

   procedure Set_Action_Group
      (Self         : not null access Gapplication_Record;
       Action_Group : Glib.Action_Group.Gaction_Group);
   pragma Obsolescent (Set_Action_Group);
   --  This used to be how actions were associated with a
   --  Glib.Application.Gapplication. Now there is Glib.Action_Map.Gaction_Map
   --  for that.
   --  Since: gtk+ 2.28
   --  Deprecated since 2.32, 1
   --  "action_group": a Glib.Action_Group.Gaction_Group, or null

   procedure Set_Default (Self : not null access Gapplication_Record);
   --  Sets or unsets the default application for the process, as returned by
   --  Glib.Application.Get_Default.
   --  This function does not take its own reference on Application. If
   --  Application is destroyed then the default application will revert back
   --  to null.
   --  Since: gtk+ 2.32

   procedure Set_Option_Context_Description
      (Self        : not null access Gapplication_Record;
       Description : UTF8_String := "");
   --  Adds a description to the Application option context.
   --  See Glib.Option.Set_Description for more information.
   --  Since: gtk+ 2.56
   --  "description": a string to be shown in `--help` output after the list
   --  of options, or null

   procedure Set_Option_Context_Parameter_String
      (Self             : not null access Gapplication_Record;
       Parameter_String : UTF8_String := "");
   --  Sets the parameter string to be used by the commandline handling of
   --  Application.
   --  This function registers the argument to be passed to Glib.Option.G_New
   --  when the internal Glib.Option.Goption_Context of Application is created.
   --  See Glib.Option.G_New for more information about Parameter_String.
   --  Since: gtk+ 2.56
   --  "parameter_string": a string which is displayed in the first line of
   --  `--help` output, after the usage summary `programname [OPTION...]`.

   procedure Set_Option_Context_Summary
      (Self    : not null access Gapplication_Record;
       Summary : UTF8_String := "");
   --  Adds a summary to the Application option context.
   --  See Glib.Option.Set_Summary for more information.
   --  Since: gtk+ 2.56
   --  "summary": a string to be shown in `--help` output before the list of
   --  options, or null

   procedure Unbind_Busy_Property
      (Self     : not null access Gapplication_Record;
       Object   : System.Address;
       Property : UTF8_String);
   --  Destroys a binding between Property and the busy state of Application
   --  that was previously created with Glib.Application.Bind_Busy_Property.
   --  Since: gtk+ 2.44
   --  "object": a Glib.Object.GObject
   --  "property": the name of a boolean property of Object

   procedure Unmark_Busy (Self : not null access Gapplication_Record);
   --  Decreases the busy count of Application.
   --  When the busy count reaches zero, the new state will be propagated to
   --  other processes.
   --  This function must only be called to cancel the effect of a previous
   --  call to Glib.Application.Mark_Busy.
   --  Since: gtk+ 2.38

   procedure Withdraw_Notification
      (Self : not null access Gapplication_Record;
       Id   : UTF8_String);
   --  Withdraws a notification that was sent with
   --  Glib.Application.Send_Notification.
   --  This call does nothing if a notification with Id doesn't exist or the
   --  notification was never sent.
   --  This function works even for notifications sent in previous executions
   --  of this application, as long Id is the same as it was for the sent
   --  notification.
   --  Note that notifications are dismissed when the user clicks on one of
   --  the buttons in a notification or triggers its default action, so there
   --  is no need to explicitly withdraw the notification in that case.
   --  Since: gtk+ 2.40
   --  "id": id of a previously sent notification

   function Get_Arguments
      (Self : not null access Gapplication_Command_Line_Record)
       return GNAT.Strings.String_List;
   --  Gets the list of arguments that was passed on the command line.
   --  The strings in the array may contain non-UTF-8 data on UNIX (such as
   --  filenames or arguments given in the system locale) but are always in
   --  UTF-8 on Windows.
   --  If you wish to use the return value with Glib.Option.Goption_Context,
   --  you must use g_option_context_parse_strv.
   --  The return value is null-terminated and should be freed using
   --  g_strfreev.
   --  Since: gtk+ 2.28

   function Get_Cwd
      (Self : not null access Gapplication_Command_Line_Record)
       return UTF8_String;
   --  Gets the working directory of the command line invocation. The string
   --  may contain non-utf8 data.
   --  It is possible that the remote application did not send a working
   --  directory, so this may be null.
   --  The return value should not be modified or freed and is valid for as
   --  long as Cmdline exists.
   --  Since: gtk+ 2.28

   function Get_Environ
      (Self : not null access Gapplication_Command_Line_Record)
       return GNAT.Strings.String_List;
   --  Gets the contents of the 'environ' variable of the command line
   --  invocation, as would be returned by Glib.Spawn.Get_Environ, ie as a
   --  null-terminated list of strings in the form 'NAME=VALUE'. The strings
   --  may contain non-utf8 data.
   --  The remote application usually does not send an environment. Use
   --  Glib.Application.G_Application_Send_Environment to affect that. Even
   --  with this flag set it is possible that the environment is still not
   --  available (due to invocation messages from other applications).
   --  The return value should not be modified or freed and is valid for as
   --  long as Cmdline exists.
   --  See Glib.Application.Getenv if you are only interested in the value of
   --  a single environment variable.
   --  Since: gtk+ 2.28

   function Get_Exit_Status
      (Self : not null access Gapplication_Command_Line_Record)
       return Glib.Gint;
   --  Gets the exit status of Cmdline. See Glib.Application.Set_Exit_Status
   --  for more information.
   --  Since: gtk+ 2.28

   procedure Set_Exit_Status
      (Self        : not null access Gapplication_Command_Line_Record;
       Exit_Status : Glib.Gint);
   --  Sets the exit status that will be used when the invoking process exits.
   --  The return value of the Glib.Application.Gapplication::command-line
   --  signal is passed to this function when the handler returns. This is the
   --  usual way of setting the exit status.
   --  In the event that you want the remote invocation to continue running
   --  and want to decide on the exit status in the future, you can use this
   --  call. For the case of a remote invocation, the remote process will
   --  typically exit when the last reference is dropped on Cmdline. The exit
   --  status of the remote process will be equal to the last value that was
   --  set with this function.
   --  In the case that the commandline invocation is local, the situation is
   --  slightly more complicated. If the commandline invocation results in the
   --  mainloop running (ie: because the use-count of the application increased
   --  to a non-zero value) then the application is considered to have been
   --  'successful' in a certain sense, and the exit status is always zero. If
   --  the application use count is zero, though, the exit status of the local
   --  Glib.Application.Gapplication_Command_Line is used.
   --  Since: gtk+ 2.28
   --  "exit_status": the exit status

   function Get_Platform_Data
      (Self : not null access Gapplication_Command_Line_Record)
       return Glib.Variant.Gvariant;
   --  Gets the platform data associated with the invocation of Cmdline.
   --  This is a Glib.Variant.Gvariant dictionary containing information about
   --  the context in which the invocation occurred. It typically contains
   --  information like the current working directory and the startup
   --  notification ID.
   --  For local invocation, it will be null.
   --  Since: gtk+ 2.28

   function Getenv
      (Self : not null access Gapplication_Command_Line_Record;
       Name : UTF8_String) return UTF8_String;
   --  Gets the value of a particular environment variable of the command line
   --  invocation, as would be returned by g_getenv. The strings may contain
   --  non-utf8 data.
   --  The remote application usually does not send an environment. Use
   --  Glib.Application.G_Application_Send_Environment to affect that. Even
   --  with this flag set it is possible that the environment is still not
   --  available (due to invocation messages from other applications).
   --  The return value should not be modified or freed and is valid for as
   --  long as Cmdline exists.
   --  Since: gtk+ 2.28
   --  "name": the environment variable to get

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Register
     (Self        : not null access Gapplication_Record;
      Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
   return Boolean;
   --  Attempts registration of the application.
   --  This is the point at which the application discovers if it is the
   --  primary instance or merely acting as a remote for an already-existing
   --  primary instance. This is implemented by attempting to acquire the
   --  application identifier as a unique bus name on the session bus using
   --  GDBus.
   --  If there is no application ID or if
   --  Glib.Application.G_Application_Non_Unique was given, then this process
   --  will always become the primary instance.
   --  Due to the internal architecture of GDBus, method calls can be
   --  dispatched at any time (even if a main loop is not running). For this
   --  reason, you must ensure that any object paths that you wish to register
   --  are registered before calling this function.
   --  If the application has already been registered then True is returned
   --  with no work performed.
   --  The Glib.Application.Gapplication::startup signal is emitted if
   --  registration succeeds and Application is the primary instance (including
   --  the non-unique case).
   --  In the event of an error (such as Cancellable being cancelled, or a
   --  failure to connect to the session bus), False is returned and Error is
   --  set appropriately.
   --  Note: the return value of this function is not an indicator that this
   --  instance is or is not the primary instance of the application. See
   --  Glib.Application.Get_Is_Remote for that.
   --  Since: gtk+ 2.28
   --  "cancellable": a Glib.Cancellable.Gcancellable, or null

   function Run
     (Self : not null access Gapplication_Record) return Gint;
   --  Same as above, but automatically sets argc argv from actual values.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Action_Added
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String);

   procedure Action_Enabled_Changed
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String;
       Enabled     : Boolean);

   procedure Action_Removed
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String);

   procedure Action_State_Changed
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String;
       State       : Glib.Variant.Gvariant);

   procedure Activate_Action
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String;
       Parameter   : Glib.Variant.Gvariant);

   procedure Change_Action_State
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String;
       Value       : Glib.Variant.Gvariant);

   function Get_Action_Enabled
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Boolean;

   function Get_Action_Parameter_Type
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type;

   function Get_Action_State
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant;

   function Get_Action_State_Hint
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant;

   function Get_Action_State_Type
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type;

   function Has_Action
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Boolean;

   function List_Actions
      (Self : not null access Gapplication_Record)
       return GNAT.Strings.String_List;

   function Query_Action
      (Self           : not null access Gapplication_Record;
       Action_Name    : UTF8_String;
       Enabled        : access Boolean;
       Parameter_Type : access Glib.Variant.Gvariant_Type;
       State_Type     : access Glib.Variant.Gvariant_Type;
       State_Hint     : access Glib.Variant.Gvariant;
       State          : access Glib.Variant.Gvariant) return Boolean;

   procedure Add_Action
      (Self   : not null access Gapplication_Record;
       Action : Glib.Action.Gaction);

   procedure Add_Action_Entries
      (Self      : not null access Gapplication_Record;
       Entries   : GAction_Entry_Array;
       User_Data : System.Address := System.Null_Address);

   function Lookup_Action
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Glib.Action.Gaction;

   procedure Remove_Action
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String);

   ---------------
   -- Functions --
   ---------------

   function Get_Default return Gapplication;
   --  Returns the default Glib.Application.Gapplication instance for this
   --  process.
   --  Normally there is only one Glib.Application.Gapplication per process
   --  and it becomes the default when it is created. You can exercise more
   --  control over this by using Glib.Application.Set_Default.
   --  If there is no default application then null is returned.
   --  Since: gtk+ 2.32

   function Id_Is_Valid (Application_Id : UTF8_String) return Boolean;
   --  Checks if Application_Id is a valid application identifier.
   --  A valid ID is required for calls to Glib.Application.G_New and
   --  Glib.Application.Set_Application_Id.
   --  Application identifiers follow the same format as [D-Bus well-known bus
   --  names](https://dbus.freedesktop.org/doc/dbus-specification.htmlmessage-protocol-names-bus).
   --  For convenience, the restrictions on application identifiers are
   --  reproduced here:
   --  - Application identifiers are composed of 1 or more elements separated
   --  by a period (`.`) character. All elements must contain at least one
   --  character.
   --  - Each element must only contain the ASCII characters
   --  `[A-Z][a-z][0-9]_-`, with `-` discouraged in new application
   --  identifiers. Each element must not begin with a digit.
   --  - Application identifiers must contain at least one `.` (period)
   --  character (and thus at least two elements).
   --  - Application identifiers must not begin with a `.` (period) character.
   --  - Application identifiers must not exceed 255 characters.
   --  Note that the hyphen (`-`) character is allowed in application
   --  identifiers, but is problematic or not allowed in various specifications
   --  and APIs that refer to D-Bus, such as [Flatpak application
   --  IDs](http://docs.flatpak.org/en/latest/introduction.htmlidentifiers),
   --  the [`DBusActivatable` interface in the Desktop Entry
   --  Specification](https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.htmldbus),
   --  and the convention that an application's "main" interface and object
   --  path resemble its application identifier and bus name. To avoid
   --  situations that require special-case handling, it is recommended that
   --  new application identifiers consistently replace hyphens with
   --  underscores.
   --  Like D-Bus interface names, application identifiers should start with
   --  the reversed DNS domain name of the author of the interface (in
   --  lower-case), and it is conventional for the rest of the application
   --  identifier to consist of words run together, with initial capital
   --  letters.
   --  As with D-Bus interface names, if the author's DNS domain name contains
   --  hyphen/minus characters they should be replaced by underscores, and if
   --  it contains leading digits they should be escaped by prepending an
   --  underscore. For example, if the owner of 7-zip.org used an application
   --  identifier for an archiving application, it might be named
   --  `org._7_zip.Archiver`.
   --  "application_id": a potential application identifier

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Action_Group_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Action_Group.Gtk_Action_Group
   --  Flags: write

   Application_Id_Property : constant Glib.Properties.Property_String;

   Flags_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Application_Flags

   Inactivity_Timeout_Property : constant Glib.Properties.Property_Uint;

   Is_Busy_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the application is currently marked as busy through
   --  Glib.Application.Mark_Busy or Glib.Application.Bind_Busy_Property.

   Is_Registered_Property : constant Glib.Properties.Property_Boolean;

   Is_Remote_Property : constant Glib.Properties.Property_Boolean;

   Resource_Base_Path_Property : constant Glib.Properties.Property_String;

   Arguments_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Variant.Gvariant
   --  Flags: write

   Options_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Variant.Gvariant
   --  Flags: write

   Platform_Data_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Variant.Gvariant
   --  Flags: write

   -------------
   -- Signals --
   -------------

   type Cb_Gapplication_Void is not null access procedure (Self : access Gapplication_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gapplication_Record;
       Call  : Cb_Gapplication_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gapplication_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::activate signal is emitted on the primary instance when an
   --  activation occurs. See Glib.Application.Activate.

   type Cb_Gapplication_Gapplication_Command_Line_Gint is not null access function
     (Self         : access Gapplication_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
   return Glib.Gint;

   type Cb_GObject_Gapplication_Command_Line_Gint is not null access function
     (Self         : access Glib.Object.GObject_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
   return Glib.Gint;

   Signal_Command_Line : constant Glib.Signal_Name := "command-line";
   procedure On_Command_Line
      (Self  : not null access Gapplication_Record;
       Call  : Cb_Gapplication_Gapplication_Command_Line_Gint;
       After : Boolean := False);
   procedure On_Command_Line
      (Self  : not null access Gapplication_Record;
       Call  : Cb_GObject_Gapplication_Command_Line_Gint;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::command-line signal is emitted on the primary instance when a
   --  commandline is not handled locally. See Glib.Application.Run and the
   --  Glib.Application.Gapplication_Command_Line documentation for more
   --  information.
   -- 
   --  Callback parameters:
   --    --  "command_line": a Glib.Application.Gapplication_Command_Line
   --    --  representing the passed commandline
   --    --  Returns An integer that is set as the exit status for the calling
   --   process. See Glib.Application.Set_Exit_Status.

   Signal_Handle_Local_Options : constant Glib.Signal_Name := "handle-local-options";
   --  The ::handle-local-options signal is emitted on the local instance
   --  after the parsing of the commandline options has occurred.
   --
   --  You can add options to be recognised during commandline option parsing
   --  using g_application_add_main_option_entries and
   --  g_application_add_option_group.
   --
   --  Signal handlers can inspect Options (along with values pointed to from
   --  the Arg_Data of an installed GOption_Entrys) in order to decide to
   --  perform certain actions, including direct local handling (which may be
   --  useful for options like --version).
   --
   --  In the event that the application is marked
   --  Glib.Application.G_Application_Handles_Command_Line the "normal
   --  processing" will send the Options dictionary to the primary instance
   --  where it can be read with g_application_command_line_get_options_dict.
   --  The signal handler can modify the dictionary before returning, and the
   --  modified dictionary will be sent.
   --
   --  In the event that Glib.Application.G_Application_Handles_Command_Line
   --  is not set, "normal processing" will treat the remaining uncollected
   --  command line arguments as filenames or URIs. If there are no arguments,
   --  the application is activated by Glib.Application.Activate. One or more
   --  arguments results in a call to g_application_open.
   --
   --  If you want to handle the local commandline arguments for yourself by
   --  converting them to calls to g_application_open or
   --  Glib.Action_Group.Activate_Action then you must be sure to register the
   --  application first. You should probably not call
   --  Glib.Application.Activate for yourself, however: just return -1 and
   --  allow the default handler to do it for you. This will ensure that the
   --  `--gapplication-service` switch works properly (i.e. no activation in
   --  that case).
   --
   --  Note that this signal is emitted from the default implementation of
   --  local_command_line. If you override that function and don't chain up
   --  then this signal will never be emitted.
   --
   --  You can override local_command_line if you need more powerful
   --  capabilities than what is provided here, but this should not normally be
   --  required.
   --    function Handler
   --       (Self    : access Gapplication_Record'Class;
   --        Options : GLib.Variant_Dict) return Glib.Gint
   -- 
   --  Callback parameters:
   --    --  "options": the options dictionary
   --    --  Returns an exit code. If you have handled your options and want
   -- to exit the process, return a non-negative option, 0 for success,
   -- and a positive value for failure. To continue, return -1 to let
   -- the default option processing continue.

   type Cb_Gapplication_Boolean is not null access function
     (Self : access Gapplication_Record'Class) return Boolean;

   type Cb_GObject_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Boolean;

   Signal_Name_Lost : constant Glib.Signal_Name := "name-lost";
   procedure On_Name_Lost
      (Self  : not null access Gapplication_Record;
       Call  : Cb_Gapplication_Boolean;
       After : Boolean := False);
   procedure On_Name_Lost
      (Self  : not null access Gapplication_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::name-lost signal is emitted only on the registered primary
   --  instance when a new instance has taken over. This can only happen if the
   --  application is using the
   --  Glib.Application.G_Application_Allow_Replacement flag.
   --
   --  The default handler for this signal calls Glib.Application.Quit.
   -- 
   --  Callback parameters:
   --    --  Returns True if the signal has been handled

   Signal_Open : constant Glib.Signal_Name := "open";
   --  The ::open signal is emitted on the primary instance when there are
   --  files to open. See g_application_open for more information.
   --    procedure Handler
   --       (Self    : access Gapplication_Record'Class;
   --        Files   : array_of_File;
   --        N_Files : Glib.Gint;
   --        Hint    : UTF8_String)
   -- 
   --  Callback parameters:
   --    --  "files": an array of GFiles
   --    --  "n_files": the length of Files
   --    --  "hint": a hint provided by the calling instance

   Signal_Shutdown : constant Glib.Signal_Name := "shutdown";
   procedure On_Shutdown
      (Self  : not null access Gapplication_Record;
       Call  : Cb_Gapplication_Void;
       After : Boolean := False);
   procedure On_Shutdown
      (Self  : not null access Gapplication_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::shutdown signal is emitted only on the registered primary
   --  instance immediately after the main loop terminates.

   Signal_Startup : constant Glib.Signal_Name := "startup";
   procedure On_Startup
      (Self  : not null access Gapplication_Record;
       Call  : Cb_Gapplication_Void;
       After : Boolean := False);
   procedure On_Startup
      (Self  : not null access Gapplication_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::startup signal is emitted on the primary instance immediately
   --  after registration. See g_application_register.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "ActionGroup"
   --
   --  - "ActionMap"

   package Implements_Gaction_Group is new Glib.Types.Implements
     (Glib.Action_Group.Gaction_Group, Gapplication_Record, Gapplication);
   function "+"
     (Widget : access Gapplication_Record'Class)
   return Glib.Action_Group.Gaction_Group
   renames Implements_Gaction_Group.To_Interface;
   function "-"
     (Interf : Glib.Action_Group.Gaction_Group)
   return Gapplication
   renames Implements_Gaction_Group.To_Object;

   package Implements_Gaction_Map is new Glib.Types.Implements
     (Glib.Action_Map.Gaction_Map, Gapplication_Record, Gapplication);
   function "+"
     (Widget : access Gapplication_Record'Class)
   return Glib.Action_Map.Gaction_Map
   renames Implements_Gaction_Map.To_Interface;
   function "-"
     (Interf : Glib.Action_Map.Gaction_Map)
   return Gapplication
   renames Implements_Gaction_Map.To_Object;

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Command_Line is access function
     (Self         : System.Address;
      Command_Line : System.Address) return Glib.Gint;
   pragma Convention (C, Virtual_Command_Line);

   type Virtual_Local_Command_Line is access function
     (Self        : System.Address;
      Arguments   : access Gtkada.Bindings.chars_ptr_array_access;
      Exit_Status : access Glib.Gint) return Glib.Gboolean;
   pragma Convention (C, Virtual_Local_Command_Line);
   --  This virtual function is always invoked in the local instance. It gets
   --  passed a pointer to a null-terminated copy of Argv and is expected to
   --  remove arguments that it handled (shifting up remaining arguments).
   --  The last argument to local_command_line is a pointer to the Status
   --  variable which can used to set the exit status that is returned from
   --  Glib.Application.Run.
   --  See Glib.Application.Run for more details on
   --  Glib.Application.Gapplication startup.
   --  "arguments": array of command line arguments
   --  "exit_status": exit status to fill after processing the command line.

   subtype Application_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Command_Line
     (Self    : Glib.Object.GObject_Class;
      Handler : Virtual_Command_Line);
   pragma Import (C, Set_Command_Line, "gtkada_Application_set_command_line");

   procedure Set_Local_Command_Line
     (Self    : Glib.Object.GObject_Class;
      Handler : Virtual_Local_Command_Line);
   pragma Import (C, Set_Local_Command_Line, "gtkada_Application_set_local_command_line");
   --  See Glib.Object.Add_Interface

private
   Platform_Data_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("platform-data");
   Options_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("options");
   Arguments_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("arguments");
   Resource_Base_Path_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("resource-base-path");
   Is_Remote_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-remote");
   Is_Registered_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-registered");
   Is_Busy_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-busy");
   Inactivity_Timeout_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("inactivity-timeout");
   Flags_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("flags");
   Application_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("application-id");
   Action_Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("action-group");
end Glib.Application;
