------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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
--  'use count' for the primary application instance. The use count can be
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
--  is called the 'primary instance'; for non-unique applications this is the
--  always the current instance. On Linux, the D-Bus session bus is used for
--  communication.
--
--  The use of Glib.Application.Gapplication differs from some other
--  commonly-used uniqueness libraries (such as libunique) in important ways.
--  The application is not expected to manually register itself and check if it
--  is the primary instance. Instead, the <code>main</code> function of a
--  Glib.Application.Gapplication should do very little more than instantiating
--  the application instance, possibly connecting signal handlers, then calling
--  Glib.Application.Run. All checks for uniqueness are done internally. If the
--  application is the primary instance then the startup signal is emitted and
--  the mainloop runs. If the application is not the primary instance then a
--  signal is sent to the primary instance and Glib.Application.Run promptly
--  returns. See the code examples below.
--
--  If used, the expected form of an application identifier is very close to
--  that of of a <ulink
--  url="http://dbus.freedesktop.org/doc/dbus-specification.htmlmessage-protocol-names-interface">DBus
--  bus name</ulink>. Examples include: "com.example.MyApp",
--  "org.example.internal-apps.Calculator". For details on valid application
--  identifiers, see Glib.Application.Id_Is_Valid.
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
--  application (which happens in Glib.Application.Register). Unfortunately,
--  this means that you cannot use Glib.Application.Get_Is_Remote to decide if
--  you want to register object paths.
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
--     * via 'Activate' (i.e. just starting the application)
--
--     * via 'Open' (i.e. opening some files)
--
--     * by handling a command-line
--
--     * via activating an action
--
--  The Glib.Application.Gapplication::startup signal lets you handle the
--  application initialization for all of these in a single place.
--
--  Regardless of which of these entry points is used to start the
--  application, GApplication passes some <firstterm
--  id="platform-data">platform data' from the launching instance to the
--  primary instance, in the form of a Glib.Variant.Gvariant dictionary mapping
--  strings to variants. To use platform data, override the Before_Emit or
--  After_Emit virtual functions in your Glib.Application.Gapplication
--  subclass. When dealing with Glib.Application.Gapplication_Command_Line
--  objects, the platform data is directly available via
--  Glib.Application.Get_Cwd, Glib.Application.Get_Environ and
--  Glib.Application.Get_Platform_Data.
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
--  <example id="gapplication-example-open">
--  == Opening files with a GApplication ==
--
--    <xi:include xmlns:xi="http://www.w3.org/2001/XInclude" parse="text" href="../../../../gio/tests/gapplication-example-open.c">
--    <xi:fallback>FIXME: MISSING XINCLUDE CONTENT</xi:fallback>
--    </xi:include>
--  <example id="gapplication-example-actions">
--  == A GApplication with actions ==
--
--    <xi:include xmlns:xi="http://www.w3.org/2001/XInclude" parse="text" href="../../../../gio/tests/gapplication-example-actions.c">
--    <xi:fallback>FIXME: MISSING XINCLUDE CONTENT</xi:fallback>
--    </xi:include>
--  <example id="gapplication-example-menu">
--  == A GApplication with menus ==
--
--    <xi:include xmlns:xi="http://www.w3.org/2001/XInclude" parse="text" href="../../../../gio/tests/gapplication-example-menu.c">
--    <xi:fallback>FIXME: MISSING XINCLUDE CONTENT</xi:fallback>
--    </xi:include>
--  <example id="gapplication-example-dbushooks">
--  == Using extra D-Bus hooks with a GApplication ==
--
--    <xi:include xmlns:xi="http://www.w3.org/2001/XInclude" parse="text" href="../../../../gio/tests/gapplication-example-dbushooks.c">
--    <xi:fallback>FIXME: MISSING XINCLUDE CONTENT</xi:fallback>
--    </xi:include>
--  </description>
pragma Ada_2005;


pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Glib;                    use Glib;
with Glib.Action;             use Glib.Action;
with Glib.Action_Group;       use Glib.Action_Group;
with Glib.Action_Map;         use Glib.Action_Map;
with Glib.Cancellable;        use Glib.Cancellable;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Glib.Variant;            use Glib.Variant;

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

   function Get_Is_Registered
      (Self : not null access Gapplication_Record) return Boolean;
   --  Checks if Application is registered.
   --  An application is registered if Glib.Application.Register has been
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
   --  Glib.Application.Register has been called. See
   --  Glib.Application.Get_Is_Registered.
   --  Since: gtk+ 2.28

   function Get_Is_Remote
      (Self : not null access Gapplication_Command_Line_Record)
       return Boolean;
   --  Determines if Cmdline represents a remote invocation.
   --  Since: gtk+ 2.28

   procedure Hold (Self : not null access Gapplication_Record);
   --  Increases the use count of Application.
   --  Use this function to indicate that the application has a reason to
   --  continue to run. For example, Glib.Application.Hold is called by GTK+
   --  when a toplevel window is on the screen.
   --  To cancel the hold, call Glib.Application.Release.

   procedure Quit (Self : not null access Gapplication_Record);
   --  Immediately quits the application.
   --  Upon return to the mainloop, Glib.Application.Run will return, calling
   --  only the 'shutdown' function before doing so.
   --  The hold count is ignored.
   --  The result of calling Glib.Application.Run again after it returns is
   --  unspecified.
   --  Since: gtk+ 2.32

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

   procedure Release (Self : not null access Gapplication_Record);
   --  Decrease the use count of Application.
   --  When the use count reaches zero, the application will stop running.
   --  Never call this function except to cancel the effect of a previous call
   --  to Glib.Application.Hold.

   function Run
      (Self : not null access Gapplication_Record;
       Argc : Gint;
       Argv : GNAT.Strings.String_List) return Gint;
   --  Runs the application.
   --  This function is intended to be run from main and its return value is
   --  intended to be returned by main. Although you are expected to pass the
   --  Argc, Argv parameters from main to this function, it is possible to pass
   --  null if Argv is not available or commandline handling is not required.
   --  First, the local_command_line virtual function is invoked. This
   --  function always runs on the local instance. It gets passed a pointer to
   --  a null-terminated copy of Argv and is expected to remove the arguments
   --  that it handled (shifting up remaining arguments). See <xref
   --  linkend="gapplication-example-cmdline2"/> for an example of parsing Argv
   --  manually. Alternatively, you may use the Glib.Option.Goption_Context
   --  API, after setting 'argc = g_strv_length (argv);'.
   --  The last argument to local_command_line is a pointer to the Status
   --  variable which can used to set the exit status that is returned from
   --  Glib.Application.Run.
   --  If local_command_line returns True, the command line is expected to be
   --  completely handled, including possibly registering as the primary
   --  instance, calling Glib.Application.Activate or g_application_open, etc.
   --  If local_command_line returns False then the application is registered
   --  and the Glib.Application.Gapplication::command-line signal is emitted in
   --  the primary instance (which may or may not be this instance). The signal
   --  handler gets passed a Glib.Application.Gapplication_Command_Line object
   --  that (among other things) contains the remaining commandline arguments
   --  that have not been handled by local_command_line.
   --  If the application has the
   --  Glib.Application.G_Application_Handles_Command_Line flag set then the
   --  default implementation of local_command_line always returns False
   --  immediately, resulting in the commandline always being handled in the
   --  primary instance.
   --  Otherwise, the default implementation of local_command_line tries to do
   --  a couple of things that are probably reasonable for most applications.
   --  First, Glib.Application.Register is called to attempt to register the
   --  application. If that works, then the command line arguments are
   --  inspected. If no commandline arguments are given, then
   --  Glib.Application.Activate is called. If commandline arguments are given
   --  and the Glib.Application.G_Application_Handles_Open flag is set then
   --  they are assumed to be filenames and g_application_open is called.
   --  If you need to handle commandline arguments that are not filenames, and
   --  you don't mind commandline handling to happen in the primary instance,
   --  you should set Glib.Application.G_Application_Handles_Command_Line and
   --  process the commandline arguments in your
   --  Glib.Application.Gapplication::command-line signal handler, either
   --  manually or using the Glib.Option.Goption_Context API.
   --  If you are interested in doing more complicated local handling of the
   --  commandline then you should implement your own
   --  Glib.Application.Gapplication subclass and override local_command_line.
   --  In this case, you most likely want to return True from your
   --  local_command_line implementation to suppress the default handling. See
   --  <xref linkend="gapplication-example-cmdline2"/> for an example.
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
   --  Since: gtk+ 2.28
   --  "argc": the argc from main (or 0 if Argv is null)
   --  "argv": the argv from main, or null

   procedure Set_Action_Group
      (Self         : not null access Gapplication_Record;
       Action_Group : Glib.Action_Group.Gaction_Group);
   pragma Obsolescent (Set_Action_Group);
   --  This used to be how actions were associated with a
   --  Glib.Application.Gapplication. Now there is Glib.Action_Map.Gaction_Map
   --  for that.
   --  Since: gtk+ 2.28
   --  Deprecated since 2.32:Use the Glib.Action_Map.Gaction_Map interface
   --  instead. Never ever mix use of this API with use of
   --  Glib.Action_Map.Gaction_Map on the same Application or things will go
   --  very badly wrong. This function is known to introduce buggy behaviour
   --  (ie, signals not emitted on changes to the action group), so you should
   --  really use Glib.Action_Map.Gaction_Map instead.
   --  "action_group": a Glib.Action_Group.Gaction_Group, or null

   procedure Set_Default (Self : not null access Gapplication_Record);
   --  Sets or unsets the default application for the process, as returned by
   --  Glib.Application.Get_Default.
   --  This function does not take its own reference on Application. If
   --  Application is destroyed then the default application will revert back
   --  to null.
   --  Since: gtk+ 2.32

   function Get_Arguments
      (Self : not null access Gapplication_Command_Line_Record)
       return GNAT.Strings.String_List;
   --  Gets the list of arguments that was passed on the command line.
   --  The strings in the array may contain non-utf8 data.
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
   --  invocation, as would be returned by g_get_environ, ie as a
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
      (Self : not null access Gapplication_Command_Line_Record) return Gint;
   --  Gets the exit status of Cmdline. See Glib.Application.Set_Exit_Status
   --  for more information.
   --  Since: gtk+ 2.28

   procedure Set_Exit_Status
      (Self        : not null access Gapplication_Command_Line_Record;
       Exit_Status : Gint);
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
   --  For convenience, the restrictions on application identifiers are
   --  reproduced here:
   -- 
   --     * Application identifiers must contain only the ASCII characters
   --  "[A-Z][a-z][0-9]_-." and must not begin with a digit.
   --     * Application identifiers must contain at least one '.' (period)
   --  character (and thus at least three elements).
   --     * Application identifiers must not begin or end with a '.' (period)
   --  character.
   --     * Application identifiers must not contain consecutive '.' (period)
   --  characters.
   --     * Application identifiers must not exceed 255 characters.
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

   Is_Registered_Property : constant Glib.Properties.Property_Boolean;

   Is_Remote_Property : constant Glib.Properties.Property_Boolean;

   Arguments_Property : constant Glib.Properties.Property_Object;
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
   return Gint;

   type Cb_GObject_Gapplication_Command_Line_Gint is not null access function
     (Self         : access Glib.Object.GObject_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
   return Gint;

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
   --    --  Returns An integer that is set as the exit status for the calling process. See Glib.Application.Set_Exit_Status.

   Signal_Open : constant Glib.Signal_Name := "open";
   --  The ::open signal is emitted on the primary instance when there are
   --  files to open. See g_application_open for more information.
   --    procedure Handler
   --       (Self    : access Gapplication_Record'Class;
   --        Files   : array_of_File;
   --        N_Files : Gint;
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
   --  after registration. See Glib.Application.Register.

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

private
   Platform_Data_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("platform-data");
   Arguments_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("arguments");
   Is_Remote_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-remote");
   Is_Registered_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-registered");
   Inactivity_Timeout_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("inactivity-timeout");
   Flags_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("flags");
   Application_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("application-id");
   Action_Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("action-group");
end Glib.Application;
