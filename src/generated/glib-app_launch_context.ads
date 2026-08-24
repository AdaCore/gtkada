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

--  Integrating the launch with the launching application. This is used to
--  handle for instance startup notification and launching the new application
--  on the same screen as the launching window.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings; use GNAT.Strings;
with Glib.Object;  use Glib.Object;
with Glib.Variant; use Glib.Variant;

package Glib.App_Launch_Context is

   type Gapp_Launch_Context_Record is new GObject_Record with null record;
   type Gapp_Launch_Context is access all Gapp_Launch_Context_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gapp_Launch_Context);
   --  Creates a new application launch context. This is not normally used,
   --  instead you instantiate a subclass of this, such as
   --  Gdk.App_Launch_Context.Gdk_App_Launch_Context.

   procedure Initialize
      (Self : not null access Gapp_Launch_Context_Record'Class);
   --  Creates a new application launch context. This is not normally used,
   --  instead you instantiate a subclass of this, such as
   --  Gdk.App_Launch_Context.Gdk_App_Launch_Context.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gapp_Launch_Context_New return Gapp_Launch_Context;
   --  Creates a new application launch context. This is not normally used,
   --  instead you instantiate a subclass of this, such as
   --  Gdk.App_Launch_Context.Gdk_App_Launch_Context.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_app_launch_context_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Environment
      (Self : not null access Gapp_Launch_Context_Record)
       return GNAT.Strings.String_List;
   --  Gets the complete environment variable list to be passed to the child
   --  process when Context is used to launch an application. This is a
   --  null-terminated array of strings, where each string has the form
   --  `KEY=VALUE`.
   --  Since: gtk+ 2.32
   --  @return the child's environment

   procedure Launch_Failed
      (Self              : not null access Gapp_Launch_Context_Record;
       Startup_Notify_Id : UTF8_String);
   --  Called when an application has failed to launch, so that it can cancel
   --  the application startup notification started in
   --  g_app_launch_context_get_startup_notify_id.
   --  @param Startup_Notify_Id the startup notification id that was returned
   --  by g_app_launch_context_get_startup_notify_id.

   procedure Setenv
      (Self     : not null access Gapp_Launch_Context_Record;
       Variable : UTF8_String;
       Value    : UTF8_String);
   --  Arranges for Variable to be set to Value in the child's environment
   --  when Context is used to launch an application.
   --  Since: gtk+ 2.32
   --  @param Variable the environment variable to set
   --  @param Value the value for to set the variable to.

   procedure Unsetenv
      (Self     : not null access Gapp_Launch_Context_Record;
       Variable : UTF8_String);
   --  Arranges for Variable to be unset in the child's environment when
   --  Context is used to launch an application.
   --  Since: gtk+ 2.32
   --  @param Variable the environment variable to remove

   -------------
   -- Signals --
   -------------

   type Cb_Gapp_Launch_Context_UTF8_String_Void is not null access procedure
     (Self              : access Gapp_Launch_Context_Record'Class;
      Startup_Notify_Id : UTF8_String);

   type Cb_GObject_UTF8_String_Void is not null access procedure
     (Self              : access Glib.Object.GObject_Record'Class;
      Startup_Notify_Id : UTF8_String);

   Signal_Launch_Failed : constant Glib.Signal_Name := "launch-failed";
   procedure On_Launch_Failed
      (Self  : not null access Gapp_Launch_Context_Record;
       Call  : Cb_Gapp_Launch_Context_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Launch_Failed
      (Self  : not null access Gapp_Launch_Context_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The Glib.App_Launch_Context.Gapp_Launch_Context::launch-failed signal
   --  is emitted when a Glib.GApp_Info launch fails. The startup notification
   --  id is provided, so that the launcher can cancel the startup
   --  notification.
   --
   --  Because a launch operation may involve spawning multiple instances of
   --  the target application, you should expect this signal to be emitted
   --  multiple times, one for each spawned instance.

   type Cb_Gapp_Launch_Context_App_Info_Gvariant_Void is not null access procedure
     (Self          : access Gapp_Launch_Context_Record'Class;
      Info          : App_Info;
      Platform_Data : Glib.Variant.Gvariant);

   type Cb_GObject_App_Info_Gvariant_Void is not null access procedure
     (Self          : access Glib.Object.GObject_Record'Class;
      Info          : App_Info;
      Platform_Data : Glib.Variant.Gvariant);

   Signal_Launch_Started : constant Glib.Signal_Name := "launch-started";
   procedure On_Launch_Started
      (Self  : not null access Gapp_Launch_Context_Record;
       Call  : Cb_Gapp_Launch_Context_App_Info_Gvariant_Void;
       After : Boolean := False);
   procedure On_Launch_Started
      (Self  : not null access Gapp_Launch_Context_Record;
       Call  : Cb_GObject_App_Info_Gvariant_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The Glib.App_Launch_Context.Gapp_Launch_Context::launch-started signal
   --  is emitted when a Glib.GApp_Info is about to be launched. If non-null
   --  the Platform_Data is an GVariant dictionary mapping strings to variants
   --  (ie `a{sv}`), which contains additional, platform-specific data about
   --  this launch. On UNIX, at least the `startup-notification-id` keys will
   --  be present.
   --
   --  The value of the `startup-notification-id` key (type `s`) is a startup
   --  notification ID corresponding to the format from the
   --  [startup-notification
   --  specification](https://specifications.freedesktop.org/startup-notification-spec/startup-notification-0.1.txt).
   --  It allows tracking the progress of the launchee through startup.
   --
   --  It is guaranteed that this signal is followed by either a
   --  Glib.App_Launch_Context.Gapp_Launch_Context::launched or
   --  Glib.App_Launch_Context.Gapp_Launch_Context::launch-failed signal.
   --
   --  Because a launch operation may involve spawning multiple instances of
   --  the target application, you should expect this signal to be emitted
   --  multiple times, one for each spawned instance.
   -- 
   --  Callback parameters:
   --    --  @param Info the Glib.GApp_Info that is about to be launched
   --    --  @param Platform_Data additional platform-specific data for this launch

   Signal_Launched : constant Glib.Signal_Name := "launched";
   --  The Glib.App_Launch_Context.Gapp_Launch_Context::launched signal is
   --  emitted when a Glib.GApp_Info is successfully launched.
   --
   --  Because a launch operation may involve spawning multiple instances of
   --  the target application, you should expect this signal to be emitted
   --  multiple times, one time for each spawned instance.
   --
   --  The Platform_Data is an GVariant dictionary mapping strings to variants
   --  (ie `a{sv}`), which contains additional, platform-specific data about
   --  this launch. On UNIX, at least the `pid` and `startup-notification-id`
   --  keys will be present.
   --
   --  Since 2.72 the `pid` may be 0 if the process id wasn't known (for
   --  example if the process was launched via D-Bus). The `pid` may not be set
   --  at all in subsequent releases.
   --
   --  On Windows, `pid` is guaranteed to be valid only for the duration of
   --  the Glib.App_Launch_Context.Gapp_Launch_Context::launched signal
   --  emission; after the signal is emitted, GLib will call
   --  Glib.Spawn.Spawn_Close_Pid. If you need to keep the GPid after the
   --  signal has been emitted, then you can duplicate `pid` using
   --  `DuplicateHandle`.
   --    procedure Handler
   --       (Self          : access Gapp_Launch_Context_Record'Class;
   --        Info          : App_Info;
   --        Platform_Data : Glib.Variant.Gvariant)
   -- 
   --  Callback parameters:
   --    --  @param Info the Glib.GApp_Info that was just launched
   --    --  @param Platform_Data additional platform-specific data for this launch

end Glib.App_Launch_Context;
