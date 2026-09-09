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

--  Glib.App_Info.Gapp_Info and Glib.App_Launch_Context.Gapp_Launch_Context
--  are used for describing and launching applications installed on the system.
--
--  As of GLib 2.20, URIs will always be converted to POSIX paths (using
--  Glib.GFile.Get_Path) when using Glib.App_Info.Launch even if the
--  application requested an URI and not a POSIX path. For example for a
--  desktop-file based application with Exec key `totem %U` and a single URI,
--  `sftp://foo/file.avi`, then `/home/user/.gvfs/sftp on foo/file.avi` will be
--  passed. This will only work if a set of suitable GIO extensions (such as
--  gvfs 2.26 compiled with FUSE support), is available and operational; if
--  this is not the case, the URI will be passed unmodified to the application.
--  Some URIs, such as `mailto:`, of course cannot be mapped to a POSIX path
--  (in gvfs there's no FUSE mount for it); such URIs will be passed unmodified
--  to the application.
--
--  Specifically for gvfs 2.26 and later, the POSIX URI will be mapped back to
--  the GIO URI in the Glib.GFile.Gfile constructors (since gvfs implements the
--  Gvfs.Gvfs extension point). As such, if the application needs to examine
--  the URI, it needs to use Glib.GFile.Get_Uri or similar on Glib.GFile.Gfile.
--  In other words, an application cannot assume that the URI passed to e.g.
--  Glib.GFile.New_For_Commandline_Arg is equal to the result of
--  Glib.GFile.Get_Uri. The following snippet illustrates this:
--
--     GFile *f;
--     char *uri;
--
--     file = g_file_new_for_commandline_arg (uri_from_commandline);
--
--     uri = g_file_get_uri (file);
--     strcmp (uri, uri_from_commandline) == 0;
--     g_free (uri);
--
--     if (g_file_has_uri_scheme (file, "cdda"))
--       {
--         // do something special with uri
--       }
--     g_object_unref (file);
--
--
--  This code will work when both `cdda://sr0/Track 1.wav` and
--  `/home/user/.gvfs/cdda on sr0/Track 1.wav` is passed to the application. It
--  should be noted that it's generally not safe for applications to rely on
--  the format of a particular URIs. Different launcher applications (e.g. file
--  managers) may have different ideas of what a given URI means.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
limited with Glib.App_Launch_Context;
with Glib.Cancellable;        use Glib.Cancellable;
with Glib.GFile;              use Glib.GFile;
with Glib.G_Icon;             use Glib.G_Icon;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Glist;              use Glib.Glist;
with Glib.Object;             use Glib.Object;
with Glib.Types;              use Glib.Types;
with Gtk.Enums;               use Gtk.Enums;
with Gtkada.Types;            use Gtkada.Types;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;         use Gtkada.Bindings;
pragma Warnings(On);

package Glib.App_Info is

   type Gapp_Info is new Glib.Types.GType_Interface;
   Null_Gapp_Info : constant Gapp_Info;

   type Create_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Create_Flags);
   --  Flags used when creating a Glib.App_Info.Gapp_Info.

   G_App_Info_Create_None : constant Create_Flags := 0;
   G_App_Info_Create_Needs_Terminal : constant Create_Flags := 1;
   G_App_Info_Create_Supports_Uris : constant Create_Flags := 2;
   G_App_Info_Create_Supports_Startup_Notification : constant Create_Flags := 4;

   function Convert (R : Glib.App_Info.Gapp_Info) return System.Address;
   function Convert (R : System.Address) return Glib.App_Info.Gapp_Info;
   package App_Info_List is new Generic_List (Glib.App_Info.Gapp_Info);

   ---------------
   -- Callbacks --
   ---------------

   type Gasync_Ready_Callback is access procedure
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   --  Type definition for a function that will be called back when an
   --  asynchronous operation within GIO has been completed.
   --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
   --  invoked in a later iteration of the thread-default main context (see
   --  [methodGlib.MainContext.push_thread_default]) where the Gtask.Gtask was
   --  created. All other users of Gasync_Ready_Callback must likewise call it
   --  asynchronously in a later iteration of the main context.
   --  The asynchronous operation is guaranteed to have held a reference to
   --  Source_Object from the time when the `*_async` function was called,
   --  until after this callback returns.
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Create_Flags_Properties is
      new Generic_Internal_Discrete_Property (Create_Flags);
   type Property_Create_Flags is new Create_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_app_info_get_type");

   -------------
   -- Methods --
   -------------

   function Add_Supports_Type
      (Self         : Gapp_Info;
       Content_Type : UTF8_String) return Boolean;
   --  Adds a content type to the application information to indicate the
   --  application is capable of opening files with the given content type.
   --  @param Content_Type a string.
   --  @return True on success, False on error.

   function Can_Delete (Self : Gapp_Info) return Boolean;
   --  Obtains the information whether the Glib.App_Info.Gapp_Info can be
   --  deleted. See Glib.App_Info.Delete.
   --  Since: gtk+ 2.20
   --  @return True if Appinfo can be deleted

   function Can_Remove_Supports_Type (Self : Gapp_Info) return Boolean;
   --  Checks if a supported content type can be removed from an application.
   --  @return True if it is possible to remove supported content types from a
   --  given Appinfo, False if not.

   function Delete (Self : Gapp_Info) return Boolean;
   --  Tries to delete a Glib.App_Info.Gapp_Info.
   --  On some platforms, there may be a difference between user-defined
   --  GApp_Infos which can be deleted, and system-wide ones which cannot. See
   --  Glib.App_Info.Can_Delete.
   --  Since: gtk+ 2.20
   --  @return True if Appinfo has been deleted

   function Dup (Self : Gapp_Info) return Gapp_Info;
   pragma Import (C, Dup, "g_app_info_dup");
   --  Creates a duplicate of a Glib.App_Info.Gapp_Info.
   --  @return a duplicate of Appinfo.

   function Equal (Self : Gapp_Info; Appinfo2 : Gapp_Info) return Boolean;
   --  Checks if two GApp_Infos are equal.
   --  Note that the check *may not* compare each individual field, and only
   --  does an identity check. In case detecting changes in the contents is
   --  needed, program code must additionally compare relevant fields.
   --  @param Appinfo2 the second Glib.App_Info.Gapp_Info.
   --  @return True if Appinfo1 is equal to Appinfo2. False otherwise.

   function Get_Commandline (Self : Gapp_Info) return UTF8_String;
   --  Gets the commandline with which the application will be started.
   --  Since: gtk+ 2.20
   --  @return a string containing the Appinfo's commandline, or null if this
   --  information is not available

   function Get_Description (Self : Gapp_Info) return UTF8_String;
   --  Gets a human-readable description of an installed application.
   --  @return a string containing a description of the application Appinfo,
   --  or null if none.

   function Get_Display_Name (Self : Gapp_Info) return UTF8_String;
   --  Gets the display name of the application. The display name is often
   --  more descriptive to the user than the name itself.
   --  Since: gtk+ 2.24
   --  @return the display name of the application for Appinfo, or the name if
   --  no display name is available.

   function Get_Executable (Self : Gapp_Info) return UTF8_String;
   --  Gets the executable's name for the installed application.
   --  This is intended to be used for debugging or labelling what program is
   --  going to be run. To launch the executable, use Glib.App_Info.Launch and
   --  related functions, rather than spawning the return value from this
   --  function.
   --  @return a string containing the Appinfo's application binaries name

   function Get_Icon (Self : Gapp_Info) return Glib.G_Icon.G_Icon;
   pragma Import (C, Get_Icon, "g_app_info_get_icon");
   --  Gets the icon for the application.
   --  @return the default Glib.G_Icon.G_Icon for Appinfo or null if there is
   --  no default icon.

   function Get_Id (Self : Gapp_Info) return UTF8_String;
   --  Gets the ID of an application. An id is a string that identifies the
   --  application. The exact format of the id is platform dependent. For
   --  instance, on Unix this is the desktop file id from the xdg menu
   --  specification.
   --  Note that the returned ID may be null, depending on how the Appinfo has
   --  been constructed.
   --  @return a string containing the application's ID.

   function Get_Name (Self : Gapp_Info) return UTF8_String;
   --  Gets the installed name of the application.
   --  @return the name of the application for Appinfo.

   function Get_Supported_Types
      (Self : Gapp_Info) return GNAT.Strings.String_List;
   --  Retrieves the list of content types that App_Info claims to support. If
   --  this information is not provided by the environment, this function will
   --  return null. This function does not take in consideration associations
   --  added with Glib.App_Info.Add_Supports_Type, but only those exported
   --  directly by the application.
   --  Since: gtk+ 2.34
   --  @return a list of content types.

   function Launch
      (Self    : Gapp_Info;
       Files   : Glib.GFile.Gfile_List.Glist;
       Context : access Glib.App_Launch_Context.Gapp_Launch_Context_Record'Class)
       return Boolean;
   --  Launches the application. Passes Files to the launched application as
   --  arguments, using the optional Context to get information about the
   --  details of the launcher (like what screen it is on). On error, Error
   --  will be set accordingly.
   --  To launch the application without arguments pass a null Files list.
   --  Note that even if the launch is successful the application launched can
   --  fail to start if it runs into problems during startup. There is no way
   --  to detect this.
   --  Some URIs can be changed when passed through a GFile (for instance
   --  unsupported URIs with strange formats like mailto:), so if you have a
   --  textual URI you want to pass in as argument, consider using
   --  Glib.App_Info.Launch_Uris instead.
   --  The launched application inherits the environment of the launching
   --  process, but it can be modified with Glib.App_Launch_Context.Setenv and
   --  Glib.App_Launch_Context.Unsetenv.
   --  On UNIX, this function sets the `GIO_LAUNCHED_DESKTOP_FILE` environment
   --  variable with the path of the launched desktop file and
   --  `GIO_LAUNCHED_DESKTOP_FILE_PID` to the process id of the launched
   --  process. This can be used to ignore `GIO_LAUNCHED_DESKTOP_FILE`, should
   --  it be inherited by further processes. The `DISPLAY`,
   --  `XDG_ACTIVATION_TOKEN` and `DESKTOP_STARTUP_ID` environment variables
   --  are also set, based on information provided in Context.
   --  @param Files a GList of Glib.GFile.Gfile objects
   --  @param Context a Glib.App_Launch_Context.Gapp_Launch_Context or null
   --  @return True on successful launch, False otherwise.

   function Launch_Uris
      (Self    : Gapp_Info;
       Uris    : Gtk.Enums.String_List.Glist;
       Context : access Glib.App_Launch_Context.Gapp_Launch_Context_Record'Class)
       return Boolean;
   --  Launches the application. This passes the Uris to the launched
   --  application as arguments, using the optional Context to get information
   --  about the details of the launcher (like what screen it is on). On error,
   --  Error will be set accordingly. If the application only supports one URI
   --  per invocation as part of their command-line, multiple instances of the
   --  application will be spawned.
   --  To launch the application without arguments pass a null Uris list.
   --  Note that even if the launch is successful the application launched can
   --  fail to start if it runs into problems during startup. There is no way
   --  to detect this.
   --  @param Uris a GList containing URIs to launch.
   --  @param Context a Glib.App_Launch_Context.Gapp_Launch_Context or null
   --  @return True on successful launch, False otherwise.

   procedure Launch_Uris_Async
      (Self        : Gapp_Info;
       Uris        : Gtk.Enums.String_List.Glist;
       Context     : access Glib.App_Launch_Context.Gapp_Launch_Context_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Async version of Glib.App_Info.Launch_Uris.
   --  The Callback is invoked immediately after the application launch, but
   --  it waits for activation in case of D-Bus–activated applications and also
   --  provides extended error information for sandboxed applications, see
   --  notes for Glib.App_Info.Launch_Default_For_Uri_Async.
   --  Since: gtk+ 2.60
   --  @param Uris a GList containing URIs to launch.
   --  @param Context a Glib.App_Launch_Context.Gapp_Launch_Context or null
   --  @param Cancellable a Glib.Cancellable.Gcancellable
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  done

   function Launch_Uris_Finish
      (Self   : Gapp_Info;
       Result : Glib.G_Async_Result) return Boolean;
   --  Finishes a Glib.App_Info.Launch_Uris_Async operation.
   --  Since: gtk+ 2.60
   --  @param Result a Glib.G_Async_Result
   --  @return True on successful launch, False otherwise.

   function Remove_Supports_Type
      (Self         : Gapp_Info;
       Content_Type : UTF8_String) return Boolean;
   --  Removes a supported type from an application, if possible.
   --  @param Content_Type a string.
   --  @return True on success, False on error.

   function Set_As_Default_For_Extension
      (Self      : Gapp_Info;
       Extension : UTF8_String) return Boolean;
   --  Sets the application as the default handler for the given file
   --  extension.
   --  @param Extension a string containing the file extension (without the
   --  dot).
   --  @return True on success, False on error.

   function Set_As_Default_For_Type
      (Self         : Gapp_Info;
       Content_Type : UTF8_String) return Boolean;
   --  Sets the application as the default handler for a given type.
   --  @param Content_Type the content type.
   --  @return True on success, False on error.

   function Set_As_Last_Used_For_Type
      (Self         : Gapp_Info;
       Content_Type : UTF8_String) return Boolean;
   --  Sets the application as the last used application for a given type.
   --  This will make the application appear as first in the list returned by
   --  Glib.App_Info.Get_Recommended_For_Type, regardless of the default
   --  application for that content type.
   --  @param Content_Type the content type.
   --  @return True on success, False on error.

   function Should_Show (Self : Gapp_Info) return Boolean;
   --  Checks if the application info should be shown in menus that list
   --  available applications.
   --  @return True if the Appinfo should be shown, False otherwise.

   function Supports_Files (Self : Gapp_Info) return Boolean;
   --  Checks if the application accepts files as arguments.
   --  @return True if the Appinfo supports files.

   function Supports_Uris (Self : Gapp_Info) return Boolean;
   --  Checks if the application supports reading files and directories from
   --  URIs.
   --  @return True if the Appinfo supports URIs.

   procedure Get_Default_For_Type_Async
      (Content_Type      : UTF8_String;
       Must_Support_Uris : Boolean;
       Cancellable       : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback          : Gasync_Ready_Callback);
   --  Asynchronously gets the default Glib.App_Info.Gapp_Info for a given
   --  content type.
   --  Since: gtk+ 2.74
   --  @param Content_Type the content type to find a Glib.App_Info.Gapp_Info
   --  for
   --  @param Must_Support_Uris if True, the Glib.App_Info.Gapp_Info is
   --  expected to support URIs
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  done

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Get_Default_For_Type_Async_User_Data is

      type Gasync_Ready_Callback is access procedure
        (Source_Object : access Glib.Object.GObject_Record'Class;
         Res           : Glib.G_Async_Result;
         Data          : User_Data_Type);
      --  Type definition for a function that will be called back when an
      --  asynchronous operation within GIO has been completed.
      --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
      --  invoked in a later iteration of the thread-default main context (see
      --  [methodGlib.MainContext.push_thread_default]) where the Gtask.Gtask was
      --  created. All other users of Gasync_Ready_Callback must likewise call it
      --  asynchronously in a later iteration of the main context.
      --  The asynchronous operation is guaranteed to have held a reference to
      --  Source_Object from the time when the `*_async` function was called,
      --  until after this callback returns.
      --  @param Source_Object the object the asynchronous operation was started
      --  with.
      --  @param Res a Glib.G_Async_Result.
      --  @param Data user data passed to the callback.

      procedure Get_Default_For_Type_Async
         (Content_Type      : UTF8_String;
          Must_Support_Uris : Boolean;
          Cancellable       : access Glib.Cancellable.Gcancellable_Record'Class;
          Callback          : Gasync_Ready_Callback;
          User_Data         : User_Data_Type);
      --  Asynchronously gets the default Glib.App_Info.Gapp_Info for a given
      --  content type.
      --  Since: gtk+ 2.74
      --  @param Content_Type the content type to find a
      --  Glib.App_Info.Gapp_Info for
      --  @param Must_Support_Uris if True, the Glib.App_Info.Gapp_Info is
      --  expected to support URIs
      --  @param Cancellable optional Glib.Cancellable.Gcancellable object,
      --  null to ignore
      --  @param Callback a Gasync_Ready_Callback to call when the request is
      --  done
      --  @param User_Data data to pass to Callback

   end Get_Default_For_Type_Async_User_Data;

   procedure Get_Default_For_Uri_Scheme_Async
      (Uri_Scheme  : UTF8_String;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously gets the default application for handling URIs with the
   --  given URI scheme. A URI scheme is the initial part of the URI, up to but
   --  not including the ':', e.g. "http", "ftp" or "sip".
   --  Since: gtk+ 2.74
   --  @param Uri_Scheme a string containing a URI scheme.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  done

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Get_Default_For_Uri_Scheme_Async_User_Data is

      type Gasync_Ready_Callback is access procedure
        (Source_Object : access Glib.Object.GObject_Record'Class;
         Res           : Glib.G_Async_Result;
         Data          : User_Data_Type);
      --  Type definition for a function that will be called back when an
      --  asynchronous operation within GIO has been completed.
      --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
      --  invoked in a later iteration of the thread-default main context (see
      --  [methodGlib.MainContext.push_thread_default]) where the Gtask.Gtask was
      --  created. All other users of Gasync_Ready_Callback must likewise call it
      --  asynchronously in a later iteration of the main context.
      --  The asynchronous operation is guaranteed to have held a reference to
      --  Source_Object from the time when the `*_async` function was called,
      --  until after this callback returns.
      --  @param Source_Object the object the asynchronous operation was started
      --  with.
      --  @param Res a Glib.G_Async_Result.
      --  @param Data user data passed to the callback.

      procedure Get_Default_For_Uri_Scheme_Async
         (Uri_Scheme  : UTF8_String;
          Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
          Callback    : Gasync_Ready_Callback;
          User_Data   : User_Data_Type);
      --  Asynchronously gets the default application for handling URIs with
      --  the given URI scheme. A URI scheme is the initial part of the URI, up
      --  to but not including the ':', e.g. "http", "ftp" or "sip".
      --  Since: gtk+ 2.74
      --  @param Uri_Scheme a string containing a URI scheme.
      --  @param Cancellable optional Glib.Cancellable.Gcancellable object,
      --  null to ignore
      --  @param Callback a Gasync_Ready_Callback to call when the request is
      --  done
      --  @param User_Data data to pass to Callback

   end Get_Default_For_Uri_Scheme_Async_User_Data;

   procedure Launch_Default_For_Uri_Async
      (URI         : UTF8_String;
       Context     : access Glib.App_Launch_Context.Gapp_Launch_Context_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Async version of Glib.App_Info.Launch_Default_For_Uri.
   --  This version is useful if you are interested in receiving error
   --  information in the case where the application is sandboxed and the
   --  portal may present an application chooser dialog to the user.
   --  This is also useful if you want to be sure that the D-Bus–activated
   --  applications are really started before termination and if you are
   --  interested in receiving error information from their activation.
   --  Since: gtk+ 2.50
   --  @param URI the uri to show
   --  @param Context an optional Glib.App_Launch_Context.Gapp_Launch_Context
   --  @param Cancellable a Glib.Cancellable.Gcancellable
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  done

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Launch_Default_For_Uri_Async_User_Data is

      type Gasync_Ready_Callback is access procedure
        (Source_Object : access Glib.Object.GObject_Record'Class;
         Res           : Glib.G_Async_Result;
         Data          : User_Data_Type);
      --  Type definition for a function that will be called back when an
      --  asynchronous operation within GIO has been completed.
      --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
      --  invoked in a later iteration of the thread-default main context (see
      --  [methodGlib.MainContext.push_thread_default]) where the Gtask.Gtask was
      --  created. All other users of Gasync_Ready_Callback must likewise call it
      --  asynchronously in a later iteration of the main context.
      --  The asynchronous operation is guaranteed to have held a reference to
      --  Source_Object from the time when the `*_async` function was called,
      --  until after this callback returns.
      --  @param Source_Object the object the asynchronous operation was started
      --  with.
      --  @param Res a Glib.G_Async_Result.
      --  @param Data user data passed to the callback.

      procedure Launch_Default_For_Uri_Async
         (URI         : UTF8_String;
          Context     : access Glib.App_Launch_Context.Gapp_Launch_Context_Record'Class;
          Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
          Callback    : Gasync_Ready_Callback;
          User_Data   : User_Data_Type);
      --  Async version of Glib.App_Info.Launch_Default_For_Uri.
      --  This version is useful if you are interested in receiving error
      --  information in the case where the application is sandboxed and the
      --  portal may present an application chooser dialog to the user.
      --  This is also useful if you want to be sure that the D-Bus–activated
      --  applications are really started before termination and if you are
      --  interested in receiving error information from their activation.
      --  Since: gtk+ 2.50
      --  @param URI the uri to show
      --  @param Context an optional
      --  Glib.App_Launch_Context.Gapp_Launch_Context
      --  @param Cancellable a Glib.Cancellable.Gcancellable
      --  @param Callback a Gasync_Ready_Callback to call when the request is
      --  done
      --  @param User_Data data to pass to Callback

   end Launch_Default_For_Uri_Async_User_Data;

   ---------------
   -- Functions --
   ---------------

   function Create_From_Commandline
      (Commandline      : UTF8_String;
       Application_Name : UTF8_String := "";
       Flags            : Create_Flags) return Gapp_Info;
   --  Creates a new Glib.App_Info.Gapp_Info from the given information.
   --  Note that for Commandline, the quoting rules of the Exec key of the
   --  [freedesktop.org Desktop Entry
   --  Specification](http://freedesktop.org/Standards/desktop-entry-spec) are
   --  applied. For example, if the Commandline contains percent-encoded URIs,
   --  the percent-character must be doubled in order to prevent it from being
   --  swallowed by Exec key unquoting. See the specification for exact quoting
   --  rules.
   --  @param Commandline the commandline to use
   --  @param Application_Name the application name, or null to use
   --  Commandline
   --  @param Flags flags that can specify details of the created
   --  Glib.App_Info.Gapp_Info
   --  @return new Glib.App_Info.Gapp_Info for given command.

   function Get_All return App_Info_List.Glist;
   --  Gets a list of all of the applications currently registered on this
   --  system.
   --  For desktop files, this includes applications that have
   --  `NoDisplay=true` set or are excluded from display by means of
   --  `OnlyShowIn` or `NotShowIn`. See Glib.App_Info.Should_Show. The returned
   --  list does not include applications which have the `Hidden` key set.

   function Get_All_For_Type
      (Content_Type : UTF8_String) return App_Info_List.Glist;
   --  Gets a list of all GApp_Infos for a given content type, including the
   --  recommended and fallback GApp_Infos. See
   --  Glib.App_Info.Get_Recommended_For_Type and
   --  Glib.App_Info.Get_Fallback_For_Type.
   --  @param Content_Type the content type to find a Glib.App_Info.Gapp_Info
   --  for

   function Get_Default_For_Type
      (Content_Type      : UTF8_String;
       Must_Support_Uris : Boolean) return Gapp_Info;
   --  Gets the default Glib.App_Info.Gapp_Info for a given content type.
   --  @param Content_Type the content type to find a Glib.App_Info.Gapp_Info
   --  for
   --  @param Must_Support_Uris if True, the Glib.App_Info.Gapp_Info is
   --  expected to support URIs
   --  @return Glib.App_Info.Gapp_Info for given Content_Type or null on
   --  error.

   function Get_Default_For_Type_Finish
      (Result : Glib.G_Async_Result) return Gapp_Info;
   pragma Import (C, Get_Default_For_Type_Finish, "g_app_info_get_default_for_type_finish");
   --  Finishes a default Glib.App_Info.Gapp_Info lookup started by
   --  Glib.App_Info.Get_Default_For_Type_Async.
   --  If no Glib.App_Info.Gapp_Info is found, then Error will be set to
   --  G_IO_ERROR_NOT_FOUND.
   --  Since: gtk+ 2.74
   --  @param Result a Glib.G_Async_Result
   --  @return Glib.App_Info.Gapp_Info for given Content_Type or null on
   --  error.

   function Get_Default_For_Uri_Scheme
      (Uri_Scheme : UTF8_String) return Gapp_Info;
   --  Gets the default application for handling URIs with the given URI
   --  scheme. A URI scheme is the initial part of the URI, up to but not
   --  including the ':', e.g. "http", "ftp" or "sip".
   --  @param Uri_Scheme a string containing a URI scheme.
   --  @return Glib.App_Info.Gapp_Info for given Uri_Scheme or null on error.

   function Get_Default_For_Uri_Scheme_Finish
      (Result : Glib.G_Async_Result) return Gapp_Info;
   pragma Import (C, Get_Default_For_Uri_Scheme_Finish, "g_app_info_get_default_for_uri_scheme_finish");
   --  Finishes a default Glib.App_Info.Gapp_Info lookup started by
   --  Glib.App_Info.Get_Default_For_Uri_Scheme_Async.
   --  If no Glib.App_Info.Gapp_Info is found, then Error will be set to
   --  G_IO_ERROR_NOT_FOUND.
   --  Since: gtk+ 2.74
   --  @param Result a Glib.G_Async_Result
   --  @return Glib.App_Info.Gapp_Info for given Uri_Scheme or null on error.

   function Get_Fallback_For_Type
      (Content_Type : UTF8_String) return App_Info_List.Glist;
   --  Gets a list of fallback GApp_Infos for a given content type, i.e. those
   --  applications which claim to support the given content type by MIME type
   --  subclassing and not directly.
   --  Since: gtk+ 2.28
   --  @param Content_Type the content type to find a Glib.App_Info.Gapp_Info
   --  for

   function Get_Recommended_For_Type
      (Content_Type : UTF8_String) return App_Info_List.Glist;
   --  Gets a list of recommended GApp_Infos for a given content type, i.e.
   --  those applications which claim to support the given content type
   --  exactly, and not by MIME type subclassing. Note that the first
   --  application of the list is the last used one, i.e. the last one for
   --  which Glib.App_Info.Set_As_Last_Used_For_Type has been called.
   --  Since: gtk+ 2.28
   --  @param Content_Type the content type to find a Glib.App_Info.Gapp_Info
   --  for

   function Launch_Default_For_Uri
      (URI     : UTF8_String;
       Context : access Glib.App_Launch_Context.Gapp_Launch_Context_Record'Class)
       return Boolean;
   --  Utility function that launches the default application registered to
   --  handle the specified uri. Synchronous I/O is done on the uri to detect
   --  the type of the file if required.
   --  The D-Bus–activated applications don't have to be started if your
   --  application terminates too soon after this function. To prevent this,
   --  use Glib.App_Info.Launch_Default_For_Uri_Async instead.
   --  @param URI the uri to show
   --  @param Context an optional Glib.App_Launch_Context.Gapp_Launch_Context
   --  @return True on success, False on error.

   function Launch_Default_For_Uri_Finish
      (Result : Glib.G_Async_Result) return Boolean;
   --  Finishes an asynchronous launch-default-for-uri operation.
   --  Since: gtk+ 2.50
   --  @param Result a Glib.G_Async_Result
   --  @return True if the launch was successful, False if Error is set

   procedure Reset_Type_Associations (Content_Type : UTF8_String);
   --  Removes all changes to the type associations done by
   --  Glib.App_Info.Set_As_Default_For_Type,
   --  Glib.App_Info.Set_As_Default_For_Extension,
   --  Glib.App_Info.Add_Supports_Type or Glib.App_Info.Remove_Supports_Type.
   --  Since: gtk+ 2.20
   --  @param Content_Type a content type

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gapp_Info"

   function "+" (W : Gapp_Info) return Gapp_Info;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Add_Supports_Type is access function
     (Self         : Gapp_Info;
      Content_Type : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
   pragma Convention (C, Virtual_Add_Supports_Type);
   --  Adds a content type to the application information to indicate the
   --  application is capable of opening files with the given content type.
   --  @param Content_Type a string.
   --  @return True on success, False on error.

   type Virtual_Can_Delete is access function (Self : Gapp_Info) return Glib.Gboolean;
   pragma Convention (C, Virtual_Can_Delete);
   --  Obtains the information whether the Glib.App_Info.Gapp_Info can be
   --  deleted. See Glib.App_Info.Delete.
   --  Since: gtk+ 2.20
   --  @return True if Appinfo can be deleted

   type Virtual_Can_Remove_Supports_Type is access function (Self : Gapp_Info) return Glib.Gboolean;
   pragma Convention (C, Virtual_Can_Remove_Supports_Type);
   --  Checks if a supported content type can be removed from an application.
   --  @return True if it is possible to remove supported content types from a
   --  given Appinfo, False if not.

   type Virtual_Do_Delete is access function (Self : Gapp_Info) return Glib.Gboolean;
   pragma Convention (C, Virtual_Do_Delete);
   --  Tries to delete a Glib.App_Info.Gapp_Info.
   --  On some platforms, there may be a difference between user-defined
   --  GApp_Infos which can be deleted, and system-wide ones which cannot. See
   --  Glib.App_Info.Can_Delete.
   --  Since: gtk+ 2.20
   --  @return True if Appinfo has been deleted

   type Virtual_Dup is access function (Self : Gapp_Info) return Gapp_Info;
   pragma Convention (C, Virtual_Dup);
   --  Creates a duplicate of a Glib.App_Info.Gapp_Info.
   --  @return a duplicate of Appinfo.

   type Virtual_Equal is access function
     (Self     : Gapp_Info;
      Appinfo2 : Gapp_Info) return Glib.Gboolean;
   pragma Convention (C, Virtual_Equal);
   --  Checks if two GApp_Infos are equal.
   --  Note that the check *may not* compare each individual field, and only
   --  does an identity check. In case detecting changes in the contents is
   --  needed, program code must additionally compare relevant fields.
   --  @param Appinfo2 the second Glib.App_Info.Gapp_Info.
   --  @return True if Appinfo1 is equal to Appinfo2. False otherwise.

   type Virtual_Get_Commandline is access function (Self : Gapp_Info) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Commandline);
   --  Gets the commandline with which the application will be started.
   --  Since: gtk+ 2.20
   --  @return a string containing the Appinfo's commandline, or null if this
   --  information is not available

   type Virtual_Get_Description is access function (Self : Gapp_Info) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Description);
   --  Gets a human-readable description of an installed application.
   --  @return a string containing a description of the application Appinfo,
   --  or null if none.

   type Virtual_Get_Display_Name is access function (Self : Gapp_Info) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Display_Name);
   --  Gets the display name of the application. The display name is often
   --  more descriptive to the user than the name itself.
   --  Since: gtk+ 2.24
   --  @return the display name of the application for Appinfo, or the name if
   --  no display name is available.

   type Virtual_Get_Executable is access function (Self : Gapp_Info) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Executable);
   --  Gets the executable's name for the installed application.
   --  This is intended to be used for debugging or labelling what program is
   --  going to be run. To launch the executable, use Glib.App_Info.Launch and
   --  related functions, rather than spawning the return value from this
   --  function.
   --  @return a string containing the Appinfo's application binaries name

   type Virtual_Get_Icon is access function (Self : Gapp_Info) return Glib.G_Icon.G_Icon;
   pragma Convention (C, Virtual_Get_Icon);
   --  Gets the icon for the application.
   --  @return the default Glib.G_Icon.G_Icon for Appinfo or null if there is
   --  no default icon.

   type Virtual_Get_Id is access function (Self : Gapp_Info) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Id);
   --  Gets the ID of an application. An id is a string that identifies the
   --  application. The exact format of the id is platform dependent. For
   --  instance, on Unix this is the desktop file id from the xdg menu
   --  specification.
   --  Note that the returned ID may be null, depending on how the Appinfo has
   --  been constructed.
   --  @return a string containing the application's ID.

   type Virtual_Get_Name is access function (Self : Gapp_Info) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Name);
   --  Gets the installed name of the application.
   --  @return the name of the application for Appinfo.

   type Virtual_Get_Supported_Types is access function (Self : Gapp_Info) return chars_ptr_array_access;
   pragma Convention (C, Virtual_Get_Supported_Types);
   --  Retrieves the list of content types that App_Info claims to support. If
   --  this information is not provided by the environment, this function will
   --  return null. This function does not take in consideration associations
   --  added with Glib.App_Info.Add_Supports_Type, but only those exported
   --  directly by the application.
   --  Since: gtk+ 2.34
   --  @return a list of content types.

   type Virtual_Launch is access function
     (Self    : Gapp_Info;
      Files   : System.Address;
      Context : System.Address) return Glib.Gboolean;
   pragma Convention (C, Virtual_Launch);
   --  Launches the application. Passes Files to the launched application as
   --  arguments, using the optional Context to get information about the
   --  details of the launcher (like what screen it is on). On error, Error
   --  will be set accordingly.
   --  To launch the application without arguments pass a null Files list.
   --  Note that even if the launch is successful the application launched can
   --  fail to start if it runs into problems during startup. There is no way
   --  to detect this.
   --  Some URIs can be changed when passed through a GFile (for instance
   --  unsupported URIs with strange formats like mailto:), so if you have a
   --  textual URI you want to pass in as argument, consider using
   --  Glib.App_Info.Launch_Uris instead.
   --  The launched application inherits the environment of the launching
   --  process, but it can be modified with g_app_launch_context_setenv and
   --  g_app_launch_context_unsetenv.
   --  On UNIX, this function sets the `GIO_LAUNCHED_DESKTOP_FILE` environment
   --  variable with the path of the launched desktop file and
   --  `GIO_LAUNCHED_DESKTOP_FILE_PID` to the process id of the launched
   --  process. This can be used to ignore `GIO_LAUNCHED_DESKTOP_FILE`, should
   --  it be inherited by further processes. The `DISPLAY`,
   --  `XDG_ACTIVATION_TOKEN` and `DESKTOP_STARTUP_ID` environment variables
   --  are also set, based on information provided in Context.
   --  @param Files a GList of Glib.GFile.Gfile objects
   --  @param Context a Glib.App_Launch_Context.Gapp_Launch_Context or null
   --  @return True on successful launch, False otherwise.

   type Virtual_Launch_Uris is access function
     (Self    : Gapp_Info;
      Uris    : System.Address;
      Context : System.Address) return Glib.Gboolean;
   pragma Convention (C, Virtual_Launch_Uris);
   --  Launches the application. This passes the Uris to the launched
   --  application as arguments, using the optional Context to get information
   --  about the details of the launcher (like what screen it is on). On error,
   --  Error will be set accordingly. If the application only supports one URI
   --  per invocation as part of their command-line, multiple instances of the
   --  application will be spawned.
   --  To launch the application without arguments pass a null Uris list.
   --  Note that even if the launch is successful the application launched can
   --  fail to start if it runs into problems during startup. There is no way
   --  to detect this.
   --  @param Uris a GList containing URIs to launch.
   --  @param Context a Glib.App_Launch_Context.Gapp_Launch_Context or null
   --  @return True on successful launch, False otherwise.

   type Virtual_Launch_Uris_Async is access procedure
     (Self        : Gapp_Info;
      Uris        : System.Address;
      Context     : System.Address;
      Cancellable : System.Address;
      Callback    : System.Address;
      User_Data   : System.Address);
   pragma Convention (C, Virtual_Launch_Uris_Async);
   --  Async version of Glib.App_Info.Launch_Uris.
   --  The Callback is invoked immediately after the application launch, but
   --  it waits for activation in case of D-Bus–activated applications and also
   --  provides extended error information for sandboxed applications, see
   --  notes for Glib.App_Info.Launch_Default_For_Uri_Async.
   --  Since: gtk+ 2.60
   --  @param Uris a GList containing URIs to launch.
   --  @param Context a Glib.App_Launch_Context.Gapp_Launch_Context or null
   --  @param Cancellable a Glib.Cancellable.Gcancellable
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  done
   --  @param User_Data data to pass to Callback

   type Virtual_Launch_Uris_Finish is access function
     (Self   : Gapp_Info;
      Result : Glib.G_Async_Result) return Glib.Gboolean;
   pragma Convention (C, Virtual_Launch_Uris_Finish);
   --  Finishes a Glib.App_Info.Launch_Uris_Async operation.
   --  Since: gtk+ 2.60
   --  @param Result a Glib.G_Async_Result
   --  @return True on successful launch, False otherwise.

   type Virtual_Remove_Supports_Type is access function
     (Self         : Gapp_Info;
      Content_Type : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
   pragma Convention (C, Virtual_Remove_Supports_Type);
   --  Removes a supported type from an application, if possible.
   --  @param Content_Type a string.
   --  @return True on success, False on error.

   type Virtual_Set_As_Default_For_Extension is access function
     (Self      : Gapp_Info;
      Extension : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
   pragma Convention (C, Virtual_Set_As_Default_For_Extension);
   --  Sets the application as the default handler for the given file
   --  extension.
   --  @param Extension a string containing the file extension (without the
   --  dot).
   --  @return True on success, False on error.

   type Virtual_Set_As_Default_For_Type is access function
     (Self         : Gapp_Info;
      Content_Type : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
   pragma Convention (C, Virtual_Set_As_Default_For_Type);
   --  Sets the application as the default handler for a given type.
   --  @param Content_Type the content type.
   --  @return True on success, False on error.

   type Virtual_Set_As_Last_Used_For_Type is access function
     (Self         : Gapp_Info;
      Content_Type : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
   pragma Convention (C, Virtual_Set_As_Last_Used_For_Type);
   --  Sets the application as the last used application for a given type.
   --  This will make the application appear as first in the list returned by
   --  Glib.App_Info.Get_Recommended_For_Type, regardless of the default
   --  application for that content type.
   --  @param Content_Type the content type.
   --  @return True on success, False on error.

   type Virtual_Should_Show is access function (Self : Gapp_Info) return Glib.Gboolean;
   pragma Convention (C, Virtual_Should_Show);
   --  Checks if the application info should be shown in menus that list
   --  available applications.
   --  @return True if the Appinfo should be shown, False otherwise.

   type Virtual_Supports_Files is access function (Self : Gapp_Info) return Glib.Gboolean;
   pragma Convention (C, Virtual_Supports_Files);
   --  Checks if the application accepts files as arguments.
   --  @return True if the Appinfo supports files.

   type Virtual_Supports_Uris is access function (Self : Gapp_Info) return Glib.Gboolean;
   pragma Convention (C, Virtual_Supports_Uris);
   --  Checks if the application supports reading files and directories from
   --  URIs.
   --  @return True if the Appinfo supports URIs.

   subtype App_Info_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Add_Supports_Type
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Add_Supports_Type);
   pragma Import (C, Set_Add_Supports_Type, "gtkada_App_Info_set_add_supports_type");

   procedure Set_Can_Delete
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Can_Delete);
   pragma Import (C, Set_Can_Delete, "gtkada_App_Info_set_can_delete");

   procedure Set_Can_Remove_Supports_Type
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Can_Remove_Supports_Type);
   pragma Import (C, Set_Can_Remove_Supports_Type, "gtkada_App_Info_set_can_remove_supports_type");

   procedure Set_Do_Delete
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Do_Delete);
   pragma Import (C, Set_Do_Delete, "gtkada_App_Info_set_do_delete");

   procedure Set_Dup
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Dup);
   pragma Import (C, Set_Dup, "gtkada_App_Info_set_dup");

   procedure Set_Equal
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Equal);
   pragma Import (C, Set_Equal, "gtkada_App_Info_set_equal");

   procedure Set_Get_Commandline
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Get_Commandline);
   pragma Import (C, Set_Get_Commandline, "gtkada_App_Info_set_get_commandline");

   procedure Set_Get_Description
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Get_Description);
   pragma Import (C, Set_Get_Description, "gtkada_App_Info_set_get_description");

   procedure Set_Get_Display_Name
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Get_Display_Name);
   pragma Import (C, Set_Get_Display_Name, "gtkada_App_Info_set_get_display_name");

   procedure Set_Get_Executable
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Get_Executable);
   pragma Import (C, Set_Get_Executable, "gtkada_App_Info_set_get_executable");

   procedure Set_Get_Icon
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Get_Icon);
   pragma Import (C, Set_Get_Icon, "gtkada_App_Info_set_get_icon");

   procedure Set_Get_Id
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Get_Id);
   pragma Import (C, Set_Get_Id, "gtkada_App_Info_set_get_id");

   procedure Set_Get_Name
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Get_Name);
   pragma Import (C, Set_Get_Name, "gtkada_App_Info_set_get_name");

   procedure Set_Get_Supported_Types
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Get_Supported_Types);
   pragma Import (C, Set_Get_Supported_Types, "gtkada_App_Info_set_get_supported_types");

   procedure Set_Launch
     (Self    : App_Info_Interface_Descr;
      Handler : Virtual_Launch);
   pragma Import (C, Set_Launch, "gtkada_App_Info_set_launch");

   procedure Set_Launch_Uris
        (Self    : App_Info_Interface_Descr;
         Handler : Virtual_Launch_Uris);
      pragma Import (C, Set_Launch_Uris, "gtkada_App_Info_set_launch_uris");

      procedure Set_Launch_Uris_Async
        (Self    : App_Info_Interface_Descr;
         Handler : Virtual_Launch_Uris_Async);
      pragma Import (C, Set_Launch_Uris_Async, "gtkada_App_Info_set_launch_uris_async");

      procedure Set_Launch_Uris_Finish
        (Self    : App_Info_Interface_Descr;
         Handler : Virtual_Launch_Uris_Finish);
      pragma Import (C, Set_Launch_Uris_Finish, "gtkada_App_Info_set_launch_uris_finish");

      procedure Set_Remove_Supports_Type
        (Self    : App_Info_Interface_Descr;
         Handler : Virtual_Remove_Supports_Type);
      pragma Import (C, Set_Remove_Supports_Type, "gtkada_App_Info_set_remove_supports_type");

      procedure Set_Set_As_Default_For_Extension
        (Self    : App_Info_Interface_Descr;
         Handler : Virtual_Set_As_Default_For_Extension);
      pragma Import (C, Set_Set_As_Default_For_Extension, "gtkada_App_Info_set_set_as_default_for_extension");

      procedure Set_Set_As_Default_For_Type
        (Self    : App_Info_Interface_Descr;
         Handler : Virtual_Set_As_Default_For_Type);
      pragma Import (C, Set_Set_As_Default_For_Type, "gtkada_App_Info_set_set_as_default_for_type");

      procedure Set_Set_As_Last_Used_For_Type
        (Self    : App_Info_Interface_Descr;
         Handler : Virtual_Set_As_Last_Used_For_Type);
      pragma Import (C, Set_Set_As_Last_Used_For_Type, "gtkada_App_Info_set_set_as_last_used_for_type");

      procedure Set_Should_Show
        (Self    : App_Info_Interface_Descr;
         Handler : Virtual_Should_Show);
      pragma Import (C, Set_Should_Show, "gtkada_App_Info_set_should_show");

      procedure Set_Supports_Files
        (Self    : App_Info_Interface_Descr;
         Handler : Virtual_Supports_Files);
      pragma Import (C, Set_Supports_Files, "gtkada_App_Info_set_supports_files");

      procedure Set_Supports_Uris
           (Self    : App_Info_Interface_Descr;
            Handler : Virtual_Supports_Uris);
         pragma Import (C, Set_Supports_Uris, "gtkada_App_Info_set_supports_uris");
         --  See Glib.Object.Add_Interface

private

   Null_Gapp_Info : constant Gapp_Info :=
      Gapp_Info (Glib.Types.Null_Interface);
end Glib.App_Info;
