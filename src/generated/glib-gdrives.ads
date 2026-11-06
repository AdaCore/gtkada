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

--  Glib.GDrives.Gdrive - this represent a piece of hardware connected to the
--  machine. It's generally only created for removable hardware or hardware
--  with removable media.
--
--  Glib.GDrives.Gdrive is a container class for Glib.GVolumes.Gvolume objects
--  that stem from the same piece of media. As such, Glib.GDrives.Gdrive
--  abstracts a drive with (or without) removable media and provides operations
--  for querying whether media is available, determining whether media change
--  is automatically detected and ejecting the media.
--
--  If the Glib.GDrives.Gdrive reports that media isn't automatically
--  detected, one can poll for media; typically one should not do this
--  periodically as a poll for media operation is potentially expensive and may
--  spin up the drive creating noise.
--
--  Glib.GDrives.Gdrive supports starting and stopping drives with
--  authentication support for the former. This can be used to support a
--  diverse set of use cases including connecting/disconnecting iSCSI devices,
--  powering down external disk enclosures and starting/stopping multi-disk
--  devices such as RAID devices. Note that the actual semantics and
--  side-effects of starting/stopping a Glib.GDrives.Gdrive may vary according
--  to implementation. To choose the correct verbs in e.g. a file manager, use
--  Glib.GDrives.Get_Start_Stop_Type.
--
--  For porting from GnomeVFS note that there is no equivalent of
--  Glib.GDrives.Gdrive in that API.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;          use GNAT.Strings;
with Glib.Cancellable;      use Glib.Cancellable;
with Glib.Error;            use Glib.Error;
with Glib.GEnums;           use Glib.GEnums;
with Glib.GMountOperations; use Glib.GMountOperations;
with Glib.GVolumes;         use Glib.GVolumes;
with Glib.G_Icon;           use Glib.G_Icon;
with Glib.Object;           use Glib.Object;
with Glib.Types;            use Glib.Types;
with Gtkada.Types;          use Gtkada.Types;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;       use Gtkada.Bindings;
pragma Warnings(On);

package Glib.GDrives is

   type Gdrive is new Glib.Types.GType_Interface;
   Null_Gdrive : constant Gdrive;

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

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_drive_get_type");

   -------------
   -- Methods --
   -------------

   function Can_Eject (Self : Gdrive) return Boolean;
   --  Checks if a drive can be ejected.
   --  @return True if the Drive can be ejected, False otherwise.

   function Can_Poll_For_Media (Self : Gdrive) return Boolean;
   --  Checks if a drive can be polled for media changes.
   --  @return True if the Drive can be polled for media changes, False
   --  otherwise.

   function Can_Start (Self : Gdrive) return Boolean;
   --  Checks if a drive can be started.
   --  Since: gtk+ 2.22
   --  @return True if the Drive can be started, False otherwise.

   function Can_Start_Degraded (Self : Gdrive) return Boolean;
   --  Checks if a drive can be started degraded.
   --  Since: gtk+ 2.22
   --  @return True if the Drive can be started degraded, False otherwise.

   function Can_Stop (Self : Gdrive) return Boolean;
   --  Checks if a drive can be stopped.
   --  Since: gtk+ 2.22
   --  @return True if the Drive can be stopped, False otherwise.

   procedure Eject_With_Operation
      (Self            : Gdrive;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback);
   --  Ejects a drive. This is an asynchronous operation, and is finished by
   --  calling Glib.GDrives.Eject_With_Operation_Finish with the Drive and
   --  Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the unmount if required for eject
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.

   function Eject_With_Operation_Finish
      (Self   : Gdrive;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes ejecting a drive. If any errors occurred during the operation,
   --  Error will be set to contain the errors and False will be returned.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the drive was successfully ejected. False otherwise.

   function Enumerate_Identifiers
      (Self : Gdrive) return GNAT.Strings.String_List;
   --  Gets the kinds of identifiers that Drive has. Use
   --  Glib.GDrives.Get_Identifier to obtain the identifiers themselves.
   --  @return a null-terminated array of strings containing kinds of
   --  identifiers. Use g_strfreev to free.

   function Get_Icon (Self : Gdrive) return Glib.G_Icon.G_Icon;
   pragma Import (C, Get_Icon, "g_drive_get_icon");
   --  Gets the icon for Drive.
   --  @return Glib.G_Icon.G_Icon for the Drive. Free the returned object with
   --  g_object_unref.

   function Get_Identifier
      (Self : Gdrive;
       Kind : UTF8_String) return UTF8_String;
   --  Gets the identifier of the given kind for Drive. The only identifier
   --  currently available is G_DRIVE_IDENTIFIER_KIND_UNIX_DEVICE.
   --  @param Kind the kind of identifier to return
   --  @return a newly allocated string containing the requested identifier,
   --  or null if the Glib.GDrives.Gdrive doesn't have this kind of identifier.

   function Get_Name (Self : Gdrive) return UTF8_String;
   --  Gets the name of Drive.
   --  @return a string containing Drive's name. The returned string should be
   --  freed when no longer needed.

   function Get_Sort_Key (Self : Gdrive) return UTF8_String;
   --  Gets the sort key for Drive, if any.
   --  Since: gtk+ 2.32
   --  @return Sorting key for Drive or null if no such key is available.

   function Get_Start_Stop_Type
      (Self : Gdrive) return Glib.GEnums.GDrive_Start_Stop_Type;
   pragma Import (C, Get_Start_Stop_Type, "g_drive_get_start_stop_type");
   --  Gets a hint about how a drive can be started/stopped.
   --  Since: gtk+ 2.22
   --  @return A value from the Glib.GEnums.GDrive_Start_Stop_Type
   --  enumeration.

   function Get_Symbolic_Icon (Self : Gdrive) return Glib.G_Icon.G_Icon;
   pragma Import (C, Get_Symbolic_Icon, "g_drive_get_symbolic_icon");
   --  Gets the icon for Drive.
   --  Since: gtk+ 2.34
   --  @return symbolic Glib.G_Icon.G_Icon for the Drive. Free the returned
   --  object with g_object_unref.

   function Get_Volumes (Self : Gdrive) return Glib.GVolumes.GVolume_GList;
   --  Get a list of mountable volumes for Drive.
   --  The returned list should be freed with g_list_free, after its elements
   --  have been unreffed with g_object_unref.
   --  @return GList containing any Glib.GVolumes.Gvolume objects on the given
   --  Drive.

   function Has_Media (Self : Gdrive) return Boolean;
   --  Checks if the Drive has media. Note that the OS may not be polling the
   --  drive for media changes; see Glib.GDrives.Is_Media_Check_Automatic for
   --  more details.
   --  @return True if Drive has media, False otherwise.

   function Has_Volumes (Self : Gdrive) return Boolean;
   --  Check if Drive has any mountable volumes.
   --  @return True if the Drive contains volumes, False otherwise.

   function Is_Media_Check_Automatic (Self : Gdrive) return Boolean;
   --  Checks if Drive is capable of automatically detecting media changes.
   --  @return True if the Drive is capable of automatically detecting media
   --  changes, False otherwise.

   function Is_Media_Removable (Self : Gdrive) return Boolean;
   --  Checks if the Drive supports removable media.
   --  @return True if Drive supports removable media, False otherwise.

   function Is_Removable (Self : Gdrive) return Boolean;
   --  Checks if the Glib.GDrives.Gdrive and/or its media is considered
   --  removable by the user. See Glib.GDrives.Is_Media_Removable.
   --  Since: gtk+ 2.50
   --  @return True if Drive and/or its media is considered removable, False
   --  otherwise.

   function Poll_For_Media_Finish
      (Self   : Gdrive;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes an operation started with g_drive_poll_for_media on a drive.
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the drive has been poll_for_mediaed successfully, False
   --  otherwise.

   procedure Start
      (Self            : Gdrive;
       Flags           : Glib.GEnums.GDrive_Start_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback);
   --  Asynchronously starts a drive.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GDrives.Start_Finish to obtain the result of the operation.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the start operation.
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.

   function Start_Finish
      (Self   : Gdrive;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes starting a drive.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the drive has been started successfully, False
   --  otherwise.

   procedure Stop
      (Self            : Gdrive;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback);
   --  Asynchronously stops a drive.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GDrives.Stop_Finish to obtain the result of the operation.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the unmount if required for stopping.
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.

   function Stop_Finish
      (Self   : Gdrive;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes stopping a drive.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the drive has been stopped successfully, False
   --  otherwise.

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Poll_For_Media
     (Self        : Gdrive;
      Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
      Callback    : Gasync_Ready_Callback);
   --  Asynchronously polls Drive to see if media has been inserted or
   --  removed.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GDrives.Poll_For_Media_Finish to obtain the result of the
   --  operation.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.

   generic
   type User_Data_Type (<>) is private;
   with procedure Destroy (Data : in out User_Data_Type) is null;
   package Poll_For_Media_User_Data is

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

      procedure Poll_For_Media
        (Self        : Glib.GDrives.Gdrive;
         Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
         Callback    : Gasync_Ready_Callback;
         User_Data   : User_Data_Type);
      --  Asynchronously polls Drive to see if media has been inserted or
      --  removed.
      --  When the operation is finished, Callback will be called. You can
      --  then call Glib.GDrives.Poll_For_Media_Finish to obtain the result of
      --  the operation.
      --  @param Cancellable optional Glib.Cancellable.Gcancellable object,
      --  null to ignore.
      --  @param Callback a Gasync_Ready_Callback, or null.
      --  @param User_Data user data to pass to Callback

   end Poll_For_Media_User_Data;

   -------------
   -- Signals --
   -------------

   type Cb_Gdrive_Void is not null access procedure (Self : Gdrive);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : Gdrive;
       Call  : Cb_Gdrive_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : Gdrive;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the drive's state has changed.

   Signal_Disconnected : constant Glib.Signal_Name := "disconnected";
   procedure On_Disconnected
      (Self  : Gdrive;
       Call  : Cb_Gdrive_Void;
       After : Boolean := False);
   procedure On_Disconnected
      (Self  : Gdrive;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the Glib.GDrives.Gdrive have been
   --  disconnected. If the recipient is holding references to the object they
   --  should release them so the object can be finalized.

   Signal_Eject_Button : constant Glib.Signal_Name := "eject-button";
   procedure On_Eject_Button
      (Self  : Gdrive;
       Call  : Cb_Gdrive_Void;
       After : Boolean := False);
   procedure On_Eject_Button
      (Self  : Gdrive;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the physical eject button (if any) of a drive has been
   --  pressed.

   Signal_Stop_Button : constant Glib.Signal_Name := "stop-button";
   procedure On_Stop_Button
      (Self  : Gdrive;
       Call  : Cb_Gdrive_Void;
       After : Boolean := False);
   procedure On_Stop_Button
      (Self  : Gdrive;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the physical stop button (if any) of a drive has been
   --  pressed.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gdrive"

   function "+" (W : Gdrive) return Gdrive;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Can_Eject is access function (Self : Gdrive) return Glib.Gboolean;
   pragma Convention (C, Virtual_Can_Eject);
   --  Checks if a drive can be ejected.
   --  @return True if the Drive can be ejected, False otherwise.

   type Virtual_Can_Poll_For_Media is access function (Self : Gdrive) return Glib.Gboolean;
   pragma Convention (C, Virtual_Can_Poll_For_Media);
   --  Checks if a drive can be polled for media changes.
   --  @return True if the Drive can be polled for media changes, False
   --  otherwise.

   type Virtual_Can_Start is access function (Self : Gdrive) return Glib.Gboolean;
   pragma Convention (C, Virtual_Can_Start);
   --  Checks if a drive can be started.
   --  Since: gtk+ 2.22
   --  @return True if the Drive can be started, False otherwise.

   type Virtual_Can_Start_Degraded is access function (Self : Gdrive) return Glib.Gboolean;
   pragma Convention (C, Virtual_Can_Start_Degraded);
   --  Checks if a drive can be started degraded.
   --  Since: gtk+ 2.22
   --  @return True if the Drive can be started degraded, False otherwise.

   type Virtual_Can_Stop is access function (Self : Gdrive) return Glib.Gboolean;
   pragma Convention (C, Virtual_Can_Stop);
   --  Checks if a drive can be stopped.
   --  Since: gtk+ 2.22
   --  @return True if the Drive can be stopped, False otherwise.

   type Virtual_Changed is access procedure (Self : Gdrive);
   pragma Convention (C, Virtual_Changed);
   --  Signal emitted when the drive is changed.

   type Virtual_Disconnected is access procedure (Self : Gdrive);
   pragma Convention (C, Virtual_Disconnected);
   --  The removed signal that is emitted when the Glib.GDrives.Gdrive have
   --  been disconnected. If the recipient is holding references to the object
   --  they should release them so the object can be finalized.

   type Virtual_Eject_Button is access procedure (Self : Gdrive);
   pragma Convention (C, Virtual_Eject_Button);
   --  Signal emitted when the physical eject button (if any) of a drive have
   --  been pressed.

   type Virtual_Eject_With_Operation is access procedure
     (Self            : Gdrive;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Virtual_Eject_With_Operation);
   --  Ejects a drive. This is an asynchronous operation, and is finished by
   --  calling Glib.GDrives.Eject_With_Operation_Finish with the Drive and
   --  Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the unmount if required for eject
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.
   --  @param User_Data user data passed to Callback.

   type Virtual_Eject_With_Operation_Finish is access function
     (Self   : Gdrive;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Virtual_Eject_With_Operation_Finish);
   --  Finishes ejecting a drive. If any errors occurred during the operation,
   --  Error will be set to contain the errors and False will be returned.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the drive was successfully ejected. False otherwise.

   type Virtual_Enumerate_Identifiers is access function (Self : Gdrive) return chars_ptr_array_access;
   pragma Convention (C, Virtual_Enumerate_Identifiers);
   --  Gets the kinds of identifiers that Drive has. Use
   --  Glib.GDrives.Get_Identifier to obtain the identifiers themselves.
   --  @return a null-terminated array of strings containing kinds of
   --  identifiers. Use g_strfreev to free.

   type Virtual_Get_Icon is access function (Self : Gdrive) return Glib.G_Icon.G_Icon;
   pragma Convention (C, Virtual_Get_Icon);
   --  Gets the icon for Drive.
   --  @return Glib.G_Icon.G_Icon for the Drive. Free the returned object with
   --  g_object_unref.

   type Virtual_Get_Identifier is access function
     (Self : Gdrive;
      Kind : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Identifier);
   --  Gets the identifier of the given kind for Drive. The only identifier
   --  currently available is G_DRIVE_IDENTIFIER_KIND_UNIX_DEVICE.
   --  @param Kind the kind of identifier to return
   --  @return a newly allocated string containing the requested identifier,
   --  or null if the Glib.GDrives.Gdrive doesn't have this kind of identifier.

   type Virtual_Get_Name is access function (Self : Gdrive) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Name);
   --  Gets the name of Drive.
   --  @return a string containing Drive's name. The returned string should be
   --  freed when no longer needed.

   type Virtual_Get_Sort_Key is access function (Self : Gdrive) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Sort_Key);
   --  Gets the sort key for Drive, if any.
   --  Since: gtk+ 2.32
   --  @return Sorting key for Drive or null if no such key is available.

   type Virtual_Get_Start_Stop_Type is access function (Self : Gdrive) return Glib.GEnums.GDrive_Start_Stop_Type;
   pragma Convention (C, Virtual_Get_Start_Stop_Type);
   --  Gets a hint about how a drive can be started/stopped.
   --  Since: gtk+ 2.22
   --  @return A value from the Glib.GEnums.GDrive_Start_Stop_Type
   --  enumeration.

   type Virtual_Get_Symbolic_Icon is access function (Self : Gdrive) return Glib.G_Icon.G_Icon;
   pragma Convention (C, Virtual_Get_Symbolic_Icon);
   --  Gets the icon for Drive.
   --  Since: gtk+ 2.34
   --  @return symbolic Glib.G_Icon.G_Icon for the Drive. Free the returned
   --  object with g_object_unref.

   type Virtual_Get_Volumes is access function (Self : Gdrive) return System.Address;
   pragma Convention (C, Virtual_Get_Volumes);
   --  Get a list of mountable volumes for Drive.
   --  The returned list should be freed with g_list_free, after its elements
   --  have been unreffed with g_object_unref.
   --  @return GList containing any Glib.GVolumes.Gvolume objects on the given
   --  Drive.

   type Virtual_Has_Media is access function (Self : Gdrive) return Glib.Gboolean;
   pragma Convention (C, Virtual_Has_Media);
   --  Checks if the Drive has media. Note that the OS may not be polling the
   --  drive for media changes; see Glib.GDrives.Is_Media_Check_Automatic for
   --  more details.
   --  @return True if Drive has media, False otherwise.

   type Virtual_Has_Volumes is access function (Self : Gdrive) return Glib.Gboolean;
   pragma Convention (C, Virtual_Has_Volumes);
   --  Check if Drive has any mountable volumes.
   --  @return True if the Drive contains volumes, False otherwise.

   type Virtual_Is_Media_Check_Automatic is access function (Self : Gdrive) return Glib.Gboolean;
   pragma Convention (C, Virtual_Is_Media_Check_Automatic);
   --  Checks if Drive is capable of automatically detecting media changes.
   --  @return True if the Drive is capable of automatically detecting media
   --  changes, False otherwise.

   type Virtual_Is_Media_Removable is access function (Self : Gdrive) return Glib.Gboolean;
   pragma Convention (C, Virtual_Is_Media_Removable);
   --  Checks if the Drive supports removable media.
   --  @return True if Drive supports removable media, False otherwise.

   type Virtual_Is_Removable is access function (Self : Gdrive) return Glib.Gboolean;
   pragma Convention (C, Virtual_Is_Removable);
   --  Checks if the Glib.GDrives.Gdrive and/or its media is considered
   --  removable by the user. See Glib.GDrives.Is_Media_Removable.
   --  Since: gtk+ 2.50
   --  @return True if Drive and/or its media is considered removable, False
   --  otherwise.

   type Virtual_Poll_For_Media is access procedure
     (Self        : Gdrive;
      Cancellable : System.Address;
      Callback    : System.Address;
      User_Data   : System.Address);
   pragma Convention (C, Virtual_Poll_For_Media);
   --  Asynchronously polls Drive to see if media has been inserted or
   --  removed.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GDrives.Poll_For_Media_Finish to obtain the result of the
   --  operation.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.
   --  @param User_Data user data to pass to Callback

   type Virtual_Poll_For_Media_Finish is access function
     (Self   : Gdrive;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Virtual_Poll_For_Media_Finish);
   --  Finishes an operation started with g_drive_poll_for_media on a drive.
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the drive has been poll_for_mediaed successfully, False
   --  otherwise.

   type Virtual_Start is access procedure
     (Self            : Gdrive;
      Flags           : Glib.GEnums.GDrive_Start_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Virtual_Start);
   --  Asynchronously starts a drive.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GDrives.Start_Finish to obtain the result of the operation.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the start operation.
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.
   --  @param User_Data user data to pass to Callback

   type Virtual_Start_Finish is access function
     (Self   : Gdrive;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Virtual_Start_Finish);
   --  Finishes starting a drive.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the drive has been started successfully, False
   --  otherwise.

   type Virtual_Stop is access procedure
     (Self            : Gdrive;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Virtual_Stop);
   --  Asynchronously stops a drive.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GDrives.Stop_Finish to obtain the result of the operation.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the unmount if required for stopping.
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.
   --  @param User_Data user data to pass to Callback

   type Virtual_Stop_Button is access procedure (Self : Gdrive);
   pragma Convention (C, Virtual_Stop_Button);
   --  Signal emitted when the physical stop button (if any) of a drive have
   --  been pressed. Since 2.22.

   type Virtual_Stop_Finish is access function
     (Self   : Gdrive;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Virtual_Stop_Finish);
   --  Finishes stopping a drive.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the drive has been stopped successfully, False
   --  otherwise.

   subtype GDrives_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Can_Eject
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Can_Eject);
   pragma Import (C, Set_Can_Eject, "gtkada_GDrives_set_can_eject");

   procedure Set_Can_Poll_For_Media
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Can_Poll_For_Media);
   pragma Import (C, Set_Can_Poll_For_Media, "gtkada_GDrives_set_can_poll_for_media");

   procedure Set_Can_Start
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Can_Start);
   pragma Import (C, Set_Can_Start, "gtkada_GDrives_set_can_start");

   procedure Set_Can_Start_Degraded
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Can_Start_Degraded);
   pragma Import (C, Set_Can_Start_Degraded, "gtkada_GDrives_set_can_start_degraded");

   procedure Set_Can_Stop
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Can_Stop);
   pragma Import (C, Set_Can_Stop, "gtkada_GDrives_set_can_stop");

   procedure Set_Changed
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Changed);
   pragma Import (C, Set_Changed, "gtkada_GDrives_set_changed");

   procedure Set_Disconnected
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Disconnected);
   pragma Import (C, Set_Disconnected, "gtkada_GDrives_set_disconnected");

   procedure Set_Eject_Button
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Eject_Button);
   pragma Import (C, Set_Eject_Button, "gtkada_GDrives_set_eject_button");

   procedure Set_Eject_With_Operation
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Eject_With_Operation);
   pragma Import (C, Set_Eject_With_Operation, "gtkada_GDrives_set_eject_with_operation");

   procedure Set_Eject_With_Operation_Finish
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Eject_With_Operation_Finish);
   pragma Import (C, Set_Eject_With_Operation_Finish, "gtkada_GDrives_set_eject_with_operation_finish");

   procedure Set_Enumerate_Identifiers
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Enumerate_Identifiers);
   pragma Import (C, Set_Enumerate_Identifiers, "gtkada_GDrives_set_enumerate_identifiers");

   procedure Set_Get_Icon
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Get_Icon);
   pragma Import (C, Set_Get_Icon, "gtkada_GDrives_set_get_icon");

   procedure Set_Get_Identifier
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Get_Identifier);
   pragma Import (C, Set_Get_Identifier, "gtkada_GDrives_set_get_identifier");

   procedure Set_Get_Name
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Get_Name);
   pragma Import (C, Set_Get_Name, "gtkada_GDrives_set_get_name");

   procedure Set_Get_Sort_Key
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Get_Sort_Key);
   pragma Import (C, Set_Get_Sort_Key, "gtkada_GDrives_set_get_sort_key");

   procedure Set_Get_Start_Stop_Type
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Get_Start_Stop_Type);
   pragma Import (C, Set_Get_Start_Stop_Type, "gtkada_GDrives_set_get_start_stop_type");

   procedure Set_Get_Symbolic_Icon
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Get_Symbolic_Icon);
   pragma Import (C, Set_Get_Symbolic_Icon, "gtkada_GDrives_set_get_symbolic_icon");

   procedure Set_Get_Volumes
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Get_Volumes);
   pragma Import (C, Set_Get_Volumes, "gtkada_GDrives_set_get_volumes");

   procedure Set_Has_Media
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Has_Media);
   pragma Import (C, Set_Has_Media, "gtkada_GDrives_set_has_media");

   procedure Set_Has_Volumes
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Has_Volumes);
   pragma Import (C, Set_Has_Volumes, "gtkada_GDrives_set_has_volumes");

   procedure Set_Is_Media_Check_Automatic
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Is_Media_Check_Automatic);
   pragma Import (C, Set_Is_Media_Check_Automatic, "gtkada_GDrives_set_is_media_check_automatic");

   procedure Set_Is_Media_Removable
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Is_Media_Removable);
   pragma Import (C, Set_Is_Media_Removable, "gtkada_GDrives_set_is_media_removable");

   procedure Set_Is_Removable
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Is_Removable);
   pragma Import (C, Set_Is_Removable, "gtkada_GDrives_set_is_removable");

   procedure Set_Poll_For_Media
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Poll_For_Media);
   pragma Import (C, Set_Poll_For_Media, "gtkada_GDrives_set_poll_for_media");

   procedure Set_Poll_For_Media_Finish
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Poll_For_Media_Finish);
   pragma Import (C, Set_Poll_For_Media_Finish, "gtkada_GDrives_set_poll_for_media_finish");

   procedure Set_Start
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Start);
   pragma Import (C, Set_Start, "gtkada_GDrives_set_start");

   procedure Set_Start_Finish
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Start_Finish);
   pragma Import (C, Set_Start_Finish, "gtkada_GDrives_set_start_finish");

   procedure Set_Stop
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Stop);
   pragma Import (C, Set_Stop, "gtkada_GDrives_set_stop");

   procedure Set_Stop_Button
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Stop_Button);
   pragma Import (C, Set_Stop_Button, "gtkada_GDrives_set_stop_button");

   procedure Set_Stop_Finish
     (Self    : GDrives_Interface_Descr;
      Handler : Virtual_Stop_Finish);
   pragma Import (C, Set_Stop_Finish, "gtkada_GDrives_set_stop_finish");
   --  See Glib.Object.Add_Interface

private

   Null_Gdrive : constant Gdrive :=
      Gdrive (Glib.Types.Null_Interface);
end Glib.GDrives;
