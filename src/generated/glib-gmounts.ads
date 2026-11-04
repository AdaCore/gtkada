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

--  The Glib.GMounts.Gmount interface represents user-visible mounts. Note,
--  when porting from GnomeVFS, Glib.GMounts.Gmount is the moral equivalent of
--  Gnome_VFSVolume.
--
--  Glib.GMounts.Gmount is a "mounted" filesystem that you can access. Mounted
--  is in quotes because it's not the same as a unix mount, it might be a gvfs
--  mount, but you can still access the files on it if you use GIO. Might or
--  might not be related to a volume object.
--
--  Unmounting a Glib.GMounts.Gmount instance is an asynchronous operation.
--  For more information about asynchronous operations, see Glib.G_Async_Result
--  and Gtask.Gtask. To unmount a Glib.GMounts.Gmount instance, first call
--  Glib.GMounts.Unmount_With_Operation with (at least) the Glib.GMounts.Gmount
--  instance and a Gasync_Ready_Callback. The callback will be fired when the
--  operation has resolved (either with success or failure), and a
--  Glib.G_Async_Result structure will be passed to the callback. That callback
--  should then call Glib.GMounts.Unmount_With_Operation_Finish with the
--  Glib.GMounts.Gmount and the Glib.G_Async_Result data to see if the
--  operation was completed successfully. If an Error is present when
--  Glib.GMounts.Unmount_With_Operation_Finish is called, then it will be
--  filled with any error information.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;          use GNAT.Strings;
with Glib.Cancellable;      use Glib.Cancellable;
with Glib.Error;            use Glib.Error;
limited with Glib.GDrives;
with Glib.GEnums;           use Glib.GEnums;
with Glib.GFile;            use Glib.GFile;
with Glib.GMountOperations; use Glib.GMountOperations;
limited with Glib.GVolumes;
with Glib.G_Icon;           use Glib.G_Icon;
with Glib.Object;           use Glib.Object;
with Glib.Types;            use Glib.Types;
with Gtkada.Types;          use Gtkada.Types;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;       use Gtkada.Bindings;
pragma Warnings(On);

package Glib.GMounts is

   type Gmount is new Glib.Types.GType_Interface;
   Null_Gmount : constant Gmount;

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
   pragma Import (C, Get_Type, "g_mount_get_type");

   -------------
   -- Methods --
   -------------

   function Can_Eject (Self : Gmount) return Boolean;
   --  Checks if Mount can be ejected.
   --  @return True if the Mount can be ejected.

   function Can_Unmount (Self : Gmount) return Boolean;
   --  Checks if Mount can be unmounted.
   --  @return True if the Mount can be unmounted.

   procedure Eject_With_Operation
      (Self            : Gmount;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback);
   --  Ejects a mount. This is an asynchronous operation, and is finished by
   --  calling Glib.GMounts.Eject_With_Operation_Finish with the Mount and
   --  Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the unmount if required for eject
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.

   function Eject_With_Operation_Finish
      (Self   : Gmount;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes ejecting a mount. If any errors occurred during the operation,
   --  Error will be set to contain the errors and False will be returned.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the mount was successfully ejected. False otherwise.

   function Get_Default_Location (Self : Gmount) return Glib.GFile.Gfile;
   pragma Import (C, Get_Default_Location, "g_mount_get_default_location");
   --  Gets the default location of Mount. The default location of the given
   --  Mount is a path that reflects the main entry point for the user (e.g.
   --  the home directory, or the root of the volume).
   --  @return a Glib.GFile.Gfile. The returned object should be unreffed with
   --  g_object_unref when no longer needed.

   function Get_Drive (Self : Gmount) return Glib.GDrives.Gdrive;
   pragma Import (C, Get_Drive, "g_mount_get_drive");
   --  Gets the drive for the Mount.
   --  This is a convenience method for getting the Glib.GVolumes.Gvolume and
   --  then using that object to get the Glib.GDrives.Gdrive.
   --  @return a Glib.GDrives.Gdrive or null if Mount is not associated with a
   --  volume or a drive. The returned object should be unreffed with
   --  g_object_unref when no longer needed.

   function Get_Icon (Self : Gmount) return Glib.G_Icon.G_Icon;
   pragma Import (C, Get_Icon, "g_mount_get_icon");
   --  Gets the icon for Mount.
   --  @return a Glib.G_Icon.G_Icon. The returned object should be unreffed
   --  with g_object_unref when no longer needed.

   function Get_Name (Self : Gmount) return UTF8_String;
   --  Gets the name of Mount.
   --  @return the name for the given Mount. The returned string should be
   --  freed with g_free when no longer needed.

   function Get_Root (Self : Gmount) return Glib.GFile.Gfile;
   pragma Import (C, Get_Root, "g_mount_get_root");
   --  Gets the root directory on Mount.
   --  @return a Glib.GFile.Gfile. The returned object should be unreffed with
   --  g_object_unref when no longer needed.

   function Get_Sort_Key (Self : Gmount) return UTF8_String;
   --  Gets the sort key for Mount, if any.
   --  Since: gtk+ 2.32
   --  @return Sorting key for Mount or null if no such key is available.

   function Get_Symbolic_Icon (Self : Gmount) return Glib.G_Icon.G_Icon;
   pragma Import (C, Get_Symbolic_Icon, "g_mount_get_symbolic_icon");
   --  Gets the symbolic icon for Mount.
   --  Since: gtk+ 2.34
   --  @return a Glib.G_Icon.G_Icon. The returned object should be unreffed
   --  with g_object_unref when no longer needed.

   function Get_Uuid (Self : Gmount) return UTF8_String;
   --  Gets the UUID for the Mount. The reference is typically based on the
   --  file system UUID for the mount in question and should be considered an
   --  opaque string. Returns null if there is no UUID available.
   --  @return the UUID for Mount or null if no UUID can be computed. The
   --  returned string should be freed with g_free when no longer needed.

   function Get_Volume (Self : Gmount) return Glib.GVolumes.Gvolume;
   pragma Import (C, Get_Volume, "g_mount_get_volume");
   --  Gets the volume for the Mount.
   --  @return a Glib.GVolumes.Gvolume or null if Mount is not associated with
   --  a volume. The returned object should be unreffed with g_object_unref
   --  when no longer needed.

   procedure Guess_Content_Type
      (Self         : Gmount;
       Force_Rescan : Boolean;
       Cancellable  : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback     : Gasync_Ready_Callback);
   --  Tries to guess the type of content stored on Mount. Returns one or more
   --  textual identifiers of well-known content types (typically prefixed with
   --  "x-content/"), e.g. x-content/image-dcf for camera memory cards. See the
   --  [shared-mime-info](http://www.freedesktop.org/wiki/Specifications/shared-mime-info-spec)
   --  specification for more on x-content types.
   --  This is an asynchronous operation (see
   --  Glib.GMounts.Guess_Content_Type_Sync for the synchronous version), and
   --  is finished by calling Glib.GMounts.Guess_Content_Type_Finish with the
   --  Mount and Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.18
   --  @param Force_Rescan Whether to force a rescan of the content. Otherwise
   --  a cached result will be used if available
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback

   function Guess_Content_Type_Finish
      (Self   : Gmount;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return GNAT.Strings.String_List;
   --  Finishes guessing content types of Mount. If any errors occurred during
   --  the operation, Error will be set to contain the errors and False will be
   --  returned. In particular, you may get an G_IO_ERROR_NOT_SUPPORTED if the
   --  mount does not support content guessing.
   --  Since: gtk+ 2.18
   --  @param Result a Glib.G_Async_Result
   --  @param Error the return location for a recoverable error
   --  @return a null-terminated array of content types or null on error.
   --  Caller should free this array with g_strfreev when done with it.

   function Guess_Content_Type_Sync
      (Self         : Gmount;
       Force_Rescan : Boolean;
       Cancellable  : access Glib.Cancellable.Gcancellable_Record'Class;
       Error        : out Glib.Error.GError) return GNAT.Strings.String_List;
   --  Tries to guess the type of content stored on Mount. Returns one or more
   --  textual identifiers of well-known content types (typically prefixed with
   --  "x-content/"), e.g. x-content/image-dcf for camera memory cards. See the
   --  [shared-mime-info](http://www.freedesktop.org/wiki/Specifications/shared-mime-info-spec)
   --  specification for more on x-content types.
   --  This is a synchronous operation and as such may block doing IO; see
   --  Glib.GMounts.Guess_Content_Type for the asynchronous version.
   --  Since: gtk+ 2.18
   --  @param Force_Rescan Whether to force a rescan of the content. Otherwise
   --  a cached result will be used if available
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Error the return location for a recoverable error
   --  @return a null-terminated array of content types or null on error.
   --  Caller should free this array with g_strfreev when done with it.

   function Is_Shadowed (Self : Gmount) return Boolean;
   --  Determines if Mount is shadowed. Applications or libraries should avoid
   --  displaying Mount in the user interface if it is shadowed.
   --  A mount is said to be shadowed if there exists one or more user visible
   --  objects (currently Glib.GMounts.Gmount objects) with a root that is
   --  inside the root of Mount.
   --  One application of shadow mounts is when exposing a single file system
   --  that is used to address several logical volumes. In this situation, a
   --  Gvolume.Monitor.Gvolume_Monitor implementation would create two
   --  Glib.GVolumes.Gvolume objects (for example, one for the camera
   --  functionality of the device and one for a SD card reader on the device)
   --  with activation URIs `gphoto2://[usb:001,002]/store1/` and
   --  `gphoto2://[usb:001,002]/store2/`. When the underlying mount (with root
   --  `gphoto2://[usb:001,002]/`) is mounted, said
   --  Gvolume.Monitor.Gvolume_Monitor implementation would create two
   --  Glib.GMounts.Gmount objects (each with their root matching the
   --  corresponding volume activation root) that would shadow the original
   --  mount.
   --  The proxy monitor in GVfs 2.26 and later, automatically creates and
   --  manage shadow mounts (and shadows the underlying mount) if the
   --  activation root on a Glib.GVolumes.Gvolume is set.
   --  Since: gtk+ 2.20
   --  @return True if Mount is shadowed.

   procedure Remount
      (Self            : Gmount;
       Flags           : Glib.GEnums.GMount_Mount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback);
   --  Remounts a mount. This is an asynchronous operation, and is finished by
   --  calling Glib.GMounts.Remount_Finish with the Mount and GAsync_Results
   --  data returned in the Callback.
   --  Remounting is useful when some setting affecting the operation of the
   --  volume has been changed, as these may need a remount to take affect.
   --  While this is semantically equivalent with unmounting and then
   --  remounting not all backends might need to actually be unmounted.
   --  @param Flags flags affecting the operation
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.

   function Remount_Finish
      (Self   : Gmount;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes remounting a mount. If any errors occurred during the
   --  operation, Error will be set to contain the errors and False will be
   --  returned.
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the mount was successfully remounted. False otherwise.

   procedure Shadow (Self : Gmount);
   pragma Import (C, Shadow, "g_mount_shadow");
   --  Increments the shadow count on Mount. Usually used by
   --  Gvolume.Monitor.Gvolume_Monitor implementations when creating a shadow
   --  mount for Mount, see Glib.GMounts.Is_Shadowed for more information. The
   --  caller will need to emit the Glib.GMounts.Gmount::changed signal on
   --  Mount manually.
   --  Since: gtk+ 2.20

   procedure Unmount_With_Operation
      (Self            : Gmount;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback);
   --  Unmounts a mount. This is an asynchronous operation, and is finished by
   --  calling Glib.GMounts.Unmount_With_Operation_Finish with the Mount and
   --  Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the operation
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.

   function Unmount_With_Operation_Finish
      (Self   : Gmount;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes unmounting a mount. If any errors occurred during the
   --  operation, Error will be set to contain the errors and False will be
   --  returned.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the mount was successfully unmounted. False otherwise.

   procedure Unshadow (Self : Gmount);
   pragma Import (C, Unshadow, "g_mount_unshadow");
   --  Decrements the shadow count on Mount. Usually used by
   --  Gvolume.Monitor.Gvolume_Monitor implementations when destroying a shadow
   --  mount for Mount, see Glib.GMounts.Is_Shadowed for more information. The
   --  caller will need to emit the Glib.GMounts.Gmount::changed signal on
   --  Mount manually.
   --  Since: gtk+ 2.20

   -------------
   -- Signals --
   -------------

   type Cb_Gmount_Void is not null access procedure (Self : Gmount);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : Gmount;
       Call  : Cb_Gmount_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : Gmount;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the mount has been changed.

   Signal_Pre_Unmount : constant Glib.Signal_Name := "pre-unmount";
   procedure On_Pre_Unmount
      (Self  : Gmount;
       Call  : Cb_Gmount_Void;
       After : Boolean := False);
   procedure On_Pre_Unmount
      (Self  : Gmount;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal may be emitted when the Glib.GMounts.Gmount is about to be
   --  unmounted.
   --
   --  This signal depends on the backend and is only emitted if GIO was used
   --  to unmount.

   Signal_Unmounted : constant Glib.Signal_Name := "unmounted";
   procedure On_Unmounted
      (Self  : Gmount;
       Call  : Cb_Gmount_Void;
       After : Boolean := False);
   procedure On_Unmounted
      (Self  : Gmount;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the Glib.GMounts.Gmount have been
   --  unmounted. If the recipient is holding references to the object they
   --  should release them so the object can be finalized.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gmount"

   function "+" (W : Gmount) return Gmount;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Can_Eject is access function (Self : Gmount) return Glib.Gboolean;
   pragma Convention (C, Virtual_Can_Eject);
   --  Checks if Mount can be ejected.
   --  @return True if the Mount can be ejected.

   type Virtual_Can_Unmount is access function (Self : Gmount) return Glib.Gboolean;
   pragma Convention (C, Virtual_Can_Unmount);
   --  Checks if Mount can be unmounted.
   --  @return True if the Mount can be unmounted.

   type Virtual_Changed is access procedure (Self : Gmount);
   pragma Convention (C, Virtual_Changed);
   --  Changed signal that is emitted when the mount's state has changed.

   type Virtual_Eject_With_Operation is access procedure
     (Self            : Gmount;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Virtual_Eject_With_Operation);
   --  Ejects a mount. This is an asynchronous operation, and is finished by
   --  calling Glib.GMounts.Eject_With_Operation_Finish with the Mount and
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
     (Self   : Gmount;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Virtual_Eject_With_Operation_Finish);
   --  Finishes ejecting a mount. If any errors occurred during the operation,
   --  Error will be set to contain the errors and False will be returned.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the mount was successfully ejected. False otherwise.

   type Virtual_Get_Default_Location is access function (Self : Gmount) return Glib.GFile.Gfile;
   pragma Convention (C, Virtual_Get_Default_Location);
   --  Gets the default location of Mount. The default location of the given
   --  Mount is a path that reflects the main entry point for the user (e.g.
   --  the home directory, or the root of the volume).
   --  @return a Glib.GFile.Gfile. The returned object should be unreffed with
   --  g_object_unref when no longer needed.

   type Virtual_Get_Drive is access function (Self : Gmount) return Glib.GDrives.Gdrive;
   pragma Convention (C, Virtual_Get_Drive);
   --  Gets the drive for the Mount.
   --  This is a convenience method for getting the Glib.GVolumes.Gvolume and
   --  then using that object to get the Glib.GDrives.Gdrive.
   --  @return a Glib.GDrives.Gdrive or null if Mount is not associated with a
   --  volume or a drive. The returned object should be unreffed with
   --  g_object_unref when no longer needed.

   type Virtual_Get_Icon is access function (Self : Gmount) return Glib.G_Icon.G_Icon;
   pragma Convention (C, Virtual_Get_Icon);
   --  Gets the icon for Mount.
   --  @return a Glib.G_Icon.G_Icon. The returned object should be unreffed
   --  with g_object_unref when no longer needed.

   type Virtual_Get_Name is access function (Self : Gmount) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Name);
   --  Gets the name of Mount.
   --  @return the name for the given Mount. The returned string should be
   --  freed with g_free when no longer needed.

   type Virtual_Get_Root is access function (Self : Gmount) return Glib.GFile.Gfile;
   pragma Convention (C, Virtual_Get_Root);
   --  Gets the root directory on Mount.
   --  @return a Glib.GFile.Gfile. The returned object should be unreffed with
   --  g_object_unref when no longer needed.

   type Virtual_Get_Sort_Key is access function (Self : Gmount) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Sort_Key);
   --  Gets the sort key for Mount, if any.
   --  Since: gtk+ 2.32
   --  @return Sorting key for Mount or null if no such key is available.

   type Virtual_Get_Symbolic_Icon is access function (Self : Gmount) return Glib.G_Icon.G_Icon;
   pragma Convention (C, Virtual_Get_Symbolic_Icon);
   --  Gets the symbolic icon for Mount.
   --  Since: gtk+ 2.34
   --  @return a Glib.G_Icon.G_Icon. The returned object should be unreffed
   --  with g_object_unref when no longer needed.

   type Virtual_Get_Uuid is access function (Self : Gmount) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Uuid);
   --  Gets the UUID for the Mount. The reference is typically based on the
   --  file system UUID for the mount in question and should be considered an
   --  opaque string. Returns null if there is no UUID available.
   --  @return the UUID for Mount or null if no UUID can be computed. The
   --  returned string should be freed with g_free when no longer needed.

   type Virtual_Get_Volume is access function (Self : Gmount) return Glib.GVolumes.Gvolume;
   pragma Convention (C, Virtual_Get_Volume);
   --  Gets the volume for the Mount.
   --  @return a Glib.GVolumes.Gvolume or null if Mount is not associated with
   --  a volume. The returned object should be unreffed with g_object_unref
   --  when no longer needed.

   type Virtual_Guess_Content_Type is access procedure
     (Self         : Gmount;
      Force_Rescan : Glib.Gboolean;
      Cancellable  : System.Address;
      Callback     : System.Address;
      User_Data    : System.Address);
   pragma Convention (C, Virtual_Guess_Content_Type);
   --  Tries to guess the type of content stored on Mount. Returns one or more
   --  textual identifiers of well-known content types (typically prefixed with
   --  "x-content/"), e.g. x-content/image-dcf for camera memory cards. See the
   --  [shared-mime-info](http://www.freedesktop.org/wiki/Specifications/shared-mime-info-spec)
   --  specification for more on x-content types.
   --  This is an asynchronous operation (see
   --  Glib.GMounts.Guess_Content_Type_Sync for the synchronous version), and
   --  is finished by calling Glib.GMounts.Guess_Content_Type_Finish with the
   --  Mount and Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.18
   --  @param Force_Rescan Whether to force a rescan of the content. Otherwise
   --  a cached result will be used if available
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback
   --  @param User_Data user data passed to Callback

   type Virtual_Guess_Content_Type_Finish is access function
     (Self   : Gmount;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return chars_ptr_array_access;
   pragma Convention (C, Virtual_Guess_Content_Type_Finish);
   --  Finishes guessing content types of Mount. If any errors occurred during
   --  the operation, Error will be set to contain the errors and False will be
   --  returned. In particular, you may get an G_IO_ERROR_NOT_SUPPORTED if the
   --  mount does not support content guessing.
   --  Since: gtk+ 2.18
   --  @param Result a Glib.G_Async_Result
   --  @param Error the return location for a recoverable error
   --  @return a null-terminated array of content types or null on error.
   --  Caller should free this array with g_strfreev when done with it.

   type Virtual_Guess_Content_Type_Sync is access function
     (Self         : Gmount;
      Force_Rescan : Glib.Gboolean;
      Cancellable  : System.Address;
      Error        : out Glib.Error.GError) return chars_ptr_array_access;
   pragma Convention (C, Virtual_Guess_Content_Type_Sync);
   --  Tries to guess the type of content stored on Mount. Returns one or more
   --  textual identifiers of well-known content types (typically prefixed with
   --  "x-content/"), e.g. x-content/image-dcf for camera memory cards. See the
   --  [shared-mime-info](http://www.freedesktop.org/wiki/Specifications/shared-mime-info-spec)
   --  specification for more on x-content types.
   --  This is a synchronous operation and as such may block doing IO; see
   --  Glib.GMounts.Guess_Content_Type for the asynchronous version.
   --  Since: gtk+ 2.18
   --  @param Force_Rescan Whether to force a rescan of the content. Otherwise
   --  a cached result will be used if available
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Error the return location for a recoverable error
   --  @return a null-terminated array of content types or null on error.
   --  Caller should free this array with g_strfreev when done with it.

   type Virtual_Pre_Unmount is access procedure (Self : Gmount);
   pragma Convention (C, Virtual_Pre_Unmount);
   --  The ::pre-unmount signal that is emitted when the Glib.GMounts.Gmount
   --  will soon be emitted. If the recipient is somehow holding the mount open
   --  by keeping an open file on it it should close the file.

   type Virtual_Remount is access procedure
     (Self            : Gmount;
      Flags           : Glib.GEnums.GMount_Mount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Virtual_Remount);
   --  Remounts a mount. This is an asynchronous operation, and is finished by
   --  calling Glib.GMounts.Remount_Finish with the Mount and GAsync_Results
   --  data returned in the Callback.
   --  Remounting is useful when some setting affecting the operation of the
   --  volume has been changed, as these may need a remount to take affect.
   --  While this is semantically equivalent with unmounting and then
   --  remounting not all backends might need to actually be unmounted.
   --  @param Flags flags affecting the operation
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.
   --  @param User_Data user data passed to Callback.

   type Virtual_Remount_Finish is access function
     (Self   : Gmount;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Virtual_Remount_Finish);
   --  Finishes remounting a mount. If any errors occurred during the
   --  operation, Error will be set to contain the errors and False will be
   --  returned.
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the mount was successfully remounted. False otherwise.

   type Virtual_Unmount_With_Operation is access procedure
     (Self            : Gmount;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Virtual_Unmount_With_Operation);
   --  Unmounts a mount. This is an asynchronous operation, and is finished by
   --  calling Glib.GMounts.Unmount_With_Operation_Finish with the Mount and
   --  Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the operation
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.
   --  @param User_Data user data passed to Callback.

   type Virtual_Unmount_With_Operation_Finish is access function
     (Self   : Gmount;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Virtual_Unmount_With_Operation_Finish);
   --  Finishes unmounting a mount. If any errors occurred during the
   --  operation, Error will be set to contain the errors and False will be
   --  returned.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the mount was successfully unmounted. False otherwise.

   type Virtual_Unmounted is access procedure (Self : Gmount);
   pragma Convention (C, Virtual_Unmounted);
   --  The unmounted signal that is emitted when the Glib.GMounts.Gmount have
   --  been unmounted. If the recipient is holding references to the object
   --  they should release them so the object can be finalized.

   subtype GMounts_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Can_Eject
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Can_Eject);
   pragma Import (C, Set_Can_Eject, "gtkada_GMounts_set_can_eject");

   procedure Set_Can_Unmount
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Can_Unmount);
   pragma Import (C, Set_Can_Unmount, "gtkada_GMounts_set_can_unmount");

   procedure Set_Changed
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Changed);
   pragma Import (C, Set_Changed, "gtkada_GMounts_set_changed");

   procedure Set_Eject_With_Operation
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Eject_With_Operation);
   pragma Import (C, Set_Eject_With_Operation, "gtkada_GMounts_set_eject_with_operation");

   procedure Set_Eject_With_Operation_Finish
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Eject_With_Operation_Finish);
   pragma Import (C, Set_Eject_With_Operation_Finish, "gtkada_GMounts_set_eject_with_operation_finish");

   procedure Set_Get_Default_Location
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Get_Default_Location);
   pragma Import (C, Set_Get_Default_Location, "gtkada_GMounts_set_get_default_location");

   procedure Set_Get_Drive
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Get_Drive);
   pragma Import (C, Set_Get_Drive, "gtkada_GMounts_set_get_drive");

   procedure Set_Get_Icon
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Get_Icon);
   pragma Import (C, Set_Get_Icon, "gtkada_GMounts_set_get_icon");

   procedure Set_Get_Name
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Get_Name);
   pragma Import (C, Set_Get_Name, "gtkada_GMounts_set_get_name");

   procedure Set_Get_Root
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Get_Root);
   pragma Import (C, Set_Get_Root, "gtkada_GMounts_set_get_root");

   procedure Set_Get_Sort_Key
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Get_Sort_Key);
   pragma Import (C, Set_Get_Sort_Key, "gtkada_GMounts_set_get_sort_key");

   procedure Set_Get_Symbolic_Icon
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Get_Symbolic_Icon);
   pragma Import (C, Set_Get_Symbolic_Icon, "gtkada_GMounts_set_get_symbolic_icon");

   procedure Set_Get_Uuid
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Get_Uuid);
   pragma Import (C, Set_Get_Uuid, "gtkada_GMounts_set_get_uuid");

   procedure Set_Get_Volume
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Get_Volume);
   pragma Import (C, Set_Get_Volume, "gtkada_GMounts_set_get_volume");

   procedure Set_Guess_Content_Type
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Guess_Content_Type);
   pragma Import (C, Set_Guess_Content_Type, "gtkada_GMounts_set_guess_content_type");

   procedure Set_Guess_Content_Type_Finish
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Guess_Content_Type_Finish);
   pragma Import (C, Set_Guess_Content_Type_Finish, "gtkada_GMounts_set_guess_content_type_finish");

   procedure Set_Guess_Content_Type_Sync
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Guess_Content_Type_Sync);
   pragma Import (C, Set_Guess_Content_Type_Sync, "gtkada_GMounts_set_guess_content_type_sync");

   procedure Set_Pre_Unmount
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Pre_Unmount);
   pragma Import (C, Set_Pre_Unmount, "gtkada_GMounts_set_pre_unmount");

   procedure Set_Remount
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Remount);
   pragma Import (C, Set_Remount, "gtkada_GMounts_set_remount");

   procedure Set_Remount_Finish
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Remount_Finish);
   pragma Import (C, Set_Remount_Finish, "gtkada_GMounts_set_remount_finish");

   procedure Set_Unmount_With_Operation
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Unmount_With_Operation);
   pragma Import (C, Set_Unmount_With_Operation, "gtkada_GMounts_set_unmount_with_operation");

   procedure Set_Unmount_With_Operation_Finish
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Unmount_With_Operation_Finish);
   pragma Import (C, Set_Unmount_With_Operation_Finish, "gtkada_GMounts_set_unmount_with_operation_finish");

   procedure Set_Unmounted
     (Self    : GMounts_Interface_Descr;
      Handler : Virtual_Unmounted);
   pragma Import (C, Set_Unmounted, "gtkada_GMounts_set_unmounted");
   --  See Glib.Object.Add_Interface

private

   Null_Gmount : constant Gmount :=
      Gmount (Glib.Types.Null_Interface);
end Glib.GMounts;
