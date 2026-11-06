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

--  The Glib.GVolumes.Gvolume interface represents user-visible objects that
--  can be mounted. Note, when porting from GnomeVFS, Glib.GVolumes.Gvolume is
--  the moral equivalent of Gnome_VFSDrive.
--
--  Mounting a Glib.GVolumes.Gvolume instance is an asynchronous operation.
--  For more information about asynchronous operations, see Glib.G_Async_Result
--  and Gtask.Gtask. To mount a Glib.GVolumes.Gvolume, first call
--  Glib.GVolumes.Mount with (at least) the Glib.GVolumes.Gvolume instance,
--  optionally a Glib.GMountOperations.Gmount_Operation object and a
--  Gasync_Ready_Callback.
--
--  Typically, one will only want to pass null for the
--  Glib.GMountOperations.Gmount_Operation if automounting all volumes when a
--  desktop session starts since it's not desirable to put up a lot of dialogs
--  asking for credentials.
--
--  The callback will be fired when the operation has resolved (either with
--  success or failure), and a Glib.G_Async_Result instance will be passed to
--  the callback. That callback should then call Glib.GVolumes.Mount_Finish
--  with the Glib.GVolumes.Gvolume instance and the Glib.G_Async_Result data to
--  see if the operation was completed successfully. If an Error is present
--  when Glib.GVolumes.Mount_Finish is called, then it will be filled with any
--  error information.
--
--  ## Volume Identifiers # {volume-identifier}
--
--  It is sometimes necessary to directly access the underlying operating
--  system object behind a volume (e.g. for passing a volume to an application
--  via the commandline). For this purpose, GIO allows to obtain an
--  'identifier' for the volume. There can be different kinds of identifiers,
--  such as Hal UDIs, filesystem labels, traditional Unix devices (e.g.
--  `/dev/sda2`), UUIDs. GIO uses predefined strings as names for the different
--  kinds of identifiers: G_VOLUME_IDENTIFIER_KIND_UUID,
--  G_VOLUME_IDENTIFIER_KIND_LABEL, etc. Use Glib.GVolumes.Get_Identifier to
--  obtain an identifier for a volume.
--
--  Note that G_VOLUME_IDENTIFIER_KIND_HAL_UDI will only be available when the
--  gvfs hal volume monitor is in use. Other volume monitors will generally be
--  able to provide the G_VOLUME_IDENTIFIER_KIND_UNIX_DEVICE identifier, which
--  can be used to obtain a hal device by means of
--  libhal_manager_find_device_string_match.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;          use GNAT.Strings;
with Glib.Cancellable;      use Glib.Cancellable;
with Glib.Error;            use Glib.Error;
limited with Glib.GDrives;
with Glib.GEnums;           use Glib.GEnums;
with Glib.GFile;            use Glib.GFile;
with Glib.GMountOperations; use Glib.GMountOperations;
limited with Glib.GMounts;
with Glib.G_Icon;           use Glib.G_Icon;
with Glib.Glist;            use Glib.Glist;
with Glib.Object;           use Glib.Object;
with Glib.Types;            use Glib.Types;
with Gtkada.Types;          use Gtkada.Types;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;       use Gtkada.Bindings;
pragma Warnings(On);

package Glib.GVolumes is

   type Gvolume is new Glib.Types.GType_Interface;
   Null_Gvolume : constant Gvolume;

   function Convert (R : Glib.GVolumes.Gvolume) return System.Address;
   function Convert (R : System.Address) return Glib.GVolumes.Gvolume;
   package GVolume_List is new Generic_List (Glib.GVolumes.Gvolume);

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
   pragma Import (C, Get_Type, "g_volume_get_type");

   -------------
   -- Methods --
   -------------

   function Can_Eject (Self : Gvolume) return Boolean;
   --  Checks if a volume can be ejected.
   --  @return True if the Volume can be ejected. False otherwise

   function Can_Mount (Self : Gvolume) return Boolean;
   --  Checks if a volume can be mounted.
   --  @return True if the Volume can be mounted. False otherwise

   procedure Eject_With_Operation
      (Self            : Gvolume;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback);
   --  Ejects a volume. This is an asynchronous operation, and is finished by
   --  calling Glib.GVolumes.Eject_With_Operation_Finish with the Volume and
   --  Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the unmount if required for eject
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback, or null

   function Eject_With_Operation_Finish
      (Self   : Gvolume;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes ejecting a volume. If any errors occurred during the
   --  operation, Error will be set to contain the errors and False will be
   --  returned.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result
   --  @param Error the return location for a recoverable error
   --  @return True if the volume was successfully ejected. False otherwise

   function Enumerate_Identifiers
      (Self : Gvolume) return GNAT.Strings.String_List;
   --  Gets the kinds of [identifiers][volume-identifier] that Volume has. Use
   --  Glib.GVolumes.Get_Identifier to obtain the identifiers themselves.
   --  @return a null-terminated array of strings containing kinds of
   --  identifiers. Use g_strfreev to free.

   function Get_Activation_Root (Self : Gvolume) return Glib.GFile.Gfile;
   pragma Import (C, Get_Activation_Root, "g_volume_get_activation_root");
   --  Gets the activation root for a Glib.GVolumes.Gvolume if it is known
   --  ahead of mount time. Returns null otherwise. If not null and if Volume
   --  is mounted, then the result of Glib.GMounts.Get_Root on the
   --  Glib.GMounts.Gmount object obtained from Glib.GVolumes.Get_Mount will
   --  always either be equal or a prefix of what this function returns. In
   --  other words, in code
   --
   --     GMount *mount;
   --     GFile *mount_root
   --     GFile *volume_activation_root;
   --
   --     mount = g_volume_get_mount (volume); // mounted, so never NULL
   --     mount_root = g_mount_get_root (mount);
   --     volume_activation_root = g_volume_get_activation_root (volume); // assume not NULL
   --
   --  then the expression
   --
   --     (g_file_has_prefix (volume_activation_root, mount_root) ||
   --      g_file_equal (volume_activation_root, mount_root))
   --
   --  will always be True.
   --  Activation roots are typically used in Gvolume.Monitor.Gvolume_Monitor
   --  implementations to find the underlying mount to shadow, see
   --  Glib.GMounts.Is_Shadowed for more details.
   --  Since: gtk+ 2.18
   --  @return the activation root of Volume or null. Use g_object_unref to
   --  free.

   function Get_Drive (Self : Gvolume) return Glib.GDrives.Gdrive;
   pragma Import (C, Get_Drive, "g_volume_get_drive");
   --  Gets the drive for the Volume.
   --  @return a Glib.GDrives.Gdrive or null if Volume is not associated with
   --  a drive. The returned object should be unreffed with g_object_unref when
   --  no longer needed.

   function Get_Icon (Self : Gvolume) return Glib.G_Icon.G_Icon;
   pragma Import (C, Get_Icon, "g_volume_get_icon");
   --  Gets the icon for Volume.
   --  @return a Glib.G_Icon.G_Icon. The returned object should be unreffed
   --  with g_object_unref when no longer needed.

   function Get_Identifier
      (Self : Gvolume;
       Kind : UTF8_String) return UTF8_String;
   --  Gets the identifier of the given kind for Volume. See the
   --  [introduction][volume-identifier] for more information about volume
   --  identifiers.
   --  @param Kind the kind of identifier to return
   --  @return a newly allocated string containing the requested identifier,
   --  or null if the Glib.GVolumes.Gvolume doesn't have this kind of
   --  identifier

   function Get_Mount (Self : Gvolume) return Glib.GMounts.Gmount;
   pragma Import (C, Get_Mount, "g_volume_get_mount");
   --  Gets the mount for the Volume.
   --  @return a Glib.GMounts.Gmount or null if Volume isn't mounted. The
   --  returned object should be unreffed with g_object_unref when no longer
   --  needed.

   procedure Mount
      (Self            : Gvolume;
       Flags           : Glib.GEnums.GMount_Mount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback);
   --  Mounts a volume. This is an asynchronous operation, and is finished by
   --  calling Glib.GVolumes.Mount_Finish with the Volume and
   --  Glib.G_Async_Result returned in the Callback.
   --  @param Flags flags affecting the operation
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback, or null

   function Get_Name (Self : Gvolume) return UTF8_String;
   --  Gets the name of Volume.
   --  @return the name for the given Volume. The returned string should be
   --  freed with g_free when no longer needed.

   function Get_Sort_Key (Self : Gvolume) return UTF8_String;
   --  Gets the sort key for Volume, if any.
   --  Since: gtk+ 2.32
   --  @return Sorting key for Volume or null if no such key is available

   function Get_Symbolic_Icon (Self : Gvolume) return Glib.G_Icon.G_Icon;
   pragma Import (C, Get_Symbolic_Icon, "g_volume_get_symbolic_icon");
   --  Gets the symbolic icon for Volume.
   --  Since: gtk+ 2.34
   --  @return a Glib.G_Icon.G_Icon. The returned object should be unreffed
   --  with g_object_unref when no longer needed.

   function Get_Uuid (Self : Gvolume) return UTF8_String;
   --  Gets the UUID for the Volume. The reference is typically based on the
   --  file system UUID for the volume in question and should be considered an
   --  opaque string. Returns null if there is no UUID available.
   --  @return the UUID for Volume or null if no UUID can be computed. The
   --  returned string should be freed with g_free when no longer needed.

   function Mount_Finish
      (Self   : Gvolume;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes mounting a volume. If any errors occurred during the
   --  operation, Error will be set to contain the errors and False will be
   --  returned.
   --  If the mount operation succeeded, Glib.GVolumes.Get_Mount on Volume is
   --  guaranteed to return the mount right after calling this function;
   --  there's no need to listen for the 'mount-added' signal on
   --  Gvolume.Monitor.Gvolume_Monitor.
   --  @param Result a Glib.G_Async_Result
   --  @param Error the return location for a recoverable error
   --  @return True, False if operation failed

   function Should_Automount (Self : Gvolume) return Boolean;
   --  Returns whether the volume should be automatically mounted.
   --  @return True if the volume should be automatically mounted

   ----------------------
   -- GtkAda additions --
   ----------------------

   subtype GVolume_GList is GVolume_List.GList;

   -------------
   -- Signals --
   -------------

   type Cb_Gvolume_Void is not null access procedure (Self : Gvolume);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : Gvolume;
       Call  : Cb_Gvolume_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : Gvolume;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the volume has been changed.

   Signal_Removed : constant Glib.Signal_Name := "removed";
   procedure On_Removed
      (Self  : Gvolume;
       Call  : Cb_Gvolume_Void;
       After : Boolean := False);
   procedure On_Removed
      (Self  : Gvolume;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the Glib.GVolumes.Gvolume have been
   --  removed. If the recipient is holding references to the object they
   --  should release them so the object can be finalized.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gvolume"

   function "+" (W : Gvolume) return Gvolume;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Can_Eject is access function (Self : Gvolume) return Glib.Gboolean;
   pragma Convention (C, Virtual_Can_Eject);
   --  Checks if a volume can be ejected.
   --  @return True if the Volume can be ejected. False otherwise

   type Virtual_Can_Mount is access function (Self : Gvolume) return Glib.Gboolean;
   pragma Convention (C, Virtual_Can_Mount);
   --  Checks if a volume can be mounted.
   --  @return True if the Volume can be mounted. False otherwise

   type Virtual_Changed is access procedure (Self : Gvolume);
   pragma Convention (C, Virtual_Changed);
   --  Changed signal that is emitted when the volume's state has changed.

   type Virtual_Eject_With_Operation is access procedure
     (Self            : Gvolume;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Virtual_Eject_With_Operation);
   --  Ejects a volume. This is an asynchronous operation, and is finished by
   --  calling Glib.GVolumes.Eject_With_Operation_Finish with the Volume and
   --  Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the unmount if required for eject
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback, or null
   --  @param User_Data user data passed to Callback

   type Virtual_Eject_With_Operation_Finish is access function
     (Self   : Gvolume;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Virtual_Eject_With_Operation_Finish);
   --  Finishes ejecting a volume. If any errors occurred during the
   --  operation, Error will be set to contain the errors and False will be
   --  returned.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result
   --  @param Error the return location for a recoverable error
   --  @return True if the volume was successfully ejected. False otherwise

   type Virtual_Enumerate_Identifiers is access function (Self : Gvolume) return chars_ptr_array_access;
   pragma Convention (C, Virtual_Enumerate_Identifiers);
   --  Gets the kinds of [identifiers][volume-identifier] that Volume has. Use
   --  Glib.GVolumes.Get_Identifier to obtain the identifiers themselves.
   --  @return a null-terminated array of strings containing kinds of
   --  identifiers. Use g_strfreev to free.

   type Virtual_Get_Activation_Root is access function (Self : Gvolume) return Glib.GFile.Gfile;
   pragma Convention (C, Virtual_Get_Activation_Root);
   --  Gets the activation root for a Glib.GVolumes.Gvolume if it is known
   --  ahead of mount time. Returns null otherwise. If not null and if Volume
   --  is mounted, then the result of Glib.GMounts.Get_Root on the
   --  Glib.GMounts.Gmount object obtained from Glib.GVolumes.Get_Mount will
   --  always either be equal or a prefix of what this function returns. In
   --  other words, in code
   --
   --     GMount *mount;
   --     GFile *mount_root
   --     GFile *volume_activation_root;
   --
   --     mount = g_volume_get_mount (volume); // mounted, so never NULL
   --     mount_root = g_mount_get_root (mount);
   --     volume_activation_root = g_volume_get_activation_root (volume); // assume not NULL
   --
   --  then the expression
   --
   --     (g_file_has_prefix (volume_activation_root, mount_root) ||
   --      g_file_equal (volume_activation_root, mount_root))
   --
   --  will always be True.
   --  Activation roots are typically used in Gvolume.Monitor.Gvolume_Monitor
   --  implementations to find the underlying mount to shadow, see
   --  Glib.GMounts.Is_Shadowed for more details.
   --  Since: gtk+ 2.18
   --  @return the activation root of Volume or null. Use g_object_unref to
   --  free.

   type Virtual_Get_Drive is access function (Self : Gvolume) return Glib.GDrives.Gdrive;
   pragma Convention (C, Virtual_Get_Drive);
   --  Gets the drive for the Volume.
   --  @return a Glib.GDrives.Gdrive or null if Volume is not associated with
   --  a drive. The returned object should be unreffed with g_object_unref when
   --  no longer needed.

   type Virtual_Get_Icon is access function (Self : Gvolume) return Glib.G_Icon.G_Icon;
   pragma Convention (C, Virtual_Get_Icon);
   --  Gets the icon for Volume.
   --  @return a Glib.G_Icon.G_Icon. The returned object should be unreffed
   --  with g_object_unref when no longer needed.

   type Virtual_Get_Identifier is access function
     (Self : Gvolume;
      Kind : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Identifier);
   --  Gets the identifier of the given kind for Volume. See the
   --  [introduction][volume-identifier] for more information about volume
   --  identifiers.
   --  @param Kind the kind of identifier to return
   --  @return a newly allocated string containing the requested identifier,
   --  or null if the Glib.GVolumes.Gvolume doesn't have this kind of
   --  identifier

   type Virtual_Get_Mount is access function (Self : Gvolume) return Glib.GMounts.Gmount;
   pragma Convention (C, Virtual_Get_Mount);
   --  Gets the mount for the Volume.
   --  @return a Glib.GMounts.Gmount or null if Volume isn't mounted. The
   --  returned object should be unreffed with g_object_unref when no longer
   --  needed.

   type Virtual_Get_Name is access function (Self : Gvolume) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Name);
   --  Gets the name of Volume.
   --  @return the name for the given Volume. The returned string should be
   --  freed with g_free when no longer needed.

   type Virtual_Get_Sort_Key is access function (Self : Gvolume) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Sort_Key);
   --  Gets the sort key for Volume, if any.
   --  Since: gtk+ 2.32
   --  @return Sorting key for Volume or null if no such key is available

   type Virtual_Get_Symbolic_Icon is access function (Self : Gvolume) return Glib.G_Icon.G_Icon;
   pragma Convention (C, Virtual_Get_Symbolic_Icon);
   --  Gets the symbolic icon for Volume.
   --  Since: gtk+ 2.34
   --  @return a Glib.G_Icon.G_Icon. The returned object should be unreffed
   --  with g_object_unref when no longer needed.

   type Virtual_Get_Uuid is access function (Self : Gvolume) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Uuid);
   --  Gets the UUID for the Volume. The reference is typically based on the
   --  file system UUID for the volume in question and should be considered an
   --  opaque string. Returns null if there is no UUID available.
   --  @return the UUID for Volume or null if no UUID can be computed. The
   --  returned string should be freed with g_free when no longer needed.

   type Virtual_Mount_Finish is access function
     (Self   : Gvolume;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Virtual_Mount_Finish);
   --  Finishes mounting a volume. If any errors occurred during the
   --  operation, Error will be set to contain the errors and False will be
   --  returned.
   --  If the mount operation succeeded, Glib.GVolumes.Get_Mount on Volume is
   --  guaranteed to return the mount right after calling this function;
   --  there's no need to listen for the 'mount-added' signal on
   --  Gvolume.Monitor.Gvolume_Monitor.
   --  @param Result a Glib.G_Async_Result
   --  @param Error the return location for a recoverable error
   --  @return True, False if operation failed

   type Virtual_Mount_Fn is access procedure
     (Self            : Gvolume;
      Flags           : Glib.GEnums.GMount_Mount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Virtual_Mount_Fn);
   --  Mounts a volume. This is an asynchronous operation, and is finished by
   --  calling Glib.GVolumes.Mount_Finish with the Volume and
   --  Glib.G_Async_Result returned in the Callback.
   --  @param Flags flags affecting the operation
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback, or null
   --  @param User_Data user data that gets passed to Callback

   type Virtual_Removed is access procedure (Self : Gvolume);
   pragma Convention (C, Virtual_Removed);
   --  The removed signal that is emitted when the Glib.GVolumes.Gvolume have
   --  been removed. If the recipient is holding references to the object they
   --  should release them so the object can be finalized.

   type Virtual_Should_Automount is access function (Self : Gvolume) return Glib.Gboolean;
   pragma Convention (C, Virtual_Should_Automount);
   --  Returns whether the volume should be automatically mounted.
   --  @return True if the volume should be automatically mounted

   subtype GVolumes_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Can_Eject
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Can_Eject);
   pragma Import (C, Set_Can_Eject, "gtkada_GVolumes_set_can_eject");

   procedure Set_Can_Mount
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Can_Mount);
   pragma Import (C, Set_Can_Mount, "gtkada_GVolumes_set_can_mount");

   procedure Set_Changed
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Changed);
   pragma Import (C, Set_Changed, "gtkada_GVolumes_set_changed");

   procedure Set_Eject_With_Operation
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Eject_With_Operation);
   pragma Import (C, Set_Eject_With_Operation, "gtkada_GVolumes_set_eject_with_operation");

   procedure Set_Eject_With_Operation_Finish
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Eject_With_Operation_Finish);
   pragma Import (C, Set_Eject_With_Operation_Finish, "gtkada_GVolumes_set_eject_with_operation_finish");

   procedure Set_Enumerate_Identifiers
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Enumerate_Identifiers);
   pragma Import (C, Set_Enumerate_Identifiers, "gtkada_GVolumes_set_enumerate_identifiers");

   procedure Set_Get_Activation_Root
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Get_Activation_Root);
   pragma Import (C, Set_Get_Activation_Root, "gtkada_GVolumes_set_get_activation_root");

   procedure Set_Get_Drive
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Get_Drive);
   pragma Import (C, Set_Get_Drive, "gtkada_GVolumes_set_get_drive");

   procedure Set_Get_Icon
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Get_Icon);
   pragma Import (C, Set_Get_Icon, "gtkada_GVolumes_set_get_icon");

   procedure Set_Get_Identifier
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Get_Identifier);
   pragma Import (C, Set_Get_Identifier, "gtkada_GVolumes_set_get_identifier");

   procedure Set_Get_Mount
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Get_Mount);
   pragma Import (C, Set_Get_Mount, "gtkada_GVolumes_set_get_mount");

   procedure Set_Get_Name
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Get_Name);
   pragma Import (C, Set_Get_Name, "gtkada_GVolumes_set_get_name");

   procedure Set_Get_Sort_Key
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Get_Sort_Key);
   pragma Import (C, Set_Get_Sort_Key, "gtkada_GVolumes_set_get_sort_key");

   procedure Set_Get_Symbolic_Icon
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Get_Symbolic_Icon);
   pragma Import (C, Set_Get_Symbolic_Icon, "gtkada_GVolumes_set_get_symbolic_icon");

   procedure Set_Get_Uuid
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Get_Uuid);
   pragma Import (C, Set_Get_Uuid, "gtkada_GVolumes_set_get_uuid");

   procedure Set_Mount_Finish
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Mount_Finish);
   pragma Import (C, Set_Mount_Finish, "gtkada_GVolumes_set_mount_finish");

   procedure Set_Mount_Fn
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Mount_Fn);
   pragma Import (C, Set_Mount_Fn, "gtkada_GVolumes_set_mount_fn");

   procedure Set_Removed
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Removed);
   pragma Import (C, Set_Removed, "gtkada_GVolumes_set_removed");

   procedure Set_Should_Automount
     (Self    : GVolumes_Interface_Descr;
      Handler : Virtual_Should_Automount);
   pragma Import (C, Set_Should_Automount, "gtkada_GVolumes_set_should_automount");
   --  See Glib.Object.Add_Interface

private

   Null_Gvolume : constant Gvolume :=
      Gvolume (Glib.Types.Null_Interface);
end Glib.GVolumes;
