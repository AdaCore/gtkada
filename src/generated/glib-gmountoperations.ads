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

--  Glib.GMountOperations.Gmount_Operation provides a mechanism for
--  interacting with the user. It can be used for authenticating mountable
--  operations, such as loop mounting files, hard drive partitions or server
--  locations. It can also be used to ask the user questions or show a list of
--  applications preventing unmount or eject operations from completing.
--
--  Note that Glib.GMountOperations.Gmount_Operation is used for more than
--  just Glib.GMounts.Gmount objects – for example it is also used in
--  Glib.GDrives.Start and Glib.GDrives.Stop.
--
--  Users should instantiate a subclass of this that implements all the
--  various callbacks to show the required dialogs, such as
--  Gtk.Mount_Operation.Gtk_Mount_Operation. If no user interaction is desired
--  (for example when automounting filesystems at login time), usually null can
--  be passed, see each method taking a Glib.GMountOperations.Gmount_Operation
--  for details.
--
--  The term ‘TCRYPT' is used to mean ‘compatible with TrueCrypt and
--  VeraCrypt'. [TrueCrypt](https://en.wikipedia.org/wiki/TrueCrypt) is a
--  discontinued system for encrypting file containers, partitions or whole
--  disks, typically used with Windows. [VeraCrypt](https://www.veracrypt.fr/)
--  is a maintained fork of TrueCrypt with various improvements and auditing
--  fixes.

pragma Warnings (Off, "*is already use-visible*");
with Glib.Arrays;     use Glib.Arrays;
with Glib.GEnums;     use Glib.GEnums;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Gtkada.Bindings; use Gtkada.Bindings;

package Glib.GMountOperations is

   type Gmount_Operation_Record is new GObject_Record with null record;
   type Gmount_Operation is access all Gmount_Operation_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gmount_Operation);
   --  Creates a new mount operation.

   procedure Initialize
      (Self : not null access Gmount_Operation_Record'Class);
   --  Creates a new mount operation.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gmount_Operation_New return Gmount_Operation;
   --  Creates a new mount operation.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_mount_operation_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Anonymous
      (Self : not null access Gmount_Operation_Record) return Boolean;
   --  Check to see whether the mount operation is being used for an anonymous
   --  user.
   --  @return True if mount operation is anonymous.

   procedure Set_Anonymous
      (Self      : not null access Gmount_Operation_Record;
       Anonymous : Boolean);
   --  Sets the mount operation to use an anonymous user if Anonymous is True.
   --  @param Anonymous boolean value.

   function Get_Choice
      (Self : not null access Gmount_Operation_Record) return Glib.Gint;
   --  Gets a choice from the mount operation.
   --  @return an integer containing an index of the user's choice from the
   --  choice's list, or `0`.

   procedure Set_Choice
      (Self   : not null access Gmount_Operation_Record;
       Choice : Glib.Gint);
   --  Sets a default choice for the mount operation.
   --  @param Choice an integer.

   function Get_Domain
      (Self : not null access Gmount_Operation_Record) return UTF8_String;
   --  Gets the domain of the mount operation.
   --  @return a string set to the domain.

   procedure Set_Domain
      (Self   : not null access Gmount_Operation_Record;
       Domain : UTF8_String := "");
   --  Sets the mount operation's domain.
   --  @param Domain the domain to set.

   function Get_Is_Tcrypt_Hidden_Volume
      (Self : not null access Gmount_Operation_Record) return Boolean;
   --  Check to see whether the mount operation is being used for a TCRYPT
   --  hidden volume.
   --  Since: gtk+ 2.58
   --  @return True if mount operation is for hidden volume.

   procedure Set_Is_Tcrypt_Hidden_Volume
      (Self          : not null access Gmount_Operation_Record;
       Hidden_Volume : Boolean);
   --  Sets the mount operation to use a hidden volume if Hidden_Volume is
   --  True.
   --  Since: gtk+ 2.58
   --  @param Hidden_Volume boolean value.

   function Get_Is_Tcrypt_System_Volume
      (Self : not null access Gmount_Operation_Record) return Boolean;
   --  Check to see whether the mount operation is being used for a TCRYPT
   --  system volume.
   --  Since: gtk+ 2.58
   --  @return True if mount operation is for system volume.

   procedure Set_Is_Tcrypt_System_Volume
      (Self          : not null access Gmount_Operation_Record;
       System_Volume : Boolean);
   --  Sets the mount operation to use a system volume if System_Volume is
   --  True.
   --  Since: gtk+ 2.58
   --  @param System_Volume boolean value.

   function Get_Password
      (Self : not null access Gmount_Operation_Record) return UTF8_String;
   --  Gets a password from the mount operation.
   --  @return a string containing the password within Op.

   function Get_Password_Save
      (Self : not null access Gmount_Operation_Record)
       return Glib.GEnums.GPassword_Save;
   --  Gets the state of saving passwords for the mount operation.
   --  @return a Glib.GEnums.GPassword_Save flag.

   procedure Set_Password_Save
      (Self : not null access Gmount_Operation_Record;
       Save : Glib.GEnums.GPassword_Save);
   --  Sets the state of saving passwords for the mount operation.
   --  @param Save a set of Glib.GEnums.GPassword_Save flags.

   function Get_Pim
      (Self : not null access Gmount_Operation_Record) return Guint;
   --  Gets a PIM from the mount operation.
   --  Since: gtk+ 2.58
   --  @return The VeraCrypt PIM within Op.

   procedure Set_Pim
      (Self : not null access Gmount_Operation_Record;
       Pim  : Guint);
   --  Sets the mount operation's PIM to Pim.
   --  Since: gtk+ 2.58
   --  @param Pim an unsigned integer.

   function Get_Username
      (Self : not null access Gmount_Operation_Record) return UTF8_String;
   --  Get the user name from the mount operation.
   --  @return a string containing the user name.

   procedure Set_Username
      (Self     : not null access Gmount_Operation_Record;
       Username : UTF8_String := "");
   --  Sets the user name within Op to Username.
   --  @param Username input username.

   procedure Reply
      (Self   : not null access Gmount_Operation_Record;
       Result : Glib.GEnums.GMount_Operation_Result);
   --  Emits the Glib.GMountOperations.Gmount_Operation::reply signal.
   --  @param Result a Glib.GEnums.GMount_Operation_Result

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Set_Password
     (Self     : not null access Gmount_Operation_Record;
      Password : UTF8_String := "");
   --  Sets the mount operation's password to Password.
   --  @param Password password to set.

   Password_Save_Property : constant Glib.GEnums.Property_GPassword_Save :=
   Glib.GEnums.Build ("password-save");
   --  Type: Password_Save
   --  Determines if and how the password information should be saved.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Anonymous_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to use an anonymous user when authenticating.

   Choice_Property : constant Glib.Properties.Property_Int;
   --  The index of the user's choice when a question is asked during the
   --  mount operation. See the
   --  Glib.GMountOperations.Gmount_Operation::ask-question signal.

   Domain_Property : constant Glib.Properties.Property_String;
   --  The domain to use for the mount operation.

   Is_Tcrypt_Hidden_Volume_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the device to be unlocked is a TCRYPT hidden volume. See [the
   --  VeraCrypt
   --  documentation](https://www.veracrypt.fr/en/Hidden%20Volume.html).

   Is_Tcrypt_System_Volume_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the device to be unlocked is a TCRYPT system volume. In this
   --  context, a system volume is a volume with a bootloader and operating
   --  system installed. This is only supported for Windows operating systems.
   --  For further documentation, see [the VeraCrypt
   --  documentation](https://www.veracrypt.fr/en/System%20Encryption.html).

   Password_Property : constant Glib.Properties.Property_String;
   --  The password that is used for authentication when carrying out the
   --  mount operation.

   Pim_Property : constant Glib.Properties.Property_Uint;
   --  The VeraCrypt PIM value, when unlocking a VeraCrypt volume. See [the
   --  VeraCrypt
   --  documentation](https://www.veracrypt.fr/en/Personal%20Iterations%20Multiplier%20(PIM).html).

   Username_Property : constant Glib.Properties.Property_String;
   --  The user name that is used for authentication when carrying out the
   --  mount operation.

   -------------
   -- Signals --
   -------------

   type Cb_Gmount_Operation_Void is not null access procedure
     (Self : access Gmount_Operation_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Aborted : constant Glib.Signal_Name := "aborted";
   procedure On_Aborted
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_Gmount_Operation_Void;
       After : Boolean := False);
   procedure On_Aborted
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted by the backend when e.g. a device becomes unavailable while a
   --  mount operation is in progress.
   --
   --  Implementations of GMountOperation should handle this signal by
   --  dismissing open password dialogs.

   type Cb_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void is not null access procedure
     (Self           : access Gmount_Operation_Record'Class;
      Message        : UTF8_String;
      Default_User   : UTF8_String;
      Default_Domain : UTF8_String;
      Flags          : Glib.GEnums.GAsk_Password_Flags);

   type Cb_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void is not null access procedure
     (Self           : access Glib.Object.GObject_Record'Class;
      Message        : UTF8_String;
      Default_User   : UTF8_String;
      Default_Domain : UTF8_String;
      Flags          : Glib.GEnums.GAsk_Password_Flags);

   Signal_Ask_Password : constant Glib.Signal_Name := "ask-password";
   procedure On_Ask_Password
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_Gmount_Operation_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void;
       After : Boolean := False);
   procedure On_Ask_Password
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_GObject_UTF8_String_UTF8_String_UTF8_String_GAsk_Password_Flags_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a mount operation asks the user for a password.
   --
   --  If the message contains a line break, the first line should be
   --  presented as a heading. For example, it may be used as the primary text
   --  in a Gtk.Message_Dialog.Gtk_Message_Dialog.
   -- 
   --  Callback parameters:
   --    --  @param Message string containing a message to display to the user.
   --    --  @param Default_User string containing the default user name.
   --    --  @param Default_Domain string containing the default domain.
   --    --  @param Flags a set of Glib.GEnums.GAsk_Password_Flags.

   type Cb_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void is not null access procedure
     (Self    : access Gmount_Operation_Record'Class;
      Message : UTF8_String;
      Choices : Gtkada.Bindings.chars_ptr_array_access);

   type Cb_GObject_UTF8_String_chars_ptr_array_access_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Message : UTF8_String;
      Choices : Gtkada.Bindings.chars_ptr_array_access);

   Signal_Ask_Question : constant Glib.Signal_Name := "ask-question";
   procedure On_Ask_Question
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_Gmount_Operation_UTF8_String_chars_ptr_array_access_Void;
       After : Boolean := False);
   procedure On_Ask_Question
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_GObject_UTF8_String_chars_ptr_array_access_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when asking the user a question and gives a list of choices for
   --  the user to choose from.
   --
   --  If the message contains a line break, the first line should be
   --  presented as a heading. For example, it may be used as the primary text
   --  in a Gtk.Message_Dialog.Gtk_Message_Dialog.
   -- 
   --  Callback parameters:
   --    --  @param Message string containing a message to display to the user.
   --    --  @param Choices an array of strings for each possible choice.

   type Cb_Gmount_Operation_GMount_Operation_Result_Void is not null access procedure
     (Self   : access Gmount_Operation_Record'Class;
      Result : Glib.GEnums.GMount_Operation_Result);

   type Cb_GObject_GMount_Operation_Result_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Result : Glib.GEnums.GMount_Operation_Result);

   Signal_Reply : constant Glib.Signal_Name := "reply";
   procedure On_Reply
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_Gmount_Operation_GMount_Operation_Result_Void;
       After : Boolean := False);
   procedure On_Reply
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_GObject_GMount_Operation_Result_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user has replied to the mount operation.

   type Cb_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void is not null access procedure
     (Self      : access Gmount_Operation_Record'Class;
      Message   : UTF8_String;
      Processes : Glib.Arrays.Garray_Access;
      Choices   : Gtkada.Bindings.chars_ptr_array_access);

   type Cb_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Message   : UTF8_String;
      Processes : Glib.Arrays.Garray_Access;
      Choices   : Gtkada.Bindings.chars_ptr_array_access);

   Signal_Show_Processes : constant Glib.Signal_Name := "show-processes";
   procedure On_Show_Processes
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_Gmount_Operation_UTF8_String_Garray_Access_chars_ptr_array_access_Void;
       After : Boolean := False);
   procedure On_Show_Processes
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_GObject_UTF8_String_Garray_Access_chars_ptr_array_access_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when one or more processes are blocking an operation e.g.
   --  unmounting/ejecting a Glib.GMounts.Gmount or stopping a
   --  Glib.GDrives.Gdrive.
   --
   --  Note that this signal may be emitted several times to update the list
   --  of blocking processes as processes close files. The application should
   --  only respond with Glib.GMountOperations.Reply to the latest signal
   --  (setting Glib.GMountOperations.Gmount_Operation:choice to the choice the
   --  user made).
   --
   --  If the message contains a line break, the first line should be
   --  presented as a heading. For example, it may be used as the primary text
   --  in a Gtk.Message_Dialog.Gtk_Message_Dialog.
   -- 
   --  Callback parameters:
   --    --  @param Message string containing a message to display to the user.
   --    --  @param Processes an array of GPid for processes blocking the operation.
   --    --  @param Choices an array of strings for each possible choice.

   type Cb_Gmount_Operation_UTF8_String_Gint64_Gint64_Void is not null access procedure
     (Self       : access Gmount_Operation_Record'Class;
      Message    : UTF8_String;
      Time_Left  : Gint64;
      Bytes_Left : Gint64);

   type Cb_GObject_UTF8_String_Gint64_Gint64_Void is not null access procedure
     (Self       : access Glib.Object.GObject_Record'Class;
      Message    : UTF8_String;
      Time_Left  : Gint64;
      Bytes_Left : Gint64);

   Signal_Show_Unmount_Progress : constant Glib.Signal_Name := "show-unmount-progress";
   procedure On_Show_Unmount_Progress
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_Gmount_Operation_UTF8_String_Gint64_Gint64_Void;
       After : Boolean := False);
   procedure On_Show_Unmount_Progress
      (Self  : not null access Gmount_Operation_Record;
       Call  : Cb_GObject_UTF8_String_Gint64_Gint64_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when an unmount operation has been busy for more than some time
   --  (typically 1.5 seconds).
   --
   --  When unmounting or ejecting a volume, the kernel might need to flush
   --  pending data in its buffers to the volume stable storage, and this
   --  operation can take a considerable amount of time. This signal may be
   --  emitted several times as long as the unmount operation is outstanding,
   --  and then one last time when the operation is completed, with Bytes_Left
   --  set to zero.
   --
   --  Implementations of GMountOperation should handle this signal by showing
   --  an UI notification, and then dismiss it, or show another notification of
   --  completion, when Bytes_Left reaches zero.
   --
   --  If the message contains a line break, the first line should be
   --  presented as a heading. For example, it may be used as the primary text
   --  in a Gtk.Message_Dialog.Gtk_Message_Dialog.
   -- 
   --  Callback parameters:
   --    --  @param Message string containing a message to display to the user
   --    --  @param Time_Left the estimated time left before the operation
   --    --  completes, in microseconds, or -1
   --    --  @param Bytes_Left the amount of bytes to be written before the
   --    --  operation completes (or -1 if such amount is not known), or zero if the
   --    --  operation is completed

private
   Username_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("username");
   Pim_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("pim");
   Password_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("password");
   Is_Tcrypt_System_Volume_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-tcrypt-system-volume");
   Is_Tcrypt_Hidden_Volume_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-tcrypt-hidden-volume");
   Domain_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("domain");
   Choice_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("choice");
   Anonymous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("anonymous");
end Glib.GMountOperations;
