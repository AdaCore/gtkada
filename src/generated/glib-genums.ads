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


pragma Warnings (Off, "*is already use-visible*");
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Glib.GEnums is

   type GMount_Mount_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GMount_Mount_Flags);
   --  Flags used when mounting a mount.

   G_Mount_Mount_None : constant GMount_Mount_Flags := 0;

   type GMount_Unmount_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GMount_Unmount_Flags);
   --  Flags used when an unmounting a mount.

   G_Mount_Unmount_None : constant GMount_Unmount_Flags := 0;
   G_Mount_Unmount_Force : constant GMount_Unmount_Flags := 1;

   type GDrive_Start_Stop_Type is (
      Unknown,
      Shutdown,
      Network,
      Multidisk,
      Password);
   pragma Convention (C, GDrive_Start_Stop_Type);
   --  Enumeration describing how a drive can be started/stopped.

   type GDrive_Start_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GDrive_Start_Flags);
   --  Flags used when starting a drive.

   G_Drive_Start_None : constant GDrive_Start_Flags := 0;

   type GMount_Operation_Result is (
      Handled,
      Aborted,
      Unhandled);
   pragma Convention (C, GMount_Operation_Result);
   --  Glib.GEnums.GMount_Operation_Result is returned as a result when a
   --  request for information is send by the mounting operation.

   type GAsk_Password_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GAsk_Password_Flags);
   --  Glib.GEnums.GAsk_Password_Flags are used to request specific
   --  information from the user, or to notify the user of their choices in an
   --  authentication situation.

   G_Ask_Password_Need_Password : constant GAsk_Password_Flags := 1;
   G_Ask_Password_Need_Username : constant GAsk_Password_Flags := 2;
   G_Ask_Password_Need_Domain : constant GAsk_Password_Flags := 4;
   G_Ask_Password_Saving_Supported : constant GAsk_Password_Flags := 8;
   G_Ask_Password_Anonymous_Supported : constant GAsk_Password_Flags := 16;
   G_Ask_Password_Tcrypt : constant GAsk_Password_Flags := 32;

   type GPassword_Save is (
      Never,
      For_Session,
      Permanently);
   pragma Convention (C, GPassword_Save);
   --  Glib.GEnums.GPassword_Save is used to indicate the lifespan of a saved
   --  password.
   --
   --  Gvfs.Gvfs stores passwords in the Gnome keyring when this flag allows
   --  it to, and later retrieves it again from there.

   type GFile_Monitor_Event is (
      Changed,
      Changes_Done_Hint,
      Deleted,
      Created,
      Attribute_Changed,
      Pre_Unmount,
      Unmounted,
      Moved,
      Renamed,
      Moved_In,
      Moved_Out);
   pragma Convention (C, GFile_Monitor_Event);
   --  Specifies what type of event a monitor event is.

   type GFile_Monitor_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GFile_Monitor_Flags);
   --  Flags used to set what a Glib.File_Monitors.Gfile_Monitor will watch
   --  for.

   G_File_Monitor_None : constant GFile_Monitor_Flags := 0;
   G_File_Monitor_Watch_Mounts : constant GFile_Monitor_Flags := 1;
   G_File_Monitor_Send_Moved : constant GFile_Monitor_Flags := 2;
   G_File_Monitor_Watch_Hard_Links : constant GFile_Monitor_Flags := 4;
   G_File_Monitor_Watch_Moves : constant GFile_Monitor_Flags := 8;

   type GFile_Attribute_Type is (
      Invalid,
      String,
      Byte_String,
      Boolean,
      Uint32,
      Int32,
      Uint64,
      Int64,
      Object,
      Stringv);
   pragma Convention (C, GFile_Attribute_Type);
   --  The data types for file attributes.

   type GFile_Attribute_Status is (
      Unset,
      Set,
      Error_Setting);
   pragma Convention (C, GFile_Attribute_Status);
   --  Used by Glib.GFile.Set_Attributes_From_Info when setting file
   --  attributes.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GMount_Mount_Flags_Properties is
      new Generic_Internal_Flags_Property (GMount_Mount_Flags);
   type Property_GMount_Mount_Flags is new GMount_Mount_Flags_Properties.Property;

   package GMount_Unmount_Flags_Properties is
      new Generic_Internal_Flags_Property (GMount_Unmount_Flags);
   type Property_GMount_Unmount_Flags is new GMount_Unmount_Flags_Properties.Property;

   package GDrive_Start_Stop_Type_Properties is
      new Generic_Internal_Discrete_Property (GDrive_Start_Stop_Type);
   type Property_GDrive_Start_Stop_Type is new GDrive_Start_Stop_Type_Properties.Property;

   package GDrive_Start_Flags_Properties is
      new Generic_Internal_Flags_Property (GDrive_Start_Flags);
   type Property_GDrive_Start_Flags is new GDrive_Start_Flags_Properties.Property;

   package GMount_Operation_Result_Properties is
      new Generic_Internal_Discrete_Property (GMount_Operation_Result);
   type Property_GMount_Operation_Result is new GMount_Operation_Result_Properties.Property;

   package GAsk_Password_Flags_Properties is
      new Generic_Internal_Flags_Property (GAsk_Password_Flags);
   type Property_GAsk_Password_Flags is new GAsk_Password_Flags_Properties.Property;

   package GPassword_Save_Properties is
      new Generic_Internal_Discrete_Property (GPassword_Save);
   type Property_GPassword_Save is new GPassword_Save_Properties.Property;

   package GFile_Monitor_Event_Properties is
      new Generic_Internal_Discrete_Property (GFile_Monitor_Event);
   type Property_GFile_Monitor_Event is new GFile_Monitor_Event_Properties.Property;

   package GFile_Monitor_Flags_Properties is
      new Generic_Internal_Flags_Property (GFile_Monitor_Flags);
   type Property_GFile_Monitor_Flags is new GFile_Monitor_Flags_Properties.Property;

   package GFile_Attribute_Type_Properties is
      new Generic_Internal_Discrete_Property (GFile_Attribute_Type);
   type Property_GFile_Attribute_Type is new GFile_Attribute_Type_Properties.Property;

   package GFile_Attribute_Status_Properties is
      new Generic_Internal_Discrete_Property (GFile_Attribute_Status);
   type Property_GFile_Attribute_Status is new GFile_Attribute_Status_Properties.Property;

end Glib.GEnums;
