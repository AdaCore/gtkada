-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Interfaces.C.Strings;
with System;

package body Gtk.Accel_Group is

   ---------------------------
   -- Accel_Groups_Activate --
   ---------------------------

   function Accel_Groups_Activate
     (Object     : access Gtk.Object.Gtk_Object_Record'Class;
      Accel_Key  : in     Gdk.Types.Gdk_Key_Type;
      Accel_Mods : in     Gdk.Types.Gdk_Modifier_Type)
      return          Boolean
   is
      function Internal
         (Object     : in System.Address;
          Accel_Key  : in Gint;
          Accel_Mods : in Gint)
          return          Gint;
      pragma Import (C, Internal, "gtk_accel_groups_activate");
   begin
      return Boolean'Val
        (Internal (Get_Object (Object),
                   Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                   Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods)));
   end Accel_Groups_Activate;

   ----------------------
   -- Accelerator_Name --
   ----------------------

   function Accelerator_Name
     (Accelerator_Key  : in Gdk.Types.Gdk_Key_Type;
      Accelerator_Mods : in Gdk.Types.Gdk_Modifier_Type)
      return                String
   is
      function Internal
         (Accelerator_Key  : in Gint;
          Accelerator_Mods : in Gint)
          return                Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_accelerator_name");
   begin
      return Interfaces.C.Strings.Value
        (Internal (Gdk.Types.Gdk_Key_Type'Pos (Accelerator_Key),
                   Gdk.Types.Gdk_Modifier_Type'Pos (Accelerator_Mods)));
   end Accelerator_Name;

   -----------------------
   -- Accelerator_Parse --
   -----------------------

   procedure Accelerator_Parse
      (Accelerator      : in     String;
       Accelerator_Key  :    out Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods :    out Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
        (Accelerator      : in     String;
         Accelerator_Key  :    out Gdk.Types.Gdk_Key_Type;
         Accelerator_Mods :    out Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_accelerator_parse");
   begin
      Internal (Accelerator & Ascii.NUL, Accelerator_Key, Accelerator_Mods);
   end Accelerator_Parse;

   -----------------------
   -- Accelerator_Valid --
   -----------------------

   function Accelerator_Valid (Keyval    : in Gdk.Types.Gdk_Key_Type;
                               Modifiers : in Gdk.Types.Gdk_Modifier_Type)
                               return         Boolean
   is
      function Internal (Keyval    : in Gdk.Types.Gdk_Key_Type;
                         Modifiers : in Gdk.Types.Gdk_Modifier_Type)
                         return         Gint;
      pragma Import (C, Internal, "gtk_accelerator_valid");
   begin
      return Boolean'Val (Internal (Keyval, Modifiers));
   end Accelerator_Valid;

   --------------
   -- Activate --
   --------------

   function Activate (Accel_Group : in Gtk_Accel_Group;
                      Accel_Key   : in Gdk.Types.Gdk_Key_Type;
                      Accel_Mods  : in Gdk.Types.Gdk_Modifier_Type)
                      return        Boolean
   is
      function Internal (Accel_Group : in System.Address;
                         Accel_Key   : in Gint;
                         Accel_Mods  : in Gint)
                         return           Gint;
      pragma Import (C, Internal, "gtk_accel_group_activate");
   begin
      return Boolean'Val
        (Internal (Get_Object (Accel_Group),
                   Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                   Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods)));
   end Activate;

   ---------
   -- Add --
   ---------

   procedure Add (Accel_Group  : in     Gtk_Accel_Group;
                  Accel_Key    : in     Gdk.Types.Gdk_Key_Type;
                  Accel_Mods   : in     Gdk.Types.Gdk_Modifier_Type;
                  Accel_Flags  : in     Gtk_Accel_Flags;
                  Object       : access Gtk.Object.Gtk_Object_Record'Class;
                  Accel_Signal : in     String)
   is
      procedure Internal (Accel_Group  : in System.Address;
                          Accel_Key    : in Gint;
                          Accel_Mods   : in Gint;
                          Accel_Flags  : in Gint;
                          Object       : in System.Address;
                          Accel_Signal : in String);
      pragma Import (C, Internal, "gtk_accel_group_add");
   begin
      Internal (Get_Object (Accel_Group),
                Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods),
                Gtk_Accel_Flags'Pos (Accel_Flags),
                Get_Object (Object),
                Accel_Signal & Ascii.NUL);
   end Add;

   ------------
   -- Attach --
   ------------

   procedure Attach (Accel_Group : in     Gtk_Accel_Group;
                     Object      : access Gtk.Object.Gtk_Object_Record'Class)
   is
      procedure Internal (Accel_Group : in System.Address;
                          Object      : in System.Address);
      pragma Import (C, Internal, "gtk_accel_group_attach");
   begin
      Internal (Get_Object (Accel_Group),
                Get_Object (Object));
   end Attach;

   ------------
   -- Detach --
   ------------

   procedure Detach (Accel_Group : in     Gtk_Accel_Group;
                     Object      : access Gtk.Object.Gtk_Object_Record'Class)
   is
      procedure Internal
         (Accel_Group : in System.Address;
          Object      : in System.Address);
      pragma Import (C, Internal, "gtk_accel_group_detach");
   begin
      Internal (Get_Object (Accel_Group),
                Get_Object (Object));
   end Detach;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gtk_Accel_Group is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_accel_group_get_default");
      Stub : Gtk_Accel_Group;
   begin
      Set_Object (Stub, Internal);
      return Stub;
   end Get_Default;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry (Accel_Group : in Gtk_Accel_Group;
                       Accel_Key   : in Gdk.Types.Gdk_Key_Type;
                       Accel_Mods  : in Gdk.Types.Gdk_Modifier_Type)
                       return        Gtk_Accel_Entry'Class
   is
      function Internal (Accel_Group : in System.Address;
                         Accel_Key   : in Gint;
                         Accel_Mods  : in Gint)
                         return           System.Address;
      pragma Import (C, Internal, "gtk_accel_group_get_entry");
      Stub : Gtk_Accel_Entry;
   begin
      Set_Object (Stub,
                  Internal (Get_Object (Accel_Group),
                            Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                            Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods)));
      return Stub;
   end Get_Entry;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Accel_Group) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_accel_group_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ----------------
   -- Handle_Add --
   ----------------

   procedure Handle_Add
     (Object          : access Gtk.Object.Gtk_Object_Record'Class;
      Accel_Signal_Id : in     Guint;
      Accel_Group     : in     Gtk_Accel_Group;
      Accel_Key       : in     Gdk.Types.Gdk_Key_Type;
      Accel_Mods      : in     Gdk.Types.Gdk_Modifier_Type;
      Accel_Flags     : in     Gtk_Accel_Flags)
   is
      procedure Internal (Object          : in System.Address;
                          Accel_Signal_Id : in Guint;
                          Accel_Group     : in System.Address;
                          Accel_Key       : in Gint;
                          Accel_Mods      : in Gint;
                          Accel_Flags     : in Gint);
      pragma Import (C, Internal, "gtk_accel_group_handle_add");
   begin
      Internal (Get_Object (Object),
                Accel_Signal_Id,
                Get_Object (Accel_Group),
                Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods),
                Gtk_Accel_Flags'Pos (Accel_Flags));
   end Handle_Add;

   -------------------
   -- Handle_Remove --
   -------------------

   procedure Handle_Remove
     (Object      : access Gtk.Object.Gtk_Object_Record'Class;
      Accel_Group : in     Gtk_Accel_Group;
      Accel_Key   : in     Gdk.Types.Gdk_Key_Type;
      Accel_Mods  : in     Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal (Object      : in System.Address;
                          Accel_Group : in System.Address;
                          Accel_Key   : in Gint;
                          Accel_Mods  : in Gint);
      pragma Import (C, Internal, "gtk_accel_group_handle_remove");
   begin
      Internal (Get_Object (Object),
                Get_Object (Accel_Group),
                Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods));
   end Handle_Remove;

   ----------
   -- Lock --
   ----------

   procedure Lock (Accel_Group : in Gtk_Accel_Group)
   is
      procedure Internal (Accel_Group : in System.Address);
      pragma Import (C, Internal, "gtk_accel_group_lock");
   begin
      Internal (Get_Object (Accel_Group));
   end Lock;

   ----------------
   -- Lock_Entry --
   ----------------

   procedure Lock_Entry (Accel_Group : in Gtk_Accel_Group;
                         Accel_Key   : in Gdk.Types.Gdk_Key_Type;
                         Accel_Mods  : in Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal (Accel_Group : in System.Address;
                          Accel_Key   : in Gint;
                          Accel_Mods  : in Gint);
      pragma Import (C, Internal, "gtk_accel_group_lock_entry");
   begin
      Internal (Get_Object (Accel_Group),
                Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods));
   end Lock_Entry;

   ------------
   -- Remove --
   ------------

   procedure Remove (Accel_Group : in     Gtk_Accel_Group;
                     Accel_Key   : in     Gdk.Types.Gdk_Key_Type;
                     Accel_Mods  : in     Gdk.Types.Gdk_Modifier_Type;
                     Object      : access Gtk.Object.Gtk_Object_Record'Class)
   is
      procedure Internal (Accel_Group : in System.Address;
                          Accel_Key   : in Gint;
                          Accel_Mods  : in Gint;
                          Object      : in System.Address);
      pragma Import (C, Internal, "gtk_accel_group_remove");
   begin
      Internal (Get_Object (Accel_Group),
                Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods),
                Get_Object (Object));
   end Remove;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Accel_Group : in Gtk_Accel_Group)
   is
      procedure Internal (Accel_Group : in System.Address);
      pragma Import (C, Internal, "gtk_accel_group_unlock");
   begin
      Internal (Get_Object (Accel_Group));
   end Unlock;

   ------------------
   -- Unlock_Entry --
   ------------------

   procedure Unlock_Entry (Accel_Group : in Gtk_Accel_Group;
                           Accel_Key   : in Gdk.Types.Gdk_Key_Type;
                           Accel_Mods  : in Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal (Accel_Group : in System.Address;
                          Accel_Key   : in Gint;
                          Accel_Mods  : in Gint);
      pragma Import (C, Internal, "gtk_accel_group_unlock_entry");
   begin
      Internal (Get_Object (Accel_Group),
                Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods));
   end Unlock_Entry;

end Gtk.Accel_Group;
