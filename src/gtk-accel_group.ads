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

with Gdk; use Gdk;
with Gdk.Types;

with Gtk.Object;

package Gtk.Accel_Group is

   type Gtk_Accel_Group is new Root_Type with private;
   type Gtk_Accel_Entry is new Root_Type with private;

   type Gtk_Accel_Flags is new Guint;
   Accel_Visible        : constant Gtk_Accel_Flags;
   Accel_Signal_Visible : constant Gtk_Accel_Flags;
   Accel_Locked         : constant Gtk_Accel_Flags;
   Accel_Mask           : constant Gtk_Accel_Flags;


   --------------------------
   --  Accelerator Groups  --
   --------------------------

   procedure Gtk_New (Widget : out Gtk_Accel_Group);

   function Get_Default return Gtk_Accel_Group;

   --  procedure Ref
   --  procedure Unref

   function Activate (Accel_Group : in Gtk_Accel_Group;
                      Accel_Key   : in Gdk.Types.Gdk_Key_Type;
                      Accel_Mods  : in Gdk.Types.Gdk_Modifier_Type)
                      return        Boolean;

   function Accel_Groups_Activate
     (Object     : access Gtk.Object.Gtk_Object_Record'Class;
      Accel_Key  : in Gdk.Types.Gdk_Key_Type;
      Accel_Mods : in Gdk.Types.Gdk_Modifier_Type)
      return       Boolean;

   procedure Attach (Accel_Group : in Gtk_Accel_Group;
                     Object      : access Gtk.Object.Gtk_Object_Record'Class);

   procedure Detach (Accel_Group : in Gtk_Accel_Group;
                     Object      : access Gtk.Object.Gtk_Object_Record'Class);

   procedure Lock (Accel_Group : in Gtk_Accel_Group);

   procedure Unlock (Accel_Group : in Gtk_Accel_Group);


   ---------------------------------
   --  Accelerator Group Entries  --
   ---------------------------------

   function Get_Entry (Accel_Group : in Gtk_Accel_Group;
                       Accel_Key   : in Gdk.Types.Gdk_Key_Type;
                       Accel_Mods  : in Gdk.Types.Gdk_Modifier_Type)
                       return        Gtk_Accel_Entry'Class;

   procedure Lock_Entry (Accel_Group : in Gtk_Accel_Group;
                         Accel_Key   : in Gdk.Types.Gdk_Key_Type;
                         Accel_Mods  : in Gdk.Types.Gdk_Modifier_Type);

   procedure Unlock_Entry (Accel_Group : in Gtk_Accel_Group;
                           Accel_Key   : in Gdk.Types.Gdk_Key_Type;
                           Accel_Mods  : in Gdk.Types.Gdk_Modifier_Type);

   procedure Add (Accel_Group  : in Gtk_Accel_Group;
                  Accel_Key    : in     Gdk.Types.Gdk_Key_Type;
                  Accel_Mods   : in     Gdk.Types.Gdk_Modifier_Type;
                  Accel_Flags  : in     Gtk_Accel_Flags;
                  Object       : access Gtk.Object.Gtk_Object_Record'Class;
                  Accel_Signal : in     String);

   procedure Remove (Accel_Group : in Gtk_Accel_Group;
                     Accel_Key   : in     Gdk.Types.Gdk_Key_Type;
                     Accel_Mods  : in     Gdk.Types.Gdk_Modifier_Type;
                     Object      : access Gtk.Object.Gtk_Object_Record'Class);


   ---------------------------
   --  Accelerator Signals  --
   ---------------------------

   procedure Handle_Add
     (Object          : access Gtk.Object.Gtk_Object_Record'Class;
      Accel_Signal_Id : in     Guint;
      Accel_Group     : in     Gtk_Accel_Group;
      Accel_Key       : in     Gdk.Types.Gdk_Key_Type;
      Accel_Mods      : in     Gdk.Types.Gdk_Modifier_Type;
      Accel_Flags     : in     Gtk_Accel_Flags);

   procedure Handle_Remove
     (Object      : access Gtk.Object.Gtk_Object_Record'Class;
      Accel_Group : in     Gtk_Accel_Group;
      Accel_Key   : in     Gdk.Types.Gdk_Key_Type;
      Accel_Mods  : in     Gdk.Types.Gdk_Modifier_Type);

   --  function Create_Add
   --  function Create_Remove
   --  procedure Marshal_Add
   --  procedure Marshal_Remove


   ---------------------
   --  Miscellaneous  --
   ---------------------

   --  function Accel_Groups_From_Object
   --  function Entries_From_Object


   --------------------
   --  Accelerators  --
   --------------------

   function Accelerator_Valid (Keyval    : in Gdk.Types.Gdk_Key_Type;
                               Modifiers : in Gdk.Types.Gdk_Modifier_Type)
                               return         Boolean;

   procedure Accelerator_Parse
      (Accelerator      : in     String;
       Accelerator_Key  :    out Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods :    out Gdk.Types.Gdk_Modifier_Type);

   function Accelerator_Name
      (Accelerator_Key  : in Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : in Gdk.Types.Gdk_Modifier_Type)
       return                String;

   procedure Accelerator_Set_Default_Mod_Mask
     (Default_Mod_Mask : in Gdk.Types.Gdk_Modifier_Type);

   function Accelerator_Get_Default_Mod_Mask
     return Gdk.Types.Gdk_Modifier_Type;

private

   type Gtk_Accel_Group is new Root_Type with null record;
   type Gtk_Accel_Entry is new Root_Type with null record;

   Accel_Visible        : constant Gtk_Accel_Flags := 2 ** 0;
   Accel_Signal_Visible : constant Gtk_Accel_Flags := 2 ** 1;
   Accel_Locked         : constant Gtk_Accel_Flags := 2 ** 2;
   Accel_Mask           : constant Gtk_Accel_Flags := 16#07#;

   pragma Import (C, Accelerator_Set_Default_Mod_Mask,
                  "gtk_accelerator_set_default_mod_mask");
   pragma Import (C, Accelerator_Get_Default_Mod_Mask,
                  "gtk_accelerator_get_default_mod_mask");

end Gtk.Accel_Group;
