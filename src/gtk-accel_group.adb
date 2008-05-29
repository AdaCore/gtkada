-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                  Copyright (C) 2000-2008, AdaCore                 --
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

with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);
with Gtk.Object; use Gtk.Object;

package body Gtk.Accel_Group is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Accel_Group_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   ---------------------------
   -- Accel_Groups_Activate --
   ---------------------------

   function Accel_Groups_Activate
     (Object     : access Gtk.Object.Gtk_Object_Record'Class;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
        (Object     : System.Address;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type) return Gboolean;
      pragma Import (C, Internal, "gtk_accel_groups_activate");

   begin
      return Internal (Get_Object (Object), Accel_Key, Accel_Mods) /= 0;
   end Accel_Groups_Activate;

   ----------------------
   -- Accelerator_Name --
   ----------------------

   function Accelerator_Name
     (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
      Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return String
   is
      function Internal
        (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
         Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_accelerator_name");
   begin
      return Interfaces.C.Strings.Value
        (Internal (Accelerator_Key, Accelerator_Mods));
   end Accelerator_Name;

   ---------------------------
   -- Accelerator_Get_Label --
   ---------------------------

   function Accelerator_Get_Label
     (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
      Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return String
   is
      function Internal
        (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
         Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_accelerator_get_label");
   begin
      return Interfaces.C.Strings.Value
        (Internal (Accelerator_Key, Accelerator_Mods));
   end Accelerator_Get_Label;

   -----------------------
   -- Accelerator_Parse --
   -----------------------

   procedure Accelerator_Parse
      (Accelerator      : String;
       Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
        (Accelerator      : String;
         Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
         Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_accelerator_parse");

   begin
      Internal (Accelerator & ASCII.NUL, Accelerator_Key, Accelerator_Mods);
   end Accelerator_Parse;

   -----------------------
   -- Accelerator_Valid --
   -----------------------

   function Accelerator_Valid
     (Keyval    : Gdk.Types.Gdk_Key_Type;
      Modifiers : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
        (Keyval    : Gdk.Types.Gdk_Key_Type;
         Modifiers : Gdk.Types.Gdk_Modifier_Type) return Gint;
      pragma Import (C, Internal, "gtk_accelerator_valid");

   begin
      return Boolean'Val (Internal (Keyval, Modifiers));
   end Accelerator_Valid;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Accel_Group : out Gtk_Accel_Group) is
   begin
      Accel_Group := new Gtk_Accel_Group_Record;
      Gtk.Accel_Group.Initialize (Accel_Group);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Accel_Group : access Gtk_Accel_Group_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_accel_group_new");

   begin
      Set_Object (Accel_Group, Internal);
   end Initialize;

   ----------
   -- Lock --
   ----------

   procedure Lock (Accel_Group : access Gtk_Accel_Group_Record) is
      procedure Internal (Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_accel_group_lock");

   begin
      Internal (Get_Object (Accel_Group));
   end Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Accel_Group : access Gtk_Accel_Group_Record) is
      procedure Internal (Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_accel_group_unlock");

   begin
      Internal (Get_Object (Accel_Group));
   end Unlock;

   -----------------
   -- From_Object --
   -----------------

   function From_Object
     (Object : access Gtk_Object_Record'Class) return Object_List.GSlist
   is
      function Internal (Container : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accel_groups_from_object");
      List : Object_List.GSlist;
   begin
      Object_List.Set_Object (List, Internal (Get_Object (Object)));
      return List;
   end From_Object;

end Gtk.Accel_Group;
