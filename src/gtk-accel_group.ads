-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

--  <c_version>partial 1.3.11</c_version>

with Gdk; use Gdk;
with Gdk.Types;
with Gtk.Object;

package Gtk.Accel_Group is

   type Gtk_Accel_Group_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Accel_Group is access all Gtk_Accel_Group_Record'Class;
   type Gtk_Accel_Group_Entry is new Gdk.C_Proxy;

   type Gtk_Accel_Flags is new Guint;
   Accel_Visible : constant Gtk_Accel_Flags;
   Accel_Locked  : constant Gtk_Accel_Flags;
   Accel_Mask    : constant Gtk_Accel_Flags;

   ------------------------
   -- Accelerator Groups --
   ------------------------

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Accel_Group.

   procedure Gtk_New (Accel_Group : out Gtk_Accel_Group);

   procedure Initialize (Accel_Group : access Gtk_Accel_Group_Record'Class);

   procedure Lock (Accel_Group : access Gtk_Accel_Group_Record);

   procedure Unlock (Accel_Group : access Gtk_Accel_Group_Record);

   --  ??? To bind:

   --  procedure Connect
   --    (GtkAccelGroup  *accel_group,
   --     guint           accel_key,
   --     GdkModifierType accel_mods,
   --     GtkAccelFlags   accel_flags,
   --     GClosure       *closure);

   --  procedure Connect_By_Path
   --    (GtkAccelGroup  *accel_group,
   --     const gchar    *accel_path,
   --     GClosure       *closure);

   --  function Disconnect
   --    (GtkAccelGroup  *accel_group,
   --     GClosure       *closure) return Boolean;

   --  function Disconnect_Key
   --    (GtkAccelGroup  *accel_group,
   --     guint           accel_key,
   --     GdkModifierType accel_mods) return Boolean;

   --------------------------
   -- Gtk_Activatable glue --
   --------------------------

   function Accel_Groups_Activate
     (Object     : access Gtk.Object.Gtk_Object_Record'Class;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type) return Boolean;

   --  ??? To bind:

   --  function Accel_Groups_From_Object
   --    (GObject *object) return GSlist*;

   --  function Find
   --   (Accel_Group : access Gtk_Accel_Group_Record,
   --    gboolean (*find_func)
   --      (GtkAccelKey *key, GClosure *closure, gpointer data),
   --    gpointer data) return Gtk_Accel_Key;

   --  function From_Accel_Closure
   --    (GClosure *closure) return Gtk_Accel_Group;

   ------------------
   -- Accelerators --
   ------------------

   function Accelerator_Valid
     (Keyval    : Gdk.Types.Gdk_Key_Type;
      Modifiers : Gdk.Types.Gdk_Modifier_Type) return Boolean;

   procedure Accelerator_Parse
     (Accelerator      : String;
      Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
      Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type);

   function Accelerator_Name
     (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
      Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return String;

   procedure Accelerator_Set_Default_Mod_Mask
     (Default_Mod_Mask : Gdk.Types.Gdk_Modifier_Type);

   function Accelerator_Get_Default_Mod_Mask
     return Gdk.Types.Gdk_Modifier_Type;

private

   type Gtk_Accel_Group_Record is new Glib.Object.GObject_Record with
     null record;

   Accel_Visible : constant Gtk_Accel_Flags := 2 ** 0;
   Accel_Locked  : constant Gtk_Accel_Flags := 2 ** 1;
   Accel_Mask    : constant Gtk_Accel_Flags := 16#07#;

   pragma Import (C, Get_Type, "gtk_accel_group_get_type");

   pragma Import (C, Accelerator_Set_Default_Mod_Mask,
                  "gtk_accelerator_set_default_mod_mask");
   pragma Import (C, Accelerator_Get_Default_Mod_Mask,
                  "gtk_accelerator_get_default_mod_mask");
end Gtk.Accel_Group;
