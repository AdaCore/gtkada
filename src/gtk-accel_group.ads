-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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

--  <description>
--  An accel group represents a group of keyboard accelerators, generally
--  attached to a toplevel window.
--  Accelerators are different from mnemonics. Accelerators are shortcuts for
--  activating a menu item. They appear alongside the menu item they are a
--  shortcut for. Mnemonics are shortcuts for GUI elements, such as buttons.
--  They appear as underline characters. Menu items can have both.
--  </description>
--  <c_version>2.8.17</c_version>

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

   type Gtk_Accel_Key is record
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
      Flags      : Gtk_Accel_Flags;
   end record;
   pragma Convention (C, Gtk_Accel_Key);

   type Gtk_Accel_Group_Activate is access function
     (Accel_Group   : access Gtk_Accel_Group_Record'Class;
      Acceleratable : Glib.Object.GObject;
      Keyval        : Gdk.Types.Gdk_Key_Type;
      Modifier      : Gdk.Types.Gdk_Modifier_Type) return Boolean;

   procedure Gtk_New (Accel_Group : out Gtk_Accel_Group);
   procedure Initialize (Accel_Group : access Gtk_Accel_Group_Record'Class);
   --  Remember to call Gtk.Window.Add_Accel_Group to active the group.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Accel_Group.

   procedure Lock (Accel_Group : access Gtk_Accel_Group_Record);
   procedure Unlock (Accel_Group : access Gtk_Accel_Group_Record);
   --  Locks or unlocks the group.  When a group is locked, the accelerators
   --  contained in it cannot be changed at runtime by the user. See
   --  Gtk_Accel_Map.Change_Entry about runtime accelerator changes.
   --  Unlock must be called the same number of time that Lock was called.

   ------------
   -- Groups --
   ------------

   function Accel_Groups_Activate
     (Object     : access Gtk.Object.Gtk_Object_Record'Class;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   --  Find the first accelerator in any group, attached to Object that matches
   --  the given key and modifier, and activate that accelerator.
   --  Returns True if an accelerator was activated.

   function From_Object
     (Object : access Gtk.Object.Gtk_Object_Record'Class)
      return Object_List.GSlist;
   --  Gets a list of all accel groups which are attached to Object.

   ------------------
   -- Accelerators --
   ------------------

   function Accelerator_Valid
     (Keyval    : Gdk.Types.Gdk_Key_Type;
      Modifiers : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   --  Determines whether a given keyval and modifier constitute a valid
   --  accelerator. For instance, GDK_Control_L is not a valid accelerator,
   --  whereas Gdk_L associated with Control_Mask is valid.

   procedure Accelerator_Parse
     (Accelerator      : String;
      Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
      Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type);
   --  Parse a string representing an accelerator. The format looks like
   --  "<Control>a", "<Shift><Alt>a" or "<Release>z" (the last one applies to
   --  a key release. Abbreviations such as "Ctrl" are allowed.

   function Accelerator_Name
     (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
      Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return String;
   --  Converts an accelerator keyval and modifier mask into a string parseable
   --  by Accelerator_Parse. For example, if you pass in GDK_q and
   --  GDK_CONTROL_MASK, this function returns "<Control>q".
   --  If you need to display accelerators in the user interface, see
   --  Accelerator_Get_Label.

   function Accelerator_Get_Label
     (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
      Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return String;
   --  Converts an accelerator keyval and modifier mask into a string
   --  which can be used to represent the accelerator to the user.

   procedure Set_Default_Mod_Mask
     (Default_Mod_Mask : Gdk.Types.Gdk_Modifier_Type);
   function Get_Default_Mod_Mask return Gdk.Types.Gdk_Modifier_Type;
   --  Sets the modifiers that will be considered significant for keyboard
   --  accelerators. The default mod mask is GDK_CONTROL_MASK | GDK_SHIFT_MASK
   --  | GDK_MOD1_MASK, that is, Control, Shift, and Alt. Other modifiers will
   --  by default be ignored by GtkAccelGroup. You must include at least the
   --  three default modifiers in any value you pass to this function.
   --
   --  The default mod mask should be changed on application startup, before
   --  using any accelerator groups.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "accel_activate"
   --    procedure Handler
   --      (Group         : access Gtk_Accel_Group_Record'Class;
   --       Acceleratable : access GObject_Record'Class;
   --       Keyval        : Gdk_Key_Type;
   --       Modifier      : Gdk_Modifier_Type);
   --    This is an implementation detail, not meant to be used by applications
   --
   --  - "accel_changed"
   --    procedure Handler
   --      (Group         : access Gtk_Accel_Group_Record'Class;
   --       Keyval        : Gdk_Key_Type;
   --       Modifier      : Gdk_Modifier_Type;
   --       Closure       : GClosure);
   --    Emitted when a Gtk_Accel_Group_Entry is added to or removed from the
   --    accel group.
   --    Widgets like Gtk_Accel_Label which display an associated accelerator
   --    should connect to this signal, and rebuild their visual representation
   --    if the accel_closure is theirs.
   --  </signals>

   Signal_Accel_Activate : constant String := "accel_activate";
   Signal_Accel_Changed  : constant String := "accel_changed";

private

   type Gtk_Accel_Group_Record is new Glib.Object.GObject_Record with
     null record;

   Accel_Visible : constant Gtk_Accel_Flags := 2 ** 0;
   Accel_Locked  : constant Gtk_Accel_Flags := 2 ** 1;
   Accel_Mask    : constant Gtk_Accel_Flags := 16#07#;

   pragma Import (C, Get_Type, "gtk_accel_group_get_type");

   pragma Import
     (C, Set_Default_Mod_Mask, "gtk_accelerator_set_default_mod_mask");
   pragma Import
     (C, Get_Default_Mod_Mask, "gtk_accelerator_get_default_mod_mask");
end Gtk.Accel_Group;

--  This function is mostly internal, better used through
--  gtk_accel_groups_activate
--  No binding: gtk_accel_group_activate

--  We are missing a binding of GClosure for the following functions
--  No binding: gtk_accel_group_connect
--  No binding: gtk_accel_group_connect_by_path
--  No binding: gtk_accel_group_disconnect
--  No binding: gtk_accel_group_disconnect_key
--  No binding: gtk_accel_group_find
--  No binding: gtk_accel_group_from_accel_closure
--  No binding: gtk_accel_group_query
