------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  A Gtk.Accel_Group.Gtk_Accel_Group represents a group of keyboard
--  accelerators, typically attached to a toplevel Gtk.Window.Gtk_Window (with
--  Gtk.Window.Add_Accel_Group). Usually you won't need to create a
--  Gtk.Accel_Group.Gtk_Accel_Group directly; instead, when using
--  Gtk.UI_Manager.Gtk_UI_Manager, GTK+ automatically sets up the accelerators
--  for your menus in the ui manager's Gtk.Accel_Group.Gtk_Accel_Group.
--
--  Note that "accelerators" are different from "mnemonics". Accelerators are
--  shortcuts for activating a menu item; they appear alongside the menu item
--  they're a shortcut for. For example "Ctrl+Q" might appear alongside the
--  "Quit" menu item. Mnemonics are shortcuts for GUI elements such as text
--  entries or buttons; they appear as underlined characters. See
--  Gtk.Label.Gtk_New_With_Mnemonic. Menu items can have both accelerators and
--  mnemonics, of course.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Types;               use Gdk.Types;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;

package Gtk.Accel_Group is

   type Gtk_Accel_Group_Record is new GObject_Record with null record;
   type Gtk_Accel_Group is access all Gtk_Accel_Group_Record'Class;

   type Gtk_Accel_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Accel_Flags);
   --  Accelerator flags used with Gtk.Accel_Group.Connect.

   Accel_Visible : constant Gtk_Accel_Flags := 1;
   Accel_Locked : constant Gtk_Accel_Flags := 2;
   Accel_Mask : constant Gtk_Accel_Flags := 7;

   type Gtk_Accel_Key is record
      Accel_Key : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
      Accel_Flags : Gtk_Accel_Flags;
   end record;
   pragma Convention (C, Gtk_Accel_Key);

   function From_Object_Free (B : access Gtk_Accel_Key) return Gtk_Accel_Key;
   pragma Inline (From_Object_Free);


   type Gtk_Accel_Group_Activate is access function
     (Accel_Group   : access Gtk_Accel_Group_Record'Class;
      Acceleratable : Glib.Object.GObject;
      Keyval        : Gdk.Types.Gdk_Key_Type;
      Modifier      : Gdk.Types.Gdk_Modifier_Type) return Boolean;

   type C_Gtk_Accel_Group_Activate is access function
     (Accel_Group   : System.Address;
      Acceleratable : System.Address;
      Keyval        : Gdk.Types.Gdk_Key_Type;
      Modifier      : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   pragma Convention (C, C_Gtk_Accel_Group_Activate);
   --  Same as Gtk_Accel_Group_Activate, but passing directly the C values.
   --  You must use Get_User_Data to convert to the Ada types.

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Accel_Group_Find_Func is access function
     (Key     : Gtk_Accel_Key;
      Closure : System.Address) return Boolean;
   --  Since: gtk+ 2.2

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Accel_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Accel_Flags);
   type Property_Gtk_Accel_Flags is new Gtk_Accel_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Accel_Group : out Gtk_Accel_Group);
   procedure Initialize
      (Accel_Group : not null access Gtk_Accel_Group_Record'Class);
   --  Creates a new Gtk.Accel_Group.Gtk_Accel_Group. Remember to call
   --  Gtk.Window.Add_Accel_Group to active the group.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Accel_Group_New return Gtk_Accel_Group;
   --  Creates a new Gtk.Accel_Group.Gtk_Accel_Group. Remember to call
   --  Gtk.Window.Add_Accel_Group to active the group.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_accel_group_get_type");

   -------------
   -- Methods --
   -------------

   function Activate
      (Accel_Group   : not null access Gtk_Accel_Group_Record;
       Accel_Quark   : Glib.GQuark;
       Acceleratable : not null access Glib.Object.GObject_Record'Class;
       Accel_Key     : Gdk.Types.Gdk_Key_Type;
       Accel_Mods    : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   --  Finds the first accelerator in Accel_Group that matches Accel_Key and
   --  Accel_Mods, and activates it.
   --  "accel_quark": the quark for the accelerator name
   --  "acceleratable": the Glib.Object.GObject, usually a
   --  Gtk.Window.Gtk_Window, on which to activate the accelerator
   --  "accel_key": accelerator keyval from a key event
   --  "accel_mods": keyboard state mask from a key event

   procedure Connect
      (Accel_Group : not null access Gtk_Accel_Group_Record;
       Accel_Key   : Gdk.Types.Gdk_Key_Type;
       Accel_Mods  : Gdk.Types.Gdk_Modifier_Type;
       Accel_Flags : Gtk_Accel_Flags;
       Closure     : C_Gtk_Accel_Group_Activate);
   --  Installs an accelerator in this group. When Accel_Group is being
   --  activated in response to a call to
   --  Gtk.Accel_Group.Accel_Groups_Activate, Closure will be invoked if the
   --  Accel_Key and Accel_Mods from Gtk.Accel_Group.Accel_Groups_Activate
   --  match those of this connection.
   --  The signature used for the Closure is that of Gtk_Accel_Group_Activate.
   --  Note that, due to implementation details, a single closure can only be
   --  connected to one accelerator group.
   --  "accel_key": key value of the accelerator
   --  "accel_mods": modifier combination of the accelerator
   --  "accel_flags": a flag mask to configure this accelerator
   --  "closure": closure to be executed upon accelerator activation

   procedure Connect_By_Path
      (Accel_Group : not null access Gtk_Accel_Group_Record;
       Accel_Path  : UTF8_String;
       Closure     : C_Gtk_Accel_Group_Activate);
   --  Installs an accelerator in this group, using an accelerator path to
   --  look up the appropriate key and modifiers (see Gtk.Accel_Map.Add_Entry).
   --  When Accel_Group is being activated in response to a call to
   --  Gtk.Accel_Group.Accel_Groups_Activate, Closure will be invoked if the
   --  Accel_Key and Accel_Mods from Gtk.Accel_Group.Accel_Groups_Activate
   --  match the key and modifiers for the path.
   --  The signature used for the Closure is that of Gtk_Accel_Group_Activate.
   --  Note that Accel_Path string will be stored in a Glib.GQuark. Therefore,
   --  if you pass a static string, you can save some memory by interning it
   --  first with g_intern_static_string.
   --  "accel_path": path used for determining key and modifiers
   --  "closure": closure to be executed upon accelerator activation

   function Disconnect
      (Accel_Group : not null access Gtk_Accel_Group_Record;
       Closure     : C_Gtk_Accel_Group_Activate) return Boolean;
   --  Removes an accelerator previously installed through
   --  Gtk.Accel_Group.Connect.
   --  Since 2.20 Closure can be null.
   --  "closure": the closure to remove from this accelerator group, or null
   --  to remove all closures

   function Disconnect_Key
      (Accel_Group : not null access Gtk_Accel_Group_Record;
       Accel_Key   : Gdk.Types.Gdk_Key_Type;
       Accel_Mods  : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   --  Removes an accelerator previously installed through
   --  Gtk.Accel_Group.Connect.
   --  "accel_key": key value of the accelerator
   --  "accel_mods": modifier combination of the accelerator

   function Find
      (Accel_Group : not null access Gtk_Accel_Group_Record;
       Find_Func   : Gtk_Accel_Group_Find_Func) return Gtk_Accel_Key;
   --  Finds the first entry in an accelerator group for which Find_Func
   --  returns True and returns its Gtk.Accel_Group.Gtk_Accel_Key.
   --  "find_func": a function to filter the entries of Accel_Group with

   function Get_Is_Locked
      (Accel_Group : not null access Gtk_Accel_Group_Record) return Boolean;
   --  Locks are added and removed using Gtk.Accel_Group.Lock and
   --  Gtk.Accel_Group.Unlock.
   --  Since: gtk+ 2.14

   function Get_Modifier_Mask
      (Accel_Group : not null access Gtk_Accel_Group_Record)
       return Gdk.Types.Gdk_Modifier_Type;
   --  Gets a Gdk.Types.Gdk_Modifier_Type representing the mask for this
   --  Accel_Group. For example, GDK_CONTROL_MASK, GDK_SHIFT_MASK, etc.
   --  Since: gtk+ 2.14

   procedure Lock (Accel_Group : not null access Gtk_Accel_Group_Record);
   --  Locks the given accelerator group.
   --  Locking an acelerator group prevents the accelerators contained within
   --  it to be changed during runtime. Refer to Gtk.Accel_Map.Change_Entry
   --  about runtime accelerator changes.
   --  If called more than once, Accel_Group remains locked until
   --  Gtk.Accel_Group.Unlock has been called an equivalent number of times.

   procedure Unlock (Accel_Group : not null access Gtk_Accel_Group_Record);
   --  Undoes the last call to Gtk.Accel_Group.Lock on this Accel_Group.

   ---------------
   -- Functions --
   ---------------

   function From_Accel_Closure
      (Closure : C_Gtk_Accel_Group_Activate) return Gtk_Accel_Group;
   --  Finds the Gtk.Accel_Group.Gtk_Accel_Group to which Closure is
   --  connected; see Gtk.Accel_Group.Connect.
   --  "closure": a GClosure

   function Accel_Groups_Activate
      (Object     : not null access Glib.Object.GObject_Record'Class;
       Accel_Key  : Gdk.Types.Gdk_Key_Type;
       Accel_Mods : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   --  Finds the first accelerator in any Gtk.Accel_Group.Gtk_Accel_Group
   --  attached to Object that matches Accel_Key and Accel_Mods, and activates
   --  that accelerator.
   --  "object": the Glib.Object.GObject, usually a Gtk.Window.Gtk_Window, on
   --  which to activate the accelerator
   --  "accel_key": accelerator keyval from a key event
   --  "accel_mods": keyboard state mask from a key event

   function From_Object
      (Object : not null access Glib.Object.GObject_Record'Class)
       return Glib.Object.Object_List.GSlist;
   --  Gets a list of all accel groups which are attached to Object.
   --  "object": a Glib.Object.GObject, usually a Gtk.Window.Gtk_Window

   function Accelerator_Valid
      (Keyval    : Gdk.Types.Gdk_Key_Type;
       Modifiers : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   --  Determines whether a given keyval and modifier mask constitute a valid
   --  keyboard accelerator. For example, the GDK_KEY_a keyval plus
   --  GDK_CONTROL_MASK is valid - this is a "Ctrl+a" accelerator. But, you
   --  can't, for instance, use the GDK_KEY_Control_L keyval as an accelerator.
   --  "keyval": a GDK keyval
   --  "modifiers": modifier mask

   procedure Accelerator_Parse
      (Accelerator      : UTF8_String;
       Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type);
   --  Parses a string representing an accelerator. The format looks like
   --  "<Control>a" or "<Shift><Alt>F1" or "<Release>z" (the last one is for
   --  key release).
   --  The parser is fairly liberal and allows lower or upper case, and also
   --  abbreviations such as "<Ctl>" and "<Ctrl>". Key names are parsed using
   --  gdk_keyval_from_name. For character keys the name is not the symbol, but
   --  the lowercase name, e.g. one would use "<Ctrl>minus" instead of
   --  "<Ctrl>-".
   --  If the parse fails, Accelerator_Key and Accelerator_Mods will be set to
   --  0 (zero).
   --  "accelerator": string representing an accelerator
   --  "accelerator_key": return location for accelerator keyval, or null
   --  "accelerator_mods": return location for accelerator modifier mask, null

   function Accelerator_Name
      (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return UTF8_String;
   --  Converts an accelerator keyval and modifier mask into a string
   --  parseable by Gtk.Accel_Group.Accelerator_Parse. For example, if you pass
   --  in GDK_KEY_q and GDK_CONTROL_MASK, this function returns "<Control>q".
   --  If you need to display accelerators in the user interface, see
   --  Gtk.Accel_Group.Accelerator_Get_Label.
   --  "accelerator_key": accelerator keyval
   --  "accelerator_mods": accelerator modifier mask

   function Accelerator_Get_Label
      (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return UTF8_String;
   --  Converts an accelerator keyval and modifier mask into a string which
   --  can be used to represent the accelerator to the user.
   --  Since: gtk+ 2.6
   --  "accelerator_key": accelerator keyval
   --  "accelerator_mods": accelerator modifier mask

   procedure Set_Default_Mod_Mask
      (Default_Mod_Mask : Gdk.Types.Gdk_Modifier_Type);
   --  Sets the modifiers that will be considered significant for keyboard
   --  accelerators. The default mod mask depends on the GDK backend in use,
   --  but will typically include GDK_CONTROL_MASK | GDK_SHIFT_MASK |
   --  GDK_MOD1_MASK | GDK_SUPER_MASK | GDK_HYPER_MASK | GDK_META_MASK. In
   --  other words, Control, Shift, Alt, Super, Hyper and Meta. Other modifiers
   --  will by default be ignored by Gtk.Accel_Group.Gtk_Accel_Group.
   --  You must include at least the three modifiers Control, Shift and Alt in
   --  any value you pass to this function.
   --  The default mod mask should be changed on application startup, before
   --  using any accelerator groups.
   --  "default_mod_mask": accelerator modifier mask

   function Get_Default_Mod_Mask return Gdk.Types.Gdk_Modifier_Type;
   --  Gets the modifier mask.
   --  The modifier mask determines which modifiers are considered significant
   --  for keyboard accelerators. See Gtk.Accel_Group.Set_Default_Mod_Mask.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Is_Locked_Property : constant Glib.Properties.Property_Boolean;

   Modifier_Mask_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Types.Gdk_Modifier_Type

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean is not null access function
     (Self          : access Gtk_Accel_Group_Record'Class;
      Acceleratable : not null access Glib.Object.GObject_Record'Class;
      Keyval        : Guint;
      Modifier      : Gdk.Types.Gdk_Modifier_Type) return Boolean;

   type Cb_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean is not null access function
     (Self          : access Glib.Object.GObject_Record'Class;
      Acceleratable : not null access Glib.Object.GObject_Record'Class;
      Keyval        : Guint;
      Modifier      : Gdk.Types.Gdk_Modifier_Type) return Boolean;

   Signal_Accel_Activate : constant Glib.Signal_Name := "accel-activate";
   procedure On_Accel_Activate
      (Self  : not null access Gtk_Accel_Group_Record;
       Call  : Cb_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean;
       After : Boolean := False);
   procedure On_Accel_Activate
      (Self  : not null access Gtk_Accel_Group_Record;
       Call  : Cb_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The accel-activate signal is an implementation detail of
   --  Gtk.Accel_Group.Gtk_Accel_Group and not meant to be used by
   --  applications.
   -- 
   --  Callback parameters:
   --    --  "acceleratable": the object on which the accelerator was activated
   --    --  "keyval": the accelerator keyval
   --    --  "modifier": the modifier combination of the accelerator
   --    --  Returns True if the accelerator was activated

   type Cb_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void is not null access procedure
     (Self          : access Gtk_Accel_Group_Record'Class;
      Keyval        : Guint;
      Modifier      : Gdk.Types.Gdk_Modifier_Type;
      Accel_Closure : System.Address);

   type Cb_GObject_Guint_Gdk_Modifier_Type_Address_Void is not null access procedure
     (Self          : access Glib.Object.GObject_Record'Class;
      Keyval        : Guint;
      Modifier      : Gdk.Types.Gdk_Modifier_Type;
      Accel_Closure : System.Address);

   Signal_Accel_Changed : constant Glib.Signal_Name := "accel-changed";
   procedure On_Accel_Changed
      (Self  : not null access Gtk_Accel_Group_Record;
       Call  : Cb_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void;
       After : Boolean := False);
   procedure On_Accel_Changed
      (Self  : not null access Gtk_Accel_Group_Record;
       Call  : Cb_GObject_Guint_Gdk_Modifier_Type_Address_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The accel-changed signal is emitted when an entry is added to or
   --  removed from the accel group.
   --
   --  Widgets like Gtk.Accel_Label.Gtk_Accel_Label which display an
   --  associated accelerator should connect to this signal, and rebuild their
   --  visual representation if the Accel_Closure is theirs.
   -- 
   --  Callback parameters:
   --    --  "keyval": the accelerator keyval
   --    --  "modifier": the modifier combination of the accelerator
   --    --  "accel_closure": the GClosure of the accelerator

private
   Modifier_Mask_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("modifier-mask");
   Is_Locked_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-locked");
end Gtk.Accel_Group;
