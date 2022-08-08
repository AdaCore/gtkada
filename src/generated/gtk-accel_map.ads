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
--  Accelerator maps are used to define runtime configurable accelerators.
--  Functions for manipulating them are are usually used by higher level
--  convenience mechanisms like Gtk.UI_Manager.Gtk_UI_Manager and are thus
--  considered "low-level". You'll want to use them if you're manually creating
--  menus that should have user-configurable accelerators.
--
--  An accelerator is uniquely defined by: - accelerator path - accelerator
--  key - accelerator modifiers
--
--  The accelerator path must consist of
--  "<WINDOWTYPE>/Category1/Category2/.../Action", where WINDOWTYPE should be a
--  unique application-specific identifier that corresponds to the kind of
--  window the accelerator is being used in, e.g. "Gimp-Image",
--  "Abiword-Document" or "Gnumeric-Settings". The "Category1/.../Action"
--  portion is most appropriately chosen by the action the accelerator
--  triggers, i.e. for accelerators on menu items, choose the item's menu path,
--  e.g. "File/Save As", "Image/View/Zoom" or "Edit/Select All". So a full
--  valid accelerator path may look like: "<Gimp-Toolbox>/File/Dialogs/Tool
--  Options...".
--
--  All accelerators are stored inside one global Gtk.Accel_Map.Gtk_Accel_Map
--  that can be obtained using Gtk.Accel_Map.Get. See [Monitoring
--  changes][monitoring-changes] for additional details.
--
--  # Manipulating accelerators
--
--  New accelerators can be added using Gtk.Accel_Map.Add_Entry. To search for
--  specific accelerator, use Gtk.Accel_Map.Lookup_Entry. Modifications of
--  existing accelerators should be done using Gtk.Accel_Map.Change_Entry.
--
--  In order to avoid having some accelerators changed, they can be locked
--  using Gtk.Accel_Map.Lock_Path. Unlocking is done using
--  Gtk.Accel_Map.Unlock_Path.
--
--  # Saving and loading accelerator maps
--
--  Accelerator maps can be saved to and loaded from some external resource.
--  For simple saving and loading from file, Gtk.Accel_Map.Save and
--  Gtk.Accel_Map.Load are provided. Saving and loading can also be done by
--  providing file descriptor to Gtk.Accel_Map.Save_Fd and
--  Gtk.Accel_Map.Load_Fd.
--
--  # Monitoring changes
--
--  Gtk.Accel_Map.Gtk_Accel_Map object is only useful for monitoring changes
--  of accelerators. By connecting to Gtk.Accel_Map.Gtk_Accel_Map::changed
--  signal, one can monitor changes of all accelerators. It is also possible to
--  monitor only single accelerator path by using it as a detail of the
--  Gtk.Accel_Map.Gtk_Accel_Map::changed signal.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Types;       use Gdk.Types;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Gtk.Accel_Group; use Gtk.Accel_Group;

package Gtk.Accel_Map is

   type Gtk_Accel_Map_Record is new GObject_Record with null record;
   type Gtk_Accel_Map is access all Gtk_Accel_Map_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Accel_Map_Foreach is access procedure
     (Accel_Path : UTF8_String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
      Changed    : Boolean);
   --  "accel_path": Accel path of the current accelerator
   --  "accel_key": Key of the current accelerator
   --  "accel_mods": Modifiers of the current accelerator
   --  "changed": Changed flag of the accelerator (if True, accelerator has
   --  changed during runtime and would need to be saved during an accelerator
   --  dump)

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_accel_map_get_type");

   -------------
   -- Methods --
   -------------

   procedure Foreach (Foreach_Func : Gtk_Accel_Map_Foreach);
   --  Loops over the entries in the accelerator map whose accel path doesn't
   --  match any of the filters added with Gtk.Accel_Map.Add_Filter, and
   --  execute Foreach_Func on each. The signature of Foreach_Func is that of
   --  Gtk_Accel_Map_Foreach, the Changed parameter indicates whether this
   --  accelerator was changed during runtime (thus, would need saving during
   --  an accelerator map dump).
   --  "foreach_func": function to be executed for each accel map entry which
   --  is not filtered out

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_User_Data is

      type Gtk_Accel_Map_Foreach is access procedure
        (Data       : User_Data_Type;
         Accel_Path : UTF8_String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean);
      --  "data": User data passed to Gtk.Accel_Map.Foreach or
      --  gtk_accel_map_foreach_unfiltered
      --  "accel_path": Accel path of the current accelerator
      --  "accel_key": Key of the current accelerator
      --  "accel_mods": Modifiers of the current accelerator
      --  "changed": Changed flag of the accelerator (if True, accelerator has
      --  changed during runtime and would need to be saved during an accelerator
      --  dump)

      procedure Foreach
         (Data         : User_Data_Type;
          Foreach_Func : Gtk_Accel_Map_Foreach);
      --  Loops over the entries in the accelerator map whose accel path
      --  doesn't match any of the filters added with Gtk.Accel_Map.Add_Filter,
      --  and execute Foreach_Func on each. The signature of Foreach_Func is
      --  that of Gtk_Accel_Map_Foreach, the Changed parameter indicates
      --  whether this accelerator was changed during runtime (thus, would need
      --  saving during an accelerator map dump).
      --  "data": data to be passed into Foreach_Func
      --  "foreach_func": function to be executed for each accel map entry
      --  which is not filtered out

   end Foreach_User_Data;

   procedure Foreach_Unfiltered (Foreach_Func : Gtk_Accel_Map_Foreach);
   --  Loops over all entries in the accelerator map, and execute Foreach_Func
   --  on each. The signature of Foreach_Func is that of Gtk_Accel_Map_Foreach,
   --  the Changed parameter indicates whether this accelerator was changed
   --  during runtime (thus, would need saving during an accelerator map dump).
   --  "foreach_func": function to be executed for each accel map entry

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_Unfiltered_User_Data is

      type Gtk_Accel_Map_Foreach is access procedure
        (Data       : User_Data_Type;
         Accel_Path : UTF8_String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean);
      --  "data": User data passed to Gtk.Accel_Map.Foreach or
      --  Gtk.Accel_Map.Foreach_Unfiltered
      --  "accel_path": Accel path of the current accelerator
      --  "accel_key": Key of the current accelerator
      --  "accel_mods": Modifiers of the current accelerator
      --  "changed": Changed flag of the accelerator (if True, accelerator has
      --  changed during runtime and would need to be saved during an accelerator
      --  dump)

      procedure Foreach_Unfiltered
         (Data         : User_Data_Type;
          Foreach_Func : Gtk_Accel_Map_Foreach);
      --  Loops over all entries in the accelerator map, and execute
      --  Foreach_Func on each. The signature of Foreach_Func is that of
      --  Gtk_Accel_Map_Foreach, the Changed parameter indicates whether this
      --  accelerator was changed during runtime (thus, would need saving
      --  during an accelerator map dump).
      --  "data": data to be passed into Foreach_Func
      --  "foreach_func": function to be executed for each accel map entry

   end Foreach_Unfiltered_User_Data;

   ---------------
   -- Functions --
   ---------------

   procedure Add_Entry
      (Accel_Path : UTF8_String;
       Accel_Key  : Gdk.Types.Gdk_Key_Type;
       Accel_Mods : Gdk.Types.Gdk_Modifier_Type);
   --  Registers a new accelerator with the global accelerator map. This
   --  function should only be called once per Accel_Path with the canonical
   --  Accel_Key and Accel_Mods for this path. To change the accelerator during
   --  runtime programatically, use Gtk.Accel_Map.Change_Entry.
   --  Set Accel_Key and Accel_Mods to 0 to request a removal of the
   --  accelerator.
   --  Note that Accel_Path string will be stored in a Glib.GQuark. Therefore,
   --  if you pass a static string, you can save some memory by interning it
   --  first with g_intern_static_string.
   --  "accel_path": valid accelerator path
   --  "accel_key": the accelerator key
   --  "accel_mods": the accelerator modifiers

   procedure Add_Filter (Filter_Pattern : UTF8_String);
   --  Adds a filter to the global list of accel path filters.
   --  Accel map entries whose accel path matches one of the filters are
   --  skipped by Gtk.Accel_Map.Foreach.
   --  This function is intended for GTK+ modules that create their own menus,
   --  but don't want them to be saved into the applications accelerator map
   --  dump.
   --  "filter_pattern": a pattern (see Gpattern.Spec.Gpattern_Spec)

   function Change_Entry
      (Accel_Path : UTF8_String;
       Accel_Key  : Gdk.Types.Gdk_Key_Type;
       Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
       Replace    : Boolean) return Boolean;
   --  Changes the Accel_Key and Accel_Mods currently associated with
   --  Accel_Path. Due to conflicts with other accelerators, a change may not
   --  always be possible, Replace indicates whether other accelerators may be
   --  deleted to resolve such conflicts. A change will only occur if all
   --  conflicts could be resolved (which might not be the case if conflicting
   --  accelerators are locked). Successful changes are indicated by a True
   --  return value.
   --  Note that Accel_Path string will be stored in a Glib.GQuark. Therefore,
   --  if you pass a static string, you can save some memory by interning it
   --  first with g_intern_static_string.
   --  "accel_path": a valid accelerator path
   --  "accel_key": the new accelerator key
   --  "accel_mods": the new accelerator modifiers
   --  "replace": True if other accelerators may be deleted upon conflicts

   function Get return Gtk_Accel_Map;
   --  Gets the singleton global Gtk.Accel_Map.Gtk_Accel_Map object. This
   --  object is useful only for notification of changes to the accelerator map
   --  via the ::changed signal; it isn't a parameter to the other accelerator
   --  map functions.
   --  Since: gtk+ 2.4

   procedure Load (File_Name : UTF8_String);
   --  Parses a file previously saved with Gtk.Accel_Map.Save for accelerator
   --  specifications, and propagates them accordingly.
   --  "file_name": a file containing accelerator specifications, in the GLib
   --  file name encoding

   procedure Load_Fd (Fd : Glib.Gint);
   --  Filedescriptor variant of Gtk.Accel_Map.Load.
   --  Note that the file descriptor will not be closed by this function.
   --  "fd": a valid readable file descriptor

   procedure Lock_Path (Accel_Path : UTF8_String);
   --  Locks the given accelerator path. If the accelerator map doesn't yet
   --  contain an entry for Accel_Path, a new one is created.
   --  Locking an accelerator path prevents its accelerator from being changed
   --  during runtime. A locked accelerator path can be unlocked by
   --  Gtk.Accel_Map.Unlock_Path. Refer to Gtk.Accel_Map.Change_Entry for
   --  information about runtime accelerator changes.
   --  If called more than once, Accel_Path remains locked until
   --  Gtk.Accel_Map.Unlock_Path has been called an equivalent number of times.
   --  Note that locking of individual accelerator paths is independent from
   --  locking the Gtk.Accel_Group.Gtk_Accel_Group containing them. For runtime
   --  accelerator changes to be possible, both the accelerator path and its
   --  Gtk.Accel_Group.Gtk_Accel_Group have to be unlocked.
   --  Since: gtk+ 2.4
   --  "accel_path": a valid accelerator path

   procedure Lookup_Entry
      (Accel_Path : UTF8_String;
       Key        : out Gtk.Accel_Group.Gtk_Accel_Key;
       Found      : out Boolean);
   --  Looks up the accelerator entry for Accel_Path and fills in Key.
   --  "accel_path": a valid accelerator path
   --  "key": the accelerator key to be filled in (optional)

   procedure Save (File_Name : UTF8_String);
   --  Saves current accelerator specifications (accelerator path, key and
   --  modifiers) to File_Name. The file is written in a format suitable to be
   --  read back in by Gtk.Accel_Map.Load.
   --  "file_name": the name of the file to contain accelerator
   --  specifications, in the GLib file name encoding

   procedure Save_Fd (Fd : Glib.Gint);
   --  Filedescriptor variant of Gtk.Accel_Map.Save.
   --  Note that the file descriptor will not be closed by this function.
   --  "fd": a valid writable file descriptor

   procedure Unlock_Path (Accel_Path : UTF8_String);
   --  Undoes the last call to Gtk.Accel_Map.Lock_Path on this Accel_Path.
   --  Refer to Gtk.Accel_Map.Lock_Path for information about accelerator path
   --  locking.
   --  Since: gtk+ 2.4
   --  "accel_path": a valid accelerator path

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void is not null access procedure
     (Self       : access Gtk_Accel_Map_Record'Class;
      Accel_Path : UTF8_String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type);

   type Cb_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void is not null access procedure
     (Self       : access Glib.Object.GObject_Record'Class;
      Accel_Path : UTF8_String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Accel_Map_Record;
       Call  : Cb_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Accel_Map_Record;
       Call  : Cb_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Notifies of a change in the global accelerator map. The path is also
   --  used as the detail for the signal, so it is possible to connect to
   --  changed::`accel_path`.
   -- 
   --  Callback parameters:
   --    --  "accel_path": the path of the accelerator that changed
   --    --  "accel_key": the key value for the new accelerator
   --    --  "accel_mods": the modifier mask for the new accelerator

end Gtk.Accel_Map;
