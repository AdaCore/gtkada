-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2002 ACT-Europe                   --
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

with Gdk.Types;
with Gtk.Accel_Group;

package Gtk.Accel_Map is

   procedure Save (File_Name : String);
   --  Save the key shortcuts to a file. These are the shortcuts that might
   --  have been changed dynamically by the user, if the RC file (see Gtk.RC)
   --  contained the line "gtk-can-change-accels=1"

   procedure Load (File_Name : String);
   --  Load the key shortcuts from a file

   procedure Add_Entry
     (Accel_Path : String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type);
   --  Register a new accelerator for a given menu item, within the global
   --  accelerator map.
   --  This function should only be called once per Accel_Path. To change it
   --  programmatically duing runtime, use Change_Entry.
   --  Accel_Path is of the form:
   --     <app>/Category1/Category2/.../Action",
   --  where "app" is a unique, application-specific identifier (for examples
   --  of valid Accel_Path, check the file created by Save above).
   --
   --  For instance, the path in the testgtk application for the menu
   --  File->Open would be
   --     <testgtk>/file/open
   --
   --  It is better to use this function instead of Add_Accelerator, since when
   --  the accelerators are changed interactively by the user, the new value
   --  will be shown properly in the menu, which wouldn't happen if they had
   --  been forced by Add_Accelerator.

   procedure Lookup_Entry
     (Accel_Path : String;
      Key        : out Gtk.Accel_Group.Gtk_Accel_Key;
      Found      : out Boolean);
   --  Look up the accelerator for Accel_Path, and set Key appropriately. If no
   --  accelerator was set, Found is set to False, and the value of Key is
   --  meaningless.

   procedure Change_Entry
     (Accel_Path : String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
      Replace    : Boolean);
   --  Change the accelerator currently associated wtih Accel_Path.
   --  A change may not always be possible due to conflicts with other
   --  accelerators. Replace should be set to True if other accelerators may be
   --  deleted to resolve such conflicts.


end Gtk.Accel_Map;


--  Missing:
--    gtk_accel_map_foreach
--    gtk_accel_map_load_fd
--    gtk_accel_map_load_scanner
--    gtk_accel_map_save_fd
--    gtk_accel_map_add_filter
--    gtk_accel_map_foreach_unfiltered
