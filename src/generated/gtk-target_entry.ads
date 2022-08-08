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
--  A Gtk.Target_Entry.Gtk_Target_Entry represents a single type of data than
--  can be supplied for by a widget for a selection or for supplied or received
--  during drag-and-drop.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;         use Glib;
with Gtk.Enums;    use Gtk.Enums;
with Gtkada.Types; use Gtkada.Types;

package Gtk.Target_Entry is

   type Gtk_Target_Entry is record
      Target : Gtkada.Types.Chars_Ptr;
      Flags : Gtk.Enums.Gtk_Target_Flags;
      Info : Guint;
   end record;
   pragma Convention (C, Gtk_Target_Entry);

   function From_Object_Free (B : access Gtk_Target_Entry) return Gtk_Target_Entry;
   pragma Inline (From_Object_Free);
   --  A Gtk.Target_Entry.Gtk_Target_Entry represents a single type of data
   --  than can be supplied for by a widget for a selection or for supplied or
   --  received during drag-and-drop.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Target_Entry : out Gtk_Target_Entry;
       Target       : UTF8_String;
       Flags        : Gtk.Enums.Gtk_Target_Flags;
       Info         : Guint);
   --  Makes a new Gtk.Target_Entry.Gtk_Target_Entry.
   --  "target": String identifier for target
   --  "flags": Set of flags, see Gtk.Enums.Gtk_Target_Flags
   --  "info": an ID that will be passed back to the application

   function Gtk_Target_Entry_New
      (Target : UTF8_String;
       Flags  : Gtk.Enums.Gtk_Target_Flags;
       Info   : Guint) return Gtk_Target_Entry;
   --  Makes a new Gtk.Target_Entry.Gtk_Target_Entry.
   --  "target": String identifier for target
   --  "flags": Set of flags, see Gtk.Enums.Gtk_Target_Flags
   --  "info": an ID that will be passed back to the application

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_target_entry_get_type");

   -------------
   -- Methods --
   -------------

   procedure Free (Target_Entry : Gtk_Target_Entry);
   pragma Import (C, Free, "gtk_target_entry_free");
   --  Frees a Gtk.Target_Entry.Gtk_Target_Entry returned from
   --  gtk_target_entry_new or gtk_target_entry_copy.

end Gtk.Target_Entry;
