------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib;
with Gdk.Types;

package Gdk.Keyval is

   type Gdk_Keymap is private;

   function Get_Type return Glib.GType;

   function Name (Keyval : Gdk.Types.Gdk_Key_Type) return String;

   function From_Name (Keyval_Name : String) return Gdk.Types.Gdk_Key_Type;

   function To_Upper
     (Keyval : Gdk.Types.Gdk_Key_Type) return Gdk.Types.Gdk_Key_Type;

   function To_Lower
     (Keyval : Gdk.Types.Gdk_Key_Type) return Gdk.Types.Gdk_Key_Type;

   function Is_Upper (Keyval : Gdk.Types.Gdk_Key_Type) return Boolean;

   function Is_Lower (Keyval : Gdk.Types.Gdk_Key_Type) return Boolean;

   function To_Unicode (Keyval : Gdk.Types.Gdk_Key_Type) return Glib.Gunichar;

private
   pragma Import (C, To_Upper, "gdk_keyval_to_upper");
   pragma Import (C, To_Lower, "gdk_keyval_to_lower");
   pragma Import (C, Get_Type, "gdk_keymap_get_type");
   pragma Import (C, To_Unicode, "gdk_keyval_to_unicode");

   type Gdk_Keymap is new Glib.C_Proxy;
end Gdk.Keyval;

--  missing:
--  gdk_keymap_lookup_key
--  gdk_keymap_translate_keyboard_state
--  gdk_keymap_get_entries_for_keyval
--  gdk_keymap_get_entries_for_keycode
--  gdk_keyval_convert_case
--  gdk_unicode_to_keyval
