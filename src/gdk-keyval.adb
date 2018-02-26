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

with Gtkada.Types;
with Glib; use Glib;

package body Gdk.Keyval is

   --------------
   -- Is_Lower --
   --------------

   function Is_Lower (Keyval : Gdk.Types.Gdk_Key_Type) return Boolean is
      function Internal (Keyval : Gdk.Types.Gdk_Key_Type) return Gboolean;
      pragma Import (C, Internal, "gdk_keyval_is_lower");

   begin
      return Internal (Keyval) /= 0;
   end Is_Lower;

   --------------
   -- Is_Upper --
   --------------

   function Is_Upper (Keyval : Gdk.Types.Gdk_Key_Type) return Boolean is
      function Internal (Keyval : Gdk.Types.Gdk_Key_Type) return Gboolean;
      pragma Import (C, Internal, "gdk_keyval_is_upper");

   begin
      return Internal (Keyval) /= 0;
   end Is_Upper;

   ---------------
   -- From_Name --
   ---------------

   function From_Name (Keyval_Name : String) return Gdk.Types.Gdk_Key_Type is
      function Internal (Keyval_Name : String) return Gdk.Types.Gdk_Key_Type;
      pragma Import (C, Internal, "gdk_keyval_from_name");

   begin
      return Internal (Keyval_Name & ASCII.NUL);
   end From_Name;

   ----------
   -- Name --
   ----------

   function Name (Keyval : Gdk.Types.Gdk_Key_Type) return String is
      function Internal
        (Keyval : Gdk.Types.Gdk_Key_Type)
         return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_keyval_name");

      use type Gtkada.Types.Chars_Ptr;
      P : constant Gtkada.Types.Chars_Ptr := Internal (Keyval);
   begin
      if P = Gtkada.Types.Null_Ptr then
         return "";
      else
         return Gtkada.Types.Value (P);
      end if;
   end Name;

end Gdk.Keyval;
