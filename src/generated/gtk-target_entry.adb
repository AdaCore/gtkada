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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Gtk.Target_Entry is

   function From_Object_Free (B : access Gtk_Target_Entry) return Gtk_Target_Entry is
      Result : constant Gtk_Target_Entry := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Target_Entry : out Gtk_Target_Entry;
       Target       : UTF8_String;
       Flags        : Gtk.Enums.Gtk_Target_Flags;
       Info         : Guint)
   is
      function Internal
         (Target : Gtkada.Types.Chars_Ptr;
          Flags  : Gtk.Enums.Gtk_Target_Flags;
          Info   : Guint) return Gtk_Target_Entry;
      pragma Import (C, Internal, "gtk_target_entry_new");
      Tmp_Target : Gtkada.Types.Chars_Ptr := New_String (Target);
      Tmp_Return : Gtk_Target_Entry;
   begin
      Tmp_Return := Internal (Tmp_Target, Flags, Info);
      Free (Tmp_Target);
      Target_Entry := Tmp_Return;
   end Gtk_New;

   --------------------------
   -- Gtk_Target_Entry_New --
   --------------------------

   function Gtk_Target_Entry_New
      (Target : UTF8_String;
       Flags  : Gtk.Enums.Gtk_Target_Flags;
       Info   : Guint) return Gtk_Target_Entry
   is
      function Internal
         (Target : Gtkada.Types.Chars_Ptr;
          Flags  : Gtk.Enums.Gtk_Target_Flags;
          Info   : Guint) return Gtk_Target_Entry;
      pragma Import (C, Internal, "gtk_target_entry_new");
      Tmp_Target   : Gtkada.Types.Chars_Ptr := New_String (Target);
      Tmp_Return   : Gtk_Target_Entry;
      Target_Entry : Gtk_Target_Entry;
   begin
      Tmp_Return := Internal (Tmp_Target, Flags, Info);
      Free (Tmp_Target);
      Target_Entry := Tmp_Return;
      return Target_Entry;
   end Gtk_Target_Entry_New;

end Gtk.Target_Entry;
