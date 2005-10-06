-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                  Copyright (C) 2002-2005 AdaCore                  --
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

--  <c_version>2.0.0</c_version>

with Gdk.Types;
with Interfaces.C.Strings;
with Gtkada.Types;

package body Gtk.Clipboard is

   use Interfaces.C.Strings;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Clipboard : Gtk_Clipboard;
      Text      : UTF8_String)
   is
      procedure Internal
        (Clipboard : Gtk_Clipboard;
         Str       : UTF8_String;
         Len       : Integer);
      pragma Import (C, Internal, "gtk_clipboard_set_text");

   begin
      Internal (Clipboard, Text, Text'Length);
   end Set_Text;

   -------------------
   -- Wait_For_Text --
   -------------------

   function Wait_For_Text (Clipboard : Gtk_Clipboard) return UTF8_String is
      function Internal (Clipboard : Gtk_Clipboard) return chars_ptr;
      pragma Import (C, Internal, "gtk_clipboard_wait_for_text");

      S : constant chars_ptr := Internal (Clipboard);

   begin
      if S /= Null_Ptr then
         declare
            Result : constant UTF8_String := Value (S);
         begin
            Gtkada.Types.g_free (S);
            return Result;
         end;
      end if;

      return "";
   end Wait_For_Text;

   ----------------------------
   -- Wait_Is_Text_Available --
   ----------------------------

   function Wait_Is_Text_Available
     (Clipboard : Gtk_Clipboard) return Boolean
   is
      function Internal (Clipboard : Gtk_Clipboard) return Gboolean;
      pragma Import (C, Internal, "gtk_clipboard_wait_is_text_available");
   begin
      return Internal (Clipboard) /= 0;
   end Wait_Is_Text_Available;

end Gtk.Clipboard;
