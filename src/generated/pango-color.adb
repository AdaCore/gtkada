------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Pango.Color is

   -----------
   -- Parse --
   -----------

   function Parse
      (Self : access Pango_Color;
       Spec : UTF8_String) return Boolean
   is
      function Internal
         (Self : access Pango_Color;
          Spec : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_color_parse");
      Tmp_Spec   : Gtkada.Types.Chars_Ptr := New_String (Spec);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Spec);
      Free (Tmp_Spec);
      return Tmp_Return /= 0;
   end Parse;

   ----------------------
   -- Parse_With_Alpha --
   ----------------------

   function Parse_With_Alpha
      (Self  : access Pango_Color;
       Alpha : access Guint16 := null;
       Spec  : UTF8_String) return Boolean
   is
      function Internal
         (Self  : access Pango_Color;
          Alpha : access Guint16;
          Spec  : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_color_parse_with_alpha");
      Tmp_Spec   : Gtkada.Types.Chars_Ptr := New_String (Spec);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Alpha, Tmp_Spec);
      Free (Tmp_Spec);
      return Tmp_Return /= 0;
   end Parse_With_Alpha;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Pango_Color) return UTF8_String is
      function Internal (Self : Pango_Color) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "pango_color_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end To_String;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Pango_Color) return Pango_Color
   is
      Result : constant Pango_Color := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

end Pango.Color;
