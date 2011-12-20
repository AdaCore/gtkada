------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with Interfaces.C.Strings;

package body Glib.G_Icon is

   ---------------------
   -- G_Icon_Get_Type --
   ---------------------

   function G_Icon_Get_Type return GType is
      function Internal return GType;
      pragma Import (C, Internal, "g_icon_get_type");
   begin
      return Internal;
   end G_Icon_Get_Type;

   ---------
   -- "=" --
   ---------

   function "=" (Icon1, Icon2 : G_Icon) return Boolean is
      function Internal (Icon1, Icon2 : G_Icon) return Gboolean;
      pragma Import (C, Internal, "g_icon_equal");
   begin
      return Boolean'Val (Internal (Icon1, Icon2));
   end "=";

   ----------
   -- Hash --
   ----------

   function Hash (Icon : G_Icon) return Guint is
      function Internal (Icon : G_Icon) return Guint;
      pragma Import (C, Internal, "g_icon_hash");
   begin
      return Internal (Icon);
   end Hash;

   ---------------
   -- To_String --
   ---------------

   function To_String (Icon : G_Icon) return UTF8_String is
      use Interfaces.C.Strings;

      function Internal (Icon : G_Icon) return chars_ptr;
      pragma Import (C, Internal, "g_icon_to_string");
   begin
      return Value (Internal (Icon));
   end To_String;

   --------------------
   -- New_For_String --
   --------------------

   procedure New_For_String
     (Widget : out G_Icon;
      Str    : String)
   is
      function Internal (Str, Error : System.Address) return G_Icon;
      pragma Import (C, Internal, "g_icon_new_for_string");

      Tmp : constant String := Str & ASCII.NUL;
   begin
      --  We ignore the Error argument.
      Widget := Internal (Tmp'Address, System.Null_Address);
   end New_For_String;

end Glib.G_Icon;
