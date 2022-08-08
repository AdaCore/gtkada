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

package body Gtk.Text_Attributes is

   function From_Object_Free (B : access Gtk_Text_Appearance) return Gtk_Text_Appearance is
      Result : constant Gtk_Text_Appearance := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gtk_Text_Attributes) return Gtk_Text_Attributes is
      Result : constant Gtk_Text_Attributes := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Text_Attr : out Gtk_Text_Attributes) is
      function Internal return Gtk_Text_Attributes;
      pragma Import (C, Internal, "gtk_text_attributes_new");
   begin
      Text_Attr := Internal;
   end Gtk_New;

   -----------------------------
   -- Gtk_Text_Attributes_New --
   -----------------------------

   function Gtk_Text_Attributes_New return Gtk_Text_Attributes is
      function Internal return Gtk_Text_Attributes;
      pragma Import (C, Internal, "gtk_text_attributes_new");
      Text_Attr : Gtk_Text_Attributes;
   begin
      Text_Attr := Internal;
      return Text_Attr;
   end Gtk_Text_Attributes_New;

end Gtk.Text_Attributes;
