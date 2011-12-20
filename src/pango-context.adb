------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Pango.Font;  use Pango.Font;
with System;      use System;

package body Pango.Context is

   --------------------------
   -- Get_Font_Description --
   --------------------------

   function Get_Font_Description
     (Context : Pango_Context)
      return Pango.Font.Pango_Font_Description
   is
      function Internal (Context : System.Address)
         return Pango_Font_Description;
      pragma Import
        (C, Internal, "pango_context_get_font_description");
   begin
      return Internal (Get_Object (Context));
   end Get_Font_Description;

   --------------------------
   -- Set_Font_Description --
   --------------------------

   procedure Set_Font_Description
     (Context     : Pango_Context;
      Description : Pango.Font.Pango_Font_Description)
   is
      procedure Internal (Context : System.Address;
                          Description : Pango_Font_Description);
      pragma Import
        (C, Internal, "pango_context_set_font_description");
   begin
      Internal (Get_Object (Context), Description);
   end Set_Font_Description;

   ---------------
   -- Load_Font --
   ---------------

   function Load_Font
     (Context : access Pango_Context_Record'Class;
      Descr   : Pango.Font.Pango_Font_Description)
      return Pango.Font.Pango_Font
   is
      function Internal (Context : System.Address;
                         Descr   : Pango_Font_Description)
         return System.Address;
      pragma Import (C, Internal, "pango_context_load_font");
      Stub : Pango_Font_Record;
   begin
      return Pango_Font
        (Get_User_Data (Internal (Get_Object (Context), Descr), Stub));
   end Load_Font;

end Pango.Context;
