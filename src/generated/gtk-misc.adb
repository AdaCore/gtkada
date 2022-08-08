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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Misc is

   package Type_Conversion_Gtk_Misc is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Misc_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Misc);

   -------------------
   -- Get_Alignment --
   -------------------

   procedure Get_Alignment
      (Misc   : not null access Gtk_Misc_Record;
       Xalign : out Gfloat;
       Yalign : out Gfloat)
   is
      procedure Internal
         (Misc   : System.Address;
          Xalign : out Gfloat;
          Yalign : out Gfloat);
      pragma Import (C, Internal, "gtk_misc_get_alignment");
   begin
      Internal (Get_Object (Misc), Xalign, Yalign);
   end Get_Alignment;

   -----------------
   -- Get_Padding --
   -----------------

   procedure Get_Padding
      (Misc : not null access Gtk_Misc_Record;
       Xpad : out Glib.Gint;
       Ypad : out Glib.Gint)
   is
      procedure Internal
         (Misc : System.Address;
          Xpad : out Glib.Gint;
          Ypad : out Glib.Gint);
      pragma Import (C, Internal, "gtk_misc_get_padding");
   begin
      Internal (Get_Object (Misc), Xpad, Ypad);
   end Get_Padding;

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
      (Misc   : not null access Gtk_Misc_Record;
       Xalign : Gfloat;
       Yalign : Gfloat)
   is
      procedure Internal
         (Misc   : System.Address;
          Xalign : Gfloat;
          Yalign : Gfloat);
      pragma Import (C, Internal, "gtk_misc_set_alignment");
   begin
      Internal (Get_Object (Misc), Xalign, Yalign);
   end Set_Alignment;

   -----------------
   -- Set_Padding --
   -----------------

   procedure Set_Padding
      (Misc : not null access Gtk_Misc_Record;
       Xpad : Glib.Gint;
       Ypad : Glib.Gint)
   is
      procedure Internal
         (Misc : System.Address;
          Xpad : Glib.Gint;
          Ypad : Glib.Gint);
      pragma Import (C, Internal, "gtk_misc_set_padding");
   begin
      Internal (Get_Object (Misc), Xpad, Ypad);
   end Set_Padding;

end Gtk.Misc;
