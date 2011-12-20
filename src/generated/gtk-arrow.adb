------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

package body Gtk.Arrow is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Arrow_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Arrow       : out Gtk_Arrow;
       Arrow_Type  : Gtk.Enums.Gtk_Arrow_Type;
       Shadow_Type : Gtk.Enums.Gtk_Shadow_Type)
   is
   begin
      Arrow := new Gtk_Arrow_Record;
      Gtk.Arrow.Initialize (Arrow, Arrow_Type, Shadow_Type);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Arrow       : access Gtk_Arrow_Record'Class;
       Arrow_Type  : Gtk.Enums.Gtk_Arrow_Type;
       Shadow_Type : Gtk.Enums.Gtk_Shadow_Type)
   is
      function Internal
         (Arrow_Type  : Integer;
          Shadow_Type : Integer) return System.Address;
      pragma Import (C, Internal, "gtk_arrow_new");
   begin
      Set_Object (Arrow, Internal (Gtk.Enums.Gtk_Arrow_Type'Pos (Arrow_Type), Gtk.Enums.Gtk_Shadow_Type'Pos (Shadow_Type)));
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set
      (Arrow       : access Gtk_Arrow_Record;
       Arrow_Type  : Gtk.Enums.Gtk_Arrow_Type;
       Shadow_Type : Gtk.Enums.Gtk_Shadow_Type)
   is
      procedure Internal
         (Arrow       : System.Address;
          Arrow_Type  : Integer;
          Shadow_Type : Integer);
      pragma Import (C, Internal, "gtk_arrow_set");
   begin
      Internal (Get_Object (Arrow), Gtk.Enums.Gtk_Arrow_Type'Pos (Arrow_Type), Gtk.Enums.Gtk_Shadow_Type'Pos (Shadow_Type));
   end Set;

end Gtk.Arrow;
