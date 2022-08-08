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

package body Gtk.Drawing_Area is

   package Type_Conversion_Gtk_Drawing_Area is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Drawing_Area_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Drawing_Area);

   --------------------------
   -- Gtk_Drawing_Area_New --
   --------------------------

   function Gtk_Drawing_Area_New return Gtk_Drawing_Area is
      Drawing_Area : constant Gtk_Drawing_Area := new Gtk_Drawing_Area_Record;
   begin
      Gtk.Drawing_Area.Initialize (Drawing_Area);
      return Drawing_Area;
   end Gtk_Drawing_Area_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Drawing_Area : out Gtk_Drawing_Area) is
   begin
      Drawing_Area := new Gtk_Drawing_Area_Record;
      Gtk.Drawing_Area.Initialize (Drawing_Area);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Drawing_Area : not null access Gtk_Drawing_Area_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_drawing_area_new");
   begin
      if not Drawing_Area.Is_Created then
         Set_Object (Drawing_Area, Internal);
      end if;
   end Initialize;

end Gtk.Drawing_Area;
