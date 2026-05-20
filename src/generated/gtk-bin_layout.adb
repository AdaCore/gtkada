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

package body Gtk.Bin_Layout is

   package Type_Conversion_Gtk_Bin_Layout is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Bin_Layout_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Bin_Layout);

   ------------------------
   -- Gtk_Bin_Layout_New --
   ------------------------

   function Gtk_Bin_Layout_New return Gtk_Bin_Layout is
      Self : constant Gtk_Bin_Layout := new Gtk_Bin_Layout_Record;
   begin
      Gtk.Bin_Layout.Initialize (Self);
      return Self;
   end Gtk_Bin_Layout_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Bin_Layout) is
   begin
      Self := new Gtk_Bin_Layout_Record;
      Gtk.Bin_Layout.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Bin_Layout_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_bin_layout_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

end Gtk.Bin_Layout;
