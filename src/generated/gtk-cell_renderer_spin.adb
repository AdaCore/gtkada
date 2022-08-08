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

package body Gtk.Cell_Renderer_Spin is

   package Type_Conversion_Gtk_Cell_Renderer_Spin is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Renderer_Spin_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Cell_Renderer_Spin);

   --------------------------------
   -- Gtk_Cell_Renderer_Spin_New --
   --------------------------------

   function Gtk_Cell_Renderer_Spin_New return Gtk_Cell_Renderer_Spin is
      Self : constant Gtk_Cell_Renderer_Spin := new Gtk_Cell_Renderer_Spin_Record;
   begin
      Gtk.Cell_Renderer_Spin.Initialize (Self);
      return Self;
   end Gtk_Cell_Renderer_Spin_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Spin) is
   begin
      Self := new Gtk_Cell_Renderer_Spin_Record;
      Gtk.Cell_Renderer_Spin.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Spin_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_renderer_spin_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

end Gtk.Cell_Renderer_Spin;
