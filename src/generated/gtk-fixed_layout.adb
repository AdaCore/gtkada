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

package body Gtk.Fixed_Layout is

   package Type_Conversion_Gtk_Fixed_Layout is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Fixed_Layout_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Fixed_Layout);

   --------------------------
   -- Gtk_Fixed_Layout_New --
   --------------------------

   function Gtk_Fixed_Layout_New return Gtk_Fixed_Layout is
      Fixed_Layout : constant Gtk_Fixed_Layout := new Gtk_Fixed_Layout_Record;
   begin
      Gtk.Fixed_Layout.Initialize (Fixed_Layout);
      return Fixed_Layout;
   end Gtk_Fixed_Layout_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Fixed_Layout : out Gtk_Fixed_Layout) is
   begin
      Fixed_Layout := new Gtk_Fixed_Layout_Record;
      Gtk.Fixed_Layout.Initialize (Fixed_Layout);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Fixed_Layout : not null access Gtk_Fixed_Layout_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_fixed_layout_new");
   begin
      if not Fixed_Layout.Is_Created then
         Set_Object (Fixed_Layout, Internal);
      end if;
   end Initialize;

end Gtk.Fixed_Layout;
