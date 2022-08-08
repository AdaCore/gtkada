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

package body Gtk.Vbutton_Box is

   package Type_Conversion_Gtk_Vbutton_Box is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Vbutton_Box_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Vbutton_Box);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Vbutton_Box) is
   begin
      Widget := new Gtk_Vbutton_Box_Record;
      Gtk.Vbutton_Box.Initialize (Widget);
   end Gtk_New;

   -------------------------
   -- Gtk_Vbutton_Box_New --
   -------------------------

   function Gtk_Vbutton_Box_New return Gtk_Vbutton_Box is
      Widget : constant Gtk_Vbutton_Box := new Gtk_Vbutton_Box_Record;
   begin
      Gtk.Vbutton_Box.Initialize (Widget);
      return Widget;
   end Gtk_Vbutton_Box_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Widget : not null access Gtk_Vbutton_Box_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vbutton_box_new");
   begin
      if not Widget.Is_Created then
         Set_Object (Widget, Internal);
      end if;
   end Initialize;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Vbutton_Box_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Vbutton_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

end Gtk.Vbutton_Box;
