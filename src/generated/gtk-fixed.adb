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

package body Gtk.Fixed is

   package Type_Conversion_Gtk_Fixed is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Fixed_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Fixed);

   -------------------
   -- Gtk_Fixed_New --
   -------------------

   function Gtk_Fixed_New return Gtk_Fixed is
      Fixed : constant Gtk_Fixed := new Gtk_Fixed_Record;
   begin
      Gtk.Fixed.Initialize (Fixed);
      return Fixed;
   end Gtk_Fixed_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Fixed : out Gtk_Fixed) is
   begin
      Fixed := new Gtk_Fixed_Record;
      Gtk.Fixed.Initialize (Fixed);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Fixed : not null access Gtk_Fixed_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_fixed_new");
   begin
      if not Fixed.Is_Created then
         Set_Object (Fixed, Internal);
      end if;
   end Initialize;

   ----------
   -- Move --
   ----------

   procedure Move
      (Fixed  : not null access Gtk_Fixed_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Glib.Gint;
       Y      : Glib.Gint)
   is
      procedure Internal
         (Fixed  : System.Address;
          Widget : System.Address;
          X      : Glib.Gint;
          Y      : Glib.Gint);
      pragma Import (C, Internal, "gtk_fixed_move");
   begin
      Internal (Get_Object (Fixed), Get_Object (Widget), X, Y);
   end Move;

   ---------
   -- Put --
   ---------

   procedure Put
      (Fixed  : not null access Gtk_Fixed_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Glib.Gint;
       Y      : Glib.Gint)
   is
      procedure Internal
         (Fixed  : System.Address;
          Widget : System.Address;
          X      : Glib.Gint;
          Y      : Glib.Gint);
      pragma Import (C, Internal, "gtk_fixed_put");
   begin
      Internal (Get_Object (Fixed), Get_Object (Widget), X, Y);
   end Put;

end Gtk.Fixed;
