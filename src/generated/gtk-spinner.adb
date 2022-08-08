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

package body Gtk.Spinner is

   package Type_Conversion_Gtk_Spinner is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Spinner_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Spinner);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Spinner : out Gtk_Spinner) is
   begin
      Spinner := new Gtk_Spinner_Record;
      Gtk.Spinner.Initialize (Spinner);
   end Gtk_New;

   ---------------------
   -- Gtk_Spinner_New --
   ---------------------

   function Gtk_Spinner_New return Gtk_Spinner is
      Spinner : constant Gtk_Spinner := new Gtk_Spinner_Record;
   begin
      Gtk.Spinner.Initialize (Spinner);
      return Spinner;
   end Gtk_Spinner_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Spinner : not null access Gtk_Spinner_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_spinner_new");
   begin
      if not Spinner.Is_Created then
         Set_Object (Spinner, Internal);
      end if;
   end Initialize;

   -----------
   -- Start --
   -----------

   procedure Start (Spinner : not null access Gtk_Spinner_Record) is
      procedure Internal (Spinner : System.Address);
      pragma Import (C, Internal, "gtk_spinner_start");
   begin
      Internal (Get_Object (Spinner));
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (Spinner : not null access Gtk_Spinner_Record) is
      procedure Internal (Spinner : System.Address);
      pragma Import (C, Internal, "gtk_spinner_stop");
   begin
      Internal (Get_Object (Spinner));
   end Stop;

end Gtk.Spinner;
