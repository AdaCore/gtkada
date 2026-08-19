------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

package body Gtk.Mnemonic_Trigger is

   package Type_Conversion_Gtk_Mnemonic_Trigger is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Mnemonic_Trigger_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Mnemonic_Trigger);

   ------------------------------
   -- Gtk_Mnemonic_Trigger_New --
   ------------------------------

   function Gtk_Mnemonic_Trigger_New
      (Keyval : Guint) return Gtk_Mnemonic_Trigger
   is
      Self : constant Gtk_Mnemonic_Trigger := new Gtk_Mnemonic_Trigger_Record;
   begin
      Gtk.Mnemonic_Trigger.Initialize (Self, Keyval);
      return Self;
   end Gtk_Mnemonic_Trigger_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Mnemonic_Trigger; Keyval : Guint) is
   begin
      Self := new Gtk_Mnemonic_Trigger_Record;
      Gtk.Mnemonic_Trigger.Initialize (Self, Keyval);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Mnemonic_Trigger_Record'Class;
       Keyval : Guint)
   is
      function Internal (Keyval : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_mnemonic_trigger_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Keyval));
      end if;
   end Initialize;

   ----------------
   -- Get_Keyval --
   ----------------

   function Get_Keyval
      (Self : not null access Gtk_Mnemonic_Trigger_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_mnemonic_trigger_get_keyval");
   begin
      return Internal (Get_Object (Self));
   end Get_Keyval;

end Gtk.Mnemonic_Trigger;
