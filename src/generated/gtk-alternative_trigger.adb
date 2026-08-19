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

package body Gtk.Alternative_Trigger is

   package Type_Conversion_Gtk_Alternative_Trigger is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Alternative_Trigger_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Alternative_Trigger);

   ---------------------------------
   -- Gtk_Alternative_Trigger_New --
   ---------------------------------

   function Gtk_Alternative_Trigger_New
      (First  : not null access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class;
       Second : not null access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class)
       return Gtk_Alternative_Trigger
   is
      Self : constant Gtk_Alternative_Trigger := new Gtk_Alternative_Trigger_Record;
   begin
      Gtk.Alternative_Trigger.Initialize (Self, First, Second);
      return Self;
   end Gtk_Alternative_Trigger_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gtk_Alternative_Trigger;
       First  : not null access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class;
       Second : not null access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class)
   is
   begin
      Self := new Gtk_Alternative_Trigger_Record;
      Gtk.Alternative_Trigger.Initialize (Self, First, Second);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Alternative_Trigger_Record'Class;
       First  : not null access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class;
       Second : not null access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class)
   is
      function Internal
         (First  : System.Address;
          Second : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_alternative_trigger_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (First), Get_Object (Second)));
      end if;
   end Initialize;

   ---------------
   -- Get_First --
   ---------------

   function Get_First
      (Self : not null access Gtk_Alternative_Trigger_Record)
       return Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_alternative_trigger_get_first");
      Stub_Gtk_Shortcut_Trigger : Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record;
   begin
      return Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Shortcut_Trigger));
   end Get_First;

   ----------------
   -- Get_Second --
   ----------------

   function Get_Second
      (Self : not null access Gtk_Alternative_Trigger_Record)
       return Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_alternative_trigger_get_second");
      Stub_Gtk_Shortcut_Trigger : Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record;
   begin
      return Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Shortcut_Trigger));
   end Get_Second;

end Gtk.Alternative_Trigger;
