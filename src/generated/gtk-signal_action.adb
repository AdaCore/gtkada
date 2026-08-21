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
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(On);

package body Gtk.Signal_Action is

   package Type_Conversion_Gtk_Signal_Action is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Signal_Action_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Signal_Action);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self        : out Gtk_Signal_Action;
       Signal_Name : UTF8_String)
   is
   begin
      Self := new Gtk_Signal_Action_Record;
      Gtk.Signal_Action.Initialize (Self, Signal_Name);
   end Gtk_New;

   ---------------------------
   -- Gtk_Signal_Action_New --
   ---------------------------

   function Gtk_Signal_Action_New
      (Signal_Name : UTF8_String) return Gtk_Signal_Action
   is
      Self : constant Gtk_Signal_Action := new Gtk_Signal_Action_Record;
   begin
      Gtk.Signal_Action.Initialize (Self, Signal_Name);
      return Self;
   end Gtk_Signal_Action_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self        : not null access Gtk_Signal_Action_Record'Class;
       Signal_Name : UTF8_String)
   is
      function Internal
         (Signal_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_signal_action_new");
      Tmp_Signal_Name : Gtkada.Types.Chars_Ptr := New_String (Signal_Name);
      Tmp_Return      : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Signal_Name);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Signal_Name);
   end Initialize;

   ---------------------
   -- Get_Signal_Name --
   ---------------------

   function Get_Signal_Name
      (Self : not null access Gtk_Signal_Action_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_signal_action_get_signal_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Signal_Name;

end Gtk.Signal_Action;
