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
with Gtkada.Bindings; use Gtkada.Bindings;

package body Gtk.Actionable is

   ---------------------
   -- Get_Action_Name --
   ---------------------

   function Get_Action_Name (Self : Gtk_Actionable) return UTF8_String is
      function Internal
         (Self : Gtk_Actionable) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_actionable_get_action_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Action_Name;

   -----------------------------
   -- Get_Action_Target_Value --
   -----------------------------

   function Get_Action_Target_Value
      (Self : Gtk_Actionable) return Glib.Variant.Gvariant
   is
      function Internal (Self : Gtk_Actionable) return System.Address;
      pragma Import (C, Internal, "gtk_actionable_get_action_target_value");
   begin
      return From_Object (Internal (Self));
   end Get_Action_Target_Value;

   ---------------------
   -- Set_Action_Name --
   ---------------------

   procedure Set_Action_Name
      (Self        : Gtk_Actionable;
       Action_Name : UTF8_String := "")
   is
      procedure Internal
         (Self        : Gtk_Actionable;
          Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_actionable_set_action_name");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Action_Name = "" then
         Tmp_Action_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Action_Name := New_String (Action_Name);
      end if;
      Internal (Self, Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Set_Action_Name;

   -----------------------------
   -- Set_Action_Target_Value --
   -----------------------------

   procedure Set_Action_Target_Value
      (Self         : Gtk_Actionable;
       Target_Value : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self         : Gtk_Actionable;
          Target_Value : System.Address);
      pragma Import (C, Internal, "gtk_actionable_set_action_target_value");
   begin
      Internal (Self, Get_Object (Target_Value));
   end Set_Action_Target_Value;

   ------------------------------
   -- Set_Detailed_Action_Name --
   ------------------------------

   procedure Set_Detailed_Action_Name
      (Self                 : Gtk_Actionable;
       Detailed_Action_Name : UTF8_String)
   is
      procedure Internal
         (Self                 : Gtk_Actionable;
          Detailed_Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_actionable_set_detailed_action_name");
      Tmp_Detailed_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Detailed_Action_Name);
   begin
      Internal (Self, Tmp_Detailed_Action_Name);
      Free (Tmp_Detailed_Action_Name);
   end Set_Detailed_Action_Name;

   function "+" (W : Gtk_Actionable) return Gtk_Actionable is
   begin
      return W;
   end "+";

end Gtk.Actionable;
