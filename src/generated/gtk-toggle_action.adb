------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Toggle_Action is

   package Type_Conversion_Gtk_Toggle_Action is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Toggle_Action_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Toggle_Action);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Action   : out Gtk_Toggle_Action;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "")
   is
   begin
      Action := new Gtk_Toggle_Action_Record;
      Gtk.Toggle_Action.Initialize (Action, Name, Label, Tooltip, Stock_Id);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Action   : not null access Gtk_Toggle_Action_Record'Class;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "")
   is
      function Internal
         (Name     : Interfaces.C.Strings.chars_ptr;
          Label    : Interfaces.C.Strings.chars_ptr;
          Tooltip  : Interfaces.C.Strings.chars_ptr;
          Stock_Id : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_toggle_action_new");
      Tmp_Name     : Interfaces.C.Strings.chars_ptr := New_String (Name);
      Tmp_Label    : Interfaces.C.Strings.chars_ptr;
      Tmp_Tooltip  : Interfaces.C.Strings.chars_ptr;
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr;
      Tmp_Return   : System.Address;
   begin
      if Label = "" then
         Tmp_Label := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      if Tooltip = "" then
         Tmp_Tooltip := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Tooltip := New_String (Tooltip);
      end if;
      if Stock_Id = "" then
         Tmp_Stock_Id := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Stock_Id := New_String (Stock_Id);
      end if;
      Tmp_Return := Internal (Tmp_Name, Tmp_Label, Tmp_Tooltip, Tmp_Stock_Id);
      Free (Tmp_Name);
      Free (Tmp_Label);
      Free (Tmp_Tooltip);
      Free (Tmp_Stock_Id);
      Set_Object (Action, Tmp_Return);
   end Initialize;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
      (Action : not null access Gtk_Toggle_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_toggle_action_get_active");
   begin
      return Boolean'Val (Internal (Get_Object (Action)));
   end Get_Active;

   -----------------------
   -- Get_Draw_As_Radio --
   -----------------------

   function Get_Draw_As_Radio
      (Action : not null access Gtk_Toggle_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_toggle_action_get_draw_as_radio");
   begin
      return Boolean'Val (Internal (Get_Object (Action)));
   end Get_Draw_As_Radio;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
      (Action    : not null access Gtk_Toggle_Action_Record;
       Is_Active : Boolean)
   is
      procedure Internal (Action : System.Address; Is_Active : Integer);
      pragma Import (C, Internal, "gtk_toggle_action_set_active");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Is_Active));
   end Set_Active;

   -----------------------
   -- Set_Draw_As_Radio --
   -----------------------

   procedure Set_Draw_As_Radio
      (Action        : not null access Gtk_Toggle_Action_Record;
       Draw_As_Radio : Boolean)
   is
      procedure Internal (Action : System.Address; Draw_As_Radio : Integer);
      pragma Import (C, Internal, "gtk_toggle_action_set_draw_as_radio");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Draw_As_Radio));
   end Set_Draw_As_Radio;

   -------------
   -- Toggled --
   -------------

   procedure Toggled (Action : not null access Gtk_Toggle_Action_Record) is
      procedure Internal (Action : System.Address);
      pragma Import (C, Internal, "gtk_toggle_action_toggled");
   begin
      Internal (Get_Object (Action));
   end Toggled;

end Gtk.Toggle_Action;
