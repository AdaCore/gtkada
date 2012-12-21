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

package body Gtk.Radio_Action is

   package Type_Conversion_Gtk_Radio_Action is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Radio_Action_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Radio_Action);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Action   : out Gtk_Radio_Action;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Gint)
   is
   begin
      Action := new Gtk_Radio_Action_Record;
      Gtk.Radio_Action.Initialize (Action, Name, Label, Tooltip, Stock_Id, Value);
   end Gtk_New;

   --------------------------
   -- Gtk_Radio_Action_New --
   --------------------------

   function Gtk_Radio_Action_New
      (Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Gint) return Gtk_Radio_Action
   is
      Action : constant Gtk_Radio_Action := new Gtk_Radio_Action_Record;
   begin
      Gtk.Radio_Action.Initialize (Action, Name, Label, Tooltip, Stock_Id, Value);
      return Action;
   end Gtk_Radio_Action_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Action   : not null access Gtk_Radio_Action_Record'Class;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Gint)
   is
      function Internal
         (Name     : Interfaces.C.Strings.chars_ptr;
          Label    : Interfaces.C.Strings.chars_ptr;
          Tooltip  : Interfaces.C.Strings.chars_ptr;
          Stock_Id : Interfaces.C.Strings.chars_ptr;
          Value    : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_radio_action_new");
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
      Tmp_Return := Internal (Tmp_Name, Tmp_Label, Tmp_Tooltip, Tmp_Stock_Id, Value);
      Free (Tmp_Stock_Id);
      Free (Tmp_Tooltip);
      Free (Tmp_Label);
      Free (Tmp_Name);
      Set_Object (Action, Tmp_Return);
   end Initialize;

   -----------------------
   -- Get_Current_Value --
   -----------------------

   function Get_Current_Value
      (Action : not null access Gtk_Radio_Action_Record) return Gint
   is
      function Internal (Action : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_radio_action_get_current_value");
   begin
      return Internal (Get_Object (Action));
   end Get_Current_Value;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
      (Action : not null access Gtk_Radio_Action_Record)
       return Gtk.Widget.Widget_SList.GSlist
   is
      function Internal (Action : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_action_get_group");
      Tmp_Return : Gtk.Widget.Widget_SList.GSlist;
   begin
      Gtk.Widget.Widget_SList.Set_Object (Tmp_Return, Internal (Get_Object (Action)));
      return Tmp_Return;
   end Get_Group;

   ----------------
   -- Join_Group --
   ----------------

   procedure Join_Group
      (Action       : not null access Gtk_Radio_Action_Record;
       Group_Source : access Gtk_Radio_Action_Record'Class)
   is
      procedure Internal
         (Action       : System.Address;
          Group_Source : System.Address);
      pragma Import (C, Internal, "gtk_radio_action_join_group");
   begin
      Internal (Get_Object (Action), Get_Object_Or_Null (GObject (Group_Source)));
   end Join_Group;

   -----------------------
   -- Set_Current_Value --
   -----------------------

   procedure Set_Current_Value
      (Action        : not null access Gtk_Radio_Action_Record;
       Current_Value : Gint)
   is
      procedure Internal (Action : System.Address; Current_Value : Gint);
      pragma Import (C, Internal, "gtk_radio_action_set_current_value");
   begin
      Internal (Get_Object (Action), Current_Value);
   end Set_Current_Value;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
      (Action : not null access Gtk_Radio_Action_Record;
       Group  : Gtk.Widget.Widget_SList.GSlist)
   is
      procedure Internal (Action : System.Address; Group : System.Address);
      pragma Import (C, Internal, "gtk_radio_action_set_group");
   begin
      Internal (Get_Object (Action), Gtk.Widget.Widget_SList.Get_Object (Group));
   end Set_Group;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self : not null access Gtk_Radio_Action_Record;
       Call : not null access procedure
         (Self    : access Gtk_Radio_Action_Record'Class;
          Current : not null access Gtk_Radio_Action_Record'Class))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self : not null access Gtk_Radio_Action_Record;
       Call : not null access procedure
         (Self    : access Glib.Object.GObject_Record'Class;
          Current : not null access Gtk_Radio_Action_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Changed;

end Gtk.Radio_Action;
