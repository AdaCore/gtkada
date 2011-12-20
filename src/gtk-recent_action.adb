------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gtkada.Bindings;      use Gtkada.Bindings;

package body Gtk.Recent_Action is

   ----------------------
   -- Get_Show_Numbers --
   ----------------------

   function Get_Show_Numbers
     (Action : access Gtk_Recent_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_recent_action_get_show_numbers");
   begin
      return Boolean'Val (Internal (Get_Object (Action)));
   end Get_Show_Numbers;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget   : out Gtk_Recent_Action;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "")
   is
   begin
      Widget := new Gtk_Recent_Action_Record;
      Initialize (Widget, Name, Label, Tooltip, Stock_Id);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget   : access Gtk_Recent_Action_Record'Class;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "")
   is
      function Internal
        (Name     : String;
         Label    : chars_ptr;
         Tooltip  : chars_ptr;
         Stock_Id : chars_ptr)
         return System.Address;
      pragma Import (C, Internal, "gtk_recent_action_new");

      C_Label    : chars_ptr := String_Or_Null (Label);
      C_Tooltip  : chars_ptr := String_Or_Null (Tooltip);
      C_Stock_Id : chars_ptr := String_Or_Null (Stock_Id);
   begin
      Set_Object
        (Widget,
         Internal
           (Name & ASCII.NUL,
            C_Label,
            C_Tooltip,
            C_Stock_Id));

      Free (C_Label);
      Free (C_Tooltip);
      Free (C_Stock_Id);
   end Initialize;

   -------------------------
   -- Gtk_New_For_Manager --
   -------------------------

   procedure Gtk_New_For_Manager
     (Widget   : out Gtk_Recent_Action;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "";
      Manager  : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class :=
                 Gtk.Recent_Manager.Get_Default)
   is
   begin
      Widget := new Gtk_Recent_Action_Record;
      Initialize_For_Manager (Widget, Name, Label, Tooltip, Stock_Id, Manager);
   end Gtk_New_For_Manager;

   ----------------------------
   -- Initialize_For_Manager --
   ----------------------------

   procedure Initialize_For_Manager
     (Widget   : access Gtk_Recent_Action_Record'Class;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "";
      Manager  : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class :=
                 Gtk.Recent_Manager.Get_Default)
   is
      function Internal
        (Name     : String;
         Label    : chars_ptr;
         Tooltip  : chars_ptr;
         Stock_Id : chars_ptr;
         Manager  : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_recent_action_new_for_manager");

      C_Label    : chars_ptr := String_Or_Null (Label);
      C_Tooltip  : chars_ptr := String_Or_Null (Tooltip);
      C_Stock_Id : chars_ptr := String_Or_Null (Stock_Id);
   begin
      Set_Object
        (Widget,
         Internal
           (Name & ASCII.NUL,
            C_Label,
            C_Tooltip,
            C_Stock_Id,
            Get_Object (Manager)));

      Free (C_Label);
      Free (C_Tooltip);
      Free (C_Stock_Id);
   end Initialize_For_Manager;

   ----------------------
   -- Set_Show_Numbers --
   ----------------------

   procedure Set_Show_Numbers
     (Action       : access Gtk_Recent_Action_Record;
      Show_Numbers : Boolean)
   is
      procedure Internal
        (Action       : System.Address;
         Show_Numbers : Gboolean);
      pragma Import (C, Internal, "gtk_recent_action_set_show_numbers");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Show_Numbers));
   end Set_Show_Numbers;

end Gtk.Recent_Action;
