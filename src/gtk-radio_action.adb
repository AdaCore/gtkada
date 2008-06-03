-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2006-2008, AdaCore                  --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types; use Gtkada.Types;
with Gtk.Widget;   use Gtk.Widget;
with Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;

package body Gtk.Radio_Action is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Radio_Action_Record);
   pragma Warnings (Off, Type_Conversion);

   -----------------------
   -- Get_Current_Value --
   -----------------------

   function Get_Current_Value
     (Action : access Gtk_Radio_Action_Record) return Gint
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
     (Action : access Gtk_Radio_Action_Record)
      return Widget_SList.GSlist
   is
      function Internal (Action : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_action_get_group");
      L : Widget_SList.GSlist;
   begin
      Widget_SList.Set_Object (L, Internal (Get_Object (Action)));
      return L;
   end Get_Group;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Action   : out Gtk_Radio_Action;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "";
      Value    : Gint) is
   begin
      Action := new Gtk_Radio_Action_Record;
      Initialize (Action, Name, Label, Tooltip,  Stock_Id, Value);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Action   : access Gtk_Radio_Action_Record'Class;
      Name     : String;
      Label    : String := "";
      Tooltip  : String := "";
      Stock_Id : String := "";
      Value    : Gint)
   is
      function Internal
        (Name     : String;
         Label    : Chars_Ptr;
         Tooltip  : Chars_Ptr;
         Stock_Id : Chars_Ptr;
         Value    : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_radio_action_new");
      L : Chars_Ptr := String_Or_Null (Label);
      T : Chars_Ptr := String_Or_Null (Tooltip);
      S : Chars_Ptr := String_Or_Null (Stock_Id);
   begin
      Set_Object (Action, Internal (Name & ASCII.NUL, L, T, S, Value));
      Interfaces.C.Strings.Free (L);
      Interfaces.C.Strings.Free (T);
      Interfaces.C.Strings.Free (S);
   end Initialize;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
     (Action : access Gtk_Radio_Action_Record;
      Group  : Widget_SList.GSlist)
   is
      procedure Internal (Action : System.Address; Group  : System.Address);
      pragma Import (C, Internal, "gtk_radio_action_set_group");
   begin
      Internal (Get_Object (Action), Widget_SList.Get_Object (Group));
   end Set_Group;

end Gtk.Radio_Action;
