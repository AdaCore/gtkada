------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Glib.Type_Conversion_Hooks;

package body Gtk.Radio_Tool_Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Radio_Tool_Button_Record);
   pragma Warnings (Off, Type_Conversion);
   use Widget_SList;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
     (Button : access Gtk_Radio_Tool_Button_Record) return Widget_SList.GSlist
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_tool_button_get_group");
      Group : Widget_SList.GSlist;
   begin
      Set_Object (Group, Internal (Get_Object (Button)));
      return Group;
   end Get_Group;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Radio  : out Gtk_Radio_Tool_Button;
      Group  : Widget_SList.GSlist := Widget_SList.Null_List)
   is
   begin
      Radio := new Gtk_Radio_Tool_Button_Record;
      Initialize (Radio, Group);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Radio  : access Gtk_Radio_Tool_Button_Record'Class;
      Group  : Widget_SList.GSlist := Widget_SList.Null_List)
   is
      function Internal (Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_tool_button_new");
   begin
      Set_Object (Radio, Internal (Get_Object (Group)));
   end Initialize;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
     (Radio    : out Gtk_Radio_Tool_Button;
      Group    : Widget_SList.GSlist := Widget_SList.Null_List;
      Stock_Id : String)
   is
   begin
      Radio := new Gtk_Radio_Tool_Button_Record;
      Initialize_From_Stock (Radio, Group, Stock_Id);
   end Gtk_New_From_Stock;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
     (Radio    : access Gtk_Radio_Tool_Button_Record'Class;
      Group    : Widget_SList.GSlist := Widget_SList.Null_List;
      Stock_Id : String)
   is
      function Internal
        (Group    : System.Address;
         Stock_Id : String)
         return System.Address;
      pragma Import (C, Internal, "gtk_radio_tool_button_new_from_stock");
   begin
      Set_Object
        (Radio,
         Internal (Get_Object (Group), Stock_Id & ASCII.NUL));
   end Initialize_From_Stock;

   -------------------------
   -- Gtk_New_From_Widget --
   -------------------------

   procedure Gtk_New_From_Widget
     (Radio  : out Gtk_Radio_Tool_Button;
      Group  : access Gtk_Radio_Tool_Button_Record'Class)
   is
   begin
      Radio := new Gtk_Radio_Tool_Button_Record;
      Initialize_From_Widget (Radio, Group);
   end Gtk_New_From_Widget;

   ----------------------------
   -- Initialize_From_Widget --
   ----------------------------

   procedure Initialize_From_Widget
     (Radio  : access Gtk_Radio_Tool_Button_Record'Class;
      Group  : access Gtk_Radio_Tool_Button_Record'Class)
   is
      function Internal (Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_tool_button_new_from_widget");
   begin
      Set_Object (Radio, Internal (Get_Object (Group)));
   end Initialize_From_Widget;

   ------------------------------------
   -- Gtk_New_With_Stock_From_Widget --
   ------------------------------------

   procedure Gtk_New_With_Stock_From_Widget
     (Radio    : out Gtk_Radio_Tool_Button;
      Group    : access Gtk_Radio_Tool_Button_Record'Class;
      Stock_Id : String)
   is
   begin
      Radio := new Gtk_Radio_Tool_Button_Record;
      Initialize_With_Stock_From_Widget (Radio, Group, Stock_Id);
   end Gtk_New_With_Stock_From_Widget;

   ---------------------------------------
   -- Initialize_With_Stock_From_Widget --
   ---------------------------------------

   procedure Initialize_With_Stock_From_Widget
     (Radio    : access Gtk_Radio_Tool_Button_Record'Class;
      Group    : access Gtk_Radio_Tool_Button_Record'Class;
      Stock_Id : String)
   is
      function Internal
        (Group    : System.Address;
         Stock_Id : String)
         return System.Address;
      pragma Import
        (C, Internal, "gtk_radio_tool_button_new_with_stock_from_widget");
   begin
      Set_Object
        (Radio,
         Internal (Get_Object (Group), Stock_Id & ASCII.NUL));
   end Initialize_With_Stock_From_Widget;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
     (Button : access Gtk_Radio_Tool_Button_Record;
      Group  : Widget_SList.GSlist)
   is
      procedure Internal
        (Button : System.Address;
         Group  : System.Address);
      pragma Import (C, Internal, "gtk_radio_tool_button_set_group");
   begin
      Internal (Get_Object (Button), Get_Object (Group));
   end Set_Group;

end Gtk.Radio_Tool_Button;
