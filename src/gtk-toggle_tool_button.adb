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

package body Gtk.Toggle_Tool_Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Toggle_Tool_Button_Record);
   pragma Warnings (Off, Type_Conversion);

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
     (Button : access Gtk_Toggle_Tool_Button_Record)
      return Boolean
   is
      function Internal (Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_toggle_tool_button_get_active");
   begin
      return Boolean'Val (Internal (Get_Object (Button)));
   end Get_Active;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Button : out Gtk_Toggle_Tool_Button) is
   begin
      Button := new Gtk_Toggle_Tool_Button_Record;
      Gtk.Toggle_Tool_Button.Initialize (Button);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Button : access Gtk_Toggle_Tool_Button_Record'Class)
   is
      function Internal  return System.Address;
      pragma Import (C, Internal, "gtk_toggle_tool_button_new");
   begin
      Set_Object (Button, Internal);
   end Initialize;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
     (Button   : out Gtk_Toggle_Tool_Button;
      Stock_Id : String) is
   begin
      Button := new Gtk_Toggle_Tool_Button_Record;
      Initialize_From_Stock (Button, Stock_Id);
   end Gtk_New_From_Stock;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
     (Button   : access Gtk_Toggle_Tool_Button_Record'Class;
      Stock_Id : String)
   is
      function Internal (Stock_Id : String) return System.Address;
      pragma Import (C, Internal, "gtk_toggle_tool_button_new_from_stock");
   begin
      Set_Object (Button, Internal (Stock_Id & ASCII.NUL));
   end Initialize_From_Stock;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
     (Button    : access Gtk_Toggle_Tool_Button_Record;
      Is_Active : Boolean)
   is
      procedure Internal
        (Button    : System.Address;
         Is_Active : Gboolean);
      pragma Import (C, Internal, "gtk_toggle_tool_button_set_active");
   begin
      Internal (Get_Object (Button), Boolean'Pos (Is_Active));
   end Set_Active;

end Gtk.Toggle_Tool_Button;
