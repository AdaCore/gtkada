------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gtk; use Gtk;
with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Cell_Renderer_Toggle is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Renderer_Toggle_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Cell_Renderer_Toggle)
   is
   begin
      Widget := new Gtk_Cell_Renderer_Toggle_Record;
      Gtk.Cell_Renderer_Toggle.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Cell_Renderer_Toggle_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ---------------
   -- Get_Radio --
   ---------------

   function Get_Radio (Toggle : access Gtk_Cell_Renderer_Toggle_Record)
                       return Boolean
   is
      function Internal (Toggle : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_get_radio");
   begin
      return Boolean'Val (Internal (Get_Object (Toggle)));
   end Get_Radio;

   ---------------
   -- Set_Radio --
   ---------------

   procedure Set_Radio
     (Toggle : access Gtk_Cell_Renderer_Toggle_Record;
      Radio  : Boolean)
   is
      procedure Internal
        (Toggle : System.Address;
         Radio  : Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_set_radio");
   begin
      Internal (Get_Object (Toggle),
                Boolean'Pos (Radio));
   end Set_Radio;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active (Toggle : access Gtk_Cell_Renderer_Toggle_Record)
                        return Boolean
   is
      function Internal (Toggle : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_get_active");
   begin
      return Boolean'Val (Internal (Get_Object (Toggle)));
   end Get_Active;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
     (Toggle  : access Gtk_Cell_Renderer_Toggle_Record;
      Setting : Boolean)
   is
      procedure Internal
        (Toggle  : System.Address;
         Setting : Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_set_active");
   begin
      Internal (Get_Object (Toggle),
                Boolean'Pos (Setting));
   end Set_Active;

end Gtk.Cell_Renderer_Toggle;
