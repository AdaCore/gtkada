------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2013, AdaCore                     --
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

--  <description>
--  A Gtk_Border_Combo is a special kind of combo box that allows the
--  user to select the border to apply to cells in a spreadsheet.
--  Its main usage seems to be with a Gtk_Sheet.
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Selectors</group>

with Gtk.Extra.Combo_Button;

package Gtk.Extra.Border_Combo is

   type Gtk_Border_Combo_Record is
     new Gtk.Extra.Combo_Button.Gtk_Combo_Button_Record with private;
   type Gtk_Border_Combo is access all Gtk_Border_Combo_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Border_Combo);
   --  Create a new border combo.
   --  The button contains the currently selected border.

   procedure Initialize (Widget : access Gtk_Border_Combo_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Border_Combo.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --  procedure Handler (Combo : access Gtk_Border_Combo_Record'Class;
   --                     Selection : Gint);
   --
   --  Emitted when a new font has been selected.
   --  Selection is the number of the selection font.
   --  </signals>
private
   type Gtk_Border_Combo_Record is
     new Gtk.Extra.Combo_Button.Gtk_Combo_Button_Record with null record;
   pragma Import (C, Get_Type, "gtk_border_combo_get_type");
end Gtk.Extra.Border_Combo;
