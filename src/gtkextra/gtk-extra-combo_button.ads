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
--  A Gtk_Combo_Button is a general form for a combo box (ie a button
--  associated with a popup window to select its value).
--  This widget should be used only if you intend to write your own kind
--  of combo box. You should look at the following widgets for specific
--  implementation: Gtk_Combo, Gtk_Color_Combo, Gtk_Border_Combo.
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Menus and Toolbars</group>

with Gtk.Box;
with Gtk.Button;
with Gtk.Toggle_Button;
with Gtk.Frame;

package Gtk.Extra.Combo_Button is

   type Gtk_Combo_Button_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Combo_Button is access all Gtk_Combo_Button_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Combo_Button);
   --  Create a new combo box.
   --  This creates all the internal subwidgets (the popup window,...) but
   --  this is your responsibility to put something inside the button or
   --  the popup window.

   procedure Initialize (Widget : access Gtk_Combo_Button_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Combo_Box.

   procedure Hide_Popdown_Window (Combo : access Gtk_Combo_Button_Record);
   --  Hide the popup window, release the mouse grabs, and restore the
   --  default aspect for the arrow.

   function Get_Button
     (Combo : access Gtk_Combo_Button_Record) return Gtk.Button.Gtk_Button;
   --  Return the button that shows the value of the combo.

   function Get_Toggle_Button
     (Combo : access Gtk_Combo_Button_Record)
      return Gtk.Toggle_Button.Gtk_Toggle_Button;
   --  Return the arrow button.
   --  The user has to click on it to open the popup window.

   function Get_Frame
     (Combo : access Gtk_Combo_Button_Record) return Gtk.Frame.Gtk_Frame;
   --  The frame displayed in the popup window.
   --  You should add whatever value the popup window should display in it.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Combo_Button_Record is new Gtk.Box.Gtk_Box_Record with null record;
   pragma Import (C, Get_Type, "gtk_combo_button_get_type");
end Gtk.Extra.Combo_Button;
