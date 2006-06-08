-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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

--  <description>
--  A Gtk_Check_Button places a discrete Gtk_Toggle_Button next to a widget,
--  (usually a Gtk_Label).
--  </description>
--  <c_version>2.8.17</c_version>
--  <screenshot>gtk-check_button</screenshot>
--  <group>Buttons and Toggles</group>
--  <testgtk>create_check_buttons.adb</testgtk>

with Glib.Properties;
with Gtk.Toggle_Button;

package Gtk.Check_Button is

   type Gtk_Check_Button_Record is new
     Toggle_Button.Gtk_Toggle_Button_Record with private;
   type Gtk_Check_Button is access all Gtk_Check_Button_Record'Class;

   procedure Gtk_New
     (Check_Button : out Gtk_Check_Button;
      Label        : UTF8_String := "");
   --  Create a check button.
   --  if Label is null, then no widget is associated with the button, and
   --  any widget can be added to the button (with Gtk.Container.Add).

   procedure Gtk_New_With_Mnemonic
     (Check_Button : out Gtk_Check_Button;
      Label        : UTF8_String);
   --  Create a new check button containing a label.
   --  If characters in Label are preceded by an underscore, they are
   --  underlined indicating that they represent a keyboard accelerator called
   --  a mnemonic.
   --  Pressing Alt and that key activates the checkbutton.
   --  Label: The text of the button, with an underscore in front of the
   --         mnemonic character

   procedure Initialize
     (Check_Button : access Gtk_Check_Button_Record'Class;
      Label        : UTF8_String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize_With_Mnemonic
     (Check_Button : access Gtk_Check_Button_Record'Class;
      Label        : UTF8_String);
   --  Internal initialization function.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Check_Button.

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Indicator_Size_Property
   --  Type:  Int
   --  Descr: Size of check or radio indicator
   --
   --  Name:  Indicator_Spacing_Property
   --  Type:  Int
   --  Descr: Spacing around check or radio indicator
   --  </style_properties>

   Indicator_Size_Property    : constant Glib.Properties.Property_Int;
   Indicator_Spacing_Property : constant Glib.Properties.Property_Int;

private
   type Gtk_Check_Button_Record is new
     Gtk.Toggle_Button.Gtk_Toggle_Button_Record with null record;
   pragma Import (C, Get_Type, "gtk_check_button_get_type");

   Indicator_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("indicator-size");
   Indicator_Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("indicator-spacing");
end Gtk.Check_Button;
