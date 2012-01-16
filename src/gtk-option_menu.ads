------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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
--  A Gtk_Option_Menu is a widget that allows the user to choose from a list of
--  valid choices. The Gtk_Option_Menu displays the selected choice. When
--  activated, the Gtk_Option_Menu displays a popup Gtk_Menu which allows the
--  user to make a new choice.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Obsolescent widgets</group>

with Glib.Properties;
with Gtk.Button;
with Gtk.Menu;
with Gtk.Widget;

package Gtk.Option_Menu is
   pragma Obsolescent;

   type Gtk_Option_Menu_Record is new Button.Gtk_Button_Record with private;
   type Gtk_Option_Menu is access all Gtk_Option_Menu_Record'Class;

   procedure Gtk_New (Option_Menu : out Gtk_Option_Menu);
   --  Create a new Gtk_Option_Menu.

   procedure Initialize (Option_Menu : access Gtk_Option_Menu_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Option_Menu.

   procedure Set_Menu
     (Option_Menu : access Gtk_Option_Menu_Record;
      Menu        : access Widget.Gtk_Widget_Record'Class);
   function Get_Menu
     (Option_Menu : access Gtk_Option_Menu_Record) return Gtk.Menu.Gtk_Menu;
   --  Provide the Gtk_Menu that is popped up to allow the user to choose a new
   --  value. You should provide a simple menu avoiding the use of tearoff menu
   --  items, submenus, and accelerators.

   procedure Remove_Menu
     (Option_Menu : access Gtk_Option_Menu_Record;
      Menu        : access Widget.Gtk_Widget_Record'Class);
   --  Remove the menu from the option menu.

   procedure Set_History
     (Option_Menu : access Gtk_Option_Menu_Record; Index : Gint);
   function Get_History
     (Option_Menu : access Gtk_Option_Menu_Record) return Gint;
   --  Select the menu item specified by index making it the newly selected
   --  value for the option menu.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.

   --  <properties>
   --  Name:  Menu_Property
   --  Type:  Object
   --  Descr: The menu of options
   --  </properties>

   Menu_Property : constant Glib.Properties.Property_Object;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Indicator_Size_Property
   --  Type:  Boxed
   --  Descr: Size of dropdown indicator
   --
   --  Name:  Indicator_Spacing_Property
   --  Type:  Boxed
   --  Descr: Spacing around indicator
   --  </style_properties>

   --  Indicator_Size_Property    : constant Glib.Properties.Property_Boxed;
   --  Indicator_Spacing_Property : constant Glib.Properties.Property_Boxed;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --    procedure Handler (Option : access Gtk_Option_Menu_Record'Class);
   --    Emitted when the selected value has changed
   --
   --  </signals>

   Signal_Changed : constant Glib.Signal_Name := "changed";

private
   type Gtk_Option_Menu_Record is new Button.Gtk_Button_Record
     with null record;

   Menu_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("menu");

--     Indicator_Size_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("indicator-size");
--     Indicator_Spacing_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("indicator-spacing");

   pragma Import (C, Get_Type, "gtk_option_menu_get_type");
end Gtk.Option_Menu;
