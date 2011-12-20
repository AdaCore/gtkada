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

--  <description>
--  Gtk_Cell_Renderer_Toggle renders a toggle button in a cell. The button is
--  drawn as a radio- or checkbutton, depending on the radio property. When
--  activated, it emits the toggled signal.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>

with Glib.Properties;
with Gtk;
with Gtk.Cell_Renderer;

package Gtk.Cell_Renderer_Toggle is

   type Gtk_Cell_Renderer_Toggle_Record is
     new Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record with private;
   type Gtk_Cell_Renderer_Toggle is
     access all Gtk_Cell_Renderer_Toggle_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Cell_Renderer_Toggle);
   procedure Initialize
     (Widget : access Gtk_Cell_Renderer_Toggle_Record'Class);
   --  Creates or initializes a new renderer.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Set_Radio
     (Toggle : access Gtk_Cell_Renderer_Toggle_Record;
      Radio  : Boolean);
   function Get_Radio
     (Toggle : access Gtk_Cell_Renderer_Toggle_Record) return Boolean;
   --  If Setting is True, the cell renderer renders a radio toggle
   --  (i.e. a toggle in a group of mutually-exclusive toggles).
   --  If False, it renders a check toggle (a standalone boolean option).
   --
   --  Note that this only affects the visual display, but your application is
   --  still responsible for enforcing the behavior, through the toggled
   --  signal.

   procedure Set_Active
     (Toggle  : access Gtk_Cell_Renderer_Toggle_Record;
      Setting : Boolean);
   function Get_Active
     (Toggle : access Gtk_Cell_Renderer_Toggle_Record) return Boolean;
   --  Whether the renderer is currently selected

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "toggled"
   --    procedure Handler
   --     (Widget : access Gtk_Cell_Renderer_Toggle_Record'Class;
   --       Path : UTF8_String);
   --
   --  </signals>

   Signal_Toggled : constant Glib.Signal_Name := "toggled";

   ----------------
   -- Properties --
   ----------------

   --  The following properties are defined for this cell_renderer.
   --  <properties>
   --
   --  Name:  Activatable_Property
   --  Type:  Boolean
   --  Descr: The toggle button can be activated
   --
   --  Name:  Active_Property
   --  Type:  Boolean
   --  Descr: The toggle state of the button
   --
   --  Name:  Inconsistent_Property
   --  Type:  Boolean
   --  Descr: The inconsistent state of the button
   --
   --  Name: Indicator_Size_Property
   --  Type: Gint
   --
   --  Name:  Radio_Property
   --  Type:  Boolean
   --  Descr: Draw the toggle button as a radio button
   --
   --  </properties>

   Activatable_Property    : constant Glib.Properties.Property_Boolean;
   Active_Property         : constant Glib.Properties.Property_Boolean;
   Inconsistent_Property   : constant Glib.Properties.Property_Boolean;
   Indicator_Size_Property : constant Glib.Properties.Property_Int;
   Radio_Property          : constant Glib.Properties.Property_Boolean;

private
   type Gtk_Cell_Renderer_Toggle_Record is
     new Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record with null record;

   Activatable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activatable");
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
   Inconsistent_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inconsistent");
   Indicator_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("indicator-size");
   Radio_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("radio");

   pragma Import (C, Get_Type, "gtk_cell_renderer_toggle_get_type");
end Gtk.Cell_Renderer_Toggle;
