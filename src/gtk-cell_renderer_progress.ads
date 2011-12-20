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

--  <description>
--  A Gtk_Cell_Renderer_Progress can be used to render a progress bar in a cell
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>

with Glib.Properties;
with Gtk.Cell_Renderer;

package Gtk.Cell_Renderer_Progress is

   type Gtk_Cell_Renderer_Progress_Record is
     new Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record with null record;
   type Gtk_Cell_Renderer_Progress is
     access all Gtk_Cell_Renderer_Progress_Record'Class;

   procedure Gtk_New    (Render : out Gtk_Cell_Renderer_Progress);
   procedure Initialize
     (Render : access Gtk_Cell_Renderer_Progress_Record'Class);
   --  Creates or initializes a new renderer.
   --  Adjust how text is drawn using object properties.
   --  Properties can be set either directly on the renderer, or in combination
   --  with a Gtk_Tree_View_Column so that the property is bound to a value
   --  in the Gtk_Tree_Model.

   function Get_Type return GType;
   --  Returns the internal value associated with this renderer

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Text_Property
   --  Type:  String
   --  Descr: Text on the progress bar
   --
   --  Name:  Value_Property
   --  Type:  Int
   --  Descr: Value of the progress bar
   --
   --  Name:  Orientation_Property
   --  Type:  Enum
   --  Descr: Orientation and growth direction of the progress bar
   --
   --  Name:  Pulse_Property
   --  Type:  Int
   --  Descr: Set this to positive values to indicate that some progress is
   --  made, but you don't know how much.
   --
   --  Name:  Text_Xalign_Property
   --  Type:  Float
   --  Descr: The horizontal text alignment, from 0 (left) to 1 (right).
   --  Reversed for RTL layouts.
   --
   --  Name:  Text_Yalign_Property
   --  Type:  Float
   --  Descr: The vertical text alignment, from 0 (top) to 1 (bottom).
   --
   --  </properties>

   Text_Property  : constant Glib.Properties.Property_String;
   Value_Property : constant Glib.Properties.Property_Int;
   --  Orientation_Property : constant Glib.Properties.Property_Enum;
   Pulse_Property : constant Glib.Properties.Property_Int;
   Text_Xalign_Property : constant Glib.Properties.Property_Float;
   Text_Yalign_Property : constant Glib.Properties.Property_Float;

private
   pragma Import (C, Get_Type, "gtk_cell_renderer_progress_get_type");

   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Value_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("value");
   --  Orientation_Property : constant Glib.Properties.Property_Enum :=
   --    Glib.Properties.Build ("orientation");
   Pulse_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pulse");
   Text_Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("text-xalign");
   Text_Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("text-yalign");

end Gtk.Cell_Renderer_Progress;
