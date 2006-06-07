-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006 AdaCore                         --
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
   --  </properties>

   Text_Property  : constant Glib.Properties.Property_String;
   Value_Property : constant Glib.Properties.Property_Int;

private
   pragma Import (C, Get_Type, "gtk_cell_renderer_progress_get_type");

   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Value_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("value");

end Gtk.Cell_Renderer_Progress;
