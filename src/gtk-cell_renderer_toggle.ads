-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2003 ACT-Europe                 --
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

--  <c_version>1.3.11</c_version>

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
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Get_Radio
     (Toggle : access Gtk_Cell_Renderer_Toggle_Record) return Boolean;

   procedure Set_Radio
     (Toggle : access Gtk_Cell_Renderer_Toggle_Record;
      Radio  : Boolean);
   --  If Setting is True, the cell renderer renders a radio toggle
   --  (i.e. a toggle in a group of mutually-exclusive toggles).
   --  If False, it renders a check toggle (a standalone boolean option).
   --
   --  Note that this only affects the visual display, but your application is
   --  still responsible for enforcing the behavior, through the toggled
   --  signal.

   function Get_Active
     (Toggle : access Gtk_Cell_Renderer_Toggle_Record) return Boolean;

   procedure Set_Active
     (Toggle  : access Gtk_Cell_Renderer_Toggle_Record;
      Setting : Boolean);

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

   ----------------
   -- Properties --
   ----------------

   --  The following properties are defined for this cell_renderer.
   --
   --   Attribute             Type in Model             Mode
   --   =========             =============             ====
   --
   --   "activatable"         Boolean                   Read / Write
   --   "active"              Boolean                   Read / Write
   --   "radio"               Boolean                   Read / Write

private
   type Gtk_Cell_Renderer_Toggle_Record is
     new Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record with null record;

   pragma Import (C, Get_Type, "gtk_cell_renderer_toggle_get_type");
end Gtk.Cell_Renderer_Toggle;
