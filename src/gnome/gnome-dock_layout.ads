-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Glib; use Glib;
with Gtk;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Object;
with Gnome.Dock;
with Gnome.Dock_Item;

package Gnome.Dock_Layout is

   type Gnome_Dock_Layout_Record is new
     Gtk.Object.Gtk_Object_Record with private;
   type Gnome_Dock_Layout is access all Gnome_Dock_Layout_Record'Class;

   procedure Gnome_New (Widget : out Gnome_Dock_Layout);

   procedure Initialize (Widget : access Gnome_Dock_Layout_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Add_From_Layout
     (Dock   : access Gnome.Dock.Gnome_Dock_Record'Class;
      Layout : access Gnome_Dock_Layout_Record) return Boolean;

   function Add_Floating_Item
     (Layout      : access Gnome_Dock_Layout_Record;
      Item        : access Gnome.Dock_Item.Gnome_Dock_Item_Record'Class;
      X           : Gint;
      Y           : Gint;
      Orientation : Gtk_Orientation)
      return Boolean;

   function Add_Item
     (Layout        : access Gnome_Dock_Layout_Record;
      Item          : access Gnome.Dock_Item.Gnome_Dock_Item_Record'Class;
      Placement     : Gnome.Dock.Gnome_Dock_Placement;
      Band_Num      : Gint;
      Band_Position : Gint;
      Offset        : Gint)
      return Boolean;

   function Add_To_Dock
     (Layout : access Gnome_Dock_Layout_Record;
      Dock   : access Gnome.Dock.Gnome_Dock_Record'Class)
      return Boolean;

   function Create_String (Layout : access Gnome_Dock_Layout_Record)
                           return String;

   --  function Get_Item
   --    (Layout : access Gnome_Dock_Layout_Record;
   --     Item   : access Gnome.Dock_Item.Gnome_Dock_Item_Record'Class)
   --     return Gnome_Dock_Layout_Item;

   --  function Get_Item_By_Name
   --    (Layout : access Gnome_Dock_Layout_Record;
   --     Name   : String)
   --     return Gnome_Dock_Layout_Item;

   function Get_Layout (Dock : access Gnome.Dock.Gnome_Dock_Record'Class)
                        return Gnome_Dock_Layout;

   function Parse_String
     (Layout : access Gnome_Dock_Layout_Record;
      Str    : String) return Boolean;

   function Remove_Item
     (Layout : access Gnome_Dock_Layout_Record;
      Item   : access Gnome.Dock_Item.Gnome_Dock_Item_Record'Class)
      return Boolean;

   function Remove_Item_By_Name
     (Layout : access Gnome_Dock_Layout_Record;
      Name   : String)
      return Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_Dock_Layout_Record is new
     Gtk.Object.Gtk_Object_Record with null record;

   pragma Import (C, Get_Type, "gnome_dock_layout_get_type");
end Gnome.Dock_Layout;
