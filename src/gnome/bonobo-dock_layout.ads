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

with Glib; use Glib;
with Gtk;
with Gtk.Enums; use Gtk.Enums;
with Glib.Object;
with Bonobo.Dock;
with Bonobo.Dock_Item;

package Bonobo.Dock_Layout is

   type Bonobo_Dock_Layout_Record is new
     Glib.Object.GObject_Record with private;
   type Bonobo_Dock_Layout is access all Bonobo_Dock_Layout_Record'Class;

   procedure Bonobo_New (Widget : out Bonobo_Dock_Layout);

   procedure Initialize (Widget : access Bonobo_Dock_Layout_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Add_From_Layout
     (Dock   : access Bonobo.Dock.Bonobo_Dock_Record'Class;
      Layout : access Bonobo_Dock_Layout_Record) return Boolean;

   function Add_Floating_Item
     (Layout      : access Bonobo_Dock_Layout_Record;
      Item        : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class;
      X           : Gint;
      Y           : Gint;
      Orientation : Gtk_Orientation)
      return Boolean;

   function Add_Item
     (Layout        : access Bonobo_Dock_Layout_Record;
      Item          : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class;
      Placement     : Bonobo.Dock.Bonobo_Dock_Placement;
      Band_Num      : Gint;
      Band_Position : Gint;
      Offset        : Gint)
      return Boolean;

   function Add_To_Dock
     (Layout : access Bonobo_Dock_Layout_Record;
      Dock   : access Bonobo.Dock.Bonobo_Dock_Record'Class)
      return Boolean;

   function Create_String (Layout : access Bonobo_Dock_Layout_Record)
                           return String;

   --  function Get_Item
   --    (Layout : access Bonobo_Dock_Layout_Record;
   --     Item   : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class)
   --     return Bonobo_Dock_Layout_Item;

   --  function Get_Item_By_Name
   --    (Layout : access Bonobo_Dock_Layout_Record;
   --     Name   : String)
   --     return Bonobo_Dock_Layout_Item;

   function Get_Layout (Dock : access Bonobo.Dock.Bonobo_Dock_Record'Class)
                        return Bonobo_Dock_Layout;

   function Parse_String
     (Layout : access Bonobo_Dock_Layout_Record;
      Str    : String) return Boolean;

   function Remove_Item
     (Layout : access Bonobo_Dock_Layout_Record;
      Item   : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class)
      return Boolean;

   function Remove_Item_By_Name
     (Layout : access Bonobo_Dock_Layout_Record;
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
   type Bonobo_Dock_Layout_Record is new
     Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "bonobo_dock_layout_get_type");
end Bonobo.Dock_Layout;
