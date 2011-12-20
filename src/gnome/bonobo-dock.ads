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
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;
with Bonobo.Dock_Item; use Bonobo.Dock_Item;

package Bonobo.Dock is

   type Bonobo_Dock_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Bonobo_Dock is access all Bonobo_Dock_Record'Class;

   type Bonobo_Dock_Placement is (
      Top,
      Right,
      Bottom,
      Left,
      Floating);

   procedure Bonobo_New (Widget : out Bonobo_Dock);

   procedure Initialize (Widget : access Bonobo_Dock_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Add_Floating_Item
     (Dock        : access Bonobo_Dock_Record;
      Widget      : access Bonobo_Dock_Item_Record;
      X           : Gint;
      Y           : Gint;
      Orientation : Gtk_Orientation);

   procedure Add_Item
     (Dock        : access Bonobo_Dock_Record;
      Item        : access Bonobo_Dock_Item_Record;
      Placement   : Bonobo_Dock_Placement;
      Band_Num    : Guint;
      Position    : Gint;
      Offset      : Guint;
      In_New_Band : Boolean);

   procedure Allow_Floating_Items
     (Dock   : access Bonobo_Dock_Record;
      Enable : Boolean);

   function Get_Client_Area (Dock   : access Bonobo_Dock_Record)
                             return Gtk.Widget.Gtk_Widget;

   procedure Get_Item_By_Name
     (Dock                 : access Bonobo_Dock_Record;
      Name                 : String;
      Placement            : out Bonobo_Dock_Placement;
      Num_Band             : out Guint;
      Band_Position        : out Guint;
      Offset               : out Guint;
      Dock_Item            : out Bonobo_Dock_Item);

   procedure Set_Client_Area
     (Dock   : access Bonobo_Dock_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "layout_changed"
   --    procedure Handler (Widget : access Bonobo_Dock_Record'Class);
   --
   --  </signals>

private
   type Bonobo_Dock_Record is new
     Gtk.Container.Gtk_Container_Record with null record;

   pragma Import (C, Get_Type, "bonobo_dock_get_type");
end Bonobo.Dock;
