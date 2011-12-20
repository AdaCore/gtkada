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
with Bonobo.Dock;
with Bonobo.Dock_Item;
with Bonobo.Dock_Layout;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Container;
with Gtk.Widget;

package Bonobo.Dock_Band is

   type Bonobo_Dock_Band_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Bonobo_Dock_Band is access all Bonobo_Dock_Band_Record'Class;

   procedure Gnome_New (Widget : out Bonobo_Dock_Band);

   procedure Initialize (Widget : access Bonobo_Dock_Band_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Append
     (Band   : access Bonobo_Dock_Band_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Offset : Guint)
      return Boolean;

   procedure Drag_Begin
     (Band : access Bonobo_Dock_Band_Record;
      Item : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class);

   procedure Drag_End
     (Band : access Bonobo_Dock_Band_Record;
      Item : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class);

   function Drag_To
     (Band   : access Bonobo_Dock_Band_Record;
      Item   : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class;
      X      : Gint;
      Y      : Gint)
      return Boolean;

   function Get_Child_Offset
     (Band   : access Bonobo_Dock_Band_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Guint;

   procedure Get_Item_By_Name
     (Band     : access Bonobo_Dock_Band_Record;
      Name     : String;
      Position : out Guint;
      Offset   : out Guint;
      Item     : out Bonobo.Dock_Item.Bonobo_Dock_Item);

   function Get_Num_Children (Band   : access Bonobo_Dock_Band_Record)
                              return Guint;

   function Get_Orientation (Band   : access Bonobo_Dock_Band_Record)
                             return Gtk_Orientation;

   function Insert
     (Band     : access Bonobo_Dock_Band_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Offset   : Guint;
      Position : Gint)
      return Boolean;

   procedure Layout_Add
     (Band      : access Bonobo_Dock_Band_Record;
      Layout    : access Bonobo.Dock_Layout.Bonobo_Dock_Layout_Record'Class;
      Placement : Bonobo.Dock.Bonobo_Dock_Placement;
      Band_Num  : Guint);

   --  procedure Move_Child
   --    (Band      : access Bonobo_Dock_Band_Record;
   --     Old_Child : out GList;
   --     New_Num   : Guint);

   function Prepend
     (Band   : access Bonobo_Dock_Band_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Offset : Guint)
      return Boolean;

   procedure Set_Child_Offset
     (Band   : access Bonobo_Dock_Band_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Offset : Guint);

   procedure Set_Orientation
     (Band        : access Bonobo_Dock_Band_Record;
      Orientation : Gtk_Orientation);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Bonobo_Dock_Band_Record is new
     Gtk.Container.Gtk_Container_Record with null record;

   pragma Import (C, Get_Type, "gnome_dock_band_get_type");
end Bonobo.Dock_Band;
