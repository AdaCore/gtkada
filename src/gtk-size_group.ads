-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2002 ACT-Europe                 --
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
--
--  Gtk_Size_Group provides a mechanism for grouping a number of widgets
--  together so they all request the same amount of space. This is typically
--  useful when you want a column of widgets to have the same size, but you
--  can't use a Gtk_Table widget.

--  Note that size groups only affect the amount of space requested, not the
--  size that the widgets finally receive. If you want the widgets in a
--  Gtk_Size_Group to actually be the same size, you need to pack them in such
--  a way that they get the size they request and not more. For example, if you
--  are packing your widgets into a table, you would not include the Fill flag.
--  </description>
--  <c_version>1.3.11</c_version>

with Glib.Object;
with Glib.Generic_Properties;
with Gtk.Widget;

package Gtk.Size_Group is

   type Gtk_Size_Group_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Size_Group is access all Gtk_Size_Group_Record'Class;

   type Size_Group_Mode is (None, Horizontal, Vertical, Both);
   pragma Convention (C, Size_Group_Mode);
   --  This type indicates how the size of all widgets in the group match:
   --  - None: The behavior is the same as if there was no size. Each widget
   --          requests its most appropriate size.
   --  - Horizontal: All the widgets in the group will have the same width.
   --  - Vertical: All the widgets in the group will have the same height
   --  - Both: All the widgets in the group will have exactly the same size.

   procedure Gtk_New
     (Size_Group : out Gtk_Size_Group; Mode : Size_Group_Mode := Both);
   --  Create a new group.
   --  Initially, it doesn't contain any widget, and you need to add them with
   --  the Add_Widget procedure.

   procedure Initialize
     (Size_Group : access Gtk_Size_Group_Record'Class; Mode : Size_Group_Mode);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Size_Group

   procedure Set_Mode
     (Size_Group : access Gtk_Size_Group_Record;
      Mode       : Size_Group_Mode);
   --  Change the way the group effects the size of its component widgets.

   function Get_Mode
     (Size_Group : access Gtk_Size_Group_Record) return Size_Group_Mode;
   --  Indicate the way the group effects the size of its component widgets.

   procedure Add_Widget
     (Size_Group : access Gtk_Size_Group_Record;
      Widget     : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Add a new widget in the group.
   --  Its size will be effected by all other widgets in the group: the size
   --  requisition of the widget will be the maximum of its requisition and the
   --  requisition of the other widgets in the group (depending on the group
   --  mode).
   --
   --  A given widget can belong to only one size group. It is removed from its
   --  previous group before being added to Size_Group.

   procedure Remove_Widget
     (Size_Group : access Gtk_Size_Group_Record;
      Widget     : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Remove a widget from the group.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Mode_Property
   --    Type:  Size_Group_Mode
   --    Flags: read-write
   --    Descr: the directions in which the size group effects the requested
   --           sizes of its component widgets
   --    See also: Set_Mode / Get_Mode
   --
   --  </properties>

   package Size_Group_Mode_Properties is new
     Glib.Generic_Properties.Generic_Internal_Discrete_Property
     (Size_Group_Mode);
   type Property_Size_Group_Mode is new Size_Group_Mode_Properties.Property;

   Mode_Property : constant Property_Size_Group_Mode;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gtk_Size_Group_Record is new Glib.Object.GObject_Record
     with null record;

   Mode_Property : constant Property_Size_Group_Mode := Build ("mode");

   pragma Import (C, Get_Type, "gtk_size_group_get_type");
end Gtk.Size_Group;
