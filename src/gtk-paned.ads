-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
--  A Gtk_Paned is a container that organizes its two children either
--  horizontally or vertically.
--  The initial size allocated to the children depends on the size
--  they request. However, the user has the possibility to interactively
--  move a separation bar between the two to enlarge one of the children,
--  while at the same time shrinking the second one.
--  The bar can be moved by clicking with the mouse on a small cursor
--  displayed in the bar, and then dragging the mouse.
--
--  No additional decoration is provided around the children.
--
--  Each child has two parameters, Resize and Shrink.
--
--  If Shrink is True, then the widget can be made smaller than its
--  requisition size by the user. Set this to False if you want to
--  set a minimum size.
--
--  if Resize is True, this means that the child accepts to be resized, and
--  will not require any size. Thus, the size allocated to it will be
--  the total size allocated to the container minus the size requested by
--  the other child.
--  If Resize is False, the child should ask for a specific size, which it
--  will get. The other child will be resized accordingly.
--  If both Child have the same value for Resize (either True or False), then
--  the size allocated to each is a ratio between the size requested by both.
--
--  When you use Set_Position with a parameter other than -1, or the user
--  moves the handle to resize the widgets, the behavior of Resize is
--  canceled.
--
--  </description>
--  <c_version>1.3.4</c_version>

with Gtk.Container;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Paned is

   type Gtk_Paned_Record is new
     Gtk.Container.Gtk_Container_Record with private;

   --  <doc_ignore>
   subtype Gtk_Hpaned_Record is Gtk_Paned_Record;
   subtype Gtk_Vpaned_Record is Gtk_Paned_Record;

   type Gtk_Paned is access all Gtk_Paned_Record'Class;
   subtype Gtk_Hpaned is Gtk_Paned;
   subtype Gtk_Vpaned is Gtk_Paned;
   --  </doc_ignore>

   procedure Gtk_New_Vpaned (Widget : out Gtk_Paned);
   --  Create a new vertical container.
   --  The children will be displayed one on top of the other.

   procedure Gtk_New_Hpaned (Widget : out Gtk_Paned);
   --  Create a new horizontal container.
   --  The children will be displayed one besides the other.

   procedure Initialize_Vpaned (Widget : access Gtk_Paned_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize_Hpaned (Widget : access Gtk_Paned_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Paned.

   function Get_Type_Vpaned return Glib.GType;
   --  Return the internal value associated with a vertical Gtk_Paned.

   function Get_Type_Hpaned return Glib.GType;
   --  Return the internal value associated with a horizontal Gtk_Paned.

   procedure Add1
     (Paned : access Gtk_Paned_Record;
      Child : access Gtk_Widget_Record'Class);
   --  Add the first child of the container.
   --  The child will be displayed either in the top or in the left pane,
   --  depending on the orientation of the container.
   --  This is equivalent to using the Pack1 procedure with its default
   --  parameters.

   procedure Pack1
     (Paned  : access Gtk_Paned_Record;
      Child  : access Gtk_Widget_Record'Class;
      Resize : Boolean := False;
      Shrink : Boolean := True);
   --  Add a child to the top or left pane.
   --  You can not change dynamically the attributes Resize and Shrink.
   --  Instead, you have to remove the child from the container, and put it
   --  back with the new value of the attributes. You should also first
   --  call Gtk.Object.Ref on the child so as to be sure it is not destroyed
   --  when you remove it, and Gtk.Object.Unref it at the end. See the
   --  example in testgtk/ in the GtkAda distribution.

   procedure Add2
     (Paned : access Gtk_Paned_Record;
      Child : access Gtk_Widget_Record'Class);
   --  Add the second child of the container.
   --  It will be displayed in the bottom or right pane, depending on the
   --  container's orientation.
   --  This is equivalent to using Pack2 with its default parameters.

   procedure Pack2
     (Paned  : access Gtk_Paned_Record;
      Child  : access Gtk_Widget_Record'Class;
      Resize : Boolean := False;
      Shrink : Boolean := False);
   --  Add a child to the bottom or right pane.

   function Get_Position (Paned : access Gtk_Paned_Record) return Gint;
   --  Return the position of the separator.

   procedure Set_Position (Paned : access Gtk_Paned_Record; Position : Gint);
   --  Change the position of the separator.
   --  If position is negative, the remembered position is forgotten,
   --  and the division is recomputed from the requisitions of the
   --  children.
   --  Position is in fact the size (either vertically or horizontally,
   --  depending on the container) set for the first child.

   function Get_Child1
     (Paned : access Gtk_Paned_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the child displayed in the top or left pane.

   function Get_Child2
     (Paned : access Gtk_Paned_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the child displayed in the bottom or right pane.

   function Get_Child1_Resize
     (Paned : access Gtk_Paned_Record) return Boolean;
   --  Get the value of the resize attribute for the first child.

   function Get_Child2_Resize (Paned : access Gtk_Paned_Record) return Boolean;
   --  Get the value of the resize attribute for the second child.

   function Get_Child1_Shrink (Paned : access Gtk_Paned_Record) return Boolean;
   --  Get the value of the shrink attribute for the first child.

   function Get_Child2_Shrink (Paned : access Gtk_Paned_Record) return Boolean;
   --  Get the value of the shrink attribute for the second child.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Paned_Record is new Gtk.Container.Gtk_Container_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_paned_get_type");
   pragma Import (C, Get_Type_Vpaned, "gtk_vpaned_get_type");
   pragma Import (C, Get_Type_Hpaned, "gtk_hpaned_get_type");
end Gtk.Paned;
