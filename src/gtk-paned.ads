-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  <c_version>2.8.17</c_version>
--  <group>Layout containers</group>

with Glib.Properties;
with Gtk.Container;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Paned is

   type Gtk_Paned_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   subtype Gtk_Hpaned_Record is Gtk_Paned_Record;
   subtype Gtk_Vpaned_Record is Gtk_Paned_Record;

   type Gtk_Paned is access all Gtk_Paned_Record'Class;
   subtype Gtk_Hpaned is Gtk_Paned;
   subtype Gtk_Vpaned is Gtk_Paned;

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
   function Get_Type_Vpaned return Glib.GType;
   function Get_Type_Hpaned return Glib.GType;
   --  Return the internal value associated with a Gtk_Paned.

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

   procedure Set_Position (Paned : access Gtk_Paned_Record; Position : Gint);
   function  Get_Position (Paned : access Gtk_Paned_Record) return Gint;
   --  Change or get the position of the separator.
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

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   procedure Set_Handle_Size (Paned : access Gtk_Paned_Record; Size : Guint16);
   pragma Obsolescent;
   --  Do nothing.
   --  Only provided for compatibility.

   function Get_Handle_Size (Paned : access Gtk_Paned_Record) return Guint16;
   pragma Obsolescent;
   --  Return 0.
   --  Only provided for compatibility.

   procedure Set_Gutter_Size (Paned : access Gtk_Paned_Record; Size : Guint16);
   pragma Obsolescent;
   --  Do nothing.
   --  Only provided for compatibility.

   function Get_Gutter_Size (Paned : access Gtk_Paned_Record) return Guint16;
   pragma Obsolescent;
   --  Return 0.
   --  Only provided for compatibility.

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Max_Position_Property
   --  Type:  Int
   --  Descr: Largest possible value for the "position" property
   --
   --  Name:  Min_Position_Property
   --  Type:  Int
   --  Descr: Smallest possible value for the "position" property. This is
   --         derived from the size and shrinkability of the widget
   --
   --  Name:  Position_Property
   --  Type:  Int
   --  Descr: Position of paned separator in pixels (0 means all the way to the
   --         left/top)
   --
   --  Name:  Position_Set_Property
   --  Type:  Boolean
   --  Descr: TRUE if the Position property should be used
   --
   --  </properties>

   Max_Position_Property : constant Glib.Properties.Property_Int;
   Min_Position_Property : constant Glib.Properties.Property_Int;
   Position_Property     : constant Glib.Properties.Property_Int;
   Position_Set_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "accept_position"
   --    procedure Handler (Paned : access Gtk_Paned_Record'Class);
   --    You should emit this signal to request that the current handle
   --    position be considered as valid, and the focus moved out of the
   --    handle. By default, this is bound to the key <enter>, so that after
   --    resizing through the keyboard (see cycle_handle_focus), the user can
   --    validate the new position
   --
   --  - "cancel_position"
   --    procedure Handler (Paned : access Gtk_Paned_Record'Class);
   --    Similar to accept_position, but cancel the last resizing operation.
   --    This is bound to <esc> by default.
   --
   --  - "cycle_child_focus"
   --    procedure Handler
   --       (Paned   : access Gtk_Paned_Record'Class;
   --        To_Next : Boolean);
   --    You should emit this signal to request that the child be moved to the
   --    next child of paned. After the last child, the focus is moved to the
   --    parent of Paned (if it is a Gtk_Paned_Record itself),...
   --    This signal is mostly intended to be used from a keybinding (by
   --    default, gtk+ attaches it to F6 and shift-F6).
   --
   --  - "cycle_handle_focus"
   --    procedure Handler
   --       (Paned   : access Gtk_Paned_Record'Class;
   --        To_Next : Boolean);
   --    You should emit this signal to request that the focus be given to the
   --    handle, so that the user can then resize the children through the
   --    keyboard. It it mostly intended to be used from a keybinding. By
   --    default, gtk+ attaches it to F8. The children can then be resized
   --    with the arrow keys, PageUp, PageDown, Home and End.
   --
   --  - "move_handle"
   --    procedure Handler
   --       (Paned : access Gtk_Paned_Record'Class;
   --        Typ   : Gtk_Scroll_Type);
   --    You should emit this signal to request a resizing of the children.
   --    This is mostly useful when bound to keys, so that when the handle has
   --    the focus (cycle_handle_focus), the children can be resized with the
   --    arrow keys, PageUp, PageDown, Home and End
   --
   --  - "toggle_handle_focus"
   --    procedure Handler (Paned : access Gtk_Paned_Record'Class);
   --    Mostly intended to be bound to a keybinding. When called, this removes
   --    the keyboard focus from the handle (if it was given through
   --    cycle_handle_focus above).
   --
   --  </signals>

   Signal_Accept_Position     : constant String := "accept_position";
   Signal_Cancel_Position     : constant String := "cancel_position";
   Signal_Cycle_Child_Focus   : constant String := "cycle_child_focus";
   Signal_Cycle_Handle_Focus  : constant String := "cycle_handle_focus";
   Signal_Move_Handle         : constant String := "move_handle";
   Signal_Toggle_Handle_Focus : constant String := "toggle_handle_focus";

private
   type Gtk_Paned_Record is new Gtk.Container.Gtk_Container_Record
   with null record;

   Max_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-position");
   Min_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("min-position");
   Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("position");
   Position_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("position-set");

   pragma Import (C, Get_Type, "gtk_paned_get_type");
   pragma Import (C, Get_Type_Vpaned, "gtk_vpaned_get_type");
   pragma Import (C, Get_Type_Hpaned, "gtk_hpaned_get_type");
end Gtk.Paned;

--  Was never implemented, and is now obsolescent anyway:
--  No binding: gtk_paned_compute_position
