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
--  Base class for widgets that have children.
--  </description>
--  <c_version>1.3.6</c_version>

with Gdk.Event;
with Glib.Properties;
with Gtk.Adjustment;
with Gtk.Enums;
with Gtk.Widget;

package Gtk.Container is

   type Gtk_Container_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Container is access all Gtk_Container_Record'Class;

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Container.

   procedure Set_Border_Width
     (Container    : access Gtk_Container_Record;
      Border_Width : Gint);
   --  Modify the size of the frame that surrounds the widget.
   --  The exact visual impact depends on the specific widget class.

   procedure Add
     (Container : access Gtk_Container_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Add a new child to the container.
   --  Note that some containers can have only one child. Nothing is done
   --  if there is already a child.
   --  This basically sends the "add" signal (see below)

   procedure Remove
     (Container : access Gtk_Container_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Remove a child from the container.
   --  Nothing is done if Widget is not a child of Container. Widget is not
   --  destroyed, but is deleted from the screen.
   --  This basically sends the "remove" signal (see below)

   procedure Set_Resize_Mode
     (Container   : access Gtk_Container_Record;
      Resize_Mode : Gtk.Enums.Gtk_Resize_Mode);
   --  Change the resizing behavior for the Container.
   --  The default value is Resize_Parent.

   function Children
     (Container : access Gtk_Container_Record)
      return Gtk.Widget.Widget_List.Glist;
   --  Return a list of all the children of the container.

   procedure Propagate_Expose
     (Container : access Gtk_Container_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event     : Gdk.Event.Gdk_Event_Expose);
   --  When a container receives an expose event, it must send synthetic
   --  expose events to all children that don't have their own Gdk_Window.
   --  This function provides a convenient way of doing this. A container,
   --  when it receives an expose event, Propagate_Expose
   --  once for each child, passing in the event the container received.
   --
   --  Propagate_Expose takes care of deciding whether
   --  an expose event needs to be sent to the child, intersecting
   --  the event's area with the child area, and sending the event.
   --
   --  In most cases, a container can simply either simply inherit the
   --  expose implementation from Gtk_Container, or, do some drawing
   --  and then chain to the expose implementation from Gtk_Container.

   procedure Set_Focus_Chain
     (Container         : access Gtk_Container_Record;
      Focusable_Widgets : Gtk.Widget.Widget_List.Glist);
   --  Set the chain of widgets that can take the focus for a given Container.

   procedure Unset_Focus_Chain (Container : access Gtk_Container_Record);
   --  Unset the focus chain.

   -----------------------
   -- Foreach functions --
   -----------------------

   type Forall_Function is
     access procedure (Item : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Function that can be call for each child of a container.
   --  This is called automatically by the Forall subprogram below.

   procedure Forall
     (Container : access Gtk_Container_Record;
      Func      : Forall_Function);
   --  Execute Func for each of the children of Container.
   --  See also the generic package Forall_Pkg if you want to pass some
   --  extra data to Func.

   --  <doc_ignore>
   generic
      type Data_Type is private;
   package Forall_Pkg is
      type Forall_Function is
        access procedure (Item : access Gtk.Widget.Gtk_Widget_Record'Class;
                          Data : Data_Type);

      procedure Forall
        (Container : access Gtk_Container_Record;
         Func      : Forall_Function;
         Data      : Data_Type);
      --  Execute Func for each of the children of Container

   end Forall_Pkg;
   --  </doc_ignore>

   --------------------------
   -- Widget-level methods --
   --------------------------

   procedure Set_Reallocate_Redraws
     (Container     : access Gtk_Container_Record;
      Needs_Redraws : Boolean := False);
   --  Set the "needs_redraws" field.
   --  If Needs_Redraws is True, then a "draw" signal is emitted for the
   --  Container whenever one is emitted for a child.

   procedure Set_Focus_Vadjustment
     (Container  : access Gtk_Container_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Set the focus to the vertical adjustment.
   --  Adjustment should have been created and displayed at some other
   --  place in your application.
   --  Container will make sure that Adjustment always matches the range
   --  for the focus widget's position (y .. y + height).

   procedure Set_Focus_Hadjustment
     (Container  : access Gtk_Container_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Set the focus to the horizontal adjustment.
   --  Adjustment should have been created and displayed at some other
   --  place in your application.
   --  Container will make sure that Adjustment always matches the range
   --  for the focus widget's position (x .. x + width).

   function Child_Type
     (Container : access Gtk_Container_Record) return Gtk.Gtk_Type;
   --  Return the type of the children in Container.
   --  If Container can contain any type of widget, Gtk_Type_None is
   --  returned.

   procedure Resize_Children (Container : access Gtk_Container_Record);
   --  The container hasn't changed size but one of its children
   --  queued a resize request. Which means that the allocation
   --  is not sufficient for the requisition of some child.
   --  Run through the list of widgets and reallocate their size appropriately.

   ----------------------
   -- Signals emission --
   ----------------------

   procedure Check_Resize (Container : access Gtk_Container_Record);
   --  Emit the "check_resize" signal

   procedure Set_Focus_Child
     (Container : access Gtk_Container_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Emit a "set_focus_child" signal.

   function Get_Focus_Child
     (Container : access Gtk_Container_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the child that currently has the keyboard focus. Note that null
   --  can be returned.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Border_Width_Property
   --    Type:  Guint
   --    Flags: read-write
   --    Descr: The width of the empty border outside the containers children.
   --    See also:  Set_Border_Width
   --
   --  - Name:  Resize_Mode_Property
   --    Type:  Gtk_Resize_Mode
   --    Flags: read-write
   --    Descr: Specify how resize events are handled
   --    See also:  Set_Resize_Mode
   --
   --  - Name:  Child_Property
   --    Type:  Widget
   --    Flags: writable
   --    Descr: Can be used to add a new child to the container.
   --    See also:  Add
   --
   --  - Name:  Reallocate_Redraws_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether redraws should be reallocated
   --    See also: Set_Reallocate_Redraws
   --  </properties>

   Border_Width_Property       : constant Glib.Properties.Property_Uint;
   Resize_Mode_Property        : constant Gtk.Enums.Property_Gtk_Resize_Mode;
   Child_Property              : constant Glib.Properties.Property_Object_WO;
   Reallocate_Redraws_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "add"
   --    procedure Handler (Container : access Gtk_Container_Record'Class;
   --                       Widget    : access Gtk_Widget_Record'Class);
   --
   --    A new widget is added to the container
   --
   --  - "remove"
   --    procedure Handler (Container : access Gtk_Container_Record'Class;
   --                       Widget    : access Gtk_Widget_Record'Class);
   --
   --    A widget is removed from the container
   --
   --  - "check_resize"
   --    procedure Handler (Container : access Gtk_Container_Record'Class);
   --
   --    Called every time the Container needs resizing.
   --    Upon receiving this signal, Container should check whether it needs
   --    to be resized, and if it does should queue a resize request.
   --
   --  - "focus"
   --    procedure Handler (Container : access Gtk_Container_Record'Class;
   --                       Direction : Gtk_Direction_Type);
   --
   --    Moves the current selection to a new widget.
   --
   --  - "set-focus-child"
   --    procedure Handler (Container : access Gtk_Container_Record'Class;
   --                       Widget    : access Gtk_Widget_Record'Class);
   --
   --    Emitted when a new widget gains the focus.
   --
   --  </signals>

private
   type Gtk_Container_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;

   Border_Width_Property       : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("border_width");
   Resize_Mode_Property        : constant Gtk.Enums.Property_Gtk_Resize_Mode :=
     Gtk.Enums.Build ("resize_mode");
   Child_Property              : constant Glib.Properties.Property_Object_WO :=
     Glib.Properties.Build ("child");
   Reallocate_Redraws_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reallocate_redraws");

   pragma Import (C, Get_Type, "gtk_container_get_type");
end Gtk.Container;

--  functions that have no equivalent in GtkAda:
--  - gtk_container_add_child_arg_type
--  - gtk_container_query_child_args
--  - gtk_container_child_getv
--  - gtk_container_child_setv
--  - gtk_container_add_with_args
--  - gtk_container_addv
--  - gtk_container_child_set
