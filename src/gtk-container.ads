-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--  <c_version>1.2.6</c_version>

with Gtk.Adjustment;
with Gtk.Enums;
with Gtk.Object;
with Gtk.Widget;

package Gtk.Container is

   type Gtk_Container_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Container is access all Gtk_Container_Record'Class;

   procedure Set_Border_Width (Container    : access Gtk_Container_Record;
                               Border_Width : in Gint);
   --  Modifies the size of the frame that surrounds the widget. The exact
   --  visual impact depends on the specific widget class.

   procedure Add (Container : access Gtk_Container_Record;
                  Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a new child to the container.
   --  Note that some containers can have only one child. Nothing is done
   --  if there is already a child.
   --  This basically sends the "add" signal (see below)

   procedure Remove (Container : access Gtk_Container_Record;
                     Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a child from the container.
   --  Nothing is done if WIDGET is not a child of CONTAINER. WIDGET is not
   --  destroyed, but is deleted from the screen.
   --  This basically sends the "remove" signal (see below)

   procedure Set_Resize_Mode (Container   : access Gtk_Container_Record;
                              Resize_Mode : in Gtk.Enums.Gtk_Resize_Mode);
   --  Changes the resizing behavior for the CONTAINER.
   --  The default value is Resize_Parent.

   function Children (Container : access Gtk_Container_Record)
                     return Gtk.Widget.Widget_List.Glist;
   --  Returns a list of all the children of the container.

   -----------------------
   -- Foreach functions --
   -----------------------

   type Forall_Function is
     access procedure (Item : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Forall (Container : access Gtk_Container_Record;
                     Func      : Forall_Function);
   --  Executes FUNC for each of the children of CONTAINER.
   --  See also the generic package Forall_Pkg if you want to pass some
   --  extra data to FUNC.

   --  <doc_ignore>
   generic
      type Data_Type is private;
   package Forall_Pkg is
      type Forall_Function is
        access procedure (Item : access Gtk.Widget.Gtk_Widget_Record'Class;
                          Data : Data_Type);

      procedure Forall (Container : access Gtk_Container_Record;
                        Func      : Forall_Function;
                        Data      : Data_Type);
      --  Executes FUNC for each of the children of CONTAINER

   end Forall_Pkg;
   --  </doc_ignore>

   --------------------------
   -- Widget-level methods --
   --------------------------

   procedure Set_Reallocate_Redraws (Container : access Gtk_Container_Record;
                                     Needs_Redraws : Boolean := False);
   --  If NEEDS_REDRAWS is True, then a "draw" signal is emitted for the
   --  CONTAINER whenever one is emitted for a child.

   procedure Set_Focus_Vadjustment
     (Container  : access Gtk_Container_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  ADJUSTMENT should have been created and displayed at some other
   --  place in your application.
   --  CONTAINER will make sure that ADJUSTMENT always matches the range
   --  for the focus widget's position (y .. y + height).

   procedure Set_Focus_Hadjustment
     (Container  : access Gtk_Container_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  ADJUSTMENT should have been created and displayed at some other
   --  place in your application.
   --  CONTAINER will make sure that ADJUSTMENT always matches the range
   --  for the focus widget's position (x .. x + width).

   function Get_Toplevels return Gtk.Widget.Widget_List.Glist;
   --  Returns the list of all the toplevel widgets, ie the ones that don't
   --  have any parent (windows, dialogs, ...)

   procedure Register_Toplevel (Container : access Gtk_Container_Record);
   --  Registers CONTAINER as a toplevel widget, returned by the subprogram
   --  Get_Toplevels.

   procedure Unregister_Toplevel (Container : access Gtk_Container_Record);
   --  Unregisters CONTAINER as a toplevel widget.

   function Child_Type (Container : access Gtk_Container_Record)
                       return Gtk.Gtk_Type;
   --  Returns the type of the children in CONTAINER.
   --  If CONTAINER can contain any type of widget, Gtk_Type_None is
   --  returned.

   ----------------------
   -- Signals emission --
   ----------------------

   procedure Check_Resize (Container : access Gtk_Container_Record);
   --  Emits the "check_resize" signal

   function Focus (Container : access Gtk_Container_Record;
                   Direction : Gtk.Enums.Gtk_Direction_Type)
                  return Boolean;
   --  Emits the "focus" signal

   procedure Set_Focus_Child
     (Container : access Gtk_Container_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Emits a "set_focus_child" signal.

   ----------------------------
   -- Support for GATE/DGATE --
   ----------------------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);
   --  Gate internal function

   procedure Generate (Container : in out Gtk.Object.Gtk_Object;
                       N         : in Node_Ptr);
   --  Dgate internal function

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
   --    Called every time the CONTAINER needs resizing.
   --    Upon receiving this signal, CONTAINER should check whether it needs
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

end Gtk.Container;

--  functions that have no equivalent in GtkAda:
--  - gtk_container_add_child_arg_type
--  - gtk_container_query_child_args
--  - gtk_container_child_getv
--  - gtk_container_child_setv
--  - gtk_container_add_with_args
--  - gtk_container_addv
--  - gtk_container_child_set
