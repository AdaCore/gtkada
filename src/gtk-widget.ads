-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
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

--  The widget is the base of the tree for displayable objects.
--  (A displayable object is one which takes up some amount
--  of screen real estate). It provides a common base and interface
--  which actual widgets must adhere to.

with Gtk.Object;
with Gdk.Types;
with Gdk.Rectangle;
with System;

package Gtk.Widget is

   --  Flags used by Widget on top of Object
   TopLevel         : constant := 2 ** 4;
   No_Window        : constant := 2 ** 5;
   Realized         : constant := 2 ** 6;
   Mapped           : constant := 2 ** 7;
   Visible          : constant := 2 ** 8;
   Sensitive        : constant := 2 ** 9;
   Parent_Sensitive : constant := 2 ** 10;
   Can_Focus        : constant := 2 ** 11;
   Has_Focus        : constant := 2 ** 12;
   Can_Default      : constant := 2 ** 13;
   Has_Default      : constant := 2 ** 14;
   Has_Grab         : constant := 2 ** 15;
   Basic            : constant := 2 ** 16;
   Reserved_3       : constant := 2 ** 17;
   Rc_Style         : constant := 2 ** 18;

   type Gtk_Widget is new Object.Gtk_Object with private;
   type Gtk_Widget_Access is access all Gtk_Widget'Class;


   procedure Activate (Widget : in out Gtk_Widget'Class);
   --  mapping: Activate gtkwidget.h gtk_widget_activate

   procedure Destroy (Widget : in out Gtk_Widget'Class);
   --  mapping: Destroy gtkwidget.h gtk_widget_destroy

   procedure Destroyed (Dummy  : in out Gtk_Widget'Class;
                        Widget : in out Gtk_Widget_Access);
   --  mapping: Destroyed gtkwidget.h gtk_widget_destroyed
   --  Set Widget to NULL

   --  GET_WINDOW : to get the window field of a widget, please
   --  see package gtk-window.ads

   procedure Draw
     (Widget : in Gtk_Widget'Class;
      Area   : in Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area);

   --  mapping: Draw gtkwidget.h gtk_widget_draw
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_draw_children
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_draw_focus
   --  FIXME --  Need Gdk_Rectangle



   procedure Set_Name (Widget : in out Gtk_Widget'Class;
                       Name : in String);
   --  mapping: Set_Name gtkwidget.h gtk_widget_set_name

   procedure Set_Sensitive (Widget    : in out Gtk_Widget'Class;
                            Sensitive : in Boolean := True);
   --  mapping: Set_Sensitive gtkwidget.h gtk_widget_set_sensitive

   procedure Set_UPosition (Widget : in Gtk_Widget'Class;
                            X, Y   : in Gint);
   --  mapping: Set_UPosition gtkwidget.h gtk_widget_set_uposition

   procedure Set_USize (Widget : in Gtk_Widget'Class;
                        Width  : in Gint;
                        Height : in Gint);
   --  mapping: Set_USize gtkwidget.h gtk_widget_set_usize


   procedure Show (Widget : in out Gtk_Widget'Class);
   --  mapping: Show gtkwidget.h gtk_widget_show

   procedure Show_All (Widget : in out Gtk_Widget'Class);
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_show_all


   procedure Hide (Widget : in out Gtk_Widget'Class);
   --  mapping: Hide gtkwidget.h gtk_widget_hide


   procedure Map (Widget : in out Gtk_Widget'Class);
   --  mapping: Map gtkwidget.h gtk_widget_map

   procedure Unmap (Widget : in out Gtk_Widget'Class);
   --  mapping: Unmap gtkwidget.h gtk_widget_unmap

   procedure Realize (Widget : in out Gtk_Widget'Class);
   --  mapping: Realize gtkwidget.h gtk_widget_realize

   procedure Unrealize (Widget : in out Gtk_Widget'Class);
   --  mapping: Unrealize gtkwidget.h gtk_widget_unrealize

   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_install_accelerator
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_remove_accelerator
   --  FIXME  --  need Gtk_Accelerator

   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_event
   --  FIXME  --   need Gdk_Event.

   procedure Reparent (Widget : in out Gtk_Widget'Class;
                       New_Parent : in Gtk_Widget'Class);
   --  mapping: Reparent gtkwidget.h gtk_widget_reparent

   function Get_Parent (Widget : in Gtk_Widget'Class)
                        return Gtk_Widget'Class;
   --  mapping: Get_Parent gtkwidget.h GtkWidget->parent

   procedure Popup (Widget : in out Gtk_Widget'Class;
                    X, Y : in Gint);
   --  mapping: Popup gtkwidget.h gtk_widget_popup

   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_intersect
   --  FIXME  --  need Gdk_Rectangle

   procedure Grab_Default (Widget : in out Gtk_Widget'Class);
   --  mapping: Grab_Default gtkwidget.h gtk_widget_grab_default

   procedure Grab_Focus (Widget : in out Gtk_Widget'Class);
   --  mapping: Grab_Focus gtkwidget.h gtk_widget_grab_focus

   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_colormap
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_default_colormap
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_default_colormap
   --  FIXME  --  need Gdk_Colormap

   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_visual
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_default_visual
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_default_visual
   --  FIXME  --  need Gdk_Visual

   --  GET_STYLE : to get the style field of a widget, please see
   --  package gtk-style.ads

   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_default_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_default_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_restore_default_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_ensure_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_reset_rc_styles

   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_state
   --  FIXME  --  need Gtk_State (enumerated type)

   procedure Set_Parent (Widget : in out Gtk_Widget'Class;
                         Parent : in     Gtk_Widget'Class);
   --  mapping: Set_Parent gtkwidget.h gtk_widget_set_parent

   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_ancestor

   procedure Get_Toplevel (Widget : in Gtk_Widget'Class;
                           Result : out Gtk_Widget'Class);
   --  mapping: Get_Toplevel gtkwidget.h gtk_widget_get_toplevel


   function Get_Events (Widget : in Gtk_Widget'Class) return Gint;
   --  mapping: Get_Events gtkwidget.h gtk_widget_get_events

   procedure Set_Events (Widget : in out Gtk_Widget'Class;
                         Events : in     Gdk.Types.Gdk_Event_Mask);
   --  mapping: Set_Events gtkwidget.h gtk_widget_set_events


   --------------------
   --  Widget flags  --
   --------------------

   function Toplevel_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function No_Window_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function Realized_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function Mapped_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function Visible_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function Drawable_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function Sensitive_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function Parent_Sensitive_Is_Set (Widget : in Gtk_Widget'Class)
                                     return Boolean;
   function Is_Sensitive_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function Can_Focus_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function Has_Focus_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function Has_Default_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function Has_Grab_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function Basic_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;
   function Rc_Style_Is_Set (Widget : in Gtk_Widget'Class) return Boolean;

   --  Drag'n drop stuff, will be implemented later.
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_dnd_data_set
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_dnd_drag_add
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_dnd_drag_set
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_dnd_drop_set
   --  FIXME  --  need Gdk_Event

   -----------------------
   -- Default callbacks --
   -----------------------

   function Get_Default_Motion_Notify_Event (Widget : in Gtk_Widget'Class)
                                             return System.Address;

   --  These methods are available only for use with
   --  Gtk.Signal.C_Unsafe_Connect

private

   type Gtk_Widget is new Object.Gtk_Object with null record;

   --  mapping: TopLevel gtkwidget.h GTK_TOPLEVEL
   --  mapping: No_Window gtkwidget.h GTK_NOWINDOW
   --  mapping: Realized gtkwidget.h GTK_REALIZED
   --  mapping: Mapped gtkwidget.h GTK_MAPPED
   --  mapping: Visible gtkwidget.h GTK_VISIBLE
   --  mapping: Sensitive gtkwidget.h GTK_SENSITIVE
   --  mapping: Parent_Sensitive gtkwidget.h GTK_PARENT_SENSITIVE
   --  mapping: Can_Focus gtkwidget.h GTK_CAN_FOCUS
   --  mapping: Has_Focus gtkwidget.h GTK_HAS_FOCUS
   --  mapping: Can_Default gtkwidget.h GTK_CAN_DEFAULT
   --  mapping: Has_Default gtkwidget.h GTK_HAS_DEFAULT
   --  mapping: Has_Grab gtkwidget.h GTK_HAS_GRAB
   --  mapping: Basic gtkwidget.h GTK_BASIC
   --  mapping: Reserved_3 gtkwidget.h GTK_RESERVED_3
   --  mapping: Rc_Style gtkwidget.h GTK_RC_STYLE

   --  mapping: USE_OBJECT_ORIENTED gtkwidget.h gtk_widget_get_type
   --  mapping: USE_OBJECT_ORIENTED gtkwidget.h gtk_widget_new

   --  mapping: INTERNAL gtkwidget.h gtk_widget_basic
   --  mapping: INTERNAL gtkwidget.h gtk_widget_draw_default
   --  mapping: INTERNAL gtkwidget.h gtk_widget_init
   --  mapping: INTERNAL gtkwidget.h gtk_widget_propagate_default_style
   --  mapping: INTERNAL gtkwidget.h gtk_widget_size_allocate
   --  mapping: INTERNAL gtkwidget.h gtk_widget_size_request
   --  mapping: INTERNAL gtkwidget.h gtk_widget_get
   --  mapping: INTERNAL gtkwidget.h gtk_widget_get_extension_events
   --  mapping: INTERNAL gtkwidget.h gtk_widget_get_name
   --  mapping: INTERNAL gtkwidget.h gtk_widget_get_pointer
   --  mapping: INTERNAL gtkwidget.h gtk_widget_getv
   --  mapping: INTERNAL gtkwidget.h gtk_widget_hide_all
   --  mapping: INTERNAL gtkwidget.h gtk_widget_hide_on_delete
   --  mapping: INTERNAL gtkwidget.h gtk_widget_is_ancestor
   --  mapping: INTERNAL gtkwidget.h gtk_widget_is_child
   --  mapping: INTERNAL gtkwidget.h gtk_widget_newv
   --  mapping: INTERNAL gtkwidget.h gtk_widget_pop_colormap
   --  mapping: INTERNAL gtkwidget.h gtk_widget_pop_style
   --  mapping: INTERNAL gtkwidget.h gtk_widget_pop_visual
   --  mapping: INTERNAL gtkwidget.h gtk_widget_push_colormap
   --  mapping: INTERNAL gtkwidget.h gtk_widget_push_style
   --  mapping: INTERNAL gtkwidget.h gtk_widget_push_visual
   --  mapping: INTERNAL gtkwidget.h gtk_widget_queue_draw
   --  mapping: INTERNAL gtkwidget.h gtk_widget_queue_resize
   --  mapping: INTERNAL gtkwidget.h gtk_widget_ref

   --  Services not mapped because they are probably not needed.
   --
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_extension_events
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_parent_window
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_rc_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_setv
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_shape_combine_mask
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_show_now
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_unparent
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_unref



end Gtk.Widget;
