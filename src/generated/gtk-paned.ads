------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

pragma Ada_05;
--  <description>
--  Gtk.Paned.Gtk_Paned has two panes, arranged either horizontally or
--  vertically. The division between the two panes is adjustable by the user by
--  dragging a handle.
--
--  Child widgets are added to the panes of the widget with Gtk.Paned.Pack1
--  and Gtk.Paned.Pack2. The division between the two children is set by
--  default from the size requests of the children, but it can be adjusted by
--  the user.
--
--  A paned widget draws a separator between the two child widgets and a small
--  handle that the user can drag to adjust the division. It does not draw any
--  relief around the children or around the separator. (The space in which the
--  separator is called the gutter.) Often, it is useful to put each child
--  inside a Gtk.Frame.Gtk_Frame with the shadow type set to
--  Gtk.Enums.Shadow_In so that the gutter appears as a ridge. No separator is
--  drawn if one of the children is missing.
--
--  Each child has two options that can be set, Resize and Shrink. If Resize
--  is true, then when the Gtk.Paned.Gtk_Paned is resized, that child will
--  expand or shrink along with the paned widget. If Shrink is true, then that
--  child can be made smaller than its requisition by the user. Setting Shrink
--  to False allows the application to set a minimum size. If Resize is false
--  for both children, then this is treated as if Resize is true for both
--  children.
--
--  The application can set the position of the slider as if it were set by
--  the user, by calling Gtk.Paned.Set_Position.
--
--  == Creating a paned widget with minimum sizes. ==
--
--    GtkWidget *hpaned = gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
--    GtkWidget *frame1 = gtk_frame_new (NULL);
--    GtkWidget *frame2 = gtk_frame_new (NULL);
--    gtk_frame_set_shadow_type (GTK_FRAME (frame1), GTK_SHADOW_IN);
--    gtk_frame_set_shadow_type (GTK_FRAME (frame2), GTK_SHADOW_IN);
--    gtk_widget_set_size_request (hpaned, 200, -1);
--    gtk_paned_pack1 (GTK_PANED (hpaned), frame1, TRUE, FALSE);
--    gtk_widget_set_size_request (frame1, 50, -1);
--    gtk_paned_pack2 (GTK_PANED (hpaned), frame2, FALSE, FALSE);
--    gtk_widget_set_size_request (frame2, 50, -1);
--
--
--  </description>
--  <screenshot>gtk-paned</screenshot>
--  <group>Layout container</group>
--  <testgtk>create_paned.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Window;      use Gdk.Window;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Paned is

   type Gtk_Paned_Record is new Gtk_Container_Record with null record;
   type Gtk_Paned is access all Gtk_Paned_Record'Class;

   subtype Gtk_Hpaned_Record is Gtk_Paned_Record;
   subtype Gtk_Hpaned is Gtk_Paned;

   subtype Gtk_Vpaned_Record is Gtk_Paned_Record;
   subtype Gtk_Vpaned is Gtk_Paned;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Paned       : out Gtk_Paned;
       Orientation : Gtk.Enums.Gtk_Orientation);
   procedure Initialize
      (Paned       : access Gtk_Paned_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation);
   --  Creates a new Gtk.Paned.Gtk_Paned widget.
   --  Since: gtk+ 3.0
   --  "orientation": the paned's orientation.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_paned_get_type");

   procedure Gtk_New_Hpaned (Paned : out Gtk_Hpaned);
   procedure Initialize_Hpaned (Paned : access Gtk_Hpaned_Record'Class);
   --  The children will be displayed next to each other

   function Get_Type_Hpaned return Glib.GType;
   pragma Import (C, Get_Type_Hpaned, "gtk_hpaned_get_type");

   procedure Gtk_New_Vpaned (Paned : out Gtk_Vpaned);
   procedure Initialize_Vpaned (Paned : access Gtk_Vpaned_Record'Class);
   --  The children will be displayed one on top of the other

   function Get_Type_Vpaned return Glib.GType;
   pragma Import (C, Get_Type_Vpaned, "gtk_vpaned_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add1
      (Paned : not null access Gtk_Paned_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Add the first child of the container. The child will be displayed
   --  either in the top or in the left pane, depending on the orientation of
   --  the container. This is equivalent to using the Pack1 procedure with its
   --  default parameters.
   --  "child": the child to add

   procedure Add2
      (Paned : not null access Gtk_Paned_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Add the second child of the container. It will be displayed in the
   --  bottom or right pane, depending on the container's orientation. This is
   --  equivalent to using Pack2 with its default parameters.
   --  "child": the child to add

   function Get_Child1
      (Paned : not null access Gtk_Paned_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Obtains the first child of the paned widget.
   --  Since: gtk+ 2.4

   function Get_Child2
      (Paned : not null access Gtk_Paned_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Obtains the second child of the paned widget.
   --  Since: gtk+ 2.4

   function Get_Handle_Window
      (Paned : not null access Gtk_Paned_Record)
       return Gdk.Window.Gdk_Window;
   --  Returns the Gdk.Window.Gdk_Window of the handle. This function is
   --  useful when handling button or motion events because it enables the
   --  callback to distinguish between the window of the paned, a child and the
   --  handle.
   --  Since: gtk+ 2.20

   function Get_Position
      (Paned : not null access Gtk_Paned_Record) return Gint;
   procedure Set_Position
      (Paned    : not null access Gtk_Paned_Record;
       Position : Gint);
   --  Sets the position of the divider between the two panes.
   --  "position": pixel position of divider, a negative value means that the
   --  position is unset.

   procedure Pack1
      (Paned  : not null access Gtk_Paned_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Resize : Boolean := False;
       Shrink : Boolean := True);
   --  Add a child to the top or left pane. You can not change dynamically the
   --  attributes Resize and Shrink. Instead, you have to remove the child from
   --  the container, and put it back with the new value of the attributes. You
   --  should also first call Glib.Object.Ref on the child so as to be sure it
   --  is not destroyed when you remove it, and Glib.Object.Unref it at the
   --  end. See the example in testgtk/ in the GtkAda distribution.
   --  "child": the child to add
   --  "resize": should this child expand when the paned widget is resized.
   --  "shrink": can this child be made smaller than its requisition.

   procedure Pack2
      (Paned  : not null access Gtk_Paned_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Resize : Boolean := False;
       Shrink : Boolean := False);
   --  Adds a child to the bottom or right pane.
   --  "child": the child to add
   --  "resize": should this child expand when the paned widget is resized.
   --  "shrink": can this child be made smaller than its requisition.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Paned_Record)
       return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : not null access Gtk_Paned_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Paned_Record, Gtk_Paned);
   function "+"
     (Widget : access Gtk_Paned_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Paned
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Paned_Record, Gtk_Paned);
   function "+"
     (Widget : access Gtk_Paned_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Paned
   renames Implements_Orientable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Max_Position_Property
   --  Type: Gint
   --  Flags: read-write
   --  The largest possible value for the position property. This property is
   --  derived from the size and shrinkability of the widget's children.
   --
   --  Name: Min_Position_Property
   --  Type: Gint
   --  Flags: read-write
   --  The smallest possible value for the position property. This property is
   --  derived from the size and shrinkability of the widget's children.
   --
   --  Name: Position_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Position_Set_Property
   --  Type: Boolean
   --  Flags: read-write

   Max_Position_Property : constant Glib.Properties.Property_Int;
   Min_Position_Property : constant Glib.Properties.Property_Int;
   Position_Property : constant Glib.Properties.Property_Int;
   Position_Set_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "accept-position"
   --     function Handler
   --       (Self : access Gtk_Paned_Record'Class) return Boolean;
   --  The ::accept-position signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to accept the current position of the handle when moving it using key
   --  bindings.
   --  The default binding for this signal is Return or Space.
   --
   --  "cancel-position"
   --     function Handler
   --       (Self : access Gtk_Paned_Record'Class) return Boolean;
   --  The ::cancel-position signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to cancel moving the position of the handle using key bindings. The
   --  position of the handle will be reset to the value prior to moving it.
   --  The default binding for this signal is Escape.
   --
   --  "cycle-child-focus"
   --     function Handler
   --       (Self     : access Gtk_Paned_Record'Class;
   --        Reversed : Boolean) return Boolean;
   --    --  "reversed": whether cycling backward or forward
   --  The ::cycle-child-focus signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to cycle the focus between the children of the paned.
   --  The default binding is f6.
   --
   --  "cycle-handle-focus"
   --     function Handler
   --       (Self     : access Gtk_Paned_Record'Class;
   --        Reversed : Boolean) return Boolean;
   --    --  "reversed": whether cycling backward or forward
   --  The ::cycle-handle-focus signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to cycle whether the paned should grab focus to allow the user to change
   --  position of the handle by using key bindings.
   --  The default binding for this signal is f8.
   --
   --  "move-handle"
   --     function Handler
   --       (Self        : access Gtk_Paned_Record'Class;
   --        Scroll_Type : Gtk.Enums.Gtk_Scroll_Type) return Boolean;
   --    --  "scroll_type": a Gtk.Enums.Gtk_Scroll_Type
   --  The ::move-handle signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to move the handle when the user is using key bindings to move it.
   --
   --  "toggle-handle-focus"
   --     function Handler
   --       (Self : access Gtk_Paned_Record'Class) return Boolean;
   --  The ::toggle-handle-focus is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to accept the current position of the handle and then move focus to the
   --  next widget in the focus chain.
   --  The default binding is Tab.

   Signal_Accept_Position : constant Glib.Signal_Name := "accept-position";
   Signal_Cancel_Position : constant Glib.Signal_Name := "cancel-position";
   Signal_Cycle_Child_Focus : constant Glib.Signal_Name := "cycle-child-focus";
   Signal_Cycle_Handle_Focus : constant Glib.Signal_Name := "cycle-handle-focus";
   Signal_Move_Handle : constant Glib.Signal_Name := "move-handle";
   Signal_Toggle_Handle_Focus : constant Glib.Signal_Name := "toggle-handle-focus";

private
   Max_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-position");
   Min_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("min-position");
   Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("position");
   Position_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("position-set");
end Gtk.Paned;
