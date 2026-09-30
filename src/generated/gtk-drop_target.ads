------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  An event controller to receive Drag-and-Drop operations.
--
--  The most basic way to use a `GtkDropTarget` to receive drops on a widget
--  is to create it via [ctorGtk.DropTarget.new], passing in the `GType` of the
--  data you want to receive and connect to the [signalGtk.DropTarget::drop]
--  signal to receive the data:
--
--  ```c static gboolean on_drop (GtkDropTarget *target, const GValue *value,
--  double x, double y, gpointer data) { MyWidget *self = data;
--
--  // Call the appropriate setter depending on the type of data // that we
--  received if (G_VALUE_HOLDS (value, G_TYPE_FILE)) my_widget_set_file (self,
--  g_value_get_object (value)); else if (G_VALUE_HOLDS (value,
--  GDK_TYPE_PIXBUF)) my_widget_set_pixbuf (self, g_value_get_object (value));
--  else return FALSE;
--
--  return TRUE; }
--
--  static void my_widget_init (MyWidget *self) { GtkDropTarget *target =
--  gtk_drop_target_new (G_TYPE_INVALID, GDK_ACTION_COPY);
--
--  // This widget accepts two types of drop types: GFile objects // and
--  GdkPixbuf objects gtk_drop_target_set_gtypes (target, (GType [2]) {
--  G_TYPE_FILE, GDK_TYPE_PIXBUF, }, 2);
--
--  g_signal_connect (target, "drop", G_CALLBACK (on_drop), self);
--  gtk_widget_add_controller (GTK_WIDGET (self), GTK_EVENT_CONTROLLER
--  (target)); } ```
--
--  `GtkDropTarget` supports more options, such as:
--
--   * rejecting potential drops via the [signalGtk.DropTarget::accept] signal
--  and the [methodGtk.DropTarget.reject] function to let other drop targets
--  handle the drop * tracking an ongoing drag operation before the drop via
--  the [signalGtk.DropTarget::enter], [signalGtk.DropTarget::motion] and
--  [signalGtk.DropTarget::leave] signals * configuring how to receive data by
--  setting the [propertyGtk.DropTarget:preload] property and listening for its
--  availability via the [propertyGtk.DropTarget:value] property
--
--  However, `GtkDropTarget` is ultimately modeled in a synchronous way and
--  only supports data transferred via `GType`. If you want full control over
--  an ongoing drop, the [classGtk.DropTargetAsync] object gives you this
--  ability.
--
--  While a pointer is dragged over the drop target's widget and the drop has
--  not been rejected, that widget will receive the
--  Gtk.Enums.Gtk_State_Flag_Drop_Active state, which can be used to style the
--  widget.
--
--  If you are not interested in receiving the drop, but just want to update
--  UI state during a Drag-and-Drop operation (e.g. switching tabs), you can
--  use [classGtk.DropControllerMotion].

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Content_Formats;  use Gdk.Content_Formats;
with Gdk.Drag;             use Gdk.Drag;
with Gdk.Drop;             use Gdk.Drop;
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Properties;      use Glib.Properties;
with Glib.Values;          use Glib.Values;
with Gtk.Event_Controller; use Gtk.Event_Controller;

package Gtk.Drop_Target is

   type Gtk_Drop_Target_Record is new Gtk_Event_Controller_Record with null record;
   type Gtk_Drop_Target is access all Gtk_Drop_Target_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self     : out Gtk_Drop_Target;
       The_Type : GType;
       Actions  : Gdk.Drag.Drag_Action);
   procedure Initialize
      (Self     : not null access Gtk_Drop_Target_Record'Class;
       The_Type : GType;
       Actions  : Gdk.Drag.Drag_Action);
   --  Creates a new `GtkDropTarget` object.
   --  If the drop target should support more than 1 type, pass G_TYPE_INVALID
   --  for Type and then call [methodGtk.DropTarget.set_gtypes].
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param The_Type The supported type or G_TYPE_INVALID
   --  @param Actions the supported actions

   function Gtk_Drop_Target_New
      (The_Type : GType;
       Actions  : Gdk.Drag.Drag_Action) return Gtk_Drop_Target;
   --  Creates a new `GtkDropTarget` object.
   --  If the drop target should support more than 1 type, pass G_TYPE_INVALID
   --  for Type and then call [methodGtk.DropTarget.set_gtypes].
   --  @param The_Type The supported type or G_TYPE_INVALID
   --  @param Actions the supported actions

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_drop_target_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Actions
      (Self : not null access Gtk_Drop_Target_Record)
       return Gdk.Drag.Drag_Action;
   --  Gets the actions that this drop target supports.
   --  @return the actions that this drop target supports

   procedure Set_Actions
      (Self    : not null access Gtk_Drop_Target_Record;
       Actions : Gdk.Drag.Drag_Action);
   --  Sets the actions that this drop target supports.
   --  @param Actions the supported actions

   function Get_Current_Drop
      (Self : not null access Gtk_Drop_Target_Record)
       return Gdk.Drop.Gdk_Drop;
   --  Gets the currently handled drop operation.
   --  If no drop operation is going on, null is returned.
   --  Since: gtk+ 4.4
   --  @return The current drop. Has transfer-ownership='none'.

   function Get_Drop
      (Self : not null access Gtk_Drop_Target_Record)
       return Gdk.Drop.Gdk_Drop;
   pragma Obsolescent (Get_Drop);
   --  Gets the currently handled drop operation.
   --  If no drop operation is going on, null is returned.
   --  Deprecated since 4.4, 1
   --  @return The current drop. Has transfer-ownership='none'.

   function Get_Formats
      (Self : not null access Gtk_Drop_Target_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats;
   --  Gets the data formats that this drop target accepts.
   --  If the result is null, all formats are expected to be supported.
   --  @return the supported data formats. Has transfer-ownership='none'.

   function Get_Preload
      (Self : not null access Gtk_Drop_Target_Record) return Boolean;
   --  Gets whether data should be preloaded on hover.
   --  @return True if drop data should be preloaded

   procedure Set_Preload
      (Self    : not null access Gtk_Drop_Target_Record;
       Preload : Boolean);
   --  Sets whether data should be preloaded on hover.
   --  @param Preload True to preload drop data

   function Get_Value
      (Self : not null access Gtk_Drop_Target_Record)
       return access constant GValue;
   --  Gets the current drop data, as a `GValue`.
   --  @return The current drop data

   procedure Reject (Self : not null access Gtk_Drop_Target_Record);
   --  Rejects the ongoing drop operation.
   --  If no drop operation is ongoing, i.e when
   --  [propertyGtk.DropTarget:current-drop] is null, this function does
   --  nothing.
   --  This function should be used when delaying the decision on whether to
   --  accept a drag or not until after reading the data.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Actions_Property : constant Gdk.Drag.Property_Drag_Action;
   --  Type: Gdk.Drag.Drag_Action
   --  The `GdkDragActions` that this drop target supports.

   Current_Drop_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.Drop.Gdk_Drop
   --  The `GdkDrop` that is currently being performed.

   Drop_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.Drop.Gdk_Drop
   --  The `GdkDrop` that is currently being performed.

   Formats_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Content_Formats
   --  The `GdkContentFormats` that determine the supported data formats.

   Preload_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the drop data should be preloaded when the pointer is only
   --  hovering over the widget but has not been released.
   --
   --  Setting this property allows finer grained reaction to an ongoing drop
   --  at the cost of loading more data.
   --
   --  The default value for this property is False to avoid downloading huge
   --  amounts of data by accident.
   --
   --  For example, if somebody drags a full document of gigabytes of text
   --  from a text editor across a widget with a preloading drop target, this
   --  data will be downloaded, even if the data is ultimately dropped
   --  elsewhere.
   --
   --  For a lot of data formats, the amount of data is very small (like
   --  GDK_TYPE_RGBA), so enabling this property does not hurt at all. And for
   --  local-only Drag-and-Drop operations, no data transfer is done, so
   --  enabling it there is free.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Drop_Target_Gdk_Drop_Boolean is not null access function
     (Self : access Gtk_Drop_Target_Record'Class;
      Drop : not null access Gdk.Drop.Gdk_Drop_Record'Class)
   return Boolean;

   type Cb_GObject_Gdk_Drop_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      Drop : not null access Gdk.Drop.Gdk_Drop_Record'Class)
   return Boolean;

   Signal_Accept : constant Glib.Signal_Name := "accept";
   procedure On_Accept
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_Gtk_Drop_Target_Gdk_Drop_Boolean;
       After : Boolean := False);
   procedure On_Accept
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_GObject_Gdk_Drop_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted on the drop site when a drop operation is about to begin.
   --
   --  If the drop is not accepted, False will be returned and the drop target
   --  will ignore the drop. If True is returned, the drop is accepted for now
   --  but may be rejected later via a call to [methodGtk.DropTarget.reject] or
   --  ultimately by returning False from a [signalGtk.DropTarget::drop]
   --  handler.
   --
   --  The default handler for this signal decides whether to accept the drop
   --  based on the formats provided by the Drop.
   --
   --  If the decision whether the drop will be accepted or rejected depends
   --  on the data, this function should return True, the
   --  [propertyGtk.DropTarget:preload] property should be set and the value
   --  should be inspected via the ::notify:value signal, calling
   --  [methodGtk.DropTarget.reject] if required.
   -- 
   --  Callback parameters:
   --    --  @param Drop the `GdkDrop`

   type Cb_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean is not null access function
     (Self  : access Gtk_Drop_Target_Record'Class;
      Value : Glib.Values.GValue;
      X     : Gdouble;
      Y     : Gdouble) return Boolean;

   type Cb_GObject_GValue_Gdouble_Gdouble_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Value : Glib.Values.GValue;
      X     : Gdouble;
      Y     : Gdouble) return Boolean;

   Signal_Drop : constant Glib.Signal_Name := "drop";
   procedure On_Drop
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean;
       After : Boolean := False);
   procedure On_Drop
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_GObject_GValue_Gdouble_Gdouble_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted on the drop site when the user drops the data onto the widget.
   --
   --  The signal handler must determine whether the pointer position is in a
   --  drop zone or not. If it is not in a drop zone, it returns False and no
   --  further processing is necessary.
   --
   --  Otherwise, the handler returns True. In this case, this handler will
   --  accept the drop. The handler is responsible for using the given Value
   --  and performing the drop operation.
   -- 
   --  Callback parameters:
   --    --  @param Value the `GValue` being dropped
   --    --  @param X the x coordinate of the current pointer position
   --    --  @param Y the y coordinate of the current pointer position

   type Cb_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action is not null access function
     (Self : access Gtk_Drop_Target_Record'Class;
      X    : Gdouble;
      Y    : Gdouble) return Gdk.Drag.Drag_Action;

   type Cb_GObject_Gdouble_Gdouble_Drag_Action is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      X    : Gdouble;
      Y    : Gdouble) return Gdk.Drag.Drag_Action;

   Signal_Enter : constant Glib.Signal_Name := "enter";
   procedure On_Enter
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action;
       After : Boolean := False);
   procedure On_Enter
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Drag_Action;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted on the drop site when the pointer enters the widget.
   --
   --  It can be used to set up custom highlighting.
   -- 
   --  Callback parameters:
   --    --  @param X the x coordinate of the current pointer position
   --    --  @param Y the y coordinate of the current pointer position

   type Cb_Gtk_Drop_Target_Void is not null access procedure
     (Self : access Gtk_Drop_Target_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Leave : constant Glib.Signal_Name := "leave";
   procedure On_Leave
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_Gtk_Drop_Target_Void;
       After : Boolean := False);
   procedure On_Leave
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted on the drop site when the pointer leaves the widget.
   --
   --  Its main purpose it to undo things done in
   --  [signalGtk.DropTarget::enter].

   Signal_Motion : constant Glib.Signal_Name := "motion";
   procedure On_Motion
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action;
       After : Boolean := False);
   procedure On_Motion
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Drag_Action;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted while the pointer is moving over the drop target.
   -- 
   --  Callback parameters:
   --    --  @param X the x coordinate of the current pointer position
   --    --  @param Y the y coordinate of the current pointer position

private
   Preload_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("preload");
   Formats_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("formats");
   Drop_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("drop");
   Current_Drop_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("current-drop");
   Actions_Property : constant Gdk.Drag.Property_Drag_Action :=
     Gdk.Drag.Build ("actions");
end Gtk.Drop_Target;
