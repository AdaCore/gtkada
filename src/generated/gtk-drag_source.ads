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

--  An event controller to initiate Drag-And-Drop operations.
--
--  `GtkDragSource` can be set up with the necessary ingredients for a DND
--  operation ahead of time. This includes the source for the data that is
--  being transferred, in the form of a [classGdk.ContentProvider], the desired
--  action, and the icon to use during the drag operation. After setting it up,
--  the drag source must be added to a widget as an event controller, using
--  [methodGtk.Widget.add_controller].
--
--  ```c static void my_widget_init (MyWidget *self) { GtkDragSource
--  *drag_source = gtk_drag_source_new ();
--
--  g_signal_connect (drag_source, "prepare", G_CALLBACK (on_drag_prepare),
--  self); g_signal_connect (drag_source, "drag-begin", G_CALLBACK
--  (on_drag_begin), self);
--
--  gtk_widget_add_controller (GTK_WIDGET (self), GTK_EVENT_CONTROLLER
--  (drag_source)); } ```
--
--  Setting up the content provider and icon ahead of time only makes sense
--  when the data does not change. More commonly, you will want to set them up
--  just in time. To do so, `GtkDragSource` has [signalGtk.DragSource::prepare]
--  and [signalGtk.DragSource::drag-begin] signals.
--
--  The ::prepare signal is emitted before a drag is started, and can be used
--  to set the content provider and actions that the drag should be started
--  with.
--
--  ```c static GdkContentProvider * on_drag_prepare (GtkDragSource *source,
--  double x, double y, MyWidget *self) { // This widget supports two types of
--  content: GFile objects // and GdkPixbuf objects; GTK will handle the
--  serialization // of these types automatically GFile *file =
--  my_widget_get_file (self); GdkPixbuf *pixbuf = my_widget_get_pixbuf (self);
--
--  return gdk_content_provider_new_union ((GdkContentProvider *[2]) {
--  gdk_content_provider_new_typed (G_TYPE_FILE, file),
--  gdk_content_provider_new_typed (GDK_TYPE_PIXBUF, pixbuf), }, 2); } ```
--
--  The ::drag-begin signal is emitted after the `GdkDrag` object has been
--  created, and can be used to set up the drag icon.
--
--  ```c static void on_drag_begin (GtkDragSource *source, GdkDrag *drag,
--  MyWidget *self) { // Set the widget as the drag icon GdkPaintable
--  *paintable = gtk_widget_paintable_new (GTK_WIDGET (self));
--  gtk_drag_source_set_icon (source, paintable, 0, 0); g_object_unref
--  (paintable); } ```
--
--  During the DND operation, `GtkDragSource` emits signals that can be used
--  to obtain updates about the status of the operation, but it is not normally
--  necessary to connect to any signals, except for one case: when the
--  supported actions include Gdk.Drag.Gdk_Action_Move, you need to listen for
--  the [signalGtk.DragSource::drag-end] signal and delete the data after it
--  has been transferred.
--
--  <group>Drag and Drop</group>
--  <gtkada_demo>create_drag_and_drop.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Content_Provider; use Gdk.Content_Provider;
with Gdk.Drag;             use Gdk.Drag;
with Gdk.Paintable;        use Gdk.Paintable;
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Properties;      use Glib.Properties;
with Gtk.Gesture_Single;   use Gtk.Gesture_Single;

package Gtk.Drag_Source is

   type Gtk_Drag_Source_Record is new Gtk_Gesture_Single_Record with null record;
   type Gtk_Drag_Source is access all Gtk_Drag_Source_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Drag_Source);
   procedure Initialize
      (Self : not null access Gtk_Drag_Source_Record'Class);
   --  Creates a new `GtkDragSource` object.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Drag_Source_New return Gtk_Drag_Source;
   --  Creates a new `GtkDragSource` object.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_drag_source_get_type");

   -------------
   -- Methods --
   -------------

   procedure Drag_Cancel (Self : not null access Gtk_Drag_Source_Record);
   --  Cancels a currently ongoing drag operation.

   function Get_Actions
      (Self : not null access Gtk_Drag_Source_Record)
       return Gdk.Drag.Drag_Action;
   --  Gets the actions that are currently set on the `GtkDragSource`.
   --  @return the actions set on Source

   procedure Set_Actions
      (Self    : not null access Gtk_Drag_Source_Record;
       Actions : Gdk.Drag.Drag_Action);
   --  Sets the actions on the `GtkDragSource`.
   --  During a DND operation, the actions are offered to potential drop
   --  targets. If Actions include Gdk.Drag.Gdk_Action_Move, you need to listen
   --  to the [signalGtk.DragSource::drag-end] signal and handle Delete_Data
   --  being True.
   --  This function can be called before a drag is started, or in a handler
   --  for the [signalGtk.DragSource::prepare] signal.
   --  @param Actions the actions to offer

   function Get_Content
      (Self : not null access Gtk_Drag_Source_Record)
       return Gdk.Content_Provider.Gdk_Content_Provider;
   --  Gets the current content provider of a `GtkDragSource`.
   --  @return the `GdkContentProvider` of Source. Has
   --  transfer-ownership='none'.

   procedure Set_Content
      (Self    : not null access Gtk_Drag_Source_Record;
       Content : access Gdk.Content_Provider.Gdk_Content_Provider_Record'Class);
   --  Sets a content provider on a `GtkDragSource`.
   --  When the data is requested in the cause of a DND operation, it will be
   --  obtained from the content provider.
   --  This function can be called before a drag is started, or in a handler
   --  for the [signalGtk.DragSource::prepare] signal.
   --  You may consider setting the content provider back to null in a
   --  [signalGtk.DragSource::drag-end] signal handler.
   --  @param Content a `GdkContentProvider`

   function Get_Drag
      (Self : not null access Gtk_Drag_Source_Record)
       return Gdk.Drag.Gdk_Drag;
   --  Returns the underlying `GdkDrag` object for an ongoing drag.
   --  @return the `GdkDrag` of the current drag operation. Has
   --  transfer-ownership='none'.

   procedure Set_Icon
      (Self      : not null access Gtk_Drag_Source_Record;
       Paintable : Gdk.Paintable.Gdk_Paintable;
       Hot_X     : Glib.Gint;
       Hot_Y     : Glib.Gint);
   --  Sets a paintable to use as icon during DND operations.
   --  The hotspot coordinates determine the point on the icon that gets
   --  aligned with the hotspot of the cursor.
   --  If Paintable is null, a default icon is used.
   --  This function can be called before a drag is started, or in a
   --  [signalGtk.DragSource::prepare] or [signalGtk.DragSource::drag-begin]
   --  signal handler.
   --  @param Paintable the `GdkPaintable` to use as icon
   --  @param Hot_X the hotspot X coordinate on the icon
   --  @param Hot_Y the hotspot Y coordinate on the icon

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Actions_Property : constant Gdk.Drag.Property_Drag_Action;
   --  Type: Gdk.Drag.Drag_Action
   --  The actions that are supported by drag operations from the source.
   --
   --  Note that you must handle the [signalGtk.DragSource::drag-end] signal
   --  if the actions include Gdk.Drag.Gdk_Action_Move.

   Content_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.Content_Provider.Gdk_Content_Provider
   --  The data that is offered by drag operations from this source.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Drag_Source_Gdk_Drag_Void is not null access procedure
     (Self : access Gtk_Drag_Source_Record'Class;
      Drag : not null access Gdk.Drag.Gdk_Drag_Record'Class);

   type Cb_GObject_Gdk_Drag_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Drag : not null access Gdk.Drag.Gdk_Drag_Record'Class);

   Signal_Drag_Begin : constant Glib.Signal_Name := "drag-begin";
   procedure On_Drag_Begin
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_Gtk_Drag_Source_Gdk_Drag_Void;
       After : Boolean := False);
   procedure On_Drag_Begin
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_GObject_Gdk_Drag_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted on the drag source when a drag is started.
   --
   --  It can be used to e.g. set a custom drag icon with
   --  [methodGtk.DragSource.set_icon].

   type Cb_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean is not null access function
     (Self   : access Gtk_Drag_Source_Record'Class;
      Drag   : not null access Gdk.Drag.Gdk_Drag_Record'Class;
      Reason : Gdk.Drag.Drag_Cancel_Reason) return Boolean;

   type Cb_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean is not null access function
     (Self   : access Glib.Object.GObject_Record'Class;
      Drag   : not null access Gdk.Drag.Gdk_Drag_Record'Class;
      Reason : Gdk.Drag.Drag_Cancel_Reason) return Boolean;

   Signal_Drag_Cancel : constant Glib.Signal_Name := "drag-cancel";
   procedure On_Drag_Cancel
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean;
       After : Boolean := False);
   procedure On_Drag_Cancel
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted on the drag source when a drag has failed.
   --
   --  The signal handler may handle a failed drag operation based on the type
   --  of error. It should return True if the failure has been handled and the
   --  default "drag operation failed" animation should not be shown.
   -- 
   --  Callback parameters:
   --    --  @param Drag the `GdkDrag` object
   --    --  @param Reason information on why the drag failed

   type Cb_Gtk_Drag_Source_Gdk_Drag_Boolean_Void is not null access procedure
     (Self        : access Gtk_Drag_Source_Record'Class;
      Drag        : not null access Gdk.Drag.Gdk_Drag_Record'Class;
      Delete_Data : Boolean);

   type Cb_GObject_Gdk_Drag_Boolean_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Drag        : not null access Gdk.Drag.Gdk_Drag_Record'Class;
      Delete_Data : Boolean);

   Signal_Drag_End : constant Glib.Signal_Name := "drag-end";
   procedure On_Drag_End
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_Gtk_Drag_Source_Gdk_Drag_Boolean_Void;
       After : Boolean := False);
   procedure On_Drag_End
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_GObject_Gdk_Drag_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted on the drag source when a drag is finished.
   --
   --  A typical reason to connect to this signal is to undo things done in
   --  [signalGtk.DragSource::prepare] or [signalGtk.DragSource::drag-begin]
   --  handlers.
   -- 
   --  Callback parameters:
   --    --  @param Drag the `GdkDrag` object
   --    --  @param Delete_Data True if the drag was performing
   --    --  Gdk.Drag.Gdk_Action_Move, and the data should be deleted

   type Cb_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider is not null access function
     (Self : access Gtk_Drag_Source_Record'Class;
      X    : Gdouble;
      Y    : Gdouble)
   return Gdk.Content_Provider.Gdk_Content_Provider;

   type Cb_GObject_Gdouble_Gdouble_Gdk_Content_Provider is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      X    : Gdouble;
      Y    : Gdouble)
   return Gdk.Content_Provider.Gdk_Content_Provider;

   Signal_Prepare : constant Glib.Signal_Name := "prepare";
   procedure On_Prepare
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider;
       After : Boolean := False);
   procedure On_Prepare
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Gdk_Content_Provider;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a drag is about to be initiated.
   --
   --  It returns the `GdkContentProvider` to use for the drag that is about
   --  to start. The default handler for this signal returns the value of the
   --  [propertyGtk.DragSource:content] property, so if you set up that
   --  property ahead of time, you don't need to connect to this signal.
   -- 
   --  Callback parameters:
   --    --  @param X the X coordinate of the drag starting point
   --    --  @param Y the Y coordinate of the drag starting point

private
   Content_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("content");
   Actions_Property : constant Gdk.Drag.Property_Drag_Action :=
     Gdk.Drag.Build ("actions");
end Gtk.Drag_Source;
