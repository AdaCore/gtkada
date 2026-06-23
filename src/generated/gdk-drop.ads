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

--  Represents the target of an ongoing DND operation.
--
--  Possible drop sites get informed about the status of the ongoing drag
--  operation with events of type `GDK_DRAG_ENTER`, `GDK_DRAG_LEAVE`,
--  `GDK_DRAG_MOTION` and `GDK_DROP_START`. The `GdkDrop` object can be
--  obtained from these [classGdk.Event] types using
--  [methodGdk.DNDEvent.get_drop].
--
--  The actual data transfer is initiated from the target side via an async
--  read, using one of the `GdkDrop` methods for this purpose:
--  [methodGdk.Drop.read_async] or [methodGdk.Drop.read_value_async].
--
--  GTK provides a higher level abstraction based on top of these functions,
--  and so they are not normally needed in GTK applications. See the "Drag and
--  Drop" section of the GTK documentation for more information.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Content_Formats; use Gdk.Content_Formats;
with Gdk.Drag;            use Gdk.Drag;
with Glib;                use Glib;
with Glib.Cancellable;    use Glib.Cancellable;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;
with Glib.Values;         use Glib.Values;

package Gdk.Drop is

   type Gdk_Drop_Record is new GObject_Record with null record;
   type Gdk_Drop is access all Gdk_Drop_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gasync_Ready_Callback is access procedure
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   --  Type definition for a function that will be called back when an
   --  asynchronous operation within GIO has been completed.
   --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
   --  invoked in a later iteration of the [thread-default main
   --  context][g-main-context-push-thread-default] where the Gtask.Gtask was
   --  created. All other users of Gasync_Ready_Callback must likewise call it
   --  asynchronously in a later iteration of the main context.
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_drop_get_type");

   -------------
   -- Methods --
   -------------

   procedure Finish
      (Self   : not null access Gdk_Drop_Record;
       Action : Gdk.Drag.Drag_Action);
   --  Ends the drag operation after a drop.
   --  The Action must be a single action selected from the actions available
   --  via [methodGdk.Drop.get_actions].
   --  @param Action the action performed by the destination or
   --  `GDK_ACTION_NONE` if the drop failed

   function Get_Actions
      (Self : not null access Gdk_Drop_Record) return Gdk.Drag.Drag_Action;
   --  Returns the possible actions for this `GdkDrop`.
   --  If this value contains multiple actions - i.e.
   --  [funcGdk.DragAction.is_unique] returns false for the result -
   --  [methodGdk.Drop.finish] must choose the action to use when accepting the
   --  drop. This will only happen if you passed `GDK_ACTION_ASK` as one of the
   --  possible actions in [methodGdk.Drop.status]. `GDK_ACTION_ASK` itself
   --  will not be included in the actions returned by this function.
   --  This value may change over the lifetime of the [classGdk.Drop] both as
   --  a response to source side actions as well as to calls to
   --  [methodGdk.Drop.status] or [methodGdk.Drop.finish]. The source side will
   --  not change this value anymore once a drop has started.
   --  @return The possible `GdkDragActions`

   function Get_Device
      (Self : not null access Gdk_Drop_Record) return Gdk.Gdk_Device;
   --  Returns the `GdkDevice` performing the drop.
   --  @return The `GdkDevice` performing the drop.

   function Get_Display
      (Self : not null access Gdk_Drop_Record) return Gdk.Gdk_Display;
   --  Gets the `GdkDisplay` that Self was created for.
   --  @return a `GdkDisplay`

   function Get_Drag
      (Self : not null access Gdk_Drop_Record) return Gdk.Drag.Gdk_Drag;
   --  If this is an in-app drag-and-drop operation, returns the `GdkDrag`
   --  that corresponds to this drop.
   --  If it is not, `NULL` is returned.
   --  @return the corresponding `GdkDrag`

   function Get_Formats
      (Self : not null access Gdk_Drop_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats;
   --  Returns the `GdkContentFormats` that the drop offers the data to be
   --  read in.
   --  @return The possible `GdkContentFormats`

   function Get_Surface
      (Self : not null access Gdk_Drop_Record) return Gdk.Gdk_Surface;
   --  Returns the `GdkSurface` performing the drop.
   --  @return The `GdkSurface` performing the drop.

   procedure Read_Value_Async
      (Self        : not null access Gdk_Drop_Record;
       The_Type    : GType;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously request the drag operation's contents converted to the
   --  given Type.
   --  For local drag-and-drop operations that are available in the given
   --  `GType`, the value will be copied directly. Otherwise, GDK will try to
   --  use [funcGdk.content_deserialize_async] to convert the data.
   --  @param The_Type a `GType` to read
   --  @param Io_Priority the I/O priority of the request.
   --  @param Cancellable optional `GCancellable` object, null to ignore.
   --  @param Callback callback to call when the request is satisfied

   function Read_Value_Finish
      (Self   : not null access Gdk_Drop_Record;
       Result : Glib.G_Async_Result) return Glib.Values.GValue;
   --  Finishes an async drop read.
   --  See [methodGdk.Drop.read_value_async].
   --  @param Result a `GAsyncResult`
   --  @return a `GValue` containing the result.

   procedure Status
      (Self      : not null access Gdk_Drop_Record;
       Actions   : Gdk.Drag.Drag_Action;
       Preferred : Gdk.Drag.Drag_Action);
   --  Selects all actions that are potentially supported by the destination.
   --  When calling this function, do not restrict the passed in actions to
   --  the ones provided by [methodGdk.Drop.get_actions]. Those actions may
   --  change in the future, even depending on the actions you provide here.
   --  The Preferred action is a hint to the drag-and-drop mechanism about
   --  which action to use when multiple actions are possible.
   --  This function should be called by drag destinations in response to
   --  `GDK_DRAG_ENTER` or `GDK_DRAG_MOTION` events. If the destination does
   --  not yet know the exact actions it supports, it should set any possible
   --  actions first and then later call this function again.
   --  @param Actions Supported actions of the destination, or
   --  `GDK_ACTION_NONE` to indicate that a drop will not be accepted
   --  @param Preferred A unique action that's a member of Actions indicating
   --  the preferred action

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Actions_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Drag_Action
   --  The possible actions for this drop

   Device_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Device
   --  The `GdkDevice` performing the drop

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  The `GdkDisplay` that the drop belongs to.

   Drag_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Drag
   --  The `GdkDrag` that initiated this drop

   Formats_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Content_Formats
   --  The possible formats that the drop can provide its data in.

   Surface_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Surface
   --  The `GdkSurface` the drop happens on

private
   Surface_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("surface");
   Formats_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("formats");
   Drag_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("drag");
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
   Device_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("device");
   Actions_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("actions");
end Gdk.Drop;
