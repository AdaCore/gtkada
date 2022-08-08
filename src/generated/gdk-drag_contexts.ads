------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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


pragma Warnings (Off, "*is already use-visible*");
with Gdk.Device;              use Gdk.Device;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;

package Gdk.Drag_Contexts is

   type Drag_Context_Record is new GObject_Record with null record;
   type Drag_Context is access all Drag_Context_Record'Class;

   type Gdk_Drag_Action is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Drag_Action);
   --  Used in Gdk.Drag_Contexts.Drag_Context to indicate what the destination
   --  should do with the dropped data.

   Action_Default : constant Gdk_Drag_Action := 1;
   Action_Copy : constant Gdk_Drag_Action := 2;
   Action_Move : constant Gdk_Drag_Action := 4;
   Action_Link : constant Gdk_Drag_Action := 8;
   Action_Private : constant Gdk_Drag_Action := 16;
   Action_Ask : constant Gdk_Drag_Action := 32;

   type Gdk_Drag_Cancel_Reason is (
      Drag_Cancel_No_Target,
      Drag_Cancel_User_Cancelled,
      Drag_Cancel_Error);
   pragma Convention (C, Gdk_Drag_Cancel_Reason);
   --  Used in Gdk.Drag_Contexts.Drag_Context to the reason of a cancelled DND
   --  operation.

   type Gdk_Drag_Protocol is (
      Drag_Proto_None,
      Drag_Proto_Motif,
      Drag_Proto_Xdnd,
      Drag_Proto_Rootwin,
      Drag_Proto_Win32_Dropfiles,
      Drag_Proto_Ole2,
      Drag_Proto_Local,
      Drag_Proto_Wayland);
   pragma Convention (C, Gdk_Drag_Protocol);
   --  Used in Gdk.Drag_Contexts.Drag_Context to indicate the protocol
   --  according to which DND is done.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Drag_Action_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Drag_Action);
   type Property_Gdk_Drag_Action is new Gdk_Drag_Action_Properties.Property;

   package Gdk_Drag_Cancel_Reason_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Drag_Cancel_Reason);
   type Property_Gdk_Drag_Cancel_Reason is new Gdk_Drag_Cancel_Reason_Properties.Property;

   package Gdk_Drag_Protocol_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Drag_Protocol);
   type Property_Gdk_Drag_Protocol is new Gdk_Drag_Protocol_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_drag_context_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Actions
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Action;
   --  Determines the bitmask of actions proposed by the source if
   --  Gdk.Drag_Contexts.Get_Suggested_Action returns
   --  Gdk.Drag_Contexts.Action_Ask.
   --  Since: gtk+ 2.22

   function Get_Dest_Window
      (Self : not null access Drag_Context_Record) return Gdk.Gdk_Window;
   --  Returns the destination window for the DND operation.
   --  Since: gtk+ 3.0

   function Get_Device
      (Self : not null access Drag_Context_Record)
       return Gdk.Device.Gdk_Device;
   --  Returns the Gdk.Device.Gdk_Device associated to the drag context.

   procedure Set_Device
      (Self   : not null access Drag_Context_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class);
   --  Associates a Gdk.Device.Gdk_Device to Context, so all Drag and Drop
   --  events for Context are emitted as if they came from this device.
   --  "device": a Gdk.Device.Gdk_Device

   function Get_Drag_Window
      (Self : not null access Drag_Context_Record) return Gdk.Gdk_Window;
   --  Returns the window on which the drag icon should be rendered during the
   --  drag operation. Note that the window may not be available until the drag
   --  operation has begun. GDK will move the window in accordance with the
   --  ongoing drag operation. The window is owned by Context and will be
   --  destroyed when the drag operation is over.
   --  Since: gtk+ 3.20

   function Get_Protocol
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Protocol;
   --  Returns the drag protocol that is used by this context.
   --  Since: gtk+ 3.0

   function Get_Selected_Action
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Action;
   --  Determines the action chosen by the drag destination.
   --  Since: gtk+ 2.22

   function Get_Source_Window
      (Self : not null access Drag_Context_Record) return Gdk.Gdk_Window;
   --  Returns the Gdk.Gdk_Window where the DND operation started.
   --  Since: gtk+ 2.22

   function Get_Suggested_Action
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Action;
   --  Determines the suggested drag action of the context.
   --  Since: gtk+ 2.22

   function Manage_Dnd
      (Self       : not null access Drag_Context_Record;
       Ipc_Window : Gdk.Gdk_Window;
       Actions    : Gdk_Drag_Action) return Boolean;
   --  Requests the drag and drop operation to be managed by Context. When a
   --  drag and drop operation becomes managed, the
   --  Gdk.Drag_Contexts.Drag_Context will internally handle all input and
   --  source-side Gdk.Event.Gdk_Event_DND events as required by the windowing
   --  system.
   --  Once the drag and drop operation is managed, the drag context will emit
   --  the following signals: - The
   --  Gdk.Drag_Contexts.Drag_Context::action-changed signal whenever the final
   --  action to be performed by the drag and drop operation changes. - The
   --  Gdk.Drag_Contexts.Drag_Context::drop-performed signal after the user
   --  performs the drag and drop gesture (typically by releasing the mouse
   --  button). - The Gdk.Drag_Contexts.Drag_Context::dnd-finished signal after
   --  the drag and drop operation concludes (after all Gdk_Selection transfers
   --  happen). - The Gdk.Drag_Contexts.Drag_Context::cancel signal if the drag
   --  and drop operation is finished but doesn't happen over an accepting
   --  destination, or is cancelled through other means.
   --  Since: gtk+ 3.20
   --  "ipc_window": Window to use for IPC messaging/events
   --  "actions": the actions supported by the drag source

   procedure Set_Hotspot
      (Self  : not null access Drag_Context_Record;
       Hot_X : Glib.Gint;
       Hot_Y : Glib.Gint);
   --  Sets the position of the drag window that will be kept under the cursor
   --  hotspot. Initially, the hotspot is at the top left corner of the drag
   --  window.
   --  Since: gtk+ 3.20
   --  "hot_x": x coordinate of the drag window hotspot
   --  "hot_y": y coordinate of the drag window hotspot

   -------------
   -- Signals --
   -------------

   type Cb_Drag_Context_Gdk_Drag_Action_Void is not null access procedure
     (Self   : access Drag_Context_Record'Class;
      Action : Gdk_Drag_Action);

   type Cb_GObject_Gdk_Drag_Action_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Action : Gdk_Drag_Action);

   Signal_Action_Changed : constant Glib.Signal_Name := "action-changed";
   procedure On_Action_Changed
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_Drag_Context_Gdk_Drag_Action_Void;
       After : Boolean := False);
   procedure On_Action_Changed
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_GObject_Gdk_Drag_Action_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  A new action is being chosen for the drag and drop operation.
   --
   --  This signal will only be emitted if the Gdk.Drag_Contexts.Drag_Context
   --  manages the drag and drop operation. See Gdk.Drag_Contexts.Manage_Dnd
   --  for more information.

   type Cb_Drag_Context_Gdk_Drag_Cancel_Reason_Void is not null access procedure
     (Self   : access Drag_Context_Record'Class;
      Reason : Gdk_Drag_Cancel_Reason);

   type Cb_GObject_Gdk_Drag_Cancel_Reason_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Reason : Gdk_Drag_Cancel_Reason);

   Signal_Cancel : constant Glib.Signal_Name := "cancel";
   procedure On_Cancel
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_Drag_Context_Gdk_Drag_Cancel_Reason_Void;
       After : Boolean := False);
   procedure On_Cancel
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_GObject_Gdk_Drag_Cancel_Reason_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The drag and drop operation was cancelled.
   --
   --  This signal will only be emitted if the Gdk.Drag_Contexts.Drag_Context
   --  manages the drag and drop operation. See Gdk.Drag_Contexts.Manage_Dnd
   --  for more information.

   type Cb_Drag_Context_Void is not null access procedure (Self : access Drag_Context_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Dnd_Finished : constant Glib.Signal_Name := "dnd-finished";
   procedure On_Dnd_Finished
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_Drag_Context_Void;
       After : Boolean := False);
   procedure On_Dnd_Finished
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The drag and drop operation was finished, the drag destination finished
   --  reading all data. The drag source can now free all miscellaneous data.
   --
   --  This signal will only be emitted if the Gdk.Drag_Contexts.Drag_Context
   --  manages the drag and drop operation. See Gdk.Drag_Contexts.Manage_Dnd
   --  for more information.

   type Cb_Drag_Context_Gint_Void is not null access procedure
     (Self : access Drag_Context_Record'Class;
      Time : Glib.Gint);

   type Cb_GObject_Gint_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Time : Glib.Gint);

   Signal_Drop_Performed : constant Glib.Signal_Name := "drop-performed";
   procedure On_Drop_Performed
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_Drag_Context_Gint_Void;
       After : Boolean := False);
   procedure On_Drop_Performed
      (Self  : not null access Drag_Context_Record;
       Call  : Cb_GObject_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The drag and drop operation was performed on an accepting client.
   --
   --  This signal will only be emitted if the Gdk.Drag_Contexts.Drag_Context
   --  manages the drag and drop operation. See Gdk.Drag_Contexts.Manage_Dnd
   --  for more information.

end Gdk.Drag_Contexts;
