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

--  Represents the source of an ongoing DND operation.
--
--  A `GdkDrag` is created when a drag is started, and stays alive for
--  duration of the DND operation. After a drag has been started with
--  [funcGdk.Drag.begin], the caller gets informed about the status of the
--  ongoing drag operation with signals on the `GdkDrag` object.
--
--  GTK provides a higher level abstraction based on top of these functions,
--  and so they are not normally needed in GTK applications. See the "Drag and
--  Drop" section of the GTK documentation for more information.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Content_Formats;     use Gdk.Content_Formats;
with Gdk.Content_Provider;    use Gdk.Content_Provider;
with Gdk.Device;
with Gdk.Surface;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;

package Gdk.Drag is

   type Gdk_Drag_Record is new GObject_Record with null record;
   type Gdk_Drag is access all Gdk_Drag_Record'Class;

   type Drag_Action is mod 2 ** Integer'Size;
   pragma Convention (C, Drag_Action);
   --  Used in `GdkDrop` and `GdkDrag` to indicate the actions that the
   --  destination can and should do with the dropped data.

   Gdk_Action_None : constant Drag_Action := 0;
   Gdk_Action_Copy : constant Drag_Action := 1;
   Gdk_Action_Move : constant Drag_Action := 2;
   Gdk_Action_Link : constant Drag_Action := 4;
   Gdk_Action_Ask : constant Drag_Action := 8;

   type Drag_Cancel_Reason is (
      No_Target,
      User_Cancelled,
      Error);
   pragma Convention (C, Drag_Cancel_Reason);
   --  Used in `GdkDrag` to the reason of a cancelled DND operation.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Drag_Action_Properties is
      new Generic_Internal_Discrete_Property (Drag_Action);
   type Property_Drag_Action is new Drag_Action_Properties.Property;

   package Drag_Cancel_Reason_Properties is
      new Generic_Internal_Discrete_Property (Drag_Cancel_Reason);
   type Property_Drag_Cancel_Reason is new Drag_Cancel_Reason_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_drag_get_type");

   -------------
   -- Methods --
   -------------

   procedure Drop_Done
      (Self    : not null access Gdk_Drag_Record;
       Success : Boolean);
   --  Informs GDK that the drop ended.
   --  Passing False for Success may trigger a drag cancellation animation.
   --  This function is called by the drag source, and should be the last call
   --  before dropping the reference to the Drag.
   --  The `GdkDrag` will only take the first [methodGdk.Drag.drop_done] call
   --  as effective, if this function is called multiple times, all subsequent
   --  calls will be ignored.
   --  @param Success whether the drag was ultimatively successful

   function Get_Actions
      (Self : not null access Gdk_Drag_Record) return Drag_Action;
   --  Determines the bitmask of possible actions proposed by the source.
   --  @return the `GdkDragAction` flags

   function Get_Content
      (Self : not null access Gdk_Drag_Record)
       return Gdk.Content_Provider.Gdk_Content_Provider;
   --  Returns the `GdkContentProvider` associated to the `GdkDrag` object.
   --  @return The `GdkContentProvider` associated to Drag.

   function Get_Device
      (Self : not null access Gdk_Drag_Record) return Gdk.Gdk_Device;
   --  Returns the `GdkDevice` associated to the `GdkDrag` object.
   --  @return The `GdkDevice` associated to Drag.

   function Get_Display
      (Self : not null access Gdk_Drag_Record) return Gdk.Gdk_Display;
   --  Gets the `GdkDisplay` that the drag object was created for.
   --  @return a `GdkDisplay`

   function Get_Drag_Surface
      (Self : not null access Gdk_Drag_Record) return Gdk.Gdk_Surface;
   --  Returns the surface on which the drag icon should be rendered during
   --  the drag operation.
   --  Note that the surface may not be available until the drag operation has
   --  begun. GDK will move the surface in accordance with the ongoing drag
   --  operation. The surface is owned by Drag and will be destroyed when the
   --  drag operation is over.
   --  @return the drag surface

   function Get_Formats
      (Self : not null access Gdk_Drag_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats;
   --  Retrieves the formats supported by this `GdkDrag` object.
   --  @return a `GdkContentFormats`

   function Get_Selected_Action
      (Self : not null access Gdk_Drag_Record) return Drag_Action;
   --  Determines the action chosen by the drag destination.
   --  @return a `GdkDragAction` value

   function Get_Surface
      (Self : not null access Gdk_Drag_Record) return Gdk.Gdk_Surface;
   --  Returns the `GdkSurface` where the drag originates.
   --  @return The `GdkSurface` where the drag originates

   procedure Set_Hotspot
      (Self  : not null access Gdk_Drag_Record;
       Hot_X : Glib.Gint;
       Hot_Y : Glib.Gint);
   --  Sets the position of the drag surface that will be kept under the
   --  cursor hotspot.
   --  Initially, the hotspot is at the top left corner of the drag surface.
   --  @param Hot_X x coordinate of the drag surface hotspot
   --  @param Hot_Y y coordinate of the drag surface hotspot

   ---------------
   -- Functions --
   ---------------

   function Begin_Drag
      (Surface : not null access Gdk.Surface.Gdk_Surface_Record'Class;
       Device  : not null access Gdk.Device.Gdk_Device_Record'Class;
       Content : not null access Gdk.Content_Provider.Gdk_Content_Provider_Record'Class;
       Actions : Drag_Action;
       Dx      : Gdouble;
       Dy      : Gdouble) return Gdk_Drag;
   --  Starts a drag and creates a new drag context for it.
   --  This function is called by the drag source. After this call, you
   --  probably want to set up the drag icon using the surface returned by
   --  [methodGdk.Drag.get_drag_surface].
   --  This function returns a reference to the [classGdk.Drag] object, but
   --  GTK keeps its own reference as well, as long as the DND operation is
   --  going on.
   --  Note: if Actions include Gdk.Drag.Gdk_Action_Move, you need to listen
   --  for the [signalGdk.Drag::dnd-finished] signal and delete the data at the
   --  source if [methodGdk.Drag.get_selected_action] returns
   --  Gdk.Drag.Gdk_Action_Move.
   --  @param Surface the source surface for this drag
   --  @param Device the device that controls this drag
   --  @param Content the offered content
   --  @param Actions the actions supported by this drag
   --  @param Dx the x offset to Device's position where the drag nominally
   --  started
   --  @param Dy the y offset to Device's position where the drag nominally
   --  started
   --  @return a newly created `GdkDrag`

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Actions_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Drag_Action
   --  The possible actions of this drag.

   Content_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Content_Provider
   --  The `GdkContentProvider`.

   Device_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Device
   --  The `GdkDevice` that is performing the drag.

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  The `GdkDisplay` that the drag belongs to.

   Formats_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Content_Formats
   --  The possible formats that the drag can provide its data in.

   Selected_Action_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Drag_Action
   --  The currently selected action of the drag.

   Surface_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Surface
   --  The surface where the drag originates.

   -------------
   -- Signals --
   -------------

   type Cb_Gdk_Drag_Drag_Cancel_Reason_Void is not null access procedure
     (Self   : access Gdk_Drag_Record'Class;
      Reason : Drag_Cancel_Reason);

   type Cb_GObject_Drag_Cancel_Reason_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Reason : Drag_Cancel_Reason);

   Signal_Cancel : constant Glib.Signal_Name := "cancel";
   procedure On_Cancel
      (Self  : not null access Gdk_Drag_Record;
       Call  : Cb_Gdk_Drag_Drag_Cancel_Reason_Void;
       After : Boolean := False);
   procedure On_Cancel
      (Self  : not null access Gdk_Drag_Record;
       Call  : Cb_GObject_Drag_Cancel_Reason_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the drag operation is cancelled.

   type Cb_Gdk_Drag_Void is not null access procedure (Self : access Gdk_Drag_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Dnd_Finished : constant Glib.Signal_Name := "dnd-finished";
   procedure On_Dnd_Finished
      (Self  : not null access Gdk_Drag_Record;
       Call  : Cb_Gdk_Drag_Void;
       After : Boolean := False);
   procedure On_Dnd_Finished
      (Self  : not null access Gdk_Drag_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the destination side has finished reading all data.
   --
   --  The drag object can now free all miscellaneous data.

   Signal_Drop_Performed : constant Glib.Signal_Name := "drop-performed";
   procedure On_Drop_Performed
      (Self  : not null access Gdk_Drag_Record;
       Call  : Cb_Gdk_Drag_Void;
       After : Boolean := False);
   procedure On_Drop_Performed
      (Self  : not null access Gdk_Drag_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the drop operation is performed on an accepting client.

private
   Surface_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("surface");
   Selected_Action_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("selected-action");
   Formats_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("formats");
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
   Device_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("device");
   Content_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("content");
   Actions_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("actions");
end Gdk.Drag;
