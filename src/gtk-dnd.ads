-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
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
--  Like all modern GUI toolkits, GtkAda has a full support for drag-and-drop
--  operations. This is a mechanism for interactively transferring data between
--  two widgets, either in the same application or in two different
--  applications. The user clicks on a widget (called a "drag source"), and,
--  while keeping the mouse button pressed, moves it to another widget, where
--  the mouse button is released (this other widget is called a "drop site").
--  As a result, and if both widgets can handle the same type of data, some
--  data is either copied or moved to this new widget.
--
--  This is a very intuitive way, in some cases, to enhance the usability of
--  your application, although you should carefully consider whether this
--  should be used or not.
--
--  GtkAda supports several drag-and-drop protocols, so as to be able to
--  communicate with the maximum number of applications. These protocols are
--  Xdnd and Motif.
--
--  Note that drag-and-drop is currently not supported under Windows systems.
--
--  Below is a summary of what is needed to add drag-and-drop capabilities to
--  your application. We highly recommend that you look at, and understand,
--  the example in testgtk (create_dnd.adb), before using these features in
--  your own application.
--
--  See also the package Gtk.Selection, that contains some lower subprograms
--  and data types that are used when implementing drag-and-drop.
--
--
--  - Defining a widget as a possible drag source
--
--  You need to call Source_Set, specifying which mouse buttons can activate
--  the drag, which types of data will be given, and which kind of action
--  will be performed.
--  You then need to connect to the signal "drag_data_get", that will be
--  emitted when the user has dropped the item and GtkAda needs to find the
--  data. You must call Selection_Data_Set in the handler to set the actual
--  data.
--  You can also connect the widget to "drag_data_delete", which will be
--  called whenever the data set for the selection is no longer required and
--  should be deleted. The signal will be emitted only if the drop site
--  requests it, or if the selected action for the drag-and-drop operation
--  was Action_Move. It will not be called automatically for an Action_Copy.
--  Note that the callback might be called several times, if for instance this
--  was an Action_Move, and the drop site requires explicitly to delete the
--  data in its call to Finish.
--
--
--  - Defining a widget as a possible drop site
--
--  You need to call Dest_Set, specifying which types of Data are accepted
--  by the widget, which actions are recognized, and whether you accept drops
--  from external applications.
--  You also need to connect to "drag_data_received", that will be emitted
--  when the user has dropped some data on the widget. The handler should
--  call Finish, to warn the source widget that the drag and drop operation
--  is finished, and whether it was successful or not.
--  </description>
--  <c_version>1.3.6</c_version>

with Glib; use Glib;
with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Event;
with Gdk.Pixmap;
with Gdk.Types;
with Gdk.Window;
with Gtk.Widget;
with Gtk.Selection;  use Gtk.Selection;
with Gtk.Enums;

package Gtk.Dnd is

   -----------------
   -- Drag_Action --
   -----------------

   type Drag_Action is new Integer;
   --  Possible actions for a drop onto a widget, during a drag-and-drop.
   --  The drag widgets (ie the ones from which the user can start a
   --  drag-and-drop operation) should set a mask, indicating which actions
   --  it wants to do. The first action in the list below has the highest
   --  priority, the last one the lowest. The actual action chosen for the
   --  drag-and-drop will be the highest-priority one that is also accepted
   --  by the drop site.
   --
   --  Note that in the case where the drag source supports multiple actions,
   --  the user can select the one he wants. As explained above, the default
   --  one is the highest priority one. But if the user pressed Shift at the
   --  same time, Action_Move will be used if present. Ctrl-Shift selects
   --  an Action_Link, and Ctrl selects an Action_Copy.

   Action_Any : constant Drag_Action;
   --  Any of the default action is accepted.

   Action_Default : constant Drag_Action;
   --  ???

   Action_Copy    : constant Drag_Action;
   --  Copy the data to the drop site.

   Action_Move    : constant Drag_Action;
   --  Copy the data to the drop site, and delete it from the drag source.
   --  The delete command is invoked automatically by GtkAda.

   Action_Link    : constant Drag_Action;
   --  Allow the drop site to access the data, without copying it.

   Action_Private : constant Drag_Action;
   --  Any action you want to implement. No automatic behavior is provided
   --  by GtkAda.

   Action_Ask     : constant Drag_Action;
   --  ???

   -------------------
   -- Drag_Protocol --
   -------------------

   type Drag_Protocol is
     (Drag_Proto_Motif,
      Drag_Proto_Xdnd,
      Drag_Proto_Rootwin,
      --  A root window with nobody claiming the drag
      Drag_Proto_None
      --  Not a valid drag window
     );
   --  The various dnd protocols recognized by a window.
   --  Note that not every window recognizes every protocol, and you should
   --  be careful as to which one you use. The function Gdk.Drag.Get_Protocol
   --  returns which one is recognized by a window.

   ------------------
   -- Drag_Context --
   ------------------

   type Drag_Context is new Gdk.C_Proxy;
   --  Structure that holds information about a drag in progress.
   --  This is used on both source and destination sides.

   function Get_Actions (Context : Drag_Context) return Drag_Action;
   --  Return the possible actions associated with the context.
   --  This is the list of actions defined by the source of the drag-and-drop
   --  operation, in Source_Set.
   --  (for instance, if Source_Set was used with Action_Copy + Action_Move,
   --  the result will be exactly this sum, whatever was used for Dest_Set).

   function Get_Suggested_Action (Context : Drag_Context) return Drag_Action;
   --  Return the suggested action for that context.
   --  This is the highest priority action that was set by the source of the
   --  drag-and-drop, ie the one it would rather use. The action that is
   --  actually used is the one returned by Get_Action, and depends on the
   --  mask set by the target.

   function Get_Action (Context : Drag_Context) return Drag_Action;
   --  Return the action selected for the drag-and-drop operation.
   --  This is the highest priority action common between the drag site and the
   --  drop widget (for instance, if Source_Set was used with Action_Copy +
   --  Action_Move and Dest_Set was used with only Action_Move, this will
   --  be Action_Move).

   function Get_Targets
     (Context : Drag_Context) return Gtk.Enums.Guint_List.Glist;
   --  List of all the targets common to the drag source and the drop site.
   --  The Guint in the list are the ones given in the Info field in the
   --  Target_Entry structure below.

   -------------------
   -- Dest_Defaults --
   -------------------

   type Dest_Defaults is new Integer;
   --  Specify the various types of action that will be taken on behalf of the
   --  user for a drag destination site.

   Dest_No_Default        : constant Dest_Defaults;
   --  No default behavior is provided for the drop site, this is your own
   --  responsabily. You need to handler the "drag_drop" signal yourself.

   Dest_Default_Motion    : constant Dest_Defaults;
   --  If set for a widget, GtkAda, during a drag over this widget will check
   --  if the drag matches this widget's list of possible targets and
   --  actions. gdk_drag_status is called as appropriate.

   Dest_Default_Highlight : constant Dest_Defaults;
   --  If set for a widget, GtkAda will draw a highlight on this widget as
   --  long as a drag is over this widget and the wiget drag format and action
   --  is acceptable.

   Dest_Default_Drop      : constant Dest_Defaults;
   --  If set for a widget, when a drop occurs, GtkAda+ will check if the drag
   --  matches this widget's list of possible targets and actions. If so,
   --  GtkAda will call Get_Data on behalf of the widget. Whether or not
   --  the drop is succesful, GtkAda will call Drag_Finish. If the
   --  action was a move, then if the drag was succesful, then True will be
   --  passed for the delete parameter to Finish.

   Dest_Default_All       : constant Dest_Defaults;
   --  If set, specifies that all default actions should be taken.

   ----------------------------
   -- To be moved to Gdk.Dnd --
   ----------------------------

   procedure Drag_Status
     (Context : Drag_Context;
      Action  : Drag_Action;
      Time    : Guint32 := 0);
   --  Set the action for the context and warns the drag source about the
   --  change.

--     procedure Drop_Reply (Context : Drag_Context;
--                           Ok      : Boolean;
--                           Time    : Guint32);
--     procedure Drop_Finish (Contex  : Drag_Context;
--                            Success : Boolean;
--                            Time    : Guint32);
   function Drag_Get_Selection
     (Context : Drag_Context) return Gdk.Types.Gdk_Atom;

--     function Get_Protocol (Xid  : Guint32;
--                        Protocol : access Drag_Protocol)
--                       return Guint32;

   pragma Import (C, Drag_Status, "gdk_drag_status");
   pragma Import (C, Drag_Get_Selection, "gdk_drag_get_selection");

   -------------------------------------------
   --  Setting up a widget as a destination --
   -------------------------------------------

   procedure Dest_Set
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags   : Dest_Defaults := Dest_No_Default;
      Targets : Target_Entry_Array := Any_Target_Entry;
      Actions : Drag_Action := Action_Any);
   --  Set a widget as a potential drop destination.
   --
   --  Flags specifies what action GtkAda should take on behalf of a widget for
   --  drops onto that widget. The Targets and Actions fields are used only
   --  if Dest_Default_Motion or Dest_Default_Drop are given.
   --
   --  Targets indicates the drop types that Widget accepts. If no item from
   --  Targets matches the list of targets emitted by the source (as set in
   --  Source_Set), then the drop will be considered illegal and refused.
   --
   --  Actions is a bitmask of possible actions for a drop onto Widget. At
   --  least of the actions must be in common with what was set for the source
   --  in Source_Set, or the drop is considered illegal.

   --  if Flags = Dest_No_Default, no default behavior is provided, and
   --  Targets and Actions are simply ignored.

   procedure Dest_Set_Proxy
     (Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
      Proxy_Window    : Gdk.Window.Gdk_Window;
      Protocol        : Drag_Protocol;
      Use_Coordinates : Boolean);
   --  Set this widget as a proxy for drops to another window.
   --  All drag events on Widget will be forwarded to Proxy_Window.
   --  Protocol is the drag protocol that Proxy_Window accepts. You can use
   --  Gdk.Drag.Get_Protocol to determine this.
   --  If Use_Coordinates is True, send the same coordinates to the destination
   --  because it is an embedded subwindow.

   procedure Dest_Unset
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Clear information about a drop destination set with Dest_Set. The
   --  widget will no longer receive notification of drags.

   -------------------------------------
   -- Setting up a widget as a source --
   -------------------------------------

   procedure Source_Set
     (Widget            : access Gtk.Widget.Gtk_Widget_Record'Class;
      Start_Button_Mask : Gdk.Types.Gdk_Modifier_Type;
      Targets           : Target_Entry_Array;
      Actions           : Drag_Action);
   --  Set up a widget so that GtkAda will start a drag operation when the
   --  user clicks and drags on the widget. The widget must have a window.
   --
   --  Targets is the list of targets that the drag can provide. The first
   --  possible target accepted by the drop site will be used. For instance,
   --  it Targets contains "text/plain" and "text/url", and the drop site only
   --  accepts "text/url", this will be the one used. However, if the drop site
   --  also accepts "text/plain", the latter will be prefered.
   --
   --  Widget needs to be able to convert the data to any of the types in
   --  Target, as any of them might be requested by the drop site.
   --
   --  Actions is a list of possible actions for drags from Widget. At least
   --  one of the actions must be in common with the drop site for the
   --  drag-and-drop operation to succeed.

   procedure Source_Unset (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Undo the effects of Source_Set.

   ---------------------------------
   -- The drag-and-drop operation --
   ---------------------------------

   procedure Finish
     (Context : Drag_Context;
      Success : Boolean;
      Del     : Boolean;
      Time    : Guint32 := 0);
   --  Inform the drag source that the drop is finished, and that the data of
   --  the drag will no longer be required.
   --  Success should indicate whether the drop was successful.
   --  Del should be set to True if the source should delete the original
   --  data (this should be True for a move).

   procedure Get_Data
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Context : Drag_Context;
      Target  : Gdk.Types.Gdk_Atom;
      Time    : Guint32 := 0);
   --  Get the data associated with a drag. When the data is received or the
   --  retrieval fails, GtkAda will emit a "drag_data_received"
   --  signal. Failure of the retrieval is indicated by the length field of
   --  the selection_data signal parameter being negative. However, when
   --  Get_Data is called implicitely because the Drag_Default_Drop was set,
   --  then the widget will not receive notification of failed drops.
   --
   --  Target is the target (form of the data) to retrieve.
   --  Time is a timestamp to retrive the data, and will be given to
   --  "drag_data_motion" or "drag_data_drop" signals.

   function Get_Source_Widget
     (Context : Drag_Context) return Gtk.Widget.Gtk_Widget;
   --  Determine the source widget for a drag.
   --  If the drag is occuring within a single application, this function
   --  returns the source widget. Otherwise, it returns null.

   procedure Highlight (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Draw a highlight around a widget.

   procedure Unhighlight (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Remove a highlight set by Highlight.

   function Drag_Begin
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Targets : Target_List;
      Actions : Drag_Action;
      Button  : Gint;
      Event   : Gdk.Event.Gdk_Event) return Drag_Context;
   --  Initiate a drag on the source side. The function only needs to be used
   --  when the application is starting drags itself, and is not needed when
   --  Source_Set is used.
   --  Targets is the list of targets (data formats) in which the source can
   --  provide the data.
   --  Actions is a bitmask of the allowed drag actions for this drag.
   --  Button is the button the user clicked to start the drag.
   --  Event is the event that triggered the start of the drag.

   -----------
   -- Icons --
   -----------

   procedure Set_Icon_Widget
     (Context : Drag_Context;
      Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Hot_X   : Gint;
      Hot_Y   : Gint);
   --  Change the icon for a drag.
   --  GtkAda will not destroy the icon, so if you don't want it to persist,
   --  you should connect to the "drag_end" signal and destroy it yourself.
   --  Context is the reference to the current drag operation.
   --  Widget is the toplevel window to use as an icon. (Hot_X, Hot_Y) is the
   --  coordinates of the hot point (that will be just under the mouse) within
   --  Widget.

   procedure Set_Icon_Pixmap
     (Context  : Drag_Context;
      Colormap : Gdk.Color.Gdk_Colormap;
      Pixmap   : Gdk.Pixmap.Gdk_Pixmap;
      Mask     : Gdk.Bitmap.Gdk_Bitmap;
      Hot_X    : Gint;
      Hot_Y    : Gint);
   --  Sets a given pixmap as the icon for a given drag. GtkAda retains a
   --  reference count for the arguments, and will release them when they are
   --  no longer needed.
   --  (Hot_X, Hot_Y) is the coordinates of the hotspot within Pixmap.

   procedure Set_Icon_Default (Context : Drag_Context);
   --  Set the icon for a particular drag to the default icon.
   --  This must be called with a context for the source side of a drag.

   procedure Set_Default_Icon
     (Colormap : Gdk.Color.Gdk_Colormap;
      Pixmap   : Gdk.Pixmap.Gdk_Pixmap;
      Mask     : Gdk.Bitmap.Gdk_Bitmap;
      Hot_X    : Gint;
      Hot_Y    : Gint);
   --  Change the default drag icon. GtkAda retains a reference count for the
   --  arguments, and will release them when they are no longer needed.

   procedure Source_Set_Icon
     (Widget   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Colormap : Gdk.Color.Gdk_Colormap;
      Pixmap   : Gdk.Pixmap.Gdk_Pixmap;
      Mask     : Gdk.Bitmap.Gdk_Bitmap);
   --  Set the icon that will be used for drags from a particular widget.
   --  GtkAda retains a reference count for the arguments, and will release
   --  them when they are no longer needed.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for the class
   --  Gtk.Widget.Gtk_Widget to support drag-and-drop.
   --  Please note that no default marshaller is provided in GtkAda for these
   --  handlers, and that you will have to use the general form of callbacks
   --  instead, getting the value of the parameters directly from the
   --  Gtk_Args structure.
   --
   --  - "drag_begin"   (source side)
   --    procedure Handler (Widget  : access Gtk_Widget_Record'Class;
   --                       Context : Drag_Context);
   --
   --    A new drag-and-drop operation has just been started from Widget. This
   --    callback can be used for instance to modify the visual aspect of the
   --    widget, so as to give a visual clue as to what widget is the source.
   --
   --  - "drag_end"     (source side)
   --    procedure Handler (Widget  : access Gtk_Widget_Record'Class;
   --                       Context : Drag_Context);
   --
   --    The drag-and-drop operation that was started from the widget has been
   --    completed, and the standard set of the widget can be restored.
   --
   --  - "drag_data_get"  (source side)
   --    procedure Handler (Widget  : access Gtk_Widget_Record'Class;
   --                       Context : Drag_Context;
   --                       Data    : Selection_Data;
   --                       Info    : Guint;
   --                       Time    : Guint);
   --
   --    This should be connected to every drag source.
   --    This is used to request the actual data to be transfered to the drop
   --    site once the drop has been done.
   --    Info is the type of the expected Data, and is in fact the third
   --    field of the Target_Entry record, whose value you have define
   --    yourself.
   --    Data should be modified to include a pointer or a copy of the data,
   --    through Selection_Data_Set.
   --
   --  - "drag_data_delete"  (source side)
   --    procedure Handler (Widget  : access Gtk_Widget_Record'Class;
   --                       Context : Drag_Context);
   --
   --    This handler is called whenever the drop site of a drag-and-drop
   --    operation has decided that the data should be deleted, or
   --    automaticallyif the selected action was Action_Move.
   --    Widget is the drag source.
   --
   --  - "drag_leave"  (target side)
   --    procedure Handler (Widget  : access Gtk_Widget_Record'Class;
   --                       Context : Drag_Context;
   --                       Time    : Guint);

   --    Signal emitted whenever a drag-and-drop operation is being performed,
   --    and the mouse has just left the area covered by a widget on the
   --    screen. This can be used to restore the default visual aspect of the
   --    widget. This is also emitted when the drop has been performed on the
   --    widget.
   --
   --  - "drag_motion"  (target side)
   --    function Handler (Widget  : access Gtk_Widget_Record'Class;
   --                      Context : Drag_Context;
   --                      X       : Gint;
   --                      Y       : Gint;
   --                      Time    : Guint)
   --                     return Boolean;
   --
   --    This is called every time the user is doing a dnd operation, and
   --    the mouse is currently over Widget (but not released yet).
   --    This can be used to change the visual aspect of Widget to provide
   --    visual clues to the user. The "opposite" signal is drag_leave.
   --
   --    The return value is ignored if Dest_Default_Motion was set when
   --    Source_Set was called. This handler should return True if Widget
   --    acknowledges that it is a possible drop site for the particular
   --    targets provided by the drag source.
   --
   --  - "drag_drop"  (target side)
   --    function Handler (Widget  : access Gtk_Widget_Record'Class;
   --                      Context : Drag_Context;
   --                      X       : Gint;
   --                      Y       : Gint;
   --                      Time    : Guint)
   --                     return Boolean;
   --
   --    This is called whenever a drop is about to be performed on the widget.
   --    Note that this is called even if no common target type has been found
   --    between the drag source and the drop site. Thus, you will need to
   --    analyze the result of Get_Targets (Context) to find the possible
   --    targets.
   --    The data is sent separately through the "drag_data_received" signal,
   --    and might not even be available when "drag_drop" is emitted.
   --    This signal is mostly used if you have chosen not to use any of the
   --    default behavior when calling Dest_Set. Otherwise, everything is
   --    already handled directly by GtkAda.
   --
   --    This handler should return True if Widget acknowledges that it is a
   --    possible drop site for the particular targets provided by the drag
   --    source.
   --
   --  - "drag_data_received"  (target_side)
   --    procedure Handler (Widget  : access Gtk_Widget_Record'Class;
   --                       Context : Drag_Context;
   --                       X       : Gint;
   --                       Y       : Gint;
   --                       Data    : Selection_Data;
   --                       Info    : Guint;
   --                       Time    : Guint);
   --
   --    This signal should be connected to every drop site.
   --    The handler is called every time some new data has been dropped onto
   --    Widget. (X, Y) are the mouse coordinates, relative to the widget's
   --    window, where the data was dropped. Info is the type of the data,
   --    has set in the third field of the Target_Entry record, and Data
   --    contains a pointer to the actual data.
   --
   --  </signals>

private
   Dest_No_Default        : constant Dest_Defaults := 0;
   Dest_Default_Motion    : constant Dest_Defaults := 2 ** 0;
   Dest_Default_Highlight : constant Dest_Defaults := 2 ** 1;
   Dest_Default_Drop      : constant Dest_Defaults := 2 ** 2;
   Dest_Default_All       : constant Dest_Defaults := 7;

   Action_Default : constant Drag_Action := 1;
   Action_Copy    : constant Drag_Action := 2;
   Action_Move    : constant Drag_Action := 4;
   Action_Link    : constant Drag_Action := 8;
   Action_Private : constant Drag_Action := 16;
   Action_Ask     : constant Drag_Action := 32;
   Action_Any     : constant Drag_Action := 255;

   pragma Import (C, Get_Actions, "ada_gtk_dnd_context_get_actions");
   pragma Import
     (C, Get_Suggested_Action,
     "ada_gtk_dnd_context_get_suggested_action");
   pragma Import (C, Get_Action, "ada_gtk_dnd_context_get_action");
   pragma Import (C, Set_Icon_Pixmap, "gtk_drag_set_icon_pixmap");
   pragma Import (C, Set_Icon_Default, "gtk_drag_set_icon_default");
   pragma Import (C, Set_Default_Icon, "gtk_drag_set_default_icon");
end Gtk.Dnd;

--  missing:
--  GdkAtom        Dest_Find_Target
--    (GtkWidget      *widget,
--     GdkDragContext *context,
--     GtkTargetList  *target_list);

--  GtkTargetList* Dest_Get_Target_List
--    (GtkWidget      *widget);

--  procedure Dest_Set_Target_List
--    (GtkWidget      *widget,
--     GtkTargetList  *target_list);

--  gboolean Check_Threshold
--    (GtkWidget *widget,
--     gint       start_x,
--     gint       start_y,
--     gint       current_x,
--     gint       current_y);
