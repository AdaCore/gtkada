-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2001-2002 ACT-Europe                --
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

--  TODO:
--  - handles multiple views of the MDI (through several top-level windows)
--  - Icons should be put at the bottom, and automatically moved when the
--    MDI window is resized.
--  - Icons should be placed correctly when there are also docked items
--  - Add support for groups (children are associated with groups, and groups
--    can have special colors, can be minimized,...). Groups could be
--    implemented as special MDI_Children ?
--  - Manipulation of the title bar for children (adding buttons, adding
--    pixmaps,...)
--  - define new signals ("float_child", ...)
--  - Automatically add a new menu bar when a child is floated (settable
--    on a per-child basis).
--  - contextual menu in the title bar of children to dock them, float them,...

with Glib;             use Glib;
with Glib.Convert;     use Glib.Convert;
with Glib.Object;      use Glib.Object;
with Pango.Font;       use Pango.Font;
with Gdk;              use Gdk;
with Gdk.Color;        use Gdk.Color;
with Gdk.Cursor;       use Gdk.Cursor;
with Gdk.Dnd;          use Gdk.Dnd;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.Event;        use Gdk.Event;
with Gdk.GC;           use Gdk.GC;
with Gdk.Main;         use Gdk.Main;
with Gdk.Pixmap;
with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gdk.Types;        use Gdk.Types;
with Gdk.Types.Keysyms;
with Gdk.Window;       use Gdk.Window;
with Gdk.Window_Attr;  use Gdk.Window_Attr;
with Gtk;              use Gtk;
with Gtk.Accel_Group;  use Gtk.Accel_Group;
with Gtk.Accel_Label;  use Gtk.Accel_Label;
with Gtk.Arguments;    use Gtk.Arguments;
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Container;    use Gtk.Container;
with Gtk.Dialog;       use Gtk.Dialog;
with Gtk.Dnd;          use Gtk.Dnd;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Event_Box;    use Gtk.Event_Box;
with Gtk.Fixed;        use Gtk.Fixed;
with Gtk.Frame;        use Gtk.Frame;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Label;        use Gtk.Label;
with Pango.Layout;     use Pango.Layout;
with Gtk.Main;         use Gtk.Main;
pragma Elaborate_All (Gtk.Main);

with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtk.Notebook;     use Gtk.Notebook;
with Gtk.Object;
with Gtk.Pixmap;       use Gtk.Pixmap;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Selection;    use Gtk.Selection;
with Gtk.Style;        use Gtk.Style;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Tags;         use Ada.Tags;
with System;           use System;

package body Gtkada.MDI is

   use Glib.Xml_Int;

   Default_Title_Bar_Focus_Color : constant String := "#000088";
   --  Default color to use for the title bar of the child that has
   --  the focus

   Default_Title_Bar_Color : constant String := "#AAAAAA";
   --  Default color to use for the title bar of children that do not
   --  have the focus.

   Default_MDI_Background_Color : constant String := "#666666";
   --  Default background color to use for the MDI window

   Default_Title_Font : constant String := "Sans 8";
   --  Default title font for the children

   Border_Thickness : constant Gint := 4;
   --  Thickness of the separators in the MDI

   Drop_Area_Thickness : constant Gint := 4;
   --  Thickness of the Dnd drop areas on each side of the MDI.

   Icons_Width : constant Gint := 100;
   --  Width to use for icons

   Handle_Size : constant Gint := 8;
   --  The default width or height of the handles.

   Min_Width  : constant Gint := 40;
   --  Minimal size for all windows

   Threshold : constant Gint := 40;
   --  Threshold used to reset coordinates when putting items in the MDI.

   Draw_Handles_Shadow : constant Boolean := False;
   --  Whether a shadow should be drawn on each handle

   Corner_Size : constant Gint := Border_Thickness * 2;
   --  Extra tolerance when the user selects a corner for resizing (if the
   --  pointer is within Corner_Size in both coordinates, then we are clicking
   --  on the corner)

   MDI_Class_Record        : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;
   Child_Class_Record      : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;
   MDI_Layout_Class_Record : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;

   MDI_Signals : constant chars_ptr_array :=
     (1 => New_String ("child_selected"));

   Child_Signals : constant chars_ptr_array :=
     (1 => New_String ("float_child"),
      2 => New_String ("unfloat_child"),
      3 => New_String ("selected"));

   Close_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("13 11 2 1"),
      New_String (".     c None"),
      New_String ("+     c #000000"),
      New_String ("............."),
      New_String ("............."),
      New_String ("...++....++.."),
      New_String ("....++..++..."),
      New_String (".....++++...."),
      New_String ("......++....."),
      New_String (".....++++...."),
      New_String ("....++..++..."),
      New_String ("...++....++.."),
      New_String ("............."),
      New_String ("............."));

   Iconify_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("13 11 2 1"),
      New_String (". c #000000"),
      New_String ("# c None"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("##.......####"),
      New_String ("##.......####"),
      New_String ("#############"),
      New_String ("#############"));

   Maximize_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("13 11 2 1"),
      New_String (".      c None"),
      New_String ("+      c #000000"),
      New_String ("............."),
      New_String ("....+++++++.."),
      New_String ("....+++++++.."),
      New_String ("....+.....+.."),
      New_String ("..+++++++.+.."),
      New_String ("..+++++++.+.."),
      New_String ("..+.....+++.."),
      New_String ("..+.....+...."),
      New_String ("..+.....+...."),
      New_String ("..+++++++...."),
      New_String ("............."));

   Widget_Target_Dnd      : constant Guint := 0;
   Root_Window_Target_Dnd : constant Guint := 1;
   Widget_Format          : constant Gint  := 111;
   --  Internal values for drag-and-drop support. Values are random, they just
   --  need to be different from one another.

   Source_Target_Table : constant Target_Entry_Array :=
     ((New_String ("gtkada/widget"), Target_No_Constraint, Widget_Target_Dnd),
      (New_String ("application/x-rootwin-drop"), Target_No_Constraint,
       Root_Window_Target_Dnd));
   Dest_Target_Table : constant Target_Entry_Array :=
     (1 => (New_String ("gtkada/widget"), Target_Same_App, Widget_Target_Dnd));
   --  The various mime types support by the drag-and-drop in the MDI.

   use Widget_List;

   type Selection_Dialog_Record is new Gtk_Window_Record with record
      Current_Child : Widget_List.Glist;
      Label         : Gtk_Label;
      Ent           : Gtk_Entry;
      Length        : Natural := 0;
      Modifier      : Gdk_Modifier_Type;
      Next_Key      : Gdk_Key_Type;
      Prev_Key      : Gdk_Key_Type;
      Icon          : Gtk.Pixmap.Gtk_Pixmap;
   end record;
   type Selection_Dialog_Access is access all Selection_Dialog_Record'Class;

   type Children_Array is array (Natural range <>) of Widget_List.Glist;

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   function Button_Pressed
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user has pressed the mouse button in the canvas.
   --  Test whether an item was selected.

   function Button_Release
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user has released the mouse button.
   --  If an item was selected, refresh the canvas.

   function Button_Motion
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user moves the mouse while a button is pressed.
   --  If an item was selected, the item is moved.

   function Button_Pressed_MDI
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   function Button_Release_MDI
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   function Button_Motion_MDI
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user has pressed the mouse while in the MDI, in
   --  particular in one of the handles

   procedure Resize_Docks_From_Mouse (MDI : access MDI_Window_Record'Class);
   --  Compute the new size of docks from the current pointer location,
   --  assuming we are in the middle of resizing docks interactively. This
   --  immediately updates the docks.

   procedure Internal_Close_Child
     (Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Internal version of Close, for a MDI_Child

   function Leave_Child
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  The pointer has left the mouse.

   procedure Reposition_Handles (M : access MDI_Window_Record'Class);
   --  Recompute the position of the four handles on each side of the MDI.

   procedure Create_Notebook
     (MDI : access MDI_Window_Record'Class; Side : Dock_Side);
   --  Create the notebook that goes to one of the sides of MDI, or to the
   --  middle. If this notebook already exists, its tabs are shown, since new
   --  children will be added to it.

   function Side
     (Child : access MDI_Child_Record'Class; X, Y : Gint)
      return Gdk_Cursor_Type;
   --  Return the cursor to use depending on the coordinates (X, Y) inside
   --  child.

   --  procedure Layout_Child
   --    (Child  : access MDI_Child_Record'Class;
   --     Region : Gdk.Region.Gdk_Region := null);
   --  Compute the coordinates for Child.
   --  If Region is null, loop through the list of all children, and try
   --  to position the child in an area where it doesn't overlap any child.
   --  If Region is not null, we use this to check that the child doesn't
   --  overlap any widget.

   function Delete_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Forward a delete_event from the toplevel window to the child.

   procedure Destroy_Child (Child : access Gtk_Widget_Record'Class);
   procedure Destroy_Initial_Child (Child : access Gtk_Widget_Record'Class);
   --  Called when either the child itself, or the widget we initially put
   --  in it, are destroyed. Remove the child from the MDI properly.

   procedure Destroy_MDI (MDI : access Gtk_Widget_Record'Class);
   --  Called when the MDI is destroyed.

   procedure Menu_Entry_Destroyed (Child : access Gtk_Widget_Record'Class);
   --  Called when the Menu_Item associated with a Child is destroyed.

   procedure Menu_Destroyed (MDI : access Gtk_Widget_Record'Class);
   --  Called when the Menu associated with a MDI is destroyed.

   procedure Compute_Docks_Size (MDI : access MDI_Window_Record'Class);
   --  Recompute the size of all the docks.

   procedure Size_Allocate_MDI_Layout
     (Layout : System.Address; Alloc : Gtk_Allocation);
   pragma Convention (C, Size_Allocate_MDI_Layout);
   --  Handles size allocations for the layout contained in the MDI.

   procedure Size_Allocate_MDI
     (MDI : System.Address; MDI_Alloc : Gtk_Allocation);
   pragma Convention (C, Size_Allocate_MDI);
   --  MDI was resized, need to resize the docks as well.

   procedure Iconify_Child (Child : access Gtk_Widget_Record'Class);
   --  Iconify a child (this act as toggles, for the title bar of all
   --  children).

   procedure Docked_Switch_Page
     (Docked_Child : access Gtk_Widget_Record'Class;
      Args : Gtk_Args);
   --  Called when the current page in Docked_Child has changed.
   --  This is used to refresh the notebook so that is reflects the selected
   --  widget.

   procedure Compute_Size
     (MDI                 : access MDI_Window_Record'Class;
      Side                : Dock_Side;
      Alloc               : out Gtk_Allocation;
      Is_Handle           : Boolean);
   --  Compute the size and position for the docks or the associated handles.
   --
   --  If Side is None, return the allocation to use for the workspace area of
   --  the MDI (which is either the layout or the middle notebook). Is_Handle
   --  is irrelevant in that case.

   procedure Draw_Child
     (Child : access MDI_Child_Record'Class; Area : Gdk_Rectangle);
   function Draw_Child
     (Child : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean;
   --  Draw the child (and the title bar)

   procedure Realize_MDI (MDI : access Gtk_Widget_Record'Class);
   procedure Realize_MDI_Layout (MDI : access Gtk_Widget_Record'Class);
   procedure Set_Dnd_Source
     (Widget : access Gtk_Widget_Record'Class;
      Child  : access Gtk_Widget_Record'Class);
   procedure Set_Dnd_Target (Widget : access Gtk_Widget_Record'Class);
   --  Called when the child is realized.

   function Expose_MDI
     (MDI : access Gtk_Widget_Record'Class; Args : Gtk_Args) return Boolean;
   --  Called when the child needs to be redrawn.

   procedure Update_Dock_Menu (Child : access MDI_Child_Record'Class);
   procedure Update_Float_Menu (Child : access MDI_Child_Record'Class);
   --  Update the state of the "Float" menu item associated with child.

   procedure Put_In_Notebook
     (MDI : access MDI_Window_Record'Class;
      Side : Dock_Side;
      Child : access MDI_Child_Record'Class);
   --  Remove Child from MDI, and put it under control of a dock box, on the
   --  specific Side.

   procedure Remove_From_Notebook
     (Child : access MDI_Child_Record'Class; Side : Dock_Side);
   --  Remove Child from the notebook it belongs to.
   --  Child will be destroyed, unless you Ref' it first.
   --  The notebook is destroyed if Child was the last item.

   function Get_Child_From_Page (Page : Gtk_Widget) return MDI_Child;
   --  Return the MDI child contained in the notebook page.

   procedure Create_Menu_Entry (Child : access MDI_Child_Record'Class);
   --  Add an entry to the MDI menu that provides easy activation of Child

   procedure Propagate_Expose_Event
     (Container : access Gtk.Container.Gtk_Container_Record'Class;
      Event     : Gdk.Event.Gdk_Event_Expose);
   --  Propagate the expose event Event to all the NO_WINDOW children of
   --  Container. You must call this when Container has a specific expose
   --  callback.

   procedure Cascade_Cb    (MDI   : access Gtk_Widget_Record'Class);
   procedure Tile_H_Cb     (MDI   : access Gtk_Widget_Record'Class);
   procedure Tile_V_Cb     (MDI   : access Gtk_Widget_Record'Class);
   procedure Dock_Cb       (MDI   : access Gtk_Widget_Record'Class);
   procedure Float_Cb      (MDI   : access Gtk_Widget_Record'Class);
   procedure Close_Cb      (MDI   : access Gtk_Widget_Record'Class);
   procedure Focus_Cb      (Child : access Gtk_Widget_Record'Class);
   procedure Maximize_Child_Cb  (Child : access Gtk_Widget_Record'Class);
   procedure Maximize_Cb   (MDI   : access Gtk_Widget_Record'Class);
   procedure Unmaximize_Cb (MDI   : access Gtk_Widget_Record'Class);
   --  Callbacks for the menu

   procedure Set_Focus_Child_MDI
     (MDI : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when the widget that has the keyboard focus has changed. This is
   --  used to automatically select its parent MDI_Child.

   function Set_Focus_Child_MDI_Floating
     (Child : access Gtk_Widget_Record'Class) return Boolean;
   --  Same as Set_Focus_Child_MDI, but for floating windows

   type Raise_Idle_Data is record
      MDI   : MDI_Window;
      Child : MDI_Child;
   end record;
   --  The data that is used for Raise_Child_Idle.

   function Raise_Child_Idle (Data : Raise_Idle_Data) return Boolean;
   --  Raise the child W in an idle loop, when it can not be done immediately
   --  for instance because the child hasn't been resized yet. This would
   --  result in a lot of flickering otherwise.

   procedure Destroy_Raise_Child_Idle (D : in out Raise_Idle_Data);
   --  Called when the idle for Raise_Child_Idle is destroyed.

   package Widget_Idle is new Gtk.Main.Idle (Raise_Idle_Data);

   procedure Source_Drag_Data_Get
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when a drag source must emit some data

   procedure Target_Drag_Data_Received
     (Notebook : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when some data is received by a drop site

   procedure Give_Focus_To_Child (Widget : access Gtk_Widget_Record'Class);
   --  Give the keyboard focus to Widget.

   function Matching_Children
     (MDI : access MDI_Window_Record'Class; Str : String)
      return Children_Array;
   --  Return the list of children of the MDI that match Str

   procedure Update_Selection_Dialog
     (MDI : access MDI_Window_Record'Class; Increment : Integer);
   --  Update the currently selected child in the selection dialog, so that it
   --  matches the filter.

   function Key_Event_In_Floating
     (Win   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Forward the key press event to the Win

   function Key_Event_Selection_Dialog
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Handle key events in the selection dialog

   procedure Update_Tab_Label (Child : access MDI_Child_Record'Class);
   --  Return the tab to use in the notebooks containing Child

   procedure Update_Menu_Item (Child : access MDI_Child_Record'Class);
   --  Update the menu entry for Child

   -------------------------
   -- Set_Focus_Child_MDI --
   -------------------------

   procedure Set_Focus_Child_MDI
     (MDI : access Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      Widget : Gtk_Widget := Gtk_Widget (To_Object (Args, 1));
   begin
      if Widget /= null then
         --  The widget is currently either a notebook or the Gtk_Fixed. Get
         --  its focus widget, which is the one we are really interested in.

         Widget := Get_Focus_Child (Gtk_Container (Widget));

         if Widget /= null then
            Set_Focus_Child (MDI_Window (MDI), Containing => Widget);
         end if;
      end if;
   end Set_Focus_Child_MDI;

   ----------------------------------
   -- Set_Focus_Child_MDI_Floating --
   ----------------------------------

   function Set_Focus_Child_MDI_Floating
     (Child : access Gtk_Widget_Record'Class) return Boolean is
   begin
      Set_Focus_Child (MDI_Child (Child), Force_Focus => False);
      return False;
   end Set_Focus_Child_MDI_Floating;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (MDI   : out MDI_Window;
      Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class) is
   begin
      MDI := new MDI_Window_Record;
      Gtkada.MDI.Initialize (MDI, Group);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (MDI   : access MDI_Window_Record'Class;
      Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => GType_Pointer));
      No_Signals : constant chars_ptr_array (1 .. 0) := (others => Null_Ptr);
   begin
      Gtk.Fixed.Initialize (MDI);
      Set_Has_Window (MDI, True);

      MDI.Group := Gtk_Accel_Group (Group);

      MDI.Title_Layout := Create_Pango_Layout (MDI, "Ap"); -- compute width
      MDI.Background_Color := Parse (Default_MDI_Background_Color);
      Alloc (Get_Default_Colormap, MDI.Background_Color);

      MDI.Title_Bar_Color := Parse (Default_Title_Bar_Color);
      Alloc (Get_Default_Colormap, MDI.Title_Bar_Color);

      MDI.Focus_Title_Color := Parse (Default_Title_Bar_Focus_Color);
      Alloc (Get_Default_Colormap, MDI.Focus_Title_Color);

      MDI.Highlight_Style := Copy (Get_Style (MDI));

      Configure (MDI,
                 Opaque_Resize     => True,
                 Opaque_Move       => True,
                 Background_Color  => MDI.Background_Color,
                 Title_Bar_Color   => MDI.Title_Bar_Color,
                 Focus_Title_Color => MDI.Focus_Title_Color);

      Gtk.Object.Initialize_Class_Record
        (MDI,
         Signals      => MDI_Signals,
         Class_Record => MDI_Class_Record,
         Type_Name    => "GtkAdaMDI",
         Parameters   => Signal_Parameters);

      Gtk_New (MDI.Layout);
      Set_Has_Window (MDI.Layout, True);
      Gtk.Object.Initialize_Class_Record
        (MDI.Layout,
         Signals      => No_Signals,
         Class_Record => MDI_Layout_Class_Record,
         Type_Name    => "GtkAdaMDI_Layout");

      Set_Dnd_Target (MDI_Window (MDI).Layout);

      Put (MDI, MDI.Layout, Drop_Area_Thickness, Drop_Area_Thickness);

      Widget_Callback.Connect
        (MDI, "realize", Widget_Callback.To_Marshaller (Realize_MDI'Access));
      Widget_Callback.Object_Connect
        (MDI.Layout, "realize",
         Widget_Callback.To_Marshaller (Realize_MDI_Layout'Access), MDI);
      Widget_Callback.Connect
        (MDI, "destroy", Widget_Callback.To_Marshaller (Destroy_MDI'Access));

      Set_Default_Size_Allocate_Handler
        (MDI_Class_Record, Size_Allocate_MDI'Access);
      Set_Default_Size_Allocate_Handler
        (MDI_Layout_Class_Record, Size_Allocate_MDI_Layout'Access);
      Return_Callback.Connect
        (MDI, "button_press_event",
         Return_Callback.To_Marshaller (Button_Pressed_MDI'Access));
      Return_Callback.Connect
        (MDI, "button_release_event",
         Return_Callback.To_Marshaller (Button_Release_MDI'Access));
      Return_Callback.Connect
        (MDI, "motion_notify_event",
         Return_Callback.To_Marshaller (Button_Motion_MDI'Access));
      Return_Callback.Connect (MDI, "expose_event", Expose_MDI'Access);

      Widget_Callback.Connect
        (MDI, "set_focus_child", Set_Focus_Child_MDI'Access);

      for S in Left .. Bottom loop
         Gtk_New (MDI.Drop_Sites (S));
         Put (MDI, MDI.Drop_Sites (S), 0, 0);
         Set_Dnd_Target (MDI.Drop_Sites (S));
      end loop;
   end Initialize;

   -----------------------
   -- Matching_Children --
   -----------------------

   function Matching_Children
     (MDI : access MDI_Window_Record'Class; Str : String) return Children_Array
   is
      use type Widget_List.Glist;
      Count    : constant Natural := Natural (Length (MDI.Items));
      Children : Children_Array (1 .. Count);
      L        : Widget_List.Glist := MDI.Items;
      Ind      : Natural := Children'First;
      C        : MDI_Child;
   begin
      while L /= Null_List loop
         C := MDI_Child (Get_Data (L));
         if Str = ""
           or else Index (To_Lower (Get_Short_Title (C)), Str) /= 0
         then
            Children (Ind) := L;
            Ind := Ind + 1;
         end if;
         L := Next (L);
      end loop;
      return Children (Children'First .. Ind - 1);
   end Matching_Children;

   -----------------------------
   -- Update_Selection_Dialog --
   -----------------------------

   procedure Update_Selection_Dialog
     (MDI : access MDI_Window_Record'Class; Increment : Integer)
   is
      D : constant Selection_Dialog_Access :=
        Selection_Dialog_Access (MDI.Selection_Dialog);
      Str : constant String := Get_Text (D.Ent);
      Children : constant Children_Array :=
        Matching_Children (MDI, To_Lower (Str));
      Index : Integer := Children'First;

   begin
      --  Update graphically the list of children matching the filter

      D.Length := Str'Length;
      Append_Text (D.Ent, " {");
      Set_Position (D.Ent, Gint (D.Length));

      for C in Children'Range loop
         if C /= Children'First then
            Append_Text (D.Ent, ",");
         end if;
         Append_Text
           (D.Ent, Locale_To_UTF8
              (Get_Short_Title (MDI_Child (Get_Data (Children (C))))));
      end loop;

      Append_Text (D.Ent, "}");

      --  Check the list of children matching the filter, and update the
      --  current child accordingly

      if Children'Length = 0 then
         D.Current_Child := Null_List;
      else
         while Index <= Children'Last loop
            exit when Children (Index) = D.Current_Child;
            Index := Index + 1;
         end loop;

         Index := Index + Increment;

         if Index > Children'Last then
            Index := Children'First;
         elsif Index < Children'First then
            Index := Children'Last;
         end if;

         D.Current_Child := Children (Index);
      end if;

      if D.Current_Child = Null_List then
         Set_Text (D.Label, "");
         Set_Child_Visible (D.Icon, False);
      else
         declare
            C : MDI_Child;
            Pixmap : Gdk_Pixmap;
            Mask   : Gdk.Gdk_Bitmap;
            Scaled : Gdk_Pixbuf;
         begin
            C := MDI_Child (Get_Data (D.Current_Child));
            Set_Text (D.Label, Locale_To_UTF8 (Get_Short_Title (C)));

            Set_Child_Visible (D.Icon, C.Icon /= null);
            if C.Icon /= null then
               Scaled := Scale_Simple (C.Icon, 32, 32);
               Render_Pixmap_And_Mask (Scaled, Pixmap, Mask, 128);
               Unref (Scaled);
               Set (D.Icon, Pixmap, Mask);
            end if;
         end;
      end if;
   end Update_Selection_Dialog;

   --------------------------------
   -- Key_Event_Selection_Dialog --
   --------------------------------

   function Key_Event_Selection_Dialog
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      M : constant MDI_Window := MDI_Window (MDI);
   begin
      return Check_Interactive_Selection_Dialog
        (M,
         Event,
         Switch_Child_Modifier => Selection_Dialog_Access
           (M.Selection_Dialog).Modifier,
         Next_Child_Key        => Selection_Dialog_Access
           (M.Selection_Dialog).Next_Key,
         Previous_Child_Key     => Selection_Dialog_Access
           (M.Selection_Dialog).Prev_Key);
   end Key_Event_Selection_Dialog;

   ----------------------------------------
   -- Check_Interactive_Selection_Dialog --
   ----------------------------------------

   function Check_Interactive_Selection_Dialog
     (MDI                       : access MDI_Window_Record;
      Event                     : Gdk.Event.Gdk_Event;
      Switch_Child_Modifier     : Gdk.Types.Gdk_Modifier_Type;
      Next_Child_Key            : Gdk.Types.Gdk_Key_Type;
      Previous_Child_Key        : Gdk.Types.Gdk_Key_Type) return Boolean
   is
      use type Widget_List.Glist;
      C : MDI_Child;
      D : Selection_Dialog_Access;
      Box, HBox : Gtk_Box;
      Frame : Gtk_Frame;
      Tmp : Gdk_Grab_Status;
      pragma Unreferenced (Tmp);

   begin
      if Get_Event_Type (Event) = Key_Press
        and then
        (Get_State (Event) = Switch_Child_Modifier
         or else Get_State (Event) = (Switch_Child_Modifier or Shift_Mask))
        and then (Get_Key_Val (Event) = Next_Child_Key
                  or else Get_Key_Val (Event) = Previous_Child_Key)
        and then MDI.Items /= Null_List
      then
         if MDI.Selection_Dialog = null then
            D := new Selection_Dialog_Record;
            Initialize (D, Window_Popup);

            if MDI.All_Floating_Mode then
               Set_Position (D, Win_Pos_Mouse);
            else
               Set_Transient_For (D, Gtk_Window (Get_Toplevel (MDI)));
               Set_Position (D, Win_Pos_Center_On_Parent);
            end if;
            Set_Default_Size (D, 300, 70);

            Gtk_New (Frame);
            Add (D, Frame);

            --  By default, switch between the last two selected items
            D.Current_Child := First (MDI.Items);

            Gtk_New_Vbox (Box, Homogeneous => False);
            Add (Frame, Box);

            Gtk_New_Hbox (HBox, Homogeneous => False);
            Pack_Start (Box, HBox, Expand => False);

            Gtk_New (D.Icon);
            Pack_Start (HBox, D.Icon, Expand => False);

            Gtk_New (D.Label);
            Pack_Start (HBox, D.Label, Expand => True, Fill => True);

            Gtk_New (D.Ent);
            Pack_Start (Box, D.Ent, Expand => True);
            Set_Editable (D.Ent, False);

            Show_All (D);
            D.Modifier := Switch_Child_Modifier;
            D.Next_Key := Next_Child_Key;
            D.Prev_Key := Previous_Child_Key;

            MDI.Selection_Dialog := Gtk_Widget (D);

            --  Make sure all the key events are forwarded to us, as otherwise
            --  if the mouse was moving out of the window we wouldn't the
            --  events
            Tmp := Keyboard_Grab (Get_Window (D), True, Time => 0);

            Return_Callback.Object_Connect
              (D, "key_release_event",
               Return_Callback.To_Marshaller
               (Key_Event_Selection_Dialog'Access), MDI);
            Return_Callback.Object_Connect
              (D, "key_press_event",
               Return_Callback.To_Marshaller
               (Key_Event_Selection_Dialog'Access), MDI);
         else
            D := Selection_Dialog_Access (MDI.Selection_Dialog);
            Delete_Text (D.Ent, Gint (D.Length), -1);
         end if;

         if Get_Key_Val (Event) = Next_Child_Key then
            Update_Selection_Dialog (MDI, +1);
         else
            Update_Selection_Dialog (MDI, -1);
         end if;

         return True;

      elsif Get_Event_Type (Event) = Key_Press
        and then MDI.Selection_Dialog /= null
      then
         D := Selection_Dialog_Access (MDI.Selection_Dialog);

         if Get_Key_Val (Event) = Gdk.Types.Keysyms.GDK_BackSpace
           or else Get_Key_Val (Event) = Gdk.Types.Keysyms.GDK_Delete
         then
            Delete_Text (D.Ent, Gint (D.Length) - 1, -1);
         else
            Delete_Text (D.Ent, Gint (D.Length), -1);
            Append_Text
              (D.Ent, Locale_To_UTF8 (Get_String (Event)));
         end if;

         Update_Selection_Dialog (MDI, 0);

         return True;

      elsif Get_Event_Type (Event) = Key_Release
        and then
          ((Switch_Child_Modifier and Control_Mask) = 0
           or else Get_Key_Val (Event) = Gdk.Types.Keysyms.GDK_Control_L
           or else Get_Key_Val (Event) = Gdk.Types.Keysyms.GDK_Control_R)
        and then
          ((Switch_Child_Modifier and Mod1_Mask) = 0
           or else Get_Key_Val (Event) = Gdk.Types.Keysyms.GDK_Meta_L
           or else Get_Key_Val (Event) = Gdk.Types.Keysyms.GDK_Meta_R
           or else Get_Key_Val (Event) = Gdk.Types.Keysyms.GDK_Alt_L
           or else Get_Key_Val (Event) = Gdk.Types.Keysyms.GDK_Alt_R)
        and then MDI.Selection_Dialog /= null
      then
         D := Selection_Dialog_Access (MDI.Selection_Dialog);
         if D.Current_Child /= Null_List then
            C := MDI_Child (Widget_List.Get_Data (D.Current_Child));
            Set_Focus_Child (C);
         end if;

         Keyboard_Ungrab (Time => 0);
         Destroy (MDI.Selection_Dialog);
         MDI.Selection_Dialog := null;

         return True;
      end if;

      return False;
   end Check_Interactive_Selection_Dialog;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (MDI                       : access MDI_Window_Record;
      Opaque_Resize             : Boolean             := False;
      Opaque_Move               : Boolean             := False;
      Opaque_Docks              : Boolean             := False;
      Close_Floating_Is_Unfloat : Boolean             := True;
      Title_Font                : Pango_Font_Description := null;
      Background_Color          : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Title_Bar_Color           : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Focus_Title_Color         : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color)
   is
      Desc : Pango_Font_Description;
      W, H : Gint;
      List : Widget_List.Glist;
      C    : MDI_Child;
      Need_Redraw : Boolean := False;
   begin
      MDI.Opaque_Resize := Opaque_Resize;
      MDI.Opaque_Move   := Opaque_Move;
      MDI.Opaque_Docks  := Opaque_Docks;
      MDI.Close_Floating_Is_Unfloat := Close_Floating_Is_Unfloat;

      if Title_Font /= null then
         Set_Font_Description (MDI.Title_Layout, Title_Font);
      else
         Desc := From_String (Default_Title_Font);
         Set_Font_Description (MDI.Title_Layout, Desc);
         Free (Desc);
      end if;

      Get_Pixel_Size (MDI.Title_Layout, W, H);
      MDI.Title_Bar_Height := 2 + H;

      --  Resize the title bar of all children already in the MDI

      List := First (MDI.Items);
      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));
         Set_USize (C.Title_Box, -1, MDI.Title_Bar_Height);
         List := Widget_List.Next (List);
      end loop;

      --  Ignore changes in colors, unless the MDI is realized

      if Background_Color /= Null_Color then
         MDI.Background_Color  := Background_Color;
      end if;

      if Title_Bar_Color /= Null_Color then
         MDI.Title_Bar_Color   := Title_Bar_Color;
      end if;

      if Focus_Title_Color /= Null_Color then
         MDI.Focus_Title_Color := Focus_Title_Color;

         Set_Foreground
           (MDI.Highlight_Style, State_Normal, MDI.Focus_Title_Color);
         Set_Foreground
           (MDI.Highlight_Style, State_Active, MDI.Focus_Title_Color);
         Set_Foreground
           (MDI.Highlight_Style, State_Selected, MDI.Focus_Title_Color);
         Set_Foreground
           (MDI.Highlight_Style, State_Prelight, MDI.Focus_Title_Color);
         Set_Foreground
           (MDI.Highlight_Style, State_Insensitive, MDI.Focus_Title_Color);
      end if;

      if Realized_Is_Set (MDI) then
         if Background_Color /= Null_Color then
            Set_Background (Get_Window (MDI), Background_Color);

            if Realized_Is_Set (MDI.Layout) then
               Set_Background (Get_Window (MDI.Layout), Background_Color);
            end if;

            Need_Redraw := True;
         end if;

         if Title_Bar_Color /= Null_Color then
            Set_Foreground (MDI.Non_Focus_GC, Title_Bar_Color);
            Need_Redraw := True;
         end if;

         if Focus_Title_Color /= Null_Color then
            Set_Foreground (MDI.Focus_GC, Focus_Title_Color);
            Need_Redraw := True;
         end if;

         if Need_Redraw then
            Queue_Draw (MDI);
         end if;
      end if;
   end Configure;

   ------------------------
   -- Realize_MDI_Layout --
   ------------------------

   procedure Realize_MDI_Layout (MDI : access Gtk_Widget_Record'Class) is
      M : constant MDI_Window := MDI_Window (MDI);
   begin
      Gdk.Window.Set_Background (Get_Window (M.Layout), M.Background_Color);
   end Realize_MDI_Layout;

   -----------------
   -- Realize_MDI --
   -----------------

   procedure Realize_MDI (MDI : access Gtk_Widget_Record'Class) is
      Window_Attr : Gdk.Window_Attr.Gdk_Window_Attr;
      M           : MDI_Window := MDI_Window (MDI);
      Cursor      : Gdk_Cursor;

   begin
      Gdk.Window.Set_Background (Get_Window (M), M.Background_Color);

      Gdk_New (M.Non_Focus_GC, Get_Window (MDI));
      Set_Foreground (M.Non_Focus_GC, M.Title_Bar_Color);
      Set_Exposures (M.Non_Focus_GC, False);

      Gdk_New (M.Focus_GC, Get_Window (MDI));
      Set_Foreground (M.Focus_GC, M.Focus_Title_Color);
      Set_Exposures (M.Focus_GC, False);

      Gdk_New (M.Xor_GC, Get_Window (MDI));
      Set_Function (M.Xor_GC, Invert);
      Set_Exposures (M.Xor_GC, False);
      Set_Subwindow (M.Xor_GC, Include_Inferiors);

      Gdk_New (Cursor, Cross);
      Gdk_New (Window_Attr,
               Window_Type => Window_Child,
               Wclass      => Input_Output,
               Cursor      => Cursor,
               Visual      => Get_Visual (MDI),
               Colormap    => Get_Colormap (MDI),
               Event_Mask  => Get_Events (MDI)
               or Exposure_Mask
               or Button_Press_Mask
               or Button_Release_Mask
               or Button_Motion_Mask);

      for J in Left .. Bottom loop
         Gdk_New (M.Handles (J),
                  Parent          => Get_Window (MDI),
                  Attributes      => Window_Attr,
                  Attributes_Mask => Wa_Cursor or Wa_Colormap or Wa_Visual);
         Set_User_Data (M.Handles (J), MDI);

         if M.Docks (J) /= null then
            Gdk.Window.Show (M.Handles (J));
         end if;
      end loop;

      --  Destroy the window attribute and the cursor

      Destroy (Cursor);
      Destroy (Window_Attr);
   end Realize_MDI;

   ----------------
   -- Expose_MDI --
   ----------------

   function Expose_MDI
     (MDI : access Gtk_Widget_Record'Class; Args : Gtk_Args)
     return Boolean
   is
      pragma Warnings (Off);
      M                 : constant MDI_Window := MDI_Window (MDI);
      Event             : constant Gdk_Event := To_Event (Args, 1);
      Area              : constant Gdk_Rectangle := Get_Area (Event);
      Orientation       : Gtk_Orientation;
      First, Last       : Gint;
      X, Y, W, H, Depth : Gint;

   begin
      if Visible_Is_Set (M) and then Mapped_Is_Set (M) then
         for J in M.Handles'Range loop
            if J = Left or else J = Right then
               Orientation := Orientation_Vertical;
            else
               Orientation := Orientation_Horizontal;
            end if;

            Paint_Handle
              (Get_Style (M),
               M.Handles (J),
               State_Normal,
               Shadow_None,
               Area,
               M,
               X => 0,
               Y => 0,
               Width => -1,
               Height => -1,
               Orientation => Orientation);
         end loop;

         --  Draw the relief lines. Note that this is slightly complex, since
         --  we might have to draw on several windows if there are several
         --  handles in the layout.

         if Draw_Handles_Shadow then
            if M.Docks (Left) /= null then
               Get_Geometry (M.Handles (Left), X, Y, W, H, Depth);
               First := 0;
               Last := H;

               Draw_Line
                 (M.Handles (Left), Get_White_GC (Get_Style (M)), 0, 0, 0, H);

               if M.Priorities (Left) < M.Priorities (Top) then
                  if M.Docks (Top) /= null then
                     First := M.Docks_Size (Top) + Handle_Size - 1
                       - Drop_Area_Thickness;
                     Draw_Line
                       (M.Handles (Left), Get_Black_GC (Get_Style (M)),
                        Handle_Size - 1, 0, Handle_Size - 1,
                        First - Handle_Size + 1);
                  else
                     Draw_Line
                       (M.Handles (Left), Get_White_GC (Get_Style (M)),
                        0, 0, Handle_Size - 1, 0);
                  end if;
               end if;

               if M.Priorities (Left) < M.Priorities (Bottom) then
                  if M.Docks (Bottom) /= null then
                     Last := H - M.Docks_Size (Bottom) - Handle_Size;
                     Draw_Line
                       (M.Handles (Left), Get_Black_GC (Get_Style (M)),
                        Handle_Size - 1, Last + Handle_Size - 1,
                        Handle_Size - 1, H);
                  else
                     Draw_Line
                       (M.Handles (Left), Get_Black_GC (Get_Style (M)),
                        0, H - 1, Handle_Size - 1, H - 1);
                  end if;
               end if;

               Draw_Line
                 (M.Handles (Left), Get_Black_GC (Get_Style (M)),
                  Handle_Size - 1, First, Handle_Size - 1, Last);
            end if;

            if M.Docks (Bottom) /= null then
               Get_Geometry (M.Handles (Bottom), X, Y, W, H, Depth);
               First := 0;
               Last := W;

               Draw_Line
                 (M.Handles (Bottom), Get_Black_GC (Get_Style (M)),
                  0, Handle_Size - 1, W, Handle_Size - 1);

               if M.Priorities (Bottom) < M.Priorities (Right) then
                  if M.Docks (Right) /= null then
                     Last := W - M.Docks_Size (Right) - Handle_Size;
                     Draw_Line
                       (M.Handles (Bottom), Get_White_GC (Get_Style (M)),
                        Last + Handle_Size - 1, 0, W, 0);
                  else
                     Draw_Line
                       (M.Handles (Bottom), Get_Black_GC (Get_Style (M)),
                        W - 1, 0, W - 1, Handle_Size - 1);
                  end if;
               end if;

               if M.Priorities (Bottom) < M.Priorities (Left) then
                  if M.Docks (Left) /= null then
                     First := M.Docks_Size (Left) + Handle_Size
                       - Drop_Area_Thickness - 1;
                     Draw_Line
                       (M.Handles (Bottom), Get_White_GC (Get_Style (M)),
                        0, 0, M.Docks_Size (Left) - Drop_Area_Thickness, 0);
                  else
                     Draw_Line
                       (M.Handles (Bottom), Get_White_GC (Get_Style (M)),
                        0, 0, 0, Handle_Size - 2);
                  end if;
               end if;

               Draw_Line
                 (M.Handles (Bottom), Get_White_GC (Get_Style (M)),
                  First, 0, Last, 0);
            end if;

            if M.Docks (Right) /= null then
               Get_Geometry (M.Handles (Right), X, Y, W, H, Depth);
               First := 0;
               Last := H;

               Draw_Line
                 (M.Handles (Right), Get_Black_GC (Get_Style (M)),
                  Handle_Size - 1, 0, Handle_Size - 1, H);

               if M.Priorities (Right) < M.Priorities (Top) then
                  if M.Docks (Top) /= null then
                     First := M.Docks_Size (Top) + Handle_Size - 1;
                     Draw_Line
                       (M.Handles (Right), Get_White_GC (Get_Style (M)),
                        0, 0, 0, First - Handle_Size);
                  else
                     Draw_Line
                       (M.Handles (Right), Get_White_GC (Get_Style (M)),
                        0, 0, Handle_Size - 1, 0);
                  end if;
               end if;

               if M.Priorities (Right) < M.Priorities (Bottom) then
                  if M.Docks (Bottom) /= null then
                     Last := H - M.Docks_Size (Bottom) - Handle_Size;
                     Draw_Line
                       (M.Handles (Right), Get_White_GC (Get_Style (M)),
                        0, Last + Handle_Size - 1, 0, H);
                  else
                     Draw_Line
                       (M.Handles (Right), Get_Black_GC (Get_Style (M)),
                        0, H - 1, Handle_Size - 1, H - 1);
                  end if;
               end if;

               Draw_Line
                 (M.Handles (Right), Get_White_GC (Get_Style (M)),
                  0, First, 0, Last);
            end if;

            if M.Docks (Top) /= null then
               Get_Geometry (M.Handles (Top), X, Y, W, H, Depth);
               First := 0;
               Last := W;

               Draw_Line
                 (M.Handles (Top), Get_White_GC (Get_Style (M)),
                  0, 0, W, 0);

               if M.Priorities (Top) <= M.Priorities (Left) then
                  if M.Docks (Left) /= null then
                     First := M.Docks_Size (Left) + Handle_Size
                       - Drop_Area_Thickness - 1;
                     Draw_Line
                       (M.Handles (Top), Get_Black_GC (Get_Style (M)), 0,
                        Handle_Size - 1,
                        M.Docks_Size (Left) - Drop_Area_Thickness,
                        Handle_Size - 1);
                  end if;
                  Draw_Line
                    (M.Handles (Top), Get_White_GC (Get_Style (M)),
                     0, 0, 0, Handle_Size);
               end if;

               if M.Priorities (Top) <= M.Priorities (Right) then
                  if M.Docks (Right) /= null then
                     Last := W - M.Docks_Size (Right) - Handle_Size;
                     Draw_Line
                       (M.Handles (Top), Get_Black_GC (Get_Style (M)),
                        Last + Handle_Size - 1, Handle_Size - 1,
                        W, Handle_Size - 1);
                  end if;
                  Draw_Line
                    (M.Handles (Top), Get_Black_GC (Get_Style (M)),
                     W - 1, 0, W - 1, Handle_Size);
               end if;

               Draw_Line
                 (M.Handles (Top), Get_Black_GC (Get_Style (M)),
                  First, Handle_Size - 1, Last, Handle_Size - 1);
            end if;
         end if;
      end if;

      return Default_Expose_Event_Handler
        (Class_From_Type (Parent (Get_Type (M)))) (Get_Object (M), Event);
   end Expose_MDI;

   ------------------
   -- Compute_Size --
   ------------------

   procedure Compute_Size
     (MDI                 : access MDI_Window_Record'Class;
      Side                : Dock_Side;
      Alloc               : out Gtk_Allocation;
      Is_Handle           : Boolean)
   is
      MDI_Width  : constant Allocation_Int := Get_Allocation_Width (MDI);
      MDI_Height : constant Allocation_Int := Get_Allocation_Height (MDI);
   begin
      case Side is
         when Left =>
            if Is_Handle then
               Alloc.X     := MDI.Docks_Size (Left);
               Alloc.Width := Allocation_Int (Handle_Size);
            else
               Alloc.X     := Drop_Area_Thickness;
               Alloc.Width := Allocation_Int (MDI.Docks_Size (Left))
                 - Drop_Area_Thickness;
            end if;

         when Right =>
            if Is_Handle then
               Alloc.Width := Allocation_Int (Handle_Size);
               Alloc.X     := MDI_Width - MDI.Docks_Size (Right) - Handle_Size
                 - Drop_Area_Thickness;
            else
               Alloc.Width := Allocation_Int (MDI.Docks_Size (Right));
               Alloc.X     := MDI_Width - Gint (Alloc.Width) -
                 Drop_Area_Thickness;
            end if;

         when Top =>
            if Is_Handle then
               Alloc.Y      := MDI.Docks_Size (Top);
               Alloc.Height := Allocation_Int (Handle_Size);
            else
               Alloc.Y      := Drop_Area_Thickness;
               Alloc.Height := Allocation_Int (MDI.Docks_Size (Top))
                 - Drop_Area_Thickness;
            end if;

         when Bottom =>
            if Is_Handle then
               Alloc.Height := Allocation_Int (Handle_Size);
               Alloc.Y :=  MDI_Height - MDI.Docks_Size (Bottom) - Handle_Size
                 - Drop_Area_Thickness;
            else
               Alloc.Height := Allocation_Int (MDI.Docks_Size (Bottom));
               Alloc.Y := MDI_Height - Gint (Alloc.Height) -
                 Drop_Area_Thickness;
            end if;

         when None =>
            if MDI.Docks (Left) /= null then
               Alloc.X := MDI.Docks_Size (Left) + Handle_Size;
            else
               Alloc.X := Drop_Area_Thickness;
            end if;

            if MDI.Docks (Top) /= null then
               Alloc.Y := MDI.Docks_Size (Top) + Handle_Size;
            else
               Alloc.Y := Drop_Area_Thickness;
            end if;

            Alloc.Width := MDI_Width - Allocation_Int (Alloc.X) -
              Drop_Area_Thickness;
            if MDI.Docks (Right) /= null then
               Alloc.Width := Alloc.Width - Handle_Size -
                 Allocation_Int (MDI.Docks_Size (Right));
            end if;

            Alloc.Height := MDI_Height - Allocation_Int (Alloc.Y) -
              Drop_Area_Thickness;
            if MDI.Docks (Bottom) /= null then
               Alloc.Height := Alloc.Height - Handle_Size
                 - Allocation_Int (MDI.Docks_Size (Bottom));
            end if;
      end case;

      case Side is
         when Left | Right =>
            if MDI.Priorities (Top) <= MDI.Priorities (Side)
              and then MDI.Docks (Top) /= null
            then
               Alloc.Y := MDI.Docks_Size (Top) + Handle_Size;
            else
               Alloc.Y := Drop_Area_Thickness;
            end if;

            Alloc.Height := MDI_Height - Allocation_Int (Alloc.Y) -
              Drop_Area_Thickness;

            if MDI.Priorities (Bottom) <= MDI.Priorities (Side)
              and then MDI.Docks (Bottom) /= null
            then
               Alloc.Height := Alloc.Height
                 - Allocation_Int (MDI.Docks_Size (Bottom)) - Handle_Size;
            end if;

         when Top | Bottom =>
            if MDI.Priorities (Left) < MDI.Priorities (Side)
              and then MDI.Docks (Left) /= null
            then
               Alloc.X := MDI.Docks_Size (Left) + Handle_Size;
            else
               Alloc.X := Drop_Area_Thickness;
            end if;

            Alloc.Width := MDI_Width - Allocation_Int (Alloc.X) -
              Drop_Area_Thickness;

            if MDI.Priorities (Right) < MDI.Priorities (Side)
              and then MDI.Docks (Right) /= null
            then
               Alloc.Width := Alloc.Width
                 - Allocation_Int (MDI.Docks_Size (Right)) - Handle_Size;
            end if;

         when None =>
            null;
      end case;
   end Compute_Size;

   ------------------------
   -- Reposition_Handles --
   ------------------------

   procedure Reposition_Handles (M : access MDI_Window_Record'Class) is
      Alloc      : Gtk_Allocation;
   begin
      if Realized_Is_Set (M) then
         for S in Left .. Bottom loop
            if M.Docks (S) /= null then
               Compute_Size (M, S, Alloc, Is_Handle => True);
               Show (M.Handles (S));
               Gdk.Window.Move_Resize
                 (M.Handles (S), Alloc.X, Alloc.Y,
                  Gint (Alloc.Width), Gint (Alloc.Height));
            else
               Hide (M.Handles (S));
            end if;
         end loop;
      end if;
   end Reposition_Handles;

   ------------------------------
   -- Size_Allocate_MDI_Layout --
   ------------------------------

   procedure Size_Allocate_MDI_Layout
     (Layout : System.Address; Alloc : Gtk_Allocation)
   is
      L : constant Gtk_Widget := Gtk.Widget.Convert (Layout);
   begin
      Set_Allocation (L, Alloc);

      if Realized_Is_Set (L) then
         Move_Resize
           (Get_Window (L), Alloc.X, Alloc.Y,
            Gint (Alloc.Width), Gint (Alloc.Height));
      end if;
   end Size_Allocate_MDI_Layout;

   ------------------------
   -- Compute_Docks_Size --
   ------------------------

   procedure Compute_Docks_Size (MDI : access MDI_Window_Record'Class) is
      MDI_Alloc_Width : constant Allocation_Int := Get_Allocation_Width (MDI);
      MDI_Alloc_Height : constant Allocation_Int :=
        Get_Allocation_Height (MDI);
      Alloc : Gtk_Allocation;
      Req   : Gtk_Requisition;
   begin
      --  If the MDI hasn't been created yet, we do nothing, it will be done
      --  later.
      if MDI_Alloc_Width = 1 or else MDI_Alloc_Height = 1 then
         return;
      end if;

      --  Resize all the handles and notebooks on the sides.

      for J in Left .. Bottom loop
         if MDI.Docks_Size (J) = -1
           and then MDI.Docks (J) /= null
         then
            Req := Get_Child_Requisition (MDI.Docks (J));

            case J is
               when Left | Right =>
                  MDI.Docks_Size (J) := Req.Width;

               when Top | Bottom =>
                  MDI.Docks_Size (J) := Req.Height;
            end case;
         end if;
      end loop;

      --  Make sure that the docks don't make the middle area too small, and
      --  assign it its new size.

      Compute_Size (MDI, None, Alloc, Is_Handle => False);

      if Alloc.Width < Min_Width then
         if MDI.Docks_Size (Left) > Min_Width - Alloc.Width then
            MDI.Docks_Size (Left) := MDI.Docks_Size (Left)
              - Min_Width + Alloc.Width;
         else
            MDI.Docks_Size (Right) := MDI.Docks_Size (Right)
              - Min_Width + Alloc.Width;
         end if;
      end if;

      if Alloc.Height < Min_Width then
         if MDI.Docks_Size (Top) > Min_Width - Alloc.Height then
            MDI.Docks_Size (Top) := MDI.Docks_Size (Top)
              - Min_Width + Alloc.Height;
         else
            MDI.Docks_Size (Bottom) := MDI.Docks_Size (Bottom)
              - Min_Width + Alloc.Height;
         end if;
      end if;

      if Alloc.Width < Min_Width or else Alloc.Height < Min_Width then
         Compute_Size (MDI, None, Alloc, Is_Handle => False);
      end if;

      if not MDI.All_Floating_Mode then
         if MDI.Children_Are_Maximized then
            Size_Allocate (MDI.Docks (None), Alloc);
         else
            Size_Allocate (MDI.Layout, Alloc);
         end if;
      end if;

      --  Once this is done, we can actually resize the docks

      for S in Left .. Bottom loop
         if MDI.Docks (S) /= null then
            Compute_Size (MDI, S, Alloc, Is_Handle => False);
            Size_Allocate (MDI.Docks (S), Alloc);
         end if;
      end loop;

      if not MDI.All_Floating_Mode then
         Alloc := (X => 0, Y => 0, Width => MDI_Alloc_Width,
                   Height => Drop_Area_Thickness);
         Size_Allocate (MDI.Drop_Sites (Top), Alloc);

         Alloc := (X => 0, Y => MDI_Alloc_Height - Drop_Area_Thickness,
                   Width => MDI_Alloc_Width,
                   Height => Drop_Area_Thickness);
         Size_Allocate (MDI.Drop_Sites (Bottom), Alloc);

         Alloc := (X => 0, Y => Drop_Area_Thickness,
                   Width => Drop_Area_Thickness,
                   Height => MDI_Alloc_Height - 2 * Drop_Area_Thickness);
         Size_Allocate (MDI.Drop_Sites (Left), Alloc);

         Alloc := (X => MDI_Alloc_Width - Drop_Area_Thickness,
                   Y => Drop_Area_Thickness,
                   Width => Drop_Area_Thickness,
                   Height => MDI_Alloc_Height - 2 * Drop_Area_Thickness);
         Size_Allocate (MDI.Drop_Sites (Right), Alloc);
      end if;

      Reposition_Handles (MDI);
   end Compute_Docks_Size;

   -----------------------
   -- Size_Allocate_MDI --
   -----------------------

   procedure Size_Allocate_MDI
     (MDI : System.Address; MDI_Alloc : Gtk_Allocation)
   is
      use type Widget_List.Glist;

      M     : constant MDI_Window := MDI_Window (Gtk.Widget.Convert (MDI));
      Alloc : Gtk_Allocation;
      Req   : Gtk_Requisition;
      List  : Widget_List.Glist;
      C     : MDI_Child;

   begin
      --  First, register the new size of the MDI itself

      if Realized_Is_Set (M) then
         Move_Resize
           (Get_Window (M),
            MDI_Alloc.X, MDI_Alloc.Y,
            Gint (MDI_Alloc.Width), Gint (MDI_Alloc.Height));
      end if;

      --  Resize the children that haven't been initialized yet.
      --  We also need to resize the unmaximized children, in case they have
      --  requested a resize (and that would be the reason we got the resize on
      --  the MDI).

      List := First (M.Items);

      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));

         if C.Uniconified_Width = -1 or else C.Uniconified_Height = -1 then
            Req := Get_Child_Requisition (C);
            C.Uniconified_Width := Req.Width;
            C.Uniconified_Height := Req.Height;
         end if;

         if (not M.Children_Are_Maximized and then C.State = Normal)
           or else (C.Uniconified_Width = -1 or else C.Uniconified_Height = -1)
         then
            Alloc := (C.X, C.Y, Allocation_Int (C.Uniconified_Width),
                      Allocation_Int (C.Uniconified_Height));
            Size_Allocate (C, Alloc);
         end if;

         List := Widget_List.Next (List);
      end loop;

      Set_Allocation (M, MDI_Alloc);

      Compute_Docks_Size (M);
   end Size_Allocate_MDI;

   -----------------
   -- Destroy_MDI --
   -----------------

   procedure Destroy_MDI (MDI : access Gtk_Widget_Record'Class) is
      use Widget_List;

      Tmp : Widget_List.Glist := First (MDI_Window (MDI).Items);
      N   : Widget_List.Glist;

   begin
      MDI_Window (MDI).Prevent_Focus_On_Page_Switch := True;

      --  Note: we only destroy the floating children. Other children will be
      --  destroyed when their parent container is destroyed, so we have
      --  nothing to do for them.

      while Tmp /= Null_List loop
         --  Get the next field first, since Destroy will actually destroy Tmp

         N := Next (Tmp);
         if MDI_Child (Get_Data (Tmp)).State = Floating then
            Destroy (Get_Data (Tmp));
         else
            --  Pretend the child is not docked or floating. Otherwise,
            --  Destroy_Child would try to undock the child. Standard gtk+
            --  containers handle this by having this destroy callback called
            --  last, but it isn't doable from GtkAda since it means modifying
            --  the pointer-to-subprogram in the Class struct.
            MDI_Child (Get_Data (Tmp)).State := Normal;
         end if;
         Tmp := N;
      end loop;

      Free (MDI_Window (MDI).Items);
      Unref (MDI_Window (MDI).Title_Layout);

      if MDI_Window (MDI).Menu /= null then
         Destroy (MDI_Window (MDI).Menu);
      end if;
   end Destroy_MDI;

   -------------------
   -- Iconify_Child --
   -------------------

   procedure Iconify_Child (Child : access Gtk_Widget_Record'Class) is
      C : constant MDI_Child := MDI_Child (Child);
   begin
      Minimize_Child (C, not (C.State = Iconified));
      Set_Focus_Child (C);

   exception
      when others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the button in Initialize
         null;
   end Iconify_Child;

   -----------
   -- Close --
   -----------

   procedure Close
     (MDI : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class;
      Force : Boolean := False)
   is
      C : constant MDI_Child := Find_MDI_Child (MDI, Child);
   begin
      if C /= null then
         Close_Child (C, Force);
      end if;
   end Close;

   --------------------------
   -- Internal_Close_Child --
   --------------------------

   procedure Internal_Close_Child (Child : access Gtk_Widget_Record'Class) is
      C : constant MDI_Child := MDI_Child (Child);
   begin
      Close_Child (C);
   end Internal_Close_Child;

   -----------------
   -- Close_Child --
   -----------------

   procedure Close_Child
     (Child : access MDI_Child_Record'Class;
      Force : Boolean := False)
   is
      Event      : Gdk_Event;
   begin
      --  Don't do anything for now if the MDI isn't realized, since we
      --  can't send create the event anyway.

      if Realized_Is_Set (Child.MDI) then
         Allocate (Event, Delete, Get_Window (Child.MDI));

         --  For a top-level window, we must rebuild the initial widget
         --  temporarily, so that the application can do all the test it wants.
         --  However, we need to restore the initial state before calling
         --  Dock_Child and Float_Child below

         if Force or else not Return_Callback.Emit_By_Name
           (Child.Initial, "delete_event", Event)
         then
            Float_Child (Child, False);
            Destroy (Child);
         end if;

         Free (Event);
      end if;

   exception
      when others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the button in Initialize
         null;
   end Close_Child;

   -------------------
   -- Destroy_Child --
   -------------------

   procedure Destroy_Child (Child : access Gtk_Widget_Record'Class) is
      use type Widget_SList.GSlist;
      C : MDI_Child := MDI_Child (Child);

   begin
      --  We know at that stage that Child has already been unparent-ed

      pragma Assert (Get_Parent (Child) = null);

      Ref (C);

      --  The child of the MDI_Child has now been taken care of, thus we need
      --  to take care of the MDI_Child itself now.

      if C.Menu_Item /= null then
         Destroy (C.Menu_Item);
      end if;

      Widget_List.Remove (C.MDI.Items, Gtk_Widget (C));

      if not Gtk.Object.In_Destruction_Is_Set (C.MDI) then
         --  Initial could be null if we are destroying a floating
         --  child explicitly (by closing its X11 window)
         if C.Initial /= null then
            --  Do not unfloat the child, since the toplevel is no longer a
            --  Gtk_Window, and we would get a CE in Float_Child.
            Dock_Child (C, False);
         end if;

         if C.MDI.Raise_Id /= 0 then
            Idle_Remove (C.MDI.Raise_Id);
         end if;

         --  For maximized children, test whether there are still enough
         --  pages. Note that if the child is destroyed through its title bar
         --  button, it has already been unparented, and thus is no longer in
         --  the notebook.

         if C.State = Normal
           and then C.MDI.Children_Are_Maximized
         then
            Remove_From_Notebook (C, None);

            Set_Show_Tabs
              (C.MDI.Docks (None),
               Get_Nth_Page (C.MDI.Docks (None), 1) /= null);

         elsif Get_Parent (C) /= null then
            Remove (Gtk_Container (Get_Parent (C)), C);
         end if;
      end if;

      --  Reset the focus child, but only after we have finished manipulating
      --  the notebooks. Otherwise, we get a switch_page event, that calls
      --  Set_Focus_Child again

      if C = C.MDI.Focus_Child then
         C.MDI.Focus_Child := null;

         --  Give the focus back to the last child that had it.
         if C.MDI.Items /= Widget_List.Null_List then
            Set_Focus_Child (MDI_Child (Get_Data (First (C.MDI.Items))));
         end if;
      end if;

      --  Destroy the child, unless the user has explicitely kept a Ref on it
      --  (therefore, do not use Destroy, only Unref). In all cases, it should
      --  be hidden on the screen

      if Get_Parent (C.Initial) /= null then
         Remove (Gtk_Container (Get_Parent (C.Initial)), C.Initial);
      end if;

      C.Initial := null;

      Free (C.Title);
      Free (C.Short_Title);

      Unref (C);
   end Destroy_Child;

   ---------------------------
   -- Destroy_Initial_Child --
   ---------------------------

   procedure Destroy_Initial_Child (Child : access Gtk_Widget_Record'Class) is
   begin
      if not Gtk.Object.Destroyed_Is_Set (Child) then
         Destroy (Child);
      end if;
   end Destroy_Initial_Child;

   ----------------
   -- Draw_Child --
   ----------------

   procedure Draw_Child
     (Child : access MDI_Child_Record'Class; Area : Gdk_Rectangle)
   is
      use Widget_List;
      pragma Unreferenced (Area);

      GC : Gdk.Gdk_GC := Child.MDI.Non_Focus_GC;
      W, H : Gint;
      X : Gint := Border_Thickness + 3;
   begin
      --  Call this function so that for a dock item is highlighted if the
      --  current page is linked to the focus child.

      if Child.MDI.Focus_Child = MDI_Child (Child) then
         GC := Child.MDI.Focus_GC;
      end if;

      Draw_Rectangle
        (Get_Window (Child),
         GC,
         True,
         Border_Thickness,
         Border_Thickness,
         Gint (Get_Allocation_Width (Child)) - 2 * Border_Thickness,
         Child.MDI.Title_Bar_Height);

      if Child.Icon /= null then
         W := Get_Width (Child.Icon);
         H := Get_Height (Child.Icon);

         Render_To_Drawable_Alpha
           (Child.Icon,
            Get_Window (Child),
            Src_X  => 0,
            Src_Y  => 0,
            Dest_X => X,
            Dest_Y => Border_Thickness + (Child.MDI.Title_Bar_Height - H) / 2,
            Width  => W,
            Height => H,
            Alpha  => Alpha_Full,
            Alpha_Threshold => 128);

         X := X + W + 1;
      end if;

      Set_Text (Child.MDI.Title_Layout, Locale_To_UTF8 (Child.Title.all));
      Get_Pixel_Size (Child.MDI.Title_Layout, W, H);
      Draw_Layout
        (Get_Window (Child),
         Get_White_GC (Get_Style (Child.MDI)),
         X,
         Border_Thickness + (Child.MDI.Title_Bar_Height - H) / 2,
         Child.MDI.Title_Layout);

      Draw_Shadow
        (Get_Style (Child),
         Get_Window (Child),
         State_Normal,
         Shadow_Out,
         1, 1,
         Gint (Get_Allocation_Width (Child)) - 1,
         Gint (Get_Allocation_Height (Child)) - 1);
   end Draw_Child;

   ----------------
   -- Draw_Child --
   ----------------

   function Draw_Child
     (Child : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean is
   begin
      Draw_Child (MDI_Child (Child), Get_Area (Event));

      Propagate_Expose_Event (MDI_Child (Child), Event);
      return False;
   end Draw_Child;

   -----------------------------
   -- Resize_Docks_From_Mouse --
   -----------------------------

   procedure Resize_Docks_From_Mouse (MDI : access MDI_Window_Record'Class) is
   begin
      case MDI.Selected is
         when Left =>
            MDI.Docks_Size (Left) := MDI.Current_X - Drop_Area_Thickness;

            if MDI.Docks_Size (Left) >
              Gint (Get_Allocation_Width (MDI)) - MDI.Docks_Size (Right)
            then
               MDI.Docks_Size (Right) := Gint (Get_Allocation_Width (MDI)) -
                 MDI.Docks_Size (Left) - 2 * Handle_Size;
            end if;

         when Right =>
            MDI.Docks_Size (Right) := Gint (Get_Allocation_Width (MDI))
              - MDI.Current_X - Drop_Area_Thickness * 2;

            if MDI.Docks_Size (Left) >
              Gint (Get_Allocation_Width (MDI)) - MDI.Docks_Size (Right)
            then
               MDI.Docks_Size (Left) := Gint (Get_Allocation_Width (MDI)) -
                 MDI.Docks_Size (Right) - 2 * Handle_Size;
            end if;

         when Top =>
            MDI.Docks_Size (Top) := MDI.Current_Y - Drop_Area_Thickness * 2;

            if MDI.Docks_Size (Top) >
              Gint (Get_Allocation_Height (MDI)) - MDI.Docks_Size (Bottom)
            then
               MDI.Docks_Size (Bottom) := Gint (Get_Allocation_Height (MDI)) -
                 MDI.Docks_Size (Top) - 2 * Handle_Size;
            end if;

         when Bottom =>
            MDI.Docks_Size (Bottom) := Gint (Get_Allocation_Height (MDI)) -
              MDI.Current_Y - Drop_Area_Thickness;

            if MDI.Docks_Size (Top) >
              Gint (Get_Allocation_Height (MDI)) - MDI.Docks_Size (Bottom)
            then
               MDI.Docks_Size (Top) := Gint (Get_Allocation_Height (MDI)) -
                 MDI.Docks_Size (Bottom) - 2 * Handle_Size;
            end if;

         when None =>
            null;
      end case;

      --  Make sure the size is at least one, or the handle will completely
      --  disappear and the user will not have access to the window any more

      MDI.Docks_Size (MDI.Selected) :=
        Gint'Max (1, MDI.Docks_Size (MDI.Selected));

      Compute_Docks_Size (MDI);
      Queue_Resize (MDI);
   end Resize_Docks_From_Mouse;

   ------------------------
   -- Button_Pressed_MDI --
   ------------------------

   function Button_Pressed_MDI
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      M      : MDI_Window := MDI_Window (MDI);
      Cursor : Gdk_Cursor;
      Tmp    : Gdk_Grab_Status;
      Win_X  : Gint;
      Win_Y  : Gint;

   begin
      if Get_Button (Event) /= 1 then
         return False;
      end if;

      M.Selected := None;

      for J in Left .. Bottom loop
         if Get_Window (Event) = M.Handles (J) then
            M.Selected := J;
         end if;
      end loop;

      if M.Selected = None then
         return False;
      end if;

      Gdk_New (Cursor, Cross);
      Tmp := Pointer_Grab
        (M.Handles (M.Selected),
         False,
         Button_Press_Mask or Button_Motion_Mask or Button_Release_Mask,
         Cursor => Cursor,
         Time   => 0);
      Destroy (Cursor);

      Get_Position (M.Handles (M.Selected), Win_X, Win_Y);

      case M.Selected is
         when Left | Right =>
            M.Current_X := Gint (Get_X (Event)) + Win_X;
            M.Current_W := M.Current_X;
            M.Current_Y := Drop_Area_Thickness;
            M.Current_H := Gint (Get_Allocation_Height (M))
              - Drop_Area_Thickness;

         when Top | Bottom =>
            M.Current_X := Drop_Area_Thickness;
            M.Current_W := Gint (Get_Allocation_Width (M))
              - Drop_Area_Thickness;
            M.Current_Y := Gint (Get_Y (Event)) + Win_Y;
            M.Current_H := M.Current_Y;

         when None =>
            null;
      end case;

      if not M.Opaque_Docks then
         Draw_Line
           (Get_Window (MDI),
            M.Xor_GC, M.Current_X, M.Current_Y, M.Current_W, M.Current_H);
      end if;

      return False;
   end Button_Pressed_MDI;

   ------------------------
   -- Button_Release_MDI --
   ------------------------

   function Button_Release_MDI
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      M : MDI_Window := MDI_Window (MDI);
   begin
      if M.Selected = None then
         return False;
      end if;

      if not M.Opaque_Docks then
         Draw_Line
           (Get_Window (MDI),
            M.Xor_GC, M.Current_X, M.Current_Y, M.Current_W, M.Current_H);
      end if;

      Resize_Docks_From_Mouse (M);

      Pointer_Ungrab (Time => 0);
      M.Selected := None;
      return False;
   end Button_Release_MDI;

   -----------------------
   -- Button_Motion_MDI --
   -----------------------

   function Button_Motion_MDI
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      M            : MDI_Window := MDI_Window (MDI);
      X, Y         : Gint;
      Win_X, Win_Y : Gint;

   begin
      if M.Selected = None then
         return False;
      end if;

      if not M.Opaque_Docks then
         Draw_Line
           (Get_Window (MDI),
            M.Xor_GC, M.Current_X, M.Current_Y, M.Current_W, M.Current_H);
      end if;

      if Get_Window (Event) /= Get_Window (M) then
         Get_Pointer (M, X, Y);
      else
         Get_Position (M.Handles (M.Selected), Win_X, Win_Y);
         X := Gint (Get_X (Event)) + Win_X;
         Y := Gint (Get_Y (Event)) + Win_Y;
      end if;

      case M.Selected is
         when Left | Right =>
            M.Current_X := X;
            M.Current_W := X;

         when Top | Bottom =>
            M.Current_Y := Y;
            M.Current_H := Y;

         when None =>
            null;
      end case;

      if M.Opaque_Docks then
         Resize_Docks_From_Mouse (M);
      else
         Draw_Line
           (Get_Window (MDI),
            M.Xor_GC, M.Current_X, M.Current_Y, M.Current_W, M.Current_H);
      end if;

      return False;
   end Button_Motion_MDI;

   --------------------
   -- Button_Pressed --
   --------------------

   function Button_Pressed
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      C      : constant MDI_Child := MDI_Child (Child);
      MDI    : MDI_Window := C.MDI;
      Cursor : Gdk.Cursor.Gdk_Cursor;
      Tmp    : Gdk_Grab_Status;
      Curs   : Gdk_Cursor_Type;

   begin
      --  It sometimes happens that widgets let events pass through (for
      --  instance scrollbars do that), and thus wouldn't be useable anymore
      --  if we do a grab.

      if Get_Window (Child) /= Get_Window (Event) then
         return False;
      end if;

      --  Double-click in the title bar of a child in the main area should
      --  maximize or unmaximize the children

      if Get_Event_Type (Event) = Gdk_2button_Press then
         if Gint (Get_Y (Event)) <= MDI.Title_Bar_Height
           and then (C.State = Normal
                     or else C.State = Iconified
                     or else (C.State = Docked and then C.Dock = None))
         then
            Maximize_Children (MDI, not MDI.Children_Are_Maximized);
         end if;

         return False;
      end if;

      if Get_Event_Type (Event) /= Button_Press then
         return False;
      end if;

      if Get_Button (Event) = 3 then
         Lower_Child (C);
         return False;
      elsif Get_Button (Event) /= 1 then
         return False;
      end if;

      Set_Focus_Child (C);
      Raise_Child (C);
      --  not done by Set_Focus_Child if the child already had the focus.

      MDI.X_Root := Gint (Get_X_Root (Event));
      MDI.Y_Root := Gint (Get_Y_Root (Event));

      --  Can't move items inside a notebook
      if C.State = Docked
        or else (C.State = Normal and then MDI.Children_Are_Maximized)
      then
         return False;
      end if;

      MDI.Selected_Child := C;

      MDI.Initial_Width := Gint (Get_Allocation_Width (Child));
      MDI.Initial_Height := Gint (Get_Allocation_Height (Child));
      MDI.Current_W := MDI.Initial_Width;
      MDI.Current_H := MDI.Initial_Height;
      MDI.Current_X := C.X;
      MDI.Current_Y := C.Y;

      Curs := Side (C, Gint (Get_X (Event)), Gint (Get_Y (Event)));
      MDI.Current_Cursor := Curs;

      if C.State = Iconified
        and then Curs /= Left_Ptr
      then
         MDI.Selected_Child := null;
         return False;
      end if;

      Gdk_New (Cursor, Curs);
      Tmp := Pointer_Grab
        (Get_Window (C),
         False,
         Button_Press_Mask or Button_Motion_Mask or Button_Release_Mask,
         Cursor => Cursor,
         Time => 0);
      Destroy (Cursor);

      if not MDI.Children_Are_Maximized
        and then
        ((not MDI.Opaque_Resize and then MDI.Current_Cursor /= Left_Ptr)
         or else (not MDI.Opaque_Move and then MDI.Current_Cursor = Left_Ptr))
      then
         Draw_Rectangle
           (Get_Window (MDI.Layout),
            MDI.Xor_GC,
            Filled => False,
            X => MDI.Current_X,
            Y => MDI.Current_Y,
            Width => MDI.Current_W,
            Height => MDI.Current_H);
      end if;

      return True;
   end Button_Pressed;

   --------------------
   -- Button_Release --
   --------------------

   function Button_Release
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      MDI   : MDI_Window := MDI_Child (Child).MDI;
      Alloc : Gtk_Allocation;

      Buttons_Width : constant := 100;
      --  Approximative width of the three title bar buttons

      Minimal : constant := 10;

   begin
      if Get_Window (Child) /= Get_Window (Event)
        or else MDI.Selected_Child = null
      then
         return False;
      end if;

      Pointer_Ungrab (Time => 0);

      Alloc :=
        (MDI.Current_X, MDI.Current_Y,
         Allocation_Int (MDI.Current_W), Allocation_Int (MDI.Current_H));

      if Alloc.X + Alloc.Width < Buttons_Width then
         Alloc.X := Buttons_Width - Alloc.Width;
      elsif Alloc.X > Get_Allocation_Width (MDI.Layout) - Minimal then
         Alloc.X := Get_Allocation_Width (MDI.Layout) - Minimal;
      end if;

      if Alloc.Y + MDI.Title_Bar_Height < Minimal then
         Alloc.Y := Minimal - MDI.Title_Bar_Height;
      elsif Alloc.Y > Get_Allocation_Height (MDI.Layout) - Minimal then
         Alloc.Y := Get_Allocation_Height (MDI.Layout) - Minimal;
      end if;

      if not MDI.Children_Are_Maximized
        and then ((not MDI.Opaque_Resize
                    and then MDI.Current_Cursor /= Left_Ptr)
          or else (not MDI.Opaque_Move and then MDI.Current_Cursor = Left_Ptr))
      then
         Draw_Rectangle
           (Get_Window (MDI.Layout),
            MDI.Xor_GC,
            Filled => False,
            X => Alloc.X,
            Y => Alloc.Y,
            Width => Gint (Alloc.Width),
            Height => Gint (Alloc.Height));
         Size_Allocate (Child, Alloc);
      end if;

      MDI_Child (Child).X := Alloc.X;
      MDI_Child (Child).Y := Alloc.Y;

      if MDI.Current_Cursor /= Left_Ptr then
         MDI_Child (Child).Uniconified_Width  := Gint (Alloc.Width);
         MDI_Child (Child).Uniconified_Height := Gint (Alloc.Height);
      end if;

      MDI.Selected_Child := null;
      return True;
   end Button_Release;

   -------------------
   -- Button_Motion --
   -------------------

   function Button_Motion
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      C       : constant MDI_Child := MDI_Child (Child);
      MDI     : MDI_Window := C.MDI;
      Delta_X : Gint;
      Delta_Y : Gint;
      Cursor  : Gdk_Cursor;
      Curs    : Gdk_Cursor_Type;
      W, H    : Gint;
      Alloc   : Gtk_Allocation;
      Min_Height : constant Gint :=
        2 * Border_Thickness + MDI.Title_Bar_Height;

   begin
      if Get_Window (Child) /= Get_Window (Event) then
         return False;
      end if;

      --  Do we have a drag-and-drop operation ? This is true if we are
      --  pressing control, or simply clicking in a maximized or docked
      --  child (otherwise, moving items in the layout would interfer with
      --  dnd).

      if MDI.X_Root /= -1
        and then (Get_State (Event) and Button1_Mask) /= 0
        and then ((Get_State (Event) and Control_Mask) /= 0
                  or else (C.State /= Normal and then C.State /= Iconified)
                  or else C.MDI.Children_Are_Maximized)
        and then Gtk.Dnd.Check_Threshold
          (C, MDI.X_Root, MDI.Y_Root, Gint (Get_X_Root (Event)),
           Gint (Get_Y_Root (Event)))
      then
         declare
            List : Target_List := Target_List_New (Source_Target_Table);
         begin
            Set_Icon_Default
              (Gtk.Dnd.Drag_Begin
               (Widget  => C,
                Targets => List,
                Actions => Action_Copy,
                Button  => 1,
                Event   => Event));
            Target_List_Unref (List);

            --  Avoid any further standard moving event
            MDI.X_Root := -1;
         end;
      end if;

      --  A button_motion event ?

      if (Get_State (Event) and Button1_Mask) /= 0
        and then MDI.Selected_Child /= null
        and then MDI.X_Root /= -1
      then
         if not MDI.Children_Are_Maximized
           and then
           ((not MDI.Opaque_Resize and then MDI.Current_Cursor /= Left_Ptr)
            or else (not MDI.Opaque_Move
                     and then MDI.Current_Cursor = Left_Ptr))
         then
            Draw_Rectangle
              (Get_Window (MDI.Layout),
               MDI.Xor_GC,
               Filled => False,
               X => MDI.Current_X,
               Y => MDI.Current_Y,
               Width => MDI.Current_W,
               Height => MDI.Current_H);
         end if;

         Delta_X := Gint (Get_X_Root (Event)) - MDI.X_Root;
         Delta_Y := Gint (Get_Y_Root (Event)) - MDI.Y_Root;
         W := MDI.Initial_Width;
         H := MDI.Initial_Height;

         MDI.Current_X := C.X;
         MDI.Current_Y := C.Y;

         case MDI.Current_Cursor is
            when Left_Ptr =>
               MDI.Current_X := Delta_X + C.X;
               MDI.Current_Y := Delta_Y + C.Y;

            when Left_Side =>
               W := Gint'Max (Min_Width, W - Delta_X);
               MDI.Current_X := C.X + Delta_X;

            when Right_Side =>
               W := Gint'Max (Min_Width, W + Delta_X);

            when Top_Side =>
               H := Gint'Max (Min_Height, H - Delta_Y);
               MDI.Current_Y := C.Y + Delta_Y;

            when Bottom_Side =>
               H := Gint'Max (Min_Height, H + Delta_Y);

            when Top_Left_Corner =>
               W := Gint'Max (Min_Width, W - Delta_X);
               H := Gint'Max (Min_Height, H - Delta_Y);
               MDI.Current_X := C.X + Delta_X;
               MDI.Current_Y := C.Y + Delta_Y;

            when Top_Right_Corner =>
               W := Gint'Max (Min_Width, W + Delta_X);
               H := Gint'Max (Min_Height, H - Delta_Y);
               MDI.Current_Y := C.Y + Delta_Y;

            when Bottom_Left_Corner =>
               W := Gint'Max (Min_Width, W - Delta_X);
               H := Gint'Max (Min_Height, H + Delta_Y);
               MDI.Current_X := C.X + Delta_X;

            when Bottom_Right_Corner =>
               W := Gint'Max (Min_Width, W + Delta_X);
               H := Gint'Max (Min_Height, H + Delta_Y);
            when others => null;
         end case;

         if MDI.Opaque_Move or else MDI.Opaque_Resize then
            if MDI.Current_Cursor /= Left_Ptr then
               MDI.Current_W := W;
               MDI.Current_H := H;

               --  Need to set these, or when the mouse is outside of the
               --  layout, the MDI will try to resize the child to the old
               --  dimensions even while the mouse is moving.
               C.Uniconified_Width  := W;
               C.Uniconified_Height := H;
            end if;

            Alloc :=
              (MDI.Current_X, MDI.Current_Y,
               Allocation_Int (MDI.Current_W),
               Allocation_Int (MDI.Current_H));
            Size_Allocate (Child, Alloc);

         elsif not MDI.Children_Are_Maximized then
            MDI.Current_W := W;
            MDI.Current_H := H;
            Draw_Rectangle
              (Get_Window (MDI.Layout),
               MDI.Xor_GC,
               Filled => False,
               X      => MDI.Current_X,
               Y      => MDI.Current_Y,
               Width  => MDI.Current_W,
               Height => MDI.Current_H);
         end if;

      --  A motion_event ? change the cursor if needed

      elsif C.State = Normal
        and then not MDI.Children_Are_Maximized
      then
         Delta_X := Gint (Get_X (Event));
         Delta_Y := Gint (Get_Y (Event));
         Curs := Side (C, Delta_X, Delta_Y);

         if Curs /= MDI.Current_Cursor then
            MDI.Current_Cursor := Curs;
            if Curs = Left_Ptr then
               Gdk.Window.Set_Cursor (Get_Window (Child), null);
            else
               Gdk_New (Cursor, MDI.Current_Cursor);
               Gdk.Window.Set_Cursor (Get_Window (Child), Cursor);
               Destroy (Cursor);
            end if;
         end if;
      end if;

      return True;
   end Button_Motion;

   -----------------
   -- Leave_Child --
   -----------------

   function Leave_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      MDI    : MDI_Window := MDI_Child (Child).MDI;
   begin
      if Get_State (Event) = 0
        and then MDI.Current_Cursor /= Left_Ptr
      then
         MDI.Current_Cursor := Left_Ptr;
         Gdk.Window.Set_Cursor (Get_Window (Child), null);
      end if;

      return False;
   end Leave_Child;

   ----------
   -- Side --
   ----------

   function Side
     (Child : access MDI_Child_Record'Class; X, Y  : Gint)
      return Gdk_Cursor_Type
   is
      X_Side, Y_Side : Gint;
   begin
      if X <= Border_Thickness then
         X_Side := -2;
      elsif X <= Corner_Size then
         X_Side := -1;
      elsif X >= Gint (Get_Allocation_Width (Child)) - Border_Thickness then
         X_Side := 2;
      elsif X >= Gint (Get_Allocation_Width (Child)) - Corner_Size then
         X_Side := 1;
      else
         X_Side := 0;
      end if;

      if Y <= Border_Thickness then
         Y_Side := -2;
      elsif Y <= Corner_Size then
         Y_Side := -1;
      elsif Y >= Gint (Get_Allocation_Height (Child)) - Border_Thickness then
         Y_Side := 2;
      elsif Y >= Gint (Get_Allocation_Height (Child)) - Corner_Size then
         Y_Side := 1;
      else
         Y_Side := 0;
      end if;

      if X_Side <= -1 and then Y_Side <= -1 then
         return Top_Left_Corner;

      elsif X_Side <= -1 and then Y_Side >= 1 then
         return Bottom_Left_Corner;

      elsif X_Side >= 1 and then Y_Side <= -1 then
         return Top_Right_Corner;

      elsif X_Side >= 1 and then Y_Side >= 1 then
         return Bottom_Right_Corner;

      elsif X_Side = -2 and then Y_Side in -1 .. 1 then
         return Left_Side;

      elsif X_Side = 2 and then Y_Side in -1 .. 1 then
         return Right_Side;

      elsif Y_Side = -2 and then X_Side in -1 .. 1 then
         return Top_Side;

      elsif Y_Side = 2 and then X_Side in -1 .. 1 then
         return Bottom_Side;
      end if;

      return Left_Ptr;
   end Side;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child   : out MDI_Child;
      Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags : Child_Flags := All_Buttons) is
   begin
      Child := new MDI_Child_Record;
      Initialize (Child, Widget, Flags);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Child   : access MDI_Child_Record;
      Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags   : Child_Flags)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => GType_None),
         2 => (1 => GType_None),
         3 => (1 => GType_None));
      Button    : Gtk_Button;
      Box       : Gtk_Box;
      Pix       : Gdk_Pixmap;
      Mask      : Gdk_Bitmap;
      Pixmap    : Gtk_Pixmap;
      Event     : Gtk_Event_Box;

   begin
      if Widget.all in Gtk_Window_Record'Class then
         raise Program_Error;
      end if;

      Gtk.Event_Box.Initialize (Child);

      Gtk.Object.Initialize_Class_Record
        (Child,
         Signals      => Child_Signals,
         Class_Record => Child_Class_Record,
         Type_Name    => "GtkAdaMDIChild",
         Parameters   => Signal_Parameters);

      Widget_Callback.Connect
        (Child, "drag_data_get", Source_Drag_Data_Get'Access);

      Child.Initial := Gtk_Widget (Widget);
      Child.Uniconified_Width := -1;
      Set_Flags (Child, App_Paintable);

      Child.State := Normal;
      Child.Flags := Flags;

      Add_Events
        (Child, Button_Press_Mask
           or Button_Motion_Mask
           or Button_Release_Mask
           or Pointer_Motion_Mask);
      Return_Callback.Connect
        (Child, "button_press_event",
         Return_Callback.To_Marshaller (Button_Pressed'Access));
      Return_Callback.Connect
        (Child, "button_release_event",
         Return_Callback.To_Marshaller (Button_Release'Access));
      Return_Callback.Connect
        (Child, "motion_notify_event",
         Return_Callback.To_Marshaller (Button_Motion'Access));
      Widget_Callback.Connect
        (Child, "destroy",
         Widget_Callback.To_Marshaller (Destroy_Child'Access));
      Return_Callback.Connect
        (Child, "leave_notify_event",
         Return_Callback.To_Marshaller (Leave_Child'Access));
      Return_Callback.Connect
        (Child, "expose_event",
         Return_Callback.To_Marshaller (Draw_Child'Access), After => True);

      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Add (Child, Box);

      --  Buttons in the title bar

      Gtk_New_Hbox (Child.Title_Box, Homogeneous => False);
      Pack_Start (Box, Child.Title_Box, Expand => False, Fill => False);

      Set_Border_Width (Box, Guint (Border_Thickness));

      if (Flags and Destroy_Button) /= 0 then
         Gdk.Pixmap.Create_From_Xpm_D
           (Pix, null, Get_Default_Colormap, Mask, Null_Color, Close_Xpm);
         Gtk_New (Pixmap, Pix, Mask);
         Gtk_New (Button);
         Add (Button, Pixmap);
         Pack_End (Child.Title_Box, Button, Expand => False, Fill => False);
         Widget_Callback.Object_Connect
           (Button, "clicked",
            Widget_Callback.To_Marshaller (Internal_Close_Child'Access),
            Child);
      end if;

      if (Flags and Maximize_Button) /= 0 then
         Gdk.Pixmap.Create_From_Xpm_D
           (Pix, null, Get_Default_Colormap, Mask, Null_Color, Maximize_Xpm);
         Gtk_New (Pixmap, Pix, Mask);
         Gtk_New (Child.Maximize_Button);
         Add (Child.Maximize_Button, Pixmap);
         Pack_End
           (Child.Title_Box, Child.Maximize_Button,
            Expand => False, Fill => False);
         Widget_Callback.Object_Connect
           (Child.Maximize_Button, "clicked",
            Widget_Callback.To_Marshaller (Maximize_Child_Cb'Access), Child);
      end if;

      if (Flags and Iconify_Button) /= 0 then
         Gdk.Pixmap.Create_From_Xpm_D
           (Pix, null, Get_Default_Colormap, Mask, Null_Color, Iconify_Xpm);
         Gtk_New (Pixmap, Pix, Mask);
         Gtk_New (Child.Minimize_Button);
         Add (Child.Minimize_Button, Pixmap);
         Pack_End (Child.Title_Box, Child.Minimize_Button, Expand
                   => False, Fill => False);
         Widget_Callback.Object_Connect
           (Child.Minimize_Button, "clicked",
            Widget_Callback.To_Marshaller (Iconify_Child'Access), Child);
      end if;

      Gtk_New (Event);
      Pack_Start (Box, Event, Expand => True, Fill => True, Padding => 0);

      --  The child widget

      Add (Event, Widget);

      Widget_Callback.Object_Connect
        (Child.Initial, "destroy",
         Widget_Callback.To_Marshaller (Destroy_Initial_Child'Access),
         Child);
   end Initialize;

   -------------------------
   -- Give_Focus_To_Child --
   -------------------------

   procedure Give_Focus_To_Child (Widget : access Gtk_Widget_Record'Class) is
   begin
      Grab_Focus (Widget);
   end Give_Focus_To_Child;

   ---------
   -- Put --
   ---------

   function Put
     (MDI   : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags : Child_Flags := All_Buttons;
      Focus_Widget : Gtk.Widget.Gtk_Widget := null) return MDI_Child
   is
      C           : MDI_Child;
      Requisition : Gtk_Requisition;

   begin
      if Child.all in MDI_Child_Record'Class then
         C := MDI_Child (Child);
      else
         Gtk_New (C, Child, Flags);
      end if;

      C.MDI := MDI_Window (MDI);
      C.X   := MDI.Default_X;
      C.Y   := MDI.Default_Y;

      Set_USize (C.Title_Box, -1, MDI.Title_Bar_Height);

      if not MDI.Children_Are_Maximized
        and then MDI.Default_X + Threshold >
        Gint (Get_Allocation_Width (MDI.Layout))
      then
         MDI.Default_X := 10;
      else
         MDI.Default_X := MDI.Default_X + 10;
      end if;

      if not MDI.Children_Are_Maximized
        and then MDI.Default_Y + Threshold >
        Gint (Get_Allocation_Height (MDI.Layout))
      then
         MDI.Default_Y := 10;
      else
         MDI.Default_Y := MDI.Default_Y + 10;
      end if;

      C.Title       := new String'(" ");
      C.Short_Title := new String'(" ");

      --  We need to show the widget before inserting it in a notebook,
      --  otherwise the notebook page will not be made visible.

      Show_All (C);

      Widget_List.Prepend (MDI.Items, Gtk_Widget (C));

      --  Set the default size request for C to that of Child.
      Size_Request (Child, Requisition);
      Set_Size_Request (C, Requisition.Width, Requisition.Height);

      --  If all items are maximized, add Child to the notebook

      if MDI.All_Floating_Mode then
         Float_Child (C, True);
      elsif MDI.Children_Are_Maximized then
         Put_In_Notebook (MDI, None, C);
      else
         Put (MDI.Layout, C, 0, 0);
      end if;

      if MDI.Menu /= null then
         Create_Menu_Entry (C);
      end if;

      if Focus_Widget /= null then
         Widget_Callback.Object_Connect
           (C, "grab_focus",
            Widget_Callback.To_Marshaller (Give_Focus_To_Child'Access),
            Focus_Widget);
      end if;

      --  Restore the keyboard focus, which might have been stolen if the new
      --  child was added to a notebook.

      if MDI.Focus_Child /= null then
         Grab_Focus (MDI.Focus_Child);
      end if;

      return C;
   end Put;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title (Child : access MDI_Child_Record) return String is
   begin
      return Child.Title.all;
   end Get_Title;

   ---------------------
   -- Get_Short_Title --
   ---------------------

   function Get_Short_Title (Child : access MDI_Child_Record) return String is
   begin
      return Child.Short_Title.all;
   end Get_Short_Title;

   ----------------------
   -- Create_Menu_Item --
   ----------------------

   procedure Update_Menu_Item (Child : access MDI_Child_Record'Class) is
      Label : Gtk_Accel_Label;
      Pixmap : Gtk_Pixmap;
      Pix : Gdk_Pixmap;
      Mask : Gdk_Bitmap;
      Box  : Gtk_Box;
   begin
      if Child.Menu_Item /= null then
         if Get_Child (Child.Menu_Item) /= null then
            Remove (Child.Menu_Item, Get_Child (Child.Menu_Item));
         end if;

         Gtk_New_Hbox (Box, Homogeneous => False, Spacing => 5);

         if Child.Icon /= null then
            Render_Pixmap_And_Mask (Child.Icon, Pix, Mask, 128);
            Gtk_New (Pixmap, Pix, Mask);
            Pack_Start (Box, Pixmap, Expand => False);
         end if;

         Gtk_New (Label, Locale_To_UTF8 (Child.Short_Title.all));
         Set_Alignment (Label, 0.0, 0.5);
         Set_Accel_Widget (Label, Child.Menu_Item);
         Pack_Start (Box, Label,  Expand => True, Fill => True);

         Show_All (Box);
         Add (Child.Menu_Item, Box);

         Set_Accel_Path
           (Child.Menu_Item, "<gtkada>/window/child/"
            & Locale_To_UTF8 (Child.Short_Title.all),
            Child.MDI.Group);
      end if;
   end Update_Menu_Item;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
     (Child : access MDI_Child_Record;
      Icon  : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      Note : Gtk_Notebook;
   begin
      if Child.Icon /= null then
         Unref (Child.Icon);
      end if;
      Child.Icon := Icon;

      if Realized_Is_Set (Child) then
         Draw_Child (Child, Full_Area);
      end if;

      Update_Menu_Item (Child);
      Update_Tab_Label (Child);
   end Set_Icon;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Child       : access MDI_Child_Record;
      Title       : String;
      Short_Title : String := "")
   is
      The_Title       : String_Access;
      The_Short_Title : String_Access;
      --  Those pointers are used to prevent problems when
      --  the Title parameter is in fact Child.Title

      Label           : Gtk_Accel_Label;
   begin
      The_Title := new String'(Title);

      if Short_Title /= "" then
         The_Short_Title := new String'(Short_Title);
      else
         The_Short_Title := new String'(Title);
      end if;

      Free (Child.Title);
      Free (Child.Short_Title);

      Child.Title := The_Title;
      Child.Short_Title := The_Short_Title;

      if Child.State = Floating then
         Set_Title (Gtk_Window (Get_Toplevel (Child.Initial)),
                    Locale_To_UTF8 (Title));
      end if;

      Update_Tab_Label (Child);

      --  Update the menu, if it exists

      if Child.Menu_Item /= null then
         Update_Menu_Item (Child);

      --  Else in case the menu entry wasn't created before because there was
      --  no title yet, we just create it now.

      elsif Child.MDI.Menu /= null and then Title /= "" then
         Create_Menu_Entry (Child);
      end if;

      if Get_Window (Child) /= Null_Window then
         Queue_Draw (Child);
      end if;
   end Set_Title;

   --------------------
   -- Find_MDI_Child --
   --------------------

   function Find_MDI_Child
     (MDI    : access MDI_Window_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return MDI_Child
   is
      use Widget_List;

      Tmp : Widget_List.Glist;
   begin
      Tmp := First (MDI.Items);

      while Tmp /= Null_List loop
         if MDI_Child (Get_Data (Tmp)).Initial = Gtk_Widget (Widget) then
            return MDI_Child (Get_Data (Tmp));
         end if;

         Tmp := Next (Tmp);
      end loop;

      return null;
   end Find_MDI_Child;

   ---------------------------
   -- Find_MDI_Child_By_Tag --
   ---------------------------

   function Find_MDI_Child_By_Tag
     (MDI    : access MDI_Window_Record;
      Tag    : Ada.Tags.Tag)
     return MDI_Child
   is
      Child : MDI_Child;
      Iter  : Child_Iterator := First_Child (MDI);
   begin
      loop
         Child := Get (Iter);
         exit when Child = null or else Child.Initial'Tag = Tag;
         Next (Iter);
      end loop;

      return Get (Iter);
   end Find_MDI_Child_By_Tag;

   ---------------------------
   -- Find_MDI_Child_By_Name --
   ---------------------------

   function Find_MDI_Child_By_Name
     (MDI    : access MDI_Window_Record;
      Name   : String)
     return MDI_Child
   is
      Child : MDI_Child;
      Iter  : Child_Iterator := First_Child (MDI);
   begin
      loop
         Child := Get (Iter);
         exit when Child = null
           or else Child.Title.all = Name;
         Next (Iter);
      end loop;

      return Get (Iter);
   end Find_MDI_Child_By_Name;

   -----------------
   -- Lower_Child --
   -----------------

   procedure Lower_Child (Child : access MDI_Child_Record'Class) is
      Num : Gint;
   begin
      Ref (Child);
      Remove (Child.MDI.Items, Gtk_Widget (Child));
      Append (Child.MDI.Items, Gtk_Widget (Child));
      Unref (Child);

      --  For an docked item, we in fact want to raise its parent dock,
      --  and make sure the current page in that dock is the correct one.

      if Child.State = Docked then
         Num := Page_Num (Child.MDI.Docks (Child.Dock), Child) + 1;
         if Get_Nth_Page (Child.MDI.Docks (Child.Dock), Num) = null then
            Set_Current_Page (Child.MDI.Docks (Child.Dock), 0);
         else
            Set_Current_Page (Child.MDI.Docks (Child.Dock), Num);
         end if;

      elsif Child.State = Normal
        and then Child.MDI.Children_Are_Maximized
      then
         Set_Current_Page
           (Child.MDI.Docks (None), Page_Num (Child.MDI.Docks (None), Child));

      elsif Realized_Is_Set (Child) then
         Gdk.Window.Lower (Get_Window (Child));

         if Child.State = Floating then
            Gdk.Window.Lower
              (Get_Window (Gtk_Window (Get_Toplevel (Child.Initial))));
         end if;
      end if;

   end Lower_Child;

   ----------------------
   -- Raise_Child_Idle --
   ----------------------

   function Raise_Child_Idle (Data : Raise_Idle_Data) return Boolean is
      Child : MDI_Child := Data.Child;
   begin
      if Child = null then
         return False;
      end if;

      Ref (Child);
      Remove (Child.MDI.Items, Gtk_Widget (Child));
      Prepend (Child.MDI.Items, Gtk_Widget (Child));
      Unref (Child);

      --  We want to be able to raise children without giving them the focus.
      Child.MDI.Prevent_Focus_On_Page_Switch := True;

      --  For an docked item, we in fact want to raise its parent dock,
      --  and make sure the current page in that dock is the correct one.

      if Child.State = Docked then
         Set_Current_Page
           (Child.MDI.Docks (Child.Dock),
            Page_Num (Child.MDI.Docks (Child.Dock), Child));

      elsif Child.State = Normal
        and then Child.MDI.Children_Are_Maximized
      then
         Set_Current_Page
           (Child.MDI.Docks (None),
            Page_Num (Child.MDI.Docks (None), Child));

      elsif Child.State = Floating
        and then Realized_Is_Set (Child.Initial)
      then
         declare
            Win : constant Gtk_Window :=
              Gtk_Window (Get_Toplevel (Child.Initial));
         begin
            Gtk.Window.Deiconify (Win);
            Gdk.Window.Gdk_Raise (Get_Window (Win));
         end;

      elsif Realized_Is_Set (Child) then
         Gdk.Window.Gdk_Raise (Get_Window (Child));
      end if;

      Child.MDI.Prevent_Focus_On_Page_Switch := False;

      --  Give the focus to the Focus_Child, since the notebook page switch
      --  might have changed that.

      if Child.MDI.Focus_Child /= null then
         Grab_Focus (Child.MDI.Focus_Child);
      end if;

      return False;
   end Raise_Child_Idle;

   -----------------
   -- Raise_Child --
   -----------------

   procedure Raise_Child (Child : access MDI_Child_Record'Class) is
      Tmp : Boolean;
   begin
      Tmp := Raise_Child_Idle ((Child.MDI, MDI_Child (Child)));
   end Raise_Child;

   ----------------------
   -- Update_Dock_Menu --
   ----------------------

   procedure Update_Dock_Menu (Child : access MDI_Child_Record'Class) is
   begin
      if Child.MDI.Dock_Menu_Item /= null then
         Gtk.Handlers.Handler_Block
           (Child.MDI.Dock_Menu_Item, Child.MDI.Dock_Menu_Item_Id);
         Set_Active (Child.MDI.Dock_Menu_Item, Child.State = Docked);
         Set_Sensitive (Child.MDI.Dock_Menu_Item, Child.Dock /= None);
         Gtk.Handlers.Handler_Unblock
           (Child.MDI.Dock_Menu_Item, Child.MDI.Dock_Menu_Item_Id);
      end if;
   end Update_Dock_Menu;

   -----------------------
   -- Update_Float_Menu --
   -----------------------

   procedure Update_Float_Menu (Child : access MDI_Child_Record'Class) is
   begin
      if Child.MDI.Float_Menu_Item /= null then
         Gtk.Handlers.Handler_Block
           (Child.MDI.Float_Menu_Item, Child.MDI.Float_Menu_Item_Id);
         Set_Active (Child.MDI.Float_Menu_Item, Child.State = Floating);
         Gtk.Handlers.Handler_Unblock
           (Child.MDI.Float_Menu_Item, Child.MDI.Float_Menu_Item_Id);
      end if;
   end Update_Float_Menu;

   ------------------------------
   -- Destroy_Raise_Child_Idle --
   ------------------------------

   procedure Destroy_Raise_Child_Idle (D : in out Raise_Idle_Data) is
   begin
      D.MDI.Raise_Id := 0;
   end Destroy_Raise_Child_Idle;

   ---------------------
   -- Set_Focus_Child --
   ---------------------

   procedure Set_Focus_Child
     (Child       : access MDI_Child_Record'Class;
      Force_Focus : Boolean := True)
   is
      procedure Emit_By_Name_Child
        (Object : System.Address; Name : String; Child : System.Address);
      pragma Import (C, Emit_By_Name_Child, "g_signal_emit_by_name");

      Old : constant MDI_Child := Child.MDI.Focus_Child;
      C   : constant MDI_Child := MDI_Child (Child);

   begin
      --  Be lazy. And avoid infinite loop when updating the MDI menu...

      if C = Old
        or else Child.MDI.Prevent_Focus_On_Page_Switch
      then
         return;
      end if;

      Child.MDI.Focus_Child := C;

      --  Make sure the page containing Child in a notebook is put on top.
      --  The actual raise is done in an idle loop. Otherwise, if the child
      --  hasn't been properly resized yet, there would be a lot of
      --  flickering.

      if Child.MDI.Raise_Id /= 0 then
         Idle_Remove (Child.MDI.Raise_Id);
      end if;

      Child.MDI.Raise_Id :=
        Widget_Idle.Add (Raise_Child_Idle'Access, (Child.MDI, C),
                         Destroy => Destroy_Raise_Child_Idle'Access);

      if Old /= null
        and then Realized_Is_Set (Old)
      then
         Queue_Draw_Area
           (Old, Border_Thickness, Border_Thickness,
            Gint (Get_Allocation_Width (Old)) - 2 * Border_Thickness,
            Child.MDI.Title_Bar_Height);
      end if;

      if Realized_Is_Set (C) then
         Queue_Draw_Area
           (C, Border_Thickness, Border_Thickness,
            Gint (Get_Allocation_Width (C)) - 2 * Border_Thickness,
            Child.MDI.Title_Bar_Height);
      end if;

      Update_Dock_Menu (C);
      Update_Float_Menu (C);

      if C.MDI.Close_Menu_Item /= null then
         Set_Sensitive
           (C.MDI.Close_Menu_Item, (C.Flags and Destroy_Button) /= 0);
      end if;

      if C.Menu_Item /= null
        and then not Get_Active (C.Menu_Item)
      then
         Set_Active (C.Menu_Item, True);
      end if;

      --  It would be nice to find the first child of C.Initial that
      --  accepts the keyboard focus. However, in the meantime, we at least
      --  want to make sure that no other widget has the focus. As a result,
      --  focus_in events will always be sent the next time the user selects a
      --  widget.
      if Force_Focus then
         Grab_Focus (C);
      end if;

      Highlight_Child (C, False);

      Widget_Callback.Emit_By_Name (C, "selected");
      Emit_By_Name_Child (Get_Object (C.MDI), "child_selected" & ASCII.NUL,
                          Get_Object (C));
   end Set_Focus_Child;

   ----------------------
   -- Cascade_Children --
   ----------------------

   procedure Cascade_Children (MDI : access MDI_Window_Record) is
      use type Widget_List.Glist;
      Level        : Gint := 1;
      W, H         : Gint;
      List         : Widget_List.Glist := First (MDI.Items);
      C            : MDI_Child;
      Num_Children : Gint := 0;
      Alloc        : Gtk_Allocation;

   begin
      Maximize_Children (MDI, False);

      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));

         if C.State = Iconified then
            Minimize_Child (C, False);
         end if;

         if C.State = Normal then
            Num_Children := Num_Children + 1;
         end if;

         List := Widget_List.Next (List);
      end loop;

      Compute_Size (MDI, None, Alloc, Is_Handle => False);
      W := Gint (Alloc.Width)  - (Num_Children - 1) * MDI.Title_Bar_Height;
      H := Gint (Alloc.Height) - (Num_Children - 1) * MDI.Title_Bar_Height;

      List := First (MDI.Items);

      --  Resize all children, except the one that has the focus (since
      --  we want it to be on top). Note that the list is traverse from the
      --  top-most child to the bottom-most one.

      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));
         List := Widget_List.Next (List);

         if C.State = Normal then
            C.X := (Num_Children - Level) * MDI.Title_Bar_Height;
            C.Y := C.X;
            C.Uniconified_Width  := W;
            C.Uniconified_Height := H;
            Alloc := (C.X, C.Y, Allocation_Int (W), Allocation_Int (H));
            Size_Allocate (C, Alloc);
            Level := Level + 1;
         end if;
      end loop;
   end Cascade_Children;

   -----------------------
   -- Tile_Horizontally --
   -----------------------

   procedure Tile_Horizontally (MDI : access MDI_Window_Record) is
      use type Widget_List.Glist;

      Level        : Gint := 0;
      W, H         : Gint;
      List         : Widget_List.Glist := First (MDI.Items);
      C            : MDI_Child;
      Num_Children : Gint := 0;
      Alloc        : Gtk_Allocation;
      Max_W, Max_H : Gint;

   begin
      if MDI.Children_Are_Maximized then
         Max_W := Gint (Get_Allocation_Width (MDI.Docks (None)));
         Max_H := Gint (Get_Allocation_Height (MDI.Docks (None)));
         Maximize_Children (MDI, False);

      else
         Max_W := Gint (Get_Allocation_Width (MDI.Layout));
         Max_H := Gint (Get_Allocation_Height (MDI.Layout));
      end if;

      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));

         if C.State = Normal then
            Num_Children := Num_Children + 1;
         end if;

         List := Widget_List.Next (List);
      end loop;

      W := Max_W / Num_Children;
      H := Max_H;

      List := First (MDI.Items);

      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));
         List := Widget_List.Next (List);

         if C.State = Normal then
            C.X := Level;
            C.Y := 0;
            C.Uniconified_Width := W;
            C.Uniconified_Height := H;
            Alloc := (C.X, C.Y,
                      Allocation_Int (C.Uniconified_Width),
                      Allocation_Int (C.Uniconified_Height));
            Size_Allocate (C, Alloc);
            Level := Level + W;
         end if;
      end loop;
   end Tile_Horizontally;

   ---------------------
   -- Tile_Vertically --
   ---------------------

   procedure Tile_Vertically (MDI : access MDI_Window_Record) is
      use type Widget_List.Glist;

      Level        : Gint := 0;
      W, H         : Gint;
      List         : Widget_List.Glist := First (MDI.Items);
      C            : MDI_Child;
      Num_Children : Gint := 0;
      Alloc        : Gtk_Allocation;
      Max_W, Max_H : Gint;

   begin
      if MDI.Children_Are_Maximized then
         Max_W := Gint (Get_Allocation_Width (MDI.Docks (None)));
         Max_H := Gint (Get_Allocation_Height (MDI.Docks (None)));
         Maximize_Children (MDI, False);

      else
         Max_W := Gint (Get_Allocation_Width (MDI.Layout));
         Max_H := Gint (Get_Allocation_Height (MDI.Layout));
      end if;

      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));

         if C.State = Normal then
            Num_Children := Num_Children + 1;
         end if;

         List := Widget_List.Next (List);
      end loop;

      W := Max_W;
      H := Max_H / Num_Children;

      List := First (MDI.Items);

      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));

         if C.State = Normal then
            C.X := 0;
            C.Y := Level;
            C.Uniconified_Width := W;
            C.Uniconified_Height := H;
            Alloc := (C.X, C.Y,
                      Allocation_Int (C.Uniconified_Width),
                      Allocation_Int (C.Uniconified_Height));
            Size_Allocate (C, Alloc);
            Level := Level + H;
         end if;

         List := Widget_List.Next (List);
      end loop;
   end Tile_Vertically;

   ------------------
   -- Delete_Child --
   ------------------

   function Delete_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean is
   begin
      if MDI_Child (Child).MDI.Close_Floating_Is_Unfloat
        and then (MDI_Child (Child).Flags and Always_Destroy_Float) = 0
        and then not MDI_Child (Child).MDI.All_Floating_Mode
      then
         Float_Child (MDI_Child (Child), False);
         return True;
      else
         return Return_Callback.Emit_By_Name
           (MDI_Child (Child).Initial, "delete_event", Event);
      end if;
   end Delete_Child;

   ---------------------------
   -- Key_Event_In_Floating --
   ---------------------------

   function Key_Event_In_Floating
     (Win   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean is
   begin
      --  Note: the following works because we are connected after the standard
      --  keypress event. Otherwise, standard keys in the child (space in
      --  editors most notably) will not work as expected.
      if Get_Event_Type (Event) = Key_Press then
         return Return_Callback.Emit_By_Name
           (Win, "key_press_event", Event);
      else
         return Return_Callback.Emit_By_Name
           (Win, "key_release_event", Event);
      end if;
   end Key_Event_In_Floating;

   -----------------
   -- Float_Child --
   -----------------

   procedure Float_Child
     (Child : access MDI_Child_Record'Class;
      Float : Boolean)
   is
      Diag  : Gtk_Dialog;
      Win   : Gtk_Window;
      Alloc : Gtk_Allocation;
      Cont  : Gtk_Container;
      Req   : Gtk_Requisition;
   begin
      if Child.State /= Floating and then Float then
         --  Ref is removed when the child is unfloated.
         Ref (Child);
         Child.Uniconified_Width  :=
           Gint (Get_Allocation_Width (Get_Widget (Child)));
         Child.Uniconified_Height :=
           Gint (Get_Allocation_Height (Get_Widget (Child)));

         Minimize_Child (Child, False);
         Dock_Child (Child, False);

         if Child.MDI.Children_Are_Maximized then
            Remove_From_Notebook (Child, None);
         else
            Remove (Child.MDI.Layout, Child);
         end if;

         if (Child.Flags and Float_As_Transient) /= 0 then
            Gtk_New (Diag,
                     Title  => Locale_To_UTF8 (Child.Title.all),
                     Parent => Gtk_Window (Get_Toplevel (Child.MDI)),
                     Flags  => No_Separator or Destroy_With_Parent);
            Set_Has_Separator (Diag, False);
            Win  := Gtk_Window (Diag);
            Cont := Gtk_Container (Get_Vbox (Diag));
         else
            Gtk_New (Win);
            Set_Title (Win, Locale_To_UTF8 (Child.Title.all));
            Cont := Gtk_Container (Win);
         end if;

         Set_Position (Win, Win_Pos_Mouse);

         --  Delete_Event should be forwarded to the child, not to the
         --  toplevel window

         Return_Callback.Object_Connect
           (Win, "delete_event",
            Return_Callback.To_Marshaller (Delete_Child'Access), Child);

         Add_Events (Win, Enter_Notify_Mask);
         Return_Callback.Object_Connect
           (Win, "focus_in_event",
            Return_Callback.To_Marshaller
               (Set_Focus_Child_MDI_Floating'Access),
            Child);

         --  Forward all key events to the toplevel of the MDI. This provides
         --  proper handling of menu key shortcuts.

         Return_Callback.Object_Connect
           (Win, "key_press_event",
            Return_Callback.To_Marshaller (Key_Event_In_Floating'Access),
            Gtk_Window (Get_Toplevel (Child.MDI)), After => True);
         Return_Callback.Object_Connect
           (Win, "key_release_event",
            Return_Callback.To_Marshaller (Key_Event_In_Floating'Access),
            Gtk_Window (Get_Toplevel (Child.MDI)), After => True);

         Reparent (Get_Parent (Child.Initial), Cont);
         Set_Default_Size
           (Win, Child.Uniconified_Width, Child.Uniconified_Height);
         Show_All (Win);

         Child.State := Floating;
         Update_Float_Menu (Child);
         Widget_Callback.Emit_By_Name (Child, "float_child");

      elsif Child.State = Floating and then not Float then
         --  Reassign the widget to Child instead of the notebook

         Win := Gtk_Window (Get_Toplevel (Child.Initial));
         Reparent (Get_Child (Win), Gtk_Box (Get_Child (Child)));
         Child.State := Normal;

         Destroy (Win);

         if Child.MDI.Children_Are_Maximized then
            Put_In_Notebook (Child.MDI, None, Child);
         else
            Put (Child.MDI.Layout, Child, Child.X, Child.Y);
            Alloc := (Child.X, Child.Y,
                      Allocation_Int (Child.Uniconified_Width),
                      Allocation_Int (Child.Uniconified_Height));
            Size_Allocate (Child, Alloc);
         end if;

         Update_Float_Menu (Child);
         Unref (Child);
         Widget_Callback.Emit_By_Name (Child, "unfloat_child");
      end if;
   end Float_Child;

   -----------------
   -- Is_Floating --
   -----------------

   function Is_Floating
     (Child : access MDI_Child_Record'Class) return Boolean is
   begin
      return Child.State = Floating;
   end Is_Floating;

   ------------------------
   -- Docked_Switch_Page --
   ------------------------

   procedure Docked_Switch_Page
     (Docked_Child : access Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      Page_Num : constant Guint := To_Guint (Args, 2);
      Child    : MDI_Child;
   begin
      if Page_Num = -1 then
         return;
      end if;

      Child := MDI_Child
        (Get_Nth_Page (Gtk_Notebook (Docked_Child), Gint (Page_Num)));

      if Child.MDI.Prevent_Focus_On_Page_Switch then
         return;
      end if;

      Set_Focus_Child (Child);
   end Docked_Switch_Page;

   ---------------------
   -- Create_Notebook --
   ---------------------

   procedure Create_Notebook
     (MDI : access MDI_Window_Record'Class; Side : Dock_Side) is
   begin
      if MDI.Docks (Side) = null then
         Gtk_New (MDI.Docks (Side));
         Set_Tab_Pos (MDI.Docks (Side), Pos_Bottom);
         Set_Show_Tabs (MDI.Docks (Side), False);
         Set_Show_Border (MDI.Docks (Side), False);
         Set_Border_Width (MDI.Docks (Side), 0);

         Set_Scrollable (MDI.Docks (Side));

         --  Coordinates don't matter, they are set in Size_Allocate_MDI.

         Put (MDI, MDI.Docks (Side), 0, 0);
         Widget_Callback.Connect
           (MDI.Docks (Side), "switch_page",
            Docked_Switch_Page'Access);

         --   Size to be computed

         if Side /= None then
            MDI.Docks_Size (Side) := -1;
         end if;

         Show_All (MDI.Docks (Side));
         Set_Dnd_Target (MDI.Docks (Side));

      else
         Set_Show_Tabs
           (MDI.Docks (Side), Get_Nth_Page (MDI.Docks (Side), 0) /= null);
      end if;
   end Create_Notebook;

   ----------------------
   -- Update_Tab_Label --
   ----------------------

   procedure Update_Tab_Label (Child : access MDI_Child_Record'Class) is
      Event : Gtk_Event_Box;
      Box   : Gtk_Box;
      Pix   : Gdk_Pixmap;
      Mask  : Gdk_Bitmap;
      Pixmap : Gtk_Pixmap;
      Tab   : Gtk_Widget;
      Side  : Dock_Side := Child.Dock;
   begin
      if Child.State = Docked
        or else (Child.State = Normal
                 and then Child.MDI.Children_Are_Maximized)
      then
         if Child.State = Normal then
            Side := None;
         end if;

         Gtk_New (Event);
         Gtk_New (Child.Tab_Label, Locale_To_UTF8 (Child.Short_Title.all));

         if Child.Icon /= null then
            Gtk_New_Hbox (Box, Homogeneous => False);

            Render_Pixmap_And_Mask (Child.Icon, Pix, Mask, 128);
            Gtk_New (Pixmap, Pix, Mask);
            Pack_Start (Box, Pixmap, Expand => False);
            Pack_Start (Box, Child.Tab_Label,  Expand => True, Fill => True);
            Add (Event, Box);
            Show_All (Box);
         else
            Add (Event, Child.Tab_Label);
            Show_All (Child.Tab_Label);
         end if;

         Set_Tab_Label (Child.MDI.Docks (Side), Child, Event);

         --  Setup drag-and-drop, so that items can be moved from one location
         --  to another.

         Set_Dnd_Source (Event, Child);
      end if;
   end Update_Tab_Label;

   ---------------------
   -- Put_In_Notebook --
   ---------------------

   procedure Put_In_Notebook
     (MDI   : access MDI_Window_Record'Class;
      Side  : Dock_Side;
      Child : access MDI_Child_Record'Class)
   is
      Label : Gtk_Label;
   begin
      --  Embed the contents of the child into the notebook, and mark
      --  Child as docked, so that we can't manipulate it afterwards.

      Ref (Child);

      if Get_Parent (Child) /= null then
         case Child.State is
            when Docked =>
               Remove_From_Notebook (Child, Child.Dock);

            when Normal =>
               if not MDI.Children_Are_Maximized
                 or else Page_Num (MDI.Docks (None), Child) = -1
               then
                  Remove (MDI.Layout, Child);
               else
                  Remove_From_Notebook (Child, None);
               end if;

            when Iconified =>
               Remove (MDI.Layout, Child);

            when Floating =>
               null;
         end case;
      end if;

      if Side = None then
         Child.State := Normal;
      else
         Child.State := Docked;
      end if;

      Create_Notebook (MDI, Side);

      Gtk_New (Label, "");
      Append_Page (MDI.Docks (Side), Child, Label);
      Update_Tab_Label (Child);

      Unref (Child);

      if Child.Minimize_Button /= null then
         Set_Sensitive (Child.Minimize_Button, False);
      end if;
   end Put_In_Notebook;

   --------------------------
   -- Remove_From_Notebook --
   --------------------------

   procedure Remove_From_Notebook
     (Child : access MDI_Child_Record'Class; Side : Dock_Side)
   is
      Note : constant Gtk_Notebook := Child.MDI.Docks (Side);
      Page : constant Gint := Page_Num (Note, Child);
      MDI  : constant MDI_Window := MDI_Window (Child.MDI);
   begin
      if Page /= -1 then
         Child.Tab_Label := null;
         Ref (Child);

         --  Do not use Remove() below, since it will generate random SE in dnd
         --  operations.
         Unparent (Child);

         Remove_Page (Note, Page);
      end if;

      if Get_Nth_Page (Note, 0) = null
        and then Side /= None
      then
         Destroy (MDI.Docks (Side));
         MDI.Docks (Side) := null;
         MDI.Docks_Size (Side) := 0;
         Reposition_Handles (MDI);
      else
         Set_Show_Tabs (Note, Get_Nth_Page (Note, 1) /= null);
      end if;

      Child.State := Normal;

      if Child.Minimize_Button /= null then
         Set_Sensitive (Child.Minimize_Button, True);
      end if;

      if Page /= -1 then
         Unref (Child);
      end if;
   end Remove_From_Notebook;

   -------------------------
   -- Get_Child_From_Page --
   -------------------------

   function Get_Child_From_Page (Page : Gtk_Widget) return MDI_Child is
   begin
      return MDI_Child (Page);
   end Get_Child_From_Page;

   ----------------
   -- Dock_Child --
   ----------------

   procedure Dock_Child
     (Child : access MDI_Child_Record'Class;
      Dock  : Boolean := True)
   is
      MDI   : constant MDI_Window := Child.MDI;
      Alloc : Gtk_Allocation;
   begin
      if MDI.All_Floating_Mode then
         return;
      end if;

      if Dock and then Child.Dock /= None then
         Float_Child (Child, False);
         Minimize_Child (Child, False);
         Put_In_Notebook (MDI, Child.Dock, Child);
         Update_Dock_Menu (Child);

         Set_Child_Visible (Child.Maximize_Button, False);
         Set_Child_Visible (Child.Minimize_Button, False);

      elsif not Dock and then Child.State = Docked then
         Ref (Child);
         Remove_From_Notebook (Child, Child.Dock);

         if MDI.Children_Are_Maximized then
            Put_In_Notebook (MDI, None, Child);

         else
            Put (MDI.Layout, Child, 0, 0);

            --  If the child was at least allocated once before (which doesn't
            --  happen if we are destroying the MDI when it hasn't been mapped
            if Child.Uniconified_Width /= -1 then
               Alloc := (Child.X, Child.Y,
                         Allocation_Int (Child.Uniconified_Width),
                         Allocation_Int (Child.Uniconified_Height));
               Size_Allocate (Child, Alloc);
            end if;
         end if;

         Unref (Child);
         Queue_Resize (Child);

         Set_Child_Visible (Child.Maximize_Button, True);
         Set_Child_Visible (Child.Minimize_Button, True);

         Update_Dock_Menu (Child);
      end if;
   end Dock_Child;

   -------------------
   -- Set_Dock_Side --
   -------------------

   procedure Set_Dock_Side
     (Child : access MDI_Child_Record'Class; Side : Dock_Side) is
   begin
      if Child.State = Docked then
         Put_In_Notebook (Child.MDI, Side, Child);
      end if;

      Child.Dock := Side;
      Update_Dock_Menu (Child);
   end Set_Dock_Side;

   --------------------
   -- Minimize_Child --
   --------------------

   procedure Minimize_Child
     (Child : access MDI_Child_Record'Class; Minimize : Boolean)
   is
      use type Widget_List.Glist;

      MDI         : constant MDI_Window := Child.MDI;
      List        : Widget_List.Glist;
      C2          : MDI_Child;
      Alloc       : Gtk_Allocation;
      Icons_Height : constant Gint :=
        MDI.Title_Bar_Height + 2 * Border_Thickness;

   begin
      if MDI.All_Floating_Mode then
         return;
      end if;

      --  Items can't be iconified if they are maximized

      if Child.State /= Iconified and then Minimize then
         Float_Child (Child, False);
         Dock_Child (Child, False);
         Child.Uniconified_X := Child.X;
         Child.Uniconified_Y := Child.Y;
         Child.State := Iconified;

         List := First (MDI.Items);
         Child.X := 0;
         Child.Y := Gint'Max
           (0, Gint (Get_Allocation_Height (MDI.Layout)) - Icons_Height);

         --  Find the best placement for the icon

         while List /= Null_List loop
            C2 := MDI_Child (Get_Data (List));

            if C2 /= MDI_Child (Child) and then C2.State = Iconified then
               if abs (C2.Y - Child.Y) / Icons_Height <= 1 then
                  if C2.X + Icons_Width >=
                    Gint (Get_Allocation_Width (MDI.Layout))
                  then
                     Child.X := 0;
                     Child.Y := C2.Y - Icons_Height;
                  elsif C2.X + Icons_Width > Child.X then
                     Child.X := C2.X + Icons_Width;
                     Child.Y := C2.Y;
                  end if;
               end if;
            end if;

            List := Next (List);
         end loop;

         Alloc := (Child.X, Child.Y,
                   Allocation_Int (Icons_Width),
                   Allocation_Int (Icons_Height));
         Size_Allocate (Child, Alloc);

         if Child.Maximize_Button /= null then
            Set_Sensitive (Child.Maximize_Button, False);
         end if;

      elsif Child.State = Iconified and then not Minimize then
         Child.State := Normal;
         Child.X := Child.Uniconified_X;
         Child.Y := Child.Uniconified_Y;
         Alloc := (Child.Uniconified_X, Child.Uniconified_Y,
                   Allocation_Int (Child.Uniconified_Width),
                   Allocation_Int (Child.Uniconified_Height));
         Size_Allocate (Child, Alloc);

         if Child.Maximize_Button /= null then
            Set_Sensitive (Child.Maximize_Button, True);
         end if;
      end if;
   end Minimize_Child;

   -----------------------
   -- Maximize_Children --
   -----------------------

   procedure Maximize_Children
     (MDI : access MDI_Window_Record; Maximize : Boolean := True)
   is
      use Widget_List;

      List      : Widget_List.Glist := Last (MDI.Items);
      C         : MDI_Child;
      Old_Focus : constant MDI_Child := MDI.Focus_Child;
      Created   : Boolean := False;

   begin
      if MDI.All_Floating_Mode then
         return;
      end if;

      if Maximize and then not MDI.Children_Are_Maximized then
         if MDI.Docks (None) /= null then
            Set_Child_Visible (MDI.Docks (None), True);
         else
            Create_Notebook (MDI, None);
         end if;

         while List /= Null_List loop
            C := MDI_Child (Get_Data (List));
            List := Prev (List);

            if C.State = Normal or else C.State = Iconified then
               Created := True;
               Put_In_Notebook (MDI, None, C);
            end if;
         end loop;

         Set_Child_Visible (MDI.Layout, False);
         MDI.Children_Are_Maximized := True;

      elsif not Maximize and then MDI.Children_Are_Maximized then
         --  The middle notebook was already destroyed by the last call to
         --  Remove_From_Notebook in the above loop

         Set_Child_Visible (MDI.Layout, True);

         loop
            C := MDI_Child (Get_Nth_Page (MDI.Docks (None), 0));
            exit when C = null;

            --  Remove from middle notebook and put in layout
            Ref (C);
            Remove_From_Notebook (C, None);
            Put (MDI.Layout, C, 0, 0);
            Unref (C);
         end loop;

         --  If the user has done a Show_All on the MDI, it is possible that
         --  both the layout and the notebook are made visible, so it's time to
         --  hide one of them.
         if MDI.Docks (None) /= null then
            Set_Child_Visible (MDI.Docks (None), False);
         end if;
         MDI.Children_Are_Maximized := False;
      end if;

      if Old_Focus /= null then
         Raise_Child (Old_Focus);
      end if;

      Queue_Resize (MDI);
   end Maximize_Children;

   ---------------------------
   -- Set_All_Floating_Mode --
   ---------------------------

   procedure Set_All_Floating_Mode
     (MDI : access MDI_Window_Record; All_Floating : Boolean)
   is
      use Widget_List;
      List      : Widget_List.Glist := First (MDI.Items);

      procedure Resize (Window : System.Address; Width, Height : Gint);
      pragma Import (C, Resize, "gtk_window_resize");
   begin
      if All_Floating /= MDI.All_Floating_Mode then
         while List /= Null_List loop
            Float_Child (MDI_Child (Get_Data (List)), All_Floating);
            List := Next (List);
         end loop;

         Set_Sensitive (MDI.Dock_Menu_Item, not All_Floating);
         Set_Sensitive (MDI.Float_Menu_Item, not All_Floating);

         MDI.All_Floating_Mode := All_Floating;
         Set_Child_Visible (MDI, not All_Floating);

         Resize (Gtk_Window (Get_Toplevel (MDI)), -1, -1);
      end if;
   end Set_All_Floating_Mode;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (Child : access MDI_Child_Record) return Gtk_Widget is
   begin
      return Gtk_Widget (Child.Initial);
   end Get_Widget;

   ---------------------
   -- Get_Focus_Child --
   ---------------------

   function Get_Focus_Child
     (MDI : access MDI_Window_Record) return MDI_Child is
   begin
      return MDI.Focus_Child;
   end Get_Focus_Child;

   ----------------
   -- Cascade_Cb --
   ----------------

   procedure Cascade_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Cascade_Children (MDI_Window (MDI));

   exception
      when others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         null;
   end Cascade_Cb;

   ---------------
   -- Tile_H_Cb --
   ---------------

   procedure Tile_H_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Tile_Horizontally (MDI_Window (MDI));

   exception
      when others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         null;
   end Tile_H_Cb;

   ---------------
   -- Tile_V_Cb --
   ---------------

   procedure Tile_V_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Tile_Vertically (MDI_Window (MDI));

   exception
      when others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         null;
   end Tile_V_Cb;

   -----------------
   -- Maximize_Cb --
   -----------------

   procedure Maximize_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Maximize_Children (MDI_Window (MDI), True);

   exception
      when others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         null;
   end Maximize_Cb;

   -----------------------
   -- Maximize_Child_Cb --
   -----------------------

   procedure Maximize_Child_Cb (Child : access Gtk_Widget_Record'Class) is
      M : constant MDI_Window := MDI_Child (Child).MDI;
   begin
      Maximize_Children (M, not M.Children_Are_Maximized);
      Set_Focus_Child (MDI_Child (Child));

   exception
      when others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the button in Initialize
         null;
   end Maximize_Child_Cb;

   -------------------
   -- Unmaximize_Cb --
   -------------------

   procedure Unmaximize_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Maximize_Children (MDI_Window (MDI), False);

   exception
      when others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         null;
   end Unmaximize_Cb;

   -------------
   -- Dock_Cb --
   -------------

   procedure Dock_Cb (MDI : access Gtk_Widget_Record'Class) is
      C : MDI_Child;
   begin
      if MDI.all in MDI_Window_Record'Class then
         C := Get_Focus_Child (MDI_Window (MDI));
      else
         C := MDI_Child (MDI);
      end if;

      if C /= null then
         Dock_Child (C, C.State /= Docked);
         Set_Focus_Child (C);
         Raise_Child (C);
      end if;

   exception
      when others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         null;
   end Dock_Cb;

   --------------
   -- Float_Cb --
   --------------

   procedure Float_Cb (MDI : access Gtk_Widget_Record'Class) is
      C : MDI_Child;
   begin
      if MDI.all in MDI_Window_Record'Class then
         C := Get_Focus_Child (MDI_Window (MDI));
      else
         C := MDI_Child (MDI);
      end if;

      if C /= null then
         Float_Child (C, C.State /= Floating);
         Set_Focus_Child (C);
         Raise_Child (C);
      end if;

   exception
      when others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         null;
   end Float_Cb;

   --------------
   -- Close_Cb --
   --------------

   procedure Close_Cb (MDI : access Gtk_Widget_Record'Class) is
      C : MDI_Child;
   begin
      if MDI.all in MDI_Window_Record'Class then
         C := MDI_Window (MDI).Focus_Child;

         --  Close automatically gets the contents of docks, instead of the
         --  dock itself

      else
         C := MDI_Child (MDI);
      end if;

      if C /= null then
         Close_Child (C);
      end if;

   exception
      when others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         null;
   end Close_Cb;

   --------------
   -- Close_Cb --
   --------------

   procedure Focus_Cb (Child : access Gtk_Widget_Record'Class) is
      C : constant MDI_Child := MDI_Child (Child);
   begin
      if Get_Active (C.Menu_Item) then
         Set_Focus_Child (C);
      end if;
   end Focus_Cb;

   --------------------------
   -- Menu_Entry_Destroyed --
   --------------------------

   procedure Menu_Entry_Destroyed (Child : access Gtk_Widget_Record'Class) is
   begin
      MDI_Child (Child).Menu_Item := null;
   end Menu_Entry_Destroyed;

   -----------------------
   -- Create_Menu_Entry --
   -----------------------

   procedure Create_Menu_Entry (Child : access MDI_Child_Record'Class) is
      use Widget_List, Widget_SList;

      G           : Widget_SList.GSlist := Widget_SList.Null_List;
      First_Child : MDI_Child;
      Tmp         : Widget_List.Glist;

   begin
      if Child.Menu_Item = null
        and then Child.Short_Title.all /= ""
      then
         --  Find the group to which the radio menu items should belong. We
         --  cannot save this group into a variable, since it might change when
         --  the first child is removed from the MDI.

         Tmp := Child.MDI.Items;

         while Tmp /= Widget_List.Null_List loop
            First_Child := MDI_Child (Get_Data (Tmp));

            if First_Child.Menu_Item /= null then
               G := Get_Group (First_Child.Menu_Item);
               exit;
            end if;

            Tmp := Next (Tmp);
         end loop;

         Gtk_New (Child.Menu_Item, G, "");
         Update_Menu_Item (Child);
         Append (Child.MDI.Menu, Child.Menu_Item);
         Set_Active
           (Child.Menu_Item, MDI_Child (Child) = Child.MDI.Focus_Child);
         Show_All (Child.Menu_Item);
         Widget_Callback.Object_Connect
           (Child.Menu_Item, "activate",
            Widget_Callback.To_Marshaller (Focus_Cb'Access), Child,
            After => True);
         Widget_Callback.Object_Connect
           (Child.Menu_Item, "destroy",
            Widget_Callback.To_Marshaller (Menu_Entry_Destroyed'Access),
            Child);
      end if;
   end Create_Menu_Entry;

   ----------------------------
   -- Propagate_Expose_Event --
   ----------------------------

   procedure Propagate_Expose_Event
     (Container : access Gtk.Container.Gtk_Container_Record'Class;
      Event     : Gdk.Event.Gdk_Event_Expose)
   is
      use Widget_List;
      Children, Tmp : Widget_List.Glist;
   begin
      Children := Get_Children (Container);
      Tmp := Children;

      while Tmp /= Null_List loop
         Propagate_Expose (Container, Get_Data (Tmp), Event);
         Tmp := Next (Tmp);
      end loop;

      Free (Children);
   end Propagate_Expose_Event;

   --------------------
   -- Menu_Destroyed --
   --------------------

   procedure Menu_Destroyed (MDI : access Gtk_Widget_Record'Class) is
   begin
      MDI_Window (MDI).Menu := null;
      MDI_Window (MDI).Dock_Menu_Item := null;
      MDI_Window (MDI).Float_Menu_Item := null;
   end Menu_Destroyed;

   -----------------
   -- Create_Menu --
   -----------------

   function Create_Menu (MDI   : access MDI_Window_Record)
      return Gtk.Menu.Gtk_Menu
   is
      Item  : Gtk_Menu_Item;
      Child : MDI_Child;
      Tmp   : Widget_List.Glist;

   begin
      if MDI.Menu = null then
         Gtk_New (MDI.Menu);

         Gtk_New (Item, "Cascade");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Cascade_Cb'Access), MDI);
         Set_Accel_Path (Item, "<gtkada>/window/cascade", MDI.Group);

         Gtk_New (Item, "Tile Horizontally");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Tile_H_Cb'Access), MDI);
         Set_Accel_Path (Item, "<gtkada>/window/tile_horizontal", MDI.Group);

         Gtk_New (Item, "Tile Vertically");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Tile_V_Cb'Access), MDI);
         Set_Accel_Path (Item, "<gtkada>/window/tile_vertical", MDI.Group);

         Gtk_New (Item, "Maximize All");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Maximize_Cb'Access), MDI);
         Set_Accel_Path (Item, "<gtkada>/window/maximize", MDI.Group);

         Gtk_New (Item, "Unmaximize All");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Unmaximize_Cb'Access), MDI);
         Set_Accel_Path (Item, "<gtkada>/window/unmaximize", MDI.Group);

         Gtk_New (Item, "Arrange Icons");
         Append (MDI.Menu, Item);
         Set_Accel_Path (Item, "<gtkada>/window/arrange_icons", MDI.Group);
         Set_Sensitive (Item, False);

         Gtk_New (Item);
         Append (MDI.Menu, Item);

         Gtk_New (MDI.Dock_Menu_Item, "Docked");
         Append (MDI.Menu, MDI.Dock_Menu_Item);
         Set_Active (MDI.Dock_Menu_Item,
                     MDI.Focus_Child /= null
                     and then MDI.Focus_Child.State = Docked);
         MDI.Dock_Menu_Item_Id := Widget_Callback.Object_Connect
           (MDI.Dock_Menu_Item, "toggled",
            Widget_Callback.To_Marshaller (Dock_Cb'Access), MDI);
         Set_Accel_Path (Item, "<gtkada>/window/docked", MDI.Group);

         Gtk_New (MDI.Float_Menu_Item, "Floating");
         Append (MDI.Menu, MDI.Float_Menu_Item);
         Set_Active (MDI.Float_Menu_Item,
                     MDI.Focus_Child /= null
                     and then MDI.Focus_Child.State = Floating);
         MDI.Float_Menu_Item_Id := Widget_Callback.Object_Connect
           (MDI.Float_Menu_Item, "toggled",
            Widget_Callback.To_Marshaller (Float_Cb'Access), MDI);
         Set_Accel_Path (Item, "<gtkada>/window/floating", MDI.Group);

         Gtk_New (Item);
         Append (MDI.Menu, Item);

         Gtk_New (MDI.Close_Menu_Item, "Close");
         Append (MDI.Menu, MDI.Close_Menu_Item);
         Widget_Callback.Object_Connect
           (MDI.Close_Menu_Item, "activate",
            Widget_Callback.To_Marshaller (Close_Cb'Access), MDI);
         Set_Accel_Path (Item, "<gtkada>/window/close", MDI.Group);

         Gtk_New (Item);
         Append (MDI.Menu, Item);

         Tmp := First (MDI.Items);

         while Tmp /= Null_List loop
            Child := MDI_Child (Get_Data (Tmp));
            Create_Menu_Entry (Child);
            Tmp := Next (Tmp);
         end loop;

         Widget_Callback.Object_Connect
           (MDI.Menu, "destroy",
            Widget_Callback.To_Marshaller (Menu_Destroyed'Access), MDI);
      end if;

      Show_All (MDI.Menu);
      return MDI.Menu;
   end Create_Menu;

   -----------------------
   -- Create_Child_Menu --
   -----------------------

   function Create_Child_Menu
     (Child : access MDI_Child_Record'Class) return Gtk.Menu.Gtk_Menu
   is
      Menu  : Gtk_Menu;
      Item  : Gtk_Menu_Item;
      Check : Gtk_Check_Menu_Item;

   begin
      Gtk_New (Menu);

      Gtk_New (Item, "Cascade");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Tile Horizontally");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Tile Vertically");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Maximize All");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Unmaximize All");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Arrange Icons");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item);
      Append (Menu, Item);

      Gtk_New (Check, "Docked");
      Append (Menu, Check);
      Set_Active (Check, Child.State = Docked);
      Set_Sensitive (Check, Child.Dock /= None);
      Widget_Callback.Object_Connect
        (Check, "toggled",
         Widget_Callback.To_Marshaller (Dock_Cb'Access), Child);

      Gtk_New (Check, "Floating");
      Append (Menu, Check);
      Set_Active (Check, Child.State = Floating);
      Widget_Callback.Object_Connect
        (Check, "toggled",
         Widget_Callback.To_Marshaller (Float_Cb'Access), Child);

      Gtk_New (Item);
      Append (Menu, Item);

      Gtk_New (Item, "Close");
      Append (Menu, Item);
      Widget_Callback.Object_Connect
        (Item, "activate",
         Widget_Callback.To_Marshaller (Close_Cb'Access), Child);

      Show_All (Menu);
      return Menu;
   end Create_Child_Menu;

   --------------------
   -- Set_Priorities --
   --------------------

   procedure Set_Priorities
     (MDI : access MDI_Window_Record; Prio : Priorities_Array) is
   begin
      MDI.Priorities := Prio;
   end Set_Priorities;

   ---------------------
   -- Set_Focus_Child --
   ---------------------

   procedure Set_Focus_Child
     (MDI : access MDI_Window_Record;
      Containing : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (MDI);
      Parent : Gtk_Widget := Gtk_Widget (Containing);
   begin
      while Parent /= null
        and then not (Parent.all in MDI_Child_Record'Class)
      loop
         Parent := Get_Parent (Parent);
      end loop;

      if Parent /= null then
         Set_Focus_Child (MDI_Child (Parent));
      end if;
   end Set_Focus_Child;

   ----------------------
   -- Desktop Handling --
   ----------------------

   procedure Add_To_Tree
     (MDI         : access MDI_Window_Record'Class;
      Tree        : in out Glib.Xml_Int.Node_Ptr;
      ID_Node     : Glib.Xml_Int.Node_Ptr;
      X           : Integer := 100;
      Y           : Integer := 100;
      Width       : Integer := 100;
      Height      : Integer := 100;
      Short_Title : String := "";
      Title       : String := "";
      State       : State_Type := Normal;
      Dock        : Dock_Side := None;
      Focus       : Boolean := False)
   is
      --  ??? some code duplication from Save_Desktop, see below.

      Child_Node : Node_Ptr;

      procedure Add (Name, Value : String);
      --  Add a new child to Child_Node

      procedure Add (Name, Value : String) is
         N : Node_Ptr;
      begin
         N := new Node;
         N.Tag := new String'(Name);
         N.Value := new String'(Value);
         Add_Child (Child_Node, N);
      end Add;

   begin
      if Tree = null then
         Tree := new Node;
         Tree.Tag := new String'("MDI");
         Child_Node := Tree;
         Add ("Maximized", Boolean'Image (MDI.Children_Are_Maximized));
      else
         Child_Node := Tree;
      end if;

      --  ??? we could improve the cases where nodes override
      --  older nodes.

      case Dock is
         when Left =>
            Add ("Left", Integer'Image (Width));
         when Right =>
            Add ("Right", Integer'Image (Width));
         when Top =>
            Add ("Top", Integer'Image (Height));
         when Bottom =>
            Add ("Bottom", Integer'Image (Height));
         when None =>
            null;
      end case;

      Child_Node := new Node;
      Child_Node.Tag := new String'("Child");

      Add ("Focus", Boolean'Image (Focus));
      Add ("Dock", Dock_Side'Image (Dock));
      Add ("State", State_Type'Image (State));
      Add ("Title", Title);
      Add ("Short_Title", Short_Title);
      Add ("Height", Integer'Image (Height));
      Add ("Width", Integer'Image (Width));
      Add ("Y", Integer'Image (Y));
      Add ("X", Integer'Image (X));

      if ID_Node /= null then
         Add_Child (Child_Node, ID_Node);
      end if;

      Add_Child (Tree, Child_Node);
   end Add_To_Tree;

   package body Desktop is

      --------------------------------
      -- Register_Desktop_Functions --
      --------------------------------

      procedure Register_Desktop_Functions
        (Save : Save_Desktop_Function;
         Load : Load_Desktop_Function) is
      begin
         Registers := new Register_Node_Record'
           (Save => Save,
            Load => Load,
            Next => Registers);
      end Register_Desktop_Functions;

      ---------------------
      -- Restore_Desktop --
      ---------------------

      procedure Restore_Desktop
        (MDI       : access MDI_Window_Record'Class;
         From_Tree : Glib.Xml_Int.Node_Ptr;
         User      : User_Data)
      is
         Child, Focus_Child : MDI_Child;
         Child_Node : Node_Ptr := From_Tree.Child;
         N          : Node_Ptr;
         Register   : Register_Node;
         Width, Height : Guint;
         State      : State_Type;
         Icons_Height : constant Gint :=
           MDI.Title_Bar_Height - 2 * Border_Thickness;

         Raised     : Boolean := False;

         Current_Pages : array (Dock_Side) of MDI_Child
           := (others => null);

      begin
         pragma Assert (From_Tree.Tag.all = "MDI");
         MDI.Desktop_Was_Loaded := True;

         while Child_Node /= null loop
            if Child_Node.Tag.all = "Left" then
               MDI.Docks_Size (Left) := Gint'Value (Child_Node.Value.all);

            elsif Child_Node.Tag.all = "Right" then
               MDI.Docks_Size (Right) := Gint'Value (Child_Node.Value.all);

            elsif Child_Node.Tag.all = "Top" then
               MDI.Docks_Size (Top) := Gint'Value (Child_Node.Value.all);

            elsif Child_Node.Tag.all = "Bottom" then
               MDI.Docks_Size (Bottom) := Gint'Value (Child_Node.Value.all);

            elsif Child_Node.Tag.all = "Maximized" then
               Maximize_Children (MDI, Boolean'Value (Child_Node.Value.all));

            elsif Child_Node.Tag.all = "Child" then
               --  Create the child

               Register := Registers;
               Child := null;

               while Child = null and then Register /= null loop
                  Child := Register.Load
                    (MDI_Window (MDI), Child_Node.Child, User);
                  Register := Register.Next;
               end loop;

               if Child /= null then
                  N := Child_Node.Child.Next;

                  while N /= null loop
                     if N.Tag.all = "X" then
                        Child.X := Gint'Value (N.Value.all);

                     elsif N.Tag.all = "Y" then
                        Child.Y := Gint'Value (N.Value.all);

                     elsif N.Tag.all = "Width" then
                        Width := Guint'Value (N.Value.all);

                     elsif N.Tag.all = "Height" then
                        Height := Guint'Value (N.Value.all);

                     elsif N.Tag.all = "Title" then
                        Set_Title (Child, N.Value.all, Child.Short_Title.all);

                     elsif N.Tag.all = "Short_Title" then
                        Set_Title (Child, Child.Title.all, N.Value.all);

                     elsif N.Tag.all = "State" then
                        State := State_Type'Value (N.Value.all);

                     elsif N.Tag.all = "Dock" then
                        Child.Dock := Dock_Side'Value (N.Value.all);

                     elsif N.Tag.all = "Uniconified_X" then
                        Child.Uniconified_X := Gint'Value (N.Value.all);

                     elsif N.Tag.all = "Uniconified_Y" then
                        Child.Uniconified_Y := Gint'Value (N.Value.all);

                     elsif N.Tag.all = "Uniconified_Width" then
                        Child.Uniconified_Width := Gint'Value (N.Value.all);

                     elsif N.Tag.all = "Uniconified_Height" then
                        Child.Uniconified_Height := Gint'Value (N.Value.all);

                     elsif N.Tag.all = "Focus"
                       and then Boolean'Value (N.Value.all)
                     then
                        Focus_Child := Child;

                     elsif N.Tag.all = "Raised"
                       and then Boolean'Value (N.Value.all)
                     then
                        Raised := True;

                     else
                        --  ??? Unknown node, just ignore for now
                        null;
                     end if;

                     N := N.Next;
                  end loop;

                  if Raised then
                     Current_Pages (Child.Dock) := Child;
                  end if;

                  Raised := False;

                  case State is
                     when Docked =>
                        Dock_Child (Child, True);

                     when Floating =>
                        Set_Size_Request (Child, Gint (Width), Gint (Height));
                        Float_Child (Child, True);

                     when Normal =>
                        Float_Child (Child, False);
                        Dock_Child (Child, False);
                        Set_Size_Request (Child, Gint (Width), Gint (Height));

                     when Iconified =>
                        Child.State := Iconified;

                        if Child.Maximize_Button /= null then
                           Set_Sensitive (Child.Maximize_Button, False);
                        end if;

                        Size_Allocate
                          (Child, (Child.X, Child.Y,
                                   Allocation_Int (Icons_Width),
                                   Allocation_Int (Icons_Height)));
                  end case;
               end if;
            end if;

            Child_Node := Child_Node.Next;
         end loop;

         --  Remove any idle that would have been set when the notebooks were
         --  created.

         if MDI.Raise_Id /= 0 then
            Idle_Remove (MDI.Raise_Id);
         end if;

         for J in Current_Pages'Range loop
            if Current_Pages (J) /= null then
               Raise_Child (Current_Pages (J));
            end if;
         end loop;

         if Focus_Child /= null then
            Set_Focus_Child (Focus_Child);
         end if;

         Queue_Resize (MDI);
      end Restore_Desktop;

      ------------------
      -- Save_Desktop --
      ------------------

      function Save_Desktop
        (MDI : access MDI_Window_Record'Class) return Glib.Xml_Int.Node_Ptr
      is
         use type Widget_List.Glist;

         Item             : Widget_List.Glist := MDI.Items;
         Root, Child_Node : Node_Ptr;
         Widget_Node      : Node_Ptr;
         Register         : Register_Node;
         Child            : MDI_Child;
         Length           : Guint;

         procedure Add (Name, Value : String);
         --  Add a new child to Child_Node

         procedure Save_Widget
           (Child  : MDI_Child;
            Raised : Boolean);
         --  Save the Child. Raised is True if Child is the current page
         --  in a notebook.

         ---------
         -- Add --
         ---------

         procedure Add (Name, Value : String) is
            N : Node_Ptr;
         begin
            N := new Node;
            N.Tag := new String'(Name);
            N.Value := new String'(Value);
            Add_Child (Child_Node, N);
         end Add;

         -----------------
         -- Save_Widget --
         -----------------

         procedure Save_Widget
           (Child  : MDI_Child;
            Raised : Boolean) is
         begin
            Register := Registers;
            Widget_Node := null;

            while Widget_Node = null and then Register /= null loop
               Widget_Node := Register.Save (Child.Initial);
               Register := Register.Next;
            end loop;

            if Widget_Node /= null then
               --  Note: We need to insert the children in the opposite order
               --  from Restore_Desktop, since the children are added at the
               --  beginning of the list.

               Child_Node := new Node;
               Child_Node.Tag := new String'("Child");

               if Child.State = Iconified then
                  Add ("Uniconified_Height",
                       Gint'Image (Child.Uniconified_Height));
                  Add ("Uniconified_Width",
                       Gint'Image (Child.Uniconified_Width));
                  Add ("Uniconified_Y", Gint'Image (Child.Uniconified_Y));
                  Add ("Uniconified_X", Gint'Image (Child.Uniconified_X));
               end if;

               Add ("Dock", Dock_Side'Image (Child.Dock));
               Add ("State", State_Type'Image (Child.State));
               Add ("Title", Child.Title.all);
               Add ("Short_Title", Child.Short_Title.all);
               Add ("Height",
                    Allocation_Int'Image (Get_Allocation_Height (Child)));
               Add ("Width",
                    Allocation_Int'Image (Get_Allocation_Width (Child)));
               Add ("Y", Gint'Image (Child.Y));
               Add ("X", Gint'Image (Child.X));

               if Child = MDI.Focus_Child then
                  Add ("Focus", "True");
               end if;

               if Raised then
                  Add ("Raised", "True");
               end if;

               Add_Child (Child_Node, Widget_Node);

               Add_Child (Root, Child_Node);
            end if;

         end Save_Widget;

         Current_Page : Gint;

      begin
         Root := new Node;
         Root.Tag := new String'("MDI");
         Child_Node := Root;

         Add ("Left",   Gint'Image (MDI.Docks_Size (Left)));
         Add ("Right",  Gint'Image (MDI.Docks_Size (Right)));
         Add ("Top",    Gint'Image (MDI.Docks_Size (Top)));
         Add ("Bottom", Gint'Image (MDI.Docks_Size (Bottom)));
         Add ("Maximized", Boolean'Image (MDI.Children_Are_Maximized));

         --  Look through all the notebooks, and save the widgets in the
         --  notebook order.

         for J in MDI.Docks'Range loop
            if MDI.Docks (J) /= null
              and then (J /= None or else MDI.Children_Are_Maximized)
            then
               Length := Page_List.Length (Get_Children (MDI.Docks (J)));
               Current_Page := Get_Current_Page (MDI.Docks (J));

               if Length > 0 then
                  for Page_Index in reverse 0 .. Length - 1 loop
                     Save_Widget
                       (Get_Child_From_Page
                          (Get_Nth_Page (MDI.Docks (J), Gint (Page_Index))),
                        (Current_Page = Gint (Page_Index)));
                  end loop;
               end if;
            end if;
         end loop;

         --  Save the floating and non-maximized widgets.

         while Item /= Widget_List.Null_List loop
            Child := MDI_Child (Widget_List.Get_Data (Item));

            case Child.State is
               when Docked =>
                  null;

               when Normal =>
                  if not MDI.Children_Are_Maximized then
                     Save_Widget (Child, False);
                  end if;

               when Floating | Iconified =>
                  Save_Widget (Child, False);
            end case;

            Item := Widget_List.Next (Item);
         end loop;

         return Root;
      end Save_Desktop;

      ---------------------------------------
      -- Free_Registered_Desktop_Functions --
      ---------------------------------------

      procedure Free_Registered_Desktop_Functions is
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (Register_Node_Record, Register_Node);
         Next : Register_Node;
      begin
         while Registers /= null loop
            Next := Registers.Next;
            Unchecked_Free (Registers);
            Registers := Next;
         end loop;
      end Free_Registered_Desktop_Functions;

   end Desktop;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (MDI : access MDI_Window_Record) return Child_Iterator is
   begin
      return (Iter => MDI.Items);
   end First_Child;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Child_Iterator) is
   begin
      Iterator.Iter := Widget_List.Next (Iterator.Iter);
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iterator : Child_Iterator) return MDI_Child is
      use type Widget_List.Glist;
   begin
      if Iterator.Iter /= Widget_List.Null_List then
         return MDI_Child (Widget_List.Get_Data (Iterator.Iter));
      else
         return null;
      end if;
   end Get;

   ---------------------
   -- Highlight_Child --
   ---------------------

   procedure Highlight_Child
     (Child : access MDI_Child_Record; Highlight : Boolean := True)
   is
      Tab     : Gtk_Widget;
      Note    : Gtk_Notebook;
      Label   : Gtk_Label;
      Style   : Gtk_Style;
   begin
      if (Child.State = Normal
          and then Child.MDI.Children_Are_Maximized)
      then
         Note := Child.MDI.Docks (None);

      elsif Child.State = Docked then
         Note := Child.MDI.Docks (Child.Dock);
      end if;

      Label := Child.Tab_Label;

      if Highlight then
         --  Do nothing if:
         --    - the child is in the layout and has the focus
         --    - the child is in a notebook and is in the current page

         if (Child.State = Normal
             and then not Child.MDI.Children_Are_Maximized
             and then Child.MDI.Selected_Child = MDI_Child (Child))
         then
            return;
         end if;

         if Note /= null
           and then Get_Current_Page (Note) = Page_Num (Note, Child)
         then
            return;
         end if;

         Style := Child.MDI.Highlight_Style;
      else
         Style := null;
      end if;

      --  Might be null if we haven't created the MDI menu yet
      if Child.Menu_Item /= null then
         declare
            Children : Widget_List.Glist := Get_Children
              (Gtk_Box (Get_Child (Child.Menu_Item)));
            Tmp      : Widget_List.Glist := Children;
         begin
            while Tmp /= Null_List loop
               if Get_Data (Tmp)'Tag = Gtk_Accel_Label_Record'Tag then
                  Set_Style (Get_Data (Tmp), Style);
               end if;
               Tmp := Next (Tmp);
            end loop;
            Free (Children);
         end;
      end if;

      if Label /= null then
         Set_Style (Label, Style);
      end if;
   end Highlight_Child;

   ------------------------
   -- Desktop_Was_Loaded --
   ------------------------

   function Desktop_Was_Loaded (MDI : access MDI_Window_Record)
      return Boolean is
   begin
      return MDI.Desktop_Was_Loaded;
   end Desktop_Was_Loaded;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (Child : access MDI_Child_Record) return State_Type is
   begin
      return Child.State;
   end Get_State;

   --------------------------
   -- Source_Drag_Data_Get --
   --------------------------

   procedure Source_Drag_Data_Get
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      Data  : constant Selection_Data := Selection_Data (To_C_Proxy (Args, 2));
      Info  : constant Guint          := To_Guint (Args, 3);
      Child : constant MDI_Child      := MDI_Child (Widget);

   begin
      if Info = Root_Window_Target_Dnd then
         Float_Child (Child, True);
         Set_Focus_Child (Child);
         Raise_Child (Child);

      elsif Info = Widget_Target_Dnd then
         Selection_Data_Set
           (Data,
            The_Type => Get_Target (Data),
            Format   => Widget_Format,
            Data     => Child'Address,
            Length   => Child'Size);
      end if;
   end Source_Drag_Data_Get;

   -------------------------------
   -- Target_Drag_Data_Received --
   -------------------------------

   procedure Target_Drag_Data_Received
     (Notebook : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args     : Gtk_Args)
   is
      Context : constant Drag_Context := Drag_Context (To_C_Proxy (Args, 1));
      Data : constant Selection_Data := Selection_Data (To_C_Proxy (Args, 4));
      Info : constant Guint := To_Guint (Args, 5);
      Time : constant Guint := To_Guint (Args, 6);

      Child : MDI_Child;
      Old   : Gtk_Notebook;

      type Gtk_Widget_Access is access Gtk_Widget;
      function Unchecked_Convert is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Widget_Access);

   begin
      if Get_Length (Data) >= 0
        and then Info = Widget_Target_Dnd
      then
         case Get_Format (Data) is
            when Widget_Format =>
               Child := MDI_Child (Unchecked_Convert (Get_Data (Data)).all);

               if Notebook.all in Gtk_Fixed_Record'Class then
                  if Child.State /= Normal then
                     Dock_Child (Child, False);
                  end if;

                  Set_Focus_Child (Child);
                  Raise_Child (Child);

                  Finish
                    (Context,
                     Success => True,
                     Del     => False,
                     Time    => Guint32 (Time));

               elsif Notebook.all in Gtk_Event_Box_Record'Class then
                  --  We have to first undock it, or we can't move it to
                  --  another dock
                  Dock_Child (Child, False);

                  for Side in Left .. Bottom loop
                     if Gtk_Event_Box (Notebook) =
                       Child.MDI.Drop_Sites (Side)
                     then
                        Child.Dock := Side;
                        Dock_Child (Child, True);
                        exit;
                     end if;
                  end loop;

                  Finish
                    (Context,
                     Success => True,
                     Del     => False,
                     Time    => Guint32 (Time));

               elsif Child.State /= Docked
                 or else Child.MDI.Docks (Child.Dock) /=
                    Gtk_Notebook (Notebook)
               then

                  Old := Child.MDI.Docks (Child.Dock);

                  --  We have to first undock it, or we can't move it to
                  --  another dock
                  Dock_Child (Child, False);

                  for Side in Dock_Side'Range loop
                     if Gtk_Notebook (Notebook) = Child.MDI.Docks (Side) then
                        if Side /= None then
                           Child.Dock := Side;
                           Dock_Child (Child, True);
                        end if;
                        exit;
                     end if;
                  end loop;

                  Set_Focus_Child (Child);
                  Raise_Child (Child);

                  Finish
                    (Context,
                     Success => True,
                     Del     => False,
                     Time    => Guint32 (Time));
               else
                  Finish
                    (Context,
                     Success => False,
                     Del     => False,
                     Time    => Guint32 (Time));
               end if;

            when others =>
               Finish
                 (Context,
                  Success => False,
                  Del     => False,
                  Time    => Guint32 (Time));
         end case;
      end if;
   end Target_Drag_Data_Received;

   --------------------
   -- Set_Dnd_Target --
   --------------------

   procedure Set_Dnd_Target (Widget : access Gtk_Widget_Record'Class) is
   begin
      --  Set up the notebook as a possible drag-and-drop target, so that
      --  items can be moved from one to another by dragging them.

      Gtk.Dnd.Dest_Set
        (Widget  => Widget,
         Flags   => Dest_Default_All,
         Targets => Dest_Target_Table,
         Actions => Action_Copy);
      Widget_Callback.Connect
        (Widget, "drag_data_received", Target_Drag_Data_Received'Access);
   end Set_Dnd_Target;

   --------------------
   -- Set_Dnd_Source --
   --------------------

   procedure Set_Dnd_Source
     (Widget : access Gtk_Widget_Record'Class;
      Child  : access Gtk_Widget_Record'Class) is
   begin
      --  Set up the drag-and-drop (when clicking in the title bar), so that
      --  the item can be moved to another notebook by dragging it

      Gtk.Dnd.Source_Set
        (Widget            => Widget,
         Start_Button_Mask => Button1_Mask,
         Targets           => Source_Target_Table,
         Actions           => Action_Copy);
      Widget_Callback.Object_Connect
        (Widget, "drag_data_get", Source_Drag_Data_Get'Access, Child);
   end Set_Dnd_Source;

end Gtkada.MDI;
