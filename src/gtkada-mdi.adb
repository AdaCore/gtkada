-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2001-2003 ACT-Europe                --
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
with Gtk.Table;        use Gtk.Table;
with Gtk.Handlers;
with Gtk.Label;        use Gtk.Label;
with Pango.Layout;     use Pango.Layout;
with Gtkada.Multi_Paned; use Gtkada.Multi_Paned;
with Gtk.Main;         use Gtk.Main;
pragma Elaborate_All (Gtk.Main);

with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtk.Notebook;     use Gtk.Notebook;
with Gtk.Object;
with Gtk.Pixmap;       use Gtk.Pixmap;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Style;        use Gtk.Style;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Tags;         use Ada.Tags;
with System;           use System;

with Ada.Exceptions; use Ada.Exceptions;
with GNAT.IO; use GNAT.IO;

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

   Max_Drag_Border_Width : constant Gint := 30;
   --  Width or height of the drag-and-drop borders for each notebook

   Icons_Width : constant Gint := 100;
   --  Width to use for icons

   Min_Width  : constant Gint := 40;
   --  Minimal size for all windows

   Threshold : constant Gint := 40;
   --  Threshold used to reset coordinates when putting items in the MDI.

   Corner_Size : constant Gint := Border_Thickness * 2;
   --  Extra tolerance when the user selects a corner for resizing (if the
   --  pointer is within Corner_Size in both coordinates, then we are clicking
   --  on the corner)

   MDI_Class_Record        : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;
   Child_Class_Record      : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;

   MDI_Signals : constant chars_ptr_array :=
     (1 => New_String ("child_selected"),
      2 => New_String ("float_child"),
      3 => New_String ("child_title_changed"));

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

   use Widget_List;

   type Selection_Dialog_Record is new Gtk_Window_Record with record
      Current_Child : Widget_List.Glist;
      Label         : Gtk_Label;
      Ent           : Gtk_Entry;
      Length        : Natural := 0;
      Modifier      : Gdk_Modifier_Type;
      Icon          : Gtk.Pixmap.Gtk_Pixmap;
   end record;
   type Selection_Dialog_Access is access all Selection_Dialog_Record'Class;

   type Children_Array is array (Natural range <>) of Widget_List.Glist;

   procedure Free is new
     Ada.Unchecked_Deallocation (UTF8_String, String_Access);

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

   procedure Internal_Close_Child
     (Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Internal version of Close, for a MDI_Child

   function Leave_Child
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  The pointer has left the mouse.

   function Create_Notebook return Gtk_Notebook;
   --  Create a notebook, and set it up for drag-and-drop

   function Side
     (Child : access MDI_Child_Record'Class; X, Y : Gint)
      return Gdk_Cursor_Type;
   --  Return the cursor to use depending on the coordinates (X, Y) inside
   --  child.

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

   procedure Iconify_Child (Child : access Gtk_Widget_Record'Class);
   --  Iconify a child (this act as toggles, for the title bar of all
   --  children).

   procedure Draw_Child
     (Child : access MDI_Child_Record'Class; Area : Gdk_Rectangle);
   function Draw_Child
     (Child : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean;
   --  Draw the child (and the title bar)

   procedure Realize_MDI (MDI : access Gtk_Widget_Record'Class);
   procedure Realize_MDI_Layout (MDI : access Gtk_Widget_Record'Class);
   --  Called when the child is realized.

   procedure Set_Dnd_Source
     (Widget : access Gtk_Widget_Record'Class;
      Child  : access Gtk_Widget_Record'Class);
   --  Setup a widget as either a source or a target for drag-and-drop ops.

   function Child_Drag_Begin
     (Child  : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Handlers for the various signals associated with drag-and-drop

   procedure Get_Dnd_Target
     (MDI       : access MDI_Window_Record'Class;
      Parent    : out Gtk_Widget;
      Rectangle : out Gdk_Rectangle;
      Side      : out Dock_Side);
   --  Return the widget that is the current target for dnd

   procedure Draw_Dnd_Rectangle (MDI : access MDI_Window_Record'Class);
   --  Draw the DND rectangle

   procedure Update_Dock_Menu (Child : access MDI_Child_Record'Class);
   procedure Update_Float_Menu (Child : access MDI_Child_Record'Class);
   --  Update the state of the "Float" menu item associated with child.

   procedure Put_In_Notebook
     (MDI : access MDI_Window_Record'Class;
      Side : Dock_Side;
      Child : access MDI_Child_Record'Class;
      Notebook : Gtk_Notebook := null);
   --  Remove Child from MDI, and put it under control of a dock box, on the
   --  specific Side.
   --  Notebook can be used to specify a specific notebook to which the child
   --  should be added

   function Get_Notebook
     (Child : access MDI_Child_Record'Class) return Gtk_Notebook;
   --  Return the notebook that directly contains Child

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
   procedure Split_H_Cb     (MDI   : access Gtk_Widget_Record'Class);
   procedure Split_V_Cb     (MDI   : access Gtk_Widget_Record'Class);
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
   procedure Set_Focus_Child_Notebook
     (Note : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when the widget that has the keyboard focus has changed. This is
   --  used to automatically select its parent MDI_Child.

   function Set_Focus_Child_MDI_Floating
     (Child : access Gtk_Widget_Record'Class) return Boolean;
   --  Same as Set_Focus_Child_MDI, but for floating windows

   procedure Give_Focus_To_Child (Child : MDI_Child);
   --  Give the focus to a specific MDI child
   --  You should never call Grab_Focus directly

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

   procedure Update_Tab_Label
     (Child : access MDI_Child_Record'Class);
   --  Return the tab to use in the notebooks containing Child

   procedure Update_Menu_Item (Child : access MDI_Child_Record'Class);
   --  Update the menu entry for Child

   function Find_Current_In_Central
     (MDI : access MDI_Window_Record'Class) return Gtk_Notebook;
   --  Return the notebook that last had the focus in the central area, or null
   --  if there is nothing in the central area.

   procedure Removed_From_Notebook
     (Note : access Gtk_Widget_Record'Class; Args  : Gtk_Args);
   --  Called when a child is removed from one of the notebooks or a paned in
   --  the central area.

   procedure Emit_By_Name_Child
     (Object : System.Address; Name : String; Child : System.Address);
   pragma Import (C, Emit_By_Name_Child, "g_signal_emit_by_name");

   ------------------
   -- Get_Notebook --
   ------------------

   function Get_Notebook
     (Child : access MDI_Child_Record'Class) return Gtk_Notebook is
   begin
      case Child.State is
         when Docked    => return Child.MDI.Docks (Child.Dock);
         when Floating  => return null;
         when Iconified => return null;
         when Normal    =>
            if Child.MDI.Central.Children_Are_Maximized
              and then Get_Parent (Child) /= null
              and then Get_Parent (Child).all in Gtk_Notebook_Record'Class
            then
               return Gtk_Notebook (Get_Parent (Child));
            end if;

            return null;
      end case;
   end Get_Notebook;

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

      --  No need to call the parent's set_focus_child, this is called
      --  automatically when the signal is propagated.
   end Set_Focus_Child_MDI;

   ------------------------------
   -- Set_Focus_Child_Notebook --
   ------------------------------

   procedure Set_Focus_Child_Notebook
     (Note : access Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      pragma Unreferenced (Note);
      Widget : constant Gtk_Widget := Gtk_Widget (To_Object (Args, 1));
   begin
      if Widget /= null then
         Set_Focus_Child (MDI_Child (Widget));
      end if;
   end Set_Focus_Child_Notebook;

   ----------------------------------
   -- Set_Focus_Child_MDI_Floating --
   ----------------------------------

   function Set_Focus_Child_MDI_Floating
     (Child : access Gtk_Widget_Record'Class) return Boolean is
   begin
      --  Let the even through if the child already has the focus. This way,
      --  the notebook tab of the focus child can still be used for
      --  drag-and-drop
      if MDI_Child (Child).MDI.Focus_Child = MDI_Child (Child) then
         return False;

      else
         Set_Focus_Child (MDI_Child (Child));

         --  We must return True here, to stop the propagation. This function
         --  is called as a result of a button_press event in the notebook's.
         --  tabs The call to Set_Focus_Child above raises the child and gives
         --  it the focus appropriately. However, if we let the signal go
         --  through it will be handled by the notebook, which will not see a
         --  change in the current page, and will give the focus to the tab
         --  itself, not to the page's contents.

         return True;
      end if;
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
        (1 => (1 => GType_Pointer),
         2 => (1 => GType_Pointer),
         3 => (1 => GType_Pointer));
      Drop_Site : Gtk_Event_Box;
   begin
      Gtk.Table.Initialize (MDI, 3, 3, False);

      Gtk_New (MDI.Main_Pane);
      Attach (MDI, MDI.Main_Pane, 1, 2, 1, 2);

      --  The MDI must have a window, so that we can change the background
      --  color. No other notebook or paned inside has a window
      Set_Has_Window (MDI.Main_Pane, True);

      MDI.Group := Gtk_Accel_Group (Group);

      MDI.Title_Layout := Create_Pango_Layout (MDI, "Ap"); -- compute width
      MDI.Background_Color := Parse (Default_MDI_Background_Color);
      Alloc (Get_Default_Colormap, MDI.Background_Color);

      MDI.Title_Bar_Color := Parse (Default_Title_Bar_Color);
      Alloc (Get_Default_Colormap, MDI.Title_Bar_Color);

      MDI.Focus_Title_Color := Parse (Default_Title_Bar_Focus_Color);
      Alloc (Get_Default_Colormap, MDI.Focus_Title_Color);

      Gtk.Object.Initialize_Class_Record
        (MDI,
         Signals      => MDI_Signals,
         Class_Record => MDI_Class_Record,
         Type_Name    => "GtkAdaMDI",
         Parameters   => Signal_Parameters);

      Gtk_New (Drop_Site);
      Attach
        (MDI, Drop_Site, 0, 3, 0, 1,
         Xoptions => Gtk.Enums.Fill, Yoptions => 0);
      Set_Size_Request (Drop_Site, -1, Drop_Area_Thickness);

      MDI.Docks (Top) := Create_Notebook;
      Add_Child
        (MDI.Main_Pane, MDI.Docks (Top), Orientation_Vertical, Height => -1,
         Fixed_Size => True);
      Set_Child_Visible (MDI.Docks (Top), False);

      MDI.Docks (Bottom) := Create_Notebook;
      Split
        (MDI.Main_Pane, MDI.Docks (Top), MDI.Docks (Bottom),
         Orientation_Vertical, Height => -1, Fixed_Size => True);
      Set_Child_Visible (MDI.Docks (Bottom), False);

      Gtk_New (Drop_Site);
      Attach
        (MDI, Drop_Site, 0, 3, 2, 3,
         Xoptions => Gtk.Enums.Fill, Yoptions => 0);
      Set_Size_Request (Drop_Site, -1, Drop_Area_Thickness);

      Gtk_New (Drop_Site);
      Attach
        (MDI, Drop_Site, 0, 1, 0, 3,
         Xoptions => 0, Yoptions => Gtk.Enums.Fill);
      Set_Size_Request (Drop_Site, Drop_Area_Thickness, -1);

      MDI.Docks (Left) := Create_Notebook;
      Split
        (MDI.Main_Pane, MDI.Docks (Top), MDI.Docks (Left),
         Orientation_Vertical, Width => -1, Fixed_Size => True);
      Set_Child_Visible (MDI.Docks (Left), False);

      Gtk_New (MDI.Central.Container);
      Split
        (MDI.Main_Pane, MDI.Docks (Left), MDI.Central.Container,
         Orientation_Horizontal);

      Add_Child (MDI.Central.Container, Create_Notebook);
      Set_Child_Visible (MDI.Central.Container, False);

      Gtk_New (MDI.Central.Layout);

      --  The layout should have a window, otherwise its children will
      --  overlap the items from the MDI when they are too big (this seems to
      --  be a bug in gtk+...)
      Set_Has_Window (MDI.Central.Layout, True);
      Split (MDI.Main_Pane, MDI.Central.Container, MDI.Central.Layout,
             Orientation_Horizontal);

      MDI.Docks (Right) := Create_Notebook;
      Split
        (MDI.Main_Pane, MDI.Central.Layout, MDI.Docks (Right),
         Orientation_Horizontal, Width => -1, Fixed_Size => True);
      Set_Child_Visible (MDI.Docks (Right), False);

      Gtk_New (Drop_Site);
      Attach
        (MDI, Drop_Site, 2, 3, 0, 3,
         Xoptions => 0, Yoptions => Gtk.Enums.Fill);
      Set_Size_Request (Drop_Site, Drop_Area_Thickness, -1);

      Configure (MDI,
                 Opaque_Resize     => True,
                 Opaque_Move       => True,
                 Background_Color  => MDI.Background_Color,
                 Title_Bar_Color   => MDI.Title_Bar_Color,
                 Focus_Title_Color => MDI.Focus_Title_Color);

      Widget_Callback.Connect
        (MDI, "realize", Widget_Callback.To_Marshaller (Realize_MDI'Access));
      Widget_Callback.Object_Connect
        (MDI.Central.Layout, "realize",
         Widget_Callback.To_Marshaller (Realize_MDI_Layout'Access), MDI);
      Widget_Callback.Connect
        (MDI, "destroy", Widget_Callback.To_Marshaller (Destroy_MDI'Access));
      Widget_Callback.Connect
        (MDI, "set_focus_child", Set_Focus_Child_MDI'Access);
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
      Str : constant UTF8_String := Get_Text (D.Ent);
      Children : constant Children_Array :=
        Matching_Children (MDI, To_Lower (Str));
      Index : Integer := Children'First;
      Tmp   : Integer;

   begin
      --  Update graphically the list of children matching the filter

      D.Length := Str'Length;
      Append_Text (D.Ent, " {");
      Set_Position (D.Ent, Gint (D.Length));

      --  Find the index of the current child
      if Children'Length /= 0 then
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

         Tmp := Index;
         loop
            if Tmp /= Index then
               Append_Text (D.Ent, ",");
            end if;

            Append_Text
              (D.Ent, Get_Short_Title (MDI_Child (Get_Data (Children (Tmp)))));

            Tmp := (Tmp + 1 - Children'First) mod Children'Length
              + Children'First;
            exit when Tmp = Index;
         end loop;

         D.Current_Child := Children (Index);

      else
         D.Current_Child := Null_List;
      end if;

      Append_Text (D.Ent, "}");

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
            Set_Text (D.Label, Get_Short_Title (C));

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
      D : constant Selection_Dialog_Access :=
        Selection_Dialog_Access (M.Selection_Dialog);
      Close : Boolean := False;
      Tmp   : Boolean;
      Key   : Gdk_Key_Type;
      pragma Unreferenced (Tmp);
   begin
      --  This isn't a key press for the next_child or previous_child
      --  functions, since those are handled by the outside application.

      if Get_Event_Type (Event) = Key_Press then
         Key := Get_Key_Val (Event);

         if Key = Gdk.Types.Keysyms.GDK_BackSpace
           or else Key = Gdk.Types.Keysyms.GDK_Delete
         then
            Delete_Text (D.Ent, Gint (D.Length) - 1, -1);
         else
            Delete_Text (D.Ent, Gint (D.Length), -1);

            Set_State (Event, 0);
            Tmp := Return_Callback.Emit_By_Name
              (D.Ent, "key_press_event", Event);
         end if;

         Update_Selection_Dialog (M, 0);
         return True;

      elsif Get_Event_Type (Event) = Key_Release then
         Key := Get_Key_Val (Event);

         --  As soon as one of the modifiers of the initial key is released,
         --  we close the dialog
         if (D.Modifier and Control_Mask) /= 0
           and then
             (Key = Gdk.Types.Keysyms.GDK_Control_L
              or else Key = Gdk.Types.Keysyms.GDK_Control_R)
         then
            Close := True;
         end if;

         if (D.Modifier and Mod1_Mask) /= 0
         and then (Key = Gdk.Types.Keysyms.GDK_Meta_L
                   or else Key = Gdk.Types.Keysyms.GDK_Meta_R
                   or else Key = Gdk.Types.Keysyms.GDK_Alt_L
                   or else Key = Gdk.Types.Keysyms.GDK_Alt_R)
         then
            Close := True;

         elsif Key = Gdk.Types.Keysyms.GDK_Escape then
            Close := True;
         end if;

         if Close then
            if D.Current_Child /= Null_List then
               Set_Focus_Child
                 (MDI_Child (Widget_List.Get_Data (D.Current_Child)));
            end if;

            Keyboard_Ungrab (Time => 0);
            Grab_Remove (M.Selection_Dialog);
            Destroy (M.Selection_Dialog);
            M.Selection_Dialog := null;
         end if;

         return True;
      end if;

      return False;
   end Key_Event_Selection_Dialog;

   ----------------------------------------
   -- Check_Interactive_Selection_Dialog --
   ----------------------------------------

   procedure Check_Interactive_Selection_Dialog
     (MDI    : access MDI_Window_Record;
      Event  : Gdk.Event.Gdk_Event;
      Move_To_Next : Boolean;
      Visible_In_Central_Only : Boolean := False)
   is
      use type Widget_List.Glist;
      D : Selection_Dialog_Access;
      Box, HBox : Gtk_Box;
      Frame : Gtk_Frame;
      Tmp : Gdk_Grab_Status;
      pragma Unreferenced (Tmp);

   begin
      if MDI.Items = Null_List then
         return;
      end if;

      if Event = null then
         declare
            List, Tmp : Widget_List.Glist;
            Notebook  : Gtk_Notebook;
            Child     : MDI_Child;
         begin
            if not Visible_In_Central_Only then
               if Move_To_Next then
                  List := Next (First (MDI.Items));
               else
                  List := Last (MDI.Items);
               end if;

               if List /= Null_List then
                  Set_Focus_Child (MDI_Child (Widget_List.Get_Data (List)));
               end if;

            elsif MDI.Central.Children_Are_Maximized then
               List := Get_Children (MDI.Central.Container);
               Notebook := Find_Current_In_Central (MDI);
               Tmp  := List;

               while Tmp /= Null_List loop
                  if Widget_List.Get_Data (Tmp) = Gtk_Widget (Notebook) then
                     if Move_To_Next then
                        Tmp := Widget_List.Next (Tmp);
                        if Tmp = Null_List then
                           Tmp := Widget_List.First (List);
                        end if;
                     else
                        Tmp := Widget_List.Prev (Tmp);
                        if Tmp = Null_List then
                           Tmp := Widget_List.Last (List);
                        end if;
                     end if;

                     Notebook := Gtk_Notebook (Widget_List.Get_Data (Tmp));
                     Child := MDI_Child
                       (Get_Nth_Page
                          (Notebook, Get_Current_Page (Notebook)));

                     if Child /= null then
                        Set_Focus_Child (Child);
                     end if;

                     exit;
                  end if;

                  Tmp := Widget_List.Next (Tmp);
               end loop;

               Free (List);
            end if;
         end;
         return;
      end if;

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

         Show_All (D);
         D.Modifier := Get_State (Event);

         MDI.Selection_Dialog := Gtk_Widget (D);

         --  Make sure all the key events are forwarded to us, as otherwise
         --  if the mouse was moving out of the window we wouldn't the
         --  events
         Tmp := Keyboard_Grab (Get_Window (D), True, Time => 0);
         Grab_Add (D);

         Grab_Focus (D.Ent);

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

      if Move_To_Next then
         Update_Selection_Dialog (MDI, +1);
      else
         Update_Selection_Dialog (MDI, -1);
      end if;
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
      Desc            : Pango_Font_Description;
      W, H            : Gint;
      List            : Widget_List.Glist;
      C               : MDI_Child;
      Highlight_Style : Gtk_Style;
      Need_Redraw     : Boolean := False;

   begin
      MDI.Opaque_Resize := Opaque_Resize;
      MDI.Opaque_Move   := Opaque_Move;
      MDI.Opaque_Docks  := Opaque_Docks;
      MDI.Close_Floating_Is_Unfloat := Close_Floating_Is_Unfloat;

      Set_Opaque_Resizing (MDI.Main_Pane, Opaque_Resize);
      Set_Opaque_Resizing (MDI.Central.Container, Opaque_Resize);

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

      if MDI.Highlight_Style /= null then
         Unref (MDI.Highlight_Style);
      end if;

      Ensure_Style (MDI);
      Highlight_Style := Get_Style (MDI);

      --  Apparently calling Ensure_Style is not sufficient on some systems.
      --  For example, Gtk+ under Solaris 2.5.1 will set Highlight_Style to
      --  null, so we have to compensate.

      --  On the other hand, we cannot use Get_Default_Style systematically,
      --  since under Windows if we do not take the style associated with MDI,
      --  we will end up using a different font when using Highlight_Style.

      if Highlight_Style = null then
         Highlight_Style := Get_Default_Style;
      end if;

      MDI.Highlight_Style := Copy (Highlight_Style);

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
            Set_Background (Get_Window (MDI.Main_Pane), Background_Color);

            if Realized_Is_Set (MDI.Central.Layout) then
               Set_Background
                 (Get_Window (MDI.Central.Layout), Background_Color);
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
      Gdk.Window.Set_Background
        (Get_Window (M.Central.Layout), M.Background_Color);
   end Realize_MDI_Layout;

   -----------------
   -- Realize_MDI --
   -----------------

   procedure Realize_MDI (MDI : access Gtk_Widget_Record'Class) is
      Window_Attr : Gdk.Window_Attr.Gdk_Window_Attr;
      M           : MDI_Window := MDI_Window (MDI);
      Cursor      : Gdk_Cursor;

   begin
      Realize (M.Main_Pane);
      Gdk.Window.Set_Background (Get_Window (M.Main_Pane), M.Background_Color);

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

      --  Destroy the window attribute and the cursor

      Destroy (Cursor);
      Destroy (Window_Attr);
      Queue_Resize (MDI);
   end Realize_MDI;


   -----------------
   -- Destroy_MDI --
   -----------------

   procedure Destroy_MDI (MDI : access Gtk_Widget_Record'Class) is
      use Widget_List;

      Tmp : Widget_List.Glist := First (MDI_Window (MDI).Items);
      N   : Widget_List.Glist;

   begin
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
      when E : others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the button in Initialize
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
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
      MDI    : constant MDI_Window := MDI_Window (Child.MDI);
      Event  : Gdk_Event;
      Item   : Widget_List.Glist;
      It     : MDI_Child;
      Dock   : Dock_Side;
   begin
      --  Don't do anything for now if the MDI isn't realized, since we
      --  can't send create the event anyway.

      if Realized_Is_Set (MDI) then
         Allocate (Event, Delete, Get_Window (MDI));

         --  For a top-level window, we must rebuild the initial widget
         --  temporarily, so that the application can do all the test it wants.
         --  However, we need to restore the initial state before calling
         --  Dock_Child and Float_Child below

         if Force or else not Return_Callback.Emit_By_Name
           (Child.Initial, "delete_event", Event)
         then
            case Child.State is
               when Normal | Iconified | Floating => Dock := None;
               when Docked                        => Dock := Child.Dock;
            end case;

            Float_Child (Child, False);

            if MDI_Child (Child) = MDI.Focus_Child then
               --  Set the focus on the child that had the focus just before,
               --  and in the same notebook.

               Item := MDI.Items;
               while Item /= Widget_List.Null_List loop
                  It := MDI_Child (Get_Data (Item));

                  if It /= MDI_Child (Child)
                    and then
                      ((Dock = None and then It.State = Normal)
                       or else (Dock /= None
                                and then It.State = Docked
                                and then It.Dock = Dock))
                  then
                     Set_Focus_Child (It);
                     exit;
                  end if;

                  Item := Widget_List.Next (Item);
               end loop;
            end if;

            Destroy (Child);
         end if;
      end if;

   exception
      when E : others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the button in Initialize
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
         null;
   end Close_Child;

   -------------------
   -- Destroy_Child --
   -------------------

   procedure Destroy_Child (Child : access Gtk_Widget_Record'Class) is
      use type Widget_SList.GSlist;
      C : MDI_Child := MDI_Child (Child);
      MDI : constant MDI_Window := C.MDI;
   begin
      --  We know at that stage that Child has already been unparent-ed

      pragma Assert (Get_Parent (Child) = null);

      Ref (C);

      C.Tab_Label := null;

      --  The child of the MDI_Child has now been taken care of, thus we need
      --  to take care of the MDI_Child itself now.

      if C.Menu_Item /= null then
         Destroy (C.Menu_Item);
      end if;

      if not Gtk.Object.In_Destruction_Is_Set (C.MDI) then
         --  Initial could be null if we are destroying a floating
         --  child explicitly (by closing its X11 window)
         if C.Initial /= null then
            --  Do not unfloat the child, since the toplevel is no longer a
            --  Gtk_Window, and we would get a CE in Float_Child.
            Dock_Child (C, False);
         end if;

         if Get_Parent (C) /= null then
            Remove (Gtk_Container (Get_Parent (C)), C);
         end if;
      end if;

      if Get_Parent (C.Initial) /= null then
         Remove (Gtk_Container (Get_Parent (C.Initial)), C.Initial);
      end if;

      C.Initial := null;

      Free (C.Title);
      Free (C.Short_Title);

      --  Do not transfer the focus elsewhere: for an interactive close, this
      --  is done in Close_Child, otherwise we do not want to change the focus
      if C = MDI.Focus_Child then
         MDI.Focus_Child := null;
      end if;

      --  Only remove it from the list of children at the end, since some of
      --  calls above might result in calls to Raise_Child_Idle, which tries
      --  to manipulate that list.
      Widget_List.Remove (C.MDI.Items, Gtk_Widget (C));

      --  Destroy the child, unless the user has explicitely kept a Ref on it
      --  (therefore, do not use Destroy, only Unref). In all cases, it should
      --  be hidden on the screen
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

      Set_Text (Child.MDI.Title_Layout, Child.Title.all);
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
      pragma Unreferenced (Tmp);
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
            Maximize_Children (MDI, not MDI.Central.Children_Are_Maximized);
         end if;

         return False;
      end if;

      if Get_Event_Type (Event) /= Button_Press then
         return False;
      end if;

      if Get_Button (Event) = 3 then
         Lower_Child (C);
         return True;
      elsif Get_Button (Event) /= 1 then
         return False;
      end if;

      --  Focus and raise the child. Raise_Child must be called explicitely
      --  since Set_Focus_Child won't do it if the child already has the focus.
      Set_Focus_Child (C);
      Raise_Child (C);

      --  Do we have a drag-and-drop operation ? This is true if we are
      --  pressing control, or simply clicking in a maximized or docked
      --  child (otherwise, moving items in the layout would interfer with
      --  dnd).

      if (Get_State (Event) and Control_Mask) /= 0
        or else (C.State /= Normal and then C.State /= Iconified)
        or else C.MDI.Central.Children_Are_Maximized
      then
         return Child_Drag_Begin (C, Event);
      end if;

      --  Can't move items inside a notebook
      if C.State = Docked
        or else (C.State = Normal and then MDI.Central.Children_Are_Maximized)
      then
         return True;
      end if;

      --  We are now in a child in the layout

      MDI.Drag_Start_X := Gint (Get_X_Root (Event));
      MDI.Drag_Start_Y := Gint (Get_Y_Root (Event));

      MDI.Selected_Child := C;

      MDI.Initial_Width := Gint (Get_Allocation_Width (Child));
      MDI.Initial_Height := Gint (Get_Allocation_Height (Child));
      MDI.Dnd_Rectangle := (C.X, C.Y, MDI.Initial_Width, MDI.Initial_Height);

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

      if not MDI.Central.Children_Are_Maximized
        and then
        ((not MDI.Opaque_Resize and then MDI.Current_Cursor /= Left_Ptr)
         or else (not MDI.Opaque_Move and then MDI.Current_Cursor = Left_Ptr))
      then
         Draw_Rectangle
           (Get_Window (MDI.Central.Layout),
            MDI.Xor_GC,
            Filled => False,
            X => MDI.Dnd_Rectangle.X,
            Y => MDI.Dnd_Rectangle.Y,
            Width => MDI.Dnd_Rectangle.Width,
            Height => MDI.Dnd_Rectangle.Height);
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
      C     : constant MDI_Child := MDI_Child (Child);
      C2    : MDI_Child;
      MDI   : MDI_Window := C.MDI;
      Alloc : Gtk_Allocation;

      Buttons_Width : constant := 100;
      --  Approximative width of the three title bar buttons

      Minimal : constant := 10;
      Side   : Dock_Side;
      Current : Gtk_Widget;
      Found  : Boolean := False;
      Note   : Gtk_Notebook;
   begin
      if Get_Window (Child) /= Get_Window (Event) then
         return False;
      end if;

      Pointer_Ungrab (Time => 0);

      case C.MDI.In_Drag is
         when In_Pre_Drag =>
            C.MDI.In_Drag := No_Drag;
            return True;

         when In_Drag =>
            Draw_Dnd_Rectangle (C.MDI);
            Get_Dnd_Target (C.MDI, Current, C.MDI.Dnd_Rectangle, Side);

            C2 := Dnd_Data
              (C, Copy => (Get_State (Event) and Shift_Mask) /= 0);

            if Current = null then
               --  Floating child ?
               Float_Child (C2, True);
            elsif Current = Gtk_Widget (C.MDI) then
               --  Dropped on one of the borders of the MDI ?
               if C2.Dock /= Side or else C2.State /= Docked then
                  Dock_Child (C2, False);
                  C2.Dock := Side;
                  Dock_Child (C2, True);
               end if;
            elsif Current = Gtk_Widget (C.MDI.Central.Layout)
              or else Current = Gtk_Widget (C.MDI.Central.Container)
            then
               --  Dropped in the layout ?
               Dock_Child (C2, False);
            else
               --  Dropped in a dock ?
               for S in C.MDI.Docks'Range loop
                  if Gtk_Widget (C.MDI.Docks (S)) = Current then
                     if C2.Dock /= S or else C2.State /= Docked then
                        Dock_Child (C2, False);
                        C2.Dock := S;
                        Dock_Child (C2, True);
                     end if;
                     Found := True;
                  end if;
               end loop;

               --  Dropped in one of the central notebooks
               if not Found then
                  --  Do nothing if the child is already in the middle area,
                  --  and in a notebook that contains only one child, and the
                  --  user is dropping on the same notebook

                  if C2.State /= Normal
                    or else Current /= Get_Parent (C2)
                    or else Get_Nth_Page (Gtk_Notebook (Current), 1) /= null
                  then
                     Dock_Child (C2, False);

                     Ref (C2);
                     Remove (Gtk_Container (Get_Parent (C2)), C2);
                     if Side = None then
                        Note := Gtk_Notebook (Current);
                     else
                        Note := Create_Notebook;
                     end if;

                     Put_In_Notebook (C.MDI, None, C2, Note);

                     case Side is
                        when None =>
                           null;
                        when Left =>
                           Split
                             (C.MDI.Central.Container,
                              Current, Note, Orientation_Horizontal,
                              After => False);
                        when Right =>
                           Split
                             (C.MDI.Central.Container,
                              Current, Note, Orientation_Horizontal,
                              After => True);
                        when Top =>
                           Split
                             (C.MDI.Central.Container,
                              Current, Note, Orientation_Vertical,
                              After => False);
                        when Bottom =>
                           Split
                             (C.MDI.Central.Container,
                              Current, Note, Orientation_Vertical,
                              After => True);
                     end case;
                     Unref (C2);
                  end if;
               end if;
            end if;

            Raise_Child (C2);
            C.MDI.In_Drag := No_Drag;
            return True;

         when No_Drag =>
            null;
      end case;

      C.MDI.In_Drag := No_Drag;

      if MDI.Selected_Child = null then
         return False;
      end if;

      Alloc :=
        (MDI.Dnd_Rectangle.X, MDI.Dnd_Rectangle.Y,
         Allocation_Int (MDI.Dnd_Rectangle.Width),
         Allocation_Int (MDI.Dnd_Rectangle.Height));

      if Alloc.X + Alloc.Width < Buttons_Width then
         Alloc.X := Buttons_Width - Alloc.Width;
      elsif Alloc.X > Get_Allocation_Width (MDI.Central.Layout) - Minimal then
         Alloc.X := Get_Allocation_Width (MDI.Central.Layout) - Minimal;
      end if;

      if Alloc.Y + MDI.Title_Bar_Height < Minimal then
         Alloc.Y := Minimal - MDI.Title_Bar_Height;
      elsif Alloc.Y > Get_Allocation_Height (MDI.Central.Layout) - Minimal then
         Alloc.Y := Get_Allocation_Height (MDI.Central.Layout) - Minimal;
      end if;

      if not MDI.Central.Children_Are_Maximized
        and then ((not MDI.Opaque_Resize
                    and then MDI.Current_Cursor /= Left_Ptr)
          or else (not MDI.Opaque_Move and then MDI.Current_Cursor = Left_Ptr))
      then
         Draw_Rectangle
           (Get_Window (MDI.Central.Layout),
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
      Set_Size_Request (Child, Alloc.Width, Alloc.Height);
      Move (MDI.Central.Layout, Child, Alloc.X, Alloc.Y);

      --  No size requested for the layout, since otherwise it will be
      --  dynamically resized when items are moved.
      Set_Size_Request (MDI.Central.Layout, 0, 0);

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
      Current : Gtk_Widget;
      S       : Dock_Side;
      Rect2   : Gdk_Rectangle;
      Tmp     : Gdk_Grab_Status;
      pragma Unreferenced (Tmp);

   begin
      if Get_Window (Child) /= Get_Window (Event) then
         return False;
      end if;

      case C.MDI.In_Drag is
         when In_Drag =>
            Get_Dnd_Target
              (C.MDI, Parent => Current, Rectangle => Rect2, Side => S);

            if Current = null then
               Draw_Dnd_Rectangle (C.MDI);
               C.MDI.Dnd_Rectangle_Owner := null;
            elsif Rect2 /= C.MDI.Dnd_Rectangle
              or else C.MDI.Dnd_Rectangle_Owner /= Get_Window (Current)
            then
               Draw_Dnd_Rectangle (C.MDI);
               C.MDI.Dnd_Rectangle := Rect2;
               C.MDI.Dnd_Rectangle_Owner := Get_Window (Current);
               Draw_Dnd_Rectangle (C.MDI);
            end if;

            return True;

         when In_Pre_Drag =>
            if Gtk.Dnd.Check_Threshold
              (C, C.MDI.Drag_Start_X, C.MDI.Drag_Start_Y,
               Gint (Get_X (Event)), Gint (Get_Y (Event)))
            then
               C.MDI.In_Drag := In_Drag;
               C.MDI.Dnd_Rectangle_Owner := null;
               Pointer_Ungrab (Time => 0);

               Gdk_New (Cursor, Fleur);
               Tmp := Pointer_Grab
                 (Get_Window (C),
                  False, Button_Motion_Mask or Button_Release_Mask,
                  Cursor => Cursor,
                  Time   => 0);
               Unref (Cursor);
               return True;
            end if;

         when others =>
            null;
      end case;

      --  A button_motion event ?

      if (Get_State (Event) and Button1_Mask) /= 0
        and then MDI.Selected_Child /= null
        and then MDI.Drag_Start_X /= -1
      then
         if not MDI.Central.Children_Are_Maximized
           and then
           ((not MDI.Opaque_Resize and then MDI.Current_Cursor /= Left_Ptr)
            or else (not MDI.Opaque_Move
                     and then MDI.Current_Cursor = Left_Ptr))
         then
            Draw_Rectangle
              (Get_Window (MDI.Central.Layout),
               MDI.Xor_GC,
               Filled => False,
               X => MDI.Dnd_Rectangle.X,
               Y => MDI.Dnd_Rectangle.Y,
               Width => MDI.Dnd_Rectangle.Width,
               Height => MDI.Dnd_Rectangle.Height);
         end if;

         Delta_X := Gint (Get_X_Root (Event)) - MDI.Drag_Start_X;
         Delta_Y := Gint (Get_Y_Root (Event)) - MDI.Drag_Start_Y;
         W := MDI.Initial_Width;
         H := MDI.Initial_Height;

         MDI.Dnd_Rectangle.X := C.X;
         MDI.Dnd_Rectangle.Y := C.Y;

         case MDI.Current_Cursor is
            when Left_Ptr =>
               MDI.Dnd_Rectangle.X := Delta_X + C.X;
               MDI.Dnd_Rectangle.Y := Delta_Y + C.Y;

            when Left_Side =>
               W := Gint'Max (Min_Width, W - Delta_X);
               MDI.Dnd_Rectangle.X := C.X + Delta_X;

            when Right_Side =>
               W := Gint'Max (Min_Width, W + Delta_X);

            when Top_Side =>
               H := Gint'Max (Min_Height, H - Delta_Y);
               MDI.Dnd_Rectangle.Y := C.Y + Delta_Y;

            when Bottom_Side =>
               H := Gint'Max (Min_Height, H + Delta_Y);

            when Top_Left_Corner =>
               W := Gint'Max (Min_Width, W - Delta_X);
               H := Gint'Max (Min_Height, H - Delta_Y);
               MDI.Dnd_Rectangle.X := C.X + Delta_X;
               MDI.Dnd_Rectangle.Y := C.Y + Delta_Y;

            when Top_Right_Corner =>
               W := Gint'Max (Min_Width, W + Delta_X);
               H := Gint'Max (Min_Height, H - Delta_Y);
               MDI.Dnd_Rectangle.Y := C.Y + Delta_Y;

            when Bottom_Left_Corner =>
               W := Gint'Max (Min_Width, W - Delta_X);
               H := Gint'Max (Min_Height, H + Delta_Y);
               MDI.Dnd_Rectangle.X := C.X + Delta_X;

            when Bottom_Right_Corner =>
               W := Gint'Max (Min_Width, W + Delta_X);
               H := Gint'Max (Min_Height, H + Delta_Y);
            when others => null;
         end case;

         if MDI.Opaque_Move or else MDI.Opaque_Resize then
            if MDI.Current_Cursor /= Left_Ptr then
               MDI.Dnd_Rectangle.Width := W;
               MDI.Dnd_Rectangle.Height := H;

               --  Need to set these, or when the mouse is outside of the
               --  layout, the MDI will try to resize the child to the old
               --  dimensions even while the mouse is moving.
               C.Uniconified_Width  := W;
               C.Uniconified_Height := H;
            end if;

            Alloc :=
              (MDI.Dnd_Rectangle.X, MDI.Dnd_Rectangle.Y,
               Allocation_Int (MDI.Dnd_Rectangle.Width),
               Allocation_Int (MDI.Dnd_Rectangle.Height));
            Size_Allocate (Child, Alloc);

         elsif not MDI.Central.Children_Are_Maximized then
            MDI.Dnd_Rectangle.Width := W;
            MDI.Dnd_Rectangle.Height := H;
            Draw_Rectangle
              (Get_Window (MDI.Central.Layout),
               MDI.Xor_GC,
               Filled => False,
               X      => MDI.Dnd_Rectangle.X,
               Y      => MDI.Dnd_Rectangle.Y,
               Width  => MDI.Dnd_Rectangle.Width,
               Height => MDI.Dnd_Rectangle.Height);
         end if;

      --  A motion_event ? change the cursor if needed

      elsif C.State = Normal
        and then not MDI.Central.Children_Are_Maximized
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

   --------------
   -- Dnd_Data --
   --------------

   function Dnd_Data
     (Child : access MDI_Child_Record; Copy : Boolean) return MDI_Child
   is
      pragma Unreferenced (Copy);
   begin
      return MDI_Child (Child);
   end Dnd_Data;

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

      Set_Border_Width (Child, 0);

      Child.Initial := Gtk_Widget (Widget);
      Child.Uniconified_Width := -1;

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

      --  This internal Event box is needed when the child is floated
      Gtk_New (Event);
      Add (Event, Widget);
      Pack_Start (Box, Event, Expand => True, Fill => True, Padding => 0);

      Widget_Callback.Object_Connect
        (Child.Initial, "destroy",
         Widget_Callback.To_Marshaller (Destroy_Initial_Child'Access),
         Child);
   end Initialize;

   -------------------------
   -- Give_Focus_To_Child --
   -------------------------

   procedure Give_Focus_To_Child (Child : MDI_Child) is
      F : Gtk_Widget := Gtk_Widget (Child);
   begin
      if Child /= null then
         if Child.Focus_Widget /= null then
            F := Child.Focus_Widget;
         end if;

         --  If we can't give the focus to the focus widget, give it to
         --  child itself. This is better than keeping it on the previous
         --  child.

         if not Child_Focus (F, Dir_Tab_Forward) then
            Grab_Focus (Child);
         end if;
      end if;
   end Give_Focus_To_Child;

   ---------
   -- Put --
   ---------

   function Put
     (MDI   : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags : Child_Flags := All_Buttons;
      Focus_Widget : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Gint := -1) return MDI_Child
   is
      C           : MDI_Child;
   begin
      if Child.all in MDI_Child_Record'Class then
         C := MDI_Child (Child);
      else
         Gtk_New (C, Child, Flags);
      end if;

      C.MDI := MDI_Window (MDI);
      C.X   := MDI.Default_X;
      C.Y   := MDI.Default_Y;
      C.Focus_Widget := Focus_Widget;

      Set_USize (C.Title_Box, -1, MDI.Title_Bar_Height);

      if not MDI.Central.Children_Are_Maximized
        and then MDI.Default_X + Threshold >
        Gint (Get_Allocation_Width (MDI.Central.Layout))
      then
         MDI.Default_X := 10;
      else
         MDI.Default_X := MDI.Default_X + 10;
      end if;

      if not MDI.Central.Children_Are_Maximized
        and then MDI.Default_Y + Threshold >
        Gint (Get_Allocation_Height (MDI.Central.Layout))
      then
         MDI.Default_Y := 10;
      else
         MDI.Default_Y := MDI.Default_Y + 10;
      end if;

      C.Title       := new UTF8_String'(" ");
      C.Short_Title := new UTF8_String'(" ");

      --  We need to show the widget before inserting it in a notebook,
      --  otherwise the notebook page will not be made visible.

      Show_All (C);

      Widget_List.Prepend (MDI.Items, Gtk_Widget (C));

      if Default_Width /= -1 or else Default_Height /= -1 then
         Set_Size_Request (C, Default_Width, Default_Height);
      end if;

      --  If all items are maximized, add Child to the notebook

      if MDI.All_Floating_Mode then
         Float_Child (C, True);
      elsif MDI.Central.Children_Are_Maximized then
         --  Pretend C is not in the central area, since Put_In_Notebook needs
         --  to find the current child to know which notebook of the splitted
         --  area to use.
         C.State := Iconified;
         Put_In_Notebook (MDI, None, C);
      else
         Put (MDI.Central.Layout, C, C.X, C.Y);
      end if;

      if MDI.Menu /= null then
         Create_Menu_Entry (C);
      end if;

      --  Restore the keyboard focus, which might have been stolen if the new
      --  child was added to a notebook.

      Give_Focus_To_Child (MDI.Focus_Child);
      return C;
   end Put;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title (Child : access MDI_Child_Record) return UTF8_String is
   begin
      return Child.Title.all;
   end Get_Title;

   ---------------------
   -- Get_Short_Title --
   ---------------------

   function Get_Short_Title
     (Child : access MDI_Child_Record) return UTF8_String is
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

         Gtk_New (Label, Child.Short_Title.all);
         Set_Alignment (Label, 0.0, 0.5);
         Set_Accel_Widget (Label, Child.Menu_Item);
         Pack_Start (Box, Label,  Expand => True, Fill => True);

         Show_All (Box);
         Add (Child.Menu_Item, Box);

         Set_Accel_Path
           (Child.Menu_Item, "<gtkada>/window/child/" & Child.Short_Title.all,
            Child.MDI.Group);
      end if;
   end Update_Menu_Item;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
     (Child : access MDI_Child_Record;
      Icon  : Gdk.Pixbuf.Gdk_Pixbuf) is
   begin
      if Child.Icon /= null then
         Unref (Child.Icon);
      end if;
      Child.Icon := Icon;

      if Realized_Is_Set (Child) then
         --  Force a refresh of the title bar
         Draw
           (Child,
            (0, 0, Get_Allocation_Width (Child), Child.MDI.Title_Bar_Height));
      end if;

      Update_Menu_Item (Child);
      Update_Tab_Label (Child);
   end Set_Icon;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Child       : access MDI_Child_Record;
      Title       : UTF8_String;
      Short_Title : UTF8_String := "")
   is
      The_Title       : String_Access;
      The_Short_Title : String_Access;
      --  Those pointers are used to prevent problems when
      --  the Title parameter is in fact Child.Title
   begin
      The_Title := new UTF8_String'(Title);

      if Short_Title /= "" then
         The_Short_Title := new UTF8_String'(Short_Title);
      else
         The_Short_Title := new UTF8_String'(Title);
      end if;

      Free (Child.Title);
      Free (Child.Short_Title);

      Child.Title := The_Title;
      Child.Short_Title := The_Short_Title;

      if Child.State = Floating then
         Set_Title (Gtk_Window (Get_Toplevel (Child.Initial)),
                    Locale_From_UTF8 (Title));
      end if;

      Update_Tab_Label (Child);

      --  Update the menu, if it exists. We need to recreate the menu item to
      --  keep it sorted

      if Child.Menu_Item /= null then
         Destroy (Child.Menu_Item);
         Create_Menu_Entry (Child);
      end if;

      if Get_Window (Child) /= Null_Window then
         Queue_Draw (Child);
      end if;

      Emit_By_Name_Child
        (Get_Object (Child.MDI), "child_title_changed" & ASCII.NUL,
         Get_Object (Child));
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

   ----------------------------
   -- Find_MDI_Child_By_Name --
   ----------------------------

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
      Note : Gtk_Notebook;
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
        and then Child.MDI.Central.Children_Are_Maximized
      then
         Note := Get_Notebook (Child);
         Set_Current_Page (Note, Page_Num (Note, Child));

      elsif Realized_Is_Set (Child) then
         Gdk.Window.Lower (Get_Window (Child));

         if Child.State = Floating then
            Gdk.Window.Lower
              (Get_Window (Gtk_Window (Get_Toplevel (Child.Initial))));
         end if;
      end if;
   end Lower_Child;

   -----------------
   -- Raise_Child --
   -----------------

   procedure Raise_Child (Child : access MDI_Child_Record'Class) is
      Old_Focus : constant MDI_Child := Child.MDI.Focus_Child;
      Note : Gtk_Notebook;
   begin
      Ref (Child);
      Remove (Child.MDI.Items, Gtk_Widget (Child));
      Prepend (Child.MDI.Items, Gtk_Widget (Child));
      Unref (Child);

      --  For an docked item, we in fact want to raise its parent dock,
      --  and make sure the current page in that dock is the correct one.

      if Child.State = Docked
        or else (Child.State = Normal
                 and then Child.MDI.Central.Children_Are_Maximized)
      then
         Note := Get_Notebook (Child);
         Set_Current_Page (Note, Page_Num (Note, Child));

      elsif Child.State = Floating
        and then Realized_Is_Set (Child.Initial)
      then
         Present (Gtk_Window (Get_Toplevel (Child.Initial)));

      elsif Realized_Is_Set (Child) then
         Gdk.Window.Gdk_Raise (Get_Window (Child));
      end if;

      --  Give the focus to the Focus_Child, since the notebook page switch
      --  might have changed that.

      Give_Focus_To_Child (Old_Focus);
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

   ---------------------
   -- Set_Focus_Child --
   ---------------------

   procedure Set_Focus_Child (Child : access MDI_Child_Record'Class) is
      Old : constant MDI_Child := Child.MDI.Focus_Child;
      C   : constant MDI_Child := MDI_Child (Child);
      Tmp  : Boolean;
      pragma Unreferenced (Tmp);

   begin
      --  Be lazy. And avoid infinite loop when updating the MDI menu...

      if C = Old or else Gtk.Object.In_Destruction_Is_Set (C.MDI) then
         return;
      end if;

      Child.MDI.Focus_Child := C;

      --  Make sure the page containing Child in a notebook is put on top.
      --  Do not raise floating children, since this is the role of the window
      --  manager.

      if C.State /= Floating then
         Raise_Child (C);
      end if;

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

      if C.Menu_Item /= null then
         Set_Active (C.Menu_Item, True);
      end if;

      --  It would be nice to find the first child of C.Initial that
      --  accepts the keyboard focus. However, in the meantime, we at least
      --  want to make sure that no other widget has the focus. As a result,
      --  focus_in events will always be sent the next time the user selects a
      --  widget.

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

      Alloc.Width  := Get_Allocation_Width (MDI.Central.Layout);
      Alloc.Height := Get_Allocation_Height (MDI.Central.Layout);
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
            Set_Size_Request (C, W, H);
            Move (MDI.Central.Layout, C, C.X, C.Y);
            Level := Level + 1;
         end if;
      end loop;

      Queue_Resize (MDI.Central.Layout);
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
      Max_W, Max_H : Gint;

   begin
      if MDI.Central.Children_Are_Maximized then
         Max_W := Gint (Get_Allocation_Width (MDI.Central.Container));
         Max_H := Gint (Get_Allocation_Height (MDI.Central.Container));
         Maximize_Children (MDI, False);

      else
         Max_W := Gint (Get_Allocation_Width (MDI.Central.Layout));
         Max_H := Gint (Get_Allocation_Height (MDI.Central.Layout));
      end if;

      if List = Null_List then
         return;
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
            Move (MDI.Central.Layout, C, C.X, C.Y);
            Set_Size_Request (C, W, H);
            Level := Level + W;
         end if;
      end loop;

      Queue_Resize (MDI.Central.Layout);
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
      Max_W, Max_H : Gint;

   begin
      if MDI.Central.Children_Are_Maximized then
         Max_W := Gint (Get_Allocation_Width (MDI.Central.Container));
         Max_H := Gint (Get_Allocation_Height (MDI.Central.Container));
         Maximize_Children (MDI, False);

      else
         Max_W := Gint (Get_Allocation_Width (MDI.Central.Layout));
         Max_H := Gint (Get_Allocation_Height (MDI.Central.Layout));
      end if;

      if List = Null_List then
         return;
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
            Move (MDI.Central.Layout, C, C.X, C.Y);
            Set_Size_Request (C, W, H);
            Level := Level + H;
         end if;

         List := Widget_List.Next (List);
      end loop;

      Queue_Resize (MDI.Central.Layout);
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
      Requisition : Gtk_Requisition;
   begin
      if Child.State /= Floating and then Float then
         --  Ref is removed when the child is unfloated.
         Ref (Child);

         if Realized_Is_Set (Child) then
            Child.Uniconified_Width  :=
              Gint (Get_Allocation_Width (Get_Widget (Child)));
            Child.Uniconified_Height :=
              Gint (Get_Allocation_Height (Get_Widget (Child)));
         else
            Size_Request (Child, Requisition);
            Child.Uniconified_Width  := Requisition.Width;
            Child.Uniconified_Height := Requisition.Height;
         end if;

         Minimize_Child (Child, False);
         Dock_Child (Child, False);

         --  This could be called before the child even has a parent if
         --  All_Floating_Mode is set.
         if Get_Parent (Child) /= null then
            if Child.MDI.Central.Children_Are_Maximized then
               Remove (Get_Notebook (Child), Child);
            else
               Remove (Child.MDI.Central.Layout, Child);
            end if;
         end if;

         if (Child.Flags and Float_As_Transient) /= 0 then
            Gtk_New (Diag,
                     Title  => Child.Title.all,
                     Parent => Gtk_Window (Get_Toplevel (Child.MDI)),
                     Flags  => No_Separator or Destroy_With_Parent);
            Set_Has_Separator (Diag, False);
            Win  := Gtk_Window (Diag);
            Cont := Gtk_Container (Get_Vbox (Diag));
         else
            Gtk_New (Win);
            Set_Title (Win, Child.Title.all);
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
         Emit_By_Name_Child (Get_Object (Child.MDI), "float_child" & ASCII.NUL,
                             Get_Object (Child));
         Widget_Callback.Emit_By_Name (Child, "float_child");

      elsif Child.State = Floating and then not Float then
         --  Reassign the widget to Child instead of the notebook

         Win := Gtk_Window (Get_Toplevel (Child.Initial));
         Reparent (Get_Child (Win), Gtk_Box (Get_Child (Child)));
         Child.State := Normal;

         Destroy (Win);

         if Child.MDI.Central.Children_Are_Maximized then
            Put_In_Notebook (Child.MDI, None, Child);
         else
            Put (Child.MDI.Central.Layout, Child, Child.X, Child.Y);
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

   ---------------------
   -- Create_Notebook --
   ---------------------

   function Create_Notebook return Gtk_Notebook is
      Notebook : Gtk_Notebook;
   begin
      Gtk_New (Notebook);
      Set_Tab_Pos (Notebook, Pos_Bottom);
      Set_Show_Tabs (Notebook, False);
      Set_Show_Border (Notebook, False);
      Set_Border_Width (Notebook, 0);
      Set_Scrollable (Notebook);

      Widget_Callback.Connect
        (Notebook, "remove", Removed_From_Notebook'Access);
      Widget_Callback.Connect
        (Notebook, "set_focus_child", Set_Focus_Child_Notebook'Access);
      return Notebook;
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
      Note  : constant Gtk_Notebook := Get_Notebook (Child);
   begin
      if Note /= null
        and then (Child.State = Docked
                  or else (Child.State = Normal
                           and then Child.MDI.Central.Children_Are_Maximized))
      then
         Gtk_New (Event);
         Gtk_New (Child.Tab_Label, Child.Short_Title.all);

         if Child.Icon /= null then
            Gtk_New_Hbox (Box, Homogeneous => False);

            Render_Pixmap_And_Mask_For_Colormap
              (Child.Icon, Get_Default_Colormap, Pix, Mask, 128);
            Gtk_New (Pixmap, Pix, Mask);
            Pack_Start (Box, Pixmap, Expand => False);
            Pack_Start (Box, Child.Tab_Label,  Expand => True, Fill => True);
            Add (Event, Box);
         else
            Add (Event, Child.Tab_Label);
         end if;

         Set_Tab_Label (Note, Child, Event);
         Show_All (Event);

         Return_Callback.Object_Connect
           (Event, "button_press_event",
            Return_Callback.To_Marshaller
            (Set_Focus_Child_MDI_Floating'Access),
            Child);

         --  Setup drag-and-drop, so that items can be moved from one location
         --  to another.

         Set_Dnd_Source (Event, Child);
      end if;
   end Update_Tab_Label;

   ---------------------
   -- Put_In_Notebook --
   ---------------------

   procedure Put_In_Notebook
     (MDI : access MDI_Window_Record'Class;
      Side : Dock_Side;
      Child : access MDI_Child_Record'Class;
      Notebook : Gtk_Notebook := null)
   is
      Note : Gtk_Notebook;
   begin
      --  Embed the contents of the child into the notebook, and mark
      --  Child as docked, so that we can't manipulate it afterwards.

      Ref (Child);

      if Get_Parent (Child) /= null then
         Remove (Gtk_Container (Get_Parent (Child)), Child);
      end if;

      if Side = None then
         if Notebook /= null then
            Note := Notebook;
         else
            Note := Find_Current_In_Central (MDI);
         end if;

         Child.State := Normal;
      else
         Child.State := Docked;
         Note := MDI.Docks (Side);
      end if;

      Append_Page (Note, Child);
      Set_Show_Tabs (Note, Get_Nth_Page (Note, 1) /= null);
      Update_Tab_Label (Child);

      Set_Child_Visible (Note, True);
      Show_All (Note);
      Queue_Resize (Note);

      Unref (Child);

      if Child.Minimize_Button /= null then
         Set_Sensitive (Child.Minimize_Button, False);
      end if;
   end Put_In_Notebook;

   -----------------------------
   -- Find_Current_In_Central --
   -----------------------------

   function Find_Current_In_Central
     (MDI : access MDI_Window_Record'Class) return Gtk_Notebook
   is
      List : Widget_List.Glist := MDI.Items;
      C    : MDI_Child;
      Note : Gtk_Notebook;
   begin
      --  Find the last child that had the focus in the central area
      while List /= Widget_List.Null_List loop
         C := MDI_Child (Get_Data (List));
         if C.State = Normal then
            Note := Get_Notebook (C);
            if Note /= null then
               return Note;
            end if;
         end if;
         List := Next (List);
      end loop;

      --  If there is only one child to the central paned, this is the central
      --  notebook

      List := Get_Children (MDI.Central.Container);
      if Length (List) = 1 then
         Note := Gtk_Notebook (Get_Data (List));
      end if;
      Free (List);

      return Note;
   end Find_Current_In_Central;

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
   begin
      if MDI.All_Floating_Mode then
         return;
      end if;

      if Dock
        and then Child.State /= Docked
        and then Child.Dock /= None
      then
         Float_Child (Child, False);
         Minimize_Child (Child, False);

         --  If there was no window docked yet, obey the size request for that
         --  child.
         if not Get_Child_Visible (MDI.Docks (Child.Dock)) then
            Set_Size (MDI.Main_Pane, MDI.Docks (Child.Dock), -1, -1);
         end if;

         Put_In_Notebook (MDI, Child.Dock, Child);
         Update_Dock_Menu (Child);

         Set_Child_Visible (Child.Maximize_Button, False);
         Set_Child_Visible (Child.Minimize_Button, False);

      elsif not Dock and then Child.State = Docked then
         Ref (Child);
         Remove (Get_Notebook (Child), Child);

         if MDI.Central.Children_Are_Maximized then
            Put_In_Notebook (MDI, None, Child);

         else
            Put (MDI.Central.Layout, Child, Child.X, Child.Y);

            --  If the child was at least allocated once before (which doesn't
            --  happen if we are destroying the MDI when it hasn't been mapped
            if Child.Uniconified_Width /= -1 then
               Set_Size_Request
                 (Child, Child.Uniconified_Width, Child.Uniconified_Height);
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
           (0,
            Gint (Get_Allocation_Height (MDI.Central.Layout)) - Icons_Height);

         --  Find the best placement for the icon

         while List /= Null_List loop
            C2 := MDI_Child (Get_Data (List));

            if C2 /= MDI_Child (Child) and then C2.State = Iconified then
               if abs (C2.Y - Child.Y) / Icons_Height <= 1 then
                  if C2.X + Icons_Width >=
                    Gint (Get_Allocation_Width (MDI.Central.Layout))
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

   begin
      if MDI.All_Floating_Mode then
         return;
      end if;

      if Maximize and then not MDI.Central.Children_Are_Maximized then
         MDI.Central.Children_Are_Maximized := True;

         while List /= Null_List loop
            C := MDI_Child (Get_Data (List));
            List := Prev (List);

            if C.State = Normal or else C.State = Iconified then
               Put_In_Notebook (MDI, None, C);
            end if;
         end loop;

         Set_Child_Visible (MDI.Central.Layout, False);
         Hide_All (MDI.Central.Layout);
         Set_Child_Visible (MDI.Central.Container, True);
         Show (MDI.Central.Container);

      elsif not Maximize and then MDI.Central.Children_Are_Maximized then
         --  The middle notebook was already destroyed by the last call to
         --  Remove_From_Notebook in the above loop

         Set_Child_Visible (MDI.Central.Layout, True);
         Set_Child_Visible (MDI.Central.Container, False);

         while List /= Null_List loop
            C := MDI_Child (Get_Data (List));
            if C.State = Normal then
               --  Remove from middle notebook and put in layout
               Ref (C);
               Remove (Gtk_Container (Get_Parent (C)), C);
               Put (MDI.Central.Layout, C, C.X, C.Y);
               Unref (C);
            end if;
            List := Prev (List);
         end loop;

         Show_All (MDI.Central.Layout);
         Hide (MDI.Central.Container);

         MDI.Central.Children_Are_Maximized := False;
      end if;

      if Old_Focus /= null then
         Set_Focus_Child (MDI, Old_Focus);
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
      when E : others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
         null;
   end Cascade_Cb;

   ---------------
   -- Tile_H_Cb --
   ---------------

   procedure Tile_H_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Tile_Horizontally (MDI_Window (MDI));

   exception
      when E : others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
         null;
   end Tile_H_Cb;

   ---------------
   -- Tile_V_Cb --
   ---------------

   procedure Tile_V_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Tile_Vertically (MDI_Window (MDI));

   exception
      when E : others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
         null;
   end Tile_V_Cb;

   ---------------------------
   -- Removed_From_Notebook --
   ---------------------------

   procedure Removed_From_Notebook
     (Note : access Gtk_Widget_Record'Class; Args  : Gtk_Args)
   is
      Children : Widget_List.Glist := Get_Children (Gtk_Container (Note));
      C : constant  Gtk_Widget := Gtk_Widget (To_Object (Args, 1));
      Child : MDI_Child;
      Parent   : Gtk_Widget;
      Len      : Guint;
   begin
      if C.all not in MDI_Child_Record'Class then
         return;
      end if;

      Child := MDI_Child (C);
      Child.Tab_Label := null;
      Len := Length (Children);
      Free (Children);

      Child.State := Normal;
      if Child.Minimize_Button /= null then
         Set_Sensitive (Child.Minimize_Button, True);
      end if;

      if not Gtk.Object.In_Destruction_Is_Set (Note) then
         Set_Show_Tabs (Gtk_Notebook (Note), Len > 1);

         if Len = 0 then
            --  Destroy a notebook when embedded in a paned widget, so that the
            --  pane automatically hides the handles if necessary
            --  If the parent is not a paned widget, then either the notebook
            --  is a dock or there is no pane widget and the notebook is in the
            --  central area.

            Parent := Get_Parent (Note);
            if Parent.all in Gtkada_Multi_Paned_Record'Class then
               --  A dock ?
               if Get_Parent (Parent).all
                 not in Gtkada_Multi_Paned_Record'Class
               then
                  Set_Child_Visible (Note, False);
               else
                  Children := Get_Children (Gtk_Container (Parent));
                  Len := Length (Children);
                  Free (Children);

                  --  Always keep at least one notebook in the paned, for
                  --  drag-and-drop purposes
                  if Len /= 1 then
                     Destroy (Note);
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Removed_From_Notebook;

   -------------------
   -- Single_Window --
   -------------------

   procedure Single_Window (MDI : access MDI_Window_Record) is
   begin
      if MDI.Central.Children_Are_Maximized then
         Maximize_Children (MDI, False);
         Maximize_Children (MDI, True);
      end if;
   end Single_Window;

   -----------
   -- Split --
   -----------

   procedure Split
     (MDI : access MDI_Window_Record; Orientation : Gtk_Orientation)
   is
      Note, Note2 : Gtk_Notebook;
      Child : MDI_Child;
   begin
      Maximize_Children (MDI, True);
      Note := Find_Current_In_Central (MDI);

      --  Only split if there are at least two children
      if Note /= null and then Get_Nth_Page (Note, 1) /= null then
         Note2 := Create_Notebook;
         Child := MDI_Child (Get_Nth_Page (Note, Get_Current_Page (Note)));
         Ref (Child);
         Remove (Note, Child);
         Put_In_Notebook (MDI, None, Child, Note2);
         Unref (Child);
         Set_Focus_Child (Child);

         Split (MDI.Central.Container,
                Ref_Widget  => Note,
                New_Child   => Note2,
                Orientation => Orientation,
                After       => False);
         Show_All (Note2);
      end if;
   end Split;

   ----------------
   -- Split_H_Cb --
   ----------------

   procedure Split_H_Cb (MDI   : access Gtk_Widget_Record'Class) is
   begin
      --  Do nothing unless the current child is in the central area, since
      --  otherwise this is disturbing for the user

      if MDI_Window (MDI).Focus_Child /= null
        and then MDI_Window (MDI).Focus_Child.State = Normal
      then
         Split (MDI_Window (MDI), Orientation => Orientation_Horizontal);
      end if;
   exception
      when E : others =>
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
         null;
   end Split_H_Cb;

   ----------------
   -- Split_V_Cb --
   ----------------

   procedure Split_V_Cb (MDI   : access Gtk_Widget_Record'Class) is
   begin
      --  Do nothing unless the current child is in the central area, since
      --  otherwise this is disturbing for the user

      if MDI_Window (MDI).Focus_Child /= null
        and then MDI_Window (MDI).Focus_Child.State = Normal
      then
         Split (MDI_Window (MDI), Orientation => Orientation_Vertical);
      end if;
   exception
      when E : others =>
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
         null;
   end Split_V_Cb;

   -----------------
   -- Maximize_Cb --
   -----------------

   procedure Maximize_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Maximize_Children (MDI_Window (MDI), True);

   exception
      when E : others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
         null;
   end Maximize_Cb;

   -----------------------
   -- Maximize_Child_Cb --
   -----------------------

   procedure Maximize_Child_Cb (Child : access Gtk_Widget_Record'Class) is
      M : constant MDI_Window := MDI_Child (Child).MDI;
   begin
      Maximize_Children (M, not M.Central.Children_Are_Maximized);
      Set_Focus_Child (MDI_Child (Child));

   exception
      when E : others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the button in Initialize
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
         null;
   end Maximize_Child_Cb;

   -------------------
   -- Unmaximize_Cb --
   -------------------

   procedure Unmaximize_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Maximize_Children (MDI_Window (MDI), False);

   exception
      when E : others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
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
      when E : others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
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
      when E : others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
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
      when E : others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         pragma Debug
           (Put_Line
              ("Unexpected exception: " & Exception_Information (E)));
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
      Position    : Gint;
      Children    : Widget_List.Glist;
      Item        : Gtk_Menu_Item;
      Ref         : String_Access;

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

               --  Find the closest menu item, to keep the Window menu sorted
               if First_Child.Short_Title.all > Child.Short_Title.all
                 and then (Ref = null
                           or else First_Child.Short_Title.all < Ref.all)
               then
                  Ref := First_Child.Short_Title;
                  Item := Gtk_Menu_Item (First_Child.Menu_Item);
               end if;
            end if;

            Tmp := Next (Tmp);
         end loop;

         --  Insert the new item sorted in the Window menu
         if Item = null then
            Position := -1;
         else
            Position := 0;
            Children := Get_Children (Child.MDI.Menu);
            Tmp := Children;
            while Tmp /= Widget_List.Null_List loop
               exit when Gtk_Menu_Item (Get_Data (Tmp)) = Item;
               Position := Position + 1;
               Tmp := Next (Tmp);
            end loop;
            Free (Children);
         end if;

         Gtk_New (Child.Menu_Item, G, "");
         Update_Menu_Item (Child);

         Insert (Child.MDI.Menu, Child.Menu_Item, Position);
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

         Gtk_New (Item, "Split Horizontally");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Split_H_Cb'Access), MDI);
         Set_Accel_Path (Item, "<gtkada>/window/split_horizontal", MDI.Group);

         Gtk_New (Item, "Split Vertically");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Split_V_Cb'Access), MDI);
         Set_Accel_Path (Item, "<gtkada>/window/split_vertical", MDI.Group);

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
     (MDI : access MDI_Window_Record; Prio : Priorities_Array)
   is
      --  Doing lots of examples manually, there appears a basic scheme for
      --  handling of priorities. The middle area should always be inserted
      --  fourth, just before the last dock.
      --  The algorithm to go from one dock to another is pretty
      --  straightforward:
      --    - From left to any other: always Split_Horizontally
      --    - From Right to any other: always Split Horizontally, insert before
      --    - From Top to any other: always Split Vertically
      --    - From Bottom to any other: always Split Vertically, insert before
      --    - From Middle: depends on the next dock

      Orientations : constant array (Left .. Bottom) of Gtk_Orientation :=
        (Orientation_Horizontal, Orientation_Horizontal,
         Orientation_Vertical,   Orientation_Vertical);
      Visible : array (Left .. Bottom) of Boolean;
      Widths, Heights : array (Left .. Bottom) of Glib.Gint;
   begin
      for Side in MDI.Docks'Range loop
         Ref (MDI.Docks (Side));
         case Side is
            when Left | Right =>
               Widths (Side)  := Get_Allocation_Width (MDI.Docks (Side));
               Heights (Side) := 0;
            when Top | Bottom =>
               Widths (Side)  := 0;
               Heights (Side) := Get_Allocation_Height (MDI.Docks (Side));
         end case;

         --  If not allocated yet
         if Heights (Side) = 1 then
            Heights (Side) := -1;
         elsif Widths (Side) = 1 then
            Widths (Side) := -1;
         end if;

         Visible (Side) := Visible_Is_Set (MDI.Docks (Side))
           and then Get_Child_Visible (MDI.Docks (Side));

         Remove (MDI.Main_Pane, MDI.Docks (Side));
      end loop;
      Ref (MDI.Central.Layout);
      Remove (MDI.Main_Pane, MDI.Central.Layout);
      Ref (MDI.Central.Container);
      Remove (MDI.Main_Pane, MDI.Central.Container);

      Add_Child
        (MDI.Main_Pane,
         MDI.Docks (Prio (Prio'First)),
         Orientations (Prio (Prio'First)),
         Fixed_Size => True,
         Width   => Widths  (Prio (Prio'First)),
         Height  => Heights (Prio (Prio'First)));

      for Side in Prio'First + 1 .. Prio'Last loop
         if Side = Prio'Last then
            Split (MDI.Main_Pane,
                   Ref_Widget  => MDI.Docks (Prio (Side - 1)),
                   New_Child   => MDI.Central.Container,
                   Orientation => Orientations (Prio (Side - 1)),
                   After       => Prio (Side - 1) = Left
                     or else Prio (Side - 1) = Top);
            Split (MDI.Main_Pane,
                   Ref_Widget  => MDI.Central.Container,
                   New_Child   => MDI.Docks (Prio (Side)),
                   Orientation => Orientations (Prio (Side)),
                   After       => Prio (Side) = Right
                   or else Prio (Side) = Bottom,
                   Fixed_Size  => True,
                   Width       => Widths (Prio (Side)),
                   Height      => Heights (Prio (Side)));
         else
            Split (MDI.Main_Pane,
                   Ref_Widget  => MDI.Docks (Prio (Side - 1)),
                   New_Child   => MDI.Docks (Prio (Side)),
                   Orientation => Orientations (Prio (Side - 1)),
                   After       => Prio (Side - 1) = Left
                   or else Prio (Side - 1) = Top,
                   Fixed_Size  => True,
                   Width       => Widths (Prio (Side)),
                   Height      => Heights (Prio (Side)));
         end if;
      end loop;

      Split (MDI.Main_Pane,
             Ref_Widget  => MDI.Central.Container,
             New_Child   => MDI.Central.Layout,
             Orientation => Orientation_Horizontal);

      if MDI.Central.Children_Are_Maximized then
         Set_Child_Visible (MDI.Central.Layout, False);
         Hide (MDI.Central.Layout);
      else
         Set_Child_Visible (MDI.Central.Container, False);
         Hide (MDI.Central.Container);
      end if;

      for Side in MDI.Docks'Range loop
         Unref (MDI.Docks (Side));
         Set_Child_Visible (MDI.Docks (Side), Visible (Side));
      end loop;
      Unref (MDI.Central.Layout);
      Unref (MDI.Central.Container);
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
      Short_Title : UTF8_String := "";
      Title       : UTF8_String := "";
      State       : State_Type := Normal;
      Dock        : Dock_Side := None;
      Focus       : Boolean := False;
      Raised      : Boolean := False)
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
         Add ("Maximized", Boolean'Image (MDI.Central.Children_Are_Maximized));
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
      Add ("Raised", Boolean'Image (Raised));

      if ID_Node /= null then
         Add_Child (Child_Node, ID_Node);
      end if;

      Add_Child (Tree, Child_Node, Append => True);
   end Add_To_Tree;

   package body Desktop is

      procedure Parse_Child_Node
        (MDI         : access MDI_Window_Record'Class;
         Child_Node  : Node_Ptr;
         User        : User_Data;
         Focus_Child : in out MDI_Child;
         Width       : out Guint;
         Height      : out Guint;
         Raised      : out Boolean;
         State       : out State_Type;
         Child       : out MDI_Child);
      --  Parse a <child> node and return the corresponding Child. The latter
      --  has not been inserted in the MDI

      procedure Parse_Notebook_Node
        (MDI         : access MDI_Window_Record'Class;
         Child_Node  : Node_Ptr;
         User        : User_Data;
         Focus_Child : in out MDI_Child;
         Notebook    : in out Gtk_Notebook);
      --  Parse a <notebook> node.
      --  Notebook is not recreated if it is already created

      procedure Parse_Pane_Node
        (MDI         : access MDI_Window_Record'Class;
         Node        : Node_Ptr;
         Focus_Child : in out MDI_Child;
         User        : User_Data;
         First_Child : Gtk_Notebook);
      --  Parse a <Pane> node

      function First_Widget_Child (Child : Node_Ptr) return Node_Ptr;
      --  Return the first non-pane child of Child

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

      -------------------------
      -- Parse_Notebook_Node --
      -------------------------

      procedure Parse_Notebook_Node
        (MDI         : access MDI_Window_Record'Class;
         Child_Node  : Node_Ptr;
         User        : User_Data;
         Focus_Child : in out MDI_Child;
         Notebook    : in out Gtk_Notebook)
      is
         N : Node_Ptr := Child_Node.Child;
         Width, Height : Guint;
         State  : State_Type;
         Raised : Boolean;
         Child  : MDI_Child;
         Raised_Child : MDI_Child;
      begin
         if Notebook = null then
            Notebook := Create_Notebook;
         end if;

         while N /= null loop
            if N.Tag.all = "Child" then
               Parse_Child_Node
                 (MDI, N, User, Focus_Child, Width, Height,
                  Raised, State, Child);
               if Raised then
                  Raised_Child := Child;
               end if;

               if Child /= null then
                  Float_Child (Child, False);
                  Put_In_Notebook (MDI, None, Child, Notebook);
               end if;

            else
               --  Invalid node
               null;
            end if;

            N := N.Next;
         end loop;

         if Raised_Child /= null then
            Set_Current_Page (Notebook, Page_Num (Notebook, Child));
         end if;
      end Parse_Notebook_Node;

      ----------------------
      -- Parse_Child_Node --
      ----------------------

      procedure Parse_Child_Node
        (MDI         : access MDI_Window_Record'Class;
         Child_Node  : Node_Ptr;
         User        : User_Data;
         Focus_Child : in out MDI_Child;
         Width       : out Guint;
         Height      : out Guint;
         Raised      : out Boolean;
         State       : out State_Type;
         Child       : out MDI_Child)
      is
         N          : Node_Ptr;
         Register   : Register_Node;
      begin
         Register := Registers;
         Child    := null;
         Raised   := False;
         State    := Normal;

         while Child = null and then Register /= null loop
            Child := Register.Load
              (MDI_Window (MDI), Child_Node.Child, User);
            Register := Register.Next;
         end loop;

         if Child /= null then
            --  Undock the child, since it is possible that its current
            --  dock is different from the one registered in the desktop. It
            --  might have been docked by Register

            Dock_Child (Child, False);

            N := Child_Node.Child.Next;

            while N /= null loop
               --  We ignore the <title> and <short_title> fields. After all,
               --  the callback that created the child has or should have set
               --  a proper title already, and there is no reason to override
               --  this.

               if N.Tag.all = "X" then
                  Child.X := Gint'Value (N.Value.all);

               elsif N.Tag.all = "Y" then
                  Child.Y := Gint'Value (N.Value.all);

               elsif N.Tag.all = "Width" then
                  Width := Guint'Value (N.Value.all);

               elsif N.Tag.all = "Height" then
                  Height := Guint'Value (N.Value.all);

--               elsif N.Tag.all = "Title" then
--                  Set_Title (Child, N.Value.all, Child.Short_Title.all);
--
--               elsif N.Tag.all = "Short_Title" then
--                  Set_Title (Child, Child.Title.all, N.Value.all);

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

               elsif N.Tag.all = "Raised" then
                  Raised := Boolean'Value (N.Value.all);

               else
                  --  ??? Unknown node, just ignore for now
                  null;
               end if;

               N := N.Next;
            end loop;
         end if;

         Set_Size_Request (Child, Gint (Width), Gint (Height));
      end Parse_Child_Node;

      ------------------------
      -- First_Widget_Child --
      ------------------------

      function First_Widget_Child (Child : Node_Ptr) return Node_Ptr is
         C : Node_Ptr := Child;
      begin
         while C /= null loop
            exit when C.Tag.all /= "Pane";
            C := C.Child;
         end loop;
         return C;
      end First_Widget_Child;

      ---------------------
      -- Parse_Pane_Node --
      ---------------------

      procedure Parse_Pane_Node
        (MDI         : access MDI_Window_Record'Class;
         Node        : Node_Ptr;
         Focus_Child : in out MDI_Child;
         User        : User_Data;
         First_Child : Gtk_Notebook)
      is
         Orientation : constant Gtk_Orientation := Gtk_Orientation'Value
           (Get_Attribute (Node, "Orientation"));
         N     : Node_Ptr;
         Note, Previous : Gtk_Notebook;
         Count : Natural := 0;
      begin
         --  Count the number of children (except for the first one, already
         --  handled)
         N := Node.Child;
         while N /= null loop
            Count := Count + 1;
            N := N.Next;
         end loop;

         declare
            First : array (1 .. Count) of Gtk_Notebook;
         begin
            First (1) := First_Child;

            --  First insert the first child, which will act as a base for
            --  further splitting
            Count := 2;
            Previous := First_Child;
            N := Node.Child.Next;
            while N /= null loop
               Parse_Notebook_Node
                 (MDI, First_Widget_Child (N), User, Focus_Child, Note);
               First (Count) := Note;
               Split (MDI.Central.Container,
                      Ref_Widget  => Previous,
                      New_Child   => Note,
                      Orientation => Orientation);
               Previous := Note;
               N := N.Next;
               Count := Count + 1;
            end loop;

            --  Then insert the rest of the widgets
            N := Node.Child;
            Count := 1;
            while N /= null loop
               if N.Tag.all = "Pane" then
                  Parse_Pane_Node (MDI, N, Focus_Child, User, First (Count));
               end if;
               N := N.Next;
               Count := Count + 1;
            end loop;
         end;
      end Parse_Pane_Node;

      ---------------------
      -- Restore_Desktop --
      ---------------------

      procedure Restore_Desktop
        (MDI       : access MDI_Window_Record'Class;
         From_Tree : Glib.Xml_Int.Node_Ptr;
         User      : User_Data)
      is
         Child, Focus_Child : MDI_Child;
         Child_Node : Node_Ptr;
         Width, Height : Guint;
         State      : State_Type;
         Icons_Height : constant Gint :=
           MDI.Title_Bar_Height - 2 * Border_Thickness;
         Raised     : Boolean;

         Current_Pages : array (Dock_Side) of MDI_Child
           := (others => null);

      begin
         if From_Tree /= null then
            Child_Node := From_Tree.Child;
            pragma Assert (From_Tree.Tag.all = "MDI");
         end if;

         MDI.Desktop_Was_Loaded := True;

         while Child_Node /= null loop
            if Child_Node.Tag.all = "Left" then
               Set_Size (MDI.Main_Pane, MDI.Docks (Left),
                         Gint'Value (Child_Node.Value.all), -1);

            elsif Child_Node.Tag.all = "Right" then
               Set_Size (MDI.Main_Pane, MDI.Docks (Right),
                         Gint'Value (Child_Node.Value.all), -1);

            elsif Child_Node.Tag.all = "Top" then
               Set_Size (MDI.Main_Pane, MDI.Docks (Top),
                         -1, Gint'Value (Child_Node.Value.all));

            elsif Child_Node.Tag.all = "Bottom" then
               Set_Size (MDI.Main_Pane, MDI.Docks (Bottom),
                         -1, Gint'Value (Child_Node.Value.all));

            elsif Child_Node.Tag.all = "Maximized" then
               Maximize_Children (MDI, Boolean'Value (Child_Node.Value.all));

            elsif Child_Node.Tag.all = "Pane" then
               declare
                  First : Gtk_Notebook;
                  N2    : constant Node_Ptr := First_Widget_Child (Child_Node);
               begin
                  pragma Assert (N2 /= null and then N2.Tag.all = "Notebook");
                  First := Find_Current_In_Central (MDI);

                  if First = null then
                     Parse_Notebook_Node (MDI, N2, User, Focus_Child, First);
                     Add_Child (MDI.Central.Container, First,
                                Gtk_Orientation'Value
                                  (Get_Attribute (Child_Node, "Orientation")));
                  else
                     Parse_Notebook_Node (MDI, N2, User, Focus_Child, First);
                  end if;
                  Parse_Pane_Node (MDI, Child_Node, Focus_Child, User, First);
               end;

            elsif Child_Node.Tag.all = "Child" then
               Parse_Child_Node
                 (MDI, Child_Node, User,
                  Focus_Child, Width, Height, Raised, State, Child);

               if Child /= null then
                  if Raised then
                     Current_Pages (Child.Dock) := Child;
                  end if;

                  case State is
                     when Docked =>
                        Dock_Child (Child, True);

                     when Floating =>
                        Size_Allocate
                          (Child, (Child.X, Child.Y,
                                   Allocation_Int (Width),
                                   Allocation_Int (Height)));
                        Float_Child (Child, True);

                     when Normal =>
                        Float_Child (Child, False);
                        Dock_Child (Child, False);
                        Child.Uniconified_Width := Gint (Width);
                        Child.Uniconified_Height := Gint (Height);

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

         --  Need to set the focus child before raising the notebook pages,
         --  since Raise_Child_Idle will restore the focus child, and thus
         --  might prevent one of the notebooks to be properly raised

         if Focus_Child /= null then
            Set_Focus_Child (Focus_Child);
         end if;

         for J in Current_Pages'Range loop
            if Current_Pages (J) /= null then
               Raise_Child (Current_Pages (J));
            end if;
         end loop;

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
         Iter             : Gtkada.Multi_Paned.Child_Iterator;

         procedure Add (Parent : Node_Ptr; Name, Value : String);
         --  Add a new child to Child_Node

         procedure Save_Widget
           (Parent : Node_Ptr;
            Child  : MDI_Child;
            Raised : Boolean);
         --  Save the Child. Raised is True if Child is the current page
         --  in a notebook.

         procedure Save_Notebook
           (Parent : Node_Ptr; Note : Gtk_Notebook);
         --  save all pages of the notebook

         ---------
         -- Add --
         ---------

         procedure Add (Parent : Node_Ptr; Name, Value : String) is
            N : Node_Ptr;
         begin
            N := new Node;
            N.Tag := new String'(Name);
            N.Value := new String'(Value);
            Add_Child (Parent, N, Append => True);
         end Add;

         -----------------
         -- Save_Widget --
         -----------------

         procedure Save_Widget
           (Parent : Node_Ptr;
            Child  : MDI_Child;
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
               Add_Child (Child_Node, Widget_Node, Append => True);

               Add (Child_Node, "Dock", Dock_Side'Image (Child.Dock));
               Add (Child_Node, "State", State_Type'Image (Child.State));
--               Add (Child_Node, "Title", Child.Title.all);
--               Add (Child_Node, "Short_Title", Child.Short_Title.all);

               if Child.State = Floating then
                  declare
                     Win : constant Gtk_Widget :=
                       Get_Toplevel (Child.Initial);
                     X, Y, W, H : Gint;
                  begin
                     --  Note: This size doesn't include the size of the window
                     --  decorations, doesn't seem to be a way to do this.
                     W := Get_Allocation_Width (Win);
                     H := Get_Allocation_Height (Win);
                     Get_Root_Origin (Get_Window (Win), X, Y);

                     Add (Child_Node, "Y", Gint'Image (Y));
                     Add (Child_Node, "X", Gint'Image (X));
                     Add (Child_Node, "Height", Gint'Image (H));
                     Add (Child_Node, "Width", Gint'Image (W));
                     Add (Child_Node, "Uniconified_Height", Gint'Image (H));
                     Add (Child_Node, "Uniconified_Width",  Gint'Image (W));
                  end;
               else
                  if Child.State = Iconified then
                     Add (Child_Node, "Uniconified_Height",
                          Gint'Image (Child.Uniconified_Height));
                     Add (Child_Node, "Uniconified_Width",
                          Gint'Image (Child.Uniconified_Width));
                     Add (Child_Node, "Uniconified_Y",
                          Gint'Image (Child.Uniconified_Y));
                     Add (Child_Node, "Uniconified_X",
                          Gint'Image (Child.Uniconified_X));
                  end if;

                  Add (Child_Node, "Y", Gint'Image (Child.Y));
                  Add (Child_Node, "X", Gint'Image (Child.X));
                  Add (Child_Node, "Height",
                       Allocation_Int'Image (Get_Allocation_Height (Child)));
                  Add (Child_Node, "Width",
                       Allocation_Int'Image (Get_Allocation_Width (Child)));
               end if;

               if Child = MDI.Focus_Child then
                  Add (Child_Node, "Focus", "True");
               end if;

               if Raised then
                  Add (Child_Node, "Raised", "True");
               end if;

               Add_Child (Parent, Child_Node, Append => True);
            end if;
         end Save_Widget;

         -------------------
         -- Save_Notebook --
         -------------------

         procedure Save_Notebook (Parent : Node_Ptr; Note : Gtk_Notebook) is
            Length : constant Guint := Page_List.Length (Get_Children (Note));
            Current_Page : constant Gint := Get_Current_Page (Note);
         begin
            if Length > 0 then
               for Page_Index in 0 .. Length - 1 loop
                  Save_Widget
                    (Parent,
                     Get_Child_From_Page
                       (Get_Nth_Page (Note, Gint (Page_Index))),
                     Current_Page = Gint (Page_Index));
               end loop;
            end if;
         end Save_Notebook;

      begin
         Root := new Node;
         Root.Tag := new String'("MDI");

         Add (Root, "Left",
              Gint'Image (Get_Allocation_Width (MDI.Docks (Left))));
         Add (Root, "Right",
              Gint'Image (Get_Allocation_Width (MDI.Docks (Right))));
         Add (Root, "Top",
              Gint'Image (Get_Allocation_Height (MDI.Docks (Top))));
         Add (Root, "Bottom",
              Gint'Image (Get_Allocation_Height (MDI.Docks (Bottom))));
         Add (Root, "Maximized",
              Boolean'Image (MDI.Central.Children_Are_Maximized));

         --  Look through all the notebooks, and save the widgets in the
         --  notebook order.

         for J in MDI.Docks'Range loop
            Save_Notebook (Root, MDI.Docks (J));
         end loop;

         declare
            Current, N : Node_Ptr;
            Depth      : Natural := 0;
         begin
            Current := Root;
            Iter := Start (MDI.Central.Container);
            while not At_End (Iter) loop
               for D in Get_Depth (Iter) + 1 .. Depth loop
                  Current := Current.Parent;
               end loop;

               Depth := Get_Depth (Iter);

               if Get_Widget (Iter) /= null then
                  N := new Node;
                  N.Tag := new String'("Notebook");
                  Add_Child (Current, N, Append => True);
                  Save_Notebook (N, Gtk_Notebook (Get_Widget (Iter)));
               else
                  N := new Node;
                  N.Tag := new String'("Pane");
                  Set_Attribute
                    (N, "Orientation",
                     Gtk_Orientation'Image (Get_Orientation (Iter)));
                  Add_Child (Current, N, Append => True);
                  Current := N;
               end if;

               Next (Iter);
            end loop;
         end;

         --  Save the floating and non-maximized widgets.

         while Item /= Widget_List.Null_List loop
            Child := MDI_Child (Widget_List.Get_Data (Item));

            case Child.State is
               when Docked =>
                  null;

               when Normal =>
                  if not MDI.Central.Children_Are_Maximized then
                     Save_Widget (Root, Child, False);
                  end if;

               when Floating | Iconified =>
                  Save_Widget (Root, Child, False);
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
      Note    : Gtk_Notebook;
      Style   : Gtk_Style;
   begin
      if Highlight then
         --  Do nothing if:
         --    - the child is in the layout and has the focus
         --    - the child is in a notebook and is in the current page

         if (Child.State = Normal
             and then not Child.MDI.Central.Children_Are_Maximized
             and then Child.MDI.Selected_Child = MDI_Child (Child))
         then
            return;
         end if;

         Note := Get_Notebook (Child);

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
               if Get_Data (Tmp).all'Tag = Gtk_Accel_Label_Record'Tag then
                  Set_Style (Get_Data (Tmp), Style);
               end if;

               Tmp := Next (Tmp);
            end loop;

            Free (Children);
         end;
      end if;

      if Child.Tab_Label /= null then
         Set_Style (Child.Tab_Label, Style);
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

   --------------------
   -- Set_Dnd_Source --
   --------------------

   procedure Set_Dnd_Source
     (Widget : access Gtk_Widget_Record'Class;
      Child  : access Gtk_Widget_Record'Class) is
   begin
      Add_Events (Widget, Button_Press_Mask);
      Return_Callback.Object_Connect
        (Widget, "button_press_event",
         Return_Callback.To_Marshaller (Child_Drag_Begin'Access),
         Child);
   end Set_Dnd_Source;

   ------------------------
   -- Draw_Dnd_Rectangle --
   ------------------------

   procedure Draw_Dnd_Rectangle (MDI : access MDI_Window_Record'Class) is
   begin
      if MDI.Dnd_Xor_GC = null then
         Gdk_New (MDI.Dnd_Xor_GC, Get_Window (MDI));
         Set_Function (MDI.Dnd_Xor_GC, Invert);
         Set_Exposures (MDI.Dnd_Xor_GC, False);
         Set_Subwindow (MDI.Dnd_Xor_GC, Include_Inferiors);
         Set_Line_Attributes
           (MDI.Dnd_Xor_GC, 2, Line_On_Off_Dash, Cap_Not_Last, Join_Bevel);
      end if;

      if MDI.Dnd_Rectangle_Owner /= null then
         Draw_Rectangle
           (MDI.Dnd_Rectangle_Owner,
            MDI.Dnd_Xor_GC,
            False,
            MDI.Dnd_Rectangle.X,
            MDI.Dnd_Rectangle.Y,
            MDI.Dnd_Rectangle.Width,
            MDI.Dnd_Rectangle.Height);
      end if;
   end Draw_Dnd_Rectangle;

   ----------------------
   -- Child_Drag_Begin --
   ----------------------

   function Child_Drag_Begin
     (Child  : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      C      : constant MDI_Child := MDI_Child (Child);
      Tmp    : Gdk_Grab_Status;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Pointer_Grab
        (Get_Window (C),
         False,
         Button_Press_Mask or Button_Motion_Mask or Button_Release_Mask,
         Cursor => null,
         Time   => 0);
      C.MDI.Drag_Start_X := Gint (Get_X (Event));
      C.MDI.Drag_Start_Y := Gint (Get_Y (Event));
      C.MDI.In_Drag := In_Pre_Drag;

      --  Let the event through, the drag hasn't started yet
      return False;
   end Child_Drag_Begin;

   --------------------
   -- Get_Dnd_Target --
   --------------------

   procedure Get_Dnd_Target
     (MDI       : access MDI_Window_Record'Class;
      Parent    : out Gtk_Widget;
      Rectangle : out Gdk_Rectangle;
      Side      : out Dock_Side)
   is
      Border_Width, Border_Height : Gint;
      Win    : Gdk.Gdk_Window;
      Current : Gtk_Widget;
      X, Y   : Gint;
   begin
      Window_At_Pointer (X, Y, Win);

      if Win = null then
         Parent := null;

      else
         Current := Gtk_Widget (Get_User_Data (Win));

         while Current /= null
           and then Get_Parent (Current) /= null
           and then
             (not (Current.all in Gtk_Notebook_Record'Class
                   or else Current = Gtk_Widget (MDI.Central.Layout))
              or else Get_Parent (Current).all
                not in Gtkada_Multi_Paned_Record'Class)
         loop
            Current := Get_Parent (Current);
         end loop;

         if Current = null or else Get_Parent (Current) = null then
            if MDI.Central.Children_Are_Maximized then
               Get_Pointer (MDI, X, Y);
               if X > Get_Allocation_X (MDI.Central.Container)
                 and then X < Get_Allocation_X (MDI.Central.Container)
                   + Get_Allocation_Width (MDI.Central.Container)
                 and then Y > Get_Allocation_Y (MDI.Central.Container)
                 and then Y < Get_Allocation_Y (MDI.Central.Container)
                   + Get_Allocation_Height (MDI.Central.Container)
               then
                  Parent := Gtk_Widget (MDI.Central.Container);
               else
                  Parent := Gtk_Widget (MDI);
               end if;
            else
               Parent := Gtk_Widget (MDI);
            end if;
         else
            Parent := Current;
         end if;

         --  Do we have one of the docks ?
         for S in MDI.Docks'Range loop
            if Gtk_Widget (MDI.Docks (S)) = Current then
               Side := None;
               Rectangle :=
                 (X      => Get_Allocation_X (Parent),
                  Y      => Get_Allocation_Y (Parent),
                  Width  => Get_Allocation_Width (Parent),
                  Height => Get_Allocation_Height (Parent));
               return;
            end if;
         end loop;

         if Parent = Gtk_Widget (MDI.Central.Layout) then
            Side := None;
            Rectangle :=
              (X      => 0,
               Y      => 0,
               Width  => Get_Allocation_Width (Parent),
               Height => Get_Allocation_Height (Parent));
            return;
         elsif Parent = Gtk_Widget (MDI.Central.Container) then
            Side := None;
            Rectangle :=
              (X      => Get_Allocation_X (Parent),
               Y      => Get_Allocation_Y (Parent),
               Width  => Get_Allocation_Width (Parent),
               Height => Get_Allocation_Height (Parent));
            return;
         end if;

         Get_Pointer (Parent, X, Y);

         Border_Height := Gint'Min
           (Max_Drag_Border_Width, Get_Allocation_Height (Parent) / 3);
         Border_Width := Gint'Min
           (Max_Drag_Border_Width, Get_Allocation_Width (Parent) / 3);

         if Y < Border_Height then
            Side := Top;
            Rectangle :=
              (X      => 0,
               Y      => 0,
               Width  => Get_Allocation_Width (Parent),
               Height => Border_Height);
         elsif Y > Get_Allocation_Height (Parent) - Border_Height then
            Side := Bottom;
            Rectangle :=
              (X      => 0,
               Y      => Get_Allocation_Height (Parent) - Border_Height,
               Width  => Get_Allocation_Width (Parent),
               Height => Border_Height);
         elsif X < Border_Width then
            Side := Left;
            Rectangle :=
              (X      => 0,
               Y      => 0,
               Width  => Border_Width,
               Height => Get_Allocation_Height (Parent));
         elsif X > Get_Allocation_Width (Parent) - Border_Width then
            Side := Right;
            Rectangle :=
              (X      => Get_Allocation_Width (Parent) - Border_Width,
               Y      => 0,
               Width  => Border_Width,
               Height => Get_Allocation_Height (Parent));

         elsif Parent = Gtk_Widget (MDI) then
            Parent := null;
            return;

         else
            Side := None;
            Rectangle :=
              (X      => Border_Width,
               Y      => Border_Height,
               Width  => Get_Allocation_Width (Parent) - 2 * Border_Width,
               Height => Get_Allocation_Height (Parent) - 2 * Border_Height);
         end if;

         if No_Window_Is_Set (Parent) then
            Rectangle.X := Rectangle.X + Get_Allocation_X (Parent);
            Rectangle.Y := Rectangle.Y + Get_Allocation_Y (Parent);
         end if;
      end if;
   end Get_Dnd_Target;

end Gtkada.MDI;
