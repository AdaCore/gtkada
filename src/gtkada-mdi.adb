-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2001-2009, AdaCore                  --
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
--  - Add support for groups (children are associated with groups, and groups
--    can have special colors, can be minimized,...). Groups could be
--    implemented as special MDI_Children ?
--  - Manipulation of the title bar for children (adding buttons, adding
--    pixmaps,...)
--  - Automatically add a new menu bar when a child is floated (settable
--    on a per-child basis).
--  - contextual menu in the title bar of children to dock them, float them,...

with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Tags;                use Ada.Tags;
with Ada.Exceptions;          use Ada.Exceptions;
with System;                  use System;
with System.Address_Image;
with Interfaces.C.Strings;    use Interfaces.C.Strings;

with GNAT.IO;                 use GNAT.IO;

with Glib.Convert;            use Glib.Convert;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;

with Pango.Font;              use Pango.Font;
with Pango.Layout;            use Pango.Layout;

with Gdk;                     use Gdk;
with Gdk.Color;               use Gdk.Color;
with Gdk.Cursor;              use Gdk.Cursor;
with Gdk.Drawable;            use Gdk.Drawable;
with Gdk.Event;               use Gdk.Event;
with Gdk.GC;                  use Gdk.GC;
with Gdk.Main;                use Gdk.Main;
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Gdk.Types;               use Gdk.Types;
with Gdk.Types.Keysyms;
with Gdk.Window;              use Gdk.Window;
with Gdk.Window_Attr;         use Gdk.Window_Attr;

with Gtk;                     use Gtk;
with Gtk.Accel_Group;         use Gtk.Accel_Group;
with Gtk.Accel_Label;         use Gtk.Accel_Label;
with Gtk.Arguments;           use Gtk.Arguments;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Button;              use Gtk.Button;
with Gtk.Check_Menu_Item;     use Gtk.Check_Menu_Item;
with Gtk.Container;           use Gtk.Container;
with Gtk.Drawing_Area;        use Gtk.Drawing_Area;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Event_Box;           use Gtk.Event_Box;
with Gtk.Frame;               use Gtk.Frame;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Image;               use Gtk.Image;
with Gtk.Label;               use Gtk.Label;
with Gtk.Main;                use Gtk.Main;
pragma Elaborate_All (Gtk.Main);
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Notebook;            use Gtk.Notebook;
with Gtk.Object;              use Gtk.Object;
with Gtk.Radio_Menu_Item;     use Gtk.Radio_Menu_Item;
with Gtk.Rc;
with Gtk.Style;               use Gtk.Style;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;

with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtkada.Multi_Paned;      use Gtkada.Multi_Paned;
with Gtkada.Types;

package body Gtkada.MDI is

   use Glib.Xml_Int;

   Traces : constant Boolean := False;
   --  True if traces should be activated

   Default_Title_Bar_Focus_Color : constant String := "#000088";
   --  Default color to use for the title bar of the child that has
   --  the focus.

   Default_Title_Bar_Color : constant String := "#AAAAAA";
   --  Default color to use for the title bar of children that do not
   --  have the focus.

   Default_MDI_Background_Color : constant String := "#666666";
   --  Default background color to use for the MDI window

   Default_Title_Font : constant String := "Sans 8";
   --  Default title font for the children

   Border_Thickness : constant Gint := 4;
   --  Thickness of the separators in the MDI

   Small_Border_Thickness : constant Gint := 2;
   --  The width of the borders around children that do not belong to a
   --  notebook with multiple pages.

   Max_Drag_Border_Width : constant Gint := 30;
   --  Width or height of the drag-and-drop borders for each notebook

   Drag_Threshold : constant Gint := 20;
   --  Our own threshold (instead of Gtk.Dnd.Check_Threshold), since on
   --  Windows the later seems to be set to 0, and thus we can't change a
   --  notebook page by clicking on its tab without splitting the notebook

   MDI_Class_Record        : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;
   Child_Class_Record      : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;

   MDI_Signals : constant chars_ptr_array :=
     (1 => New_String (String (Signal_Child_Selected)),
      2 => New_String (String (Signal_Float_Child)),
      3 => New_String (String (Signal_Child_Title_Changed)),
      4 => New_String (String (Signal_Child_Added)),
      5 => New_String (String (Signal_Child_Removed)),
      6 => New_String (String (Signal_Child_Icon_Changed)),
      7 => New_String (String (Signal_Children_Reorganized)));

   Child_Signals : constant chars_ptr_array :=
     (1 => New_String (String (Signal_Float_Child)),
      2 => New_String (String (Signal_Unfloat_Child)),
      3 => New_String (String (Signal_Selected)));

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

   use Widget_List;

   type Selection_Dialog_Record is new Gtk_Window_Record with record
      Current_Child : Widget_List.Glist;
      Label         : Gtk_Label;
      Ent           : Gtk_Entry;
      Length        : Natural := 0;
      Modifier      : Gdk_Modifier_Type;
      Icon          : Gtk.Image.Gtk_Image;
   end record;
   type Selection_Dialog_Access is access all Selection_Dialog_Record'Class;

   type MDI_Notebook_Record is new Gtk_Notebook_Record with record
      Is_Default_Notebook : Boolean := False;
   end record;
   type MDI_Notebook is access all MDI_Notebook_Record'Class;
   --  The type of notebooks used in the MDI.
   --  Is_Default_Notebook is set to true if the notebook should be used when
   --  Position_Default children are inserted in the MDI and no other child is
   --  available. Such a notebook is also kept empty when its last child is
   --  removed, provided no other Position_Default child exists.

   package Child_User_Data is new Glib.Object.User_Data (MDI_Child);

   type Children_Array is array (Natural range <>) of Widget_List.Glist;

   procedure Free is new
     Ada.Unchecked_Deallocation (UTF8_String, String_Access);

   function Button_Pressed
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user has pressed the mouse button in the canvas.
   --  Test whether an item was selected.

   function Button_Pressed_Forced
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Same as above, except we also act even if the event wasn't started in
   --  Child's window.

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

   function On_Notebook_Button_Press
     (Child    : access Gtk_Widget_Record'Class;
      Event    : Gdk.Event.Gdk_Event) return Boolean;
   --  Manage the contextual menu on tabs

   procedure Child_Widget_Shown
     (Widget : access Gtk_Widget_Record'Class);
   procedure Child_Widget_Hidden
     (Widget : access Gtk_Widget_Record'Class);
   --  Called when the child widget is shown or hidden by the user, to reflect
   --  that fact at the MDI_Child level, no matter whether the child is
   --  currently floating or not.

   procedure Internal_Close_Child
     (Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Internal version of Close, for a MDI_Child

   function Create_Notebook
     (MDI : access MDI_Window_Record'Class) return Gtk_Notebook;
   --  Create a notebook, and set it up for drag-and-drop

   procedure Configure_Notebook_Tabs
     (MDI           : access MDI_Window_Record'Class;
      Notebook      : access Gtk_Notebook_Record'Class;
      Hide_If_Empty : Boolean := False);
   --  Configure the visibility and position of notebook tabs.
   --  If there are no visible pages and Hide_If_Empty is true, then the
   --  notebook itself is hidden

   procedure Update_Tab_Color
     (Child : access MDI_Child_Record'Class;
      Force : Boolean := False);
   --  Change the background color of the notebook tab containing child,
   --  depending on whether the child is selected or not.

   function Delete_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Forward a delete_event from the toplevel window to the child

   procedure Destroy_Child (Child : access Gtk_Widget_Record'Class);
   procedure Destroy_Initial_Child (Child : access Gtk_Widget_Record'Class);
   --  Called when either the child itself, or the widget we initially put
   --  in it, are destroyed. Remove the child from the MDI properly.

   procedure Destroy_MDI (MDI : access Gtk_Widget_Record'Class);
   --  Called when the MDI is destroyed

   procedure Menu_Entry_Destroyed (Child : access Gtk_Widget_Record'Class);
   --  Called when the Menu_Item associated with a Child is destroyed

   procedure Menu_Destroyed (MDI : access Gtk_Widget_Record'Class);
   --  Called when the Menu associated with a MDI is destroyed

   procedure Draw_Child
     (Child : access MDI_Child_Record'Class; Area : Gdk_Rectangle);
   function Draw_Child
     (Child : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean;
   --  Draw the child (and the title bar)

   procedure Realize_MDI (MDI : access Gtk_Widget_Record'Class);
   --  Called when the child is realized

   procedure Set_Dnd_Source
     (Widget : access Gtk_Widget_Record'Class;
      Child  : access Gtk_Widget_Record'Class);
   --  Setup a widget as either a source or a target for drag-and-drop ops.

   procedure Get_Dnd_Target
     (MDI       : access MDI_Window_Record'Class;
      Parent    : out Gtk_Widget;
      Position  : out Child_Position;
      Rectangle : out Gdk_Rectangle);
   --  Return the widget that is the current target for dnd
   --  Position indicated where in the parent the child would be dropped:
   --    Position_Bottom .. Position_Right: To one of the sides
   --    Position_Automatic:                In the center

   procedure Draw_Dnd_Rectangle (MDI : access MDI_Window_Record'Class);
   --  Draw the DND rectangle

   procedure Update_Float_Menu (Child : access MDI_Child_Record'Class);
   --  Update the state of the "Float" menu item associated with child

   procedure Put_In_Notebook
     (MDI                      : access MDI_Window_Record'Class;
      Child                    : access MDI_Child_Record'Class;
      Notebook                 : Gtk_Notebook := null;
      Initial_Position         : Child_Position := Position_Automatic;
      Force_Parent_Destruction : Boolean := True);
   --  Remove Child from MDI, and put it under control of a notebook.
   --  Notebook can be used to specify a specific notebook to which the child
   --  should be added. If null, this function will compute what notebook
   --  should be used or created depending on the Child's position attribute.
   --  If Force_Parent_Destruction is True, then the notebook containing the
   --  Child will always be destroyed if Child was its last child. Otherwise,
   --  it is possible that the notebook will be kept, albeit empty.

   function Get_Notebook
     (Child : access MDI_Child_Record'Class) return Gtk_Notebook;
   --  Return the notebook that directly contains Child

   procedure Create_Menu_Entry (Child : access MDI_Child_Record'Class);
   --  Add an entry to the MDI menu that provides easy activation of Child

   procedure Propagate_Expose_Event
     (Container : access Gtk.Container.Gtk_Container_Record'Class;
      Event     : Gdk.Event.Gdk_Event_Expose);
   --  Propagate the expose event Event to all the NO_WINDOW children of
   --  Container. You must call this when Container has a specific expose
   --  callback.

   procedure Split_H_Cb (MDI   : access Gtk_Widget_Record'Class);
   procedure Split_V_Cb (MDI   : access Gtk_Widget_Record'Class);
   procedure Float_Cb   (MDI   : access Gtk_Widget_Record'Class);
   procedure Close_Cb   (MDI   : access Gtk_Widget_Record'Class);
   procedure Focus_Cb   (Child : access Gtk_Widget_Record'Class);
   --  Callbacks for the menu

   function Has_Default_Child
     (MDI         : access MDI_Window_Record'Class;
      Ignore      : MDI_Child := null;
      Ignore_Note : Gtk_Notebook := null) return Boolean;
   --  Return True if the MDI still contains a child in the Default_Group,
   --  apart from Ignore and all children of Ignore_Note.

   procedure Set_Focus_Child_MDI
     (MDI : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   procedure Set_Focus_Child_Notebook
     (Note : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when the widget that has the keyboard focus has changed. This is
   --  used to automatically select its parent MDI_Child.

   function Set_Focus_Child_MDI_Floating
     (Child : access Gtk_Widget_Record'Class) return Boolean;
   --  Same as Set_Focus_Child_MDI, but for floating windows

   function Set_Focus_Child_MDI_From_Tab
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Gives the focus to Child when the notebook tab associated with it is
   --  pressed.

   procedure Set_Focus_Child_Switch_Notebook_Page
     (Note : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when a new page from a notebook has been selected, in particular
   --  when using the scroll arrows when there are too many pages to be
   --  displayed

   function Toplevel_Focus_In
     (MDI : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the toplevel window that contains a the MDI gains the focus
   --  from the window manager

   procedure Give_Focus_To_Child (Child : MDI_Child);
   --  Give the focus to a specific MDI child
   --  You should never call Grab_Focus directly

   procedure Give_Focus_To_Previous_Child
     (Child : access MDI_Child_Record'Class);
   --  Give focus to the last child in the same area/notebook as Child, and
   --  which is not Child itself.

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

   procedure Update_Menu_Item
     (Child : access MDI_Child_Record'Class);
   --  Update the menu entry for Child

   function Find_Current_In_Central
     (MDI              : access MDI_Window_Record'Class;
      Group            : Child_Group := Group_Any;
      Initial_Position : Child_Position := Position_Automatic)
      return Gtk_Notebook;
   --  Return the first notebook that contains at least one child within the
   --  given Group. The search starts in the notebook that currently has the
   --  focus.
   --  A new notebook is created if needed (ie if no notebook has a child with
   --  the same attribute).

   procedure Removed_From_Notebook
     (Note : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when a child is removed from one of the notebooks

   procedure Update_Dnd_Window
     (MDI : access MDI_Window_Record'Class; Text : String);
   --  Create and update the contents of the small window displayed while a
   --  drag-and-drop operation is taking place

   procedure Destroy_Dnd_Window (MDI : access MDI_Window_Record'Class);
   --  Destroy the small window displayed while a drag-and-drop operation is
   --  taking place.

   procedure Emit_By_Name_Child
     (Object : System.Address; Name : String; Child : System.Address);
   pragma Import (C, Emit_By_Name_Child, "ada_g_signal_emit_by_name_ptr");

   procedure Emit_By_Name (Object : System.Address; Name : String);
   pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name");

   procedure Internal_Float_Child
     (Child             : access MDI_Child_Record'Class;
      Float             : Boolean;
      Position_At_Mouse : Boolean;
      X, Y              : Gint);
   --  Internal version of Float_Child, where the user can choose whether the
   --  new floating window should be located where the mouse is, or at
   --  coordinates specified by (X, Y)

   procedure Set_Child_Title_Bar (Child : access MDI_Child_Record'Class);
   --  Hide or display the title bar of the child, depending on its status.

   function Find_Empty_Notebook
     (MDI : access MDI_Window_Record'Class) return Gtk_Notebook;
   --  Return the empty notebook, if there is any, in the MDI.

   procedure Note_Notify (Data : System.Address; Where : System.Address);
   pragma Convention (C, Note_Notify);
   --  Notified if the old notebook that contained Child is destroyed

   ------------------
   -- Get_Notebook --
   ------------------

   function Get_Notebook
     (Child : access MDI_Child_Record'Class) return Gtk_Notebook is
   begin
      case Child.State is
         when Floating  => return null;
         when Normal    =>
            if Get_Parent (Child) /= null
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
      --  This code must always be executed (we cannot take into account
      --  whether the MDI has the focus or not). Otherwise, clicking
      --  inside an open editor in GPS, for instance, will not properly give
      --  the focus to the MDI child
      if Widget /= null then
         --  The widget is currently either a notebook or the Gtk_Fixed. Get
         --  its focus widget, which is the one we are really interested in.

         Widget := Get_Focus_Child (Gtk_Container (Widget));

         if Widget /= null then
            if Traces then
               Put_Line ("MDI: Set_Focus_Child_MDI");
            end if;
            Set_Focus_Child (MDI_Window (MDI), Containing => Widget);
         end if;
      end if;

      --  No need to call the parent's set_focus_child, this is called
      --  automatically when the signal is propagated.
   end Set_Focus_Child_MDI;

   ------------------------------------------
   -- Set_Focus_Child_Switch_Notebook_Page --
   ------------------------------------------

   procedure Set_Focus_Child_Switch_Notebook_Page
     (Note : access Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      N     : constant Gtk_Notebook := Gtk_Notebook (Note);
      Page  : constant Guint := To_Guint (Args, 2);
      Child : MDI_Child;
   begin
      Child := MDI_Child (Get_Nth_Page (N, Gint (Page)));
      if Child /= null then
         if Traces then
            Put_Line ("MDI: Set_Focus_Child_Switch_Notebook_Page "
                      & Get_Title (Child));
         end if;
         Set_Focus_Child (Child);
      end if;
   end Set_Focus_Child_Switch_Notebook_Page;

   ------------------------------
   -- Set_Focus_Child_Notebook --
   ------------------------------

   procedure Set_Focus_Child_Notebook
     (Note : access Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      pragma Unreferenced (Note);
      Widget : constant Gtk_Widget := Gtk_Widget (To_Object (Args, 1));
   begin
      --  This code must always be executed (we cannot take into account
      --  whether the notebook has the focus or not). Otherwise, clicking
      --  inside an open editor in GPS, for instance, will not properly give
      --  the focus to the MDI child
      if Widget /= null then
         if Traces then
            Put_Line ("MDI: Set_Focus_Child_Notebook "
                      & Get_Title (MDI_Child (Widget)));
         end if;
         Set_Focus_Child (MDI_Child (Widget));
      end if;
   end Set_Focus_Child_Notebook;

   ----------------------------------
   -- Set_Focus_Child_MDI_Floating --
   ----------------------------------

   function Set_Focus_Child_MDI_Floating
     (Child : access Gtk_Widget_Record'Class) return Boolean is
   begin
      if Traces then
         Put_Line ("MDI: Set_Focus_Child_MDI_Floating");
      end if;
      Set_Focus_Child (MDI_Child (Child));
      return False;
   end Set_Focus_Child_MDI_Floating;

   ----------------------------------
   -- Set_Focus_Child_MDI_From_Tab --
   ----------------------------------

   function Set_Focus_Child_MDI_From_Tab
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      if Get_Event_Type (Event) = Button_Release then
         Tmp := Button_Release (Child => Child, Event => Event);
         return False;

      elsif Get_Button (Event) = 1 then
         --  Let the event through if the child already has the focus. This way
         --  the notebook tab of the focus child can still be used for
         --  drag-and-drop
         if MDI_Child (Child).MDI.Focus_Child = MDI_Child (Child) then
            return False;

         else
            --  Process the button press event to select the child and start a
            --  drag-and-drop operation

            Tmp := Button_Pressed_Forced (Child => Child, Event => Event);

            --  is called as a result of a button_press event in the
            --  notebook's. tabs The call to Set_Focus_Child above raises the
            --  child and gives it the focus appropriately. However, if we let
            --  the signal go through it will be handled by the notebook, which
            --  will not see change in the current page, and will give the
            --  focus to the tab itself, not to the page's contents.

            return True;
         end if;
      end if;
      return False;
   end Set_Focus_Child_MDI_From_Tab;

   -----------------------
   -- Toplevel_Focus_In --
   -----------------------

   function Toplevel_Focus_In
     (MDI : access Gtk_Widget_Record'Class) return Boolean
   is
      M : constant MDI_Window := MDI_Window (MDI);
   begin
      if Traces then
         Put_Line ("MDI: Toplevel_Focus_In");
      end if;

      --  If the current child was a floating window, make sure it keeps the
      --  focus, and that no one gains the keyboard focus in the main window.
      --  This avoids a situation where an TextView has the keyboard focus, but
      --  isn't the MDI focus child.

      if M.Focus_Child = null then
         Set_Focus (Gtk_Window (Get_Toplevel (M)), null);

      elsif M.Focus_Child.State = Floating then
         Set_Focus (Gtk_Window (Get_Toplevel (M)), null);

      else
         --  Make sure the keyboard focus is correctly restored, for instance
         --  if we had open a temporary dialog and then closed it to go back
         --  to GPS.
         Give_Focus_To_Child (M.Focus_Child);
      end if;

      return False;
   end Toplevel_Focus_In;

   ---------------------------
   -- Setup_Toplevel_Window --
   ---------------------------

   procedure Setup_Toplevel_Window
     (MDI    : access MDI_Window_Record;
      Parent : access Gtk.Window.Gtk_Window_Record'Class) is
   begin
      Return_Callback.Object_Connect
        (Parent, Signal_Focus_In_Event,
         Return_Callback.To_Marshaller (Toplevel_Focus_In'Access),
         MDI);
   end Setup_Toplevel_Window;

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
      Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
        (1 => (1 => GType_Pointer),
         2 => (1 => GType_Pointer),
         3 => (1 => GType_Pointer),
         4 => (1 => GType_Pointer),
         5 => (1 => GType_Pointer),
         6 => (1 => GType_Pointer),
         7 => (1 => GType_None));
   begin
      Gtkada.Multi_Paned.Initialize (MDI);

      --  Request a null size, so that the window can be resized at will, even
      --  though we have played with Set_Size_Request on the children.

      Set_Size_Request (MDI, 0, 0);

      --  The MDI must have a window, so that we can change the background
      --  color. No other notebook or paned inside has a window

      Set_Has_Window (MDI, True);

      MDI.Group := Gtk_Accel_Group (Group);

      MDI.Title_Layout := Create_Pango_Layout (MDI, "Ap"); -- compute width
      MDI.Background_Color := Parse (Default_MDI_Background_Color);
      Alloc (Get_Default_Colormap, MDI.Background_Color);

      MDI.Title_Bar_Color := Parse (Default_Title_Bar_Color);
      Alloc (Get_Default_Colormap, MDI.Title_Bar_Color);

      MDI.Focus_Title_Color := Parse (Default_Title_Bar_Focus_Color);
      Alloc (Get_Default_Colormap, MDI.Focus_Title_Color);

      MDI.Default_Title_Color := Get_Bg (Get_Default_Style, State_Normal);

      Gtk.Object.Initialize_Class_Record
        (MDI,
         Signals      => MDI_Signals,
         Class_Record => MDI_Class_Record,
         Type_Name    => "GtkAdaMDI",
         Parameters   => Signal_Parameters);

      Configure
        (MDI,
         Background_Color  => MDI.Background_Color,
         Title_Bar_Color   => MDI.Title_Bar_Color,
         Focus_Title_Color => MDI.Focus_Title_Color);

      --  Put an empty notebook in the MDI, which will act as a recipient for
      --  the Position_Default widgets

      Add_Child
        (MDI, New_Child => Create_Notebook (MDI),
         Width       => -1,
         Height      => -1,
         Orientation => Orientation_Vertical);

      Widget_Callback.Connect
        (MDI, Gtk.Widget.Signal_Realize,
         Widget_Callback.To_Marshaller (Realize_MDI'Access));
      Widget_Callback.Connect
        (MDI, Signal_Destroy,
         Widget_Callback.To_Marshaller (Destroy_MDI'Access));
      Widget_Callback.Connect
        (MDI, Signal_Set_Focus_Child, Set_Focus_Child_MDI'Access);
   end Initialize;

   -----------------------
   -- Matching_Children --
   -----------------------

   function Matching_Children
     (MDI : access MDI_Window_Record'Class; Str : String) return Children_Array
   is
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
      D        : constant Selection_Dialog_Access :=
                   Selection_Dialog_Access (MDI.Selection_Dialog);
      Str      : constant UTF8_String := Get_Text (D.Ent);
      Children : constant Children_Array :=
                   Matching_Children (MDI, To_Lower (Str));
      Index    : Integer := Children'First;
      Tmp      : Integer;
      Pos      : Gint := -1;

   begin
      --  Update graphically the list of children matching the filter

      D.Length := Str'Length;
      Insert_Text (D.Ent, " {", Pos);
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
               Insert_Text (D.Ent, ",", Pos);
            end if;

            Insert_Text
              (D.Ent,
               Get_Short_Title (MDI_Child (Get_Data (Children (Tmp)))),
               Pos);

            Tmp := (Tmp + 1 - Children'First) mod Children'Length
              + Children'First;
            exit when Tmp = Index;
         end loop;

         D.Current_Child := Children (Index);

      else
         D.Current_Child := Null_List;
      end if;

      Insert_Text (D.Ent, "}", Pos);

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
               Gdk.Drawable.Unref (Pixmap);
               Gdk.Drawable.Unref (Mask);
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
      M     : constant MDI_Window := MDI_Window (MDI);
      D     : constant Selection_Dialog_Access :=
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
              or else Key = Gdk.Types.Keysyms.GDK_Control_R
              or else Key = Gdk.Types.Keysyms.GDK_ISO_Next_Group)
         then
            Close := True;

         elsif (D.Modifier and Mod1_Mask) /= 0
            and then (Key = Gdk.Types.Keysyms.GDK_Meta_L
                      or else Key = Gdk.Types.Keysyms.GDK_Meta_R
                      or else Key = Gdk.Types.Keysyms.GDK_Alt_L
                      or else Key = Gdk.Types.Keysyms.GDK_Alt_R)
         then
            Close := True;

         elsif (D.Modifier and Shift_Mask) /= 0
           and then (Key = Gdk.Types.Keysyms.GDK_Shift_L
                     or else Key = Gdk.Types.Keysyms.GDK_Shift_R)
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
     (MDI          : access MDI_Window_Record;
      Event        : Gdk.Event.Gdk_Event;
      Move_To_Next : Boolean;
      Only_Group   : Child_Group := Group_Any)
   is
      D         : Selection_Dialog_Access;
      Box, HBox : Gtk_Box;
      Frame     : Gtk_Frame;
      Tmp       : Gdk_Grab_Status;
      pragma Unreferenced (Tmp);

   begin
      if MDI.Items = Null_List then
         return;
      end if;

      if Event = null
        or else (Get_Event_Type (Event) /= Key_Press
                 and then Get_Event_Type (Event) /= Key_Release)
      then
         declare
            List     : Widget_List.Glist;
            Current  : Gtk_Notebook;
            Child    : MDI_Child;
         begin
            if MDI.Focus_Child /= null then
               Current := Get_Notebook (MDI.Focus_Child);
            end if;

            if Move_To_Next then
               List := Next (First (MDI.Items));
            else
               List := Last (MDI.Items);
            end if;

            while List /= Null_List loop
               --  Return the first window from another notebook that belongs
               --  to the same group
               Child := MDI_Child (Get_Data (List));
               if Get_Notebook (Child) /= Current
                 and then (Only_Group = Child_Group'Last
                           or else Child.Group = Only_Group)
               then
                  Set_Focus_Child (Child);
                  exit;
               end if;

               if Move_To_Next then
                  List := Next (List);
               else
                  List := Prev (List);
               end if;
            end loop;
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
           (D, Signal_Key_Release_Event,
            Return_Callback.To_Marshaller
              (Key_Event_Selection_Dialog'Access), MDI);
         Return_Callback.Object_Connect
           (D, Signal_Key_Press_Event,
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
      Close_Floating_Is_Unfloat : Boolean             := True;
      Title_Font                : Pango_Font_Description := null;
      Background_Color          : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Title_Bar_Color           : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Focus_Title_Color         : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Draw_Title_Bars           : Boolean             := True;
      Tabs_Position             : Gtk.Enums.Gtk_Position_Type :=
        Gtk.Enums.Pos_Bottom;
      Show_Tabs_Policy          : Show_Tabs_Policy_Enum := Automatic)
   is
      Desc        : Pango_Font_Description;
      W, H        : Gint;
      List        : Widget_List.Glist;
      C           : MDI_Child;
      Need_Redraw : Boolean := MDI.Draw_Title_Bars /= Draw_Title_Bars;
      Iter        : Gtkada.Multi_Paned.Child_Iterator;
      Pos_Changed : constant Boolean :=
        MDI.Tabs_Position /= Tabs_Position;

   begin
      MDI.Close_Floating_Is_Unfloat := Close_Floating_Is_Unfloat;
      MDI.Draw_Title_Bars  := Draw_Title_Bars;
      MDI.Tabs_Position    := Tabs_Position;
      MDI.Show_Tabs_Policy := Show_Tabs_Policy;

      Set_Opaque_Resizing (MDI, Opaque_Resize);

      if Title_Font /= null then
         Set_Font_Description (MDI.Title_Layout, Title_Font);
      else
         Desc := From_String (Default_Title_Font);
         Set_Font_Description (MDI.Title_Layout, Desc);
         Free (Desc);
      end if;

      Get_Pixel_Size (MDI.Title_Layout, W, H);
      MDI.Title_Bar_Height := 2 + H;

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

      Gtk_New (MDI.Highlight_Style);

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

      Iter := Start (MDI);
      while not At_End (Iter) loop
         if Get_Widget (Iter) /= null then
            if Pos_Changed then
               Set_Tab_Pos
                 (Gtk_Notebook (Get_Widget (Iter)), MDI.Tabs_Position);
            end if;

            Configure_Notebook_Tabs (MDI, Gtk_Notebook (Get_Widget (Iter)));
         end if;
         Next (Iter);
      end loop;

      if Realized_Is_Set (MDI) then
         if Background_Color /= Null_Color then
            Set_Background (Get_Window (MDI), Background_Color);
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
      end if;

      --  Resize the title bar of all children already in the MDI

      List := First (MDI.Items);
      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));
         Set_Child_Title_Bar (C);
         Update_Tab_Color (C, Force => True);
         List := Widget_List.Next (List);
      end loop;

      if Need_Redraw then
         Queue_Draw (MDI);
      end if;
   end Configure;

   -----------------
   -- Realize_MDI --
   -----------------

   procedure Realize_MDI (MDI : access Gtk_Widget_Record'Class) is
      M           : constant MDI_Window := MDI_Window (MDI);
      Window_Attr : Gdk.Window_Attr.Gdk_Window_Attr;

   begin
      Gdk.Window.Set_Background (Get_Window (M), M.Background_Color);

      Gdk_New (M.Non_Focus_GC, Get_Window (MDI));
      Set_Foreground (M.Non_Focus_GC, M.Title_Bar_Color);
      Set_Exposures (M.Non_Focus_GC, False);

      Gdk_New (M.Focus_GC, Get_Window (MDI));
      Set_Foreground (M.Focus_GC, M.Focus_Title_Color);
      Set_Exposures (M.Focus_GC, False);

      if M.Cursor_Cross = null then
         Gdk_New (M.Cursor_Cross, Cross);
      end if;
      Gdk_New (Window_Attr,
               Window_Type => Window_Child,
               Wclass      => Input_Output,
               Cursor      => M.Cursor_Cross,
               Visual      => Get_Visual (MDI),
               Colormap    => Get_Colormap (MDI),
               Event_Mask  => Get_Events (MDI)
               or Exposure_Mask
               or Button_Press_Mask
               or Button_Release_Mask
               or Button_Motion_Mask);

      --  Destroy the window attribute and the cursor

      Destroy (Window_Attr);
      Queue_Resize (MDI);
   end Realize_MDI;

   -----------------
   -- Destroy_MDI --
   -----------------

   procedure Destroy_MDI (MDI : access Gtk_Widget_Record'Class) is
      M   : constant MDI_Window := MDI_Window (MDI);
      Tmp : Widget_List.Glist := First (M.Items);
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

      Free (M.Items);
      Unref (M.Title_Layout);

      if M.Cursor_Cross /= null then
         Unref (M.Cursor_Cross);
      end if;

      if M.Highlight_Style /= null then
         Unref (M.Highlight_Style);
      end if;

      if M.Non_Focus_GC /= null then
         Unref (M.Non_Focus_GC);
      end if;

      if M.Focus_GC /= null then
         Unref (M.Focus_GC);
      end if;

      if M.Menu /= null then
         Destroy (M.Menu);
      end if;

      Free (M.Accel_Path_Prefix);
   end Destroy_MDI;

   -----------
   -- Close --
   -----------

   procedure Close
     (MDI   : access MDI_Window_Record;
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
      MDI   : constant MDI_Window := MDI_Window (Child.MDI);
      Event : Gdk_Event;
   begin
      --  Don't do anything for now if the MDI isn't realized, since we
      --  can't send create the event anyway.

      if Realized_Is_Set (MDI) then
         Allocate (Event, Delete, Get_Window (MDI));

         --  For a top-level window, we must rebuild the initial widget
         --  temporarily, so that the application can do all the test it wants.
         --  However, we need to restore the initial state before calling
         --  Dock_Child and Float_Child below.
         --  We should not test this when the MDI is being destroyed, though,
         --  to avoid memory leaks

         if Force
           or else In_Destruction_Is_Set (MDI)
           or else not Return_Callback.Emit_By_Name
           (Child.Initial, "delete_event", Event)
         then
            --  Transfer the focus before unfloating, so that the parent in
            --  which the child is unfloated (which might be random from the
            --  user's point of view) doesn't influence who gets the focus.
            if MDI_Child (Child) = MDI.Focus_Child then
               Give_Focus_To_Previous_Child (Child);
            end if;

            Float_Child (Child, False);

            if Traces then
               Put_Line ("Close_Child: destroying " & Get_Title (Child));
            end if;

            Destroy (Child);

         elsif Traces then
            Put_Line ("Close_Child: not destroying " & Get_Title (Child));
         end if;

         Free (Event);
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
      C                   : constant MDI_Child := MDI_Child (Child);
      MDI                 : constant MDI_Window := C.MDI;
      In_Selection_Dialog : Boolean := False;

   begin
      --  We know at that stage that Child has already been unparent-ed

      pragma Assert (Get_Parent (Child) = null);

      if Traces then
         Put_Line ("Destroy_Child " & Get_Title (C));
      end if;

      Ref (C);

      C.Tab_Label := null;

      --  The child of the MDI_Child has now been taken care of, thus we need
      --  to take care of the MDI_Child itself now.

      if C.Menu_Item /= null then
         Destroy (C.Menu_Item);
      end if;

      if not Gtk.Object.In_Destruction_Is_Set (C.MDI) then
         --  Do not unfloat the child, since the toplevel is no longer a
         --  Gtk_Window, and we would get a CE in Float_Child.

         if Get_Parent (C) /= null then
            Remove (Gtk_Container (Get_Parent (C)), C);
         end if;
      end if;

      if Get_Parent (C.Initial) /= null then
         if Traces then
            Put_Line ("Destroy_Child removing initial child from parent");
         end if;
         Remove (Gtk_Container (Get_Parent (C.Initial)), C.Initial);
      end if;

      C.Initial := null;

      --  Do not transfer the focus elsewhere: for an interactive close, this
      --  is done in Close_Child, otherwise we do not want to change the focus.
      --  No need to send a signal to signal that a new child has been selected
      --  since Give_Focus_To_Previous_Child has been called already
      if C = MDI.Focus_Child then
         MDI.Focus_Child := null;
      end if;

      In_Selection_Dialog := MDI.Selection_Dialog /= null
        and then C = MDI_Child (Get_Data (Selection_Dialog_Access
          (MDI.Selection_Dialog).Current_Child));

      --  Only remove it from the list of children at the end, since some of
      --  calls above might result in calls to Raise_Child_Idle, which tries
      --  to manipulate that list.
      Widget_List.Remove (MDI.Items, Gtk_Widget (C));

      --  Report that the child has been removed only after it has indeed be
      --  fully removed, but before we actually free it
      Emit_By_Name_Child
        (Get_Object (MDI), "child_removed" & ASCII.NUL, Get_Object (C));

      --  If we are currently displaying the window selection dialog, update it
      --  so that the widget that has been destroyed does not show up in the
      --  selection window.
      if In_Selection_Dialog then
         Update_Selection_Dialog (MDI, +1);
      end if;

      Free (C.Title);
      Free (C.Short_Title);

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

   -------------------------
   -- Set_Child_Title_Bar --
   -------------------------

   procedure Set_Child_Title_Bar (Child : access MDI_Child_Record'Class) is
   begin
      if not Child.MDI.Draw_Title_Bars then
         Hide (Child.Title_Box);
         Set_Child_Visible (Child.Title_Box, False);
         Set_USize (Child.Title_Box, -1, Child.MDI.Title_Bar_Height);

      else
         Show (Child.Title_Box);
         Set_Child_Visible (Child.Title_Box, True);
      end if;
   end Set_Child_Title_Bar;

   ----------------
   -- Draw_Child --
   ----------------

   procedure Draw_Child
     (Child : access MDI_Child_Record'Class; Area : Gdk_Rectangle)
   is
      pragma Unreferenced (Area);

      Border_Thickness : constant Gint :=
                           Gint (Get_Border_Width (Child.Main_Box));
      GC   : Gdk.Gdk_GC := Child.MDI.Non_Focus_GC;
      W, H : Gint;
      X    : Gint := 1;
   begin
      --  Call this function so that for a dock item is highlighted if the
      --  current page is linked to the focus child.

      if Child.MDI.Focus_Child = MDI_Child (Child) then
         GC := Child.MDI.Focus_GC;
      end if;

      --  Set the color of the notebook page and label.

      Update_Tab_Color (Child);

      if Realized_Is_Set (Child.Title_Area) then
         Draw_Rectangle
           (Get_Window (Child.Title_Area),
            GC, True, 0, 0,
            Gint (Get_Allocation_Width (Child.Title_Area)),
            Child.MDI.Title_Bar_Height);

         if Child.Icon /= null then
            W := Get_Width (Child.Icon);
            H := Get_Height (Child.Icon);

            Render_To_Drawable_Alpha
              (Child.Icon,
               Get_Window (Child.Title_Area),
               Src_X           => 0,
               Src_Y           => 0,
               Dest_X          => X,
               Dest_Y          => (Child.MDI.Title_Bar_Height - H) / 2,
               Width           => W,
               Height          => H,
               Alpha           => Alpha_Full,
               Alpha_Threshold => 128);

            X := X + W + 1;
         end if;

         Set_Text (Child.MDI.Title_Layout, Child.Title.all);
         Get_Pixel_Size (Child.MDI.Title_Layout, W, H);
         Draw_Layout
           (Get_Window (Child.Title_Area),
            Get_White_GC (Get_Style (Child.MDI)),
            X,
            0,
            Child.MDI.Title_Layout);

         if Border_Thickness /= 0 then
            Paint_Shadow
              (Style       => Get_Style (Child),
               Window      => Get_Window (Child),
               State_Type  => State_Normal,
               Shadow_Type => Shadow_Out,
               Widget      => Child,
               X           => 0,
               Y           => 0,
               Width       => Gint (Get_Allocation_Width (Child)),
               Height      => Gint (Get_Allocation_Height (Child)));
         end if;
      end if;
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

   -----------------------
   -- Update_Dnd_Window --
   -----------------------

   procedure Update_Dnd_Window
     (MDI : access MDI_Window_Record'Class; Text : String)
   is
      Frame : Gtk_Frame;
      Box   : Gtk_Box;
   begin
      if MDI.Dnd_Window = null then
         Gtk_New (MDI.Dnd_Window, Window_Popup);
         Set_Transient_For (MDI.Dnd_Window, Gtk_Window (Get_Toplevel (MDI)));
         Set_Position (MDI.Dnd_Window, Win_Pos_Center_On_Parent);

         Gtk_New (Frame);
         Add (MDI.Dnd_Window, Frame);

         Gtk_New_Vbox (Box, Homogeneous => False);
         Add (Frame, Box);
         Set_Border_Width (Box, 10);

         Gtk_New (MDI.Dnd_Window_Label, Text);
         Pack_Start (Box, MDI.Dnd_Window_Label, Expand => True);
         Show_All (MDI.Dnd_Window);
      else
         Set_Text (MDI.Dnd_Window_Label, Text);
      end if;
   end Update_Dnd_Window;

   ------------------------
   -- Destroy_Dnd_Window --
   ------------------------

   procedure Destroy_Dnd_Window (MDI : access MDI_Window_Record'Class) is
   begin
      if MDI.Dnd_Window /= null then
         Destroy (MDI.Dnd_Window);
         MDI.Dnd_Window := null;
      end if;
   end Destroy_Dnd_Window;

   --------------------
   -- Button_Pressed --
   --------------------

   function Button_Pressed
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean is
   begin
      --  It sometimes happens that widgets let events pass through (for
      --  instance scrollbars do that), and thus wouldn't be useable anymore
      --  if we do a grab.

      if Get_Window (Child) /= Get_Window (Event) then
         return False;
      end if;

      return Button_Pressed_Forced (Child, Event);
   end Button_Pressed;

   ---------------------------
   -- Button_Pressed_Forced --
   ---------------------------

   function Button_Pressed_Forced
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      C    : constant MDI_Child := MDI_Child (Child);
      W, H : Gint;
   begin
      C.MDI.In_Drag := No_Drag;

      --  Double-click on left icon => close child
      if Get_Event_Type (Event) = Gdk_2button_Press
        and then Get_Button (Event) = 1
      then
         if C.Icon /= null then
            W := Get_Width (C.Icon);
            H := Get_Height (C.Icon);

            if Gint (Get_X (Event)) <= W
              and then Gint (Get_Y (Event)) <= H
            then
               Close_Child (C);
               return True;
            end if;
         end if;
         return False;

      elsif Get_Event_Type (Event) /= Button_Press
        or else Get_Button (Event) /= 1
      then
         return False;
      end if;

      --  Start a drag-and-drop operation. This won't be effective unless
      --  the user actually drags the mouse a while

      if Traces then
         Put_Line ("Button_Pressed_Forced");
      end if;
      Child_Drag_Begin (C, Event);

      --  Let the event through, the drag hasn't started yet
      return False;
   end Button_Pressed_Forced;

   --------------------
   -- Button_Release --
   --------------------

   function Button_Release
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      C                    : constant MDI_Child := MDI_Child (Child);
      MDI                  : constant MDI_Window := C.MDI;
      Move_Whole_Notebook  : constant Boolean :=
                               (Get_State (Event) and Control_Mask) /= 0;
      Copy_Instead_Of_Move : constant Boolean :=
                               (Get_State (Event) and Shift_Mask) /= 0;
      C2                   : MDI_Child;
      Current              : Gtk_Widget;
      Note                 : Gtk_Notebook;
      Position             : Child_Position;
   begin
      if Traces then
         Put_Line
           ("Button release, drag=" & Drag_Status'Image (C.MDI.In_Drag));
      end if;

      Pointer_Ungrab (Time => 0);

      if Get_Window (Child) /= Get_Window (Event) then
         C.MDI.In_Drag := No_Drag;
         return False;
      end if;

      case C.MDI.In_Drag is
         when In_Pre_Drag =>
            Destroy_Dnd_Window (C.MDI);
            Child_Drag_Finished (C);

         when In_Drag =>
            Destroy_Dnd_Window (C.MDI);
            Draw_Dnd_Rectangle (C.MDI);
            Get_Dnd_Target (C.MDI, Current, Position, C.MDI.Dnd_Rectangle);

            C2 := Dnd_Data (C, Copy => Copy_Instead_Of_Move);
            if C2 = null then
               C2 := C;
            end if;

            if Current = null then
               --  Floating child ?
               Float_Child (C2, True);

            else
               --  Dropped in one of the notebooks
               --  Do nothing if the child is already in the middle area,
               --  and in a notebook that contains only one child, and the
               --  user is dropping on the same notebook

               if C2.State /= Normal
                 or else Current /= Get_Parent (C2)
                 or else (Position in Side_Position
                          and then not Move_Whole_Notebook
                          and then Get_Nth_Page
                            (Gtk_Notebook (Current), 1) /= null)
               then
                  declare
                     Item : Widget_List.Glist := MDI.Items;
                     It   : MDI_Child;
                  begin
                     --  Raise the page that last had it in the same pane

                     if C /= C2 then
                        if Traces then
                           Put_Line ("MDI: Button_Release raising last1 "
                                     & Get_Title (C));
                        end if;
                        Raise_Child (C, False);
                     else
                        while Item /= Widget_List.Null_List loop
                           It := MDI_Child (Get_Data (Item));
                           if It /= C2
                             and then Get_Parent (C2) = Get_Parent (It)
                           then
                              if Traces then
                                 Put_Line ("MDI: Button_Release raising last2 "
                                           & Get_Title (It));
                              end if;
                              Raise_Child (It, False);
                              exit;
                           end if;

                           Item := Widget_List.Next (Item);
                        end loop;
                     end if;
                  end;

                  if Position = Position_Automatic then
                     Note := Gtk_Notebook (Current);
                  else
                     Note := Create_Notebook (MDI);
                  end if;

                  if Move_Whole_Notebook then
                     declare
                        Children : Widget_List.Glist :=
                          Get_Children (Get_Notebook (C2));
                        L : Widget_List.Glist := Children;
                     begin
                        while L /= Null_List loop
                           Put_In_Notebook
                             (C.MDI, MDI_Child (Get_Data (L)), Note,
                              Force_Parent_Destruction => False);
                           L := Next (L);
                        end loop;
                        Free (Children);
                     end;
                  else
                     Put_In_Notebook
                       (C.MDI, C2, Note, Force_Parent_Destruction => False);
                  end if;

                  case Position is
                     when Position_Bottom =>
                        Split
                          (C.MDI,
                           Current, Note, Orientation_Vertical,
                           Width  => 0,
                           Height => 0,
                           After  => True);
                     when Position_Top =>
                        Split
                          (C.MDI,
                           Current, Note, Orientation_Vertical,
                           Width  => 0,
                           Height => 0,
                           After  => False);
                     when Position_Left =>
                        Split
                          (C.MDI,
                           Current, Note, Orientation_Horizontal,
                           Width  => 0,
                           Height => 0,
                           After  => False);
                     when Position_Right =>
                        Split
                          (C.MDI,
                           Current, Note, Orientation_Horizontal,
                           Width  => 0,
                           Height => 0,
                           After  => True);
                     when others =>
                        Emit_By_Name
                          (Get_Object (MDI),
                           "children_reorganized" & ASCII.NUL);
                  end case;
               end if;
            end if;

            Child_Drag_Finished (C);

            if Traces then
               Put_Line ("MDI: Button_Release raising "
                         & Get_Title (C2));
            end if;

            Raise_Child (C2, False);
            if Traces then
               Put_Line ("MDI: Button_Release, set_focus "
                         & Get_Title (C2));
            end if;
            Set_Focus_Child (C2);

         when No_Drag =>
            --  Let the even through, we have nothing to do here
            return False;
      end case;

      C.MDI.In_Drag := No_Drag;
      return True;
   end Button_Release;

   -------------------
   -- Button_Motion --
   -------------------

   function Button_Motion
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      C        : constant MDI_Child := MDI_Child (Child);
      Current  : Gtk_Widget;
      C3       : MDI_Child;
      Note     : Gtk_Notebook;
      Rect2    : Gdk_Rectangle;
      Tmp      : Gdk_Grab_Status;
      Position : Child_Position;
      Delta_X, Delta_Y : Gint;
      pragma Unreferenced (Tmp);

   begin
      if Get_Window (Child) /= Get_Window (Event) then
         return False;
      end if;

      case C.MDI.In_Drag is
         when In_Drag =>
            Get_Dnd_Target (C.MDI, Parent => Current,
                            Position => Position, Rectangle => Rect2);

            --  Show the user what will happen if he drops at the current
            --  location

            if Current = null then
               Update_Dnd_Window (C.MDI, "Float");

            elsif Current = Gtk_Widget (C.MDI) then
               Update_Dnd_Window (C.MDI, "Put in central area");

            elsif Current = Get_Parent (C)
              and then Position = Position_Automatic
            then
               Update_Dnd_Window (C.MDI, "Leave at current position");

            else
               Note := Gtk_Notebook (Current);
               C3  := MDI_Child (Get_Nth_Page (Note, Get_Current_Page (Note)));

               if C3 = null then
                  Update_Dnd_Window (C.MDI, "Put in central area");

               else
                  case Position is
                     when Position_Bottom =>
                        Update_Dnd_Window
                          (C.MDI, "Put below " & Get_Short_Title (C3));
                     when Position_Top =>
                        Update_Dnd_Window
                          (C.MDI, "Put above " & Get_Short_Title (C3));
                     when Position_Left =>
                        Update_Dnd_Window
                          (C.MDI,
                           "Put on the left of " & Get_Short_Title (C3));
                     when Position_Right =>
                        Update_Dnd_Window
                          (C.MDI,
                           "Put on the right of " & Get_Short_Title (C3));
                     when others =>
                        Update_Dnd_Window
                          (C.MDI, "Put on top of " & Get_Short_Title (C3));
                  end case;
               end if;
            end if;

            --  Highlight the destination area itself. This doesn't work on
            --  windows which doesn't support drawing on top of child windows
            --  in the graphic context.

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
            --  If we are still in the tabs area, do nothing so that tabs can
            --  be reordered graphically

            Delta_X := abs (Gint (Get_X_Root (Event)) - C.MDI.Drag_Start_X);
            Delta_Y := abs (Gint (Get_Y_Root (Event)) - C.MDI.Drag_Start_Y);

            Note := Get_Notebook (C);
            if Note /= null
              and then Get_Show_Tabs (Note)
            then
               case Get_Tab_Pos (Note) is
                  when Pos_Top | Pos_Bottom =>
                     if Delta_Y < Drag_Threshold / 2
                        and then Delta_Y < Delta_X
                     then
                        return False;
                     end if;

                  when Pos_Left | Pos_Right =>
                     if Delta_X < Drag_Threshold / 2
                        and then Delta_X < Delta_Y
                     then
                        return False;
                     end if;
               end case;
            end if;

            --  Else start a drag operation if appropriate

            if Delta_X > Drag_Threshold
              or else Delta_Y > Drag_Threshold
            then
               --  If we had a tab reorder operation, but the tab was left at
               --  the same position, the signal "page_reordered" has not been
               --  emitted. Still, the pointer has been ungrabbed, so we do
               --  the following test below, so that we do not start our own
               --  dnd operation

               if not Pointer_Is_Grabbed then
                  return False;
               end if;

               C.MDI.In_Drag := In_Drag;
               C.MDI.Dnd_Rectangle_Owner := null;
               Pointer_Ungrab (Time => 0);

               if C.MDI.Cursor_Fleur = null then
                  Gdk_New (C.MDI.Cursor_Fleur, Fleur);
               end if;
               Tmp := Pointer_Grab
                 (Get_Window (C),
                  False, Button_Motion_Mask or Button_Release_Mask,
                  Cursor => C.MDI.Cursor_Fleur,
                  Time   => 0);
               return True;
            end if;

         when others =>
            null;
      end case;
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

   -------------------------
   -- Child_Widget_Hidden --
   -------------------------

   procedure Child_Widget_Hidden
     (Widget : access Gtk_Widget_Record'Class)
   is
      Child : constant MDI_Child := MDI_Child (Widget);
      Note  : Gtk_Notebook;
   begin
      if Child.State = Floating then
         Hide (Get_Toplevel (Get_Widget (Child)));
      else
         Hide (Child);

         --  At startup, the notebook might be null
         Note := Get_Notebook (Child);
         if Note /= null then
            Configure_Notebook_Tabs (Child.MDI, Note, Hide_If_Empty => True);
         end if;
      end if;
   end Child_Widget_Hidden;

   ------------------------
   -- Child_Widget_Shown --
   ------------------------

   procedure Child_Widget_Shown
     (Widget : access Gtk_Widget_Record'Class)
   is
      Child : constant MDI_Child := MDI_Child (Widget);
      Note  : Gtk_Notebook;
   begin
      if Child.State = Floating then
         Show (Get_Toplevel (Get_Widget (Child)));
      else
         Show (Child);

         --  At startup, the notebook might be null
         Note := Get_Notebook (Child);
         if Note /= null then
            Configure_Notebook_Tabs (Child.MDI, Note, Hide_If_Empty => True);
         end if;
      end if;
   end Child_Widget_Shown;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child        : out MDI_Child;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags        : Child_Flags := All_Buttons;
      Group        : Child_Group := Group_Default;
      Focus_Widget : Gtk.Widget.Gtk_Widget := null) is
   begin
      Child := new MDI_Child_Record;
      Initialize (Child, Widget, Flags, Group, Focus_Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Child        : access MDI_Child_Record'Class;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags        : Child_Flags := All_Buttons;
      Group        : Child_Group := Group_Default;
      Focus_Widget : Gtk.Widget.Gtk_Widget := null)
   is
      Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
                            (1 => (1 => GType_None),
                             2 => (1 => GType_None),
                             3 => (1 => GType_None));
      Button            : Gtk_Button;
      Pix               : Gdk_Pixbuf;
      Pixmap            : Gtk_Image;
      Event             : Gtk_Event_Box;

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

      Child.Initial      := Gtk_Widget (Widget);
      Child.State        := Normal;
      Child.Flags        := Flags;
      Child.Group        := Group;
      Child.Focus_Widget := Focus_Widget;
      Child.MDI          := null;
      Child.Title        := new UTF8_String'(" ");
      Child.Short_Title  := new UTF8_String'(" ");

      Add_Events
        (Child, Button_Press_Mask
           or Button_Motion_Mask
           or Button_Release_Mask
           or Pointer_Motion_Mask);
      Return_Callback.Connect
        (Child, Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Button_Pressed'Access));
      Return_Callback.Connect
        (Child, Signal_Button_Release_Event,
         Return_Callback.To_Marshaller (Button_Release'Access));
      Return_Callback.Connect
        (Child, Signal_Motion_Notify_Event,
         Return_Callback.To_Marshaller (Button_Motion'Access));
      Widget_Callback.Connect
        (Child, Signal_Destroy,
         Widget_Callback.To_Marshaller (Destroy_Child'Access));

      Gtk_New_Vbox (Child.Main_Box, Homogeneous => False, Spacing => 0);
      Add (Child, Child.Main_Box);

      --  Buttons in the title bar

      Gtk_New_Hbox (Child.Title_Box, Homogeneous => False);
      Pack_Start
        (Child.Main_Box, Child.Title_Box, Expand => False, Fill => False);

      Gtk_New (Child.Title_Area);
      Pack_Start
        (Child.Title_Box, Child.Title_Area, Expand => True, Fill => True);

      Return_Callback.Object_Connect
        (Child.Title_Area, Signal_Expose_Event,
         Return_Callback.To_Marshaller (Draw_Child'Access),
         Slot_Object => Child,
         After => True);

      Return_Callback.Object_Connect
        (Child, Signal_Expose_Event,
         Return_Callback.To_Marshaller (Draw_Child'Access),
         Slot_Object => Child,
         After => True);

      if (Flags and Destroy_Button) /= 0 then
         Pix := Gdk_New_From_Xpm_Data (Close_Xpm);
         Gtk_New (Pixmap, Pix);
         Unref (Pix);
         Gtk_New (Button);
         Add (Button, Pixmap);
         Pack_End (Child.Title_Box, Button, Expand => False, Fill => False);
         Widget_Callback.Object_Connect
           (Button, Signal_Clicked,
            Widget_Callback.To_Marshaller (Internal_Close_Child'Access),
            Child);
      end if;

      --  This internal Event box is needed when the child is floated
      Gtk_New (Event);
      Add (Event, Widget);
      Pack_Start
        (Child.Main_Box, Event, Expand => True, Fill => True, Padding => 0);

      Widget_Callback.Object_Connect
        (Child.Initial, Signal_Destroy,
         Widget_Callback.To_Marshaller (Destroy_Initial_Child'Access),
         Child);
      Widget_Callback.Connect
        (Child, Signal_Hide, Child_Widget_Hidden'Access);
      Widget_Callback.Connect
        (Child, Signal_Show, Child_Widget_Shown'Access);
      Widget_Callback.Object_Connect
        (Child.Initial, Signal_Hide, Child_Widget_Hidden'Access, Child);
      Widget_Callback.Object_Connect
        (Child.Initial, Signal_Show, Child_Widget_Shown'Access, Child);
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

         Grab_Focus (F);
      end if;
   end Give_Focus_To_Child;

   ----------------------------------
   -- Give_Focus_To_Previous_Child --
   ----------------------------------

   procedure Give_Focus_To_Previous_Child
     (Child : access MDI_Child_Record'Class)
   is
      Item : Widget_List.Glist;
      It   : MDI_Child;
      Last : MDI_Child;
   begin
      --  Set the focus on the child that had the focus just before,
      --  and in the same notebook, and still visible

      Item := Child.MDI.Items;
      while Item /= Widget_List.Null_List loop
         It := MDI_Child (Get_Data (Item));

         if Visible_Is_Set (It) and then It /= MDI_Child (Child) then
            if Last = null then
               Last := It;
            end if;

            if It.State = Child.State
              and then Get_Parent (It) = Get_Parent (Child)
            then
               if Traces then
                  Put_Line ("MDI: Give_Focus_To_Previous_Child "
                            & Get_Title (It));
               end if;
               Set_Focus_Child (It);
               return;
            end if;
         end if;

         Item := Widget_List.Next (Item);
      end loop;

      --  No such child, give it to the last child that had the focus
      if Last = null then
         if Traces then
            Put_Line ("MDI: Give_Focus_To_Previous_Child: no one");
         end if;
         Child.MDI.Focus_Child := null;
         Emit_By_Name_Child
           (Get_Object (Child.MDI), "child_selected" & ASCII.NUL,
            System.Null_Address);
      else
         Set_Focus_Child (Last);
      end if;
   end Give_Focus_To_Previous_Child;

   ---------
   -- Put --
   ---------

   procedure Put
     (MDI              : access MDI_Window_Record;
      Child            : access MDI_Child_Record'Class;
      Initial_Position : Child_Position := Position_Automatic) is
   begin
      Child.MDI := MDI_Window (MDI);

      Set_USize (Child.Title_Box, -1, MDI.Title_Bar_Height);

      --  We need to show the widget before inserting it in a notebook,
      --  otherwise the notebook page will not be made visible.

      Show_All (Child);
      Set_Child_Title_Bar (Child);

      Child.State := Normal;
      Float_Child (Child, MDI.All_Floating_Mode);

      if not MDI.All_Floating_Mode then
         Put_In_Notebook (MDI, Child, Initial_Position => Initial_Position);
      end if;

      Widget_List.Prepend (MDI.Items, Gtk_Widget (Child));

      if MDI.Menu /= null then
         Create_Menu_Entry (Child);
      end if;

      --  Restore the keyboard focus, which might have been stolen if the new
      --  child was added to a notebook.

      Give_Focus_To_Child (MDI.Focus_Child);

      Emit_By_Name_Child
        (Get_Object (MDI), "child_added" & ASCII.NUL, Get_Object (Child));
   end Put;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (MDI        : access MDI_Window_Record;
      Child      : access MDI_Child_Record'Class;
      Width      : Glib.Gint;
      Height     : Glib.Gint;
      Fixed_Size : Boolean := False)
   is
      Notebook : constant Gtk_Notebook := Get_Notebook (Child);
   begin
      --  Ignore specific size requests while loading the desktop, since the
      --  latter should force the size
      if not MDI.Loading_Desktop and then Notebook /= null then
         --  Only take this into account if we have a single page
         if Get_Nth_Page (Notebook, 1) = null
           and then MDI.Show_Tabs_Policy /= Always
         then
            Set_Size (MDI,
                      Widget     => Notebook,
                      Width      => Width, Height => Height,
                      Fixed_Size => Fixed_Size);
         else
            Set_Size_Request (Child, Width, Height);
            Set_Size (MDI,
                      Widget     => Notebook,
                      Width      => Width,
                      Height     => Height
                      + Get_Allocation_Height (Notebook)
                      - Get_Allocation_Height (Child),
                      Fixed_Size => Fixed_Size);
         end if;
      end if;
   end Set_Size;

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
      Label  : Gtk_Accel_Label;
      Pixmap : Gtk_Image;
      Pix    : Gdk_Pixmap;
      Mask   : Gdk_Bitmap;
      Box    : Gtk_Box;
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
            Gdk.Drawable.Unref (Pix);
            Gdk.Drawable.Unref (Mask);
         end if;

         Gtk_New (Label, Child.Short_Title.all);
         Set_Alignment (Label, 0.0, 0.5);
         Set_Accel_Widget (Label, Child.Menu_Item);
         Pack_Start (Box, Label,  Expand => True, Fill => True);

         Show_All (Box);
         Add (Child.Menu_Item, Box);

         Set_Accel_Path
           (Child.Menu_Item, Child.MDI.Accel_Path_Prefix.all
            & "/window/child/" & Child.Short_Title.all,
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

      Emit_By_Name_Child
        (Get_Object (Child.MDI), "child_icon_changed" & ASCII.NUL,
         Get_Object (Child));
   end Set_Icon;

   --------------
   -- Get_Icon --
   --------------

   function Get_Icon
     (Child : access MDI_Child_Record) return Gdk.Pixbuf.Gdk_Pixbuf is
   begin
      return Child.Icon;
   end Get_Icon;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Child       : access MDI_Child_Record;
      Title       : UTF8_String;
      Short_Title : UTF8_String := "")
   is
      Title_Changed       : constant Boolean := Child.Title = null
                              or else Child.Title.all /= Title;
      Short_Title_Changed : constant Boolean := Child.Short_Title = null
                              or else Child.Short_Title.all /= Short_Title;
      The_Title           : String_Access;
      The_Short_Title     : String_Access;
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

      if Title_Changed and then Child.State = Floating then
         Set_Title
           (Gtk_Window (Get_Toplevel (Child.Initial)),
            Locale_From_UTF8 (Title));
      end if;

      if Short_Title_Changed then
         Update_Tab_Label (Child);

         --  Update the menu, if it exists. We need to recreate the menu item
         --  to keep it sorted

         if Child.Menu_Item /= null then
            Destroy (Child.Menu_Item);
            Create_Menu_Entry (Child);
         end if;
      end if;

      if Title_Changed or else Short_Title_Changed then
         if Get_Window (Child) /= Null_Window then
            Queue_Draw (Child);
         end if;
         if Child.MDI /= null then
            Emit_By_Name_Child
              (Get_Object (Child.MDI), "child_title_changed" & ASCII.NUL,
               Get_Object (Child));
         end if;
      end if;
   end Set_Title;

   --------------------
   -- Find_MDI_Child --
   --------------------

   function Find_MDI_Child
     (MDI    : access MDI_Window_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return MDI_Child
   is
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

   --------------------------------
   -- Find_MDI_Child_From_Widget --
   --------------------------------

   function Find_MDI_Child_From_Widget
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return MDI_Child
   is
      W   : Gtk_Widget := Gtk_Widget (Widget);
      Win : Gtk_Window;
   begin
      --  As a special case, if the widget's parent is a notebook, we check
      --  whether the associated page is a MDI child, and behave as if that
      --  child had the focus (EC19-008)

      while W /= null loop
         if W.all in MDI_Child_Record'Class then
            return MDI_Child (W);

         elsif W.all in Gtk_Notebook_Record'Class
           and then Get_Nth_Page
             (Gtk_Notebook (W), Get_Current_Page (Gtk_Notebook (W))).all
              in MDI_Child_Record'Class
         then
            return MDI_Child
              (Get_Nth_Page
                 (Gtk_Notebook (W), Get_Current_Page (Gtk_Notebook (W))));
         end if;

         W := Get_Parent (W);
      end loop;

      --  Not found ? We might have a floating window. Unfortunately, these
      --  windows do not keep track of the MDI child they belong to...

      Win := Gtk_Window (Get_Toplevel (Widget));
      if Win /= null then
         begin
            return Child_User_Data.Get (Win, "parent_mdi_child");
         exception
            when Gtkada.Types.Data_Error =>
               return null;
         end;
      else
         return null;
      end if;
   end Find_MDI_Child_From_Widget;

   ---------------------------
   -- Find_MDI_Child_By_Tag --
   ---------------------------

   function Find_MDI_Child_By_Tag
     (MDI : access MDI_Window_Record;
      Tag : Ada.Tags.Tag) return MDI_Child
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
     (MDI  : access MDI_Window_Record;
      Name : String) return MDI_Child
   is
      Child : MDI_Child;
      Iter  : Child_Iterator := First_Child (MDI);
   begin
      loop
         Child := Get (Iter);
         exit when Child = null
           or else Child.Title.all = Name
           or else Child.Short_Title.all = Name;
         Next (Iter);
      end loop;

      return Get (Iter);
   end Find_MDI_Child_By_Name;

   -----------------
   -- Lower_Child --
   -----------------

   procedure Lower_Child (Child : access MDI_Child_Record'Class) is
      Note : Gtk_Notebook;
   begin
      Ref (Child);
      Remove (Child.MDI.Items, Gtk_Widget (Child));
      Append (Child.MDI.Items, Gtk_Widget (Child));
      Unref (Child);

      if Child.State = Normal then
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

   ---------------
   -- Is_Raised --
   ---------------

   function Is_Raised (Child : access MDI_Child_Record'Class) return Boolean is
      Note : Gtk_Notebook;
   begin
      case Child.State is
         when Floating =>
            return True;
         when Normal =>
            Note := Get_Notebook (Child);
            return Get_Nth_Page (Note, Get_Current_Page (Note)) =
              Gtk_Widget (Child);
      end case;
   end Is_Raised;

   -----------------
   -- Raise_Child --
   -----------------

   procedure Raise_Child
     (Child : access MDI_Child_Record'Class; Give_Focus : Boolean := True)
   is
      Old_Focus     : constant MDI_Child := Child.MDI.Focus_Child;
      Note          : Gtk_Notebook;
      Current_Focus : MDI_Child;
      Give          : Boolean := Give_Focus;
   begin
      Show (Child);  --  Make sure the child is visible

      --  For a docked item, we in fact want to raise its parent dock,
      --  and make sure the current page in that dock is the correct one.

      if Child.State = Normal then
         Note := Get_Notebook (Child);
         Current_Focus := Child.MDI.Focus_Child;

         --  We'll have to transfer the focus if the current focus window is in
         --  the same dock, since otherwise that means an invisible window
         --  would have the focus.

         if Current_Focus /= null
           and then Current_Focus.State = Normal
           and then Get_Notebook (Current_Focus) = Note
         then
            Give := True;
         end if;

         --  Temporary fool the system, so that the child doesn't necessarily
         --  gain the focus. Otherwise, switching a notebook page gives the
         --  child the focus.
         Child.MDI.Focus_Child := MDI_Child (Child);

         --  There could be no parent if we are in all-floating mode
         if Note /= null then
            Set_Current_Page (Note, Page_Num (Note, Child));
         end if;
         Child.MDI.Focus_Child := Current_Focus;

      elsif Child.State = Floating
        and then Give_Focus
        and then Realized_Is_Set (Child.Initial)
      then
         Present (Gtk_Window (Get_Toplevel (Child.Initial)));

      elsif Realized_Is_Set (Child) then
         Gdk.Window.Gdk_Raise (Get_Window (Child));
      end if;

      --  Give the focus to the Focus_Child, since the notebook page switch
      --  might have changed that.

      if not Child.MDI.Loading_Desktop then
         if not Give then
            --  This must be done even if Old_Focus = MDI.Focus_Child.
            --  Otherwise, clicking inside an editor in GPS for instance will
            --  not properly refresh the outline view
            Give_Focus_To_Child (Old_Focus);
         else
            if Traces then
               Put_Line ("MDI: Raise_Child, give focus to "
                         & Get_Title (Child));
            end if;
            Set_Focus_Child (Child);
         end if;
      end if;
   end Raise_Child;

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

   ----------------------
   -- Update_Tab_Color --
   ----------------------

   procedure Update_Tab_Color
     (Child : access MDI_Child_Record'Class;
      Force : Boolean := False)
   is
      Color : Gdk_Color := Get_Bg (Get_Default_Style, State_Normal);
      Note  : constant Gtk_Notebook := Get_Notebook (Child);
      Label : Gtk_Widget;

      function Color_Equal (A, B : Gdk_Color) return Boolean;
      --  Coloc comparison not taking into account the Pixel value.

      -----------------
      -- Color_Equal --
      -----------------

      function Color_Equal (A, B : Gdk_Color) return Boolean is
      begin
         return Red (A) = Red (B)
           and then Green (A) = Green (B)
           and then Blue (A) = Blue (B);
      end Color_Equal;

   begin
      if not Force and then MDI_Child (Child) = Child.MDI.Focus_Child then
         Color := Child.MDI.Focus_Title_Color;
      end if;

      if (Force or else not Child.MDI.Draw_Title_Bars)
        and then Note /= null
      then
         --  If the color is already being applied to this notebook, avoid
         --  the call to Modify_BG, which is quite costly since it causes
         --  a queue_resize on the notebook.
         --  Also avoids a potential loop caused by the behavior above.

         if not Color_Equal (Get_Bg (Get_Style (Note), State_Normal), Color)
           and then not Color_Equal
             (Get_Bg (Gtk.Rc.Get_Style (Note), State_Normal), Color)
         then
            Modify_Bg (Note, State_Normal, Color);
            Label := Get_Tab_Label (Note, Child);
            if Label /= null then
               Modify_Bg (Label, State_Normal, Color);
            end if;
         end if;
      end if;
   end Update_Tab_Color;

   ---------------------
   -- Set_Focus_Child --
   ---------------------

   procedure Set_Focus_Child (Child : access MDI_Child_Record) is
      Old : constant MDI_Child := Child.MDI.Focus_Child;
      C   : constant MDI_Child := MDI_Child (Child);
      Tmp : Boolean;
      pragma Unreferenced (Tmp);

      Previous_Focus_Child : constant MDI_Child := Child.MDI.Focus_Child;
   begin
      if Child.MDI.Loading_Desktop then
         return;
      end if;

      --  Be lazy. And avoid infinite loop when updating the MDI menu...

      if C = Old or else Gtk.Object.In_Destruction_Is_Set (C.MDI) then
         return;
      end if;

      --  It is possible that this function is called before the child is
      --  even in the list of items. In this case, we do nothing at this
      --  point (might be called because we insert the child in a notebook
      --  first for instance)

      if Widget_List.Find (C.MDI.Items, Gtk_Widget (Child)) = Null_List then
         return;
      end if;

      Show (C);  --  Make sure the child is visible
      Child.MDI.Focus_Child := C;

      if Traces then
         Put_Line ("MDI: Set_Focus_Child on " & Get_Title (C));
      end if;

      if Previous_Focus_Child /= null then
         Update_Tab_Color (Previous_Focus_Child);
      end if;

      Update_Tab_Color (C);

      Ref (C);
      Remove (C.MDI.Items, Gtk_Widget (Child));
      Prepend (C.MDI.Items, Gtk_Widget (Child));
      Unref (C);

      --  Make sure the page containing Child in a notebook is put on top.
      --  Do not raise floating children, since this is the role of the window
      --  manager.

      if C.State /= Floating then
         if Traces then
            Put_Line ("MDI: Set_Focus_Child, raise child " & Get_Title (C));
         end if;
         Raise_Child (C, False);
      end if;

      --  Give the actual keyboard focus to the appropriate subwindow of
      --  the focus child.

      Give_Focus_To_Child (Child.MDI.Focus_Child);

      if Old /= null
        and then Realized_Is_Set (Old)
      then
         Queue_Draw_Area
           (Old, Border_Thickness, Border_Thickness,
            Gint (Get_Allocation_Width (Old)) - 2 * Border_Thickness,
            Child.MDI.Title_Bar_Height);
      end if;

      if Realized_Is_Set (C.Initial) then
         Queue_Draw_Area
           (C, Border_Thickness, Border_Thickness,
            Gint (Get_Allocation_Width (C)) - 2 * Border_Thickness,
            Child.MDI.Title_Bar_Height);

         --  Give the focus to the window containing the child.
         --  Giving the focus to a window has the side effect of moving the
         --  window to the current desktop. Therefore, we only do this when the
         --  input focus was already on a window of the MDI.

         if not Child.MDI.Loading_Desktop
           and then Previous_Focus_Child /= null
           and then Realized_Is_Set
             (Get_Toplevel (Previous_Focus_Child.Initial))
           and then Get_Property
             (Get_Toplevel (Previous_Focus_Child.Initial),
              Has_Toplevel_Focus_Property)
         then
            Raise_Child (C);
         end if;
      end if;

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

   ------------------
   -- Delete_Child --
   ------------------

   function Delete_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean is
   begin
      if In_Destruction_Is_Set (MDI_Child (Child).MDI) then
         --  We can always close a child when the MDI is being destroyed
         return False;

      elsif MDI_Child (Child).MDI.Close_Floating_Is_Unfloat
        and then (MDI_Child (Child).Flags and Always_Destroy_Float) = 0
        and then not MDI_Child (Child).MDI.All_Floating_Mode
      then
         Float_Child (MDI_Child (Child), False);

         if Traces then
            Put_Line
              ("MDI: Delete_Child, raising " & Get_Title (MDI_Child (Child)));
         end if;
         Raise_Child (MDI_Child (Child), False);
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
           (Win, Signal_Key_Press_Event, Event);
      else
         return Return_Callback.Emit_By_Name
           (Win, Signal_Key_Release_Event, Event);
      end if;
   end Key_Event_In_Floating;

   -----------------
   -- Float_Child --
   -----------------

   procedure Float_Child
     (Child : access MDI_Child_Record'Class;
      Float : Boolean) is
   begin
      Internal_Float_Child
        (Child, Float, Position_At_Mouse => True, X => 0, Y => 0);
   end Float_Child;

   --------------------------
   -- Internal_Float_Child --
   --------------------------

   procedure Internal_Float_Child
     (Child             : access MDI_Child_Record'Class;
      Float             : Boolean;
      Position_At_Mouse : Boolean;
      X, Y              : Gint)
   is
      use Object_List;
      Diag        : Gtk_Dialog;
      Win         : Gtk_Window;
      Cont        : Gtk_Container;
      Requisition : Gtk_Requisition;
      Groups      : Object_List.GSlist;
      W, H        : Gint;
   begin
      if Traces then
         Put_Line ("Float_Child " & Get_Title (Child)
                   & " State=" & State_Type'Image (Child.State)
                   & " Float=" & Boolean'Image (Float));
      end if;

      --  If the Child already has a window, the resulting floating window
      --  should have the same size.
      --  Otherwise, ask the Child for its requisiton.

      if Mapped_Is_Set (Child) then
         W := Get_Allocation_Width (Child);
         H := Get_Allocation_Height (Child);
      else
         Size_Request (Child, Requisition);
         W := Requisition.Width;
         H := Requisition.Height;
      end if;

      if Child.State /= Floating and then Float then
         --  Ref is removed when the child is unfloated
         Ref (Child);

         --  This could be called before the child even has a parent if
         --  All_Floating_Mode is set.

         if Get_Parent (Child) /= null then
            Remove (Gtk_Container (Get_Parent (Child)), Child);
         end if;

         if (Child.Flags and Float_As_Transient) /= 0 then
            declare
               Parent : Gtk_Window;
               Item   : Widget_List.Glist;
               It     : MDI_Child;
            begin
               --  If the current child is floating, we do not want to float
               --  the dialog as transient for the main window, but for the
               --  current child.
               --  ??? Should we introduce a flag for childs that are allways
               --  transient for the main window ?

               Item := Child.MDI.Items;
               while Item /= Widget_List.Null_List loop
                  It := MDI_Child (Get_Data (Item));

                  if It /= MDI_Child (Child) then
                     if It.State = Floating
                       and then Realized_Is_Set (It.Initial)
                     then
                        Parent := Gtk_Window (Get_Toplevel (It.Initial));
                     else
                        Parent := Gtk_Window (Get_Toplevel (Child.MDI));
                     end if;

                     exit;
                  end if;

                  Item := Widget_List.Next (Item);
               end loop;

               Gtk_New (Diag,
                        Title  => Child.Title.all,
                        Parent => Parent,
                        Flags  => No_Separator or Destroy_With_Parent);
            end;

            Set_Has_Separator (Diag, False);
            Win  := Gtk_Window (Diag);
            Cont := Gtk_Container (Get_Vbox (Diag));
         else
            Gtk_New (Win);

            if Child.MDI.Use_Short_Titles_For_Floats then
               Set_Title (Win, Locale_From_UTF8 (Child.Short_Title.all));
            else
               Set_Title (Win, Locale_From_UTF8 (Child.Title.all));
            end if;

            Cont := Gtk_Container (Win);
         end if;

         Set_Default_Size (Win, W, H);

         --  Memorize the MDI_Child associated with the window, for faster
         --  lookup for instance in Find_MDI_Child_From_Widget.

         Child_User_Data.Set (Win, MDI_Child (Child), "parent_mdi_child");

         --  Set the accelerators for this window, so that menu key shortcuts
         --  behave the same as in the main window.
         --  ??? Should we do the same for mnemonics, even though the menu
         --  bar isn't available on this floating window.

         Groups := From_Object (Get_Toplevel (Child.MDI));
         while Groups /= Object_List.Null_List loop
            Add_Accel_Group (Win, Gtk_Accel_Group (Get_Data (Groups)));
            Groups := Next (Groups);
         end loop;

         if Position_At_Mouse then
            Set_Position (Win, Win_Pos_Mouse);
         else
            Set_UPosition (Win, X, Y);
            Set_Position (Win, Win_Pos_None);
         end if;

         --  Delete_Event should be forwarded to the child, not to the
         --  toplevel window

         Return_Callback.Object_Connect
           (Win, Signal_Delete_Event,
            Return_Callback.To_Marshaller (Delete_Child'Access), Child);

         Add_Events (Win, Enter_Notify_Mask);
         Return_Callback.Object_Connect
           (Win, Signal_Focus_In_Event,
            Return_Callback.To_Marshaller
               (Set_Focus_Child_MDI_Floating'Access),
            Child);

         --  Forward all key events to the toplevel of the MDI. This provides
         --  proper handling of menu key shortcuts.

         Return_Callback.Object_Connect
           (Win, Signal_Key_Press_Event,
            Return_Callback.To_Marshaller (Key_Event_In_Floating'Access),
            Gtk_Window (Get_Toplevel (Child.MDI)), After => True);
         Return_Callback.Object_Connect
           (Win, Signal_Key_Release_Event,
            Return_Callback.To_Marshaller (Key_Event_In_Floating'Access),
            Gtk_Window (Get_Toplevel (Child.MDI)), After => True);

         Reparent (Get_Parent (Child.Initial), Cont);

         Child.State := Floating;
         Update_Float_Menu (Child);
         Emit_By_Name_Child (Get_Object (Child.MDI), "float_child" & ASCII.NUL,
                             Get_Object (Child));
         Widget_Callback.Emit_By_Name (Child, "float_child");
         Show_All (Win);

      elsif Child.State = Floating and then not Float then
         --  Reassign the widget to Child instead of the notebook

         Win := Gtk_Window (Get_Toplevel (Child.Initial));
         Reparent (Get_Parent (Child.Initial),
                   New_Parent => Gtk_Box (Get_Child (Child)));
         Child.State := Normal;
         Destroy (Win);

         Put_In_Notebook (Child.MDI, Child);

         Update_Float_Menu (Child);
         Unref (Child);
         Widget_Callback.Emit_By_Name (Child, "unfloat_child");
      end if;
   end Internal_Float_Child;

   -----------------
   -- Is_Floating --
   -----------------

   function Is_Floating
     (Child : access MDI_Child_Record'Class) return Boolean is
   begin
      return Child.State = Floating;
   end Is_Floating;

   ----------------
   -- On_Tab_Pos --
   ----------------

   package Tab_Pos_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Notebook_Record, Gtk.Enums.Gtk_Position_Type);
   procedure On_Tab_Pos
     (Note : access Gtk_Notebook_Record'Class;
      Pos  : Gtk.Enums.Gtk_Position_Type);

   procedure On_Tab_Pos
     (Note : access Gtk_Notebook_Record'Class;
      Pos  : Gtk.Enums.Gtk_Position_Type) is
   begin
      Set_Tab_Pos (Note, Pos);
   end On_Tab_Pos;

   -------------
   -- Get_MDI --
   -------------

   function Get_MDI (Child : access MDI_Child_Record) return MDI_Window is
   begin
      return Child.MDI;
   end Get_MDI;

   -------------------------------------
   -- Set_Tab_Contextual_Menu_Factory --
   -------------------------------------

   procedure Set_Tab_Contextual_Menu_Factory
     (MDI     : access MDI_Window_Record;
      Factory : Tab_Contextual_Menu_Factory)
   is
   begin
      MDI.Tab_Factory := Factory;
   end Set_Tab_Contextual_Menu_Factory;

   ------------------------------
   -- On_Notebook_Button_Press --
   ------------------------------

   function On_Notebook_Button_Press
     (Child    : access Gtk_Widget_Record'Class;
      Event    : Gdk.Event.Gdk_Event) return Boolean
   is
      C    : constant MDI_Child := MDI_Child (Child);
      Note : constant Gtk_Notebook := Get_Notebook (C);
      Menu : Gtk_Menu;
      Submenu : Gtk_Menu;
      Item : Gtk_Menu_Item;
   begin
      if Get_Button (Event) = 3 then
         Gtk_New (Menu);

         Gtk_New (Item, "Close");
         Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate, Close_Cb'Access, Child);
         Append (Menu, Item);

         Gtk_New (Item, "Tabs location");
         Append (Menu, Item);

         Gtk_New (Submenu);
         Set_Submenu (Item, Submenu);

         Gtk_New (Item, "Top");
         Tab_Pos_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Tab_Pos'Access, Note, Pos_Top);
         Append (Submenu, Item);

         Gtk_New (Item, "Bottom");
         Tab_Pos_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Tab_Pos'Access, Note, Pos_Bottom);
         Append (Submenu, Item);

         Gtk_New (Item, "Left");
         Tab_Pos_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Tab_Pos'Access, Note, Pos_Left);
         Append (Submenu, Item);

         Gtk_New (Item, "Right");
         Tab_Pos_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Tab_Pos'Access, Note, Pos_Right);
         Append (Submenu, Item);

         if C.MDI.Tab_Factory /= null then
            C.MDI.Tab_Factory (C, Menu);
         end if;

         Show_All (Menu);
         Popup (Menu,
                Button        => 3,
                Activate_Time => Gdk.Event.Get_Time (Event));
         return True;
      end if;
      return False;
   end On_Notebook_Button_Press;

   ---------------------
   -- Create_Notebook --
   ---------------------

   function Create_Notebook
     (MDI : access MDI_Window_Record'Class) return Gtk_Notebook
   is
      Notebook : Gtk_Notebook;
   begin
      Notebook := new MDI_Notebook_Record;
      Gtk.Notebook.Initialize (Notebook);
      Configure_Notebook_Tabs (MDI, Notebook);
      Set_Show_Border (Notebook, False);
      Set_Border_Width (Notebook, 0);
      Set_Scrollable (Notebook);
      Set_Tab_Pos  (Notebook, MDI.Tabs_Position);

      Widget_Callback.Connect
        (Notebook, Signal_Remove, Removed_From_Notebook'Access);
      Widget_Callback.Connect
        (Notebook, Signal_Set_Focus_Child, Set_Focus_Child_Notebook'Access);
      Widget_Callback.Connect
        (Notebook, Signal_Switch_Page,
         Set_Focus_Child_Switch_Notebook_Page'Access);
      return Notebook;
   end Create_Notebook;

   -----------------------------
   -- Configure_Notebook_Tabs --
   -----------------------------

   procedure Configure_Notebook_Tabs
     (MDI      : access MDI_Window_Record'Class;
      Notebook : access Gtk_Notebook_Record'Class;
      Hide_If_Empty : Boolean := False)
   is
      Child : MDI_Child;
      Page_Count : constant Gint := Get_N_Pages (Notebook);
      Visible_Page_Count : Natural := 0;
   begin
      Set_Property (Notebook, Tab_Border_Property, 0);

      --  Some pages might be hidden, in which case they should not be counted
      --  when we compute whether the tabs should be made visible

      for P in 0 .. Page_Count - 1 loop
         if Visible_Is_Set (Get_Nth_Page (Notebook, P)) then
            Visible_Page_Count := Visible_Page_Count + 1;
         end if;
      end loop;

      if Visible_Page_Count >= 2 then
         Set_Show_Tabs (Notebook, MDI.Show_Tabs_Policy /= Never);
      else
         Set_Show_Tabs (Notebook, MDI.Show_Tabs_Policy = Always);
      end if;

      Child := MDI_Child (Get_Nth_Page (Notebook, 0));
      if Child = null then
         null;
      elsif Get_Nth_Page (Notebook, 1) /= null
         or else MDI.Show_Tabs_Policy = Always
      then
         Set_Border_Width (Child.Main_Box, 0);
      else
         Set_Border_Width (Child.Main_Box, Guint (Small_Border_Thickness));
      end if;

      if Hide_If_Empty then
         if Visible_Page_Count = 0 then
            Hide (Notebook);
         else
            Show (Notebook);
         end if;
      end if;
   end Configure_Notebook_Tabs;

   ----------------------
   -- Update_Tab_Label --
   ----------------------

   procedure Update_Tab_Label (Child : access MDI_Child_Record'Class) is
      Note   : constant Gtk_Notebook := Get_Notebook (Child);
      Event  : Gtk_Event_Box;
      Box    : Gtk_Box;
      Pix    : Gdk_Pixmap;
      Mask   : Gdk_Bitmap;
      Pixmap : Gtk_Image;
   begin
      if Note /= null and then Child.State = Normal then
         Gtk_New (Event);

         --  This fails with gtk+ 2.2.0,
         --         Set_Flags (Event, No_Window);
         --  Instead, for 2.4.0, we use the following proper call,
         --  even though the corresponding function doesn't exist in
         --  2.2. This means that 2.2 will have a bug that sometimes
         --  the background color of tabs, when no title bars are
         --  displayed, will not be correct, with a grey rectangle
         --  where the label is.

         Set_Visible_Window (Event, False);

         Gtk_New (Child.Tab_Label, Child.Short_Title.all);

         if Child.Icon /= null then
            Gtk_New_Hbox (Box, Homogeneous => False);

            Render_Pixmap_And_Mask_For_Colormap
              (Child.Icon, Get_Default_Colormap, Pix, Mask, 128);
            Gtk_New (Pixmap, Pix, Mask);
            Gdk.Drawable.Unref (Pix);
            Gdk.Drawable.Unref (Mask);
            Pack_Start (Box, Pixmap, Expand => False);
            Pack_Start (Box, Child.Tab_Label,  Expand => True, Fill => True);
            Add (Event, Box);
         else
            Add (Event, Child.Tab_Label);
         end if;

         Set_Tab_Label (Note, Child, Event);
         Show_All (Event);

         Update_Tab_Color (Child);

         Return_Callback.Object_Connect
           (Event, Signal_Button_Press_Event,
            Return_Callback.To_Marshaller
            (Set_Focus_Child_MDI_From_Tab'Access),
            Child);
         Return_Callback.Object_Connect
           (Event, Signal_Button_Press_Event,
            Return_Callback.To_Marshaller (On_Notebook_Button_Press'Access),
            Child);
         Return_Callback.Object_Connect
           (Event, Signal_Button_Release_Event,
            Return_Callback.To_Marshaller
            (Set_Focus_Child_MDI_From_Tab'Access),
            Child);

         --  Setup drag-and-drop, so that items can be moved from one location
         --  to another.

         Set_Dnd_Source (Event, Child);
      end if;
   end Update_Tab_Label;

   -----------------
   -- Note_Notify --
   -----------------

   procedure Note_Notify (Data : System.Address; Where : System.Address) is
      pragma Unreferenced (Where);
      Old_Note_Was_Destroyed : aliased Boolean;
      for Old_Note_Was_Destroyed'Address use Data;
   begin
      Old_Note_Was_Destroyed := True;
   end Note_Notify;

   ---------------------
   -- Put_In_Notebook --
   ---------------------

   procedure Put_In_Notebook
     (MDI                      : access MDI_Window_Record'Class;
      Child                    : access MDI_Child_Record'Class;
      Notebook                 : Gtk_Notebook := null;
      Initial_Position         : Child_Position := Position_Automatic;
      Force_Parent_Destruction : Boolean := True)
   is
      Note, Old_Note         : Gtk_Notebook;
      Destroy_Old            : Boolean := False;
      Old_Note_Was_Destroyed : aliased Boolean := False;

   begin
      --  Embed the contents of the child into the notebook

      if Notebook /= null then
         Note := Notebook;
      else
         Note := Find_Current_In_Central (MDI, Child.Group, Initial_Position);
      end if;

      if Get_Parent (Child) = Gtk_Widget (Note) then
         return;
      end if;

      Ref (Child);

      if Get_Parent (Child) /= null then
         Old_Note := Gtk_Notebook (Get_Parent (Child));

         --  Always destroy the notebook we were in, since we are
         --  putting the item elsewhere anyway, there will still be
         --  a notebook for items in the same position.

         Destroy_Old := Force_Parent_Destruction
           and then not MDI_Notebook (Old_Note).Is_Default_Notebook
           and then Get_Nth_Page (Old_Note, 1) = null;

         Weak_Ref
           (Old_Note, Note_Notify'Access, Old_Note_Was_Destroyed'Address);
         Remove (Old_Note, Child);

         if not Old_Note_Was_Destroyed then
            Weak_Unref
              (Old_Note, Note_Notify'Access, Old_Note_Was_Destroyed'Address);
         end if;

         --  Problem: Old_Note might no longer exist not, since
         --  Removed_From_Notebook might have destroyed it.

         if Destroy_Old and then not Old_Note_Was_Destroyed then
            Destroy (Old_Note);
         end if;
      end if;

      Child.State := Normal;

      Append_Page (Note, Child);
      Set_Tab_Reorderable (Note, Child, Reorderable => True);

      Configure_Notebook_Tabs (MDI, Note);

      Update_Tab_Label (Child);

      Set_Child_Visible (Note, True);
      Show (Note);
      Queue_Resize (Note);

      if Child.Group = Group_Default then
         declare
            Children : Widget_List.Glist := Get_Children (MDI);
            L        : Widget_List.Glist := Children;
            N        : MDI_Notebook;
         begin
            while L /= Null_List loop
               N := MDI_Notebook (Get_Data (L));
               N.Is_Default_Notebook := False;
               L := Next (L);
            end loop;
            Free (Children);
         end;
      end if;

      Unref (Child);
   end Put_In_Notebook;

   -------------------------
   -- Find_Empty_Notebook --
   -------------------------

   function Find_Empty_Notebook
     (MDI : access MDI_Window_Record'Class) return Gtk_Notebook
   is
      Children : Widget_List.Glist := Get_Children (MDI);
      L        : Widget_List.Glist := Children;
      N        : Gtk_Notebook;
   begin
      while L /= Null_List loop
         N := Gtk_Notebook (Get_Data (L));

         if Get_Nth_Page (N, 0) = null then
            Free (Children);
            return N;
         end if;

         L := Next (L);
      end loop;

      Free (Children);
      return null;
   end Find_Empty_Notebook;

   -----------------------------
   -- Find_Current_In_Central --
   -----------------------------

   function Find_Current_In_Central
     (MDI              : access MDI_Window_Record'Class;
      Group            : Child_Group := Group_Any;
      Initial_Position : Child_Position := Position_Automatic)
      return Gtk_Notebook
   is
      List                  : Widget_List.Glist := MDI.Items;
      C                     : MDI_Child;
      Note                  : Gtk_Notebook;
      Current               : Gtk_Notebook;
      Default_Current_Found : Boolean := False;
   begin
      --  Do we already have a child within the same group ?

      while List /= Widget_List.Null_List loop
         C := MDI_Child (Get_Data (List));

         if C.State = Normal then
            Note := Get_Notebook (C);
            if Current = null then
               Current := Note;
            end if;

            if not Default_Current_Found
              and then C.Group = Group_Default
            then
               Default_Current_Found := True;
               Current := Note;
            end if;

            exit when Note /= null
              and then (Group = Group_Any or else C.Group = Group);
            Note := null;
         end if;

         List := Next (List);
      end loop;

      --  Look for an empty notebook

      if Note = null and then Initial_Position = Position_Automatic then
         Note := Find_Empty_Notebook (MDI);
      end if;

      if Note = null then
         case Initial_Position is
            when Position_Bottom =>
               Note := Create_Notebook (MDI);
               Split (MDI,
                      New_Child   => Note,
                      Ref_Pane    => Root_Pane,
                      Orientation => Orientation_Vertical,
                      Width       => -1,
                      Height      => -1,
                      After       => True);
            when Position_Top =>
               Note := Create_Notebook (MDI);
               Split (MDI,
                      New_Child   => Note,
                      Ref_Pane    => Root_Pane,
                      Orientation => Orientation_Vertical,
                      Width       => -1,
                      Height      => -1,
                      After       => False);
            when Position_Left =>
               Note := Create_Notebook (MDI);
               Split (MDI,
                      New_Child   => Note,
                      Ref_Pane    => Root_Pane,
                      Orientation => Orientation_Horizontal,
                      Width       => -1,
                      Height      => -1,
                      After       => False);
            when Position_Right =>
               Note := Create_Notebook (MDI);
               Split (MDI,
                      New_Child   => Note,
                      Ref_Pane    => Root_Pane,
                      Orientation => Orientation_Horizontal,
                      Width       => -1,
                      Height      => -1,
                      After       => True);
            when others =>
               if Current /= null then
                  Note := Current;
               else
                  Note := Create_Notebook (MDI);
                  Add_Child (MDI, New_Child => Note);
               end if;
         end case;
      end if;

      return Note;
   end Find_Current_In_Central;

   ---------------------------
   -- Set_All_Floating_Mode --
   ---------------------------

   procedure Set_All_Floating_Mode
     (MDI : access MDI_Window_Record; All_Floating : Boolean)
   is
      List : Widget_List.Glist := First (MDI.Items);
      C    : MDI_Child;
   begin
      if All_Floating /= MDI.All_Floating_Mode then
         --  We cannot do a simple loop here. When a child is floated, it
         --  can happen that the mouse enters the window, and the focus changes
         --  immediately, resulting in a change in the order of children in the
         --  list, even though not all windows have been floated yet.

         MDI.All_Floating_Mode := All_Floating;

         loop
            List := First (MDI.Items);

            while List /= Null_List loop
               C := MDI_Child (Get_Data (List));
               if (C.State /= Floating and then All_Floating)
                 or else (C.State = Floating and then not All_Floating)
               then
                  Float_Child (C, All_Floating);
                  exit;
               end if;

               List := Next (List);
            end loop;

            exit when List = Null_List;
         end loop;

         Set_Sensitive (MDI.Float_Menu_Item, not All_Floating);

         Set_Child_Visible (MDI, not All_Floating);

         --  Force a recomputation of the size
         Resize (Gtk_Window (Get_Toplevel (MDI)), -1, -1);
      end if;
   end Set_All_Floating_Mode;

   ---------------------------------
   -- Use_Short_Titles_For_Floats --
   ---------------------------------

   procedure Use_Short_Titles_For_Floats
     (MDI : access MDI_Window_Record; Short_Titles : Boolean)
   is
      List  : Widget_List.Glist := First (MDI.Items);
      Child : MDI_Child;
   begin
      if MDI.Use_Short_Titles_For_Floats = Short_Titles then
         --  Nothing to be changed
         return;
      end if;

      MDI.Use_Short_Titles_For_Floats := Short_Titles;

      --  The property has been changed. We need to walk though all children
      --  and enforce the title to the short one for floating children.

      loop
         List := First (MDI.Items);

         while List /= Null_List loop
            Child := MDI_Child (Get_Data (List));
            if Child.State = Floating then
               if Short_Titles then
                  Set_Title
                    (Gtk_Window (Get_Toplevel (Child.Initial)),
                     Locale_From_UTF8 (Child.Short_Title.all));
               else
                  Set_Title
                    (Gtk_Window (Get_Toplevel (Child.Initial)),
                     Locale_From_UTF8 (Child.Title.all));
               end if;
            end if;

            List := Next (List);
         end loop;

         exit when List = Null_List;
      end loop;
   end Use_Short_Titles_For_Floats;

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

   -----------------------
   -- Has_Default_Child --
   -----------------------

   function Has_Default_Child
     (MDI         : access MDI_Window_Record'Class;
      Ignore      : MDI_Child := null;
      Ignore_Note : Gtk_Notebook := null) return Boolean
   is
      Child_Is_Being_Destroyed : constant Boolean :=
                                   Ignore = null
                                   or else Ignore.MDI.In_Drag = No_Drag;
      L                        : Widget_List.Glist := MDI.Items;
      C                        : MDI_Child;
   begin
      while L /= Null_List loop
         C := MDI_Child (Get_Data (L));
         if (Ignore /= C or else not Child_Is_Being_Destroyed)
           and then C.State = Normal   --  In a notebook currently
           and then C.Group = Group_Default
           and then (Ignore_Note = null
                     or else Get_Notebook (C) /= Ignore_Note)
         then
            return True;
         end if;
         L := Next (L);
      end loop;
      return False;
   end Has_Default_Child;

   ---------------------------
   -- Removed_From_Notebook --
   ---------------------------

   procedure Removed_From_Notebook
     (Note : access Gtk_Widget_Record'Class; Args  : Gtk_Args)
   is
      C                     : constant  Gtk_Widget :=
                                Gtk_Widget (To_Object (Args, 1));
      Page1                 : Gtk_Widget;
      Child                 : MDI_Child;
      Default_Child_Remains : Boolean := False;
      Must_Destroy          : Boolean := False;
   begin
      if C.all not in MDI_Child_Record'Class then
         return;
      end if;

      Child := MDI_Child (C);
      Child.Tab_Label := null;

      Child.State := Normal;

      if not Gtk.Object.In_Destruction_Is_Set (Note) then
         Configure_Notebook_Tabs
           (Child.MDI, Gtk_Notebook (Note), Hide_If_Empty => True);

         Page1 := Get_Nth_Page (Gtk_Notebook (Note), 0);

         --  Do we have any child remaining with Group_Default ?

         if Child.Group = Group_Default then
            --  The current child should be taken into account only when it is
            --  moved to another notebook, ie will remain as part of the MDI.
            --  If it is being destroyed, it should no longer count as a
            --  Position_Default child.
            Default_Child_Remains := Has_Default_Child
              (Child.MDI, Ignore => Child);
         end if;

         if Traces then
            Put_Line ("Removed_From_Notebook: " & Get_Title (Child)
                      & " default child remains ? "
                      & Boolean'Image (Default_Child_Remains));
         end if;

         --  No more pages in the notebook ?
         if Page1 = null then
            --  Are there any other notebook with only children in normal
            --  position ? (We need to ignore the notebooks with at least one
            --  child in the bottom, left,... position, since these are special
            --  notebooks
            --  Destroy the current notebook if:
            --    - There is no other empty notebook
            --    - Or there is at least one notebook with Position_Default
            --      children

            if Child.Group /= Group_Default then
               if not MDI_Notebook (Note).Is_Default_Notebook then
                  Destroy (Note);
               end if;
               return;
            end if;

            --  The notebook will be destroyed if we already have an
            --  empty notebook, or if there is at least one Position_Default
            --  child remaining. Otherwise we keep this notebook as empty,
            --  and mark it as special.

            Must_Destroy := Default_Child_Remains;
            if not Must_Destroy then
               declare
                  Children : Widget_List.Glist :=
                    Get_Children (Gtk_Container (Child.MDI));
                  Has_Empty_Notebook : Boolean := False;
                  L : Widget_List.Glist;
                  N : Gtk_Notebook;
               begin
                  --  Identify which notebooks are empty
                  L := Children;
                  while L /= Null_List loop
                     N := Gtk_Notebook (Get_Data (L));
                     if N /= Gtk_Notebook (Note)
                       and then Get_Nth_Page (N, 0) = null
                     then
                        Has_Empty_Notebook := True;
                        exit;
                     end if;
                     L := Next (L);
                  end loop;
                  Free (Children);

                  Must_Destroy := Has_Empty_Notebook;
               end;
            end if;

            if Must_Destroy then
               Destroy (Note);
            else
               MDI_Notebook (Note).Is_Default_Notebook := True;
               Show (Note);  --  Default notebook always visible
            end if;

         else
            --  If we have only one page:
            if Child.Group = Group_Default
              and then not Default_Child_Remains
            then
               MDI_Notebook (Note).Is_Default_Notebook := True;
            end if;
         end if;
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
   end Removed_From_Notebook;

   -----------
   -- Split --
   -----------

   procedure Split
     (MDI               : access MDI_Window_Record;
      Orientation       : Gtk_Orientation;
      Reuse_If_Possible : Boolean := False;
      After             : Boolean := False;
      Width, Height     : Glib.Gint := 0)
   is
      Note, Note2 : Gtk_Notebook;
      Child       : MDI_Child;
   begin
      Note := Find_Current_In_Central (MDI);

      --  Only split if there are at least two children
      if Note /= null and then Get_Nth_Page (Note, 1) /= null then

         Child := MDI_Child (Get_Nth_Page (Note, Get_Current_Page (Note)));
         Ref (Child);

         Note2 := Gtk_Notebook (Splitted_Area
           (MDI, Note, Orientation, After));

         if not Reuse_If_Possible or else Note2 = null then
            Note2 := Create_Notebook (MDI);
            Show_All (Note2);
            Split (MDI,
                   Ref_Widget  => Note,
                   New_Child   => Note2,
                   Width       => Width,
                   Height      => Height,
                   Orientation => Orientation,
                   After       => After);
         end if;

         Give_Focus_To_Previous_Child (Child);
         Remove (Note, Child);
         Put_In_Notebook (MDI, Child, Note2);
         Unref (Child);
         Set_Focus_Child (Child);

         Show (Note2);

         Emit_By_Name
           (Get_Object (MDI), "children_reorganized" & ASCII.NUL);
      end if;
   end Split;

   ----------------
   -- Split_H_Cb --
   ----------------

   procedure Split_H_Cb (MDI : access Gtk_Widget_Record'Class) is
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

   procedure Split_V_Cb (MDI : access Gtk_Widget_Record'Class) is
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
         Raise_Child (C, False);
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
   -- Focus_Cb --
   --------------

   procedure Focus_Cb (Child : access Gtk_Widget_Record'Class) is
      C : constant MDI_Child := MDI_Child (Child);
   begin
      if Get_Active (C.Menu_Item) then
         --  If C is floating, raise the window.
         if C.State = Floating then
            Raise_Child (C, False);
         end if;

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
      use Widget_SList;

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
           (Child.Menu_Item, Gtk.Menu_Item.Signal_Activate,
            Widget_Callback.To_Marshaller (Focus_Cb'Access), Child,
            After => True);
         Widget_Callback.Object_Connect
           (Child.Menu_Item, Signal_Destroy,
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
      MDI_Window (MDI).Float_Menu_Item := null;
   end Menu_Destroyed;

   -----------------
   -- Create_Menu --
   -----------------

   function Create_Menu
     (MDI               : access MDI_Window_Record;
      Accel_Path_Prefix : String := "<gtkada>") return Gtk.Menu.Gtk_Menu
   is
      Item  : Gtk_Menu_Item;
      Child : MDI_Child;
      Tmp   : Widget_List.Glist;

   begin
      if MDI.Menu = null then
         MDI.Accel_Path_Prefix := new String'(Accel_Path_Prefix);
         Gtk_New (MDI.Menu);

         Gtk_New (Item, "Split Side-by-Side");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Widget_Callback.To_Marshaller (Split_H_Cb'Access), MDI);
         Set_Accel_Path (Item, Accel_Path_Prefix
           & "/window/split_horizontal", MDI.Group);

         Gtk_New (Item, "Split Up-Down");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Widget_Callback.To_Marshaller (Split_V_Cb'Access), MDI);
         Set_Accel_Path (Item, Accel_Path_Prefix
           & "/window/split_vertical", MDI.Group);

         Gtk_New (Item);
         Append (MDI.Menu, Item);

         Gtk_New (MDI.Float_Menu_Item, "Floating");
         Append (MDI.Menu, MDI.Float_Menu_Item);
         Set_Active (MDI.Float_Menu_Item,
                     MDI.Focus_Child /= null
                     and then MDI.Focus_Child.State = Floating);
         MDI.Float_Menu_Item_Id := Widget_Callback.Object_Connect
           (MDI.Float_Menu_Item, Signal_Toggled,
            Widget_Callback.To_Marshaller (Float_Cb'Access), MDI);
         Set_Accel_Path
           (MDI.Float_Menu_Item, Accel_Path_Prefix
           & "/window/floating", MDI.Group);

         Gtk_New (Item);
         Append (MDI.Menu, Item);

         Gtk_New (MDI.Close_Menu_Item, "Close");
         Append (MDI.Menu, MDI.Close_Menu_Item);
         Widget_Callback.Object_Connect
           (MDI.Close_Menu_Item, Gtk.Menu_Item.Signal_Activate,
            Widget_Callback.To_Marshaller (Close_Cb'Access), MDI);
         Set_Accel_Path (Item, Accel_Path_Prefix
           & "/window/close", MDI.Group);

         Gtk_New (Item);
         Append (MDI.Menu, Item);

         Tmp := First (MDI.Items);

         while Tmp /= Null_List loop
            Child := MDI_Child (Get_Data (Tmp));
            Create_Menu_Entry (Child);
            Tmp := Next (Tmp);
         end loop;

         Widget_Callback.Object_Connect
           (MDI.Menu, Signal_Destroy,
            Widget_Callback.To_Marshaller (Menu_Destroyed'Access), MDI);

      elsif Accel_Path_Prefix /= MDI.Accel_Path_Prefix.all then
         Put_Line
           ("Accel_Path_Prefix must have the same prefix across calls"
            & " to Create_Menu");
      end if;

      Show_All (MDI.Menu);
      return MDI.Menu;
   end Create_Menu;

   ---------------------
   -- Set_Focus_Child --
   ---------------------

   procedure Set_Focus_Child
     (MDI        : access MDI_Window_Record;
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

   -------------
   -- Desktop --
   -------------

   package body Desktop is

      Empty_Notebook_Filler : MDI_Child;
      --  Used to fill the empty notebook, and prevent it from being destroyed
      --  during a desktop load.

      procedure Parse_Child_Node
        (MDI         : access MDI_Window_Record'Class;
         Child_Node  : Node_Ptr;
         User        : User_Data;
         Focus_Child : in out MDI_Child;
         X           : out Gint;
         Y           : out Gint;
         Raised      : out Boolean;
         State       : out State_Type;
         Child       : out MDI_Child;
         To_Hide     : in out Gtk.Widget.Widget_List.Glist);
      --  Parse a <child> node and return the corresponding Child. The latter
      --  has not been inserted in the MDI

      procedure Parse_Notebook_Node
        (MDI                   : access MDI_Window_Record'Class;
         Child_Node            : Node_Ptr;
         User                  : User_Data;
         Focus_Child           : in out MDI_Child;
         Width, Height         : out Gint;
         Notebook              : out Gtk_Notebook;
         To_Raise              : in out Gtk.Widget.Widget_List.Glist;
         To_Hide               : in out Gtk.Widget.Widget_List.Glist;
         Reuse_Empty_If_Needed : in out Boolean);
      --  Parse a <notebook> node.
      --  A new notebook is created and returned.
      --  If Reuse_Empty_If_Needed and we need to insert an empty notebook,
      --  we'll try and reuse an existing empty notebook. In this case, the
      --  variable is set to False.
      --  To_Raise is the children that are visible in the notebooks. It cannot
      --  be changed within this procedure, since when other items are loaded
      --  into the desktop, they might be put in the same notebook temporarily,
      --  before being moved to their actual location, and that would change
      --  the current page.

      procedure Parse_Pane_Node
        (MDI                   : access MDI_Window_Record'Class;
         Node                  : Node_Ptr;
         Focus_Child           : in out MDI_Child;
         User                  : User_Data;
         Initial_Ref_Child     : Gtk_Notebook := null;
         To_Raise              : in out Gtk.Widget.Widget_List.Glist;
         To_Hide               : in out Gtk.Widget.Widget_List.Glist;
         Reuse_Empty_If_Needed : in out Boolean);
      --  Parse a <Pane> node
      --  First_Child is the first notebook insert in pane (possibly inserted

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
        (MDI                   : access MDI_Window_Record'Class;
         Child_Node            : Node_Ptr;
         User                  : User_Data;
         Focus_Child           : in out MDI_Child;
         Width, Height         : out Gint;
         Notebook              : out Gtk_Notebook;
         To_Raise              : in out Gtk.Widget.Widget_List.Glist;
         To_Hide               : in out Gtk.Widget.Widget_List.Glist;
         Reuse_Empty_If_Needed : in out Boolean)
      is
         N            : Node_Ptr := Child_Node.Child;
         State        : State_Type;
         Raised       : Boolean;
         Raised_Child : MDI_Child;
         Child        : MDI_Child;
         X, Y         : Gint;
         Dummy        : Gtk_Label;
         Is_Default   : Boolean;
         Pos          : Gtk_Position_Type;
      begin
         Width  := Gint'Value (Get_Attribute (Child_Node, "Width", "-1"));
         Height := Gint'Value (Get_Attribute (Child_Node, "Height", "-1"));
         Pos    := Gtk_Position_Type'Value
           (Get_Attribute (Child_Node, "Tabs",
            Gtk_Position_Type'Image (MDI.Tabs_Position)));
         Is_Default := Boolean'Value
           (Get_Attribute (Child_Node, "default", "false"));

         if Traces then
            Put_Line ("MDI Parse_Notebook_Node Width=" & Gint'Image (Width)
                      & " Height=" & Gint'Image (Height));
         end if;

         Notebook := null;

         if Child_Node.Child = null
           and then Reuse_Empty_If_Needed
         then
            Notebook := Find_Empty_Notebook (MDI);
            if Notebook /= null then
               Reuse_Empty_If_Needed := False;
               if Traces then
                  Put_Line ("MDI Using existing empty notebook "
                            & System.Address_Image (Notebook.all'Address));
               end if;
            end if;
         end if;

         if Notebook = null then
            Notebook := Create_Notebook (MDI);
            MDI_Notebook (Notebook).Is_Default_Notebook := Is_Default;
            if Traces then
               Put_Line ("MDI About to create new notebook "
                         & System.Address_Image (Notebook.all'Address));
            end if;
         end if;

         Set_Tab_Pos (Notebook, Pos);
         Set_Child_Visible (Notebook, True);
         Show_All (Notebook);

         while N /= null loop
            if N.Tag.all = "Child" then
               Parse_Child_Node
                 (MDI, N, User, Focus_Child, X, Y,
                  Raised, State, Child, To_Hide => To_Hide);
               if Raised then
                  Raised_Child := Child;
               end if;

               --  Child cannot be floating while in a notebook
               if Child /= null then
                  if Traces then
                     Put_Line
                       ("MDI: Parse_Notebook_Node, moving child into the"
                        & " the notebook");
                  end if;
                  Float_Child (Child, False);
                  Put_In_Notebook (MDI, Child, Notebook);
                  if Traces then
                     Put_Line ("MDI: Parse_Notebook_Node, done moving child");
                  end if;
               end if;

            else
               --  Invalid node
               null;
            end if;

            N := N.Next;
         end loop;

         if Traces then
            Put_Line ("MDI Parse_Notebook_Node: done adding all children");
         end if;

         --  Create a dummy node if necessary, since otherwise the calls to
         --  Split afterward will simply discard that notebook. This dummy
         --  widget is destroyed at the end of restoring the desktop

         if Child_Node.Child = null and then Empty_Notebook_Filler = null then
            Gtk_New (Dummy, "");
            Gtk_New (Empty_Notebook_Filler, Dummy);
            Set_Title (Empty_Notebook_Filler, "<Dummy, notebook filler>");
            Put (MDI, Empty_Notebook_Filler);
            Put_In_Notebook (MDI, Empty_Notebook_Filler, Notebook);
            MDI_Notebook (Notebook).Is_Default_Notebook := True;
         end if;

         if Raised_Child /= null then
            Prepend (To_Raise, Gtk_Widget (Raised_Child));

            --  Make sure the child appears first in the list for this
            --  notebook. That way, if the current focus child is closed by the
            --  user, we know the focus won't fallback to a child currently not
            --  visible in the notebook, which would result in a raise.
            Ref (Raised_Child);
            Remove (MDI.Items, Gtk_Widget (Raised_Child));
            Prepend (MDI.Items, Gtk_Widget (Raised_Child));
            Unref (Raised_Child);
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
         X           : out Gint;
         Y           : out Gint;
         Raised      : out Boolean;
         State       : out State_Type;
         Child       : out MDI_Child;
         To_Hide     : in out Gtk.Widget.Widget_List.Glist)
      is
         N        : Node_Ptr;
         Register : Register_Node;
         W, H     : Allocation_Int := -1;
         Visible  : constant Boolean := Boolean'Value
           (Get_Attribute (Child_Node, "visible", "true"));
      begin
         Register := Registers;
         Child    := null;
         Raised   := False;
         State    := Normal;
         X        := 0;
         Y        := 0;

         if Traces then
            Put_Line ("MDI About to insert child. Will be moved elsewhere");
         end if;

         while Child = null and then Register /= null loop
            Child := Register.Load
              (MDI_Window (MDI), Child_Node.Child, User);
            Register := Register.Next;
         end loop;

         if Child = null then
            return;
         end if;

         Child.Group := Child_Group'Value
           (Get_Attribute (Child_Node, "Group",
                           Child_Group'Image (Child.Group)));
         State := State_Type'Value
           (Get_Attribute (Child_Node, "State", "NORMAL"));
         Raised := Boolean'Value
           (Get_Attribute (Child_Node, "Raised", "False"));
         if Boolean'Value (Get_Attribute (Child_Node, "Focus", "False")) then
            Focus_Child := Child;
         end if;

         N := Child_Node.Child.Next;

         while N /= null loop
            --  We ignore the <title> and <short_title> fields. After all,
            --  the callback that created the child has or should have set
            --  a proper title already, and there is no reason to override
            --  this.

            if N.Tag.all = "X" then
               X := Gint'Value (N.Value.all);

            elsif N.Tag.all = "Y" then
               Y := Gint'Value (N.Value.all);

            elsif N.Tag.all = "Width" then
               W := Gint'Value (N.Value.all);

            elsif N.Tag.all = "Height" then
               H := Gint'Value (N.Value.all);

            else
               --  ??? Unknown node, just ignore for now
               null;
            end if;

            N := N.Next;
         end loop;

         if W /= -1 or else H /= -1 then
            Set_Size_Request (Child, W, H);
         end if;

         if not Visible then
            Prepend (To_Hide, Gtk_Widget (Child));
         end if;

         if Traces then
            Put_Line ("MDI: Parse_Child_Node: done");
         end if;
      end Parse_Child_Node;

      ---------------------
      -- Parse_Pane_Node --
      ---------------------

      procedure Parse_Pane_Node
        (MDI                   : access MDI_Window_Record'Class;
         Node                  : Node_Ptr;
         Focus_Child           : in out MDI_Child;
         User                  : User_Data;
         Initial_Ref_Child     : Gtk_Notebook := null;
         To_Raise              : in out Gtk.Widget.Widget_List.Glist;
         To_Hide               : in out Gtk.Widget.Widget_List.Glist;
         Reuse_Empty_If_Needed : in out Boolean)
      is
         Orientation : constant Gtk_Orientation := Gtk_Orientation'Value
           (Get_Attribute (Node, "Orientation"));
         N             : Node_Ptr;
         Width, Height : Gint;
         Ref_Item      : Gtk_Notebook := Initial_Ref_Child;
         Count         : Natural := 0;
         Notebook_Node : Node_Ptr;

      begin
         if Traces then
            Put_Line
              ("MDI Parsing pane node " & Gtk_Orientation'Image (Orientation));
         end if;

         N := Node.Child;
         while N /= null loop
            Count := Count + 1;
            N := N.Next;
         end loop;

         declare
            Notebooks : array (1 .. Count) of Gtk_Notebook;
         begin
            --  First insert all direct children of the pane, splitting as
            --  needed. Only then process the Pane children. Otherwise, the
            --  children of Pane will have been split and reorganized so that
            --  we won't be able to get a reference item for further splitting.

            Count := Notebooks'First;
            N := Node.Child;

            while N /= null loop
               --  Find the first notebook node of N
               Notebook_Node := N;
               while Notebook_Node.Tag /= null
                 and then Notebook_Node.Tag.all /= "Notebook"
               loop
                  Notebook_Node := Notebook_Node.Child;
               end loop;

               if Count = Notebooks'First
                 and then Initial_Ref_Child /= null
               then
                  Notebooks (Count) := Initial_Ref_Child;
               else
                  Parse_Notebook_Node
                    (MDI         => MDI,
                     Child_Node  => Notebook_Node,
                     User        => User,
                     Focus_Child => Focus_Child,
                     Width       => Width,
                     Height      => Height,
                     Notebook    => Notebooks (Count),
                     To_Raise    => To_Raise,
                     To_Hide     => To_Hide,
                     Reuse_Empty_If_Needed => Reuse_Empty_If_Needed);
                  if Traces then
                     Put_Line
                       ("MDI: Parse_Pane_Node: done parsing notebook node");
                  end if;

                  if Get_Parent (Notebooks (Count)) = null then
                     if Ref_Item = null then
                        if Traces then
                           Put_Line
                             ("MDI: Parse_Pane_Node, add notebook in MDI "
                              & System.Address_Image
                                (Notebooks (Count).all'Address));
                        end if;
                        Add_Child (Win         => MDI,
                                   New_Child   => Notebooks (Count),
                                   Orientation => Orientation,
                                   Width       => Width,
                                   Height      => Height);
                     else
                        if Traces then
                           Put_Line
                             ("MDI: Parse_Pane_Node Split notebook into MDI "
                              & System.Address_Image
                                (Notebooks (Count).all'Address)
                              & " ref="
                              & System.Address_Image (Ref_Item.all'Address)
                              & " Orient="
                              & Gtk_Orientation'Image (Orientation));
                        end if;
                        Split (MDI,
                               Ref_Widget  => Ref_Item,
                               New_Child   => Notebooks (Count),
                               Width       => Width,
                               Height      => Height,
                               Orientation => Orientation);
                     end if;
                  else
                     if Traces then
                        Put_Line
                          ("MDI: Parse_Pane_Node: notebook already in MDI");
                        Set_Size (MDI,
                                  Notebooks (Count),
                                  Width => Width,
                                  Height => Height);
                     end if;
                  end if;
               end if;

               Ref_Item := Notebooks (Count);
               Count := Count + 1;
               N := N.Next;
            end loop;

            --  Now process the Pane children recursively, splitting as needed

            N := Node.Child;
            Count := Notebooks'First;
            while N /= null loop
               if N.Tag.all = "Pane" then
                  Parse_Pane_Node
                    (MDI                   => MDI,
                     Node                  => N,
                     Focus_Child           => Focus_Child,
                     User                  => User,
                     Initial_Ref_Child     => Notebooks (Count),
                     To_Raise              => To_Raise,
                     To_Hide               => To_Hide,
                     Reuse_Empty_If_Needed => Reuse_Empty_If_Needed);
               end if;
               Count := Count + 1;
               N := N.Next;
            end loop;
         end;
      end Parse_Pane_Node;

      ---------------------
      -- Restore_Desktop --
      ---------------------

      function Restore_Desktop
        (MDI       : access MDI_Window_Record'Class;
         From_Tree : Glib.Xml_Int.Node_Ptr;
         User      : User_Data) return Boolean
      is
         Child, Focus_Child : MDI_Child;
         Child_Node         : Node_Ptr;
         State              : State_Type;
         Raised             : Boolean;
         X, Y               : Gint := 0;
         Items_Removed      : Boolean := False;
         To_Raise           : Gtk.Widget.Widget_List.Glist;
         To_Hide            : Gtk.Widget.Widget_List.Glist;

         procedure Remove_All_Items (Remove_All_Empty : Boolean);
         --  Remove all the items currently in the MDI.
         --  If Remove_All_Empty is False, then a single empty notebook is kept
         --  if there is one.
         --  Does nothing if called multiple times

         ----------------------
         -- Remove_All_Items --
         ----------------------

         procedure Remove_All_Items (Remove_All_Empty : Boolean) is
            Children    : Widget_List.Glist;
            L, L2       : Widget_List.Glist;
            Found_Empty : Boolean := Remove_All_Empty;
            Note        : Gtk_Notebook;
         begin
            if Traces then
               Put_Line ("MDI Remove_All_Items: remove_empty="
                         & Boolean'Image (Remove_All_Empty));
            end if;
            if not Items_Removed then
               --  First loop is to remove all children. We give them a chance
               --  to react to the delete_event, in case they do some cleanup
               --  at that point
               L := MDI.Items;
               while L /= Null_List loop
                  L2 := Next (L);
                  Close_Child (MDI_Child (Get_Data (L)));
                  L := L2;
               end loop;

               --  Children that haven't been deleted at this point are those
               --  that refused the delete_event. We thus keep them, since they
               --  might need special handling later on. At worse, we break the
               --  desktop.

               --  We now force the closing of all empty notebooks
               Children := Get_Children (MDI);
               L := Children;
               while L /= Null_List loop
                  Note := Gtk_Notebook (Get_Data (L));
                  if Get_Nth_Page (Note, 0) = null then
                     if Found_Empty then
                        Remove (MDI, Note);
                     else
                        Found_Empty := True;
                     end if;
                  end if;
                  L := Next (L);
               end loop;
               Free (Children);
               Items_Removed := True;
            end if;

            if Traces then
               Put_Line ("MDI Remove_All_Items: done");
               New_Line;
               New_Line;
               New_Line;
            end if;
         end Remove_All_Items;

         Reuse_Empty_If_Needed : Boolean := True;
         Initial_All_Floating_Mode : constant Boolean := MDI.All_Floating_Mode;
         Do_Size_Allocate : Boolean := True;
      begin
         if From_Tree = null then
            return False;
         end if;

         --  Temporarily disable the user of all floating mode, so that we can
         --  properly restore the desktop even if notebooks are referenced.
         MDI.All_Floating_Mode := False;
         Empty_Notebook_Filler := null;

         Child_Node := From_Tree.Child;
         pragma Assert (From_Tree.Tag.all = "MDI");

         if Traces then
            Put_Line ("MDI Restore_Desktop");
            Put_Line ("Current MDI size is"
                      & Gint'Image (Get_Allocation_Width (MDI))
                      & "x" & Gint'Image (Get_Allocation_Height (MDI)));
            Put_Line
              ("Current window size is"
               & Gint'Image (Get_Allocation_Width (Get_Toplevel (MDI)))
               & "x"
               & Gint'Image (Get_Allocation_Height (Get_Toplevel (MDI))));
         end if;

         MDI.Loading_Desktop := True;

         Freeze (MDI);

         --  We must restore the size of the main window first, so that the
         --  rest of the desktop makes sense

         declare
            Width, Height : Gint;
            State         : Gdk_Window_State;
         begin
            State  := Gdk_Window_State'Value
              (Get_Attribute (From_Tree, "state", "0"));

            if (State and Window_State_Maximized) /= 0 then
               --  Issue: this will not be done immediately, since the
               --  window might not be mapped when loading the initial desktop.
               --  As a result, the first call to Size_Allocate below will
               --  use whatever current size the window has, and thus might
               --  break the desktop. See the call to Realize below
               Maximize (Gtk_Window (Get_Toplevel (MDI)));
               Do_Size_Allocate := False;
            else
               Width  :=
                 Gint'Value (Get_Attribute (From_Tree, "width",  "640"));
               Height :=
                 Gint'Value (Get_Attribute (From_Tree, "height", "480"));

               Set_Default_Size
                 (Gtk_Window (Get_Toplevel (MDI)), Width, Height);
            end if;
         exception
            when others =>
               --  An invalid attribute in XML ?
               null;
         end;

         --  Now restore the rest of the desktop

         Child_Node := From_Tree.Child;

         while Child_Node /= null loop
            if Child_Node.Tag.all = "Pane" then
               Remove_All_Items (Remove_All_Empty => True);
               Parse_Pane_Node
                 (MDI, Child_Node, Focus_Child, User, null,
                  To_Raise              => To_Raise,
                  To_Hide               => To_Hide,
                  Reuse_Empty_If_Needed => Reuse_Empty_If_Needed);

            elsif Child_Node.Tag.all = "Bottom_Dock_Height" then
               --  An old desktop ? Do not load it at all, and use the default
               --  desktop instead, so that at least we give the user something
               --  looking correct
               return False;

            elsif Child_Node.Tag.all = "Child" then
               --  Used for floating children, and children in the default
               --  desktop (see Add_To_Tree)

               --  ??? Why would we want this, in case we already added some
               --  widgets to the tree
               --  Remove_All_Items (Remove_All_Empty => False);

               Parse_Child_Node
                 (MDI, Child_Node, User,
                  Focus_Child, X, Y, Raised, State, Child,
                  To_Hide => To_Hide);

               if Child /= null then
                  case State is
                     when Floating =>
                        Internal_Float_Child
                          (Child, True, Position_At_Mouse => False,
                           X => X, Y => Y);

                     when Normal =>
                        Float_Child (Child, False);
                  end case;
               end if;
            end if;

            Child_Node := Child_Node.Next;
         end loop;

         MDI.Desktop_Was_Loaded := True;

         Queue_Resize (MDI);

         if Empty_Notebook_Filler /= null then
            --  The empty notebook has been created during the desktop load
            declare
               Note : constant Gtk_Notebook :=
                        Gtk_Notebook (Get_Parent (Empty_Notebook_Filler));
            begin
               if Traces then
                  Put_Line
                    ("MDI: Restore desktop, removing empty_notebook_filler");
               end if;

               Remove_Page (Note, 0);
            end;
         end if;

         Set_All_Floating_Mode (MDI, Initial_All_Floating_Mode);

         --  Raise all appropriate items at the end, so that even if some items
         --  are added temporarily to notebooks, then have no long-lasting
         --  impact on the notebook itself.
         declare
            Item : Widget_List.Glist := To_Raise;
         begin
            while Item /= Widget_List.Null_List loop
               Child := MDI_Child (Widget_List.Get_Data (Item));
               if Traces then
                  Put_Line
                    ("MDI: Restore desktop, raising child with no focus "
                     & Get_Title (Child));
               end if;
               Raise_Child (Child, Give_Focus => False);
               Item := Widget_List.Next (Item);
            end loop;
            Free (To_Raise);

            Item := To_Hide;
            while Item /= Widget_List.Null_List loop
               Child := MDI_Child (Widget_List.Get_Data (Item));
               Hide (Child);
               Item := Widget_List.Next (Item);
            end loop;
            Free (To_Hide);
         end;

         --  Realize the window while frozen, so that windows that insist on
         --  setting their own size when realized (eg. the search window in
         --  GPS) will not break the desktop.
         --  However, don't do this when attempting to maximize the desktop,
         --  since otherwise we get a first Size_Allocate for whatever current
         --  size we have, and then a second one for the maximized size. The
         --  first one breaks the desktop partially.
         if Do_Size_Allocate then
            Realize (MDI);
         end if;

         MDI.Loading_Desktop := False;
         Thaw (MDI);

         if Do_Size_Allocate then
            if Traces then
               Put_Line ("MDI: Restore_Desktop, forcing a Size_Allocate");
            end if;

            Size_Allocate
              (MDI,
               Allocation => (X      => Get_Allocation_X (MDI),
                              Y      => Get_Allocation_Y (MDI),
                              Width  => Get_Allocation_Width (MDI),
                              Height => Get_Allocation_Height (MDI)));
         end if;

         Emit_By_Name (Get_Object (MDI), "children_reorganized" & ASCII.NUL);

         if Focus_Child /= null then
            if Traces then
               Put_Line
                 ("MDI: Desktop set focus on " & Get_Title (Focus_Child));
            end if;
            Set_Focus_Child (Focus_Child);
         end if;

         return True;
      end Restore_Desktop;

      ------------------
      -- Save_Desktop --
      ------------------

      function Save_Desktop
        (MDI  : access MDI_Window_Record'Class;
         User : User_Data) return Glib.Xml_Int.Node_Ptr
      is
         Item             : Widget_List.Glist;
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
           (Current : Node_Ptr; Note : Gtk_Notebook);
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
               Widget_Node := Register.Save (Child.Initial, User);
               Register := Register.Next;
            end loop;

            if Widget_Node /= null then
               --  Note: We need to insert the children in the opposite order
               --  from Restore_Desktop, since the children are added at the
               --  beginning of the list.

               Child_Node := new Node;
               Child_Node.Tag := new String'("Child");
               Add_Child (Child_Node, Widget_Node, Append => True);

               Set_Attribute (Child_Node, "State",
                              State_Type'Image (Child.State));
               Set_Attribute (Child_Node, "Group",
                              Child_Group'Image (Child.Group));

               if Child.State = Floating then
                  declare
                     Win : constant Gtk_Widget :=
                       Get_Toplevel (Child.Initial);
                     W, H : Gint;
                  begin
                     --  Note: This size doesn't include the size of the window
                     --  decorations, doesn't seem to be a way to do this.
                     W := Get_Allocation_Width (Win);
                     H := Get_Allocation_Height (Win);
                     Add (Child_Node, "Height", Gint'Image (H));
                     Add (Child_Node, "Width", Gint'Image (W));
                  end;
               end if;

               if Child = MDI.Focus_Child then
                  Set_Attribute (Child_Node, "Focus", "True");
               end if;

               if Raised then
                  Set_Attribute (Child_Node, "Raised", "True");
               end if;

               if not Visible_Is_Set (Child) then
                  Set_Attribute (Child_Node, "visible", "False");
               end if;

               Add_Child (Parent, Child_Node, Append => True);
            end if;
         end Save_Widget;

         -------------------
         -- Save_Notebook --
         -------------------

         procedure Save_Notebook (Current : Node_Ptr; Note : Gtk_Notebook) is
            Length                  : constant Gint := Get_N_Pages (Note);
            Current_Page            : constant Gint := Get_Current_Page (Note);
            Parent                  : Node_Ptr;
            Has_Default_Group_Child : Boolean := False;
            Child                   : MDI_Child;
            Is_Default_Notebook     : Boolean := False;

            Border_Width            : constant Allocation_Int := 0;
            --  +4 is to take into account the border of the notebook

         begin
            Parent := new Node;
            Parent.Tag := new String'("Notebook");
            Set_Attribute
              (Parent, "Width",
               Allocation_Int'Image
                 (Get_Allocation_Width (Note) + Border_Width));
            Set_Attribute
              (Parent, "Height",
               Allocation_Int'Image
                 (Get_Allocation_Height (Note) + Border_Width));
            Set_Attribute
              (Parent, "Tabs",
               Gtk_Position_Type'Image (Get_Tab_Pos (Note)));

            if Length > 0 then
               for Page_Index in 0 .. Length - 1 loop
                  Child := MDI_Child
                    (Get_Nth_Page (Note, Page_Index));
                  Has_Default_Group_Child := Has_Default_Group_Child
                    or else Child.Group = Group_Default;
                  Save_Widget
                    (Parent,
                     Child,
                     Raised => Current_Page = Page_Index);
               end loop;
            end if;

            --  Do not append the Notebook node to the parent if no child in
            --  the notebook was found, unless the number of pages is 0, in
            --  which case this is a real empty space which should be saved
            --  in the desktop. Also add the default notebook always, since
            --  it plays a special role

            Is_Default_Notebook := MDI_Notebook (Note).Is_Default_Notebook
              or else (Has_Default_Group_Child
                       and not Has_Default_Child (MDI, Ignore_Note => Note));
            if Is_Default_Notebook then
               Set_Attribute (Parent, "default", "true");
            end if;

            if Traces then
               Put_Line ("Saving notebook, Length="
                         & Gint'Image (Length)
                         & " Is_Default="
                         & Boolean'Image (Is_Default_Notebook)
                         & " parent.child is null="
                         & Boolean'Image (Parent.Child = null));
            end if;

            if Length = 0
              or else Parent.Child /= null
              or else Is_Default_Notebook
            then
               Add_Child (Current, Parent, Append => True);
            else
               Free (Parent);
            end if;
         end Save_Notebook;

      begin
         Root := new Node;
         Root.Tag := new String'("MDI");

         --  Save the general configuration of the MDI

         declare
            Win   : constant Gtk_Window := Gtk_Window (Get_Toplevel (MDI));
            State : Gdk_Window_State;
            X, Y  : Gint;
         begin
            if Win /= null then
               State := Get_State (Get_Window (Win));
               if (State and Window_State_Maximized) = 0 then
                  Set_Attribute
                    (Root, "width",
                     Allocation_Int'Image (Get_Allocation_Width (Win)));
                  Set_Attribute
                    (Root, "height",
                     Allocation_Int'Image (Get_Allocation_Height (Win)));

                  Get_Root_Origin (Get_Window (Win), X, Y);

                  Set_Attribute (Root, "x", Gint'Image (X));
                  Set_Attribute (Root, "y", Gint'Image (Y));
               end if;

               Set_Attribute (Root, "state", Gdk_Window_State'Image (State));
            end if;
         end;

         --  Look through all the notebooks, and save the widgets in the
         --  notebook order.

         declare
            Current, N : Node_Ptr;
            Depth      : Natural := 0;
         begin
            Current := Root;
            Iter := Start (MDI);
            while not At_End (Iter) loop
               for N in Get_Depth (Iter) + 1 .. Depth loop
                  Current := Current.Parent;
               end loop;

               if Get_Widget (Iter) /= null then
                  Save_Notebook (Current, Gtk_Notebook (Get_Widget (Iter)));
               else
                  N := new Node;
                  N.Tag := new String'("Pane");
                  Set_Attribute
                    (N, "Orientation",
                     Gtk_Orientation'Image (Get_Orientation (Iter)));
                  Add_Child (Current, N, Append => True);
                  Current := N;
               end if;

               Depth := Get_Depth (Iter);
               Next (Iter);
            end loop;
         end;

         --  A pass to eliminate all empty

         declare
            procedure Prune_Empty (N : in out Node_Ptr);
            --  Prunes empty panes below N

            -----------------
            -- Prune_Empty --
            -----------------

            procedure Prune_Empty (N : in out Node_Ptr) is
               C : Node_Ptr;
            begin
               if N.Tag.all = "Pane" then
                  C := N.Child;

                  while C /= null loop
                     Prune_Empty (C);

                     if C /= null then
                        C := C.Next;
                     end if;
                  end loop;

                  if N.Child = null then
                     Free (N);
                  end if;
               end if;
            end Prune_Empty;

         begin
            Prune_Empty (Root.Child);
         end;

         --  Save the floating widgets

         Item := MDI.Items;
         while Item /= Widget_List.Null_List loop
            Child := MDI_Child (Widget_List.Get_Data (Item));

            case Child.State is
               when Normal   => null;
               when Floating => Save_Widget (Root, Child, False);
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
     (MDI               : access MDI_Window_Record;
      Group_By_Notebook : Boolean := False) return Child_Iterator
   is
      Children : Widget_List.Glist;
   begin
      if Group_By_Notebook then
         Children := Get_Children (MDI);
         if Children /= Null_List then
            declare
               Iter : Child_Iterator :=
                 (Group_By_Notebook => True,
                  Notebook          => Gtk_Notebook (Get_Data (Children)),
                  Notebook_Page     => 0,
                  Floating_Iter     => MDI.Items,
                  MDI               => MDI_Window (MDI));
            begin
               while Iter.Floating_Iter /= Null_List
                 and then MDI_Child
                  (Widget_List.Get_Data (Iter.Floating_Iter)).State /= Floating
               loop
                  Iter.Floating_Iter := Widget_List.Next (Iter.Floating_Iter);
               end loop;

               Free (Children);
               return Iter;
            end;
         else
            return (Group_By_Notebook => True,
                    Notebook       => null,
                    Notebook_Page  => Gint'Last,
                    Floating_Iter  => Null_List,
                    MDI            => MDI_Window (MDI));
         end if;
      else
         return (Group_By_Notebook => False, Iter => MDI.Items);
      end if;
   end First_Child;

   ------------------
   -- Get_Notebook --
   ------------------

   function Get_Notebook
     (Iterator : Child_Iterator) return Gtk.Notebook.Gtk_Notebook is
   begin
      return Get_Notebook (Get (Iterator));
   end Get_Notebook;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Child_Iterator) is
      Children, Child : Widget_List.Glist;
   begin
      if Iterator.Group_By_Notebook then
         if Iterator.Notebook = null then
            --  Find the next floating child
            while Iterator.Iter /= Null_List
              and then MDI_Child (Widget_List.Get_Data (Iterator.Iter)).State
                 /= Floating
            loop
               Iterator.Iter := Widget_List.Next (Iterator.Iter);
            end loop;

         else
            Iterator.Notebook_Page := Iterator.Notebook_Page + 1;
            if Get_Nth_Page
              (Iterator.Notebook, Iterator.Notebook_Page) = null
            then
               Iterator.Notebook_Page := 0;
               Children := Get_Children (Iterator.MDI);
               Child := First (Children);

               while Child /= Null_List
                 and then Get_Data (Child) /= Gtk_Widget (Iterator.Notebook)
               loop
                  Child := Next (Child);
               end loop;

               if Child = Null_List or else Next (Child) = Null_List then
                  Iterator.Notebook := null;
                  --  We will start returning floating children
               else
                  Iterator.Notebook := Gtk_Notebook (Get_Data (Next (Child)));
               end if;

               Free (Children);
            end if;
         end if;

      else
         Iterator.Iter := Widget_List.Next (Iterator.Iter);
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iterator : Child_Iterator) return MDI_Child is
   begin
      if Iterator.Group_By_Notebook then
         if Iterator.Notebook = null then
            if Iterator.Floating_Iter = Widget_List.Null_List then
               return null;
            else
               return MDI_Child
                 (Widget_List.Get_Data (Iterator.Floating_Iter));
            end if;

         else
            return MDI_Child
              (Get_Nth_Page (Iterator.Notebook, Iterator.Notebook_Page));
         end if;

      elsif Iterator.Iter /= Widget_List.Null_List then
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
      Note  : constant Gtk_Notebook := Get_Notebook (Child);
      Style : Gtk_Style;
   begin
      if Highlight then
         Show (Child);  --  Make sure the child is visible

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
        (Widget, Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Button_Pressed_Forced'Access),
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

   procedure Child_Drag_Begin
     (Child : access MDI_Child_Record'Class;
      Event : Gdk_Event)
   is
      Tmp : Gdk_Grab_Status;
      Win : Gdk.Window.Gdk_Window;
      pragma Unreferenced (Tmp);
   begin
      --  Focus and raise the child. Raise_Child must be called explicitly
      --  since Set_Focus_Child won't do it if the child already has the focus.
      --  We have to raise the child, since otherwise the Pointer_Grab below
      --  will fail

      if Traces then
         Put_Line ("MDI: Child_Drag_Begin, focus and raise "
                   & Get_Title (Child));
      end if;

      Set_Focus_Child (Child);
      Raise_Child (Child, False);

      Win := Get_Window (Child);

      --  If Child is floating, Win may be null at this point. In this case,
      --  do nothing.

      if Win /= null then
         --  Grab the pointer, so that we can detect whether the mouse moved
         --  far enough from its initial position to start a drag. This also
         --  ensures we get all the button_motion events

         Tmp := Pointer_Grab
           (Win,
            False,
            Button_Press_Mask or Button_Motion_Mask or Button_Release_Mask,
            Cursor => null,
            Time   => 0);

         Child.MDI.Drag_Start_X := Gint (Get_X_Root (Event));
         Child.MDI.Drag_Start_Y := Gint (Get_Y_Root (Event));
         Child.MDI.In_Drag := In_Pre_Drag;
         Child.MDI.Dnd_Rectangle_Owner := null;
         Child.MDI.Dnd_Rectangle := (0, 0, 0, 0);
      elsif Traces then
         Put_Line ("MDI: Child is floating, did not initiate DnD");
      end if;
   end Child_Drag_Begin;

   -----------------------
   -- Cancel_Child_Drag --
   -----------------------

   procedure Cancel_Child_Drag (Child : access MDI_Child_Record'Class) is
   begin
      if Traces then
         Put_Line ("MDI: Cancel_Child_Drag");
      end if;

      Pointer_Ungrab;
      Child.MDI.In_Drag := No_Drag;
   end Cancel_Child_Drag;

   -------------------------
   -- Child_Drag_Finished --
   -------------------------

   procedure Child_Drag_Finished (Child  : access MDI_Child_Record) is
      pragma Unreferenced (Child);
   begin
      null;
   end Child_Drag_Finished;

   --------------------
   -- Get_Dnd_Target --
   --------------------

   procedure Get_Dnd_Target
     (MDI       : access MDI_Window_Record'Class;
      Parent    : out Gtk_Widget;
      Position  : out Child_Position;
      Rectangle : out Gdk_Rectangle)
   is
      Border_Width, Border_Height : Gint;
      Win                         : Gdk.Gdk_Window;
      Current                     : Gtk_Widget;
      X, Y                        : Gint;
   begin
      Window_At_Pointer (X, Y, Win);

      if Win = null then
         Position := Position_Automatic;
         Parent := null;

      else
         Current := Gtk_Widget (Get_User_Data (Win));

         while Current /= null
           and then Current /= Gtk_Widget (MDI)
           and then Get_Parent (Current) /= null
           and then (Current.all not in Gtk_Notebook_Record'Class
                     or else Get_Parent (Current) /= Gtk_Widget (MDI))
           and then Get_Parent (Current) /= Gtk_Widget (MDI)
         loop
            Current := Get_Parent (Current);
         end loop;

         --  If the cursor was put in a floating window, we should make the
         --  new child floating as well.
         if Current = null or else Get_Parent (Current) = null then
            Parent := null;
            Position := Position_Automatic;
            return;
         end if;

         if Current = Gtk_Widget (MDI) then
            Current := Gtk_Widget (Find_Empty_Notebook (MDI));
         end if;

         if Current = null then
            Parent   := null;
            Position := Position_Automatic;
            return;
         end if;

         Parent := Current;

         Rectangle :=
           (X      => Get_Allocation_X (Parent),
            Y      => Get_Allocation_Y (Parent),
            Width  => Get_Allocation_Width (Parent),
            Height => Get_Allocation_Height (Parent));

         --  Never split the empty area
         if Get_Nth_Page (Gtk_Notebook (Current), 0) = null then
            Position := Position_Automatic;
            return;
         end if;

         Get_Pointer (Parent, X, Y);

         Border_Height := Gint'Min
           (Max_Drag_Border_Width, Get_Allocation_Height (Parent) / 3);
         Border_Width := Gint'Min
           (Max_Drag_Border_Width, Get_Allocation_Width (Parent) / 3);

         if Y < Border_Height then
            Position := Position_Top;
            Rectangle :=
              (X      => 0,
               Y      => 0,
               Width  => Get_Allocation_Width (Parent),
               Height => Border_Height);

         elsif Y > Get_Allocation_Height (Parent) - Border_Height then
            Position := Position_Bottom;
            Rectangle :=
              (X      => 0,
               Y      => Get_Allocation_Height (Parent) - Border_Height,
               Width  => Get_Allocation_Width (Parent),
               Height => Border_Height);

         elsif X < Border_Width then
            Position := Position_Left;
            Rectangle :=
              (X      => 0,
               Y      => 0,
               Width  => Border_Width,
               Height => Get_Allocation_Height (Parent));

         elsif X > Get_Allocation_Width (Parent) - Border_Width then
            Position := Position_Right;
            Rectangle :=
              (X      => Get_Allocation_Width (Parent) - Border_Width,
               Y      => 0,
               Width  => Border_Width,
               Height => Get_Allocation_Height (Parent));

         else
            Position := Position_Automatic;
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
