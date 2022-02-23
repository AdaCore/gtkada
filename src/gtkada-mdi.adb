------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2022, AdaCore                     --
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

with Interfaces.C.Strings;    use Interfaces.C.Strings;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Tags;                use Ada.Tags;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;                  use System;
with System.Address_Image;

with GNAT.IO;                 use GNAT.IO;
with GNAT.Strings;            use GNAT.Strings;

with Glib.Convert;            use Glib.Convert;
with Glib.Error;              use Glib.Error;
with Glib.G_Icon;             use Glib.G_Icon;
with Glib.Menu;               use Glib.Menu;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Simple_Action;      use Glib.Simple_Action;
with Glib.Unicode;            use Glib.Unicode;
with Glib.Values;             use Glib.Values;
with Glib.Variant;            use Glib.Variant;

with Cairo;                   use Cairo;

with Pango;                   use Pango;
with Pango.Enums;             use Pango.Enums;
with Pango.Font;              use Pango.Font;
with Pango.Layout;            use Pango.Layout;

with Gdk;                     use Gdk;
with Gdk.Cairo;               use Gdk.Cairo;
with Gdk.Cursor;              use Gdk.Cursor;
with Gdk.Display;             use Gdk.Display;
with Gdk.Event;               use Gdk.Event;
with Gdk.Main;                use Gdk.Main;
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Gdk.RGBA;                use Gdk.RGBA;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Gdk.Types;               use Gdk.Types;
with Gdk.Types.Keysyms;
with Gdk.Window;              use Gdk.Window;

with Gtk;                     use Gtk;
with Gtk.Accel_Group;         use Gtk.Accel_Group;
with Gtk.Accel_Label;         use Gtk.Accel_Label;
with Gtk.Application;         use Gtk.Application;
with Gtk.Arguments;           use Gtk.Arguments;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Check_Menu_Item;     use Gtk.Check_Menu_Item;
with Gtk.Css_Provider;        use Gtk.Css_Provider;
with Gtk.Container;           use Gtk.Container;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Event_Box;           use Gtk.Event_Box;
with Gtk.Frame;               use Gtk.Frame;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Image;               use Gtk.Image;
with Gtk.Image_Menu_Item;     use Gtk.Image_Menu_Item;
with Gtk.Label;               use Gtk.Label;
with Gtk.Main;                use Gtk.Main;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Notebook;            use Gtk.Notebook;
with Gtk.Radio_Menu_Item;     use Gtk.Radio_Menu_Item;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Stock;               use Gtk.Stock;
with Gtk.Style_Context;       use Gtk.Style_Context;
with Gtk.Style_Provider;      use Gtk.Style_Provider;
with Gtk.Window;              use Gtk.Window;

with Gtkada.Bindings;         use Gtkada.Bindings;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtkada.Multi_Paned;      use Gtkada.Multi_Paned;
with Gtkada.Style;

pragma Warnings
  (Off, "call to obsolescent procedure ""Get_Background_Color""");
pragma Warnings (Off, "call to obsolescent procedure ""Popup""");
--  Deprecated in Gtk+ 3.24

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

   Default_Title_Font : constant String := "Sans 8";
   --  Default title font for the children

   Max_Drag_Border_Width : constant Gint := 30;
   --  Width or height of the drag-and-drop borders for each notebook. On the
   --  sides of the MDI, half of it is dedicated to moving the window so that
   --  it occupies that whole side of the MDI

   Drag_Threshold : constant Gint := 20;
   --  Our own threshold (instead of Gtk.Dnd.Check_Threshold), since on
   --  Windows the later seems to be set to 0, and thus we can't change a
   --  notebook page by clicking on its tab without splitting the notebook

   Icon_Size_In_Title : constant := 8;
   Icon_Size_In_Tabs  : constant := 8;

   MDI_Class_Record        : Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;
   Child_Class_Record      : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   MDI_Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => New_String (String (Signal_Child_Selected)),
      2 => New_String (String (Signal_Float_Child)),
      3 => New_String (String (Signal_Child_Title_Changed)),
      4 => New_String (String (Signal_Child_Added)),
      5 => New_String (String (Signal_Child_Removed)),
      6 => New_String (String (Signal_Child_Icon_Changed)),
      7 => New_String (String (Signal_Children_Reorganized)),
      8 => New_String (String (Signal_Perspective_Changed)),
      9 => New_String (String (Signal_Unfloat_Child)),
      10 => New_String (String (Signal_Before_Unfloat_Child)),
      11 => New_String (String (Signal_Before_Destroy_Child)),
      12 => New_String (String (Signal_Perspectives_Added)));

   Child_Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => New_String (String (Signal_Float_Child)),
      2 => New_String (String (Signal_Unfloat_Child)),
      3 => New_String (String (Signal_Before_Unfloat_Child)),
      4 => New_String (String (Signal_Selected)),
      5 => New_String (String (Signal_Child_State_Changed)),
      6 => New_String (String (Signal_Before_Destroy_Child)),
      7 => New_String (String (Signal_Before_Remove_Child)),
      8 => New_String (String (Signal_Maximize_Child)),
      9 => New_String (String (Signal_Unmaximize)));

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtkada_MDI_Window_MDI_Child_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtkada_MDI_Window_MDI_Child_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_MDI_Child_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_MDI_Child_Void);

   procedure Connect
     (Object  : access MDI_Window_Record'Class;
      C_Name  : Glib.Signal_Name;
      Handler : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After   : Boolean);

   procedure Connect_Slot
     (Object  : access MDI_Window_Record'Class;
      C_Name  : Glib.Signal_Name;
      Handler : Cb_GObject_MDI_Child_Void;
      After   : Boolean;
      Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_MDI_Child_Void
     (Closure         : GClosure;
      Return_Value    : Glib.Values.GValue;
      N_Params        : Glib.Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_MDI_Child_Void);

   procedure Marsh_MDI_Window_MDI_Child_Void
     (Closure         : GClosure;
      Return_Value    : Glib.Values.GValue;
      N_Params        : Glib.Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Marsh_MDI_Window_MDI_Child_Void);

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

   type MDI_Check_Menu_Item_Record is new Gtk_Check_Menu_Item_Record with
      record
         MDI : MDI_Window;
      end record;
   type MDI_Check_Menu_Item is access all MDI_Check_Menu_Item_Record'Class;

   type Child_Menu_Item_Record is new Gtk_Image_Menu_Item_Record with record
      Child : MDI_Child;
   end record;
   type Child_Menu_Item is access all Child_Menu_Item_Record'Class;
   --  A menu item used in a contextual menu, to switch to an existing notebook
   --  page.

   type Menu_Item_For_Child_Record is new Gtk_Radio_Menu_Item_Record with
      record
         Child : MDI_Child;
      end record;
   type Menu_Item_For_Child is access all Menu_Item_For_Child_Record'Class;
   --  The special type of item added to the /Window menu. We use a special
   --  widget so that we know which items to remove when we refresh the menu.

   type MDI_Menu_Record is new Gtk_Menu_Record with record
      MDI : MDI_Window;
   end record;
   type MDI_Menu is access all MDI_Menu_Record'Class;

   type MDI_Tab_Record is new Gtk_Event_Box_Record with record
      Timestamp : Natural := 0;
   end record;
   type MDI_Tab is access all MDI_Tab_Record'Class;
   --  A container that requests a specific size for a Gtk_Label that is
   --  displayed in a Gtk_Notebook tab. This is a workaround against the naive
   --  algorithm used in gtk+ to compute the size of tabs. What we want is the
   --  following:
   --  * if the sum of natural sizes of all tab labels is less than the
   --    notebook's size, use the natural size (we need to pass this as the
   --    minimal size as well, since gtk+ notebook always uses the latter).
   --  * Otherwise, we want all tabs to have the same size (down to a minimum
   --    after which we'll let the notebook scroll), and use ellipsization in
   --    the labels.
   --  This label will automatically collaborate with all other such labels in
   --  the same notebook for size negociation.

   MDI_Tab_Class_Record : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   procedure Tab_Class_Init (Self : GObject_Class);
   pragma Convention (C, Tab_Class_Init);
   --  Initialize the gtk+ class fields

   function Short_Title_Less_Than (C1, C2 : MDI_Child) return Boolean;
   function Short_Title_Less_Than (C1, C2 : MDI_Child) return Boolean is
   begin
      return C1.Get_Short_Title < C2.Get_Short_Title;
   end Short_Title_Less_Than;

   package Child_Vectors is new Ada.Containers.Vectors (Natural, MDI_Child);
   package Vector_Sort is new Child_Vectors.Generic_Sorting
      (Short_Title_Less_Than);

   procedure Tab_Get_Preferred_Width
     (Label : System.Address; Minimum_Size, Natural_Size : out Gint);
   pragma Convention (C, Tab_Get_Preferred_Width);
   procedure Tab_Get_Preferred_Height
     (Label : System.Address; Minimum_Size, Natural_Size : out Gint);
   pragma Convention (C, Tab_Get_Preferred_Height);

   package Tab_Orientation_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Notebook_Record, Tab_Orientation_Type);

   procedure On_Tab_Orientation
     (Notebook    : access Gtk_Notebook_Record'Class;
      Orientation : Tab_Orientation_Type);
   --  Called when the user selects a new orientation for the tabs.

   type MDI_Notebook_Record is new Gtk.Notebook.Gtk_Notebook_Record with record
      Timestamp : Natural := 0;
      Tab_Size  : Gint;
      --  Used to handle the sizing of tabs (through MDI_Tab_Record). Tab_Size
      --  is set to -1 if the tabs should use their natural size.

      Tab_Orientation : Tab_Orientation_Type := Automatic;
      --  Orientation for the tabs, as requested by the user

      Actual_Tab_Orientation : Tab_Orientation_Type := Horizontal;
      --  The orientation after "Automatic" has been resolved.
   end record;
   type MDI_Notebook is access all MDI_Notebook_Record'Class;
   --  The type of notebooks used in the MDI.

   procedure Menu_Switch_Page (Item : access Gtk_Widget_Record'Class);
   --  Called when a Child_Menu_Item is activated

   package Child_User_Data is new Glib.Object.User_Data (MDI_Child);

   type Children_Array is array (Natural range <>) of Widget_List.Glist;

   procedure Free is new
     Ada.Unchecked_Deallocation (UTF8_String, String_Access);

   package Tab_Pos_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Notebook_Record, Gtk.Enums.Gtk_Position_Type);
   procedure On_Tab_Pos
     (Note : access Gtk_Notebook_Record'Class;
      Pos  : Gtk.Enums.Gtk_Position_Type);
   --  Called when the user changes the position of tabs for a specific
   --  notebook.

   function Button_Pressed
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   function Button_Release
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   function Button_Motion
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean;
   --  Handle the various mouse events on a child (or the tab of a child)

   function Button_Pressed_Forced
     (Child : access GObject_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   function Button_Motion_Forced
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean;
   function Button_Release_Forced
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button;
      Allow_Move : Boolean := True) return Boolean;
   --  Same as Button_Pressed, Button_Motion and Button_Release, but perform
   --  an action even if the event is not related to the child's window.

   function Button_Motion_Notebook
     (Note : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean;
   function Button_Release_Notebook
     (Note : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   --  Same as Button_Motion and Button_Release, but monitor the events on the
   --  notebook itself (which is where the event is sent during a reorder
   --  operation).

   function Button_Pressed_On_Title_Icon
     (Child : access GObject_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   --  The title icon of a child window was clicked (and if it was a
   --  double click, we want to close that child).

   function On_Notebook_Button_Press
     (Child    : access Gtk_Widget_Record'Class;
      Event    : Gdk.Event.Gdk_Event) return Boolean;
   --  Manage the contextual menu on tabs

   procedure On_Notebook_Before_Floating_Destroy
     (Slot : access Glib.Object.GObject_Record'Class);
   --  Called when a notebook stored before floating a child is destroyed.
   --  Used to avoid dangling pointers.

   function Hover_Tabs
     (N : MDI_Notebook; X_Root, Y_Root : Gdouble) return Boolean;
   --  Returns True if the given root coordinates are on top of the tabs area
   --  of the specified notebook.

   procedure Child_Widget_Shown
     (Widget : access Gtk_Widget_Record'Class);
   procedure Child_Widget_Hidden
     (Widget : access Gtk_Widget_Record'Class);
   --  Called when the child widget is shown or hidden by the user, to reflect
   --  that fact at the MDI_Child level, no matter whether the child is
   --  currently floating or not.

   procedure On_Select_Child_Update_Close_Menu
      (Item   : access Gtk_Widget_Record'Class;
       Params : GValues);
   --  Called to update the "Close" menu item, when a new child is selected

   procedure On_Float_Child_Update_Menu
      (Check : access Gtk_Widget_Record'Class);
   --  Updates the "/Window/Floating" menu item when a child is floated or
   --  unfloated. The argument is a MDI_Check_Menu_Item

   function Insert_Child_If_Needed
     (MDI   : access MDI_Window_Record'Class;
      Child : MDI_Child) return MDI_Child;
   --  If the child is currently invisible in the perspective, insert it back
   --  in the MDI. In both case, return the child itself

   function Create_Notebook
     (MDI : access MDI_Window_Record'Class) return MDI_Notebook;
   --  Create a notebook, and set it up for drag-and-drop

   procedure Configure_Notebook_Tabs
     (MDI           : access MDI_Window_Record'Class;
      Notebook      : access Gtk_Notebook_Record'Class;
      Hide_If_Empty : Boolean := False);
   --  Configure the visibility and position of notebook tabs.
   --  If there are no visible pages and Hide_If_Empty is true, then the
   --  notebook itself is hidden

   procedure Update_Tab_Color
     (Note    : access MDI_Notebook_Record'Class;
      Focused : Boolean);
   --  Change the background color of the notebook tab containing child,
   --  depending on whether the child is selected or not.

   procedure Set_State
     (Child : access MDI_Child_Record'Class; New_State : State_Type);
   --  Change the state of Child

   function Delete_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Forward a delete_event from the toplevel window to the child

   procedure Destroy_Child (Child : access Gtk_Widget_Record'Class);
   procedure Destroy_Initial_Child
      (Child : access Gtk_Widget_Record'Class);
   --  Called when either the child itself, or the widget we initially put
   --  in it, are destroyed. Remove the child from the MDI properly.

   procedure Destroy_MDI (MDI : access Gtk_Widget_Record'Class);
   --  Called when the MDI is destroyed

   procedure Realize_MDI (MDI : access Gtk_Widget_Record'Class);
   --  Called when the child is realized

   procedure Map_MDI (MDI : access Gtk_Widget_Record'Class);
   --  Called when the child is mapped

   procedure Set_Dnd_Source
     (Widget : access Gtk_Widget_Record'Class;
      Child  : access Gtk_Widget_Record'Class);
   --  Setup a widget as either a source or a target for drag-and-drop ops.

   procedure Get_Dnd_Target
     (MDI              : not null access MDI_Window_Record'Class;
      Parent           : out Gtk_Widget;
      Position         : out Child_Position;
      Parent_Rectangle : out Gdk_Rectangle;
      Rectangle        : out Gdk_Rectangle;
      In_Central       : out Boolean;
      Allowed          : out Boolean);
   --  Return the widget that is the current target for dnd
   --  Position indicated where in the parent the child would be dropped:
   --    Position_Bottom .. Position_Right: To one of the sides
   --    Position_Automatic:                In the center
   --  Rectangle are the coordinates of the target rectangle, relative to the
   --  top-left corner of the MDI. Parent_Rectangle is the area of the
   --  notebook on top of which the pointer is (so Rectangle is a subrectangle
   --  of it).
   --  Allowed is set to False if the current target is invalid for the child.
   --  In_Central is set to true if the Parent is inside the central area.

   procedure Draw_Dnd_Rectangle
     (Child : access MDI_Child_Record'Class;
      Hide_Only : Boolean := False);
   --  Draw the DND rectangle

   procedure Update_Float_Menu (Child : access MDI_Child_Record'Class);
   --  Update the state of the "Float" menu item associated with child

   procedure Put_In_Notebook
     (MDI                      : access MDI_Window_Record'Class;
      Child                    : access MDI_Child_Record'Class;
      Notebook                 : MDI_Notebook := null;
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
     (Child : access MDI_Child_Record'Class) return MDI_Notebook;
   --  Return the notebook that directly contains Child

   procedure Get_Sorted_List_Of_Visible_Children
      (MDI  : not null access MDI_Window_Record'Class;
       Vec  : out Child_Vectors.Vector);
   --  Return the list of visible children in Vec.

   procedure Update_Menu_Model_List_Of_Children
     (MDI : not null access MDI_Window_Record'Class);
   procedure Add_Child_Menu
     (Self  : access Glib.Object.GObject_Record'Class;
      Child : not null access MDI_Child_Record'Class);
   procedure Remove_Child_Menu
     (Self  : access Glib.Object.GObject_Record'Class;
      Child : not null access MDI_Child_Record'Class);
   procedure Update_Child_Menu
     (Self  : access Glib.Object.GObject_Record'Class;
      Child : not null access MDI_Child_Record'Class);
   procedure Recompute_Menu
     (Menu : access Gtk_Widget_Record'Class);
   procedure On_Child_Selected_Update_Menu
     (Item : access Gtk_Widget_Record'Class);
   --  Update the contents of the menu with the list of children.

   function Find_Child_Menu
     (Menu  : access MDI_Menu_Record'Class;
      Child : not null access MDI_Child_Record'Class)
      return Menu_Item_For_Child;
   --  Return the Menu of Child in MDI

   procedure Internal_Add_Child_Menu
     (Menu  : access MDI_Menu_Record'Class;
      Child : not null access MDI_Child_Record'Class;
      Group : in out Widget_SList.GSlist);
   --  Add a menu for Child in Group of Menu

   procedure Internal_Update_Menu_Content
     (Child : access MDI_Child_Record'Class;
      It    : not null access Gtk_Radio_Menu_Item_Record'Class);
   --  Destroy and recreate the content of a Menu

   procedure Split_H_Cb    (MDI  : access Gtk_Widget_Record'Class);
   procedure Split_V_Cb    (MDI  : access Gtk_Widget_Record'Class);
   procedure Maximize_Cb   (MDI  : access Gtk_Widget_Record'Class);
   procedure Unmaximize_Cb (MDI  : access Gtk_Widget_Record'Class);
   procedure Float_Cb      (MDI  : access GObject_Record'Class);
   procedure Close_Cb      (MDI  : access Gtk_Widget_Record'Class);
   procedure Focus_Cb      (Item : access Gtk_Check_Menu_Item_Record'Class);
   --  Callbacks for the menu

   procedure On_Action_Floating
     (MDI       : access GObject_Record'Class;
      Parameter : Glib.Variant.Gvariant);
   procedure On_Action_Close
     (MDI       : access GObject_Record'Class;
      Parameter : Glib.Variant.Gvariant);
   procedure On_Action_Split_H
     (MDI       : access GObject_Record'Class;
      Parameter : Glib.Variant.Gvariant);
   procedure On_Action_Split_V
     (MDI       : access GObject_Record'Class;
      Parameter : Glib.Variant.Gvariant);
   procedure On_Action_Select
     (MDI       : access GObject_Record'Class;
      Parameter : Glib.Variant.Gvariant);
   --  Callbacks for the Gaction associated with menus.

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
     (Child : access GObject_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   --  Gives the focus to Child when the notebook tab associated with it is
   --  pressed.

   procedure Set_Focus_Child_Switch_Notebook_Page
     (Note : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when a new page from a notebook has been selected, in particular
   --  when using the scroll arrows when there are too many pages to be
   --  displayed

   function Toplevel_Focus_In
     (MDI   : access GObject_Record'Class;
      Event : Gdk_Event_Focus) return Boolean;
   --  Called when the toplevel window that contains a the MDI gains the focus
   --  from the window manager

   procedure Give_Focus_To_Child (Child : MDI_Child);
   --  Give the focus to a specific MDI child
   --  You should never call Grab_Focus directly

   function Matching_Children
     (MDI : access MDI_Window_Record'Class; Str : String)
      return Children_Array;
   --  Return the list of children of the MDI that match Str

   procedure Reset_Title_Bars_And_Colors
     (MDI  : access MDI_Window_Record'Class);
   --  Reset the color and title bar of the MDI Child.

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

   function Find_Current_In_Central
     (Pane             : access Gtkada_Multi_Paned_Record'Class;
      MDI              : access MDI_Window_Record'Class;
      Group            : Child_Group := Group_Any;
      Initial_Position : Child_Position := Position_Automatic)
      return MDI_Notebook;
   --  Return the first notebook that contains at least one child within the
   --  given Group. The search starts in the notebook that currently has the
   --  focus.
   --  A new notebook is created if needed (ie if no notebook has a child with
   --  the same attribute).

   procedure Removed_From_Notebook
     (Note : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when a child is removed from one of the notebooks

   procedure Emit_By_Name_Child
     (Object : System.Address; Name : String; Child : System.Address);
   pragma Import (C, Emit_By_Name_Child, "ada_g_signal_emit_by_name_ptr");

   procedure Emit_By_Name (Object : System.Address; Name : String);
   pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name");

   procedure Emit_By_Name_Str (Object : System.Address; Name, Param : String);
   pragma Import (C, Emit_By_Name_Str, "ada_g_signal_emit_by_name_ptr");
   --  Both Name and Param must end with ASCII.NUL

   procedure Internal_Float_Child
     (Child             : access MDI_Child_Record'Class;
      Float             : Boolean;
      Position_At_Mouse : Boolean;
      X, Y              : Gint;
      Width, Height     : Gint := -1);
   --  internal version of Float_Child, where the user can choose whether the
   --  new floating window should be located where the mouse is, or at
   --  coordinates specified by (X, Y).
   --  (Width, Height) can be use to force a specific size.

   procedure Set_Child_Title_Bar (Child : access MDI_Child_Record'Class);
   --  Hide or display the title bar of the child, depending on its status.

   procedure Note_Notify (Data : System.Address; Where : System.Address);
   pragma Convention (C, Note_Notify);
   --  Notified if the old notebook that contained Child is destroyed

   function In_Central_Area
     (MDI   : access MDI_Window_Record'Class;
      Child : access Gtk_Widget_Record'Class) return Boolean;
   --  Whether Child is in the central area

   procedure Move_To_Next_Notebook (Iterator : in out Child_Iterator);
   --  Move to the next notebook for this iterator (does nothing if Iterator
   --  already points to a notebook).

   procedure Freeze_Focus (MDI : access MDI_Window_Record'Class);
   --  Freeze emission of focus changes. Each of these calls should correspond
   --  to a call to Thaw_Focus below.

   procedure Thaw_Focus (MDI : access MDI_Window_Record'Class);
   --  Unfreeze the emission of the focus, and do the actual focus change on
   --  the current child.

   procedure For_Each_Notebook
      (Self         : not null access MDI_Window_Record'Class;
       Central_Only : Boolean;
       Callback     : not null access procedure (Note : MDI_Notebook));
   --  Traverse all notebooks contained in Self.

   package Close_Button is

      --  We use an event box as a basis so that we have a gdk_window
      --  available for handling mouse events. We'll set this event box as
      --  transparent to be able to draw transparent buttons as we wish.
      type Gtkada_MDI_Close_Button_Record is new Gtk_Event_Box_Record
      with record
         Child        : MDI_Child;
         --  The child this button is attached to

         Tab_Over     : Boolean;
         --  Whether the mouse is over the button's container

         Over         : Boolean;
         --  Whether the mouse is over the button

         Pressed      : Boolean;
         --  Whether the button is pressed

         In_Titlebar  : Boolean;
         --  Whether the button is in the title bar or in the tab

         Position     : Gtk_Position_Type;
         --  The tab's position

         Default_Size : Glib.Gint;
         --  The button's default size. The actual drawing depends on the final
         --  allocated space.
      end record;
      type Gtkada_MDI_Close_Button is
        access all Gtkada_MDI_Close_Button_Record'Class;

      procedure Gtk_New
        (Button      : out Gtkada_MDI_Close_Button;
         Tab         : access Gtk_Widget_Record'Class;
         Child       : access MDI_Child_Record'Class;
         Position    : Gtk.Enums.Gtk_Position_Type;
         In_Titlebar : Boolean);
      --  Tab: the button's container. This container shall have a Gdk_Window
      --   to allow mouse motion event retrieval.
      --  Child: the MDI child that button is attached to. This child is closed
      --   upon button click.
      --  In_Titlebar: set to True if the button is in the title bar, to false
      --   if it's in the notebook tab.

   end Close_Button;
   package body Close_Button is separate;

   package Maximize_Action is
      function Is_Maximized_Mode
        (MDI : access MDI_Window_Record'Class) return Boolean;
      --  Return True if a child is currently maximized in MDI

      procedure On_Toggle_Maximize
        (Child : access Gtk_Widget_Record'Class);
      --  Callback when the signal "double_click_child_tab" is sent

      procedure On_Remove_Child
        (Self : access Gtk.Widget.Gtk_Widget_Record'Class);
      --  Called before destroying a child

      procedure Hide_When_Maximized (MDI : access MDI_Window_Record'Class);
      --  Hide the non-maximized MDI children

      procedure Free_Saved_Data (MDI : access MDI_Window_Record'Class);
      --  Free the data used by the maximize feature

   end Maximize_Action;
   package body Maximize_Action is separate;

   function Get_Icon
     (Child : not null access MDI_Child_Record'Class)
      return Gtk_Image;
   --  Retrieve icon. Can return null.

   ---------------------
   -- In_Central_Area --
   ---------------------

   function In_Central_Area
     (MDI   : access MDI_Window_Record'Class;
      Child : access Gtk_Widget_Record'Class) return Boolean
   is
      P : Gtk_Widget := Get_Parent (Child);
   begin
      while P /= null and then P /= Gtk_Widget (MDI) loop
         if P = Gtk_Widget (MDI.Central) then
            return True;
         end if;

         P := Get_Parent (P);
      end loop;

      return False;
   end In_Central_Area;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
     (Child : access MDI_Child_Record'Class; New_State : State_Type)
   is
      Old_State : constant State_Type := Child.State;
   begin
      if New_State /= Old_State then
         Child.State := New_State;
         Widget_Callback.Emit_By_Name (Child, Signal_Child_State_Changed);
      end if;
   end Set_State;

   ------------------------
   -- Get_Child_Notebook --
   ------------------------

   function Get_Child_Notebook
     (Child : access MDI_Child_Record'Class) return Gtk.Notebook.Gtk_Notebook
   is
      Notebook : constant MDI_Notebook := Get_Notebook (Child);
   begin
      return Gtk_Notebook (Notebook);
   end Get_Child_Notebook;

   ------------------
   -- Get_Notebook --
   ------------------

   function Get_Notebook
     (Child : access MDI_Child_Record'Class) return MDI_Notebook is
   begin
      case Child.State is
         when Floating  => return null;
         when Invisible => return null;
         when Normal    =>
            if Get_Parent (Child) /= null
              and then Get_Parent (Child).all in Gtk_Notebook_Record'Class
            then
               return MDI_Notebook (Get_Parent (Child));
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
            Print_Debug ("Set_Focus_Child_MDI");
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

      --  Avoid setting the focus child at this point when we are performing
      --  a Drag'n'Drop operation: the focus will be set properly once it
      --  has fully finished.

      if Child /= null and then Child.MDI.In_Drag = No_Drag then
         Print_Debug ("Set_Focus_Child_Switch_Notebook_Page "
                      & Get_Title (Child));
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
      if Widget /= null
        and then Widget.all in MDI_Child_Record'Class
      then
         Print_Debug ("Set_Focus_Child_Notebook "
                      & Get_Title (MDI_Child (Widget)));
         Set_Focus_Child (MDI_Child (Widget));
      end if;
   end Set_Focus_Child_Notebook;

   ----------------------------------
   -- Set_Focus_Child_MDI_Floating --
   ----------------------------------

   function Set_Focus_Child_MDI_Floating
     (Child : access Gtk_Widget_Record'Class) return Boolean
   is
      C  : constant MDI_Child := MDI_Child (Child);
   begin
      Print_Debug ("Set_Focus_Child_MDI_Floating");
      Set_Focus_Child (C);
      return False;
   end Set_Focus_Child_MDI_Floating;

   ----------------------------------
   -- Set_Focus_Child_MDI_From_Tab --
   ----------------------------------

   function Set_Focus_Child_MDI_From_Tab
     (Child : access GObject_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      if Event.The_Type = Button_Release then
         Tmp := Button_Release (Child => Gtk_Widget (Child), Event => Event);
         return False;

      elsif Event.Button = 1 then
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
     (MDI   : access GObject_Record'Class;
      Event : Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
      M   : constant MDI_Window := MDI_Window (MDI);
      Win : constant Gtk_Window := Gtk_Window (Get_Toplevel (M));
   begin
      Print_Debug ("Toplevel_Focus_In");

      --  When the main window gains the focus, make sure to give the focus
      --  to the MDI child that had the focus before leaving the main window.

      if (M.Focus_Child /= null
          and then M.Focus_Child.State = Floating)
        or else M.Focus_Child = null
      then
         declare
            Focus_Widget : constant Gtk_Widget := Win.Get_Focus;
            Child        : MDI_Child := null;
         begin
            if Focus_Widget /= null then
               Child := Find_MDI_Child_From_Widget (Focus_Widget);
            end if;

            --  If the focus widget of the main window is contained in a MDI
            --  child, make sure to give it the focus too.
            --
            --  Otherwise, give the focus to the last focused MDI child of the
            --  main window.

            if Child /= null then
               Give_Focus_To_Child (Child);
               M.Set_Focus_Child (Child);
            end if;
         end;
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
      Parent.On_Focus_In_Event (Toplevel_Focus_In'Access, MDI);
   end Setup_Toplevel_Window;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (MDI   : out MDI_Window;
      Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class;
      Independent_Perspectives  : Boolean := False) is
   begin
      MDI := new MDI_Window_Record;
      Gtkada.MDI.Initialize (MDI, Group, Independent_Perspectives);
   end Gtk_New;

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType is
      Signal_Parameters : constant Glib.Object.Signal_Parameter_Types
         (Integer (MDI_Signals'First) .. Integer (MDI_Signals'Last), 1 .. 1) :=
        (1 => (1 => GType_Pointer),
         2 => (1 => GType_Pointer),
         3 => (1 => GType_Pointer),
         4 => (1 => GType_Pointer),
         5 => (1 => GType_Pointer),
         6 => (1 => GType_Pointer),
         7 => (1 => GType_None),
         8 => (1 => GType_None),
         9 => (1 => GType_Pointer),
         10 => (1 => GType_Pointer),
         11 => (1 => GType_Pointer),
         12 => (1 => GType_String));
   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtkada.Multi_Paned.Get_Type,
         Signals      => MDI_Signals,
         Class_Record => MDI_Class_Record,
         Type_Name    => "GtkAdaMDI",
         Parameters   => Signal_Parameters);
      return MDI_Class_Record.The_Type;
   end Get_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (MDI   : access MDI_Window_Record'Class;
      Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class;
      Independent_Perspectives  : Boolean := False)
   is
      Ctx : Gtk_Style_Context;
      Success : Boolean;
      Title_Color : Gdk.RGBA.Gdk_RGBA;
      Focus_Color : Gdk.RGBA.Gdk_RGBA;
   begin
      G_New (MDI, Gtkada.MDI.Get_Type);     --  void if already initialized
      Gtkada.Multi_Paned.Initialize (MDI);  --  Initialize parent Ada fields

      Get_Style_Context (MDI).Add_Class ("mdi");

      --  Request a null size, so that the window can be resized at will, even
      --  though we have played with Set_Size_Request on the children.

      Set_Size_Request (MDI, 0, 0);

      --  The MDI must have a window, so that we can change the background
      --  color. No other notebook or paned inside has a window

      Set_Has_Window (MDI, True);

      MDI.Group := Gtk_Accel_Group (Group);
      MDI.Independent_Perspectives := Independent_Perspectives;

      Set_Dnd_Message (MDI, "");

      Parse (Title_Color, Default_Title_Bar_Color, Success);
      Parse (Focus_Color, Default_Title_Bar_Focus_Color, Success);

      Gtk_New (MDI.Css_Provider);
      Gtk.Style_Context.Add_Provider_For_Screen
        (Get_Style_Context (MDI).Get_Screen, +MDI.Css_Provider,
         Priority => Gtk.Style_Provider.Priority_Settings);

      Ctx := Get_Style_Context (MDI);
      Ctx.Get_Color (Gtk_State_Flag_Normal, MDI.Default_Title_Color);

      Configure
        (MDI,
         Title_Bar_Color   => Title_Color,
         Focus_Title_Color => Focus_Color);

      --  Create a default empty central area. That will be overridden if the
      --  user loads a perspective later on

      Gtk_New (MDI.Central);

      Add_Child (MDI, MDI.Central);

      --  Put an empty notebook in the MDI, which will act as a recipient for
      --  the Position_Default widgets

      Add_Child
        (MDI.Central, New_Child => Create_Notebook (MDI),
         Width       => -1,
         Height      => -1,
         Orientation => Orientation_Vertical);

      Widget_Callback.Connect
        (MDI, Gtk.Widget.Signal_Realize,
         Widget_Callback.To_Marshaller (Realize_MDI'Access));
      Widget_Callback.Connect
        (MDI, Gtk.Widget.Signal_Map,
         Widget_Callback.To_Marshaller (Map_MDI'Access));
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
            Scaled : Gdk_Pixbuf;
         begin
            C := MDI_Child (Get_Data (D.Current_Child));
            Set_Text (D.Label, Get_Short_Title (C));

            if C.Icon_Name /= null then
               D.Icon.Set_From_Icon_Name
                  (C.Icon_Name.all, Size => Icon_Size_Menu);
            elsif C.Title_Icon /= null then
               Scaled := Get_Icon (C);
               Set_Child_Visible (D.Icon, Scaled /= null);
               if Scaled /= null then
                  Scaled := Scale_Simple (Scaled, 32, 32);
                  Gtk.Image.Set (D.Icon, Scaled);
                  Unref (Scaled);
               end if;
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
         Key := Event.Key.Keyval;

         if Key = Gdk.Types.Keysyms.GDK_BackSpace
           or else Key = Gdk.Types.Keysyms.GDK_Delete
         then
            Delete_Text (D.Ent, Gint (D.Length) - 1, -1);
         else
            Delete_Text (D.Ent, Gint (D.Length), -1);

            Event.Key.State := 0;
            Tmp := Return_Callback.Emit_By_Name
              (D.Ent, "key_press_event", Event);
         end if;

         Update_Selection_Dialog (M, 0);
         return True;

      elsif Get_Event_Type (Event) = Key_Release then
         Key := Event.Key.Keyval;

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
            M.Selection_Dialog.Grab_Remove;
            Destroy (M.Selection_Dialog);
            M.Selection_Dialog := null;
         end if;

         return True;
      end if;

      return False;
   end Key_Event_Selection_Dialog;

   -----------------------
   -- For_Each_Notebook --
   -----------------------

   procedure For_Each_Notebook
      (Self         : not null access MDI_Window_Record'Class;
       Central_Only : Boolean;
       Callback     : not null access procedure (Note : MDI_Notebook))
   is
      procedure For_Multi_Paned
         (Pane : not null access Gtkada_Multi_Paned_Record'Class);
      --  Process a specific multi-pane recursively

      procedure For_Multi_Paned
         (Pane : not null access Gtkada_Multi_Paned_Record'Class)
      is
         Iter : Gtkada.Multi_Paned.Child_Iterator := Pane.Start;
         W    : Gtk_Widget;
      begin
         while not At_End (Iter) loop
            W := Get_Widget (Iter);
            if W = null then
               null;   --  separator
            elsif W.all in Gtkada_Multi_Paned_Record'Class then
               For_Multi_Paned (Gtkada_Multi_Paned (W));
            else
               Callback (MDI_Notebook (W));
            end if;

            Next (Iter);
         end loop;
      end For_Multi_Paned;

   begin
      if Central_Only then
         For_Multi_Paned (Self.Central);
      else
         For_Multi_Paned (Self);
      end if;
   end For_Each_Notebook;

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
         if MDI.Focus_Child = null then
            --  Nothing to do
            return;
         end if;

         --  We can't simply look at MDI.Items, since otherwise we
         --  will only be traversing the same two items in the
         --  following scenario: split main window in 3; select window 1;
         --  move to next window (window 2 is selected and moved to front
         --  of MDI.Items); move to next window (window 1 is selected
         --  again).

         declare
            Current : constant MDI_Notebook := Get_Notebook (MDI.Focus_Child);
            Child   : MDI_Child;

            procedure On_Notebook (Note : MDI_Notebook);
            --  Called for each notebook in the MDI

            First_Notebook : MDI_Notebook;
            Prev_Notebook  : MDI_Notebook;
            Next_Notebook  : MDI_Notebook;
            Last_Notebook  : MDI_Notebook;
            --  The notebook just before or just after the current notebook,
            --  for which the current child belongs to the right group.

            Found : Boolean := False;

            procedure On_Notebook (Note : MDI_Notebook) is
               C : MDI_Child;
            begin
               if Note = Current then
                  Found := True;
               else
                  C := MDI_Child (Note.Get_Nth_Page (Note.Get_Current_Page));
                  if Only_Group /= Group_Any
                     and then C.Group /= Only_Group
                  then
                     --  Not a notebook we are interested in
                     return;
                  end if;

                  --  Store first and last notebook, to wrap around
                  Last_Notebook := Note;
                  if First_Notebook = null then
                     First_Notebook := Note;
                  end if;

                  if not Found then
                     Prev_Notebook := Note;
                  elsif Next_Notebook = null then
                     Next_Notebook := Note;
                  end if;
               end if;
            end On_Notebook;

         begin
            For_Each_Notebook
               (MDI, Central_Only => False, Callback => On_Notebook'Access);

            if Move_To_Next then
               if Next_Notebook = null then
                  Next_Notebook := First_Notebook;
               end if;

               if Next_Notebook /= null then
                  Child := MDI_Child (Next_Notebook.Get_Nth_Page
                     (Next_Notebook.Get_Current_Page));
                  Set_Focus_Child (Child);
               end if;

            else
               if Prev_Notebook = null then
                  Prev_Notebook := Last_Notebook;
               end if;

               if Prev_Notebook /= null then
                  Child := MDI_Child (Prev_Notebook.Get_Nth_Page
                     (Prev_Notebook.Get_Current_Page));
                  Set_Focus_Child (Child);
               end if;
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
         D.Grab_Add;

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
      Title_Bar_Color           : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
      Focus_Title_Color         : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
      Draw_Title_Bars           : Title_Bars_Policy   := Always;
      Tabs_Position             : Gtk.Enums.Gtk_Position_Type :=
        Gtk.Enums.Pos_Top;
      Show_Tabs_Policy          : Show_Tabs_Policy_Enum := Automatic;
      Homogeneous_Tabs          : Boolean := True;
      Tabs_Orientation          : Tab_Orientation_Type := Automatic)
   is
      C            : MDI_Child;
      Need_Redraw  : Boolean := MDI.Draw_Title_Bars /= Draw_Title_Bars;
      Iter         : Child_Iterator;
      Old_Tabs_Pos : constant Gtk_Position_Type := MDI.Tabs_Position;
      Pos_Changed  : constant Boolean := Old_Tabs_Pos /= Tabs_Position;
      Old_Tabs_Rot : constant Tab_Orientation_Type := MDI.Tabs_Orientation;
      Rot_Changed  : constant Boolean := Old_Tabs_Rot /= Tabs_Orientation;
      Note         : Gtk_Notebook;
      Success      : Boolean;
      pragma Unreferenced (Success);
      Default_Bg   : Gdk.RGBA.Gdk_RGBA;
      Homogeneous_Changed : constant Boolean :=
         MDI.Homogeneous_Tabs /= Homogeneous_Tabs;

   begin
      MDI.Close_Floating_Is_Unfloat := Close_Floating_Is_Unfloat;
      MDI.Draw_Title_Bars  := Draw_Title_Bars;
      MDI.Tabs_Position    := Tabs_Position;
      MDI.Tabs_Orientation := Tabs_Orientation;
      MDI.Show_Tabs_Policy := Show_Tabs_Policy;
      MDI.Homogeneous_Tabs := Homogeneous_Tabs;

      Set_Opaque_Resizing (MDI, Opaque_Resize);

      if MDI.Central /= null then
         MDI.Central.Set_Opaque_Resizing (Opaque_Resize);
      end if;

      if MDI.Title_Font /= null then
         Free (MDI.Title_Font);
      end if;

      if Title_Font = null then
         MDI.Title_Font := From_String (Default_Title_Font);
      else
         MDI.Title_Font := Copy (Title_Font);
      end if;

      MDI.Title_Bar_Height :=
        2 + Get_Size (MDI.Title_Font) / Pango.Enums.Pango_Scale;

      if (Focus_Title_Color /= Gdk.RGBA.Null_RGBA
          and then MDI.Focus_Title_Color /= Focus_Title_Color)
        or (Title_Bar_Color /= Gdk.RGBA.Null_RGBA
            and then MDI.Title_Bar_Color /= Title_Bar_Color)
      then
         MDI.Focus_Title_Color := Focus_Title_Color;
         MDI.Title_Bar_Color := Title_Bar_Color;

         Get_Style_Context (MDI).Get_Background_Color
           (Gtk.Enums.Gtk_State_Flag_Normal,
            Default_Bg);

         declare
            Err  : aliased GError;
            C    : constant String :=
                     Gdk.RGBA.To_String (MDI.Focus_Title_Color);
            Bg   : constant String :=
                     Gdk.RGBA.To_String (MDI.Title_Bar_Color);
            LF   : Character renames ASCII.LF;

            --  This will highlight the whole notebook in blue when not
            --  using a proper theme. Otherwise, the theme will override
            --  some of the settings, and only the current focused tab
            --  will be highlighted with the title color.
            --  The theme should define
            --       .mdifocused {background-image: -gtk-gradient{...}}
            --  or some such, to give a background to the tabs. We can't
            --  do it here, since the following is loaded with a higher
            --  priority than the theme, and thus it would override the
            --  theme.

            Css  : constant String :=
                     ".mdititle {" & LF & --  Normal title
                     "  border-width: 2px;" & LF &
                     "  background-image: -gtk-gradient(" & LF &
                     "            linear," & LF &
                     "            left top, left bottom," & LF &
                     "            from(" & Bg & ")," & LF &
                     "            to(shade(" & Bg & ",1.1)));" & LF &
                     "}" & LF &
                     ".mdifocused .mdititle {" & LF & --  Focused title
                     "  background-image: -gtk-gradient(" & LF &
                     "            linear," & LF &
                     "            left top, left bottom," & LF &
                     "            from(" & C & ")," & LF &
                     "            to(shade(" & C & ",1.1)));" & LF &
                     "}";
         begin
            Success := MDI.Css_Provider.Load_From_Data (Css, Err'Access);
         end;
      end if;

      --  Ignore changes in colors, unless the MDI is realized

      Iter := First_Child
        (MDI, Group_By_Notebook => True, Visible_Only => True);

      loop
         C := Get (Iter);
         exit when C = null;

         if Get_Notebook (Iter) /= Note then
            Note := Get_Notebook (Iter);

            if Note /= null then
               --  Unless we had a specific position for tabs in this notebook

               if Pos_Changed
                 and then Get_Tab_Pos (Note) = Old_Tabs_Pos
               then
                  On_Tab_Pos (Note, MDI.Tabs_Position);
               elsif Rot_Changed
                 and then MDI_Notebook (Note).Tab_Orientation = Old_Tabs_Rot
               then
                  On_Tab_Orientation (Note, MDI.Tabs_Orientation);
               end if;

               Configure_Notebook_Tabs (MDI, Note);
            end if;
         end if;

         if Homogeneous_Changed then
            Update_Tab_Label (C);
         end if;

         Next (Iter);
      end loop;

      if MDI.Get_Realized then
         if Title_Bar_Color /= Null_RGBA then
            Need_Redraw := True;
         end if;

         if Focus_Title_Color /= Null_RGBA then
            Need_Redraw := True;
         end if;
      end if;

      Reset_Title_Bars_And_Colors (MDI);

      if Need_Redraw then
         Queue_Draw (MDI);
      end if;
   end Configure;

   ------------------------------
   -- Independent_Perspectives --
   ------------------------------

   function Independent_Perspectives
     (MDI : access MDI_Window_Record) return Boolean is
   begin
      return MDI.Independent_Perspectives;
   end Independent_Perspectives;

   ---------------------------------
   -- Reset_Title_Bars_And_Colors --
   ---------------------------------

   procedure Reset_Title_Bars_And_Colors
     (MDI  : access MDI_Window_Record'Class)
   is
      List : Widget_List.Glist;
      C    : MDI_Child;
   begin
      List := First (MDI.Items);
      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));
         C.Title_Label.Override_Font (MDI.Title_Font);
         Set_Child_Title_Bar (C);
         List := Widget_List.Next (List);
      end loop;
   end Reset_Title_Bars_And_Colors;

   -----------------
   -- Realize_MDI --
   -----------------

   procedure Realize_MDI (MDI : access Gtk_Widget_Record'Class) is
      M : constant MDI_Window := MDI_Window (MDI);

   begin
      if M.Cursor_Cross = null then
         Gdk_New (M.Cursor_Cross, Cross);
      end if;

      Queue_Resize (MDI);
   end Realize_MDI;

   -------------
   -- Map_MDI --
   -------------

   procedure Map_MDI (MDI : access Gtk_Widget_Record'Class) is
      M : constant MDI_Window := MDI_Window (MDI);

   begin
      if M.Focus_Child /= null then
         Update_Tab_Color (Get_Notebook (M.Focus_Child), True);
      end if;
   end Map_MDI;

   -----------------
   -- Destroy_MDI --
   -----------------

   procedure Destroy_MDI (MDI : access Gtk_Widget_Record'Class) is
      M   : constant MDI_Window := MDI_Window (MDI);
      Tmp : Widget_List.Glist := First (M.Items);
      N   : Widget_List.Glist;
      C   : MDI_Child;
   begin
      Print_Debug ("Destroy_MDI");
      --  Note: we only destroy the floating children. Other children will be
      --  destroyed when their parent container is destroyed, so we have
      --  nothing to do for them.

      while Tmp /= Null_List loop
         --  Get the next field first, since Destroy will actually destroy Tmp

         C := MDI_Child (Get_Data (Tmp));

         N := Next (Tmp);
         if C.State = Floating then
            Print_Debug
              ("Destroy_MDI => Destroying floating " & Get_Title (C));
            Destroy (C);

         elsif C.State = Invisible then
            Print_Debug
              ("Destroy_MDI => Unref invisible " & Get_Title (C));
            Set_State (C, Normal);
            Unref (C);
         else
            Print_Debug
              ("Destroy_MDI => Do nothing to " & Get_Title (C));
            --  Pretend the child is not docked or floating. Otherwise,
            --  Destroy_Child would try to undock the child. Standard gtk+
            --  containers handle this by having this destroy callback called
            --  last, but it isn't doable from GtkAda since it means modifying
            --  the pointer-to-subprogram in the Class struct.
            Set_State (C, Normal);
         end if;
         Tmp := N;
      end loop;

      Free (M.Items);

      if M.Cursor_Cross /= null then
         Unref (M.Cursor_Cross);
      end if;

      Free (M.Dnd_Message);
      Free (M.Perspectives);
      Free (M.View_Contents);
      Free (M.Perspective_Names);

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

   -----------------
   -- Close_Child --
   -----------------

   procedure Close_Child
     (Child           : not null access MDI_Child_Record'Class;
      Force           : Boolean := False;
      Focus_Same_Area : Boolean := True)
   is
      MDI   : constant MDI_Window := Child.MDI;
      Event : Gdk_Event;
      Prevent_Delete : Boolean;
      Focus_Child : MDI_Child;
   begin
      --  Don't do anything for now if the MDI isn't realized, since we
      --  can't send create the event anyway.

      Print_Debug ("Close_Child, " & Get_Title (Child) & " force="
                   & Boolean'Image (Force) & " focus_same_parent="
                   & Boolean'Image (Focus_Same_Area));

      Widget_Callback.Emit_By_Name (Child, Signal_Before_Remove_Child);

      if MDI.Get_Realized then
         --  For a top-level window, we must rebuild the initial widget
         --  temporarily, so that the application can do all the test it wants.
         --  However, we need to restore the initial state before calling
         --  Dock_Child and Float_Child below.
         --  We should not test this when the MDI is being destroyed, though,
         --  to avoid memory leaks

         if Force
           or else MDI.In_Destruction
         then
            Prevent_Delete := False;
         else
            Print_Debug ("Close_Child, emitting delete_event", Debug_Increase);

            if (Child.Flags and Destroy_Button) = 0 then
               Prevent_Delete := True;
            else
               Gdk_New (Event, Delete);
               Event.Any.Window := Get_Window (Child.Initial);

               Prevent_Delete := Return_Callback.Emit_By_Name
                 (Child.Initial, "delete_event", Event);

               --  Unref'ing the event causes an unref of Event.Any.Window.
               --  Avoid this by setting this field to null before freeing the
               --  event.
               Event.Any.Window := null;

               Free (Event);
            end if;

            Print_Debug ("Close_Child, done delete_event, prevent_delete ?"
                         & Boolean'Image (Prevent_Delete),
                         Debug_Decrease);
         end if;

         if not Prevent_Delete then
            --  Transfer the focus before unfloating, so that the parent in
            --  which the child is unfloated (which might be random from the
            --  user's point of view) doesn't influence who gets the focus.
            if MDI_Child_Record (Child.all)'Unchecked_Access
              = MDI.Focus_Child
            then
               Give_Focus_To_Previous_Child
                 (Child          => Child,
                  From_Same_Area => Focus_Same_Area);
            end if;

            --  The call to Float_Child below will change the focus_child
            --  to the child we are about to destroy! Save the focus_child
            --  here...
            Focus_Child := MDI.Focus_Child;

            --  ... unfloat...
            Float_Child (Child, False);

            --- ... and restore the focus child.
            MDI.Set_Focus_Child (Focus_Child);

            Print_Debug ("Close_Child: about to destroy " & Get_Title (Child));
            Destroy (Child);
         end if;
      end if;
   end Close_Child;

   -------------------
   -- Destroy_Child --
   -------------------

   procedure Destroy_Child (Child : access Gtk_Widget_Record'Class) is
      C                   : constant MDI_Child := MDI_Child (Child);
      MDI                 : constant MDI_Window := C.MDI;
      In_Selection_Dialog : Boolean := False;

   begin
      --  We know at that stage that Child has already been unparent-ed

      pragma Assert (Get_Parent (Child) = null);

      Print_Debug ("Destroy_Child " & Get_Title (C));

      Ref (C);

      --  Do not transfer the focus elsewhere: for an interactive close, this
      --  is done in Close_Child, otherwise we do not want to change the focus.
      --  No need to send a signal to signal that a new child has been selected
      --  since Give_Focus_To_Previous_Child has been called already
      --
      --  P111-059: Focus child should be reset before any manipulation with
      --  widget tree, otherwise GPS can use it to recompute selection context.
      if C = MDI.Focus_Child then
         MDI.Focus_Child := null;
      end if;

      --  Emit the signal now, giving clients a chance to react to the
      --  imminent destruction of the child

      Emit_By_Name_Child
        (Get_Object (MDI),
         String (Signal_Before_Destroy_Child) & ASCII.NUL,
         Get_Object (C));
      Widget_Callback.Emit_By_Name (C, Signal_Before_Destroy_Child);

      C.Tab_Label := null;

      --  The child of the MDI_Child has now been taken care of, thus we need
      --  to take care of the MDI_Child itself now.

      Update_Menu_Model_List_Of_Children (MDI);

      if not C.MDI.In_Destruction then
         --  Do not unfloat the child, since the toplevel is no longer a
         --  Gtk_Window, and we would get a CE in Float_Child.

         if Get_Parent (C) /= null then
            Remove (Gtk_Container (Get_Parent (C)), C);
         end if;
      end if;

      if Get_Parent (C.Initial) /= null then
         Print_Debug ("Destroy_Child removing initial child from parent");
         Remove (Gtk_Container (Get_Parent (C.Initial)), C.Initial);
      end if;

      C.Initial := null;

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
        (Get_Object (MDI),
         String (Signal_Child_Removed) & ASCII.NUL, Get_Object (C));

      --  If we are currently displaying the window selection dialog, update it
      --  so that the widget that has been destroyed does not show up in the
      --  selection window.
      if In_Selection_Dialog then
         Update_Selection_Dialog (MDI, +1);
      end if;

      Unref (C.Title_Icon);
      Free (C.Icon_Name);
      Free (C.Title);
      Free (C.Short_Title);
      Free (C.XML_Node_Name);

      if C.State = Invisible then
         --  We owned an extra reference in this case
         Unref (C);
      end if;

      --  Destroy the child, unless the user has explicitely kept a Ref on it
      --  (therefore, do not use Destroy, only Unref). In all cases, it should
      --  be hidden on the screen
      Unref (C);
   end Destroy_Child;

   ---------------------------
   -- Destroy_Initial_Child --
   ---------------------------

   procedure Destroy_Initial_Child
      (Child : access Gtk_Widget_Record'Class) is
   begin
      if not Child.In_Destruction then
         Destroy (Child);
      end if;
   end Destroy_Initial_Child;

   -------------------------
   -- Set_Child_Title_Bar --
   -------------------------

   procedure Set_Child_Title_Bar (Child : access MDI_Child_Record'Class) is
      Event : constant Gtk_Event_Box :=
        Gtk_Event_Box (Get_Parent (Child.Title_Box));
   begin
      if not Has_Title_Bar (Child) then
         Hide (Event);
         Set_Child_Visible (Event, False);
         Set_Size_Request (Event, -1, 0);

      else
         Show (Event);
         Set_Child_Visible (Event, True);
         Set_Size_Request (Event, -1, Child.MDI.Title_Bar_Height);
      end if;
   end Set_Child_Title_Bar;

   ---------------------
   -- Set_Dnd_Message --
   ---------------------

   procedure Set_Dnd_Message
     (MDI     : access MDI_Window_Record;
      Message : String) is
   begin
      Free (MDI.Dnd_Message);

      if Message /= "" then
         MDI.Dnd_Message := new String'(Message);
      else
         MDI.Dnd_Message := new String'
           ("<i><b>control</b> moves whole notebook"
            & ASCII.LF
            & "<b>shift</b> creates new view</i>");
      end if;
   end Set_Dnd_Message;

   -----------------------
   -- Get_Allowed_Areas --
   -----------------------

   function Get_Allowed_Areas
     (Child : not null access MDI_Child_Record'Class) return Allowed_Areas is
   begin
      return Child.Areas;
   end Get_Allowed_Areas;

   -------------------------
   -- Get_Tab_Orientation --
   -------------------------

   function Get_Tab_Orientation
     (Child : not null access MDI_Child_Record'Class)
      return Tab_Orientation_Type
   is
      Angle : constant Gdouble := Child.Tab_Label.Get_Angle;
   begin
      if Angle = 0.0 then
         return Horizontal;
      elsif Angle = 90.0 then
         return Bottom_To_Top;
      else
         return Top_To_Bottom;
      end if;
   end Get_Tab_Orientation;

   ----------------------------------
   -- Button_Pressed_On_Title_Icon --
   ----------------------------------

   function Button_Pressed_On_Title_Icon
     (Child : access GObject_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
   begin
      if Event.The_Type = Gdk_2button_Press
        and then Event.Button = 1
      then
         Close_Child (MDI_Child (Child));
         return False;
      end if;

      return Button_Pressed_Forced (Child, Event);
   end Button_Pressed_On_Title_Icon;

   --------------------
   -- Button_Pressed --
   --------------------

   function Button_Pressed
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean is
   begin
      --  It sometimes happens that widgets let events pass through (for
      --  instance scrollbars do that), and thus wouldn't be useable anymore
      --  if we do a grab.
      --  ??? The comment above was kept for reference. We used to check that
      --  Event.Window was Child.Get_Window, but this prevents dragging from
      --  the title bar.

      if Event.Window /= Child.Get_Window then
         return False;
      end if;

      return Button_Pressed_Forced (Child, Event);
   end Button_Pressed;

   ---------------------------
   -- Button_Pressed_Forced --
   ---------------------------

   function Button_Pressed_Forced
     (Child : access GObject_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      C    : constant MDI_Child := MDI_Child (Child);
   begin
      C.MDI.In_Drag := No_Drag;

      if Event.The_Type /= Button_Press or else Event.Button /= 1 then
         return False;
      end if;

      --  Start a drag-and-drop operation. This won't be effective unless
      --  the user actually drags the mouse a while

      Print_Debug ("Button_Pressed_Forced");
      Child_Drag_Begin (C, Event, C.Areas);

      --  Let the event through, the drag hasn't started yet
      return False;
   end Button_Pressed_Forced;

   --------------------
   -- Button_Release --
   --------------------

   function Button_Release
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      C : constant MDI_Child := MDI_Child (Child);
   begin
      if Get_Window (Child) /= Event.Window then
         C.MDI.In_Drag := No_Drag;
         Pointer_Ungrab (Time => 0);
         return False;
      end if;

      return Button_Release_Forced (Child, Event);
   end Button_Release;

   ---------------------------
   -- Button_Release_Forced --
   ---------------------------

   function Button_Release_Forced
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button;
      Allow_Move : Boolean := True) return Boolean
   is
      C                    : constant MDI_Child := MDI_Child (Child);
      MDI                  : constant MDI_Window := C.MDI;
      Move_Whole_Notebook  : constant Boolean :=
                               (Event.State and Control_Mask) /= 0;
      Copy_Instead_Of_Move : constant Boolean :=
                               (Event.State and Shift_Mask) /= 0;
      C2                   : MDI_Child;
      Current              : Gtk_Widget;
      Note                 : MDI_Notebook;
      Position             : Child_Position;
      Pane                 : Gtkada_Multi_Paned;
      Parent_Rect          : Gdk_Rectangle;
      Allowed              : Boolean;
      In_Central           : Boolean;
   begin
      Print_Debug
        ("Button release, drag=" & Drag_Status'Image (C.MDI.In_Drag));

      Pointer_Ungrab (Time => 0);

      case C.MDI.In_Drag is
         when In_Pre_Drag =>
            Gtkada.Style.Delete_Overlay (C.MDI, C.MDI.Dnd_Overlay);
            Child_Drag_Finished (C);

         when In_Drag =>
            if C.MDI.Central /= null then
               Set_Border_Width (C.MDI.Central, 0);
            end if;

            Gtkada.Style.Delete_Overlay (C.MDI, C.MDI.Dnd_Overlay);

            if Allow_Move then
               Get_Dnd_Target
                 (C.MDI, Current, Position,
                  Parent_Rect, C.MDI.Dnd_Rectangle,
                  In_Central => In_Central,
                  Allowed    => Allowed);
            else
               Allowed := False;
            end if;

            if not Allowed then
               Child_Drag_Finished (C);
               C.MDI.In_Drag := No_Drag;
               return True;
            end if;

            if Current = null then --  outside of the main window ?
               Pane := null;
            elsif Current = Gtk_Widget (C.MDI) then
               Pane := Gtkada_Multi_Paned (C.MDI);
            elsif Get_Parent (Current) = Gtk_Widget (C.MDI.Central) then
               Pane := C.MDI.Central;
            else
               Pane := Gtkada_Multi_Paned (C.MDI);
            end if;

            C2 := Dnd_Data (C, Copy => Copy_Instead_Of_Move);
            if C2 = null then
               C2 := C;
            end if;

            if Current = null then
               --  Floating child ?
               Float_Child (C2, True);

            --  If the child is dropped at the same location, nothing to do

            elsif C2.State = Normal  --  A floating child is always moved
              and then Current = Get_Parent (C2)  --  same notebook ?
              and then
                (Position = Position_Automatic  --  inside the nook
                 or else Move_Whole_Notebook  --  to one side but moving all
                 or else Get_Nth_Page (Gtk_Notebook (Current), 1) = null)
            then
               null;

            --  Do the actual moving

            else
               --  In the notebook that contains the window we are moving, we
               --  now raise the last window that had the focus

               declare
                  Item : Widget_List.Glist := MDI.Items;
                  It   : MDI_Child;
               begin
                  if C /= C2 then
                     Print_Debug ("Button_Release raising last1 "
                                  & Get_Title (C));
                     Raise_Child (C, False);
                  else
                     while Item /= Widget_List.Null_List loop
                        It := MDI_Child (Get_Data (Item));
                        if It /= C2
                          and then Get_Parent (C2) = Get_Parent (It)
                        then
                           Print_Debug
                             ("Button_Release raising last2 "
                              & Get_Title (It));
                           Raise_Child (It, False);
                           exit;
                        end if;

                        Item := Widget_List.Next (Item);
                     end loop;
                  end if;
               end;

               --  Find in which notebook the widget should be moved.

               if Current = Gtk_Widget (C.MDI.Central)
                 or else Current = Gtk_Widget (C.MDI)
               then
                  --  The central area is empty if Current has this value, we
                  --  always create a new notebook
                  Note := Create_Notebook (MDI);

                  if Current = Gtk_Widget (C.MDI) then
                     Current := null;
                  end if;

               else
                  --  We dropped in a notebook, should we reuse or create one ?
                  if Position = Position_Automatic then
                     Note := MDI_Notebook (Current);
                  else
                     Note := Create_Notebook (MDI);
                  end if;
               end if;

               --  Add to the contents of this notebook

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
                     if Current = null then
                        Split
                          (Pane,
                           Root_Pane, Note, Orientation_Vertical,
                           Height => -1);
                     else
                        Split (Pane, Current, Note, Orientation_Vertical);
                     end if;

                  when Position_Top =>
                     if Current = null then
                        Split
                          (Pane,
                           Root_Pane, Note, Orientation_Vertical,
                           Height => -1, After  => False);
                     else
                        Split
                          (Pane,
                           Current, Note, Orientation_Vertical,
                           After  => False);
                     end if;
                  when Position_Left =>
                     if Current = null then
                        Split
                          (Pane,
                           Root_Pane, Note, Orientation_Horizontal,
                           Width => -1, After  => False);
                     else
                        Split
                          (Pane,
                           Current, Note, Orientation_Horizontal,
                           After  => False);
                     end if;

                  when Position_Right =>
                     if Current = null then
                        Split
                          (Pane,
                           Root_Pane, Note, Orientation_Horizontal,
                           Width => -1);
                     else
                        Split (Pane, Current, Note, Orientation_Horizontal);
                     end if;

                  when Position_Automatic | Position_Float =>
                     if C.MDI.Central /= null
                        and then Current = Gtk_Widget (C.MDI.Central)
                     then
                        Add_Child
                          (Win         => C.MDI.Central,
                           New_Child   => Note,
                           Orientation => Orientation_Horizontal,
                           Width       => 0,
                           Height      => 0);
                     end if;
               end case;

               Emit_By_Name
                 (Get_Object (MDI),
                  String (Signal_Children_Reorganized)
                  & ASCII.NUL);
            end if;

            Child_Drag_Finished (C);

            Print_Debug ("Button_Release raising " & Get_Title (C2));
            Raise_Child (C2, False);
            Print_Debug ("Button_Release, set_focus " & Get_Title (C2));
            Set_Focus_Child (C2);

            --  For a focus manually, because Set_Focus_Child above might not
            --  have had any effect (the selected window is still the same,
            --  although in some cases gtk+ has transfered the keyboard focus
            --  elsewhere).
            Give_Focus_To_Child (C2);

            MDI.Dnd_Target := null;

         when No_Drag =>
            --  Let the even through, we have nothing to do here
            return False;
      end case;

      C.MDI.In_Drag := No_Drag;
      return True;
   end Button_Release_Forced;

   -----------------------------
   -- Button_Release_Notebook --
   -----------------------------

   function Button_Release_Notebook
     (Note : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      N : constant MDI_Notebook := MDI_Notebook (Note);
      Child : constant MDI_Child :=
        MDI_Child (Get_Nth_Page (N, Get_Current_Page (N)));
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      if Child.MDI.In_Drag = In_Drag
        or else Child.MDI.In_Drag = In_Pre_Drag
      then
         Tmp := Button_Release_Forced
           (Child, Event,
            Allow_Move => not Hover_Tabs (N, Event.X_Root, Event.Y_Root));

         --  Fallback to standard gtk+ handler, so that it terminates its
         --  reorder operation if any
         --  Unfortunately, the notebook might no longer exist at this point,
         --  so it is dangerous to forward.

         if Child.Get_State = Floating then
            return True;  --  stop propagating
         end if;
      end if;

      return False;
   end Button_Release_Notebook;

   -------------------
   -- Button_Motion --
   -------------------

   function Button_Motion
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean
   is
   begin
      if Get_Window (Child) /= Event.Window then
         return False;
      end if;

      return Button_Motion_Forced (Child, Event);
   end Button_Motion;

   --------------------------
   -- Button_Motion_Forced --
   --------------------------

   function Button_Motion_Forced
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean
   is
      C        : constant MDI_Child := MDI_Child (Child);
      Note     : MDI_Notebook;
      Tmp      : Gdk_Grab_Status;
      Delta_X, Delta_Y : Gint;
      pragma Unreferenced (Tmp);

   begin
      --  This is no longer called once the GtkNotebook has started reordering
      --  the tab, because it creates a transparent overlay window that grabs
      --  all the events (so the motion is sent to the GtkNotebook, no longer
      --  to the child).

      case C.MDI.In_Drag is
         when In_Drag =>
            Draw_Dnd_Rectangle (C);
            return True;

         when In_Pre_Drag =>
            --  If we are still in the tabs area, do nothing so that tabs can
            --  be reordered graphically

            Delta_X := abs (Gint (Event.X_Root) - C.MDI.Drag_Start_X);
            Delta_Y := abs (Gint (Event.Y_Root) - C.MDI.Drag_Start_Y);

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
   end Button_Motion_Forced;

   ----------------
   -- Hover_Tabs --
   ----------------

   function Hover_Tabs
     (N : MDI_Notebook;
      X_Root, Y_Root : Gdouble) return Boolean
   is
      In_Tabs_Area          : Boolean;
      X_Win, Y_Win          : Gint;
      W, H                  : Gint;
      Mouse_X, Mouse_Y      : Gint;
      Alloc                 : Gtk_Allocation;
      Tab_Label             : Gtk_Widget;
      Tab_Width, Tab_Height : Gint;
   begin
      Get_Origin (N.Get_Window, X_Win, Y_Win);
      N.Get_Allocation (Alloc);
      Mouse_X := Gint (X_Root) - X_Win - Alloc.X;
      Mouse_Y := Gint (Y_Root) - Y_Win - Alloc.Y;

      W := N.Get_Allocated_Width;
      H := N.Get_Allocated_Height;
      Tab_Label := N.Get_Tab_Label (N.Get_Nth_Page (0));

      case N.Actual_Tab_Orientation is
         when Horizontal | Automatic =>   --  'Automatic' is never set
            Tab_Width  := Tab_Label.Get_Allocated_Width;
            Tab_Height := 30;   --   ??? hard-coded
         when Bottom_To_Top | Top_To_Bottom =>
            Tab_Width := 30;    --    ??? hard-coded
            Tab_Height := Tab_Label.Get_Allocated_Height;
      end case;

      case N.Get_Tab_Pos is
         when Pos_Top =>
            In_Tabs_Area :=
              (Mouse_X >= 0 and then Mouse_X <= W
               and then Mouse_Y >= 0 and then Mouse_Y <= Tab_Height);
         when Pos_Bottom =>
            In_Tabs_Area :=
              (Mouse_X >= 0 and then Mouse_X <= W
               and then Mouse_Y >= H - Tab_Height
               and then Mouse_Y <= H);
         when Pos_Left =>
            In_Tabs_Area :=
              (Mouse_X >= 0 and then Mouse_X <= Tab_Width
               and then Mouse_Y >= 0 and then Mouse_Y <= H);
         when Pos_Right =>
            In_Tabs_Area :=
              (Mouse_X >= W - Tab_Width and then Mouse_X <= W
               and then Mouse_Y >= 0 and then Mouse_Y <= H);
      end case;

      return In_Tabs_Area;
   end Hover_Tabs;

   ----------------------------
   -- Button_Motion_Notebook --
   ----------------------------

   function Button_Motion_Notebook
     (Note : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean
   is
      N : constant MDI_Notebook := MDI_Notebook (Note);
      Child : constant MDI_Child :=
        MDI_Child (Get_Nth_Page (N, Get_Current_Page (N)));
      In_Tabs_Area          : Boolean;
   begin
      if Child /= null
        and then (Child.MDI.In_Drag = In_Drag
                  or else Child.MDI.In_Drag = In_Pre_Drag)
      then
         In_Tabs_Area := Hover_Tabs (N, Event.X_Root, Event.Y_Root);

         if not In_Tabs_Area then
            Child.MDI.In_Drag := In_Drag;
            return Button_Motion_Forced (Child, Event);
         end if;

         if Child.MDI.In_Drag = In_Drag then
            --  Remove the previous highlighting, since we are back to
            --  reordering tabs
            Draw_Dnd_Rectangle (Child, Hide_Only => True);
         end if;
      end if;

      return False;
   end Button_Motion_Notebook;

   --------------
   -- Dnd_Data --
   --------------

   function Dnd_Data
     (Child : not null access MDI_Child_Record; Copy : Boolean)
      return MDI_Child
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
      Note  : MDI_Notebook;
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
      Note  : MDI_Notebook;
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

   --------------------
   -- Child_Get_Type --
   --------------------

   function Child_Get_Type return Glib.GType is
      Signal_Parameters : constant Glib.Object.Signal_Parameter_Types
        (Integer (Child_Signals'First) .. Integer (Child_Signals'Last),
         1 .. 1) := (others => (1 => GType_None));
   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Event_Box.Get_Type,
         Signals      => Child_Signals,
         Class_Record => Child_Class_Record,
         Type_Name    => "GtkAdaMDIChild",
         Parameters   => Signal_Parameters);
      return Child_Class_Record.The_Type;
   end Child_Get_Type;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child        : out MDI_Child;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags        : Child_Flags := All_Buttons;
      Group        : Child_Group := Group_Default;
      Focus_Widget : Gtk.Widget.Gtk_Widget := null;
      Areas        : Allowed_Areas := Both) is
   begin
      Child := new MDI_Child_Record;
      Initialize (Child, Widget, Flags, Group, Focus_Widget, Areas);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Child        : access MDI_Child_Record'Class;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags        : Child_Flags := All_Buttons;
      Group        : Child_Group := Group_Default;
      Focus_Widget : Gtk.Widget.Gtk_Widget := null;
      Areas        : Allowed_Areas := Both)
   is
      Event             : Gtk_Event_Box;
      Button            : Close_Button.Gtkada_MDI_Close_Button;

   begin
      if Widget.all in Gtk_Window_Record'Class then
         raise Program_Error;
      end if;

      G_New (Child, Child_Get_Type);

      Child.Set_Border_Width (0);
      Get_Style_Context (Child).Add_Class ("mdichild");
      Child.Initial      := Gtk_Widget (Widget);
      Child.State        := Normal;
      Child.Flags        := Flags;
      Child.Group        := Group;
      Child.Focus_Widget := Focus_Widget;
      Child.MDI          := null;
      Child.Areas        := Areas;

      Add_Events
        (Child, Button_Press_Mask
           or Button_Motion_Mask
           or Button_Release_Mask
           or Pointer_Motion_Mask);

      Child.On_Button_Press_Event (Button_Pressed'Access);
      Child.On_Button_Release_Event (Button_Release'Access);
      Child.On_Motion_Notify_Event (Button_Motion'Access);
      Widget_Callback.Connect
        (Child, Signal_Destroy,
         Widget_Callback.To_Marshaller (Destroy_Child'Access));

      Gtk_New_Vbox (Child.Main_Box, Homogeneous => False, Spacing => 0);
      Add (Child, Child.Main_Box);

      --  Buttons in the title bar

      Gtk_New (Event);
      Get_Style_Context (Event).Add_Class ("mdititle");
      Pack_Start
        (Child.Main_Box, Event, Expand => False, Fill => False);
      Gtk_New_Hbox (Child.Title_Box, Homogeneous => False);
      Add (Event, Child.Title_Box);
      Event.On_Button_Press_Event (Button_Pressed_Forced'Access, Child);

      Gtk_New (Event);
      Gtk_New (Child.Title_Icon);
      Ref (Child.Title_Icon);  --  floating a child should not destroy icon
      Event.Add (Child.Title_Icon);
      Child.Title_Box.Pack_Start (Event, Expand => False);
      Event.On_Button_Press_Event
        (Button_Pressed_On_Title_Icon'Access, Child);

      Gtk_New (Child.Title_Label);
      Child.Title_Box.Pack_Start
        (Child.Title_Label, Expand => True, Fill => True);
      Child.Title_Label.Set_Ellipsize (Pango.Layout.Ellipsize_Start);
      Child.Title_Label.Set_Alignment (0.0, 0.5);
      Child.Title_Label.Set_Padding (5, 0);

      --  Do not set the label as selectable, otherwise it has its own window,
      --  and we can no longer drag from the title
      Child.Title_Label.Set_Selectable (False);

      if (Flags and Destroy_Button) /= 0 then
         Close_Button.Gtk_New
           (Button, Child, Child,
            Position    => Pos_Top,
            In_Titlebar => True);
         Pack_End
           (Child.Title_Box,
            Button, Expand => False, Fill => False, Padding => 2);
      end if;

      --  This internal Event box is needed when the child is floated
      Gtk_New (Event);
      Add (Event, Widget);
      Pack_Start
        (Child.Main_Box, Event, Expand => True, Fill => True, Padding => 0);

      Child.Set_Title (" ");

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
      Widget_Callback.Connect
        (Child, Signal_Maximize_Child,
         Maximize_Action.On_Toggle_Maximize'Access);
      Widget_Callback.Connect
        (Child, Signal_Unmaximize,
         Maximize_Action.On_Toggle_Maximize'Access);
      Widget_Callback.Connect
        (Child, Signal_Before_Remove_Child,
         Maximize_Action.On_Remove_Child'Access);
   end Initialize;

   ------------------
   -- Change_Group --
   ------------------

   procedure Change_Group
     (Child : not null access MDI_Child_Record'Class;
      Group : Child_Group) is
   begin
      Child.Group := Group;
   end Change_Group;

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
     (Child          : access MDI_Child_Record'Class;
      From_Same_Area : Boolean := True)
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

         if It.Get_Visible
           and then Child /= null
           and then It /= MDI_Child_Record (Child.all)'Unchecked_Access
         then
            if Last = null then
               Last := It;
            end if;

            if not From_Same_Area
              or else (It.State = Child.State
                       and then  Get_Parent (It) = Get_Parent (Child))
            then
               Print_Debug ("Give_Focus_To_Previous_Child "
                            & Get_Title (It));
               Set_Focus_Child (It);
               return;
            end if;
         end if;

         Item := Widget_List.Next (Item);
      end loop;

      --  No such child, give it to the last child that had the focus
      if Last = null then
         Print_Debug ("Give_Focus_To_Previous_Child: no one");
         Child.MDI.Focus_Child := null;
         Child_Selected (Child.MDI, null);
      else
         Set_Focus_Child (Last);
      end if;
   end Give_Focus_To_Previous_Child;

   --------------------
   -- Child_Selected --
   --------------------

   procedure Child_Selected
      (Self  : not null access MDI_Window_Record'Class;
       Child : access MDI_Child_Record'Class := null) is
   begin
      Emit_By_Name_Child
        (Get_Object (Self),
         String (Signal_Child_Selected) & ASCII.NUL,
         Get_Object_Or_Null (GObject (Child)));
   end Child_Selected;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Self : not null access MDI_Child_Record) return Glib.Xml_Int.Node_Ptr
   is
      pragma Unreferenced (Self);
   begin
      return null;
   end Save_Desktop;

   ---------
   -- Put --
   ---------

   procedure Put
     (MDI              : access MDI_Window_Record;
      Child            : access MDI_Child_Record'Class;
      Initial_Position : Child_Position := Position_Automatic;
      Position_At_Mouse : Boolean := True;
      X, Y              : Gint := 0)
   is
      Float : constant Boolean :=
         MDI.All_Floating_Mode
         or else (not MDI.Loading_Desktop
                  and then Initial_Position = Position_Float);
   begin
      Child.MDI := MDI_Window (MDI);

      --  We need to show the widget before inserting it in a notebook,
      --  otherwise the notebook page will not be made visible.

      Ref (Child);

      Show_All (Child);

      if Child.State = Invisible then
         Unref (Child);  --  Set in Remove_All_Items
         return;
      end if;

      Set_State (Child, Normal);

      Float_Child (Child, Float, Position_At_Mouse, X, Y);

      if not Float then
         Put_In_Notebook (MDI, Child, Initial_Position => Initial_Position);
      end if;

      Set_Child_Title_Bar (Child);

      --  Add the child to the list of widgets. It could in fact already be in
      --  the list if we are reusing a Invisible child from a previous
      --  perspective. We however want to move it to the front of the list

      Remove (MDI.Items, Gtk_Widget (Child));
      Prepend (MDI.Items, Gtk_Widget (Child));
      Unref (Child);

      Update_Menu_Model_List_Of_Children (MDI);

      --  Restore the keyboard focus, which might have been stolen if the new
      --  child was added to a notebook.

      Give_Focus_To_Child (MDI.Focus_Child);

      Emit_By_Name_Child
        (Get_Object (MDI),
         String (Signal_Child_Added) & ASCII.NUL, Get_Object (Child));
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
      Notebook : constant MDI_Notebook := Get_Notebook (Child);
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
            Set_Size (MDI,
                      Widget     => Notebook,
                      Width      => Width,
                      Height     => Height
                      + Get_Allocated_Height (Notebook)
                      - Get_Allocated_Height (Child),
                      Fixed_Size => Fixed_Size);
         end if;
      end if;
   end Set_Size;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
     (Child : not null access MDI_Child_Record) return UTF8_String is
   begin
      if Child.Title = null then
         return "";
      else
         return Child.Title.all;
      end if;
   end Get_Title;

   ---------------------
   -- Get_Short_Title --
   ---------------------

   function Get_Short_Title
     (Child : not null access MDI_Child_Record) return UTF8_String is
   begin
      return Child.Short_Title.all;
   end Get_Short_Title;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
     (Child : not null access MDI_Child_Record;
      Icon  : Gdk.Pixbuf.Gdk_Pixbuf) is
   begin
      Free (Child.Icon_Name);
      Child.Title_Icon.Set (Icon);
      if Icon /= null then
         Child.Title_Icon.Show;
      else
         Child.Title_Icon.Hide;
      end if;

      if Child.Tab_Icon /= null then
         Child.Tab_Icon.Set (Icon);
         if Icon /= null then
            Child.Tab_Icon.Show;
         else
            Child.Tab_Icon.Hide;
         end if;
      end if;

      Update_Menu_Model_List_Of_Children (Child.MDI);
      Emit_By_Name_Child
        (Get_Object (Child.MDI),
         String (Signal_Child_Icon_Changed) & ASCII.NUL,
         Get_Object (Child));
   end Set_Icon;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name
     (Child     : not null access MDI_Child_Record;
      Icon_Name : String)
   is
      Changed : constant Boolean :=
        (Child.Icon_Name = null or else
         Child.Icon_Name.all /= Icon_Name);
   begin
      Free (Child.Icon_Name);
      Child.Icon_Name := new String'(Icon_Name);

      if Icon_Name /= "" then
         Child.Title_Icon.Show;
         Child.Title_Icon.Set_From_Icon_Name (Icon_Name, Icon_Size_In_Title);
      else
         Child.Title_Icon.Hide;
      end if;

      if Child.Tab_Icon /= null then
         if Icon_Name /= "" then
            Child.Tab_Icon.Show;
            Child.Tab_Icon.Set_From_Icon_Name (Icon_Name, Icon_Size_In_Tabs);
         else
            Child.Tab_Icon.Hide;
         end if;
      end if;

      Update_Menu_Model_List_Of_Children (Child.MDI);

      if Changed then
         Emit_By_Name_Child
           (Get_Object (Child.MDI),
            String (Signal_Child_Icon_Changed) & ASCII.NUL,
            Get_Object (Child));
      end if;
   end Set_Icon_Name;

   --------------
   -- Get_Icon --
   --------------

   function Get_Icon
     (Child : not null access MDI_Child_Record) return Gdk.Pixbuf.Gdk_Pixbuf
   is
   begin
      return Child.Title_Icon.Get;
   end Get_Icon;

   --------------
   -- Get_Icon --
   --------------

   function Get_Icon
     (Child : not null access MDI_Child_Record'Class)
      return Gtk_Image
   is
      Result : Gtk_Image;

      Pixbuf : Gdk_Pixbuf;
      G_Icon : Glib.G_Icon.G_Icon;
      Size   : Gtk.Enums.Gtk_Icon_Size;

   begin
      Pixbuf := Child.Title_Icon.Get;   --  still owned by the image
      if Pixbuf /= null then
         Gtk_New (Result, Pixbuf);

      else
         Child.Title_Icon.Get (G_Icon, Size);
         if G_Icon /= Null_G_Icon then
            Result := Gtk_Image_New_From_Gicon (G_Icon, Size);
         end if;
      end if;

      return Result;
   end Get_Icon;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
     (Child : not null access MDI_Child_Record) return String
   is
   begin
      if Child.Icon_Name = null then
         return "";
      else
         return Child.Icon_Name.all;
      end if;
   end Get_Icon_Name;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Child       : not null access MDI_Child_Record;
      Title       : String;
      Short_Title : String := "")
   is
      function To_UTF8 (Str : String) return String;
      --  Ensure the output string is valid UTF8

      function To_UTF8 (Str : String) return String is
         Valid : Boolean;
         Invalid_Pos : Natural;
      begin
         UTF8_Validate (Str, Valid, Invalid_Pos);

         if Valid then
            return Str;
         else
            declare
               S : constant String := Glib.Convert.Filename_To_UTF8 (Str);
            begin
               if S /= "" then
                  return S;
               end if;
            end;

            declare
               S : constant String := Glib.Convert.Locale_To_UTF8 (Str);
            begin
               if S /= "" then
                  return S;
               end if;
            end;

            return Str (Str'First .. Str'First + Invalid_Pos);
         end if;
      end To_UTF8;

      T : constant String := To_UTF8 (Title);
      S : constant String := To_UTF8 (Short_Title);

      Title_Changed       : constant Boolean := Child.Title = null
                              or else Child.Title.all /= T;
      Short_Title_Changed : constant Boolean := Child.Short_Title = null
                              or else Child.Short_Title.all /= S;

   begin
      if Title_Changed then
         Free (Child.Title);
         Child.Title := new UTF8_String'(T);
      end if;

      if Short_Title_Changed then
         Free (Child.Short_Title);
         if S /= "" then
            Child.Short_Title := new UTF8_String'(S);
         else
            Child.Short_Title := new UTF8_String'(T);
         end if;
      end if;

      if Child.MDI /= null
        and then Child.MDI.Use_Short_Titles_For_Floats
      then
         Child.Title_Label.Set_Text (Child.Short_Title.all);
      else
         Child.Title_Label.Set_Text (Child.Title.all);
      end if;

      if Title_Changed and then Child.State = Floating then
         if Child.MDI.Use_Short_Titles_For_Floats then
            Set_Title
              (Gtk_Window (Get_Toplevel (Child.Initial)),
               Locale_From_UTF8 (Child.Short_Title.all));
         else
            Set_Title
              (Gtk_Window (Get_Toplevel (Child.Initial)),
               Locale_From_UTF8 (Child.Title.all));
         end if;
      end if;

      if Short_Title_Changed then
         --  Update the menu, if it exists. We need to recreate the menu item
         --  to keep it sorted

         if Child.MDI /= null then
            Update_Menu_Model_List_Of_Children (Child.MDI);
         end if;
      end if;

      if Title_Changed or else Short_Title_Changed then
         Update_Tab_Label (Child);   --  this also updated the tooltip
         Child.Queue_Draw;
         if Child.MDI /= null then
            Emit_By_Name_Child
              (Get_Object (Child.MDI),
               String (Signal_Child_Title_Changed) & ASCII.NUL,
               Get_Object (Child));
         end if;
      end if;
   end Set_Title;

   ----------------------------
   -- Insert_Child_If_Needed --
   ----------------------------

   function Insert_Child_If_Needed
     (MDI   : access MDI_Window_Record'Class;
      Child : MDI_Child) return MDI_Child is
   begin
      if Child /= null and then Child.State = Invisible then
         Set_State (Child, Normal);
         Put (MDI, Child);

         --  When a child is made invisible, it is Ref'ed - we need to Unref to
         --  remove this reference, and we need to do this after the Put so
         --  that the child doesn't get destroyed.
         Unref (Child);
      end if;

      return Child;
   end Insert_Child_If_Needed;

   --------------------
   -- Find_MDI_Child --
   --------------------

   function Find_MDI_Child
     (MDI    : access MDI_Window_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return MDI_Child
   is
      Tmp : Widget_List.Glist;
      Child_Widget : constant Gtk_Widget :=
        (if Widget /= null then
           Gtk_Widget_Record (Widget.all)'Unchecked_Access
         else
           null);
   begin
      Tmp := First (MDI.Items);

      while Tmp /= Null_List loop
         if MDI_Child (Get_Data (Tmp)).Initial = Child_Widget then
            return Insert_Child_If_Needed (MDI, MDI_Child (Get_Data (Tmp)));
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
      Win : Gtk_Widget;
      C   : MDI_Child;
   begin
      --  As a special case, if the widget's parent is a notebook, we check
      --  whether the associated page is a MDI child, and behave as if that
      --  child had the focus (EC19-008)

      while W /= null loop
         if W.all in MDI_Child_Record'Class then
            return Insert_Child_If_Needed (MDI_Child (W).MDI, MDI_Child (W));

         elsif W.all in Gtk_Notebook_Record'Class
           and then Get_Nth_Page
             (Gtk_Notebook (W), Get_Current_Page (Gtk_Notebook (W))).all
              in MDI_Child_Record'Class
         then
            C := MDI_Child
              (Get_Nth_Page
                 (Gtk_Notebook (W), Get_Current_Page (Gtk_Notebook (W))));
            return Insert_Child_If_Needed (C.MDI, C);
         end if;

         W := Get_Parent (W);
      end loop;

      --  Not found ? We might have a floating window. Unfortunately, these
      --  windows do not keep track of the MDI child they belong to...

      Win := Get_Toplevel (Widget);
      if Win /= null
         and then Child_User_Data.Is_Set (Win, "parent_mdi_child")
      then
         --  The call to Get will never raise a Data_Error exception
         C := Child_User_Data.Get (Win, "parent_mdi_child");
         return Insert_Child_If_Needed (C.MDI, C);
      end if;

      return null;
   end Find_MDI_Child_From_Widget;

   ---------------------------
   -- Find_MDI_Child_By_Tag --
   ---------------------------

   function Find_MDI_Child_By_Tag
     (MDI : access MDI_Window_Record;
      Tag : Ada.Tags.Tag;
      Visible_Only : Boolean := False) return MDI_Child
   is
      Child : MDI_Child;
      Iter  : Child_Iterator :=
        First_Child (MDI, Visible_Only => Visible_Only);
   begin
      loop
         Child := Get (Iter);
         exit when Child = null or else Child.Initial'Tag = Tag;
         Next (Iter);
      end loop;

      if Child /= null then
         return Insert_Child_If_Needed (MDI, Child);
      else
         return null;
      end if;
   end Find_MDI_Child_By_Tag;

   ----------------------------
   -- Find_MDI_Child_By_Name --
   ----------------------------

   function Find_MDI_Child_By_Name
     (MDI  : access MDI_Window_Record;
      Name : String) return MDI_Child
   is
      Child : MDI_Child;
      Iter  : Child_Iterator := First_Child (MDI, Visible_Only => False);
   begin
      loop
         Child := Get (Iter);
         exit when Child = null
           or else Child.Title.all = Name
           or else Child.Short_Title.all = Name;
         Next (Iter);
      end loop;

      return Insert_Child_If_Needed (MDI, Get (Iter));
   end Find_MDI_Child_By_Name;

   -----------------
   -- Lower_Child --
   -----------------

   procedure Lower_Child (Child : not null access MDI_Child_Record'Class) is
      Note : MDI_Notebook;
   begin
      Ref (Child);
      Remove (Child.MDI.Items, Gtk_Widget (Child));
      Append (Child.MDI.Items, Gtk_Widget (Child));
      Unref (Child);

      if Child.State = Normal then
         Note := Get_Notebook (Child);
         Set_Current_Page (Note, Page_Num (Note, Child));

      elsif Child.Get_Realized then
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

   function Is_Raised
     (Child : not null access MDI_Child_Record'Class) return Boolean
   is
      Note : MDI_Notebook;
   begin
      case Child.State is
         when Floating =>
            return True;
         when Invisible =>
            return False;
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
     (Child      : not null access MDI_Child_Record'Class;
      Give_Focus : Boolean := True)
   is
      Old_Focus     : constant MDI_Child := Child.MDI.Focus_Child;
      Note          : MDI_Notebook;
      Give          : Boolean := Give_Focus;
   begin
      Show (Child);  --  Make sure the child is visible

      if Child.State = Invisible then
         --  child was in another desktop, not visible in the current one

         Put_In_Notebook (Child.MDI, Child);
      end if;

      --  For a docked item, we in fact want to raise its parent dock,
      --  and make sure the current page in that dock is the correct one.

      if Child.State = Normal then
         Note := Get_Notebook (Child);

         --  We'll have to transfer the focus if the current focus window is in
         --  the same dock, since otherwise that means an invisible window
         --  would have the focus.

         if Old_Focus /= null
           and then Old_Focus.State = Normal
           and then Get_Notebook (Old_Focus) = Note
         then
            Give := True;
         end if;

         --  Temporary fool the system, so that the child doesn't necessarily
         --  gain the focus. Otherwise, switching a notebook page gives the
         --  child the focus.
         Child.MDI.Focus_Child :=
           MDI_Child_Record (Child.all)'Unchecked_Access;

         --  There could be no parent if we are in all-floating mode
         if Note /= null then
            Set_Current_Page (Note, Page_Num (Note, Child));
         end if;

         Child.MDI.Focus_Child := Old_Focus;

         if Give_Focus and then Child.Initial.Get_Realized then
            Present (Gtk_Window (Get_Toplevel (Child.Initial)));
         end if;

      elsif Child.State = Floating
        and then Give_Focus
        and then Child.Initial.Get_Realized
      then
         --  When raising a floating child that has the Float_As_Transient
         --  flag set to True, set it as transient for the current window
         --  (i.e: the currently focused child's window if any or the MDI's
         --  main window).
         --  This ensures that the floating child will always be on top of
         --  the currently focused window, which is what we want in the
         --  Float_As_Transient mode.

         if Old_Focus /= Child
           and then (Child.Flags and Float_As_Transient) /= 0
         then
            declare
               Child_Window : constant Gtk_Window :=
                                Gtk_Window
                                  (Child.Initial.Get_Toplevel);
               New_Parent   : Gtk_Window := null;
            begin
               if Old_Focus = null then
                  New_Parent := Gtk_Window (Child.MDI.Get_Toplevel);
               else
                  New_Parent := Gtk_Window (Old_Focus.Initial.Get_Toplevel);
               end if;

               if New_Parent /= null then
                  Child_Window.Set_Transient_For (New_Parent);
               end if;
            end;
         end if;

         Present (Gtk_Window (Get_Toplevel (Child.Initial)));

      elsif Child.Get_Realized then
         Gdk.Window.Gdk_Raise (Get_Window (Child));

      else
         --  We still need to raise the window, but don't give it the focus
         --  on systems where it is possible.
         Gdk.Window.Gdk_Raise
            (Get_Window (Gtk_Window (Get_Toplevel (Child.Initial))));
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
            Print_Debug ("Raise_Child, give focus to "
                         & Get_Title (Child));
            Set_Focus_Child (Child);
         end if;
      end if;
   end Raise_Child;

   -----------------------
   -- Update_Float_Menu --
   -----------------------

   procedure Update_Float_Menu (Child : access MDI_Child_Record'Class) is
   begin
      if Child.MDI.Action_Float /= null then
         Child.MDI.Action_Float.Set_State
            (Gvariant_New_Boolean (Child.State = Floating));
      end if;
   end Update_Float_Menu;

   -------------------
   -- Has_Title_Bar --
   -------------------

   function Has_Title_Bar
     (Child : not null access MDI_Child_Record) return Boolean is
   begin
      case Child.MDI.Draw_Title_Bars is
         when Always       => return True;
         when Never        => return False;
         when Central_Only => return In_Central_Area (Child.MDI, Child);
      end case;
   end Has_Title_Bar;

   ----------------------
   -- Update_Tab_Color --
   ----------------------

   procedure Update_Tab_Color
     (Note    : access MDI_Notebook_Record'Class;
      Focused : Boolean)
   is
   begin
      if Note = null then
         return;
      end if;

      if not Note.Get_Mapped then
         return;
      end if;

      if not Focused then
         Get_Style_Context (Note).Remove_Class ("mdifocused");
      else
         Get_Style_Context (Note).Add_Class ("mdifocused");
      end if;

      Note.Queue_Draw;
   end Update_Tab_Color;

   ---------------------
   -- Set_Focus_Child --
   ---------------------

   procedure Set_Focus_Child (Child : not null access MDI_Child_Record) is
      Old : constant MDI_Child := Child.MDI.Focus_Child;
      C   : constant MDI_Child := Child.all'Unchecked_Access;
      Tmp : Boolean;
      pragma Unreferenced (Tmp);

      Previous_Focus_Child : constant MDI_Child := Child.MDI.Focus_Child;
      Widget : constant Gtk_Widget :=
        Gtk_Widget_Record (Child.all)'Unchecked_Access;
   begin
      if Child.MDI.Loading_Desktop then
         return;
      end if;

      if not Child.Is_Visible
        and then Maximize_Action.Is_Maximized_Mode (Child.MDI)
      then
         --  The child is hidden of another maximized child, thus unmaximize
         Widget_Callback.Emit_By_Name (Child, Signal_Unmaximize);
      end if;

      --  Be lazy. And avoid infinite loop when updating the MDI menu...

      if C = Old or else C.MDI.In_Destruction then
         return;
      end if;

      --  It is possible that this function is called before the child is
      --  even in the list of items. In this case, we do nothing at this
      --  point (might be called because we insert the child in a notebook
      --  first for instance)

      if Widget_List.Find (C.MDI.Items, Widget) = Null_List then
         return;
      end if;

      Child.MDI.Focus_Child := C;

      if Child.MDI.Focus_Freeze > 0 then
         return;
      end if;

      Show (C);  --  Make sure the child is visible

      if Traces then
         Print_Debug ("Set_Focus_Child on " & Get_Title (C));
      end if;

      if Previous_Focus_Child /= null then
         Update_Tab_Color (Get_Notebook (Previous_Focus_Child), False);
      end if;

      Update_Tab_Color (Get_Notebook (C), True);

      Ref (C);
      Remove (C.MDI.Items, Widget);
      Prepend (C.MDI.Items, Widget);
      Unref (C);

      --  Make sure the page containing Child in a notebook is put on top.
      --  Do not raise floating children, since this is the role of the window
      --  manager.

      if C.State /= Floating then
         Print_Debug ("Set_Focus_Child, raise child " & Get_Title (C));
         Raise_Child (C, False);
      end if;

      --  Give the actual keyboard focus to the appropriate subwindow of
      --  the focus child.

      Give_Focus_To_Child (Child.MDI.Focus_Child);

      if Old /= null
        and then Old.Get_Realized
      then
         Old.Title_Box.Queue_Draw;
      end if;

      if C.Initial.Get_Realized then
         C.Title_Box.Queue_Draw;

         --  Give the focus to the window containing the child.
         --  Giving the focus to a window has the side effect of moving the
         --  window to the current desktop. Therefore, we only do this when the
         --  input focus was already on a window of the MDI.

         if not Child.MDI.Loading_Desktop
           and then Previous_Focus_Child /= null
           and then Get_Toplevel (Previous_Focus_Child.Initial).Get_Realized
           and then Get_Property
             (Get_Toplevel (Previous_Focus_Child.Initial),
              Has_Toplevel_Focus_Property)
         then
            Raise_Child (C);
         end if;
      end if;

      Update_Float_Menu (C);

      if C.MDI.Action_Close /= null then
         C.MDI.Action_Close.Set_Enabled ((C.Flags and Destroy_Button) /= 0);
      end if;

      if C.MDI.Action_Select /= null then
         C.MDI.Action_Select.Set_State (Gvariant_New_String (C.Get_Title));
      end if;

      --  It would be nice to find the first child of C.Initial that
      --  accepts the keyboard focus. However, in the meantime, we at least
      --  want to make sure that no other widget has the focus. As a result,
      --  focus_in events will always be sent the next time the user selects a
      --  widget.

      Highlight_Child (C, False);

      Widget_Callback.Emit_By_Name (C, "selected");
      Child_Selected (C.MDI, C);
   end Set_Focus_Child;

   ------------------
   -- Delete_Child --
   ------------------

   function Delete_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean is
   begin
      if MDI_Child (Child).MDI.In_Destruction then
         --  We can always close a child when the MDI is being destroyed
         return False;

      elsif MDI_Child (Child).MDI.Close_Floating_Is_Unfloat
        and then (MDI_Child (Child).Flags and Always_Destroy_Float) = 0
        and then not MDI_Child (Child).MDI.All_Floating_Mode
      then
         Float_Child (MDI_Child (Child), False);

         Print_Debug
           ("Delete_Child, raising " & Get_Title (MDI_Child (Child)));
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

      --  We do not want to forward to the main window any event.
      --  For instance, a simple character key press sent to a floating
      --  tree view should not be sent to a docked editor.

      --  We are interested in forwarding only a very specific set of
      --  key events, for instance the alt-<character> combinations that
      --  would open a menu.
      --  The test below is a coarse-grain for this.
      --  ??? We should probably do the opposite: rather than passing all
      --  events, only pass those that are known to be of interest.
      if Get_State (Event) = 0 then
         return False;
      end if;

      if Get_Event_Type (Event) = Key_Press then
         return Return_Callback.Emit_By_Name
           (Win, Signal_Key_Press_Event, Event);
      else
         return Return_Callback.Emit_By_Name
           (Win, Signal_Key_Release_Event, Event);
      end if;
   end Key_Event_In_Floating;

   ------------------------------------------
   -- Set_Default_Size_For_Floating_Window --
   ------------------------------------------

   procedure Set_Default_Size_For_Floating_Window
     (Child : not null access MDI_Child_Record;
      Win   : not null access Gtk_Window_Record'Class;
      Width, Height : Glib.Gint)
   is
      pragma Unreferenced (Child);
   begin
      Win.Set_Default_Size (Width, Height);
   end Set_Default_Size_For_Floating_Window;

   -----------------
   -- Float_Child --
   -----------------

   procedure Float_Child
     (Child             : not null access MDI_Child_Record'Class;
      Float             : Boolean;
      Position_At_Mouse : Boolean := True;
      X, Y              : Gint := 0) is
   begin
      Internal_Float_Child
        (Child, Float, Position_At_Mouse => Position_At_Mouse, X => X, Y => Y);
   end Float_Child;

   -----------------------------------
   -- Create_Float_Window_For_Child --
   -----------------------------------

   procedure Create_Float_Window_For_Child
      (Child     : not null access MDI_Child_Record;
       Win       : out Gtk_Window;
       Container : out Gtk_Container)
   is
      function Get_Current_Floating_Child return MDI_Child;
      --  If the current MDI child (i.e: the one that has the focus) is
      --  floating and realized, return it. Return null otherwise.

      --------------------------------
      -- Get_Current_Floating_Child --
      --------------------------------

      function Get_Current_Floating_Child return MDI_Child
      is
         Children : Widget_List.Glist;
         It       : MDI_Child;
      begin
         Children := Child.MDI.Items;
         It := MDI_Child (Get_Data (Children));

         if It /= null and then It /= MDI_Child (Child)
           and then It.State = Floating
              and then It.Initial.Get_Realized
         then
            return It;
         else
            return null;
         end if;
      end Get_Current_Floating_Child;

      Parent                 : Gtk_Window := null;
      Current_Floating_Child : constant MDI_Child :=
                                 Get_Current_Floating_Child;
   begin
      Gtk_New (Win);
      Container := Gtk_Container (Win);

      --  If the mode is set to Float_To_Main, we want to float the dialog as
      --  transient for the MDI's main window.
      --
      --  If the mode is set to Float_As_Transient , we want to float the
      --  dialog as transient for the current child when floating or the main
      --  window otherwise.
      --
      --  In any other case, we don't set any transient window.

      if (Child.Flags and Float_To_Main) /= 0 then
         Parent := Gtk_Window (Child.MDI.Get_Toplevel);
      elsif (Child.Flags and Float_As_Transient) /= 0 then
         if Current_Floating_Child /= null then
            Parent := Gtk_Window (Current_Floating_Child.Initial.Get_Toplevel);
         else
            Parent := Gtk_Window (Child.MDI.Get_Toplevel);
         end if;
      end if;

      if Parent /= null then
         Win.Set_Transient_For (Parent);

         --  Set the window's type hint to dialog to avoid displaying the
         --  minimize/maximize buttons: these buttons can lead to some strange
         --  behavior when a window is set as transient to a parent one (e.g:
         --  minimizing a window also minimizes the parent window on Windows).
         Win.Set_Type_Hint (Window_Type_Hint_Dialog);
      end if;
   end Create_Float_Window_For_Child;

   --------------------------
   -- Internal_Float_Child --
   --------------------------

   procedure Internal_Float_Child
     (Child             : access MDI_Child_Record'Class;
      Float             : Boolean;
      Position_At_Mouse : Boolean;
      X, Y              : Gint;
      Width, Height     : Gint := -1)
   is
      use Object_List;
      Win         : Gtk_Window;
      Cont        : Gtk_Container;
      Widget      : Gtk_Widget;
      Min, Requisition : Gtk_Requisition;
      Groups      : Object_List.GSlist;
      Box         : Gtk_Box;
      W, H        : Gint;
   begin
      if Traces then
         Print_Debug
           ("Float_Child " & Get_Title (Child)
            & " State=" & State_Type'Image (Child.State)
            & " Float=" & Boolean'Image (Float),
            Debug_Increase);
      end if;

      if Child.State /= Floating and then Float then
         --  If the Child already has a window, the resulting floating window
         --  should have the same size.
         --  Otherwise, ask the Child for its requisiton.

         if Width /= -1 and then Height /= -1 then
            W := Width;
            H := Height;
            if Traces then
               Print_Debug ("Forced size:" & Gint'Image (W) & "x"
                            & Gint'Image (H));
            end if;

         elsif Child.Get_Mapped then
            W := Child.Get_Parent.Get_Allocated_Width;
            H := Child.Get_Parent.Get_Allocated_Height;
            if Traces then
               Print_Debug ("Size from allocated:" & Gint'Image (W) & "x"
                            & Gint'Image (H));
            end if;

         else
            Child.Get_Preferred_Size (Min, Requisition);
            W := Requisition.Width;
            H := Requisition.Height;
            if Traces then
               Print_Debug ("Use preferred size:" & Gint'Image (W) & "x"
                            & Gint'Image (H));
            end if;
         end if;

         --  Ref is removed when the child is unfloated

         Ref (Child);

         --  Store the original notebook of the child we want to float: we want
         --  to put it back in the same notebook if the user unfloats it.

         Child.Notebook_Before_Floating := Gtk_Notebook (Get_Notebook (Child));

         if Child.Notebook_Before_Floating /= null then
            Child.Notebook_Before_Floating.On_Destroy
              (On_Notebook_Before_Floating_Destroy'Access, Slot => Child);
         end if;

         --  This could be called before the child even has a parent if
         --  All_Floating_Mode is set.

         if Get_Parent (Child) /= null then
            Remove (Gtk_Container (Get_Parent (Child)), Child);
         end if;

         Child.Create_Float_Window_For_Child (Win, Cont);
         Child.Set_Default_Size_For_Floating_Window (Win, W, H);

         if Child.MDI.Use_Short_Titles_For_Floats then
            Set_Title (Win, Locale_From_UTF8 (Child.Short_Title.all));
         else
            Set_Title (Win, Locale_From_UTF8 (Child.Title.all));
         end if;

         --  Memorize the MDI_Child associated with the window, for faster
         --  lookup for instance in Find_MDI_Child_From_Widget.

         Child_User_Data.Set (Win, MDI_Child (Child), "parent_mdi_child");

         --  Add the 'mdichild' CSS class to the newly created floating window
         Get_Style_Context (Win).Add_Class ("mdichild");

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
            Move (Win, X, Y);
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

         Widget := Child.Initial.Get_Parent;
         Ref (Widget);
         Gtk_Container (Widget.Get_Parent).Remove (Widget);
         Cont.Add (Widget);
         Unref (Widget);
         if Cont.all in Gtk_Box_Record'Class then
            Gtk_Box (Cont).Set_Child_Packing
               (Widget, Expand => True, Fill => True, Padding => 0,
                Pack_Type => Pack_Start);
         end if;

         Set_State (Child, Floating);
         Update_Float_Menu (Child);
         Show_All (Win);

         Emit_By_Name_Child
           (Get_Object (Child.MDI), String (Signal_Float_Child) & ASCII.NUL,
            Get_Object (Child));
         Widget_Callback.Emit_By_Name (Child, Signal_Float_Child);

      elsif Child.State = Floating and then not Float then
         --  We are about to unfloat the child: emit the "before" signal
         --  before the actual unfloat, so that clients have the possibility
         --  to grab the coordinates of the floating window, for instance.

         Emit_By_Name_Child
           (Get_Object (Child.MDI),
            String (Signal_Before_Unfloat_Child) & ASCII.NUL,
            Get_Object (Child));
         Widget_Callback.Emit_By_Name (Child, Signal_Before_Unfloat_Child);

         --  Reassign the widget to Child instead of the notebook

         Win := Gtk_Window (Get_Toplevel (Child.Initial));
         Box := Gtk_Box (Get_Child (Child));
         Widget := Child.Initial.Get_Parent;

         Ref (Widget);
         Gtk_Container (Widget.Get_Parent).Remove (Widget);
         Box.Add (Widget);
         Unref (Widget);
         Box.Set_Child_Packing
            (Widget, Expand => True, Fill => True, Padding => 0,
             Pack_Type => Pack_Start);

         Set_State (Child, Normal);
         Destroy (Win);

         Put_In_Notebook
           (MDI      => Child.MDI,
            Child    => Child,
            Notebook => MDI_Notebook (Child.Notebook_Before_Floating));

         Update_Float_Menu (Child);

         Emit_By_Name_Child
           (Get_Object (Child.MDI), String (Signal_Unfloat_Child) & ASCII.NUL,
            Get_Object (Child));
         Widget_Callback.Emit_By_Name (Child, Signal_Unfloat_Child);

         Unref (Child);
      end if;

      Print_Debug ("", Debug_Decrease);
   end Internal_Float_Child;

   -----------------
   -- Is_Floating --
   -----------------

   function Is_Floating
     (Child : not null access MDI_Child_Record'Class) return Boolean is
   begin
      return Child.State = Floating;
   end Is_Floating;

   ------------------------
   -- On_Tab_Orientation --
   ------------------------

   procedure On_Tab_Orientation
     (Notebook    : access Gtk_Notebook_Record'Class;
      Orientation : Tab_Orientation_Type)
   is
      Child : MDI_Child;
      Length : constant Gint := Notebook.Get_N_Pages;
   begin
      MDI_Notebook (Notebook).Tab_Orientation := Orientation;

      if Length > 0 then
         for Page_Index in 0 .. Length - 1 loop
            Child := MDI_Child (Notebook.Get_Nth_Page (Page_Index));
            Update_Tab_Label (Child);
         end loop;
      end if;
   end On_Tab_Orientation;

   ----------------
   -- On_Tab_Pos --
   ----------------

   procedure On_Tab_Pos
     (Note : access Gtk_Notebook_Record'Class;
      Pos  : Gtk.Enums.Gtk_Position_Type)
   is
   begin
      Note.Set_Tab_Pos (Pos);

      Get_Style_Context (Note).Remove_Class ("rightTabs");
      Get_Style_Context (Note).Remove_Class ("leftTabs");
      Get_Style_Context (Note).Remove_Class ("bottomTabs");

      case Pos is
         when Pos_Top =>
            null;
         when Pos_Left =>
            Get_Style_Context (Note).Add_Class ("leftTabs");
         when Pos_Right =>
            Get_Style_Context (Note).Add_Class ("rightTabs");
         when Pos_Bottom =>
            Get_Style_Context (Note).Add_Class ("bottomTabs");
      end case;

      On_Tab_Orientation (Note, MDI_Notebook (Note).Tab_Orientation);
   end On_Tab_Pos;

   -------------
   -- Get_MDI --
   -------------

   function Get_MDI
     (Child : not null access MDI_Child_Record) return MDI_Window is
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
      Note : constant MDI_Notebook := Get_Notebook (C);
      Menu : Gtk_Menu;
      Submenu : Gtk_Menu;
      Sep  : Gtk_Separator_Menu_Item;
      Item : Gtk_Menu_Item;
      MItem : Child_Menu_Item;
      Widget : MDI_Child;
      Image  : Gtk_Image;
      Current : Gint;
   begin
      if Get_Button (Event) = 3 then
         Gtk_New (Menu);

         Current := Note.Get_Current_Page;

         for P in 1 .. Note.Get_N_Pages loop
            Widget := MDI_Child (Note.Get_Nth_Page (P - 1));
            MItem := new Child_Menu_Item_Record;
            MItem.Child := Widget;
            Gtk.Image_Menu_Item.Initialize
              (MItem, Label => Widget.Short_Title.all);

            if P - 1 = Current then
               Gtk_Label (MItem.Get_Child).Set_Markup
                 ("<b>" & Widget.Short_Title.all & "</b>");
            end if;

            if Widget.Tab_Icon /= null then
               Gtk_New (Image, Gdk_Pixbuf'(Widget.Tab_Icon.Get));
               MItem.Set_Image (Image);
               MItem.Set_Always_Show_Image (True);
            end if;

            Menu.Append (MItem);
            Widget_Callback.Connect
              (MItem, Gtk.Menu_Item.Signal_Activate, Menu_Switch_Page'Access);
         end loop;

         Gtk_New (Sep);
         Menu.Append (Sep);

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

         Gtk_New (Item, "Tabs rotation");
         Menu.Append (Item);

         Gtk_New (Submenu);
         Item.Set_Submenu (Submenu);

         Gtk_New (Item, "Automatic");
         Tab_Orientation_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Tab_Orientation'Access, Note, Automatic);
         Submenu.Append (Item);

         Gtk_New (Item, "Horizontal");
         Tab_Orientation_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Tab_Orientation'Access, Note, Horizontal);
         Submenu.Append (Item);

         Gtk_New (Item, "Bottom to top");
         Tab_Orientation_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Tab_Orientation'Access, Note, Bottom_To_Top);
         Submenu.Append (Item);

         Gtk_New (Item, "Top to bottom");
         Tab_Orientation_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            On_Tab_Orientation'Access, Note, Top_To_Bottom);
         Submenu.Append (Item);

         Gtk_New (Sep);
         Menu.Append (Sep);

         if (C.Flags and Destroy_Button) /= 0 then
            Gtk_New (Item, "Close");
            Widget_Callback.Object_Connect
              (Item, Gtk.Menu_Item.Signal_Activate, Close_Cb'Access, Child);
            Prepend (Menu, Item);
         end if;

         if C.MDI.Tab_Factory /= null then
            C.MDI.Tab_Factory (C, Menu);
         end if;

         Show_All (Menu);
         Popup (Menu,
                Button        => 3,
                Activate_Time => Gdk.Event.Get_Time (Event));
         return True;

      elsif Get_Event_Type (Event) = Gdk_2button_Press
        and then Get_Button (Event) = 1
      then
         if not Maximize_Action.Is_Maximized_Mode (C.MDI) then
            Widget_Callback.Emit_By_Name (C, Signal_Maximize_Child);
         else
            Widget_Callback.Emit_By_Name (C, Signal_Unmaximize);
         end if;
         return True;

      elsif Get_Button (Event) = 2 then
         C.Close_Child;
         return True;

      end if;
      return False;
   end On_Notebook_Button_Press;

   -----------------------------------------
   -- On_Notebook_Before_Floating_Destroy --
   -----------------------------------------

   procedure On_Notebook_Before_Floating_Destroy
     (Slot : access Glib.Object.GObject_Record'Class)
   is
      Child : constant MDI_Child := MDI_Child (Slot);
   begin
      Child.Notebook_Before_Floating := null;
   end On_Notebook_Before_Floating_Destroy;

   ----------------------
   -- Menu_Switch_Page --
   ----------------------

   procedure Menu_Switch_Page (Item : access Gtk_Widget_Record'Class) is
      It : constant Child_Menu_Item := Child_Menu_Item (Item);
   begin
      It.Child.Set_Focus_Child;
   end Menu_Switch_Page;

   ---------------------
   -- Create_Notebook --
   ---------------------

   function Create_Notebook
     (MDI : access MDI_Window_Record'Class) return MDI_Notebook
   is
      Notebook : MDI_Notebook;
   begin
      Notebook := new MDI_Notebook_Record;
      Notebook.Tab_Orientation := MDI.Tabs_Orientation;
      Gtk.Notebook.Initialize (Notebook);
      Configure_Notebook_Tabs (MDI, Notebook);
      Set_Border_Width (Notebook, 0);
      Set_Scrollable (Notebook);
      Notebook.Set_Group_Name ("MDI");
      Get_Style_Context (Notebook).Add_Class ("mdi");

      Widget_Callback.Connect
        (Notebook, Signal_Remove, Removed_From_Notebook'Access);
      Widget_Callback.Connect
        (Notebook, Signal_Set_Focus_Child, Set_Focus_Child_Notebook'Access);
      Widget_Callback.Connect
        (Notebook, Signal_Switch_Page,
         Set_Focus_Child_Switch_Notebook_Page'Access);

      Notebook.On_Motion_Notify_Event (Button_Motion_Notebook'Access);
      Notebook.On_Button_Release_Event (Button_Release_Notebook'Access);

      On_Tab_Pos (Notebook, MDI.Tabs_Position);
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
      --  Some pages might be hidden, in which case they should not be counted
      --  when we compute whether the tabs should be made visible

      for P in 0 .. Page_Count - 1 loop
         if Get_Nth_Page (Notebook, P).Get_Visible then
            Visible_Page_Count := Visible_Page_Count + 1;
         end if;
      end loop;

      if Visible_Page_Count >= 2 then
         Set_Show_Tabs (Notebook, MDI.Show_Tabs_Policy /= Never);
      else
         Set_Show_Tabs (Notebook, MDI.Show_Tabs_Policy = Always);
      end if;

      if Notebook.Get_Show_Tabs then
         Get_Style_Context (Notebook).Remove_Class ("noTabs");
      else
         Get_Style_Context (Notebook).Add_Class ("noTabs");
      end if;

      Child := MDI_Child (Get_Nth_Page (Notebook, 0));
      if Child /= null then
         Set_Property
            (Notebook, Show_Border_Property,
             Get_Nth_Page (Notebook, 1) = null
             and then MDI.Show_Tabs_Policy /= Always);
      end if;

      if Hide_If_Empty then
         if Visible_Page_Count = 0 then
            Hide (Notebook);
         else
            Show (Notebook);
         end if;
      end if;
   end Configure_Notebook_Tabs;

   -----------------------------
   -- Tab_Get_Preferred_Width --
   -----------------------------

   procedure Tab_Get_Preferred_Width
     (Label : System.Address; Minimum_Size, Natural_Size : out Gint)
   is
      Tab   : constant MDI_Tab := MDI_Tab (Glib.Object.Convert (Label));
      Note  : constant MDI_Notebook := MDI_Notebook (Tab.Get_Parent);
      Pages : Gint;
      Lab   : MDI_Tab;
      Min, Nat : Gint;
      Total    : Gint;
      Val : GValue;
      Focus_Width : Gint;
      Focus_Pad   : Gint;

   begin
      case Note.Get_Tab_Pos is
         when Pos_Left | Pos_Right =>
            --  fallback to gtk behavior
            Tab.Get_Child.Get_Preferred_Width (Minimum_Size, Natural_Size);

         when Pos_Top | Pos_Bottom =>
            --  Do we need to recompute, or has another tab already done the
            --  computation ?

            Tab.Timestamp := Tab.Timestamp + 1;
            if Note.Timestamp < Tab.Timestamp then
               Note.Timestamp := Tab.Timestamp;

               Pages := Note.Get_N_Pages;
               Minimum_Size := 0;
               Natural_Size := 0;

               --  Find all other MDI_Tab_Labels.

               for N in 0 .. Pages - 1 loop
                  Lab := MDI_Tab (Note.Get_Tab_Label (Note.Get_Nth_Page (N)));
                  Lab.Get_Child.Get_Preferred_Width (Min, Nat);
                  Natural_Size := Natural_Size + Nat;
                  Minimum_Size := Minimum_Size + Min;
               end loop;

               --  Compute borders from the theme. This is copied from
               --  gtknotebook.c. Apparently we do not need to get
               --     Get_Style_Context (Note).Get_Padding
               Init (Val, GType_Int);
               Note.Style_Get_Property ("focus-line-width", Val);
               Focus_Width := Get_Int (Val);
               Note.Style_Get_Property ("focus-padding", Val);
               Focus_Pad := Get_Int (Val);
               Unset (Val);

               Total := Note.Get_Allocated_Width
                 - Pages * 2 * (Focus_Width + Focus_Pad);

               if Traces then
                  Tab.Get_Child.Get_Preferred_Width (Min, Nat);
                  Put_Line ("Tabsize: total {Nat=" & Gint'Image (Natural_Size)
                            & " Min=" & Gint'Image (Minimum_Size)
                            & "} Child={"
                            & Gint'Image (Min) & Gint'Image (Nat)
                            & "} Notebook="
                            & Gint'Image (Total) & " Pages="
                            & Gint'Image (Pages));
               end if;

               if Natural_Size <= Total then
                  --  All tabs can use their natural size, but we must set the
                  --  tab's minimum size to its natural size since gtk+ always
                  --  uses the minimum size.

                  Note.Tab_Size := -1;
               else
                  --  All tabs should use the same size (and have a min. size)
                  --  "100" is random here, seems to be good enough to display
                  --  enough chars to distinguish editors.
                  Nat := Gint'Max (100, Total / Pages);
                  Note.Tab_Size := Nat;
               end if;

               --  Will need to resize all other tabs (nothing will happen for
               --  those that have already been refreshed, because of timestamp
               --  comparison)

               for N in 0 .. Pages - 1 loop
                  Lab := MDI_Tab (Note.Get_Tab_Label (Note.Get_Nth_Page (N)));
                  if Lab /= Tab then
                     Lab.Queue_Resize;
                  end if;
               end loop;

               Minimum_Size := Nat;
               Natural_Size := Nat;

               if Traces then
                  Put_Line ("tab size => note.timestamp="
                            & Integer'Image (Note.Timestamp)
                            & " tab.timestamp="
                            & Integer'Image (Tab.Timestamp)
                            & " size=" & Gint'Image (Nat));
               end if;
            else
               Tab.Timestamp := Note.Timestamp;
               if Note.Tab_Size = -1 then
                  Tab.Get_Child.Get_Preferred_Width (Min, Natural_Size);
                  Minimum_Size := Natural_Size;
               else
                  Minimum_Size := Note.Tab_Size;
                  Natural_Size := Note.Tab_Size;
               end if;
            end if;
      end case;
   end Tab_Get_Preferred_Width;

   ------------------------------
   -- Tab_Get_Preferred_Height --
   ------------------------------

   procedure Tab_Get_Preferred_Height
     (Label : System.Address; Minimum_Size, Natural_Size : out Gint)
   is
      Tab  : constant MDI_Tab := MDI_Tab (Glib.Object.Convert (Label));
      Note : constant MDI_Notebook := MDI_Notebook (Tab.Get_Parent);
      Pages       : Gint;
      Lab         : MDI_Tab;
      Max_Of_Min, Min, Nat    : Gint;
      Total       : Gint;
   begin
      case Note.Get_Tab_Pos is
         when Pos_Left | Pos_Right =>
            --  Do we need to recompute, or has another tab already done the
            --  computation ?

            Tab.Timestamp := Tab.Timestamp + 1;
            if Note.Timestamp < Tab.Timestamp then
               Note.Timestamp := Tab.Timestamp;

               Pages := Note.Get_N_Pages;
               Minimum_Size := 0;
               Natural_Size := 0;
               Max_Of_Min := 0;

               --  Find all other MDI_Tab_Labels.

               for N in 0 .. Pages - 1 loop
                  Lab := MDI_Tab (Note.Get_Tab_Label (Note.Get_Nth_Page (N)));
                  Lab.Get_Child.Get_Preferred_Height (Min, Nat);
                  Natural_Size := Natural_Size + Nat;
                  Minimum_Size := Minimum_Size + Min;
                  Max_Of_Min := Gint'Max (Max_Of_Min, Min);
               end loop;

               Total := Note.Get_Allocated_Height;

               if Traces then
                  Put_Line ("Tabsize: total {Nat=" & Gint'Image (Natural_Size)
                            & " Min=" & Gint'Image (Minimum_Size)
                            & "} Child={"
                            & Gint'Image (Min) & Gint'Image (Nat)
                            & "} Notebook="
                            & Gint'Image (Total) & " Pages="
                            & Gint'Image (Pages));
               end if;

               if Natural_Size <= Total then
                  --  All tabs can use their natural size, but we must set the
                  --  tab's minimum size to its natural size since gtk+ always
                  --  uses the minimum size.

                  Note.Tab_Size := -1;
               else
                  --  All tabs should use the same size (and have a min. size)
                  --  "100" is random here, seems to be good enough to display
                  --  enough chars to distinguish editors.
                  Nat := Gint'Max (Max_Of_Min, Total / Pages);
                  Note.Tab_Size := Nat;
               end if;

               --  Will need to resize all other tabs (nothing will happen for
               --  those that have already been refreshed, because of timestamp
               --  comparison)

               for N in 0 .. Pages - 1 loop
                  Lab := MDI_Tab (Note.Get_Tab_Label (Note.Get_Nth_Page (N)));
                  if Lab /= Tab then
                     Lab.Queue_Resize;
                  end if;
               end loop;

               Minimum_Size := Nat;
               Natural_Size := Nat;

               if Traces then
                  Put_Line ("tab size => note.timestamp="
                            & Integer'Image (Note.Timestamp)
                            & " tab.timestamp="
                            & Integer'Image (Tab.Timestamp)
                            & " size=" & Gint'Image (Nat));
               end if;
            else
               Tab.Timestamp := Note.Timestamp;
               if Note.Tab_Size = -1 then
                  Tab.Get_Child.Get_Preferred_Height (Min, Natural_Size);
                  Minimum_Size := Natural_Size;
               else
                  Minimum_Size := Note.Tab_Size;
                  Natural_Size := Note.Tab_Size;
               end if;
            end if;

         when Pos_Top | Pos_Bottom =>
            --  fallback to gtk behavior
            Tab.Get_Child.Get_Preferred_Height (Minimum_Size, Natural_Size);
      end case;
   end Tab_Get_Preferred_Height;

   --------------------
   -- Tab_Class_Init --
   --------------------

   procedure Tab_Class_Init (Self : GObject_Class) is
   begin
      Set_Default_Get_Preferred_Width_Handler
        (Self, Tab_Get_Preferred_Width'Access);
      Set_Default_Get_Preferred_Height_Handler
        (Self, Tab_Get_Preferred_Height'Access);
   end Tab_Class_Init;

   -----------------
   -- Get_Tooltip --
   -----------------

   function Get_Tooltip
     (Child : not null access MDI_Child_Record) return String
   is
      pragma Unreferenced (Child);
   begin
      return "";
   end Get_Tooltip;

   ---------------------------
   -- Get_Tooltip_Is_Markup --
   ---------------------------

   function Get_Tooltip_Is_Markup
     (Child : not null access MDI_Child_Record) return Boolean
   is
      pragma Unreferenced (Child);
   begin
      return False;
   end Get_Tooltip_Is_Markup;

   ----------------------
   -- Update_Tab_Label --
   ----------------------

   procedure Update_Tab_Label (Child : access MDI_Child_Record'Class) is
      Note   : constant MDI_Notebook := Get_Notebook (Child);
      Event  : Gtk_Event_Box;
      Box    : Gtk_Box;
      Close  : Close_Button.Gtkada_MDI_Close_Button;
      Orientation : Tab_Orientation_Type;

      procedure Add_Icon;
      procedure Add_Close_Button;
      procedure Add_Label;

      procedure Add_Icon is
      begin
         if Child.Icon_Name = null then
            Gtk_New (Child.Tab_Icon);
            Child.Tab_Icon.Hide;
         else
            Gtk_New_From_Icon_Name
              (Child.Tab_Icon, Child.Icon_Name.all, Icon_Size_In_Tabs);
            Child.Tab_Icon.Show_All;
         end if;

         --  The visibility of the Tab_Icon should be controlled by
         --  the actual presence of the icon. Set it to No_Show_All
         --  so that it does not take up space if the icon is not set.
         Child.Tab_Icon.Set_No_Show_All (No_Show_All => True);

         Box.Pack_Start (Child.Tab_Icon, Expand => False, Padding => 1);
      end Add_Icon;

      procedure Add_Close_Button is
      begin
         if (Child.Flags and Destroy_Button) /= 0 then
            Close_Button.Gtk_New
              (Close, Event, Child,
               Position    => Note.Get_Tab_Pos,
               In_Titlebar => False);
            Box.Pack_Start (Close, Expand => False, Padding => 2);
         end if;
      end Add_Close_Button;

      procedure Add_Label is
      begin
         Gtk_New (Child.Tab_Label, Child.Short_Title.all);
         Box.Pack_Start (Child.Tab_Label, Expand => True, Fill => True);

         declare
            Tooltip : constant String := Child.Get_Tooltip;
         begin
            if Tooltip = "" then
               Child.Tab_Label.Set_Tooltip_Text (Child.Title.all);
            elsif Child.Get_Tooltip_Is_Markup then
               Child.Tab_Label.Set_Tooltip_Markup (Tooltip);
            else
               Child.Tab_Label.Set_Tooltip_Text (Tooltip);
            end if;
         end;

         if Child.MDI.Homogeneous_Tabs then
            Child.Tab_Label.Set_Ellipsize (Pango.Layout.Ellipsize_Middle);
         end if;

         case Note.Actual_Tab_Orientation is
            when Horizontal | Automatic =>
               Child.Tab_Label.Set_Angle (0.0);
            when Bottom_To_Top =>
               Child.Tab_Label.Set_Angle (90.0);
            when Top_To_Bottom =>
               Child.Tab_Label.Set_Angle (270.0);
         end case;
      end Add_Label;

   begin
      if Note /= null and then Child.State = Normal then
         if Child.MDI.Homogeneous_Tabs then
            Event := new MDI_Tab_Record;
            MDI_Tab (Event).Timestamp := Note.Timestamp;

            Glib.Object.Initialize_Class_Record
              (Ancestor     => Gtk.Event_Box.Get_Type,
               Signals      => (1 .. 0 => Interfaces.C.Strings.Null_Ptr),
               Parameters   => (1 .. 0 => (1 => GType_None)),
               Class_Record => MDI_Tab_Class_Record,
               Type_Name    => "MDITab",
               Class_Init   => Tab_Class_Init'Access);

            G_New (Event, MDI_Tab_Class_Record.The_Type);
         else
            Gtk_New (Event);
         end if;

         Event.Set_App_Paintable (True);  --  prevent gtk_event_box_draw
         Event.Set_Visible_Window (False);

         Orientation := Note.Tab_Orientation;
         if Orientation = Automatic then
            case Note.Get_Tab_Pos is
               when Pos_Top | Pos_Bottom =>
                  Orientation := Horizontal;
               when Pos_Left =>
                  Orientation := Bottom_To_Top;
               when Pos_Right =>
                  Orientation := Top_To_Bottom;
            end case;
         end if;

         Note.Actual_Tab_Orientation := Orientation;

         if Note.Actual_Tab_Orientation = Horizontal then
            Gtk_New_Hbox (Box, Homogeneous => False);
         else
            Gtk_New_Vbox (Box, Homogeneous => False);
         end if;

         if Note.Actual_Tab_Orientation = Bottom_To_Top then
            Add_Close_Button;
            Add_Label;
            Add_Icon;
         else
            Add_Icon;
            Add_Label;
            Add_Close_Button;
         end if;

         Event.Add (Box);

         Note.Set_Tab_Label (Child, Event);
         Note.Set_Tab_Detachable (Child, False);
         Note.Set_Tab_Reorderable (Child, True);

         Show_All (Event);

         Event.On_Button_Press_Event
           (Set_Focus_Child_MDI_From_Tab'Access, Slot => Child);
         Return_Callback.Object_Connect
           (Event, Signal_Button_Press_Event,
            Return_Callback.To_Marshaller (On_Notebook_Button_Press'Access),
            Child);
         Event.On_Button_Release_Event
           (Set_Focus_Child_MDI_From_Tab'Access, Slot => Child);

         --  Setup drag-and-drop, so that items can be moved from one location
         --  to another.

         Set_Dnd_Source (Event, Child);

         Event.Queue_Resize;
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
      Notebook                 : MDI_Notebook := null;
      Initial_Position         : Child_Position := Position_Automatic;
      Force_Parent_Destruction : Boolean := True)
   is
      Note                   : MDI_Notebook;
      Old_Parent             : Gtk_Container;
      Destroy_Old            : Boolean := False;
      Old_Note_Was_Destroyed : aliased Boolean := False;

   begin
      --  Embed the contents of the child into the notebook

      if Notebook /= null then
         Note   := Notebook;

      elsif Child.Areas = Central_Only
        or else (Child.Group = Group_Default
                 and then not MDI.Independent_Perspectives
                 and then MDI.Central /= null
                 and then Child.Areas /= Sides_Only)
      then
         Note := Find_Current_In_Central
           (MDI.Central, MDI, Child.Group, Initial_Position);

      else
         Note := Find_Current_In_Central
           (MDI, MDI, Child.Group, Initial_Position);
      end if;

      if Get_Parent (Child) = Gtk_Widget (Note) then
         return;
      end if;

      Ref (Child);

      if Get_Parent (Child) /= null then
         Old_Parent := Gtk_Container (Get_Parent (Child));

         --  Always destroy the notebook we were in, since we are
         --  putting the item elsewhere anyway, there will still be
         --  a notebook for items in the same position.

         Destroy_Old := Force_Parent_Destruction
           and then Old_Parent.all in Gtk_Notebook_Record'Class
           and then Get_Nth_Page (Gtk_Notebook (Old_Parent), 1) = null;

         Weak_Ref
           (Old_Parent, Note_Notify'Access, Old_Note_Was_Destroyed'Address);
         Remove (Old_Parent, Child);

         if not Old_Note_Was_Destroyed then
            Weak_Unref
              (Old_Parent, Note_Notify'Access, Old_Note_Was_Destroyed'Address);
         end if;

         --  Problem: Old_Note might no longer exist not, since
         --  Removed_From_Notebook might have destroyed it.

         if Destroy_Old and then not Old_Note_Was_Destroyed then
            Destroy (Old_Parent);
         end if;
      end if;

      Set_State (Child, Normal);

      Append_Page (Note, Child);
      Note.Set_Menu_Label_Text (Child, Child.Short_Title.all);

      Configure_Notebook_Tabs (MDI, Note);

      if Child = Child.MDI.Focus_Child then
         Update_Tab_Color (Note, True);
      end if;

      Update_Tab_Label (Child);

      --  In case the user displays title bars only in the central area, we
      --  might need to change its visibility when moving in or out of the
      --  central area
      Set_Child_Title_Bar (Child);

      Set_Child_Visible (Note, True);
      Show (Note);

      Unref (Child);
   end Put_In_Notebook;

   -----------------------------
   -- Find_Current_In_Central --
   -----------------------------

   function Find_Current_In_Central
     (Pane             : access Gtkada_Multi_Paned_Record'Class;
      MDI              : access MDI_Window_Record'Class;
      Group            : Child_Group := Group_Any;
      Initial_Position : Child_Position := Position_Automatic)
      return MDI_Notebook
   is
      List                  : Widget_List.Glist := MDI.Items;
      C                     : MDI_Child;
      Note                  : MDI_Notebook;
      Current               : MDI_Notebook;
      Default_Current_Found : Boolean := False;
   begin
      if Gtkada_Multi_Paned (Pane) = Gtkada_Multi_Paned (MDI) then
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

      else
         --  In the central area, look for the last child used, and put the new
         --  window on top of it

         if not MDI.Independent_Perspectives then
            while List /= Widget_List.Null_List loop
               C := MDI_Child (Get_Data (List));

               if In_Central_Area (MDI, C) and then C.Group = Group then
                  Note := Get_Notebook (C);
                  Current := Note;
                  exit;
               end if;

               List := Next (List);
            end loop;
         end if;

         --  No last child ? It means the central area is empty (or contains
         --  an empty notebook, in case we could not reload the desktop, for
         --  instance because a file previously edited no longer exists).

         if Current = null then
            if Traces then
               Print_Debug
                 ("Find_Current_In_Central: no last child in <central>,"
                  & " checking whether we have an empty notebook");
            end if;

            if not MDI.Independent_Perspectives then
               declare
                  Iter : Gtkada.Multi_Paned.Child_Iterator :=
                    Start (MDI.Central);
               begin
                  while not At_End (Iter)
                    and then Get_Widget (Iter) = null
                  loop
                     Next (Iter);
                  end loop;

                  if not At_End (Iter) then
                     Print_Debug ("Found empty notebook, using it");
                     Note := MDI_Notebook (Get_Widget (Iter));
                     Current := Note;
                  end if;
               end;
            end if;

            --  Current might still be null if the central area really is empty
         end if;
      end if;

      if Note = null then
         if Traces then
            Print_Debug ("no notebook yet, Position="
                         & Child_Position'Image (Initial_Position));
         end if;

         case Initial_Position is
            when Position_Bottom =>
               Note := Create_Notebook (MDI);
               Split (Pane,
                      New_Child   => Note,
                      Ref_Pane    => Root_Pane,
                      Orientation => Orientation_Vertical,
                      Width       => -1,
                      Height      => -1,
                      After       => True);
            when Position_Top =>
               Note := Create_Notebook (MDI);
               Split (Pane,
                      New_Child   => Note,
                      Ref_Pane    => Root_Pane,
                      Orientation => Orientation_Vertical,
                      Width       => -1,
                      Height      => -1,
                      After       => False);
            when Position_Left =>
               Note := Create_Notebook (MDI);
               Split (Pane,
                      New_Child   => Note,
                      Ref_Pane    => Root_Pane,
                      Orientation => Orientation_Horizontal,
                      Width       => -1,
                      Height      => -1,
                      After       => False);
            when Position_Right =>
               Note := Create_Notebook (MDI);
               Split (Pane,
                      New_Child   => Note,
                      Ref_Pane    => Root_Pane,
                      Orientation => Orientation_Horizontal,
                      Width       => -1,
                      Height      => -1,
                      After       => True);
            when Position_Automatic | Position_Float =>
               if Current /= null then
                  Note := Current;
               else
                  Note := Create_Notebook (MDI);
                  Add_Child
                    (Pane, New_Child => Note, Width => -1, Height => -1);
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
         MDI.All_Floating_Mode := All_Floating;

         --  We cannot do a simple loop here. When a child is floated, it
         --  can happen that the mouse enters the window, and the focus changes
         --  immediately, resulting in a change in the order of children in the
         --  list, even though not all windows have been floated yet.
         --  To fix this, we do two loops: one to list all the widgets, and
         --  one to float them.

         MDI.Freeze_Focus;
         declare
            package Child_List is new Ada.Containers.Doubly_Linked_Lists
              (MDI_Child);
            --  ??? We should use this to implement MDI.Items, rather than
            --  a Widget_List.Glist.
            use Child_List;
            To_Process : Child_List.List;
            Cursor     : Child_List.Cursor;
         begin
            List := First (MDI.Items);

            while List /= Null_List loop
               C := MDI_Child (Get_Data (List));
               if (C.State /= Floating and then All_Floating)
                 or else (C.State = Floating and then not All_Floating)
               then
                  To_Process.Append (C);
               end if;
               List := Next (List);
            end loop;

            Cursor := To_Process.First;
            while Has_Element (Cursor) loop
               Float_Child (Element (Cursor), All_Floating);
               Next (Cursor);
            end loop;

            To_Process.Clear;

            MDI.Thaw_Focus;
         exception
            when others =>
               MDI.Thaw_Focus;
               raise;
         end;

         if MDI.Action_Float /= null then
            MDI.Action_Float.Set_Enabled (not All_Floating);
         end if;

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

            declare
               T : constant String := Child.Title.all;
               S : constant String := Child.Short_Title.all;
            begin
               Free (Child.Title);  --  Force a refresh
               Free (Child.Short_Title);
               Child.Set_Title (T, S);
            end;

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
      if Child = null then
         return null;
      else
         return Child.Initial;
      end if;
   end Get_Widget;

   ---------------------
   -- Get_Focus_Child --
   ---------------------

   function Get_Focus_Child
     (MDI : access MDI_Window_Record) return MDI_Child is
   begin
      return MDI.Focus_Child;
   end Get_Focus_Child;

   ---------------------------
   -- Removed_From_Notebook --
   ---------------------------

   procedure Removed_From_Notebook
     (Note : access Gtk_Widget_Record'Class; Args  : Gtk_Args)
   is
      C     : constant Gtk_Widget := Gtk_Widget (To_Object (Args, 1));
      Child : MDI_Child;
   begin
      if C.all not in MDI_Child_Record'Class then
         return;
      end if;

      Child := MDI_Child (C);
      Child.Tab_Label := null;
      Child.Tab_Icon := null;
      Set_State (Child, Normal);

      if Child.MDI.Focus_Child = Child then
         Update_Tab_Color (Get_Notebook (Child), False);
      end if;

      if not Note.In_Destruction then
         Print_Debug ("Removed_From_Notebook: " & Get_Title (Child));

         --  No more pages in the notebook ? => Destroy it

         if Get_Nth_Page (Gtk_Notebook (Note), 0) = null then
            Destroy (Note);
         else
            Configure_Notebook_Tabs
              (Child.MDI, Gtk_Notebook (Note), Hide_If_Empty => True);

            --  force computation of tab size
            MDI_Notebook (Note).Timestamp := 0;
            Note.Queue_Resize;
         end if;

         if Traces then
            Print_Debug ("Removed_From_Notebook: desktop is now");
            Dump (Child.MDI);
         end if;
      end if;

   exception
      when E : others =>
         --  Silently ignore the exceptions for now, to avoid crashes.
         --  The application using the MDI can not do it, since this callback
         --  is called directly from the menu in Create_Menu.
         Print_Debug
           ("Unexpected exception: " & Exception_Information (E));
   end Removed_From_Notebook;

   -----------
   -- Split --
   -----------

   procedure Split
     (MDI           : access MDI_Window_Record;
      Orientation   : Gtk.Enums.Gtk_Orientation;
      Child         : MDI_Child := null;
      Mode          : Split_Mode := Before;
      Width, Height : Glib.Gint := 0)
   is
      Note, Note2 : MDI_Notebook;
      Target      : MDI_Child;
      Pane        : Gtkada_Multi_Paned;
      W           : Gtk_Widget;
      After       : Boolean := True;
   begin
      if Child /= null then
         Target := Child;
      elsif MDI.Focus_Child /= null then
         Target := MDI.Focus_Child;
      elsif MDI.Items = Widget_List.Null_List then
         return;
      else
         Target := MDI_Child (Get_Data (MDI.Items));
      end if;

      Note := Get_Notebook (Target);

      --  Only split if there are at least two children
      if Note /= null and then Get_Nth_Page (Note, 1) /= null then

         if In_Central_Area (MDI, Target) then
            Pane := MDI.Central;
         else
            Pane := Gtkada_Multi_Paned (MDI);
         end if;

         case Mode is
            when Before =>
               Note2 := null;
               After := False;

            when Gtkada.MDI.After =>
               Note2 := null;
               After := True;

            when Before_Reuse =>
               W := Splitted_Area (Pane, Note, Orientation, After => False);
               After := False;

            when After_Reuse =>
               W := Splitted_Area (Pane, Note, Orientation, After => True);
               After := True;

            when Any_Side_Reuse =>
               W := Splitted_Area (Pane, Note, Orientation, After => True);
               if W = null then
                  W := Splitted_Area (Pane, Note, Orientation, After => False);
               end if;
               After := True;
         end case;

         if W /= null and then W.all in Gtk_Notebook_Record'Class then
            Note2 := MDI_Notebook (W);
         end if;

         if Note2 = null then
            Note2 := Create_Notebook (MDI);
            Show_All (Note2);
            Split (Pane,
                   Ref_Widget  => Note,
                   New_Child   => Note2,
                   Width       => Width,
                   Height      => Height,
                   Orientation => Orientation,
                   After       => After);
         end if;

         Show (Note2);

         Ref (Target);
         Give_Focus_To_Previous_Child (Target);
         Remove (Note, Target);
         Put_In_Notebook (MDI, Target, Note2);
         Unref (Target);
         Set_Focus_Child (Target);

         Emit_By_Name
           (Get_Object (MDI),
            String (Signal_Children_Reorganized) & ASCII.NUL);
      end if;

      if Traces then
         Print_Debug ("After split " & Gtk_Orientation'Image (Orientation));
         Dump (MDI);
      end if;
   end Split;

   ----------------
   -- Split_H_Cb --
   ----------------

   procedure Split_H_Cb (MDI : access Gtk_Widget_Record'Class) is
      M : constant MDI_Window := MDI_Window (MDI);
   begin
      --  Do nothing unless the current child is in the central area, since
      --  otherwise this is disturbing for the user

      if M.Focus_Child /= null
        and then M.Focus_Child.State = Normal
      then
         Split (M, Orientation => Orientation_Horizontal);
      end if;

   exception
      when E : others =>
         Print_Debug
           ("Unexpected exception: " & Exception_Information (E));
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
         Print_Debug
           ("Unexpected exception: " & Exception_Information (E));
   end Split_V_Cb;

   -----------------
   -- Maximize_Cb --
   -----------------

   procedure Maximize_Cb (MDI : access Gtk_Widget_Record'Class)
   is
      Child : constant MDI_Child := MDI_Window (MDI).Focus_Child;
   begin
      if Child /= null
        and then not Maximize_Action.Is_Maximized_Mode (MDI_Window (MDI))
      then
         Widget_Callback.Emit_By_Name (Child, Signal_Maximize_Child);
      end if;
   exception
      when E : others =>
         Print_Debug
           ("Unexpected exception: " & Exception_Information (E));
   end Maximize_Cb;

   -------------------
   -- Unmaximize_Cb --
   -------------------

   procedure Unmaximize_Cb (MDI : access Gtk_Widget_Record'Class)
   is
      Child : constant MDI_Child := MDI_Window (MDI).Focus_Child;
   begin
      if Child /= null
        and then Maximize_Action.Is_Maximized_Mode (MDI_Window (MDI))
      then
         Widget_Callback.Emit_By_Name (Child, Signal_Unmaximize);
      end if;
   exception
      when E : others =>
         Print_Debug
           ("Unexpected exception: " & Exception_Information (E));
   end Unmaximize_Cb;

   ---------------------------------------
   -- On_Select_Child_Update_Close_Menu --
   ---------------------------------------

   procedure On_Select_Child_Update_Close_Menu
      (Item   : access Gtk_Widget_Record'Class;
       Params : GValues)
   is
      C : constant MDI_Child :=
         MDI_Child (Get_User_Data_Or_Null (To_Address (Params, 1)));
   begin
      if C = null then
         Item.Set_Sensitive (False);
      else
         Item.Set_Sensitive ((C.Flags and Destroy_Button) /= 0);
      end if;
   end On_Select_Child_Update_Close_Menu;

   --------------------------------
   -- On_Float_Child_Update_Menu --
   --------------------------------

   procedure On_Float_Child_Update_Menu
      (Check : access Gtk_Widget_Record'Class)
   is
      C : constant MDI_Check_Menu_Item := MDI_Check_Menu_Item (Check);
      Focus : constant MDI_Child := C.MDI.Get_Focus_Child;
   begin
      C.MDI.Freeze_Float_Menu := True;

      --  Update the child's tab color if the child has been unfloated
      if Focus /= null and then Focus.State = Normal then
         Update_Tab_Color
           (Note    => Get_Notebook (Focus),
            Focused => True);
      end if;

      C.Set_Active (Focus /= null and then Focus.State = Floating);
      C.MDI.Freeze_Float_Menu := False;
   end On_Float_Child_Update_Menu;

   --------------
   -- Float_Cb --
   --------------

   procedure Float_Cb (MDI : access GObject_Record'Class) is
      C : constant MDI_Child := Get_Focus_Child (MDI_Window (MDI));
   begin
      if not MDI_Window (MDI).Freeze_Float_Menu and then C /= null then
         Float_Child (C, C.State /= Floating);
         Set_Focus_Child (C);
         Raise_Child (C, False);
      end if;
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
         Print_Debug ("Unexpected exception: " & Exception_Information (E));
   end Close_Cb;

   --------------
   -- Focus_Cb --
   --------------

   procedure Focus_Cb (Item  : access Gtk_Check_Menu_Item_Record'Class) is
      It : constant Menu_Item_For_Child := Menu_Item_For_Child (Item);
   begin
      if It.Child.MDI.Internal_Updating_Menu then
         return;
      end if;

      if It.Get_Active then
         --  If C is floating, raise the window.
         if It.Child.State = Floating then
            Raise_Child (It.Child, True);
         end if;
         Set_Focus_Child (It.Child);
      end if;
   end Focus_Cb;

   -----------------------------------------
   -- Get_Sorted_List_Of_Visible_Children --
   -----------------------------------------

   procedure Get_Sorted_List_Of_Visible_Children
      (MDI  : not null access MDI_Window_Record'Class;
       Vec  : out Child_Vectors.Vector)
   is
      use Child_Vectors;
      Tmp   : Widget_List.Glist;
      Child : MDI_Child;
   begin
      Vec.Clear;
      Tmp := First (MDI.Items);
      while Tmp /= Null_List loop
         Child := MDI_Child (Get_Data (Tmp));
         if Child.State /= Invisible then
            Vec.Append (Child);
         end if;
         Tmp := Next (Tmp);
      end loop;

      Vector_Sort.Sort (Vec);
   end Get_Sorted_List_Of_Visible_Children;

   -----------------------------------
   -- On_Child_Selected_Update_Menu --
   -----------------------------------

   procedure On_Child_Selected_Update_Menu
      (Item : access Gtk_Widget_Record'Class)
   is
      It : constant Menu_Item_For_Child := Menu_Item_For_Child (Item);
   begin
      It.Child.MDI.Internal_Updating_Menu := True;
      It.Set_Active (It.Child = It.Child.MDI.Get_Focus_Child);
      It.Child.MDI.Internal_Updating_Menu := False;
   end On_Child_Selected_Update_Menu;

   -----------------------------
   -- Internal_Add_Child_Menu --
   -----------------------------

   procedure Internal_Add_Child_Menu
     (Menu  : access MDI_Menu_Record'Class;
      Child : not null access MDI_Child_Record'Class;
      Group : in out Widget_SList.GSlist)
   is
      It : Gtk_Radio_Menu_Item;
   begin
      It := new Menu_Item_For_Child_Record'
        (Gtk_Radio_Menu_Item_Record with Child => MDI_Child (Child));
      Gtk.Radio_Menu_Item.Initialize (It, Group, "");
      Group := It.Get_Group;

      --  Insert the new child's menu item in alphabetical order

      declare
         Children  : Widget_List.Glist := Menu.Get_Children;
         Idx       : Gint := 0;
         Widget    : Gtk_Widget;
         Menu_Item : Menu_Item_For_Child;
      begin
         while Children /= Null_List loop
            Widget := Get_Data (Children);

            if Widget.all in Menu_Item_For_Child_Record'Class then
               Menu_Item := Menu_Item_For_Child (Widget);

               exit when Short_Title_Less_Than
                 (MDI_Child (Child), Menu_Item.Child);
            end if;

            Idx := Idx + 1;
            Children := Next (Children);
         end loop;

         Free (Children);

         Menu.Insert (It, Idx);
      end;

      Internal_Update_Menu_Content (Child, It);

      It.On_Toggled (Focus_Cb'Access, After => True);
      Widget_Callback.Object_Connect
        (Menu.MDI, Signal_Child_Selected,
         On_Child_Selected_Update_Menu'Access, It,
         After => True);

      It.Set_Accel_Path
        (Child.MDI.Accel_Path_Prefix.all
         & "/window/child/" & Child.Short_Title.all,
         Child.MDI.Group);
   end Internal_Add_Child_Menu;

   ----------------------------------
   -- Internal_Update_Menu_Content --
   ----------------------------------

   procedure Internal_Update_Menu_Content
     (Child : access MDI_Child_Record'Class;
      It    : not null access Gtk_Radio_Menu_Item_Record'Class)
   is
      Box   : Gtk_Box;
      Label : Gtk_Accel_Label;
      Icon  : Gtk_Image;
   begin
      It.Remove (It.Get_Child);

      Gtk_New_Hbox (Box, Homogeneous => False, Spacing => 5);
      It.Add (Box);

      Icon := Get_Icon (Child);
      if Icon /= null then
         Box.Pack_Start (Icon, Expand => False);
      end if;

      Gtk_New (Label, Child.Short_Title.all);
      Label.Set_Alignment (0.0, 0.5);
      Label.Set_Accel_Widget (It);
      Box.Pack_Start (Label, Expand => True, Fill => True);
      It.Set_Active (Child = Child.MDI.Focus_Child);

      --  Refresh graphically the menu
      It.Show_All;
   end Internal_Update_Menu_Content;

   ---------------------
   -- Find_Child_Menu --
   ---------------------

   function Find_Child_Menu
     (Menu  : access MDI_Menu_Record'Class;
      Child : not null access MDI_Child_Record'Class)
      return Menu_Item_For_Child
   is
      Children, L : Widget_List.Glist;
      W           : Gtk_Widget;
   begin
      Children := Menu.Get_Children;
      L := Children;
      while L /= Null_List loop
         W := Get_Data (L);
         L := Next (L);
         if W.all in Menu_Item_For_Child_Record'Class then
            declare
               It : constant Menu_Item_For_Child := Menu_Item_For_Child (W);
            begin
               if It.Child = Child then
                  return It;
               end if;
            end;
         end if;
      end loop;

      return null;
   end Find_Child_Menu;

   --------------------
   -- Add_Child_Menu --
   --------------------

   procedure Add_Child_Menu
     (Self  : access Glib.Object.GObject_Record'Class;
      Child : not null access MDI_Child_Record'Class)
   is
      Menu        : constant MDI_Menu := MDI_Menu (Self);
      G           : Widget_SList.GSlist := Widget_SList.Null_List;
      Children, L : Widget_List.Glist;
      W           : Gtk_Widget;
   begin
      Children := Menu.Get_Children;
      L := Children;
      while L /= Null_List loop
         W := Get_Data (L);
         L := Next (L);
         if W.all in Menu_Item_For_Child_Record'Class then
            G := Menu_Item_For_Child (W).Get_Group;
            exit;
         end if;
      end loop;
      Free (Children);

      if Find_Child_Menu (Menu, Child) = null then
         Internal_Add_Child_Menu (Menu, Child, G);
      end if;
   end Add_Child_Menu;

   -----------------------
   -- Remove_Child_Menu --
   -----------------------

   procedure Remove_Child_Menu
     (Self  : access Glib.Object.GObject_Record'Class;
      Child : not null access MDI_Child_Record'Class)
   is
      Menu : constant MDI_Menu := MDI_Menu (Self);
   begin
      if Self = null then
         return;
      end if;

      declare
         It : constant Menu_Item_For_Child := Find_Child_Menu (Menu, Child);
      begin
         if It /= null then
            It.Destroy;
         end if;
      end;
   end Remove_Child_Menu;

   -----------------------
   -- Update_Child_Menu --
   -----------------------

   procedure Update_Child_Menu
     (Self  : access Glib.Object.GObject_Record'Class;
      Child : not null access MDI_Child_Record'Class)
   is
      Menu : constant MDI_Menu := MDI_Menu (Self);
   begin
      if Self = null then
         return;
      end if;

      declare
         It : constant Menu_Item_For_Child := Find_Child_Menu (Menu, Child);
      begin
         if It /= null then
            Internal_Update_Menu_Content (Child, It);
         end if;
      end;
   end Update_Child_Menu;

   --------------------
   -- Recompute_Menu --
   --------------------

   procedure Recompute_Menu
      (Menu : access Gtk_Widget_Record'Class)
   is
      M : constant MDI_Menu := MDI_Menu (Menu);

      use Child_Vectors;
      G           : Widget_SList.GSlist := Widget_SList.Null_List;
      Vec         : Child_Vectors.Vector;
      Curs        : Child_Vectors.Cursor;
      Child       : MDI_Child;
      Children, L : Widget_List.Glist;
      W           : Gtk_Widget;
   begin
      --  Remove all items

      Children := M.Get_Children;
      L := Children;
      while L /= Null_List loop
         W := Get_Data (L);
         L := Next (L);
         if W.all in Menu_Item_For_Child_Record'Class then
            W.Destroy;
         end if;
      end loop;
      Free (Children);

      --  Add the list of children

      Get_Sorted_List_Of_Visible_Children (M.MDI, Vec);

      Curs := First (Vec);
      while Has_Element (Curs) loop
         Child := Element (Curs);
         Internal_Add_Child_Menu (M, Child, G);
         Next (Curs);
      end loop;

      M.Show_All;
   end Recompute_Menu;

   ---------------------
   -- Set_Focus_Child --
   ---------------------

   procedure Set_Focus_Child
     (MDI        : access MDI_Window_Record;
      Containing : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Parent : Gtk_Widget := Gtk_Widget (Containing);
   begin
      if Containing = null then
         MDI.Focus_Child := null;
         Child_Selected (MDI, null);
      else
         while Parent /= null
           and then not (Parent.all in MDI_Child_Record'Class)
         loop
            Parent := Get_Parent (Parent);
         end loop;

         if Parent /= null then
            Set_Focus_Child (MDI_Child (Parent));
         end if;
      end if;
   end Set_Focus_Child;

   ------------------------
   -- On_Action_Floating --
   ------------------------

   procedure On_Action_Floating
      (MDI       : access GObject_Record'Class;
       Parameter : Glib.Variant.Gvariant)
   is
      pragma Unreferenced (Parameter);
   begin
      Float_Cb (MDI_Window (MDI));
   end On_Action_Floating;

   ---------------------
   -- On_Action_Close --
   ---------------------

   procedure On_Action_Close
      (MDI       : access GObject_Record'Class;
       Parameter : Glib.Variant.Gvariant)
   is
      pragma Unreferenced (Parameter);
   begin
      Close_Cb (MDI_Window (MDI));
   end On_Action_Close;

   -----------------------
   -- On_Action_Split_H --
   -----------------------

   procedure On_Action_Split_H
      (MDI       : access GObject_Record'Class;
       Parameter : Glib.Variant.Gvariant)
   is
      pragma Unreferenced (Parameter);
   begin
      Split_H_Cb (MDI_Window (MDI));
   end On_Action_Split_H;

   -----------------------
   -- On_Action_Split_V --
   -----------------------

   procedure On_Action_Split_V
      (MDI       : access GObject_Record'Class;
       Parameter : Glib.Variant.Gvariant)
   is
      pragma Unreferenced (Parameter);
   begin
      Split_V_Cb (MDI_Window (MDI));
   end On_Action_Split_V;

   ----------------------
   -- On_Action_Select --
   ----------------------

   procedure On_Action_Select
     (MDI       : access GObject_Record'Class;
      Parameter : Glib.Variant.Gvariant)
   is
      M      : constant MDI_Window := MDI_Window (MDI);
      Length : aliased Gsize;
      Name   : constant String := Get_String (Parameter, Length'Access);
      C      : constant MDI_Child := M.Find_MDI_Child_By_Name (Name);
   begin
      if C /= null then
         if C.State = Floating then
            Raise_Child (C, True);
         end if;
         C.Set_Focus_Child;
      end if;
   end On_Action_Select;

   -------------
   -- Desktop --
   -------------

   package body Desktop is

      Invalid_Desktop : exception;

      procedure Get_XML_For_Widget
        (Child            : MDI_Child;
         User             : User_Data;
         Widget_Is_Unique : out Boolean;
         Data             : out Node_Ptr);
      --  Get the XML node for a given widget. This automatically sets
      --  Child.XML_Node_Name as well.
      --  Widget_Is_Unique is set to True if there can be only one of the
      --  corresponding widget.

      procedure Parse_Child_Node
        (MDI         : access MDI_Window_Record'Class;
         Child_Node  : Node_Ptr;
         User        : User_Data;
         Focus_Child : in out MDI_Child;
         X           : out Gint;
         Y           : out Gint;
         W, H        : out Gint;
         Raised      : out Boolean;
         State       : out State_Type;
         Child       : out MDI_Child;
         To_Hide     : in out Gtk.Widget.Widget_List.Glist);
      --  Parse a <child> node and return the corresponding Child. The latter
      --  has not been inserted in the MDI.

      procedure Parse_Notebook_Node
        (MDI                   : access MDI_Window_Record'Class;
         Child_Node            : Node_Ptr;
         User                  : User_Data;
         Parent_Width, Parent_Height : Gint;
         Children_Count        : Integer;
         Parent_Orientation          : Gtk_Orientation;
         Focus_Child           : in out MDI_Child;
         Width, Height         : out Gint;
         Notebook              : out MDI_Notebook;
         To_Raise              : in out Gtk.Widget.Widget_List.Glist;
         To_Hide               : in out Gtk.Widget.Widget_List.Glist;
         Empty_Notebook_Filler : in out MDI_Child);
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
      --  Paned_Width and Paned_Height are the size of the multi_paned widget
      --  to which the "width" and "height" attributes are relative.

      procedure Parse_Pane_Node
        (Paned                 : access Gtkada_Multi_Paned_Record'Class;
         MDI                   : access MDI_Window_Record'Class;
         Node                  : Node_Ptr;
         Focus_Child           : in out MDI_Child;
         Parent_Width, Parent_Height : Gint;
         Parent_Children_Count : Integer;
         Parent_Orientation    : Gtk_Orientation;
         User                  : User_Data;
         Initial_Ref_Child     : MDI_Notebook := null;
         To_Raise              : in out Gtk.Widget.Widget_List.Glist;
         To_Hide               : in out Gtk.Widget.Widget_List.Glist;
         Empty_Notebook_Filler : in out MDI_Child);
      --  Parse a <Pane> node
      --  First_Child is the first notebook insert in pane (possibly inserted
      --  From_Tree points to the project-specific part of the desktop, where
      --  the contents of the children are saved.

      procedure Restore_Multi_Pane
        (Pane                    : access Gtkada_Multi_Paned_Record'Class;
         MDI                     : access MDI_Window_Record'Class;
         Focus_Child             : in out MDI_Child;
         To_Raise                : in out Gtk.Widget.Widget_List.Glist;
         To_Hide                 : in out Gtk.Widget.Widget_List.Glist;
         Node                    : Node_Ptr;
         User                    : User_Data;
         Full_Width, Full_Height : Gint);
      --  Restore a multi paned widget (either the perspective or the contents
      --  of the editor area)
      --  From_Tree points to the project-specific part of the desktop, where
      --  the contents of the children are saved.

      procedure Internal_Load_Perspective
        (MDI              : access MDI_Window_Record'Class;
         Name             : String;
         User             : User_Data;
         Focus_Child      : in out MDI_Child;
         To_Raise         : in out Gtk.Widget.Widget_List.Glist;
         To_Hide          : in out Gtk.Widget.Widget_List.Glist;
         MDI_Width, MDI_Height : Gint;
         Do_Size_Allocate : Boolean);
      --  Internal version of Load_Perspective.
      --  If Name is "", the first perspective is loaded.

      procedure Compute_Size_From_Attributes
        (MDI                         : access MDI_Window_Record'Class;
         Node                        : Node_Ptr;
         Parent_Width, Parent_Height : Gint;
         Parent_Orientation          : Gtk_Orientation;
         Width, Height               : out Gint;
         Children_Count              : Integer := 1);
      --  Compute the actual size of the widget represented by node, from the
      --  attributes of the node ("width" and "height", which use percent of
      --  the total pane size), and the attributes of the parent container.
      --  Children_Count is the number of children for the widget represented
      --  by Node, since the size returned is the one really available for
      --  sharing between the children (thus omitting the resize handles)

      type MDI_Menu_Item_Record is new Gtk_Menu_Item_Record with record
         MDI  : MDI_Window;
         User : User_Data;
      end record;
      type MDI_Menu_Item is access all MDI_Menu_Item_Record'Class;
      --  A menu item that stores a reference to the MDI and user data

      procedure Update_Perspective_Menu_Model
         (MDI : not null access MDI_Window_Record'Class);
      --  Update the menu model for /Window/Perspectives
      --
      procedure Create_Perspective_Query_Name
        (MDI  : not null access MDI_Window_Record'Class;
         User : User_Data);
      --  Ask the user for the name of a new perspective, and create it

      procedure Recompute_Perspective_Names
        (MDI : access MDI_Window_Record'Class);
      --  Recompute the name of all perspectives, and cache them

      package MDI_User_Data_Cb is new Gtk.Handlers.User_Callback
         (MDI_Window_Record, User_Data);

      ------------------------
      -- Change_Perspective --
      ------------------------

      procedure Change_Perspective
        (Item : access Gtk_Widget_Record'Class)
      is
         Persp : constant Perspective_Menu_Item :=
           Perspective_Menu_Item (Item);

         Name : constant String :=
           Persp.MDI.Perspective_Names (Persp.Name).all;
         --  Make a copy of the name, since Load_Perspective changes
         --  Persp.MDI.Perspective_Names
      begin
         if Get_Active (Persp) then
            Print_Debug ("++++ Change_Perspective to " & Name
                         & Integer'Image (Persp.Name));
            if not Persp.MDI.Loading_Desktop then
               Load_Perspective (Persp.MDI, Name, Persp.User);
            end if;
         end if;
      end Change_Perspective;

      -----------------------------------
      -- Create_Perspective_Query_Name --
      -----------------------------------

      procedure Create_Perspective_Query_Name
        (MDI  : not null access MDI_Window_Record'Class;
         User : User_Data)
      is
         Dialog : Gtk_Dialog;
         Label  : Gtk_Label;
         Ent    : Gtk_Entry;
         Button : Gtk_Widget with Unreferenced;

      begin
         Gtk_New (Dialog, Title => "Enter perspective name",
                  Parent => Gtk_Window (Get_Toplevel (MDI)),
                  Flags  => Modal and Destroy_With_Parent);
         Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
         Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
         Set_Default_Response (Dialog, Gtk_Response_OK);

         Gtk_New (Label, "Enter name of new perspective:");
         Pack_Start (Get_Content_Area (Dialog), Label, Expand => False);

         Gtk_New (Ent);
         Set_Activates_Default (Ent, True);
         Pack_Start (Get_Content_Area (Dialog), Ent, Expand => False);

         Show_All (Dialog);

         if Run (Dialog) = Gtk_Response_OK then
            Create_Perspective (MDI, Get_Text (Ent), User);
         end if;

         Destroy (Dialog);
      end Create_Perspective_Query_Name;

      ---------------------------
      -- Create_Perspective_CB --
      ---------------------------

      procedure Create_Perspective_CB
        (Item : access Gtk_Widget_Record'Class)
      is
         Persp : constant Perspective_Menu_Item :=
           Perspective_Menu_Item (Item);
      begin
         Create_Perspective_Query_Name (Persp.MDI, Persp.User);
      end Create_Perspective_CB;

      ------------------------
      -- Create_Perspective --
      ------------------------

      procedure Create_Perspective
        (MDI          : access MDI_Window_Record'Class;
         Name         : String;
         User         : User_Data)
      is
         Perspectives, Central : Node_Ptr;
      begin
         MDI.Current_Perspective := null;
         Save_Desktop (MDI, User, Perspectives, Central);
         Set_Attribute (MDI.Current_Perspective, "name", Name);
         Free (Perspectives);
         Free (Central);

         Recompute_Perspective_Names (MDI);
         Update_Perspective_Menu_Model (MDI);
         Emit_By_Name_Str
           (Get_Object (MDI),
            String (Signal_Perspectives_Added) & ASCII.NUL,
            Name & ASCII.NUL);
      end Create_Perspective;

      ------------------------
      -- Define_Perspective --
      ------------------------

      procedure Define_Perspective
        (MDI          : access MDI_Window_Record'Class;
         XML          : Glib.Xml_Int.Node_Ptr;
         User         : User_Data)
      is
         pragma Unreferenced (User);
         Name : constant String := Get_Attribute (XML, "name");
         Tmp : Node_Ptr;
      begin
         if Name = "" or else MDI.Perspectives = null then
            return;
         end if;

         Tmp := MDI.Perspectives.Child;

         while Tmp /= null loop
            if Get_Attribute (Tmp, "name") = Name then
               --  Perspective already exists
               return;
            end if;

            Tmp := Tmp.Next;
         end loop;

         Add_Child (MDI.Perspectives, Deep_Copy (XML), Append => True);
         Update_Perspective_Menu_Model (MDI);
         Emit_By_Name_Str
           (Get_Object (MDI),
            String (Signal_Perspectives_Added) & ASCII.NUL,
            Name & ASCII.NUL);
      end Define_Perspective;

      ----------------------------------------
      -- On_Perspective_Changed_Update_Menu --
      ----------------------------------------

      procedure On_Perspective_Changed_Update_Menu
         (Menu : access Gtk_Widget_Record'Class)
      is
         M : constant MDI_Menu_Item := MDI_Menu_Item (Menu);
         Submenu : Gtk_Menu;
         Persp   : Perspective_Menu_Item;
         Group   : Widget_SList.GSlist := Widget_SList.Null_List;
      begin
         Print_Debug ("Create_Perspective_Menu", Debug_Increase);

         --  Prevent changing perspective when setting "Active" on the buttons
         M.MDI.Loading_Desktop := True;

         Gtk_New (Submenu);
         Set_Submenu (M, Submenu);

         if M.MDI.Perspective_Names /= null then
            for N in M.MDI.Perspective_Names'Range loop
               Persp := new Perspective_Menu_Item_Record;
               Persp.MDI  := M.MDI;
               Persp.Name := N;
               Persp.User := M.User;

               Initialize (Persp, Group, M.MDI.Perspective_Names (N).all);
               Set_Active
                  (Persp,
                   M.MDI.Current_Perspective /= null
                   and then M.MDI.Perspective_Names (N).all =
                     Get_Attribute (M.MDI.Current_Perspective, "name"));
               Group := Get_Group (Persp);
               Append (Submenu, Persp);
               Widget_Callback.Connect
                 (Persp, Gtk.Menu_Item.Signal_Activate, CP_Access);
            end loop;
         end if;

         Persp := new Perspective_Menu_Item_Record;
         Persp.MDI  := M.MDI;
         Persp.User := M.User;
         Gtk.Menu_Item.Initialize (Persp, "<create new>");

         Widget_Callback.Connect
           (Persp, Gtk.Menu_Item.Signal_Activate, CreateP_Access);
         Append (Submenu, Persp);

         Show_All (Submenu);
         Show (Menu);

         M.MDI.Loading_Desktop := False;
         Print_Debug ("", Debug_Decrease);
      end On_Perspective_Changed_Update_Menu;

      -----------------------------------
      -- Update_Perspective_Menu_Model --
      -----------------------------------

      procedure Update_Perspective_Menu_Model
         (MDI : not null access MDI_Window_Record'Class)
      is
      begin
         if MDI.Perspectives_Menu /= null then
            MDI.Perspectives_Menu.Remove_All;

            if MDI.Perspective_Names /= null then
               for N in MDI.Perspective_Names'Range loop
                  MDI.Perspectives_Menu.Append
                      (MDI.Perspective_Names (N).all,
                       "app.mdi_set_perspective("""
                       & MDI.Perspective_Names (N).all
                       & """)");
               end loop;
            end if;

            MDI.Perspectives_Menu.Append
               ("<create new>", "app.mdi_create_perspective");

            MDI.Action_Select_Perspective.Set_State
               (Gvariant_New_String
                   (Get_Attribute (MDI.Current_Perspective, "name")));
         end if;

         Emit_By_Name
           (Get_Object (MDI), String (Signal_Perspective_Changed) & ASCII.NUL);
      end Update_Perspective_Menu_Model;

      -----------------
      -- Create_Menu --
      -----------------

      function Create_Menu
        (MDI               : access MDI_Window_Record'Class;
         Accel_Path_Prefix : String := "<gtkada>";
         User              : User_Data;
         Registration      : Menu_Registration_Procedure := null)
         return Gtk.Menu.Gtk_Menu
      is
         Menu  : MDI_Menu;
         Item  : Gtk_Menu_Item;
         Check : Gtk_Check_Menu_Item;
         Sep   : Gtk_Separator_Menu_Item;

         procedure Connect_Menu
           (Item       : Gtk_Menu_Item;
            Callback   : Widget_Callback.Marshallers.Void_Marshaller.Handler;
            Accel_Path : String);
         --  Utility function, factorizes code

         ------------------
         -- Connect_Menu --
         ------------------

         procedure Connect_Menu
           (Item       : Gtk_Menu_Item;
            Callback   : Widget_Callback.Marshallers.Void_Marshaller.Handler;
            Accel_Path : String)
         is
            Full_Accel_Path : constant String :=
                                Accel_Path_Prefix & "/window/" & Accel_Path;
         begin
            Append (Menu, Item);
            Widget_Callback.Object_Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               Widget_Callback.To_Marshaller (Callback), MDI);
            Set_Accel_Path (Item, Full_Accel_Path, MDI.Group);
            if Registration /= null then
               Registration (User, Get_Label (Item), Full_Accel_Path);
            end if;
         end Connect_Menu;

      begin
         if MDI.Accel_Path_Prefix = null then
            MDI.Accel_Path_Prefix := new String'(Accel_Path_Prefix);
         end if;

         Menu := new MDI_Menu_Record;
         Menu.MDI := MDI_Window (MDI);
         Gtk.Menu.Initialize (Menu);
         Menu.Set_Name ("gtkada-mdi-children-menu");

         Item := new MDI_Menu_Item_Record'
            (Gtk_Menu_Item_Record with
             MDI => MDI_Window (MDI), User => User);
         Gtk.Menu_Item.Initialize (Item, "Perspectives");
         Append (Menu, Item);
         Widget_Callback.Object_Connect
            (MDI, Signal_Perspective_Changed,
             On_Perspective_Changed_Update_Menu_Access, Item);
         On_Perspective_Changed_Update_Menu (Item);

         Gtk_New (Item, "Split Side-by-Side");
         Connect_Menu (Item, Split_H_Cb'Access, "split_horizontal");

         Gtk_New (Item, "Split Up-Down");
         Connect_Menu (Item, Split_V_Cb'Access, "split_vertical");

         Gtk_New (Sep);
         Append (Menu, Sep);

         Gtk_New (Item, "Maximize");
         Connect_Menu (Item, Maximize_Cb'Access, "maximize_current_child");

         Gtk_New (Item, "Unmaximize");
         Connect_Menu (Item, Unmaximize_Cb'Access, "unmaximize_current_child");

         Check := new MDI_Check_Menu_Item_Record'
            (Gtk_Check_Menu_Item_Record with MDI => MDI_Window (MDI));
         Gtk.Check_Menu_Item.Initialize (Check, "Floating");
         Append (Menu, Check);
         Check.On_Toggled (Float_Cb'Access, MDI);
         Check.Set_Accel_Path
            (Accel_Path_Prefix & "/window/floating", MDI.Group);
         Widget_Callback.Object_Connect
            (MDI, Signal_Float_Child,
             Widget_Callback.To_Marshaller
                (On_Float_Child_Update_Menu'Access), Check);
         Widget_Callback.Object_Connect
            (MDI, Signal_Unfloat_Child,
             Widget_Callback.To_Marshaller
                (On_Float_Child_Update_Menu'Access), Check);

         Gtk_New (Sep);
         Append (Menu, Sep);

         Gtk_New (Item, "Close");
         Connect_Menu (Item, Close_Cb'Access, "close");
         Widget_Callback.Object_Connect
            (MDI, Signal_Child_Selected,
             On_Select_Child_Update_Close_Menu'Access, Item);

         Gtk_New (Sep);
         Append (Menu, Sep);

         Recompute_Menu (Menu);
         MDI.On_Child_Removed (Remove_Child_Menu'Access, Slot => Menu);
         MDI.On_Child_Title_Changed (Add_Child_Menu'Access, Slot => Menu);
         MDI.On_Child_Icon_Changed (Update_Child_Menu'Access, Slot => Menu);
         MDI.On_Child_Title_Changed (Update_Child_Menu'Access, Slot => Menu);
         Widget_Callback.Object_Connect
            (MDI, Signal_Perspective_Changed, Recompute_Menu'Access, Menu);

         Show_All (Menu);
         return Gtk_Menu (Menu);
      end Create_Menu;

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

      ----------------------------------
      -- Compute_Size_From_Attributes --
      ----------------------------------

      procedure Compute_Size_From_Attributes
        (MDI                         : access MDI_Window_Record'Class;
         Node                        : Node_Ptr;
         Parent_Width, Parent_Height : Gint;
         Parent_Orientation          : Gtk_Orientation;
         Width, Height               : out Gint;
         Children_Count              : Integer := 1)
      is
         WAttr : constant String := Get_Attribute (Node, "width", "100%");
         HAttr : constant String := Get_Attribute (Node, "height", "100%");
         Tmp   : Gint;
         Handle_Size : constant Gint := MDI.Handle_Size;
      begin
         if WAttr (WAttr'Last) /= '%'
           or else HAttr (HAttr'Last) /= '%'
         then
            raise Invalid_Desktop with
              "Old-format perspectives are no longer supported";
         end if;

         --  For backward compatibility, we accept absolute sizes in the XML
         --  nodes, but that might lead to inconsistencies (and incorrect
         --  reload of desktop) if the user modifies this by hand
         --
         --  Depending on the orientation of the parent, one of the dimensions
         --  is in fact fixed (the full height or width of the parent).
         --
         --  If there are multiple children (case of panes for instance), the
         --  size we return is the one really available for children, not the
         --  physical size of the pane itself.

         case Parent_Orientation is
            when Orientation_Horizontal =>
               Height := Parent_Height;
               Tmp :=
                 Parent_Width - Gint (Children_Count - 1) * Handle_Size;
               Width  := Gint
                 (Float'Value (WAttr (WAttr'First .. WAttr'Last - 1))
                  * Float (Tmp) / 100.0);

            when Orientation_Vertical =>
               Width := Parent_Width;
               Tmp :=
                 Parent_Height - Gint (Children_Count - 1) * Handle_Size;
               Height := Gint
                 (Float'Value (HAttr (HAttr'First .. HAttr'Last - 1))
                  * Float (Tmp) / 100.0);
         end case;

         if Traces then
            Print_Debug
              ("Compute_Size_From_Attributes WAttr=" & WAttr
               & " HAttr=" & HAttr
               & " children=" & Integer'Image (Children_Count)
               & " => size " & Gint'Image (Width) & Gint'Image (Height));
         end if;
      end Compute_Size_From_Attributes;

      -------------------------
      -- Parse_Notebook_Node --
      -------------------------

      procedure Parse_Notebook_Node
        (MDI                         : access MDI_Window_Record'Class;
         Child_Node                  : Node_Ptr;
         User                        : User_Data;
         Parent_Width, Parent_Height : Gint;
         Children_Count              : Integer;
         Parent_Orientation          : Gtk_Orientation;
         Focus_Child                 : in out MDI_Child;
         Width, Height               : out Gint;
         Notebook                    : out MDI_Notebook;
         To_Raise                    : in out Gtk.Widget.Widget_List.Glist;
         To_Hide                     : in out Gtk.Widget.Widget_List.Glist;
         Empty_Notebook_Filler       : in out MDI_Child)
      is
         N            : Node_Ptr := Child_Node.Child;
         State        : State_Type;
         Raised       : Boolean;
         Raised_Child : MDI_Child;
         Child        : MDI_Child;
         X, Y, W, H   : Gint;
         Dummy        : Gtk_Label;
         Pos          : Gtk_Position_Type;

      begin
         if Traces then
            Print_Debug ("Parse_Notebook_Node Parent_Width="
                         & Gint'Image (Parent_Width) & " Parent_Height="
                         & Gint'Image (Parent_Height) & " Parent_Orientation="
                         & Gtk_Orientation'Image (Parent_Orientation),
                         Debug_Increase);
         end if;

         Compute_Size_From_Attributes
           (MDI, Child_Node, Parent_Width, Parent_Height, Parent_Orientation,
            Width, Height, Children_Count => Children_Count);

         Pos    := Gtk_Position_Type'Value
           (Get_Attribute (Child_Node, "Tabs",
            Gtk_Position_Type'Image (MDI.Tabs_Position)));

         Notebook := Create_Notebook (MDI);
         Print_Debug
           ("Parse_Notebook_Node: created new notebook "
            & System.Address_Image (Notebook.all'Address));

         Notebook.Tab_Orientation :=
           Tab_Orientation_Type'Value
             (Get_Attribute (Child_Node, "orientation",
              Tab_Orientation_Type'Image (Automatic)));

         --  Make sure Width and Height are not too small: that could happen
         --  if the main window has not been resized yet (thus has a size 1x1)
         --  and we load a perspective (since keeping place for the children
         --  windows might end up with negative sizes.

         Width := Gint'Max (Width, -1);
         Height := Gint'Max (Height, -1);

         On_Tab_Pos (Notebook, Pos);
         Set_Child_Visible (Notebook, True);
         Show_All (Notebook);

         while N /= null loop
            if N.Tag.all = "Child" then
               Parse_Child_Node
                 (MDI, N, User, Focus_Child, X, Y, W, H,
                  Raised, State, Child, To_Hide => To_Hide);

               --  Child cannot be floating while in a notebook
               if Child /= null then
                  if Raised
                    or else Raised_Child = null
                    or else Focus_Child = Child
                  then
                     Raised_Child := Child;
                  end if;

                  Print_Debug
                    ("Parse_Notebook_Node, moving child into the"
                     & " the notebook");
                  Float_Child (Child, False);
                  Put_In_Notebook (MDI, Child, Notebook);
                  Print_Debug
                    ("Parse_Notebook_Node, done moving child");
               else
                  Print_Debug ("Parse_Notebook_Node: no child created");
               end if;

            else
               --  Invalid node
               null;
            end if;

            N := N.Next;
         end loop;

         Print_Debug ("Parse_Notebook_Node: done adding all children");

         --  Create a dummy node if necessary, since otherwise the calls to
         --  Split afterward will simply discard that notebook. This dummy
         --  widget is destroyed at the end of restoring the desktop

         if Child_Node.Child = null and then Empty_Notebook_Filler = null then
            Gtk_New (Dummy, "");
            Gtk_New (Empty_Notebook_Filler, Dummy);
            Set_Title (Empty_Notebook_Filler, "<Dummy, notebook filler>");
            Put (MDI, Empty_Notebook_Filler);
            Put_In_Notebook (MDI, Empty_Notebook_Filler, Notebook);
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

         Print_Debug ("", Debug_Decrease);
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
         W, H        : out Gint;
         Raised      : out Boolean;
         State       : out State_Type;
         Child       : out MDI_Child;
         To_Hide     : in out Gtk.Widget.Widget_List.Glist)
      is
         N        : Node_Ptr;
         Register : Register_Node;
         Visible  : constant Boolean := Boolean'Value
           (Get_Attribute (Child_Node, "visible", "true"));
         Iter     : Child_Iterator;
         Tmp      : MDI_Child;
      begin
         Print_Debug ("Parse_Child_Node", Debug_Increase);

         W        := -1;
         H        := -1;
         Child    := null;
         Raised   := False;
         State    := Normal;
         X        := 0;
         Y        := 0;

         --  Check whether this child was already in a previous perspective.
         --  If that's the case, reuse it

         Iter := First_Child (MDI, Visible_Only => False);
         loop
            Tmp := Get (Iter);
            exit when Tmp = null;

            --  If not already used in the perspective

            if Tmp.State = Invisible
              and then Tmp.XML_Node_Name /= null
              and then Tmp.XML_Node_Name.all = Child_Node.Child.Tag.all
            then
               Print_Debug ("Reusing existing hidden view for "
                            & Child_Node.Child.Tag.all);
               Child := Tmp;
               Put (MDI, Child);  --  put it back in the MDI
               exit;
            end if;

            Next (Iter);
         end loop;

         --  Is there data associated with the node (in particular for widgets
         --  in the central area)

         if Child = null
           and then (Child_Node.Child.Child /= null
                     or else (Child_Node.Child.Attributes /= null
                              and then Child_Node.Child.Attributes.all /= ""))
         then
            Register := Registers;
            while Child = null and then Register /= null loop
               Child := Register.Load
                 (MDI_Window (MDI), Child_Node.Child, User);
               Register := Register.Next;
            end loop;
         end if;

         --  Check whether we have a project-specific contents for this child.
         --  This always takes priority other any project-independent contents.
         --  When we have multiple children with the same XML node name, we
         --  should use the first project-dependent part, then the second,...,
         --  and not reuse multiple times the first one. To do this, we simply
         --  remove the nodes from the project-dependent part as we use them,
         --  which also saves memory.

         N := MDI.View_Contents;
         if Child = null and then N /= null then
            N := N.Child;
            while N /= null loop
               if N.Tag.all = Child_Node.Child.Tag.all then
                  Register := Registers;
                  while Child = null and then Register /= null loop
                     Child := Register.Load (MDI_Window (MDI), N, User);
                     Register := Register.Next;
                  end loop;

                  if Child /= null then
                     Print_Debug ("Found project-specific contents for "
                                  & Child_Node.Child.Tag.all);

                     Free (N);
                     exit;
                  end if;
               end if;

               N := N.Next;
            end loop;
         end if;

         --  Else search for project-specific contents

         Register := Registers;

         while Child = null and then Register /= null loop
            Child := Register.Load (MDI_Window (MDI), Child_Node.Child, User);
            if Child /= null then
               Print_Debug ("Found project-independent contents for "
                            & Child_Node.Child.Tag.all);
            end if;
            Register := Register.Next;
         end loop;

         if Child = null then
            Print_Debug ("Parse_Child_Node: Could not create the child");
            return;
         end if;

         Print_Debug ("Parse_Child_Node: created " & Get_Title (Child));

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

            elsif N.Tag.all = "width" then
               W := Gint'Value (N.Value.all);

            elsif N.Tag.all = "height" then
               H := Gint'Value (N.Value.all);

            else
               --  ??? Unknown node, just ignore for now
               null;
            end if;

            N := N.Next;
         end loop;

         Set_Size (MDI, Widget => Child, Width => W, Height => H);

         if not Visible then
            Print_Debug ("Parse_Child_Node: child will be hidden");
            Prepend (To_Hide, Gtk_Widget (Child));
         end if;

         Print_Debug ("", Debug_Decrease);

         exception
            when E : others =>
               pragma Debug
                 (Put_Line
                    ("Unexpected exception: " & Exception_Information (E)));

               if Traces then
                  Print_Debug ("Unexpected exception "
                               & Exception_Information (E));
               end if;
      end Parse_Child_Node;

      ---------------------
      -- Get_XML_Content --
      ---------------------

      function Get_XML_Content
        (MDI : access MDI_Window_Record'Class;
         Tag : String) return Glib.Xml_Int.Node_Ptr
      is
         function Internal_Get_XML_Content
           (N : Glib.Xml_Int.Node_Ptr) return Glib.Xml_Int.Node_Ptr;

         ------------------------------
         -- Internal_Get_XML_Content --
         ------------------------------

         function Internal_Get_XML_Content
           (N : Glib.Xml_Int.Node_Ptr) return Glib.Xml_Int.Node_Ptr
         is
            Node  : Glib.Xml_Int.Node_Ptr := N;
            Child : Glib.Xml_Int.Node_Ptr;

         begin
            while Node /= null loop
               if Node.Tag.all = Tag then
                  return Node;
               end if;

               Child := Internal_Get_XML_Content (Node.Child);

               if Child /= null then
                  return Child;
               end if;

               Node := Node.Next;
            end loop;

            return null;
         end Internal_Get_XML_Content;

      begin
         return Internal_Get_XML_Content (MDI.View_Contents);
      end Get_XML_Content;

      ---------------------
      -- Parse_Pane_Node --
      ---------------------

      procedure Parse_Pane_Node
        (Paned                       : access Gtkada_Multi_Paned_Record'Class;
         MDI                         : access MDI_Window_Record'Class;
         Node                        : Node_Ptr;
         Focus_Child                 : in out MDI_Child;
         Parent_Width, Parent_Height : Gint;
         Parent_Children_Count       : Integer;
         Parent_Orientation          : Gtk_Orientation;
         User                        : User_Data;
         Initial_Ref_Child           : MDI_Notebook := null;
         To_Raise                    : in out Gtk.Widget.Widget_List.Glist;
         To_Hide                     : in out Gtk.Widget.Widget_List.Glist;
         Empty_Notebook_Filler       : in out MDI_Child)
      is
         Orientation : constant Gtk_Orientation := Gtk_Orientation'Value
           (Get_Attribute (Node, "Orientation"));
         N             : Node_Ptr;
         Ref_Item      : Gtk_Widget := Gtk_Widget (Initial_Ref_Child);
         Count         : constant Natural := Children_Count (Node);
         Notebook_Node : Node_Ptr;
         Width, Height : Gint;

         Width_For_Children  : Gint := Parent_Width;
         Height_For_Children : Gint := Parent_Height;

      begin
         Compute_Size_From_Attributes
           (MDI, Node, Parent_Width, Parent_Height, Parent_Orientation,
            Width_For_Children, Height_For_Children, Parent_Children_Count);

         if Traces then
            New_Line;
            Print_Debug
              ("Parse_Pane_Node " & Gtk_Orientation'Image (Orientation)
               & " children=" & Integer'Image (Count)
               & " child_size=" & Gint'Image (Width_For_Children)
               & "x" & Gint'Image (Height_For_Children),
               Debug_Increase);
         end if;

         declare
            Notebooks : array (1 .. Count) of MDI_Notebook;
            W         : Gtk_Widget;
            Tmp_Width, Tmp_Height : Gint;
            Tmp_Orientation : Gtk_Orientation;
            Index     : Natural := Notebooks'First;
            Child_Count : Integer := Count;
         begin
            --  First insert all direct children of the pane, splitting as
            --  needed. Only then process the Pane children. Otherwise, the
            --  children of Pane will have been split and reorganized so that
            --  we won't be able to get a reference item for further splitting.

            N := Node.Child;

            while N /= null loop
               Tmp_Width       := Width_For_Children;
               Tmp_Height      := Height_For_Children;
               Tmp_Orientation := Orientation;

               --  Find the first notebook node of N
               Notebook_Node := N;
               while Notebook_Node.Tag /= null
                 and then Notebook_Node.Tag.all = "Pane"
               loop
                  Child_Count := Children_Count (Notebook_Node);
                  Compute_Size_From_Attributes
                    (MDI,
                     Notebook_Node,
                     Parent_Width       => Tmp_Width,
                     Parent_Height      => Tmp_Height,
                     Parent_Orientation => Tmp_Orientation,
                     Width              => Tmp_Width,
                     Height             => Tmp_Height,
                     Children_Count     => Child_Count);
                  Tmp_Orientation := Gtk_Orientation'Value
                    (Get_Attribute (Notebook_Node, "Orientation"));
                  Print_Debug
                    ("Descending into pane while looking for first notebook w="
                     & Gint'Image (Tmp_Width) & "x" & Gint'Image (Tmp_Height));

                  Notebook_Node := Notebook_Node.Child;
               end loop;

               if Index = Notebooks'First
                 and then Initial_Ref_Child /= null
               then
                  Notebooks (Index) := Initial_Ref_Child;
                  W := Gtk_Widget (Initial_Ref_Child);

               else
                  if Notebook_Node.Tag.all = "Notebook" then
                     Parse_Notebook_Node
                       (MDI                => MDI,
                        Child_Node         => Notebook_Node,
                        Parent_Width       => Tmp_Width,
                        Parent_Height      => Tmp_Height,
                        Parent_Orientation => Tmp_Orientation,
                        Children_Count     => Child_Count,
                        User         => User,
                        Focus_Child  => Focus_Child,
                        Width        => Width,
                        Height       => Height,
                        Notebook     => Notebooks (Index),
                        To_Raise     => To_Raise,
                        To_Hide      => To_Hide,
                        Empty_Notebook_Filler => Empty_Notebook_Filler);

                     W := Gtk_Widget (Notebooks (Index));

                  elsif Notebook_Node.Tag.all = "central" then
                     Print_Debug ("Parse_Pane_Node: seen <central>");
                     W      := Gtk_Widget (MDI.Central);
                     Compute_Size_From_Attributes
                       (MDI,
                        Notebook_Node,
                        Parent_Width       => Tmp_Width,
                        Parent_Height      => Tmp_Height,
                        Parent_Orientation => Tmp_Orientation,
                        Width              => Width,
                        Height             => Height,
                        Children_Count     => Child_Count);
                  end if;

                  if Get_Parent (W) = null then
                     if Ref_Item = null then
                        Print_Debug
                          ("Parse_Pane_Node, add notebook in MDI "
                           & System.Address_Image (W.all'Address));
                        Add_Child (Win         => Paned,
                                   New_Child   => W,
                                   Orientation => Orientation,
                                   Width       => Width,
                                   Height      => Height);
                     else
                        Print_Debug
                          ("Parse_Pane_Node Split notebook into MDI "
                           & System.Address_Image (W.all'Address)
                           & " ref="
                           & System.Address_Image (Ref_Item.all'Address)
                           & " Orient="
                           & Gtk_Orientation'Image (Orientation));
                        Split (Paned,
                               Ref_Widget  => Ref_Item,
                               New_Child   => W,
                               Width       => Width,
                               Height      => Height,
                               Orientation => Orientation);
                     end if;
                  else
                     Print_Debug
                       ("Parse_Pane_Node: notebook already in MDI");
                     Set_Size (Paned,
                               W,
                               Width  => Width,
                               Height => Height);
                  end if;
               end if;

               Ref_Item := W;
               Index := Index + 1;
               N := N.Next;
            end loop;

            --  Now process the Pane children recursively, splitting as needed

            Print_Debug ("Parse_Pane_Node: now process pane children");

            N := Node.Child;
            Index := Notebooks'First;
            while N /= null loop
               if N.Tag.all = "Pane" then
                  Parse_Pane_Node
                    (Paned                 => Paned,
                     MDI                   => MDI,
                     Node                  => N,
                     Focus_Child           => Focus_Child,
                     User                  => User,
                     Parent_Width          => Width_For_Children,
                     Parent_Height         => Height_For_Children,
                     Parent_Children_Count => Count,
                     Parent_Orientation    => Orientation,
                     Initial_Ref_Child     => Notebooks (Index),
                     To_Raise              => To_Raise,
                     To_Hide               => To_Hide,
                     Empty_Notebook_Filler => Empty_Notebook_Filler);
               end if;
               Index := Index + 1;
               N := N.Next;
            end loop;
         end;

         Print_Debug ("", Debug_Decrease);
      end Parse_Pane_Node;

      ------------------------
      -- Restore_Multi_Pane --
      ------------------------

      procedure Restore_Multi_Pane
        (Pane                    : access Gtkada_Multi_Paned_Record'Class;
         MDI                     : access MDI_Window_Record'Class;
         Focus_Child             : in out MDI_Child;
         To_Raise                : in out Gtk.Widget.Widget_List.Glist;
         To_Hide                 : in out Gtk.Widget.Widget_List.Glist;
         Node                    : Node_Ptr;
         User                    : User_Data;
         Full_Width, Full_Height : Gint)
      is
         Child_Node : Node_Ptr := Node.Child;
         Raised     : Boolean;
         X, Y, W, H : Gint;
         Child      : MDI_Child;
         State      : State_Type;

         Empty_Notebook_Filler : MDI_Child;
         --  Used to fill the empty notebook, and prevent it from being
         --  destroyed during a desktop load.

      begin
         Print_Debug ("Restore_Multi_Pane full_size="
                      & Gint'Image (Full_Width) & "x"
                      & Gint'Image (Full_Height), Debug_Increase);

         MDI.Freeze_Focus;
         declare
         begin
            while Child_Node /= null loop
               if Traces then
                  Print_Debug
                    ("Restore_Multi_Pane, got child """
                     & Child_Node.Tag.all & """");
               end if;

               if Child_Node.Tag.all = "Pane" then
                  Parse_Pane_Node
                    (Pane,
                     MDI                   => MDI,
                     Node                  => Child_Node,
                     Focus_Child           => Focus_Child,
                     Parent_Width          => Full_Width,
                     Parent_Height         => Full_Height,
                     Parent_Children_Count => 1,
                     Parent_Orientation    => Orientation_Horizontal,
                     User                  => User,
                     Initial_Ref_Child     => null,
                     To_Raise              => To_Raise,
                     To_Hide               => To_Hide,
                     Empty_Notebook_Filler => Empty_Notebook_Filler);

               elsif Child_Node.Tag.all = "Child" then
                  --  Used for floating children, and children in the default
                  --  desktop (see Add_To_Tree)

                  Parse_Child_Node
                    (MDI, Child_Node, User,
                     Focus_Child, X, Y, W, H, Raised, State, Child,
                     To_Hide => To_Hide);

                  if Child /= null then
                     case State is
                     when Floating =>
                        Internal_Float_Child
                          (Child, True, Position_At_Mouse => False,
                           X => X, Y => Y, Width => W, Height => H);

                     when Invisible =>
                        null;

                     when Normal =>
                        Float_Child (Child, False);
                     end case;
                  end if;

               elsif Child_Node.Tag.all = "central" then
                  Add_Child (MDI, MDI.Central,
                             Width => Full_Width, Height => Full_Height);
               end if;

               Child_Node := Child_Node.Next;
            end loop;
            MDI.Thaw_Focus;
         exception
            when others =>
               MDI.Thaw_Focus;
               raise;
         end;

         if Empty_Notebook_Filler /= null then
            --  The empty notebook has been created during the desktop load
            declare
               Note : constant Gtk_Notebook :=
                        Gtk_Notebook (Get_Parent (Empty_Notebook_Filler));
            begin
               Print_Debug
                 ("Restore desktop, removing empty_notebook_filler");

               Remove_Page (Note, 0);
            end;
         end if;

         Print_Debug ("", Debug_Decrease);
      end Restore_Multi_Pane;

      ---------------------------------
      -- Recompute_Perspective_Names --
      ---------------------------------

      procedure Recompute_Perspective_Names
        (MDI : access MDI_Window_Record'Class)
      is
         N     : Node_Ptr;
         Count : Natural := 0;
      begin
         Free (MDI.Perspective_Names);

         if MDI.Perspectives /= null then
            N := MDI.Perspectives.Child;

            while N /= null loop
               Count := Count + 1;
               N := N.Next;
            end loop;

            MDI.Perspective_Names := new GNAT.Strings.String_List (1 .. Count);

            Count := MDI.Perspective_Names'First;
            N := MDI.Perspectives.Child;
            while N /= null loop
               MDI.Perspective_Names (Count) :=
                 new String'(Get_Attribute (N, "name"));
               Count := Count + 1;
               N := N.Next;
            end loop;
         end if;
      end Recompute_Perspective_Names;

      ---------------------
      -- Restore_Desktop --
      ---------------------

      function Restore_Desktop
        (MDI          : access MDI_Window_Record'Class;
         Perspectives : Glib.Xml_Int.Node_Ptr;
         From_Tree    : Glib.Xml_Int.Node_Ptr;
         User         : User_Data) return Boolean
      is
         To_Raise                  : Gtk.Widget.Widget_List.Glist;
         To_Hide                   : Gtk.Widget.Widget_List.Glist;
         Focus_Child               : MDI_Child;
         Initial_All_Floating_Mode : constant Boolean := MDI.All_Floating_Mode;
         Do_Size_Allocate          : Boolean := True;
         MDI_Width, MDI_Height               : Gint;

      begin
         Maximize_Action.Free_Saved_Data (MDI);

         if Perspectives = null
           or else Perspectives.Child = null  --   <perspective> node
         then
            --  No desktop to load, but we still have to setup a minimal
            --  environment to avoid critical errors later on.
            if MDI.Central /= null then
               Destroy (MDI.Central);
            end if;

            Gtk_New (MDI.Central);
            Add_Child (MDI, MDI.Central);
            Print_Debug ("No perspective to restore");
            return False;
         end if;

         Free (MDI.Perspectives);
         MDI.Perspectives := Deep_Copy (Perspectives);
         MDI.Current_Perspective := null;
         Recompute_Perspective_Names (MDI);

         Free (MDI.View_Contents);
         MDI.View_Contents := Deep_Copy (From_Tree);
         --  ??? We could save some memory by freeing the <pane> nodes, but is
         --  there any point ?

         --  Temporarily disable the use of all floating mode, so that we can
         --  properly restore the desktop even if notebooks are referenced.
         MDI.All_Floating_Mode := False;

         if From_Tree /= null and then From_Tree.Tag.all /= "desktop" then
            return False;
         end if;

         Print_Debug ("Restore_Desktop", Debug_Increase);

         --  We must restore the size of the main window first, so that the
         --  rest of the desktop makes sense.

         declare
            Maximized : Boolean;
            Toplevel_Width, Toplevel_Height : Gint := -1;
         begin
            Maximized := Integer'Value
               (Get_Attribute (Perspectives, "state", "0")) = 4;
            if Maximized then

               --  Compute the width the window will have when maximized.
               --  We cannot simply do a Maximize and then read the allocation
               --  size, since that is asynchronous.
               --  On many systems, the following calls seem to fail, so we
               --  just simulate a size (this is irrelevant anyway, since the
               --  call to Maximize will find the correct size, but it helps
               --  debugging when we use the real sizes).

               --  A value big enough that when the window is finally
               --  resized the MDI is able to keep the proportions of its
               --  children (which would not be the case if we had created
               --  a 10x10 window for instance).
               MDI_Width := 1000;
               MDI_Height := 1000;

               Maximize (Gtk_Window (Get_Toplevel (MDI)));
               Do_Size_Allocate := False;

            else
               Toplevel_Width  := Gint'Value
                 (Get_Attribute (Perspectives, "width",  "-1"));
               Toplevel_Height := Gint'Value
                 (Get_Attribute (Perspectives, "height", "-1"));

               --  More recent versions of the desktop also explicitly store
               --  the size of the MDI
               MDI_Width  := Gint'Value
                 (Get_Attribute (Perspectives, "mdi_width",
                  Gint'Image (Toplevel_Width)));
               MDI_Height := Gint'Value
                 (Get_Attribute (Perspectives, "mdi_height",
                  Gint'Image (Toplevel_Height)));

               if Traces then
                  Print_Debug
                    ("Toplevel size from perspective:"
                     & Gint'Image (Toplevel_Width) & "x"
                     & Gint'Image (Toplevel_Height));
                  Print_Debug
                    ("MDI size from perspective:"
                     & Gint'Image (MDI_Width) & "x"
                     & Gint'Image (MDI_Height));
               end if;

               if Toplevel_Width /= -1 then
                  Set_Default_Size
                    (Gtk_Window (MDI.Get_Toplevel),
                     Toplevel_Width, Toplevel_Height);
               end if;

               --  ??? Should not call MDI.Set_Size_Request, since that
               --  sets a minimal size for the MDI, not a default size.
               --     MDI.Set_Size_Request (MDI_Width, MDI_Height);
            end if;
         exception
            when others =>
               --  An invalid attribute in XML ?
               null;
         end;

         --  Close all existing windows (internal_load_perspective would try to
         --  preserve them, but they do not apply to the current desktop)

         MDI.Freeze_Focus;
         declare
            Tmp              : Widget_List.Glist := MDI.Items;
            Tmp2             : Widget_List.Glist;
            C                : MDI_Child;
            Widget_Node      : Node_Ptr;
            Widget_Is_Unique : Boolean;
         begin
            while Tmp /= Null_List loop
               Tmp2 := Next (Tmp);

               --  Do not force closure, we want to keep desktop-independent
               --  views
               Close (MDI, MDI_Child (Get_Data (Tmp)), Force => False);
               Tmp := Tmp2;
            end loop;

            Tmp := MDI.Items;
            while Tmp /= Null_List loop
               C := MDI_Child (Get_Data (Tmp));

               --  For those items still in the list, we must ensure we know
               --  their XML node name, otherwise they will never be reused and
               --  just waste memory (and result in memory leaks)

               if C.XML_Node_Name = null then
                  Get_XML_For_Widget
                    (Child => C, User => User, Data => Widget_Node,
                     Widget_Is_Unique => Widget_Is_Unique);
                  Free (Widget_Node);
               end if;

               Tmp := Next (Tmp);
            end loop;
            MDI.Thaw_Focus;
         exception
            when others =>
               MDI.Thaw_Focus;
               raise;
         end;

         --  Prepare the contents of the central area. This will automatically
         --  replace the central area's contents in the perspective

         if MDI.Central /= null then
            Print_Debug ("Destroying central area", Debug_Increase);
            --  It could come from a previous desktop
            Destroy (MDI.Central);
            Print_Debug ("", Debug_Decrease);
         end if;

         Gtk_New (MDI.Central);

         --  The central area describes the floating children, so they are not
         --  part of MDI.Central.

         To_Raise := Widget_List.Null_List;
         To_Hide  := Widget_List.Null_List;

         MDI.Loading_Desktop := True;

         if not MDI.Independent_Perspectives and then From_Tree /= null then
            --  ??? Incorrect: the size of the central area is only known once
            --  we have loaded the perspectives, it isn't MDI_Width. But we
            --  need the central area to load the rest of the desktop.

            if Traces then
               Print_Debug ("Loading central area", Debug_Increase);
            end if;

            Restore_Multi_Pane
              (Pane        => MDI.Central,
               MDI         => MDI,
               Focus_Child => Focus_Child,
               To_Raise    => To_Raise,
               To_Hide     => To_Hide,
               User        => User,
               Node        => From_Tree,
               Full_Width  => MDI_Width,
               Full_Height => MDI_Height);

            Print_Debug ("Done loading central area", Debug_Decrease);

            Set_Child_Visible (MDI.Central, True);
         end if;

         --  Now restore the appropriate perspective, which gives the global
         --  organization of the desktop apart from the default area (which is
         --  restored later on).

         Internal_Load_Perspective
           (MDI,
            Get_Attribute (From_Tree, "perspective", ""),
            User, Focus_Child => Focus_Child,
            To_Raise          => To_Raise,
            To_Hide           => To_Hide,
            MDI_Width         => MDI_Width,
            MDI_Height        => MDI_Height,
            Do_Size_Allocate  => Do_Size_Allocate);

         Set_All_Floating_Mode (MDI, Initial_All_Floating_Mode);

         if Focus_Child /= null then
            Print_Debug
              ("Desktop set focus on " & Get_Title (Focus_Child));
            Set_Focus_Child (Focus_Child);
         end if;

         Emit_By_Name
           (Get_Object (MDI), String (Signal_Perspective_Changed) & ASCII.NUL);
         Emit_By_Name
           (Get_Object (MDI),
            String (Signal_Children_Reorganized) & ASCII.NUL);

         Print_Debug ("Done Restore_Desktop", Debug_Decrease);

         return True;

      exception
         when E : Invalid_Desktop =>
            Put_Line ("Invalid_Desktop: " & Exception_Message (E));
            return False;
      end Restore_Desktop;

      ------------------------
      -- Get_XML_For_Widget --
      ------------------------

      procedure Get_XML_For_Widget
        (Child            : MDI_Child;
         User             : User_Data;
         Widget_Is_Unique : out Boolean;
         Data             : out Node_Ptr)
      is
         Register    : Register_Node := Registers;
         Widget_Node : Node_Ptr;
      begin

         Widget_Node := Child.Save_Desktop;  --  first, the primitif op.

         while Widget_Node = null and then Register /= null loop
            if Register.Save /= null then
               Widget_Node := Register.Save (Child.Initial, User);
            end if;
            Register := Register.Next;
         end loop;

         if Widget_Node /= null then
            --  Save the XML node name, which might be useful when switching
            --  perspectives

            Free (Child.XML_Node_Name);
            Child.XML_Node_Name := new String'(Widget_Node.Tag.all);
         end if;

         Data := Widget_Node;

         --  ??? Hard-coded for now. See comments in Save_Widget
         Widget_Is_Unique := True;
      end Get_XML_For_Widget;

      ------------------
      -- Save_Desktop --
      ------------------

      procedure Save_Desktop
        (MDI          : access MDI_Window_Record'Class;
         User         : User_Data;
         Perspectives : out Glib.Xml_Int.Node_Ptr;
         Central      : out Glib.Xml_Int.Node_Ptr)
      is
         Item             : Widget_List.Glist;
         Child_Node       : Node_Ptr;
         Child            : MDI_Child;

         Has_Negative_Values : Boolean := False;

         procedure Add (Parent : Node_Ptr; Name, Value : String);
         --  Add a new child to Child_Node

         procedure Save_Widget
           (Parent     : Node_Ptr;
            Child      : MDI_Child;
            Raised     : Boolean;
            In_Central : Boolean);
         --  Save the Child. Raised is True if Child is the current page
         --  in a notebook. In_Central is True if we are saving a child part of
         --  the central area of the desktop

         procedure Save_Size
           (Iter   : Gtkada.Multi_Paned.Child_Iterator;
            Node   : Node_Ptr);
         --  Set the size of Widget (relative to the total size of the window)
         --  as attributes of Node

         procedure Save_Paned
           (Paned      : access Gtkada_Multi_Paned_Record'Class;
            Parent     : Node_Ptr;
            In_Central : Boolean);
         --  Look through all the notebooks, and save the widgets in the
         --  notebook order.

         function Save_Notebook
           (Current    : Node_Ptr;
            Note       : MDI_Notebook;
            In_Central : Boolean) return Node_Ptr;
         --  save all pages of the notebook

         procedure Prune_Empty (N : in out Node_Ptr);
         --  Prunes empty panes below N

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
           (Parent     : Node_Ptr;
            Child      : MDI_Child;
            Raised     : Boolean;
            In_Central : Boolean)
         is
            Widget_Node      : Node_Ptr;
            Tmp_Node         : Node_Ptr;
            Widget_Is_Unique : Boolean;
         begin
            if Child.State = Invisible then
               return;
            end if;

            Get_XML_For_Widget (Child, User, Widget_Is_Unique, Widget_Node);

            if Widget_Node /= null then
               --  When a window is in the perspective (and not in the central
               --  area), we used to save its location in the <perspective>
               --  node, and its contents in <central>, since it is project
               --  specific..
               --  This works fine for the location window, for instance.
               --  But this doesn't work well for editors (for which there
               --  appears an empty area in the newly loaded desktop). Getting
               --  rid of the emplty areas requires restarting the MDI.
               --  Since the latter is blocking, the safest route for now is to
               --  always save the contents in the perspective, independently
               --  of the project. Most users always load the same project
               --  anyway.
               --  For windows that are in multiple perspectives, their
               --  contents is only saved in the current perspective. That
               --  works as expected because this is also the perspective that
               --  is reloaded.
               --  ??? One solution might be to let the widget itself decide
               --  where to save the contents when the window is not in the
               --  central area. Note that saving in <central> works best if
               --  there is a single window of a kind (so not for editors).

               Tmp_Node := Widget_Node;

               if Widget_Is_Unique
                 and then not MDI.Independent_Perspectives
                 and then not In_Central
                 and then
                   (Widget_Node.Child /= null
                    or else Widget_Node.Attributes /= null)
               then
                  Tmp_Node := new Node;
                  Tmp_Node.Tag := new String'(Widget_Node.Tag.all);
                  Add_Child (Central, Widget_Node, Append => True);
               end if;

               --  Note: We need to insert the children in the opposite order
               --  from Restore_Desktop, since the children are added at the
               --  beginning of the list.

               Child_Node := new Node;
               Child_Node.Tag := new String'("Child");
               Add_Child (Child_Node, Tmp_Node, Append => True);

               Set_Attribute (Child_Node, "State",
                              State_Type'Image (Child.State));
               Set_Attribute (Child_Node, "Group",
                              Child_Group'Image (Child.Group));

               if Child.State = Floating then
                  declare
                     Win           : constant Gtk_Window :=
                       Gtk_Window (Get_Toplevel (Child.Initial));
                     X, Y          : Gint;
                     Width, Height : Gint;
                  begin
                     Win.Get_Size (Width  => Width,
                                   Height => Height);
                     Add (Child_Node, "height", Gint'Image (Height));
                     Add (Child_Node, "width", Gint'Image (Width));

                     --  Save the floating window's coordinates

                     Win.Get_Position
                       (Root_X => X,
                        Root_Y => Y);

                     Add (Child_Node, "X", Gint'Image (X));
                     Add (Child_Node, "Y", Gint'Image (Y));
                  end;
               end if;

               if Child = MDI.Focus_Child then
                  Set_Attribute (Child_Node, "Focus", "True");
               end if;

               if Raised then
                  Set_Attribute (Child_Node, "Raised", "True");
               end if;

               if not Child.Get_Visible then
                  Set_Attribute (Child_Node, "visible", "False");
               end if;

               Add_Child (Parent, Child_Node, Append => True);
            end if;
         end Save_Widget;

         ---------------
         -- Save_Size --
         ---------------

         procedure Save_Size
           (Iter   : Gtkada.Multi_Paned.Child_Iterator;
            Node   : Node_Ptr)
         is
            Parent_Width, Parent_Height, Width, Height : Gint;
            Orientation : Gtk_Orientation;
         begin
            Get_Size (Iter, Width, Height, Parent_Width, Parent_Height,
                      Orientation);

            if Width < 0
              or else Height < 0
              or else Parent_Width < 0
              or else Parent_Height < 0
            then
               Has_Negative_Values := True;
            end if;

            case Orientation is
               when Orientation_Horizontal =>
                  Set_Attribute
                    (Node, "width",
                     Float'Image
                       (Float (Width) * 100.0 / Float (Parent_Width)) & "%");
               when Orientation_Vertical =>
                  Set_Attribute
                    (Node, "height",
                     Float'Image
                       (Float (Height) * 100.0 / Float (Parent_Height)) & "%");
            end case;
         end Save_Size;

         -------------------
         -- Save_Notebook --
         -------------------

         function Save_Notebook
           (Current    : Node_Ptr;
            Note       : MDI_Notebook;
            In_Central : Boolean) return Node_Ptr
         is
            Length                  : constant Gint := Get_N_Pages (Note);
            Current_Page            : constant Gint := Get_Current_Page (Note);
            Parent                  : Node_Ptr;
            Has_Default_Group_Child : Boolean := False;
            Child                   : MDI_Child;
         begin
            Parent := new Node;
            Parent.Tag := new String'("Notebook");
            Set_Attribute
              (Parent, "Tabs",
               Gtk_Position_Type'Image (Get_Tab_Pos (Note)));

            if Note.Tab_Orientation /= Automatic then
               Set_Attribute
                 (Parent, "orientation",
                  Tab_Orientation_Type'Image (Note.Tab_Orientation));
            end if;

            if Length > 0 then
               for Page_Index in 0 .. Length - 1 loop
                  Child := MDI_Child
                    (Get_Nth_Page (Note, Page_Index));
                  Has_Default_Group_Child := Has_Default_Group_Child
                    or else Child.Group = Group_Default;
                  Save_Widget
                    (Parent,
                     Child,
                     Raised     => Current_Page = Page_Index,
                     In_Central => In_Central);
               end loop;
            end if;

            --  Do not append the Notebook node to the parent if no child in
            --  the notebook was found, unless the number of pages is 0, in
            --  which case this is a real empty space which should be saved
            --  in the desktop. Also add the default notebook always, since
            --  it plays a special role

            Print_Debug
              ("Saving notebook, Length="
               & Gint'Image (Length)
               & " parent.child is null="
               & Boolean'Image (Parent.Child = null));

            if Length = 0
              or else Parent.Child /= null
            then
               Add_Child (Current, Parent, Append => True);
               return Parent;
            else
               Free (Parent);
               return null;
            end if;
         end Save_Notebook;

         -----------------
         -- Prune_Empty --
         -----------------

         procedure Prune_Empty (N : in out Node_Ptr) is
            C, Tmp : Node_Ptr;
         begin
            if N.Tag.all = "Pane" then
               C := N.Child;

               while C /= null loop
                  Tmp := C.Next;
                  Prune_Empty (C);
                  C := Tmp;
               end loop;

               if N.Child = null then
                  Free (N);
               end if;
            end if;
         end Prune_Empty;

         ----------------
         -- Save_Paned --
         ----------------

         procedure Save_Paned
           (Paned      : access Gtkada_Multi_Paned_Record'Class;
            Parent     : Node_Ptr;
            In_Central : Boolean)
         is
            Current    : Node_Ptr := Parent;
            N          : Node_Ptr;
            Depth      : Natural := 0;
            Iter       : Gtkada.Multi_Paned.Child_Iterator := Start (Paned);
            Orientation   : Gtk_Orientation;

         begin
            while not At_End (Iter) loop
               for N in Get_Depth (Iter) + 1 .. Depth loop
                  Current := Current.Parent;
               end loop;

               Orientation := Get_Orientation (Iter);

               if Get_Widget (Iter) = Gtk_Widget (MDI.Central) then
                  if MDI.Independent_Perspectives then
                     Save_Paned (MDI.Central, Current, In_Central => True);
                  else
                     N := new Node;
                     N.Tag := new String'("central");
                     Save_Size (Iter, N);
                     Add_Child (Current, N, Append => True);
                  end if;

               elsif Get_Widget (Iter) /= null then
                  N := Save_Notebook
                    (Current, MDI_Notebook (Get_Widget (Iter)),
                     In_Central => In_Central);
                  if N /= null then
                     Save_Size (Iter, N);
                  end if;

               else
                  N := new Node;
                  N.Tag := new String'("Pane");
                  Set_Attribute
                    (N, "Orientation", Gtk_Orientation'Image (Orientation));
                  Save_Size (Iter, N);
                  Add_Child (Current, N, Append => True);
                  Current := N;
               end if;

               Depth := Get_Depth (Iter);
               Next (Iter);
            end loop;

            if Parent.Child /= null then
               Prune_Empty (Parent.Child);
            end if;
         end Save_Paned;

         Old_Perspectives : Glib.Xml_Int.Node_Ptr;

      begin
         if MDI.Perspectives = null then
            MDI.Perspectives := new Node;
            MDI.Perspectives.Tag := new String'("perspectives");
         end if;

         if MDI.Current_Perspective /= null then
            if To_Lower (Get_Attribute
               (MDI.Current_Perspective, "save_on_exit", "True")) = "false"
            then
               return;
            end if;

            Old_Perspectives := Deep_Copy (MDI.Perspectives);

            --  Replace (in place) the perspective. This is so that the
            --  order in the /Window/Perspectives menu is preserved as much
            --  as possible

            declare
               N : Node_Ptr := MDI.Current_Perspective.Child;
               N2 : Node_Ptr;
            begin
               while N /= null loop
                  N2 := N.Next;
                  Free (N);
                  N := N2;
               end loop;
            end;

         else
            MDI.Current_Perspective := new Node;
            MDI.Current_Perspective.Tag := new String'("perspective");
            Set_Attribute (MDI.Current_Perspective, "name", "default");
            Add_Child
              (MDI.Perspectives, MDI.Current_Perspective, Append => False);
            Old_Perspectives := Deep_Copy (MDI.Perspectives);
         end if;

         Central := new Node;
         Central.Tag := new String'("desktop");

         --  Save the general configuration of the MDI

         declare
            Win    : constant Gtk_Window :=
              Gtk_Window (Get_Toplevel (MDI));
            Width  : Gint;
            Height : Gint;
         begin
            if Win /= null then
               if not Win.Is_Maximized and then Get_Window (Win) /= null then
                  Win.Get_Size
                    (Width  => Width,
                     Height => Height);
                  Set_Attribute
                    (MDI.Perspectives, "width",
                     Gint'Image (Width));
                  Set_Attribute
                    (MDI.Perspectives, "height",
                     Gint'Image (Height));
               end if;

               Set_Attribute
                 (MDI.Perspectives, "mdi_width",
                  Gint'Image (MDI.Get_Allocated_Width));
               Set_Attribute
                 (MDI.Perspectives, "mdi_height",
                  Gint'Image (MDI.Get_Allocated_Height));

               --  We are only interested in whether the window is maximized
               if Win.Is_Maximized then
                  Set_Attribute (MDI.Perspectives, "state", " 4");
               else
                  Set_Attribute (MDI.Perspectives, "state", " 0");
               end if;

               Set_Attribute
                 (Central, "perspective", Current_Perspective (MDI));
            end if;
         end;

         Print_Debug ("Save_Desktop: window size reported as"
                      & Gint'Image (MDI.Get_Toplevel.Get_Allocated_Width) & "x"
                      & Gint'Image (MDI.Get_Toplevel.Get_Allocated_Height));

         Print_Debug ("Save_Desktop: saving the perspective");
         Save_Paned (MDI, MDI.Current_Perspective, In_Central => False);

         if not MDI.Independent_Perspectives then
            Print_Debug ("Save_Desktop: saving central area");
            Save_Paned (MDI.Central, Central, In_Central => True);
         end if;

         --  Save the floating widgets (these are part of the perspective)

         Print_Debug ("Save_Desktop: saving floating widgets");
         Item := MDI.Items;
         while Item /= Widget_List.Null_List loop
            Child := MDI_Child (Widget_List.Get_Data (Item));

            case Child.State is
               when Normal | Invisible  => null;
               when Floating =>
                  Save_Widget (Central, Child, False, In_Central => True);
            end case;

            Item := Widget_List.Next (Item);
         end loop;

         if Has_Negative_Values then
            --  In case of negative values, retrieve the old perspectives
            --  But keep the current perspective as the active one

            declare
               N    : Node_Ptr := Old_Perspectives.Child;
               Name : constant String := Get_Attribute
                 (MDI.Current_Perspective, "name");
            begin
               while N /= null loop
                  if Get_Attribute (N, "name") = Name then
                     MDI.Current_Perspective := N;
                     exit;
                  end if;
                  N := N.Next;
               end loop;
            end;

            Free (MDI.Perspectives);
            MDI.Perspectives := Old_Perspectives;

         else
            Free (Old_Perspectives);
         end if;

         Perspectives := Deep_Copy (MDI.Perspectives);

         if Traces then
            Print_Debug ("After saving the desktop (current perspective is "
                         & Current_Perspective (MDI) & "), desktop is");
            Print (MDI.Perspectives);

            if not MDI.Independent_Perspectives then
               Print_Debug ("And the central area is");
               Print (Central);
            end if;
         end if;
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

      -------------------------------
      -- Internal_Load_Perspective --
      -------------------------------

      procedure Internal_Load_Perspective
        (MDI              : access MDI_Window_Record'Class;
         Name             : String;
         User             : User_Data;
         Focus_Child      : in out MDI_Child;
         To_Raise         : in out Gtk.Widget.Widget_List.Glist;
         To_Hide          : in out Gtk.Widget.Widget_List.Glist;
         MDI_Width, MDI_Height : Gint;
         Do_Size_Allocate : Boolean)
      is
         Alloc    : Gtk_Allocation;
         Child    : MDI_Child;

         procedure Remove_All_Items (Remove_All_Empty : Boolean);
         --  Remove all items from the MDI

         ----------------------
         -- Remove_All_Items --
         ----------------------

         procedure Remove_All_Items (Remove_All_Empty : Boolean) is
            Children    : Widget_List.Glist;
            L           : Widget_List.Glist;
            Note        : Gtk_Notebook;
            C           : MDI_Child;
            Parent      : Gtk_Widget;
         begin
            Print_Debug ("Remove_All_Items: remove_empty="
                         & Boolean'Image (Remove_All_Empty),
                         Debug_Increase);

            --  Remove all children from the MDI. However, we do not close them
            --  in case we switch back to the perspective (or the user opens
            --  them while in the perspective). They will just be marked as
            --  Invisible for now.

            L := MDI.Items;
            while L /= Null_List loop
               C := MDI_Child (Get_Data (L));

               if C.State = Normal
                 and then (MDI.Independent_Perspectives
                           or else not In_Central_Area (MDI, C))
               then
                  Print_Debug ("Remove_All_Items, marking "
                               & Get_Title (C) & " as invisible");
                  Ref (C);   --  Unref called in Destroy_Child and Put

                  Parent := Get_Parent (C);

                  if Parent /= null then
                     Remove (Gtk_Container (Get_Parent (C)), C);
                  end if;

                  Set_State (C, Invisible);
               end if;

               L := Next (L);
            end loop;

            --  We now force the closing of all empty notebooks
            --  The central area should no longer be part of the MDI at this
            --  stage, so all we get are notebooks

            Children := Get_Children (MDI);
            L := Children;
            while L /= Null_List loop
               Note := Gtk_Notebook (Get_Data (L));
               if Get_Nth_Page (Note, 0) = null then
                  Remove (MDI, Note);
               end if;
               L := Next (L);
            end loop;
            Free (Children);

            --  We used to close empty notebooks, but in fact such notebooks
            --  can now only be in the central area, whose contents has not
            --  changed anyway.

            Print_Debug ("", Debug_Decrease);
         end Remove_All_Items;

         Tmp_Persp : Node_Ptr;

      begin
         MDI.Freeze_Focus;

         --  Find the right perspective node

         Tmp_Persp := MDI.Perspectives.Child;
         while Tmp_Persp /= null
           and then Get_Attribute (Tmp_Persp, "name") /= Name
         loop
            Tmp_Persp := Tmp_Persp.Next;
         end loop;

         if Tmp_Persp = null then
            --  If not found, and we already have a perspective => do nothing

            if MDI.Current_Perspective /= null then
               return;
            end if;

            --  Else load the first one
            Print_Debug ("Perspective not found, loading default one");
            Tmp_Persp := MDI.Perspectives.Child;
         end if;

         MDI.Current_Perspective := Tmp_Persp;

         if Traces then
            Print_Debug ("Loading perspective " & Name,
                         Debug_Increase);
         end if;

         --  Remove central from the MDI, and it will be put in the new
         --  perspective

         Ref (MDI.Central);
         if Get_Parent (MDI.Central) /= null then
            Remove (Gtk_Container (Get_Parent (MDI.Central)), MDI.Central);
         end if;

         MDI.Loading_Desktop := True;
         Freeze (MDI);

         --  Clean up MDI if necessary

         Remove_All_Items (Remove_All_Empty => True);

         Restore_Multi_Pane
           (Pane                  => MDI,
            MDI                   => MDI,
            Focus_Child           => Focus_Child,
            To_Raise              => To_Raise,
            To_Hide               => To_Hide,
            User                  => User,
            Node                  => MDI.Current_Perspective,
            Full_Width            => MDI_Width,
            Full_Height           => MDI_Height);

         --  If the central area was not in the desktop, that's an error and
         --  the application will not be usable anyway, so better break the
         --  desktop but show the central area

         if not MDI.Independent_Perspectives
           and then Get_Parent (MDI.Central) = null
         then
            Print_Debug ("central not in desktop, force adding");
            Add_Child (MDI, MDI.Central);
         end if;

         MDI.Desktop_Was_Loaded := True;

         --  Raise all appropriate items at the end, so that even if some items
         --  are added temporarily to notebooks, they have no long-lasting
         --  impact on the notebook itself.
         declare
            Item : Widget_List.Glist := To_Raise;
         begin
            while Item /= Widget_List.Null_List loop
               Child := MDI_Child (Widget_List.Get_Data (Item));

               if Focus_Child = null then
                  Focus_Child := Child;
               end if;

               Print_Debug
                 ("Restore_Desktop, raising child with no focus "
                  & Get_Title (Child));
               Raise_Child (Child, Give_Focus => False);
               Item := Widget_List.Next (Item);
            end loop;
            Free (To_Raise);

            Print_Debug ("Restore_Desktop, hidding children");
            Item := To_Hide;
            while Item /= Widget_List.Null_List loop
               Child := MDI_Child (Widget_List.Get_Data (Item));
               Print_Debug
                 ("Restore_Desktop, hidding " & Get_Title (Child));
               Hide (Child);
               Item := Widget_List.Next (Item);
            end loop;
            Free (To_Hide);

            if Traces then
               Dump (MDI);
            end if;
         end;

         if not MDI.Independent_Perspectives then
            Realize (MDI.Central);
            Show_All (MDI.Central);
            Unref (MDI.Central);
         end if;

         Show_All (MDI);
         Reset_Title_Bars_And_Colors (MDI);

         MDI.Loading_Desktop := False;
         Thaw (MDI);
         Queue_Resize (MDI);

         --  Update to show which menu is active
         Update_Menu_Model_List_Of_Children (MDI);

         Emit_By_Name
           (Get_Object (MDI), String (Signal_Perspective_Changed) & ASCII.NUL);

         --  Realize the window while frozen, so that windows that insist on
         --  setting their own size when realized (eg. the search window in
         --  GPS) will not break the desktop.
         --  However, don't do this when attempting to maximize the desktop,
         --  since otherwise we get a first Size_Allocate for whatever current
         --  size we have, and then a second one for the maximized size. The
         --  first one breaks the desktop partially.

         if Do_Size_Allocate then
            Print_Debug ("Internal_Load_Perspective, forcing a Size_Allocate");
            Realize (MDI);
            MDI.Get_Allocation (Alloc);
            Size_Allocate (MDI, Alloc);
         end if;

         if Traces then
            Print_Debug ("Done loading perspective", Debug_Decrease);
         end if;

         MDI.Thaw_Focus;
      exception
         when others =>
            MDI.Thaw_Focus;
            raise;
      end Internal_Load_Perspective;

      ----------------------
      -- Load_Perspective --
      ----------------------

      procedure Load_Perspective
        (MDI          : access MDI_Window_Record'Class;
         Name         : String;
         User         : User_Data)
      is
         To_Raise     : Gtk.Widget.Widget_List.Glist;
         To_Hide      : Gtk.Widget.Widget_List.Glist;
         Focus_Child  : MDI_Child;
         Perspectives : Node_Ptr;
      begin
         --  Save modifications to current perspective, so that we can restore
         --  them when the user switches back to that perspective. A signal
         --  "perspective_changed" will be sent to the user, so that he has a
         --  chance to save the changes in an external file for persistency

         Print_Debug ("++++++ Load_Perspective " & Name);
         if MDI.Current_Perspective /= null then
            if Get_Attribute (MDI.Current_Perspective, "name") = Name then
               return;
            end if;

            --  We only rely on the side effect of changing MDI.Perspectives,
            --  since the central area does not change in any case
            Free (MDI.View_Contents);
            Save_Desktop (MDI, User, Perspectives, MDI.View_Contents);
            Free (Perspectives);
         end if;

         Print_Debug ("++++ Load_Perspective, desktop was saved, now loading");
         Internal_Load_Perspective
           (MDI, Name, User,
            Focus_Child      => Focus_Child,
            To_Raise         => To_Raise,
            To_Hide          => To_Hide,
            MDI_Width        => MDI.Get_Allocated_Width,
            MDI_Height       => MDI.Get_Allocated_Height,
            Do_Size_Allocate => False);

         Maximize_Action.Hide_When_Maximized (MDI);
      end Load_Perspective;

      ----------------------------------
      -- On_Action_Select_Perspective --
      ----------------------------------

      procedure On_Action_Select_Perspective
         (MDI       : access MDI_Window_Record'Class;
          Params    : GValues;
          User      : User_Data)
      is
         Length : aliased Gsize;
         Val    : constant Gvariant := From_Object (To_Address (Params, 1));
         Name   : constant String := Get_String (Val, Length'Access);
      begin
         if not MDI.Loading_Desktop then
            Print_Debug ("++++ Change_Perspective to " & Name);
            Load_Perspective (MDI, Name, User);
         end if;
      end On_Action_Select_Perspective;

      ----------------------------------
      -- On_Action_Create_Perspective --
      ----------------------------------

      procedure On_Action_Create_Perspective
         (MDI       : access MDI_Window_Record'Class;
          Params    : GValues;
          User      : User_Data)
      is
         pragma Unreferenced (Params);
      begin
         Create_Perspective_Query_Name (MDI, User);
      end On_Action_Create_Perspective;

      --------------------
      -- Set_Menu_Model --
      --------------------

      procedure Set_Menu_Model
        (MDI   : not null access MDI_Window_Record'Class;
         App   : not null access Gtk.Application.Gtk_Application_Record'Class;
         Model : access Glib.Menu.Gmenu_Record'Class;
         User  : User_Data)
      is
         Section : Gmenu;
         Act     : Gsimple_Action;
      begin
         if Gmenu (Model) /= MDI.Menu_Model
            and then MDI.Menu_Model /= null
         then
            Unref (MDI.Menu_Model);
            Unref (MDI.Menu_Items_Section);
            MDI.Menu_Model := null;
            MDI.Menu_Items_Section := null;
         end if;

         MDI.Menu_Model := Gmenu (Model);
         MDI.Application := Gtk_Application (App);

         Ref (MDI.Menu_Model);

         --  Create the actions if necessary (the first time)

         if MDI.Action_Close = null then
            G_New (Act, "mdi_split_h", null);
            Act.On_Activate (On_Action_Split_H'Access, MDI);
            App.Add_Action (+Act);

            G_New (Act, "mdi_split_v", null);
            Act.On_Activate (On_Action_Split_V'Access, MDI);
            App.Add_Action (+Act);

            G_New_Stateful
               (MDI.Action_Float, "mdi_floating",
                Parameter_Type => null,
                State          => Gvariant_New_Boolean (False));
            MDI.Action_Float.On_Activate (On_Action_Floating'Access, MDI);
            App.Add_Action (+MDI.Action_Float);

            G_New (MDI.Action_Close, "mdi_close", null);
            MDI.Action_Close.On_Activate (On_Action_Close'Access, MDI);
            App.Add_Action (+MDI.Action_Close);

            G_New_Stateful
               (MDI.Action_Select_Perspective, "mdi_set_perspective",
                Parameter_Type => Gvariant_Type_String,
                State          => Gvariant_New_String (""));
            MDI_User_Data_Cb.Object_Connect
               (MDI.Action_Select_Perspective,
                Glib.Simple_Action.Signal_Activate,
                On_Action_Select_Perspective'Access, MDI, User);
            App.Add_Action (+MDI.Action_Select_Perspective);

            G_New (Act, "mdi_create_perspective", null);
            MDI_User_Data_Cb.Object_Connect
               (Act, Glib.Simple_Action.Signal_Activate,
                On_Action_Create_Perspective'Access, MDI, User);
            App.Add_Action (+Act);

            G_New_Stateful
               (MDI.Action_Select, "mdi_select",
                Parameter_Type => Gvariant_Type_String,
                State          => Gvariant_New_String (""));
            MDI.Action_Select.On_Activate (On_Action_Select'Access, MDI);
            App.Add_Action (+MDI.Action_Select);
         end if;

         --  Create the menus

         if Model /= null then
            MDI.Menu_Model.Remove_All;

            Section := Gmenu_New;
            MDI.Menu_Model.Append_Section ("", Section);

            MDI.Perspectives_Menu := Gmenu_New;
            Section.Append_Submenu ("Perspectives", MDI.Perspectives_Menu);

            Section.Append ("Split Side-by-Side", "app.mdi_split_h");
            Section.Append ("Split Up-Down", "app.mdi_split_v");

            Section := Gmenu_New;
            MDI.Menu_Model.Append_Section ("", Section);

            Section.Append ("Floating", "app.mdi_floating");
            Section.Append ("Close", "app.mdi_close");

            MDI.Menu_Items_Section := Gmenu_New;
            MDI.Menu_Model.Append_Section ("", MDI.Menu_Items_Section);

            Update_Menu_Model_List_Of_Children (MDI);
         end if;
      end Set_Menu_Model;

   end Desktop;

   ----------------------------------------
   -- Update_Menu_Model_List_Of_Children --
   ----------------------------------------

   procedure Update_Menu_Model_List_Of_Children
      (MDI : not null access MDI_Window_Record'Class)
   is
      use Child_Vectors;
      Child   : MDI_Child;
      Vec     : Child_Vectors.Vector;
      Curs    : Child_Vectors.Cursor;
   begin
      if MDI.Menu_Items_Section /= null then
         MDI.Menu_Items_Section.Remove_All;

         Get_Sorted_List_Of_Visible_Children (MDI, Vec);

         Curs := First (Vec);
         while Has_Element (Curs) loop
            Child := Element (Curs);
            MDI.Menu_Items_Section.Append
               (Child.Get_Short_Title,
                "app.mdi_select(""" & Child.Get_Title & """)");
            Next (Curs);
         end loop;

         if MDI.Focus_Child = null then
            MDI.Action_Select.Set_State (Gvariant_New_String (""));
         else
            MDI.Action_Select.Set_State
               (Gvariant_New_String (MDI.Focus_Child.Get_Title));
         end if;
      end if;
   end Update_Menu_Model_List_Of_Children;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (MDI               : access MDI_Window_Record;
      Group_By_Notebook : Boolean := False;
      Visible_Only      : Boolean := True) return Child_Iterator
   is
      Children : Widget_List.Glist;
      C        : MDI_Child;
   begin
      if Group_By_Notebook then
         declare
            Iter : Child_Iterator :=
              (Group_By_Notebook => True,
               Visible_Only      => Visible_Only,
               Paned_Iter        => Start (MDI),
               In_Central        => False,
               Notebook          => null,
               Notebook_Page     => 0,
               Floating_Iter     => MDI.Items,
               MDI               => MDI_Window (MDI));
         begin
            if MDI.Central /= null then
               Iter.Paned_Iter := Start (MDI.Central);
               Iter.In_Central := True;
            end if;

            Move_To_Next_Notebook (Iter);

            while Iter.Floating_Iter /= Null_List
              and then MDI_Child
                (Widget_List.Get_Data (Iter.Floating_Iter)).State /= Floating
            loop
               Iter.Floating_Iter := Widget_List.Next (Iter.Floating_Iter);
            end loop;

            return Iter;
         end;
      else
         Children := MDI.Items;

         if Children /= Widget_List.Null_List and then Visible_Only then
            C := MDI_Child (Widget_List.Get_Data (MDI.Items));
            if  C.State = Invisible then
               --  There are no visible child, since the first one should be
               --  the one with the focus
               Children := Widget_List.Null_List;
            end if;
         end if;

         return (Group_By_Notebook => False,
                 Visible_Only      => Visible_Only,
                 Iter              => Children);
      end if;
   end First_Child;

   ------------------
   -- Get_Notebook --
   ------------------

   function Get_Notebook
     (Iterator : Child_Iterator) return Gtk.Notebook.Gtk_Notebook
   is
      C : constant MDI_Child := Get (Iterator);
   begin
      if C = null then
         return null;
      else
         return Gtk_Notebook (Get_Notebook (C));
      end if;
   end Get_Notebook;

   ---------------------------
   -- Move_To_Next_Notebook --
   ---------------------------

   procedure Move_To_Next_Notebook (Iterator : in out Child_Iterator) is
   begin
      Iterator.Notebook := null;
      Iterator.Notebook_Page := 0;

      loop
         if At_End (Iterator.Paned_Iter) then
            if Iterator.In_Central then
               Iterator.In_Central := False;
               Iterator.Paned_Iter := Start (Iterator.MDI);
               exit when At_End (Iterator.Paned_Iter);
            else
               exit;
            end if;
         end if;

         --  Assert (not At_End (Iterator.Paned_Iter))

         if Get_Widget (Iterator.Paned_Iter) /= null
           and then
             (not Iterator.Visible_Only
              or else Get_Widget (Iterator.Paned_Iter).Get_Visible)
           and then Get_Widget (Iterator.Paned_Iter).all
             in Gtk_Notebook_Record'Class
         then
            Iterator.Notebook :=
              Gtk_Notebook (Get_Widget (Iterator.Paned_Iter));
            exit;
         end if;

         Next (Iterator.Paned_Iter);
      end loop;
   end Move_To_Next_Notebook;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Child_Iterator) is
      C  : MDI_Child;
   begin
      if Iterator.Group_By_Notebook then
         if Iterator.Notebook = null then
            --  Find the next floating child
            loop
               Iterator.Floating_Iter :=
                 Widget_List.Next (Iterator.Floating_Iter);

               exit when Iterator.Floating_Iter = Null_List
                 or else MDI_Child
                   (Widget_List.Get_Data (Iterator.Floating_Iter)).State =
                 Floating;
            end loop;

         else
            Iterator.Notebook_Page := Iterator.Notebook_Page + 1;
            if Get_Nth_Page
              (Iterator.Notebook, Iterator.Notebook_Page) = null
            then
               Next (Iterator.Paned_Iter);
               Move_To_Next_Notebook (Iterator);
            end if;
         end if;

      else
         loop
            Iterator.Iter := Widget_List.Next (Iterator.Iter);
            if Iterator.Visible_Only then
               C := Get (Iterator);
               exit when C = null or else C.State /= Invisible;
            else
               exit;
            end if;
         end loop;
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
     (Child : not null access MDI_Child_Record; Highlight : Boolean := True)
   is
      Note      : constant MDI_Notebook := Get_Notebook (Child);
   begin
      --  Do nothing if Child is not in a notebook, if it has no tab label, or
      --  if it already has the focus.
      if Note = null
        or else Child.Tab_Label = null
        or else (Highlight
                   and then Note.Get_Current_Page = Note.Page_Num (Child))
      then
         return;
      end if;

      if Highlight then
         Get_Style_Context (Child.Tab_Label).Add_Class
           ("mdi-highlighted-tab");
      else
         Get_Style_Context (Child.Tab_Label).Remove_Class
           ("mdi-highlighted-tab");
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

   function Get_State
     (Child : not null access MDI_Child_Record) return State_Type is
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
      Widget.On_Button_Press_Event (Button_Pressed_Forced'Access, Child);
   end Set_Dnd_Source;

   ------------------------
   -- Draw_Dnd_Rectangle --
   ------------------------

   procedure Draw_Dnd_Rectangle
     (Child : access MDI_Child_Record'Class;
      Hide_Only : Boolean := False)
   is
      MDI : constant MDI_Window := Child.MDI;

      Current    : Gtk_Widget;
      Position   : Child_Position;
      New_Pos    : Gdk_Rectangle;
      Parent_Rect : Gdk_Rectangle;
      Message    : Unbounded_String;
      In_Central : Boolean;
      C3         : MDI_Child;
      Note       : Gtk_Notebook;
      Allowed    : Boolean;

      procedure Do_Draw (Cr : Cairo_Context; Draw : Boolean);
      procedure Do_Draw (Cr : Cairo_Context; Draw : Boolean) is
         Color : Gdk_RGBA;
         Layout : Pango_Layout;
         Ink_Rect, Logical_Rect : Pango_Rectangle;
         X, Y : Gdouble;
         W, H : Gdouble;
      begin
         if Draw then
            if not Allowed
              or else Current = null
            then
               --  A future floating child ? Nothing to draw for now, and there
               --  will be nothing to hide either.
               MDI.Dnd_Rectangle_Real.Width := 0;
               return;
            end if;

            --  Adapt the color to make it more visible on the current theme
            Color := Gtkada.Style.Shade_Or_Lighten (MDI.Title_Bar_Color, 0.1);

            Color.Alpha := 0.5;
            Set_Source_RGBA (Cr, Color);
            Cairo.Rectangle
              (Cr,
               Gdouble (Parent_Rect.X),
               Gdouble (Parent_Rect.Y),
               Gdouble (Parent_Rect.Width),
               Gdouble (Parent_Rect.Height));
            Cairo.Fill (Cr);

            Color.Alpha := 0.6;
            Set_Source_RGBA (Cr, Color);

            MDI.Dnd_Rectangle := New_Pos;

            --  Offset here is to compensate the line width
            Gtkada.Style.Rounded_Rectangle
              (Cr,
               Gdouble (New_Pos.X) + 3.0,
               Gdouble (New_Pos.Y) + 3.0,
               Gdouble (New_Pos.Width) - 6.0,
               Gdouble (New_Pos.Height) - 6.0,
               Radius => 8.0);
            Cairo.Fill_Preserve (Cr);

            Color.Alpha := 0.9;
            Set_Source_RGBA (Cr, Color);
            Set_Line_Width (Cr, 3.0);
            Stroke (Cr);

            Layout := MDI.Create_Pango_Layout;
            Layout.Set_Markup
              (ASCII.HT & To_String (Message)
               & ASCII.LF & MDI.Dnd_Message.all);

            Layout.Get_Extents (Ink_Rect, Logical_Rect);
            W := Gdouble (Logical_Rect.Width / Pango_Scale);
            H := Gdouble (Logical_Rect.Height / Pango_Scale);

            X := Gdouble (MDI.Dnd_Rectangle.X) +
              (Gdouble (New_Pos.Width) - W) / 2.0;
            Y := Gdouble (MDI.Dnd_Rectangle.Y) +
              (Gdouble (New_Pos.Height) - H) / 2.0;

            --  Keep the text window within the MDI

            X := Gdouble'Max (X, 0.0);
            Y := Gdouble'Max (Y, 0.0);

            if X + W > Gdouble (MDI.Get_Allocated_Width) then
               X := Gdouble (MDI.Get_Allocated_Width) - W;
            end if;

            if Y + H > Gdouble (MDI.Get_Allocated_Height) then
               Y := Gdouble (MDI.Get_Allocated_Height) - H;
            end if;

            --  Slightly lighter (and non-transparent) background for the msg.
            Set_Line_Width (Cr, 1.0);
            Set_Source_RGBA
              (Cr, Gtkada.Style.Shade_Or_Lighten (Color, 0.2));
            Gtkada.Style.Rounded_Rectangle
              (Cr, X - 2.0, Y - 2.0, W + 4.0, H + 4.0, 4.0);
            Cairo.Fill (Cr);

            Get_Style_Context (MDI).Render_Layout (Cr, X, Y, Layout);
            Unref (Layout);

            MDI.Dnd_Rectangle_Real.X := Gint'Min (Parent_Rect.X, Gint (X) - 3);
            MDI.Dnd_Rectangle_Real.Y := Gint'Min (Parent_Rect.Y, Gint (Y) - 3);
            MDI.Dnd_Rectangle_Real.Width := Gint'Max
              (Parent_Rect.Width + Parent_Rect.X - MDI.Dnd_Rectangle_Real.X,
               Gint (W + X) + 4 - MDI.Dnd_Rectangle_Real.X);
            MDI.Dnd_Rectangle_Real.Height := Gint'Max
              (Parent_Rect.Height + Parent_Rect.Y - MDI.Dnd_Rectangle_Real.Y,
               Gint (H + Y) + 4 - MDI.Dnd_Rectangle_Real.Y);

         elsif MDI.Dnd_Rectangle_Real.Width /= 0 then
            Cairo.Rectangle
              (Cr,
               Gdouble (MDI.Dnd_Rectangle_Real.X),
               Gdouble (MDI.Dnd_Rectangle_Real.Y),
               Gdouble (MDI.Dnd_Rectangle_Real.Width),
               Gdouble (MDI.Dnd_Rectangle_Real.Height));
            Cairo.Fill (Cr);
         end if;
      end Do_Draw;

      Old_Dnd_Target : constant Gdk.Gdk_Window := MDI.Dnd_Target;

   begin
      Get_Dnd_Target (MDI              => Child.MDI,
                      Parent           => Current,
                      Position         => Position,
                      Parent_Rectangle => Parent_Rect,
                      Rectangle        => New_Pos,
                      Allowed          => Allowed,
                      In_Central       => In_Central);

      if Hide_Only then
         Allowed := False;
      end if;

      if not Allowed then
         Current := null;
         MDI.Dnd_Target := null;

      elsif Current = null then
         MDI.Dnd_Target := null;

      elsif Current = Gtk_Widget (MDI) then
         MDI.Dnd_Target := Get_Window (MDI);
         case Position is
            when Position_Bottom =>
               Message := To_Unbounded_String ("Below all other windows");
            when Position_Top =>
               Message := To_Unbounded_String ("Above all other windows");
            when Position_Left =>
               Message := To_Unbounded_String ("Left of all other windows");
            when Position_Right =>
               Message := To_Unbounded_String ("Right of all other windows");
            when others =>
               null;   --  Cannot occur
         end case;

      elsif Current = Gtk_Widget (MDI.Central) then
         MDI.Dnd_Target := Get_Window (MDI.Central);
         case Position is
            when Position_Bottom =>
               Message := To_Unbounded_String ("Below central area");
            when Position_Top =>
               Message := To_Unbounded_String ("Above central area");
            when Position_Left =>
               Message := To_Unbounded_String ("Left of central area");
            when Position_Right =>
               Message := To_Unbounded_String ("Right of central area");
            when others =>
               Message := To_Unbounded_String ("In central area");
               In_Central := True;
         end case;

      elsif Current = Get_Parent (Child)
        and then Position = Position_Automatic
      then
         MDI.Dnd_Target := Get_Window (Child);
         Message := To_Unbounded_String ("Leave at current position");

      else
         Note := Gtk_Notebook (Current);
         C3  := MDI_Child (Get_Nth_Page (Note, Get_Current_Page (Note)));

         if C3 = null then
            Message := To_Unbounded_String ("In central area");
            MDI.Dnd_Target := Get_Window (MDI.Central);

         else
            MDI.Dnd_Target := Get_Window (C3);

            case Position is
               when Position_Bottom =>
                  Message := To_Unbounded_String
                    ("Below <b>"
                     & Glib.Convert.Escape_Text (Get_Short_Title (C3))
                     & "</b>");
               when Position_Top =>
                  Message := To_Unbounded_String
                    ("Above <b>"
                     & Glib.Convert.Escape_Text (Get_Short_Title (C3))
                     & "</b>");
               when Position_Left =>
                  Message := To_Unbounded_String
                    ("Left of <b>"
                     & Glib.Convert.Escape_Text (Get_Short_Title (C3))
                     & "</b>");
               when Position_Right =>
                  Message := To_Unbounded_String
                    ("Right of <b>"
                     & Glib.Convert.Escape_Text (Get_Short_Title (C3))
                     & "</b>");
               when others =>
                  Message := To_Unbounded_String
                    ("On top of <b>"
                     & Glib.Convert.Escape_Text (Get_Short_Title (C3))
                     & "</b>");
            end case;
         end if;
      end if;

      --  Call this if the state has changed to hide the current highlight. We
      --  might, or not, highlight the target depending whether it is allowed
      --  for Child.

      if not (Position = MDI.Old_Dnd_Position
              and then MDI.Dnd_Target = Old_Dnd_Target)
      then
         Gtkada.Style.Draw_Overlay (MDI, MDI.Dnd_Overlay, Do_Draw'Access);
      end if;

      --  Update the drag state.

      MDI.Old_Dnd_Position := Position;

   end Draw_Dnd_Rectangle;

   ----------------------
   -- Child_Drag_Begin --
   ----------------------

   procedure Child_Drag_Begin
     (Child : not null access MDI_Child_Record'Class;
      Event : Gdk_Event_Button;
      Areas : Allowed_Areas)
   is
      Tmp  : Gdk_Grab_Status;
      Win  : Gdk.Gdk_Window;
      pragma Unreferenced (Tmp);
   begin
      --  Focus and raise the child. Raise_Child must be called explicitly
      --  since Set_Focus_Child won't do it if the child already has the focus.
      --  We have to raise the child, since otherwise the Pointer_Grab below
      --  will fail

      Print_Debug ("Child_Drag_Begin, focus and raise "
                   & Get_Title (Child));

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

         Child.MDI.Drag_Start_X := Gint (Event.X_Root);
         Child.MDI.Drag_Start_Y := Gint (Event.Y_Root);
         Child.MDI.In_Drag := In_Pre_Drag;
         Child.MDI.Dnd_Rectangle := (0, 0, 0, 0);
         Child.MDI.Drag_Areas := Areas;

      else
         Print_Debug ("Child is floating, did not initiate DnD");
      end if;
   end Child_Drag_Begin;

   -----------------------
   -- Cancel_Child_Drag --
   -----------------------

   procedure Cancel_Child_Drag
     (Child : not null access MDI_Child_Record'Class) is
   begin
      Print_Debug ("Cancel_Child_Drag");

      Pointer_Ungrab;
      Child.MDI.In_Drag := No_Drag;
   end Cancel_Child_Drag;

   -------------------------
   -- Child_Drag_Finished --
   -------------------------

   procedure Child_Drag_Finished (Child : not null access MDI_Child_Record) is
      pragma Unreferenced (Child);
   begin
      null;
   end Child_Drag_Finished;

   --------------------
   -- Get_Dnd_Target --
   --------------------

   procedure Get_Dnd_Target
     (MDI              : not null access MDI_Window_Record'Class;
      Parent           : out Gtk_Widget;
      Position         : out Child_Position;
      Parent_Rectangle : out Gdk_Rectangle;
      Rectangle        : out Gdk_Rectangle;
      In_Central       : out Boolean;
      Allowed          : out Boolean)
   is
      Border_Width, Border_Height : Gint;
      Win                         : Gdk.Gdk_Window;
      Mask                        : Gdk_Modifier_Type;
      Current                     : Gtk_Widget;
      X, Y                        : Gint;
      Alloc                       : Gtk_Allocation;
   begin
      Gdk.Display.Get_Window_At_Pointer
         (Get_Display (Get_Window (MDI)), X, Y, Win);

      if Win = null then
         Position := Position_Automatic;
         Parent := null;
         Allowed := True;   --  floating

      else
         Current := Gtk_Widget (Get_User_Data (Win));

         while Current /= null
           and then Current /= Gtk_Widget (MDI)
           and then Current /= Gtk_Widget (MDI.Central)
           and then Get_Parent (Current) /= null
           and then
             (Current.all not in Gtk_Notebook_Record'Class
              or else
                 (Get_Parent (Current) /= Gtk_Widget (MDI)
                  and then Get_Parent (Current) /= Gtk_Widget (MDI.Central)))
         loop
            Current := Get_Parent (Current);
         end loop;

         --  If the cursor was put in a floating window, we should make the
         --  new child floating as well.
         if Current = null or else Get_Parent (Current) = null then
            Parent   := null;
            Position := Position_Automatic;
            Allowed  := True;
            return;
         end if;

         if Current = Gtk_Widget (MDI)
           and then MDI.Central /= null
         then
            Current := Gtk_Widget (MDI.Central);

            --  Central area not empty ? We have therefore passed the mouse on
            --  one of the handles, and should not allow a drop there

            if not At_End (Start (MDI.Central)) then
               Position := Position_Automatic;
               Parent   := null;
               Allowed  := True;
               return;
            end if;
         end if;

         Parent := Current;

         --  Are we on the sides of the MDI itself ?

         Rectangle :=
           (X      => 0,
            Y      => 0,
            Width  => Get_Allocated_Width (MDI),
            Height => Get_Allocated_Height (MDI));
         Parent_Rectangle := Rectangle;

         Gdk.Window.Get_Device_Position
            (Self   => Get_Window (MDI),
             Device => Gtk.Main.Get_Current_Event_Device,
             X      => X,
             Y      => Y,
             Mask   => Mask,
             Window => Win);

         if Y < Max_Drag_Border_Width / 2 then
            Position := Position_Top;
            Parent := Gtk_Widget (MDI);
            In_Central := False;
            Rectangle :=
              (X      => 0,
               Y      => 0,
               Width  => Rectangle.Width,
               Height => Max_Drag_Border_Width / 2);

         elsif Y > Rectangle.Height - Max_Drag_Border_Width / 2 then
            Position := Position_Bottom;
            Parent := Gtk_Widget (MDI);
            In_Central := False;
            Rectangle :=
              (X      => 0,
               Y      => Rectangle.Height - Max_Drag_Border_Width / 2,
               Width  => Rectangle.Width,
               Height => Max_Drag_Border_Width / 2);

         elsif X < Max_Drag_Border_Width / 2 then
            Position := Position_Left;
            Parent := Gtk_Widget (MDI);
            In_Central := False;
            Rectangle :=
              (X      => 0,
               Y      => 0,
               Width  => Max_Drag_Border_Width / 2,
               Height => Rectangle.Height);

         elsif X > Rectangle.Width - Max_Drag_Border_Width / 2 then
            Position := Position_Right;
            Parent := Gtk_Widget (MDI);
            In_Central := False;
            Rectangle :=
              (X      => Rectangle.Width - Max_Drag_Border_Width / 2,
               Y      => 0,
               Width  => Max_Drag_Border_Width / 2,
               Height => Rectangle.Height);

         else
            --  Are we on the sides of the current MDI child ?
            --  Parent is likey a Gtk_Notebook

            Gtkada.Style.Get_Offset
              (Parent, MDI, Rectangle.X, Rectangle.Y);
            Parent.Get_Allocation (Alloc);

            --  Compute device position relative to the MDI
            Gdk.Window.Get_Device_Position
               (Self   => Get_Window (MDI),
                Device => Gtk.Main.Get_Current_Event_Device,
                X      => X,
                Y      => Y,
                Mask   => Mask,
                Window => Win);

            --  size of the parent, and position relative to MDI
            Rectangle := (X      => Rectangle.X + Alloc.X,
                          Y      => Rectangle.Y + Alloc.Y,
                          Width  => Alloc.Width,
                          Height => Alloc.Height);
            Parent_Rectangle := Rectangle;

            In_Central := In_Central_Area (MDI, Parent);

            Border_Height := Gint'Min
              (Max_Drag_Border_Width, Rectangle.Height / 3);
            Border_Width :=
              Gint'Min (Max_Drag_Border_Width, Rectangle.Width / 3);

            if Y < Rectangle.Y + Border_Height then
               Position := Position_Top;
               Rectangle.Height := Border_Height;

            elsif Y > Rectangle.Y + Rectangle.Height - Border_Height then
               Position := Position_Bottom;
               Rectangle.Y := Rectangle.Y + Rectangle.Height - Border_Height;
               Rectangle.Height := Border_Height;

            elsif X < Rectangle.X + Border_Width then
               Position := Position_Left;
               Rectangle.Width := Border_Width;

            elsif X > Rectangle.X + Rectangle.Width - Border_Width then
               Position := Position_Right;
               Rectangle.X := Rectangle.X + Rectangle.Width - Border_Width;
               Rectangle.Width := Border_Width;

            else
               Position := Position_Automatic;
               Rectangle :=
                 (X      => Rectangle.X + Border_Width,
                  Y      => Rectangle.Y + Border_Height,
                  Width  => Rectangle.Width - 2 * Border_Width,
                  Height => Rectangle.Height - 2 * Border_Height);
            end if;
         end if;

         Allowed :=
           MDI.Drag_Areas = Both
           or else (MDI.Drag_Areas = Central_Only and then In_Central)
           or else (MDI.Drag_Areas = Sides_Only and then not In_Central);
      end if;
   end Get_Dnd_Target;

   --------------------------
   -- List_Of_Perspectives --
   --------------------------

   function List_Of_Perspectives
     (MDI : access MDI_Window_Record)
      return GNAT.Strings.String_List_Access
   is
   begin
      return MDI.Perspective_Names;
   end List_Of_Perspectives;

   -------------------------
   -- Current_Perspective --
   -------------------------

   function Current_Perspective
     (MDI : access MDI_Window_Record'Class) return String is
   begin
      if MDI.Current_Perspective = null then
         return "";
      else
         return Get_Attribute (MDI.Current_Perspective, "name", "");
      end if;
   end Current_Perspective;

   ------------------
   -- Freeze_Focus --
   ------------------

   procedure Freeze_Focus (MDI : access MDI_Window_Record'Class) is
   begin
      MDI.Focus_Freeze := MDI.Focus_Freeze + 1;
   end Freeze_Focus;

   ----------------
   -- Thaw_Focus --
   ----------------

   procedure Thaw_Focus (MDI : access MDI_Window_Record'Class) is
   begin
      if MDI.Focus_Freeze = 0 then
         if Traces then
            Print_Debug ("Calls to (Freeze|Thaw)_Focus do not match");
         end if;
         return;
      end if;

      MDI.Focus_Freeze := MDI.Focus_Freeze - 1;
      if MDI.Focus_Freeze = 0
        and then MDI.Focus_Child /= null
      then
         MDI.Set_Focus_Child (MDI.Focus_Child);
      end if;
   end Thaw_Focus;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Object  : access MDI_Window_Record'Class;
      C_Name  : Glib.Signal_Name;
      Handler : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After   : Boolean) is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_MDI_Window_MDI_Child_Void'Access,
         Handler     => Cb_To_Address (Handler), --  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
     (Object  : access MDI_Window_Record'Class;
      C_Name  : Glib.Signal_Name;
      Handler : Cb_GObject_MDI_Child_Void;
      After   : Boolean;
      Slot    : access Glib.Object.GObject_Record'Class := null) is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_MDI_Child_Void'Access,
         Handler     => Cb_To_Address (Handler), --  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ----------------------------------
   -- Marsh_GObject_MDI_Child_Void --
   ----------------------------------

   procedure Marsh_GObject_MDI_Child_Void
     (Closure         : GClosure;
      Return_Value    : Glib.Values.GValue;
      N_Params        : Glib.Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_MDI_Child_Void :=
        Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject :=
        Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, MDI_Child (Unchecked_To_Object (Params, 1)));
   exception when E : others => Process_Exception (E);
   end Marsh_GObject_MDI_Child_Void;

   -------------------------------------
   -- Marsh_MDI_Window_MDI_Child_Void --
   -------------------------------------

   procedure Marsh_MDI_Window_MDI_Child_Void
     (Closure         : GClosure;
      Return_Value    : Glib.Values.GValue;
      N_Params        : Glib.Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtkada_MDI_Window_MDI_Child_Void :=
        Address_To_Cb (Get_Callback (Closure));
      Obj : constant MDI_Window :=
        MDI_Window (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, MDI_Child (Unchecked_To_Object (Params, 1)));
   exception when E : others => Process_Exception (E);
   end Marsh_MDI_Window_MDI_Child_Void;

   -----------------------
   -- On_Child_Selected --
   -----------------------

   procedure On_Child_Selected
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After : Boolean := False) is
   begin
      Connect (Self, Signal_Child_Selected & ASCII.NUL, Call, After);
   end On_Child_Selected;

   -----------------------
   -- On_Child_Selected --
   -----------------------

   procedure On_Child_Selected
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_GObject_MDI_Child_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False) is
   begin
      Connect_Slot
        (Self, Signal_Child_Selected & ASCII.NUL, Call, After, Slot);
   end On_Child_Selected;

   --------------------
   -- On_Float_Child --
   --------------------

   procedure On_Float_Child
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After : Boolean := False) is
   begin
      Connect (Self, Signal_Float_Child & ASCII.NUL, Call, After);
   end On_Float_Child;

   --------------------
   -- On_Float_Child --
   --------------------

   procedure On_Float_Child
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_GObject_MDI_Child_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False) is
   begin
      Connect_Slot
        (Self, Signal_Float_Child & ASCII.NUL, Call, After, Slot);
   end On_Float_Child;

   ----------------------------
   -- On_Child_Title_Changed --
   ----------------------------

   procedure On_Child_Title_Changed
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After : Boolean := False) is
   begin
      Connect (Self, Signal_Child_Title_Changed & ASCII.NUL, Call, After);
   end On_Child_Title_Changed;

   ----------------------------
   -- On_Child_Title_Changed --
   ----------------------------

   procedure On_Child_Title_Changed
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_GObject_MDI_Child_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False) is
   begin
      Connect_Slot
        (Self, Signal_Child_Title_Changed & ASCII.NUL, Call, After, Slot);
   end On_Child_Title_Changed;

   --------------------
   -- On_Child_Added --
   --------------------

   procedure On_Child_Added
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After : Boolean := False) is
   begin
      Connect (Self, Signal_Child_Added & ASCII.NUL, Call, After);
   end On_Child_Added;

   --------------------
   -- On_Child_Added --
   --------------------

   procedure On_Child_Added
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_GObject_MDI_Child_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False) is
   begin
      Connect_Slot
        (Self, Signal_Child_Added & ASCII.NUL, Call, After, Slot);
   end On_Child_Added;

   ----------------------
   -- On_Child_Removed --
   ----------------------

   procedure On_Child_Removed
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After : Boolean := False) is
   begin
      Connect (Self, Signal_Child_Removed & ASCII.NUL, Call, After);
   end On_Child_Removed;

   ----------------------
   -- On_Child_Removed --
   ----------------------

   procedure On_Child_Removed
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_GObject_MDI_Child_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False) is
   begin
      Connect_Slot
        (Self, Signal_Child_Removed & ASCII.NUL, Call, After, Slot);
   end On_Child_Removed;

   ---------------------------
   -- On_Child_Icon_Changed --
   ---------------------------

   procedure On_Child_Icon_Changed
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After : Boolean := False) is
   begin
      Connect (Self, Signal_Child_Icon_Changed & ASCII.NUL, Call, After);
   end On_Child_Icon_Changed;

   ---------------------------
   -- On_Child_Icon_Changed --
   ---------------------------

   procedure On_Child_Icon_Changed
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_GObject_MDI_Child_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False) is
   begin
      Connect_Slot
        (Self, Signal_Child_Icon_Changed & ASCII.NUL, Call, After, Slot);
   end On_Child_Icon_Changed;

   ---------------------
   -- On_Delete_Event --
   ---------------------

   procedure On_Delete_Event
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After : Boolean := False) is
   begin
      Connect (Self, Signal_Delete_Event & ASCII.NUL, Call, After);
   end On_Delete_Event;

   ---------------------
   -- On_Delete_Event --
   ---------------------

   procedure On_Delete_Event
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_GObject_MDI_Child_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False) is
   begin
      Connect_Slot
        (Self, Signal_Delete_Event & ASCII.NUL, Call, After, Slot);
   end On_Delete_Event;

   ----------------------
   -- On_Unfloat_Child --
   ----------------------

   procedure On_Unfloat_Child
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After : Boolean := False) is
   begin
      Connect (Self, Signal_Unfloat_Child & ASCII.NUL, Call, After);
   end On_Unfloat_Child;

   ----------------------
   -- On_Unfloat_Child --
   ----------------------

   procedure On_Unfloat_Child
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_GObject_MDI_Child_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False) is
   begin
      Connect_Slot
        (Self, Signal_Unfloat_Child & ASCII.NUL, Call, After, Slot);
   end On_Unfloat_Child;

   -----------------------------
   -- On_Before_Unfloat_Child --
   -----------------------------

   procedure On_Before_Unfloat_Child
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After : Boolean := False) is
   begin
      Connect (Self, Signal_Before_Unfloat_Child & ASCII.NUL, Call, After);
   end On_Before_Unfloat_Child;

   -----------------------------
   -- On_Before_Unfloat_Child --
   -----------------------------

   procedure On_Before_Unfloat_Child
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_GObject_MDI_Child_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False) is
   begin
      Connect_Slot
        (Self, Signal_Before_Unfloat_Child & ASCII.NUL, Call, After, Slot);
   end On_Before_Unfloat_Child;

   -----------------------------
   -- On_Before_Destroy_Child --
   -----------------------------

   procedure On_Before_Destroy_Child
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After : Boolean := False) is
   begin
      Connect (Self, Signal_Before_Destroy_Child & ASCII.NUL, Call, After);
   end On_Before_Destroy_Child;

   -----------------------------
   -- On_Before_Unfloat_Child --
   -----------------------------

   procedure On_Before_Destroy_Child
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_GObject_MDI_Child_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False) is
   begin
      Connect_Slot
        (Self, Signal_Before_Destroy_Child & ASCII.NUL, Call, After, Slot);
   end On_Before_Destroy_Child;

   ----------------------------
   -- On_Before_Remove_Child --
   ----------------------------

   procedure On_Before_Remove_Child
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_Gtkada_MDI_Window_MDI_Child_Void;
      After : Boolean := False) is
   begin
      Connect (Self, Signal_Before_Remove_Child & ASCII.NUL, Call, After);
   end On_Before_Remove_Child;

   -----------------------------
   -- On_Before_Unfloat_Child --
   -----------------------------

   procedure On_Before_Remove_Child
     (Self  : not null access MDI_Window_Record'Class;
      Call  : Cb_GObject_MDI_Child_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False) is
   begin
      Connect_Slot
        (Self, Signal_Before_Remove_Child & ASCII.NUL, Call, After, Slot);
   end On_Before_Remove_Child;
end Gtkada.MDI;
