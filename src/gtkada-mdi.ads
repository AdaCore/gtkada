-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2001-2004 ACT-Europe                --
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

with Ada.Tags;
with Glib;        use Glib;
with Glib.Xml_Int;
with Gdk.GC;
with Gdk.Color;
with Gdk.Event;
with Gdk.Pixbuf;
with Gdk.Rectangle;
with Gtk.Accel_Group;
with Gtk.Box;
with Gtk.Drawing_Area;
with Gtk.Enums;
with Gtk.Event_Box;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Menu_Item;
with Gtk.Style;
with Gtk.Check_Menu_Item;
with Gtk.Radio_Menu_Item;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.Multi_Paned;
with Pango.Font;
with Pango.Layout;

package Gtkada.MDI is

   type MDI_Window_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type MDI_Window is access all MDI_Window_Record'Class;
   --  Although this widget is implemented as a gtk_layout, you shouldn't
   --  use the standard Gtk_Layout functions like Put and Move yourself.

   type MDI_Child_Record is new Gtk.Event_Box.Gtk_Event_Box_Record
     with private;
   type MDI_Child is access all MDI_Child_Record'Class;
   --  A child of the MDI, that encapsulates the widgets you have put in the
   --  MDI window.
   --  You can easily convert from this to the initial widget using the
   --  functions Find_MDI_Child and Get_Widget.

   type MDI_Child_Array is array (Natural range <>) of MDI_Child;
   No_Children : constant MDI_Child_Array := (1 .. 0 => null);

   type State_Type is (Normal, Floating);
   --  This type indicates the state of an item in the MDI:
   --  - Normal: the item can be manipulated (moved and resized) by the user.
   --      It is found either in the middle notebook (maximized items), or
   --      in the layout.
   --  - Floating: the item has its own toplevel window, and is thus managed
   --      by the window manager.

   procedure Gtk_New
     (MDI   : out MDI_Window;
      Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   --  Create a new MDI window.
   --  Note that it is recommended that you modify the style (Set_Background
   --  in State_Normal) to have a different color.
   --  You should call Setup_Toplevel_Window once you have added the MDI to a
   --  toplevel widget, so that focus is correctly handled when the toplevel
   --  window gains the focus

   procedure Initialize
     (MDI   : access MDI_Window_Record'Class;
      Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Setup_Toplevel_Window
     (MDI    : access MDI_Window_Record;
      Parent : access Gtk.Window.Gtk_Window_Record'Class);
   --  Setup Parent to properly handle focus when the window manager changes
   --  the window that currently has the focus.
   --  Parent must be the toplevel window that contains the MDI.

   type Show_Tabs_Policy_Enum is (Always, Never, Automatic);

   procedure Configure
     (MDI                       : access MDI_Window_Record;
      Opaque_Resize             : Boolean := False;
      Close_Floating_Is_Unfloat : Boolean := True;
      Title_Font         : Pango.Font.Pango_Font_Description := null;
      Background_Color   : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Title_Bar_Color    : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Focus_Title_Color  : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Draw_Title_Bars    : Boolean             := True;
      Tabs_Position      : Gtk.Enums.Gtk_Position_Type := Gtk.Enums.Pos_Bottom;
      Show_Tabs_Policy   : Show_Tabs_Policy_Enum := Automatic);
   --  Change the setup of the MDI.
   --  Close_Floating_Is_Unfloat, if True, means that closing a floating child
   --  will put it back in the MDI instead of destroying it (unless its flag
   --  Always_Destroy_Float is set).
   --  Title_Font is the font used in the title bars (if null, "sans 8"
   --  is used).
   --  The colors, when Null_Color, will not change the current setup.
   --  If Draw_Title_Bars is False, then no extra title bar will be displayed
   --  for the MDI children when they are maximized. This saves space on the
   --  screen. However, the notebook tabs will be highlighted with
   --  Title_Bar_Color in exchange.
   --  Tabs_Position indicates where the notebook tabs should be put.
   --  Show_Tabs_Policy indicates when the notebook tabs should be displayed.

   type Child_Flags is mod 2 ** 5;
   Destroy_Button       : constant Child_Flags := 2 ** 2;
   Float_As_Transient   : constant Child_Flags := 2 ** 3;
   Always_Destroy_Float : constant Child_Flags := 2 ** 4;
   All_Buttons          : constant Child_Flags := Destroy_Button;
   --  Special flags to set up the widgets:
   --  The first is the buttons that should be displayed in the title
   --  bar of the MDI children.
   --  If Float_As_Transient is set, then the child will be set up as a
   --  transient window when floating: on most window managers, it will stay on
   --  top of the MDI, but the window will have less decorations in its title
   --  bar, in particular no destroy button. In such a case, <Esc> will close
   --  the window, or unfloat it depending on the MDI's setup, as is the case
   --  for all dialogs in GtkAda. The MDI's setup will be ignored (and the
   --  child always destroyed when Esc is pressed) if Always_Destroy_Float is
   --  true.

   procedure Gtk_New
     (Child  : out MDI_Child;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags  : Child_Flags := All_Buttons);
   --  Create a new MDI child that contains widget.

   procedure Initialize
     (Child  : access MDI_Child_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags  : Child_Flags);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   type Child_Position is new Positive;
   Position_Bottom  : constant Child_Position := 1;
   Position_Top     : constant Child_Position := 2;
   Position_Left    : constant Child_Position := 3;
   Position_Right   : constant Child_Position := 4;
   Position_Default : constant Child_Position := 5;

   function Put
     (MDI          : access MDI_Window_Record;
      Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position     : Child_Position := Position_Default;
      Flags        : Child_Flags := All_Buttons;
      Focus_Widget : Gtk.Widget.Gtk_Widget := null) return MDI_Child;
   --  Add a new child to the MDI window, and return its embedding widget.
   --  Child mustn't be a Gtk_Window (or one of its inheriting types).
   --  Otherwise, Program_Error is raised.
   --
   --  Flags indicates which buttons should be made visible in the title bar.
   --
   --  You shouldn't access Child directly afterwards, but should manipulate
   --  its MDI_Child instead. However, as a special exception, you can
   --  still pass Child as a parameter to the subprograms in this package to
   --  manipulate it (e.g. in Raise_Child, ...)
   --
   --  Note: You might have to call Set_Size_Request on Child to set its
   --  initial size. This won't prevent it from being resized by the user.
   --
   --  If Child is a MDI_Child, its location is recomputed automatically.
   --
   --  If Focus_Widget is not null, this is the widget that gets the keyboard
   --  focus when the child is selected.
   --
   --  Calling Put does not give the focus to the newly inserted widget.
   --  To do that, you should call Set_Focus_Child.
   --
   --  Position is used to place the new widget at the right position in the
   --  MDI. The algorithm is the following:
   --  - If position is Bottom, then the MDI first check
   --      whether there is already a child with that attribute.
   --      * If yes, the new child is put in the same notebook as that one.
   --      * If no, a new window is created below all others, splitting the MDI
   --        as needed. The new child is put in that new window and has the
   --        attribute Bottom.
   --      When the user moves (through drag-and-drop or split) a Bottom window
   --      out of the notebook, that window keeps its Bottom attribute. As a
   --      result, there can be multiple notebooks with this attribute.
   --  - Likewise for Top, Left and Right
   --  - If position is something other: the MDI checks whether the current
   --    notebook has a window with the same attribute value. If yet, the new
   --    child is put in that same notebook. If no, it looks for the first
   --    notebook with at least one child with that attribute. The goal of this
   --    rule is to ensure that larger windows, like editors, are not put on
   --    smaller windows, like a project view, even if the latter currently has
   --    the focus.
   --    If there is no window with a child with the same position, then the
   --    window is placed in the default area (empty initially)
   --
   --  You can create new Position types as needed, for instance if you want
   --  to group similar windows together automatically.

   procedure Set_Size
     (MDI    : access MDI_Window_Record;
      Child  : access MDI_Child_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint;
      Fixed_Size : Boolean := False);
   --  Forces a new size for a child. If Width or Height is left to -1, the
   --  matching size will be computed from the child's requisition. If they are
   --  left to 0, the corresponding length is left to its current value.
   --  If Fixed_Size is True, then the widget will not be resized when the MDI
   --  itself is resized (unless the user has first moved one of the handles to
   --  manually resize it). Otherwise, it will grow proportionally with the
   --  rest of the MDI.

   procedure Close
     (MDI   : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class;
      Force : Boolean := False);
   --  Close the child that contains Child, and remove its window from the
   --  MDI. See also Close_Child if you need to close a MDI_Child itself.
   --  This first checks through a delete_event callback whether the child
   --  accepts to be closed.
   --  "delete_event" is not sent, and the child is automatically closed, if
   --  Force is set to True.

   procedure Set_Title
     (Child       : access MDI_Child_Record;
      Title       : UTF8_String;
      Short_Title : UTF8_String := "");
   --  Set the title for a child. Title is the title put in titlebar of
   --  the children, whereas Short_Title is the name of the notebook tab when
   --  children are maximized. By default, it is the same as Title.
   --
   --  The default title is the empty string.
   --  This title will be the one used for the window when the child is set to
   --  floating state.

   function Get_Title (Child : access MDI_Child_Record) return UTF8_String;
   --  Return the title for a specific child

   function Get_Short_Title
     (Child : access MDI_Child_Record) return UTF8_String;
   --  Return the name of the notebook tab used when children are maximized.

   function Get_State (Child : access MDI_Child_Record) return State_Type;
   --  Return the current state of the child

   procedure Set_Icon
     (Child : access MDI_Child_Record;
      Icon  : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Associate an icon with Child. This icon is visible in the title bar, the
   --  notebook tabs, the Window menu and the interactive selection dialog.
   --  The icon is updated dynamically on the screen.

   function Get_Icon
     (Child : access MDI_Child_Record) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Returns the icon associated with Child

   function Dnd_Data
     (Child : access MDI_Child_Record; Copy : Boolean) return MDI_Child;
   --  When a drag-and-drop operation took place to move a child from one
   --  position to the next, this function is called to know what child should
   --  be moved.
   --  As a result, the implementer can choose whether a copy of the child
   --  should be returned (creating a new view for an editor for instance), or
   --  if the child itself should be moved (the default).
   --  The returned MDI_Child must have been added to the MDI before it is
   --  returned.
   --  Copy is set to true if a copy operation was requested, to False if a
   --  simple move operation was requested.

   -----------
   -- Menus --
   -----------

   function Create_Menu
     (MDI : access MDI_Window_Record) return Gtk.Menu.Gtk_Menu;
   --  Create a dynamic menu that can then be inserted into a menu bar. This
   --  menu is dynamic, ie its content will changed based on the focus
   --  child.
   --  If this function is called several times, the same menu is returned
   --  every time.

   ------------------------
   -- Selecting children --
   ------------------------

   procedure Highlight_Child
     (Child : access MDI_Child_Record; Highlight : Boolean := True);
   --  Highlight the child until it is selected by the user.
   --  The color of its menu label and of the text in the notebook tabs is
   --  changed.
   --  Nothing is done if the child is already fully visible (either in the
   --  active page in one of the notebooks, or the child that has the selection
   --  in the layout).
   --  This is meant to be used as a graphical note to the user that the child
   --  has been updated and the user should look at it.

   function Get_Focus_Child
     (MDI : access MDI_Window_Record) return MDI_Child;
   --  Return the child that currently has the MDI focus.
   --  null is returned if no child has the focus.

   procedure Set_Focus_Child
     (MDI : access MDI_Window_Record;
      Containing : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Give the focus to the child containing Containing. This will not
   --  Grab_Focus for the child in all cases, since you might want to give the
   --  focus to some specific part of your widget (an entry field,...) in some
   --  cases.

   procedure Set_Focus_Child (Child : access MDI_Child_Record'Class);
   --  Make Child the active widget, and raise it at the top.

   procedure Check_Interactive_Selection_Dialog
     (MDI          : access MDI_Window_Record;
      Event        : Gdk.Event.Gdk_Event;
      Move_To_Next : Boolean;
      Visible_In_Central_Only : Boolean := False);
   --  Open the interactive dialog for selecting windows.
   --  This dialog should be open as a result of a key press event.
   --  Move_To_Next indicates whether we want to select the next child (True)
   --  or the previous child (False).
   --  This dialog will be closed only when the key that opened it is fully
   --  released. For instance, if the dialog was opened as a result of
   --  pressing Ctrl-Tab, the dialog will only be closed when Ctrl itself is
   --  released.
   --  You can call this procedure even if a dialog is currently open. This
   --  simply forces a move to the next or previous child. In fact, it is
   --  your responsability to call this procedure when the user presses
   --  the keys to move between children.
   --
   --  If Event is null, then no dialog is displayed. Instead, the next or
   --  previous child is immediately selected.
   --
   --  If Visible_In_Central_Only is set, then only the children currently
   --  visible in the central area can be selected. There is only one such
   --  child unless the central area was splitted. Event is ignored in this
   --  case, and the selection is not interactive

   --  This function is not internal to the MDI since connecting to the
   --  key_press_event and key_release_event should be done in the gtk_window
   --  that contains the MDI. Otherwise, some events are intercepted by gtk+,
   --  for instance the key_release_events, and the key_press_events for some
   --  specified keys.
   --  It also gives the choice to the application of whether this feature is
   --  wanted or not.

   -----------------------------------------
   -- MDI_Child and encapsulated children --
   -----------------------------------------

   function Get_Widget
     (Child : access MDI_Child_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the widget that Child encapsulates. This is the widget you
   --  initially Put() in MDI.

   function Find_MDI_Child
     (MDI    : access MDI_Window_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return MDI_Child;
   --  Return the MDI_Child that encapsulates Widget.
   --  Widget must be the exact same one you gave in argument to Put.

   function Find_MDI_Child_By_Tag
     (MDI : access MDI_Window_Record;
      Tag : Ada.Tags.Tag) return MDI_Child;
   --  Return the first child matching Tag

   function Find_MDI_Child_By_Name
     (MDI  : access MDI_Window_Record;
      Name : String) return MDI_Child;
   --  Return the first child matching Name.

   type Child_Iterator is private;

   function First_Child (MDI : access MDI_Window_Record) return Child_Iterator;
   --  Return an access to the first child of the MDI.
   --  It is garanteed that the first child is the one that currently has the
   --  focus in the MDI.

   procedure Next (Iterator : in out Child_Iterator);
   --  Move to the next child in the MDI

   function Get (Iterator : Child_Iterator) return MDI_Child;
   --  Return the child pointed to by Iterator.
   --  If Iterator is no longer valid, null is returned.

   -----------------------------------
   -- Floating and closing children --
   -----------------------------------

   procedure Float_Child
     (Child : access MDI_Child_Record'Class; Float : Boolean);
   --  Change the floating state of a child

   function Is_Floating
     (Child : access MDI_Child_Record'Class) return Boolean;
   --  Return True if Child is currently in a separate window

   procedure Close_Child
     (Child : access MDI_Child_Record'Class;
      Force : Boolean := False);
   --  Same as Close, but applies directly to a MDI_Child.

   procedure Set_All_Floating_Mode
     (MDI : access MDI_Window_Record; All_Floating : Boolean);
   --  If All_Floating is set to true, the MDI will have a size of 0x0, and all
   --  children are set to floating. This can be used if you wish to let the
   --  window manager handle the windows. If All_Floating is True, children
   --  can no longer be maximized.

   ---------------------------
   -- Reorganizing children --
   ---------------------------

   procedure Raise_Child
     (Child : access MDI_Child_Record'Class; Give_Focus : Boolean := True);
   --  Put Child in the foreground.
   --  Note that this does not give the focus to this child, unless
   --  Give_Focus is set to True

   function Is_Raised (Child : access MDI_Child_Record'Class) return Boolean;
   --  Whether the child is currently raised, ie fully visible to the user

   procedure Lower_Child (Child : access MDI_Child_Record'Class);
   --  Put Child in the background.
   --  If the children are maximized, this selected the next page from the
   --  notebook.

   procedure Split
     (MDI               : access MDI_Window_Record;
      Orientation       : Gtk.Enums.Gtk_Orientation;
      Reuse_If_Possible : Boolean := False;
      After             : Boolean := False;
      Width, Height     : Glib.Gint := 0);
   --  Split the central area. The split starting from either the currently
   --  selected child or the last child that had the focus in that area.
   --  If Reuse_If_Possible is True, and the current child is already splitted
   --  in the right directory, we reuse that area.
   --  If After is true, then the currently selected child is put below or
   --  to the right in the splitted area, otherwise it is left on the top or
   --  left of that area).
   --  Width and Height indicate the desired geometry for the splitted area,
   --  0 indicate a 50/50 split.

   ----------------------
   -- Desktop Handling --
   ----------------------
   --  The MDI provides a way to save desktops, i.e the list of children
   --  currently open in the MDI and their location. It can then restore the
   --  desktop at some later point.
   --
   --  Desktops require support from the widgets that are put in the MDI. They
   --  need to register a function to save them and a function to recreate
   --  them. Using Ada streams for this didn't prove workable since some
   --  children might need extra parameters not available to them through
   --  streams. This is why the following subprograms are in a generic package,
   --  so that you can pass whatever parameter(s) is needed in your
   --  application.
   --
   --  Desktops are saved and restored in XML trees.
   --
   --  Note that you can instantiate several of these packages, but you need to
   --  call Save_Desktop and Restore_Desktop from each of them if you want to
   --  save the whole contents of the MDI. The resulting XML nodes can then be
   --  merged into a single XML tree if needed.

   procedure Add_To_Tree
     (Tree        : in out Glib.Xml_Int.Node_Ptr;
      ID_Node     : Glib.Xml_Int.Node_Ptr;
      Position    : Child_Position := Position_Default;
      Focus       : Boolean := False;
      Raised      : Boolean := False);
   --  Add an item to a Tree that can then be loaded through
   --  a Load_Desktop_Function, see below.
   --  Tree can be null, in which case it will be initialized with
   --  values relative to the MDI.
   --  If Focus is True, then the widget will be given the focus, unless
   --  another widget is also registered later on with Focus set to True.
   --  It isn't possible to define a floating child as a default

   generic
      type User_Data (<>) is private;
      --  Generic type of parameter that is passed to all the children's save
      --  and restore functions.

      --  This package needs to be instantiated at library level

   package Desktop is

      type Save_Desktop_Function is access function
        (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
         User   : User_Data)
         return Glib.Xml_Int.Node_Ptr;
      --  A general function that dumps the parameters of a widget into an XML
      --  tree.
      --
      --  Note: you should register one such function for all the widget types
      --  you will put in the MDI and that need to be saved when a desktop is
      --  saved. The MDI will call all the registered functions one after the
      --  other. Therefore, your function should return null if Widget is not
      --  of a type that is it can handle.

      type Load_Desktop_Function is access function
        (MDI : MDI_Window; Node : Glib.Xml_Int.Node_Ptr; User : User_Data)
         return MDI_Child;
      --  A general function that loads a widget from an XML tree.
      --
      --  As for Save_Desktop_Function, this function should return null if it
      --  doesn't know how to handle Node or if Node doesn't describe a widget
      --  type that it can handle.
      --
      --  This function returns an MDI_Widget that has been put in the MDI.

      procedure Register_Desktop_Functions
        (Save : Save_Desktop_Function;
         Load : Load_Desktop_Function);
      --  Register a set of functions to save and load desktops for some
      --  specific widget types.
      --  Neither Save nor Load can be null.

      function Restore_Desktop
        (MDI       : access MDI_Window_Record'Class;
         From_Tree : Glib.Xml_Int.Node_Ptr;
         User      : User_Data) return Boolean;
      --  Restore the contents of the MDI from its saved XML tree.
      --  User is passed as a parameter to all of the Load_Desktop_Function
      --  registered by the widgets.
      --  Return False if the desktop couldn't be loaded

      function Save_Desktop
        (MDI : access MDI_Window_Record'Class;
         User : User_Data) return Glib.Xml_Int.Node_Ptr;
      --  Return an XML tree that describes the current contents of the MDI.
      --  This function calls each of the registered function for the children
      --  of the MDI.

      procedure Free_Registered_Desktop_Functions;
      --  Free the memory allocated for the registered functions.

   private
      type Register_Node_Record;
      type Register_Node is access Register_Node_Record;
      type Register_Node_Record is record
         Save : Save_Desktop_Function;
         Load : Load_Desktop_Function;
         Next : Register_Node;
      end record;

      Registers : Register_Node;
      --  Global variable that contains the list of functions that have been
      --  registered.
   end Desktop;

   function Desktop_Was_Loaded (MDI : access MDI_Window_Record) return Boolean;
   --  Return True if a desktop was loaded, False if the MDI is only the result
   --  of calls to Gtk_New and Put.

   procedure Present_On_Child_Focus
     (MDI     : access MDI_Window_Record;
      Present : Boolean);
   --  Whether giving the focus to a child should cause the window containing
   --  this child to be "Presented".
   --  This behaviour is enabled by default; it might be useful to disable it,
   --  for example when loading a desktop.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "child_selected"
   --    procedure Handler
   --       (MDI : access MDI_Window_Record'Class; Child : System.Address);
   --
   --    This signal is emitted when a new child has gained the focus. Convert
   --    Child to a MDI_Child by calling Gtk.Arguments.To_Object. This can be
   --    used to change some global information at the MDI level. You should
   --    connect to "selected" (see below) instead if you want to change some
   --    information at the child level.
   --    Child might be null if no child has the focus anymore
   --
   --  - "float_child"
   --    procedure Handler
   --       (MDI : access MDI_Window_Record'Class; Child : System.Address);
   --
   --    A child was set as floating in the MDI. A similar signal is emitted on
   --    the child itself.
   --
   --  - "child_title_changed"
   --    procedure Handler
   --      (MDI : access MDI_Window_Record'Class; Child : System.Address);
   --
   --    Emitted when the title of a child is changed.
   --
   --  </signals>
   --
   --  <signals>
   --  The following new signals are defined for the MDI_Child_Record object:
   --
   --  - "delete_event"
   --    function Handler (Child : access Gtk_Widget_Record'Class)
   --                     return Boolean;
   --
   --    This signal is emitted for each item in the MDI window before it is
   --    actually deleted. The child is destroyed only if the handler returns
   --    False.
   --    Note that the Child passed in argument is exactly the one you passed
   --    to Put to insert it in the MDI window.
   --    Note that this is also the signal to use to prevent top level
   --    Gtk_Window from being destroyed.
   --
   --  - "selected"
   --    procedure Handler (Child : access MDI_Child_Record'Class);
   --
   --    This is emitted when the child is selected, ie gains the
   --    MDI focus. You should probably also connect to the "grab_focus" signal
   --    to be informed when the child gets the keyboard focus. This can be
   --    used to transfer the focus to some specific part of the
   --    widget. Connecting to "grab_focus" should be done with the After
   --    parameter set to True.
   --
   --  - "float_child"
   --    procedure Handler (Child : access MDI_Child_Record'Class);
   --
   --    Emitted when a child is set as floating
   --
   --  - "unfloat_child"
   --    procedure Handler (Child : access MDI_Child_Record'Class);
   --
   --    Emitted when a child is put back in the main MDI window
   --
   --  </signals>

private
   type String_Access is access all UTF8_String;

   type MDI_Child_Record is new Gtk.Event_Box.Gtk_Event_Box_Record with record
      Initial : Gtk.Widget.Gtk_Widget;
      --  The widget we use to build this child.

      Main_Box : Gtk.Box.Gtk_Box;
      --  The main container.

      State : State_Type := Normal;

      Position : Child_Position := Position_Default;
      --  The attribute of the child.

      Title       : String_Access;
      Short_Title : String_Access;
      --  Title of the item, as it appears in the title bar.
      --  These are UTF8-Encoded

      MDI : MDI_Window;
      --  The MDI to which the child belongs. We cannot get this information
      --  directly from Get_Parent since some children are actually floating
      --  and do not belong to the MDI anymore.

      Menu_Item : Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item;
      --  The item in the dynamic menu that represents this child.

      Flags : Child_Flags;

      Focus_Widget : Gtk.Widget.Gtk_Widget;
      --  The widget which should actually get the keyboard focus

      Icon : Gdk.Pixbuf.Gdk_Pixbuf;

      Title_Box : Gtk.Box.Gtk_Box;
      --  Box that contains the title. It will be resized whenever the title
      --  font changes.

      Title_Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
      --  Area used to draw the title and icon.

      Tab_Label : Gtk.Label.Gtk_Label;
      --  label used when child is in a notebook, null if not in a notebook
   end record;

   type Child_Iterator is record
      Iter : Gtk.Widget.Widget_List.Glist;
   end record;

   type Drag_Status is (No_Drag, In_Pre_Drag, In_Drag);

   type MDI_Window_Record is new Gtkada.Multi_Paned.Gtkada_Multi_Paned_Record
   with record
      Items : Gtk.Widget.Widget_List.Glist := Gtk.Widget.Widget_List.Null_List;
      --  The list of all MDI children.

      Desktop_Was_Loaded : Boolean := False;
      --  True if a desktop was loaded

      Present_Window_On_Child_Focus : Boolean := True;
      --  Whether the window containing a child should be Presented when the
      --  child gets the focus.

      Focus_GC     : Gdk.GC.Gdk_GC;
      Non_Focus_GC : Gdk.GC.Gdk_GC;
      --  The various graphic contexts used to draw the titles of the
      --  children.

      Focus_Child : MDI_Child := null;
      --  The child that currently has the focus. Some default actions will
      --  apply to this child only.

      Menu               : Gtk.Menu.Gtk_Menu;
      Float_Menu_Item    : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
      Float_Menu_Item_Id : Gtk.Handlers.Handler_Id;
      Close_Menu_Item    : Gtk.Menu_Item.Gtk_Menu_Item;
      --  The dynamic menu used to provide access to the most common
      --  functions of MDI.

      Title_Layout        : Pango.Layout.Pango_Layout;
      --  Layout used to draw titles in the MDI children

      Title_Bar_Height    : Glib.Gint;
      --  Height of the title bar for all the children

      Close_Floating_Is_Unfloat : Boolean;
      --  True if destroying a floating window will put the child back in the
      --  MDI instead of destroying it. False if the child should be destroyed
      --  (provided it accepts so in its delete_event handler).

      Highlight_Style : Gtk.Style.Gtk_Style;
      --  Style to use to highlight the tabs and menus for the highlighted
      --  children.

      Background_Color  : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Title_Bar_Color   : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Focus_Title_Color : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Default_Title_Color : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;

      Draw_Title_Bars   : Boolean             := True;
      Tabs_Position     : Gtk.Enums.Gtk_Position_Type := Gtk.Enums.Pos_Bottom;
      Show_Tabs_Policy  : Show_Tabs_Policy_Enum := Automatic;

      Selection_Dialog : Gtk.Widget.Gtk_Widget;
      --  The interactive dialog for selecting new children.

      Dnd_Window       : Gtk.Window.Gtk_Window;
      Dnd_Window_Label : Gtk.Label.Gtk_Label;
      --  The small window displayed while a drag-and-drop operation is
      --  taking place.

      Group : Gtk.Accel_Group.Gtk_Accel_Group;

      All_Floating_Mode : Boolean := False;
      --  Set to true if all windows should be set to floating

      --  Handling of Dnd
      Drag_Start_X, Drag_Start_Y : Gint;
      In_Drag : Drag_Status := No_Drag;
      Dnd_Rectangle : Gdk.Rectangle.Gdk_Rectangle;
      Dnd_Rectangle_Owner : Gdk.Gdk_Window;
      Dnd_Xor_GC : Gdk.Gdk_GC;
   end record;

   pragma Inline (Get_Widget);
   pragma Inline (Get_Focus_Child);
   pragma Inline (Get);
   pragma Inline (Next);
   pragma Inline (First_Child);
end Gtkada.MDI;
