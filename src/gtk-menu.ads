-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--
--  This widget implements a drop-down menu.
--  This is basically a simple box that contains a series of Gtk_Menu_Item
--  widgets, on which the user can click to perform actions.
--
--  Such a menu is usually part of a Gtk_Menu_Bar (at the top of the window),
--  or activated by clicking on an item in another Gtk_Menu.
--  See also Gtk_Option_Menu for another way of displaying menus.
--
--  All the menus in GtkAda can be "Tear off" menus, i.e you can detach them
--  them from their parent (either a menu bar or another menu) to keep them
--  visible on the screen at all times).
--
--  </description>
--  <c_version> 1.2.6 </c_version>

with Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Menu_Item;
with Gtk.Menu_Shell;
with Gtk.Widget;

package Gtk.Menu is

   type Gtk_Menu_Record is new Gtk.Menu_Shell.Gtk_Menu_Shell_Record
     with private;
   type Gtk_Menu is access all Gtk_Menu_Record'Class;

   type Gtk_Menu_Detach_Func is access procedure
     (Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu          : access Gtk_Menu_Record'Class);
   --  Function called when a menu previously attached to a widget is detached.
   --  An access to this function is given in Attach_To_Widget.

   ---------------------
   -- Creating a menu --
   ---------------------

   procedure Gtk_New (Widget : out Gtk_Menu);
   --  Creates a new empty menu

   procedure Initialize (Widget : access Gtk_Menu_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Append
     (Menu  : access Gtk_Menu_Record;
      Child : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class);
   --  Append a new item to the menu.
   --  The new item is added at the end of the menu.

   procedure Insert
     (Menu     : access Gtk_Menu_Record;
      Child    : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Position : in Gint := 0);
   --  Adds a new item to the menu, at a given position.
   --  The first position in the menu is number 0.
   --  If POSITION is 0, this procedure is the same as Prepend.

   procedure Prepend
     (Menu  : access Gtk_Menu_Record;
      Child : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class);
   --  Adds a new item at the end of a menu

   procedure Reorder_Child
     (Menu     : access Gtk_Menu_Record;
      Child    : in     Gtk.Widget.Gtk_Widget_Record'Class;
      Position : in     Gint);
   --  Moves an existing menu_item within the menu.
   --  Its new position is given by POSITION, 0 being the first item in the
   --  menu.
   --  If CHILD does not exist in the menu, nothing is done.

   procedure Set_Tearoff_State (Menu     : access Gtk_Menu_Record;
                                Torn_Off : in     Boolean);
   --  Modifies the tearoff status of the menu.
   --  If TORN_OFF is False, the menu is displayed as a drop down menu which
   --  disappears when the menu is not active. If TORN_OFF is True, the menu
   --  persists until it is closed or reattached.
   --  Note that you can give the user access to this functionnality by
   --  inserting a Gtk_Tearoff_Menu_Item in the menu.

   procedure Set_Title (Menu  : access Gtk_Menu_Record;
                        Title : in     String);
   --  Sets the title of the menu.
   --  TITLE is displayed when the menu is displayed as a tearoff menu in an
   --  independant window.

   procedure Set_Active
     (Menu  : access Gtk_Menu_Record;
      Index : in Guint);
   --  Selects a specified item in the menu.
   --  You will almost never need this function, it is used internally by
   --  Gtk_Option_Menu.
   --  Note that the item is not considered as being pressed by the user, and
   --  thus no callback is called as a result.

   function Get_Active (Menu : access Gtk_Menu_Record)
                        return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Gets the active menu item.
   --  In a Gtk_Option_Menu, this is the item that is currently shown in the
   --  button.

   -----------------------
   -- Displaying a menu --
   -----------------------

   type Gtk_Menu_Position_Func is access procedure
     (Menu : access Gtk_Menu_Record'Class;
      X    : out Gint;
      Y    : out Gint);
   --  This function is called displaying a popup menu on the screen.
   --  It should return the (X, Y) coordinates of the menu.
   --  Note that you might want to attach the menu to a widget first if you
   --  want to display the menu relative to its attached widget.
   --
   --  Note that there is a second version of this function (with added
   --  user data in the package User_Menu_Popup below

   procedure Popup
     (Menu              : access Gtk_Menu_Record;
      Parent_Menu_Shell : in Gtk.Menu_Shell.Gtk_Menu_Shell := null;
      Parent_Menu_Item  : in Gtk.Menu_Item.Gtk_Menu_Item := null;
      Func              : in Gtk_Menu_Position_Func := null;
      Button            : in Guint := 1;
      Activate_Time     : in Guint32 := 0);
   --  Displays a menu on the screen.
   --  This is the function to use to create contextual menus.
   --  Most of the time, PARENT_MENU_SHELL, PARENT_MENU_ITEM and FUNC can
   --  have a null value.
   --  PARENT_MENU_SHELL is the Gtk_Menu_Shell that contains PARENT_MENU_ITEM,
   --  ie the widget that triggered the display of the menu.
   --  FUNC is a function that returns the coordinates for the menu. If it is
   --  null, then a default function that positions the menu at the pointer
   --  location is used.
   --  BUTTON is the mouse button that was pressed to initiate the event.
   --  ACTIVATE_TIME is the time at which the event occured (you can get it
   --  directly from the Gdk_Event structure).
   --
   --  Note that a variant of this function is available in the generic package
   --  User_Menu_Popup.

   --  Note: in the Popup function, the Parent_* parameters are not access
   --  parameters because they might be null.

   generic
      type Data_Type is private;
   package User_Menu_Popup is
      type Gtk_Menu_Position_Func is access procedure
        (Menu      : access Gtk_Menu_Record;
         X         : out Gint;
         Y         : out Gint;
         User_Data : access Data_Type);

      procedure Popup
        (Menu              : access Gtk_Menu_Record;
         Data              : access Data_Type;
         Parent_Menu_Shell : in Gtk.Menu_Shell.Gtk_Menu_Shell := null;
         Parent_Menu_Item  : in Gtk.Menu_Item.Gtk_Menu_Item := null;
         Func              : in Gtk_Menu_Position_Func := null;
         Button            : in Guint := 1;
         Activate_Time     : in Guint32 := 0);

      --  Same as the Popup function above.
      --  Note that DATA is not duplicated, thus you should take care of the
      --  memory allocation/deallocation yourself.
      --  Note also that the order of parameters is slightly different from the
      --  C version.
   end User_Menu_Popup;

   procedure Popdown (Menu : access Gtk_Menu_Record);
   --  Removes the menu from the screen

   procedure Reposition (Menu : access Gtk_Menu_Record);
   --  Repositions a menu according to its position function.
   --  This function is set when Popup is called.

   --------------------------------
   -- Modifying the accelerators --
   --------------------------------

   procedure Set_Accel_Group
      (Menu        : access Gtk_Menu_Record;
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group'Class);
   --  Sets the ACCEL_GROUP that holds the global accelerators and key bindings
   --  for the menu.

   function Get_Accel_Group (Menu : access Gtk_Menu_Record)
                            return Accel_Group.Gtk_Accel_Group;
   --  Gets the accelerator group used to set the key bindings in the menu.

   function Get_Uline_Accel_Group (Menu : access Gtk_Menu_Record)
                                  return Accel_Group.Gtk_Accel_Group;
   --  Get the accelerator group that is used internally by the menu for
   --  underline accelerators while the menu is popped up.

   function Ensure_Uline_Accel_Group (Menu : access Gtk_Menu_Record)
                                     return Accel_Group.Gtk_Accel_Group;
   --  Make sure there exists a group of accelerators for the underline
   --  characters when the menu is popped up.

   ----------------------------------
   -- Attaching a menu to a widget --
   ----------------------------------

   procedure Attach_To_Widget
     (Menu          : access Gtk_Menu_Record;
      Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Detacher      : in Gtk_Menu_Detach_Func);
   --  Attach a menu to the widget.
   --  When the menu is detached from the widget (for instance when it is
   --  destroyed), the procedure DETACHED will be called.
   --  You will almost never need to use this function, unless you specifically
   --  want a call back when a widget becomes unavailable.
   --  If ATTACH_WIDGET is a menu_item with a single label in it, the name of
   --  the window created when MENU is teared-off will be the label in the
   --  menu_item.

   procedure Detach (Menu : access Gtk_Menu_Record);
   --  Detach the menu from its widget, and call the DETACHER set in
   --  Attach_To_Widget.

   function Get_Attach_Widget (Menu : access Gtk_Menu_Record)
                              return Gtk.Widget.Gtk_Widget;
   --  Returns the widget to which the menu was attached.
   --  If the menu was not attached, this function returns null




   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);
   --  Gate internal function

   procedure Generate (Menu : in out Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function


private
   type Gtk_Menu_Record is new Gtk.Menu_Shell.Gtk_Menu_Shell_Record
     with null record;

end Gtk.Menu;

--  <example>
--  <include>../examples/documentation/contextual.adb</include>
--  </example>
