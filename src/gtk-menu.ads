-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
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
--
--  This widget implements a drop-down menu.
--  This is basically a simple box that contains a series of Gtk_Menu_Item
--  widgets, on which the user can click to perform actions.
--
--  Such a menu is usually part of a Gtk_Menu_Bar (at the top of the window),
--  or activated by clicking on an item in another Gtk_Menu.
--  @pxref{Package_Gtk.Option_Menu} for another way of displaying menus.
--
--  All the menus in GtkAda can be "Tear off" menus, i.e you can detach
--  them from their parent (either a menu bar or another menu) to keep them
--  visible on the screen at all times).
--
--  It is worth noting that by default, the user of your application will be
--  able to dynamically modify the shortcuts associated with each menu item.
--  For instance, selecting a menu item and pressing a key will assign this
--  new shortcut to the item, possibly removing the shortcut from any other
--  item it was associated with.
--
--  Note that pressing <backspace> will simply remove the shortcut.
--
--  This default behavior, somewhat unexpected, can be canceled.
--  There are two ways to control this behavior: you can lock a specific menu
--  item by calling Gtk.Widget.Lock_Accelerators on it. But you can also
--  lock all the menu items at once by calling Gtk.Accel_Group.Lock for all
--  the accelerator groups that were used (the GUI builder gate generally
--  creates a single one), as well as on the group returned by
--  Gtk.Accel_Group.Get_Default, which is the one used for items that don't
--  initially have a shortcut.
--
--  </description>
--  <c_version>1.3.4</c_version>

with Gtk.Accel_Group;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu_Shell;
with Gtk.Widget;

package Gtk.Menu is

   type Gtk_Menu_Record is new
     Gtk.Menu_Shell.Gtk_Menu_Shell_Record with private;
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
   --  Create a new empty menu.

   procedure Initialize (Widget : access Gtk_Menu_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Menu.

   procedure Set_Active (Menu : access Gtk_Menu_Record; Index : Guint);
   --  Select a specified item in the menu.
   --  You will almost never need this function, it is used internally by
   --  Gtk_Option_Menu.
   --  Note that the item is not considered as being pressed by the user, and
   --  thus no callback is called as a result.

   function Get_Active
     (Menu : access Gtk_Menu_Record) return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Get the active menu item.
   --  In a Gtk_Option_Menu, this is the item that is currently shown in the
   --  button.

   procedure Set_Tearoff_State
     (Menu : access Gtk_Menu_Record; Torn_Off : Boolean);
   --  Modify the tearoff status of the menu.
   --  If Torn_Off is False, the menu is displayed as a drop down menu which
   --  disappears when the menu is not active. If Torn_Off is True, the menu
   --  persists until it is closed or reattached.
   --  Note that you can give the user access to this functionality by
   --  inserting a Gtk_Tearoff_Menu_Item in the menu.

   procedure Set_Title
     (Menu : access Gtk_Menu_Record; Title : String);
   --  Set the title of the menu.
   --  Title is displayed when the menu is displayed as a tearoff menu in an
   --  independent window.

   procedure Reorder_Child
     (Menu     : access Gtk_Menu_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : Gint);
   --  Move an existing menu_item within the menu.
   --  Its new position is given by Position, 0 being the first item in the
   --  menu.
   --  If Child does not exist in the menu, nothing is done.

   -----------------------
   -- Displaying a menu --
   -----------------------

   type Gtk_Menu_Position_Func is access procedure
     (Menu : access Gtk_Menu_Record'Class;
      X    : out Gint;
      Y    : out Gint);
   --  This function is called when displaying a popup menu on the screen.
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
   --  Display a menu on the screen.
   --  This is the function to use to create contextual menus.
   --  Most of the time, the parameters can have a null value.
   --  Parent_Menu_Shell is the Gtk_Menu_Shell that contains Parent_Menu_Item,
   --  i.e. the widget that triggered the display of the menu.
   --  Func is a function that returns the coordinates for the menu. If it is
   --  null, then a default function that positions the menu at the pointer
   --  location is used.
   --  Button is the mouse button that was pressed to initiate the event.
   --  Activate_Time is the time at which the event occurred (you can get it
   --  directly from the Gdk_Event structure).
   --
   --  Note that a variant of this function is given in the generic package
   --  User_Menu_Popup.

   --  Note: in the Popup function, the Parent_* parameters are not access
   --  parameters because they might be null.

   generic
      --  <doc_ignore>
      type Data_Type is private;
      --  </doc_ignore>

   package User_Menu_Popup is
      --  <doc_ignore>
      type Gtk_Menu_Position_Func is access procedure
        (Menu      : access Gtk_Menu_Record;
         X         : out Gint;
         Y         : out Gint;
         User_Data : access Data_Type);
      --  </doc_ignore>

      procedure Popup
        (Menu              : access Gtk_Menu_Record;
         Data              : access Data_Type;
         Parent_Menu_Shell : in Gtk.Menu_Shell.Gtk_Menu_Shell := null;
         Parent_Menu_Item  : in Gtk.Menu_Item.Gtk_Menu_Item := null;
         Func              : in Gtk_Menu_Position_Func := null;
         Button            : in Guint := 1;
         Activate_Time     : in Guint32 := 0);
      --  Same as the Popup function above.
      --  Note that Data is not duplicated, thus you should take care of the
      --  memory allocation/deallocation yourself.

      --  Note also that the order of parameters is slightly different from the
      --  C version.
   end User_Menu_Popup;

   procedure Popdown (Menu : access Gtk_Menu_Record);
   --  Remove the menu from the screen

   procedure Reposition (Menu : access Gtk_Menu_Record);
   --  Reposition a menu according to its position function.
   --  This function is set when Popup is called.

   --------------------------------
   -- Modifying the accelerators --
   --------------------------------

   procedure Set_Accel_Group
     (Menu  : access Gtk_Menu_Record;
      Accel : Accel_Group.Gtk_Accel_Group);
   --  Set the Accel_Group that holds the global accelerators and key bindings
   --  for the menu.

   function Get_Accel_Group
     (Menu : access Gtk_Menu_Record) return Accel_Group.Gtk_Accel_Group;
   --  Get the accelerator group used to set the key bindings in the menu.

   ----------------------------------
   -- Attaching a menu to a widget --
   ----------------------------------

   procedure Attach_To_Widget
     (Menu          : access Gtk_Menu_Record;
      Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Detacher      : Gtk_Menu_Detach_Func);
   --  Attach a menu to the widget.
   --  When the menu is detached from the widget (for instance when it is
   --  destroyed), the procedure Detached will be called.
   --  You will almost never need to use this function, unless you specifically
   --  want a call back when a widget becomes unavailable.
   --  If Attach_Widget is a menu_item with a single label in it, the name of
   --  the window created when Menu is teared-off will be the label in the
   --  menu_item.

   procedure Detach (Menu : access Gtk_Menu_Record);
   --  Detach the menu from its widget, and call the Detacher set in
   --  Attach_To_Widget.

   function Get_Attach_Widget
     (Menu : access Gtk_Menu_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the widget to which the menu was attached.
   --  If the menu was not attached, this function returns null.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gtk_Menu_Record is new Gtk.Menu_Shell.Gtk_Menu_Shell_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_menu_get_type");
end Gtk.Menu;

--  <example>
--  <include>../examples/documentation/contextual.adb</include>
--  </example>
