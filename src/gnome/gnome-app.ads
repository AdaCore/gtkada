------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Glib; use Glib;
with Gtk.Container;
with Gtk.Toolbar;
with Gtk.Menu_Bar;
with Gtk.Status_Bar;
with Gtk.Widget;
with Gtk.Window;
with Bonobo.Dock;
with Bonobo.Dock_Item;

package Gnome.App is

   type Gnome_App_Record is new Gtk.Window.Gtk_Window_Record with private;
   type Gnome_App is access all Gnome_App_Record'Class;

   procedure Gnome_New
     (App     : out Gnome_App;
      Appname : String;
      Title   : String := "");
   --  Create a new (empty) application window.
   --  You must specify the application's name (used internally as an
   --  identifier). The window title can be left as a null string, in which
   --  case the window's title will not be set.

   procedure Initialize
     (App     : access Gnome_App_Record'Class;
      Appname : String;
      Title   : String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Window.

   procedure Add_Dock_Item
     (App           : access Gnome_App_Record;
      Item          : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class;
      Placement     : Bonobo.Dock.Bonobo_Dock_Placement;
      Band_Num      : Gint;
      Band_Position : Gint;
      Offset        : Gint);
   --  Warning: Bonobo_Dock is described as an immature API.

   procedure Add_Docked
     (App           : access Gnome_App_Record;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Name          : String;
      Behavior      : Bonobo.Dock_Item.Bonobo_Dock_Item_Behavior;
      Placement     : Bonobo.Dock.Bonobo_Dock_Placement;
      Band_Num      : Gint;
      Band_Position : Gint;
      Offset        : Gint);
   --  Warning: Bonobo_Dock is described as an immature API.

   procedure Add_Toolbar
     (App           : access Gnome_App_Record;
      Toolbar       : access Gtk.Toolbar.Gtk_Toolbar_Record'Class;
      Name          : String;
      Behavior      : Bonobo.Dock_Item.Bonobo_Dock_Item_Behavior;
      Placement     : Bonobo.Dock.Bonobo_Dock_Placement;
      Band_Num      : Gint;
      Band_Position : Gint;
      Offset        : Gint);
   --  Warning: Bonobo_Dock is described as an immature API.

   procedure Enable_Layout_Config
     (App    : access Gnome_App_Record;
      Enable : Boolean);

   function Get_Dock
     (App : access Gnome_App_Record) return Bonobo.Dock.Bonobo_Dock;
   --  Warning: Bonobo_Dock is described as an immature API.

   function Get_Dock_Item_By_Name
     (App  : access Gnome_App_Record;
      Name : String) return Bonobo.Dock_Item.Bonobo_Dock_Item;
   --  Warning: Bonobo_Dock is described as an immature API.

   procedure Set_Contents
     (App      : access Gnome_App_Record;
      Contents : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Set the content area of the application window

   procedure Set_Menus
     (App     : access Gnome_App_Record;
      Menubar : access Gtk.Menu_Bar.Gtk_Menu_Bar_Record'Class);
   --  Set the menu bar of the application window

   procedure Set_StatusBar
     (App       : access Gnome_App_Record;
      Statusbar : access Gtk.Status_Bar.Gtk_Status_Bar_Record'Class);
   --  Set the status bar of the application window

   procedure Set_Statusbar_Custom
     (App       : access Gnome_App_Record;
      Container : access Gtk.Container.Gtk_Container_Record'Class;
      Statusbar : access Gtk.Status_Bar.Gtk_Status_Bar_Record'Class);
   --  Set the status bar of the application window, but use the given
   --  container widget rather than creating a new one.

   procedure Set_Toolbar
     (App     : access Gnome_App_Record;
      Toolbar : access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   --  Set the main toolbar of the application window

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_App_Record is new Gtk.Window.Gtk_Window_Record
     with null record;

   pragma Import (C, Get_Type, "gnome_app_get_type");
end Gnome.App;
