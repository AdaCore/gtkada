------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  A GtkShortcutsWindow shows brief information about the keyboard shortcuts
--  and gestures of an application. The shortcuts can be grouped, and you can
--  have multiple sections in this window, corresponding to the major modes of
--  your application.
--
--  Additionally, the shortcuts can be filtered by the current view, to avoid
--  showing information that is not relevant in the current application
--  context.
--
--  The recommended way to construct a GtkShortcutsWindow is with GtkBuilder,
--  by populating a Gtk.Shortcuts_Window.Gtk_Shortcuts_Window with one or more
--  Gtk.Shortcuts_Section.Gtk_Shortcuts_Section objects, which contain
--  Gtk_Shortcuts_Groups that in turn contain objects of class
--  Gtk.Shortcuts_Shortcut.Gtk_Shortcuts_Shortcut.
--
--  # A simple example:
--
--  ![](gedit-shortcuts.png)
--
--  This example has as single section. As you can see, the shortcut groups
--  are arranged in columns, and spread across several pages if there are too
--  many to find on a single page.
--
--  The .ui file for this example can be found
--  [here](https://git.gnome.org/browse/gtk+/tree/demos/gtk-demo/shortcuts-gedit.ui).
--
--  # An example with multiple views:
--
--  ![](clocks-shortcuts.png)
--
--  This example shows a Gtk.Shortcuts_Window.Gtk_Shortcuts_Window that has
--  been configured to show only the shortcuts relevant to the "stopwatch"
--  view.
--
--  The .ui file for this example can be found
--  [here](https://git.gnome.org/browse/gtk+/tree/demos/gtk-demo/shortcuts-clocks.ui).
--
--  # An example with multiple sections:
--
--  ![](builder-shortcuts.png)
--
--  This example shows a Gtk.Shortcuts_Window.Gtk_Shortcuts_Window with two
--  sections, "Editor Shortcuts" and "Terminal Shortcuts".
--
--  The .ui file for this example can be found
--  [here](https://git.gnome.org/browse/gtk+/tree/demos/gtk-demo/shortcuts-builder.ui).
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Window;      use Gtk.Window;

package Gtk.Shortcuts_Window is

   type Gtk_Shortcuts_Window_Record is new Gtk_Window_Record with null record;
   type Gtk_Shortcuts_Window is access all Gtk_Shortcuts_Window_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_shortcuts_window_get_type");

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Section_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the section to show.
   --
   --  This should be the section-name of one of the
   --  Gtk.Shortcuts_Section.Gtk_Shortcuts_Section objects that are in this
   --  shortcuts window.

   View_Name_Property : constant Glib.Properties.Property_String;
   --  The view name by which to filter the contents.
   --
   --  This should correspond to the
   --  Gtk.Shortcuts_Group.Gtk_Shortcuts_Group:view property of some of the
   --  Gtk.Shortcuts_Group.Gtk_Shortcuts_Group objects that are inside this
   --  shortcuts window.
   --
   --  Set this to null to show all groups.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Shortcuts_Window_Void is not null access procedure
     (Self : access Gtk_Shortcuts_Window_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Close : constant Glib.Signal_Name := "close";
   procedure On_Close
      (Self  : not null access Gtk_Shortcuts_Window_Record;
       Call  : Cb_Gtk_Shortcuts_Window_Void;
       After : Boolean := False);
   procedure On_Close
      (Self  : not null access Gtk_Shortcuts_Window_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::close signal is a [keybinding signal][GtkBindingSignal] which
   --  gets emitted when the user uses a keybinding to close the window.
   --
   --  The default binding for this signal is the Escape key.

   Signal_Search : constant Glib.Signal_Name := "search";
   procedure On_Search
      (Self  : not null access Gtk_Shortcuts_Window_Record;
       Call  : Cb_Gtk_Shortcuts_Window_Void;
       After : Boolean := False);
   procedure On_Search
      (Self  : not null access Gtk_Shortcuts_Window_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::search signal is a [keybinding signal][GtkBindingSignal] which
   --  gets emitted when the user uses a keybinding to start a search.
   --
   --  The default binding for this signal is Control-F.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Shortcuts_Window_Record, Gtk_Shortcuts_Window);
   function "+"
     (Widget : access Gtk_Shortcuts_Window_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Shortcuts_Window
   renames Implements_Gtk_Buildable.To_Object;

private
   View_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("view-name");
   Section_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("section-name");
end Gtk.Shortcuts_Window;
