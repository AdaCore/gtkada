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
--  Gtk.Search_Bar.Gtk_Search_Bar is a container made to have a search entry
--  (possibly with additional connex widgets, such as drop-down menus, or
--  buttons) built-in. The search bar would appear when a search is started
--  through typing on the keyboard, or the application's search mode is toggled
--  on.
--
--  For keyboard presses to start a search, events will need to be forwarded
--  from the top-level window that contains the search bar. See
--  Gtk.Search_Bar.Handle_Event for example code. Common shortcuts such as
--  Ctrl+F should be handled as an application action, or through the menu
--  items.
--
--  You will also need to tell the search bar about which entry you are using
--  as your search entry using Gtk.Search_Bar.Connect_Entry. The following
--  example shows you how to create a more complex search entry.
--
--  # CSS nodes
--
--  GtkSearchBar has a single CSS node with name searchbar.
--
--  ## Creating a search bar
--
--  [A simple
--  example](https://gitlab.gnome.org/GNOME/gtk/blob/gtk-3-24/examples/search-bar.c)
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;       use Gdk.Event;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.GEntry;      use Gtk.GEntry;

package Gtk.Search_Bar is

   type Gtk_Search_Bar_Record is new Gtk_Bin_Record with null record;
   type Gtk_Search_Bar is access all Gtk_Search_Bar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Search_Bar);
   procedure Initialize (Self : not null access Gtk_Search_Bar_Record'Class);
   --  Creates a Gtk.Search_Bar.Gtk_Search_Bar. You will need to tell it about
   --  which widget is going to be your text entry using
   --  Gtk.Search_Bar.Connect_Entry.
   --  Since: gtk+ 3.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Search_Bar_New return Gtk_Search_Bar;
   --  Creates a Gtk.Search_Bar.Gtk_Search_Bar. You will need to tell it about
   --  which widget is going to be your text entry using
   --  Gtk.Search_Bar.Connect_Entry.
   --  Since: gtk+ 3.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_search_bar_get_type");

   -------------
   -- Methods --
   -------------

   procedure Connect_Entry
      (Self   : not null access Gtk_Search_Bar_Record;
       GEntry : not null access Gtk.GEntry.Gtk_Entry_Record'Class);
   --  Connects the Gtk.GEntry.Gtk_Entry widget passed as the one to be used
   --  in this search bar. The entry should be a descendant of the search bar.
   --  This is only required if the entry isn't the direct child of the search
   --  bar (as in our main example).
   --  Since: gtk+ 3.10
   --  "entry": a Gtk.GEntry.Gtk_Entry

   function Get_Search_Mode
      (Self : not null access Gtk_Search_Bar_Record) return Boolean;
   --  Returns whether the search mode is on or off.
   --  Since: gtk+ 3.10

   procedure Set_Search_Mode
      (Self        : not null access Gtk_Search_Bar_Record;
       Search_Mode : Boolean);
   --  Switches the search mode on or off.
   --  Since: gtk+ 3.10
   --  "search_mode": the new state of the search mode

   function Get_Show_Close_Button
      (Self : not null access Gtk_Search_Bar_Record) return Boolean;
   --  Returns whether the close button is shown.
   --  Since: gtk+ 3.10

   procedure Set_Show_Close_Button
      (Self    : not null access Gtk_Search_Bar_Record;
       Visible : Boolean);
   --  Shows or hides the close button. Applications that already have a
   --  "search" toggle button should not show a close button in their search
   --  bar, as it duplicates the role of the toggle button.
   --  Since: gtk+ 3.10
   --  "visible": whether the close button will be shown or not

   function Handle_Event
      (Self  : not null access Gtk_Search_Bar_Record;
       Event : Gdk.Event.Gdk_Event) return Boolean;
   --  This function should be called when the top-level window which contains
   --  the search bar received a key event.
   --  If the key event is handled by the search bar, the bar will be shown,
   --  the entry populated with the entered text and GDK_EVENT_STOP will be
   --  returned. The caller should ensure that events are not propagated
   --  further.
   --  If no entry has been connected to the search bar, using
   --  Gtk.Search_Bar.Connect_Entry, this function will return immediately with
   --  a warning.
   --  ## Showing the search bar on key presses
   --  |[<!-- language="C" --> static gboolean on_key_press_event (GtkWidget
   --  *widget, GdkEvent *event, gpointer user_data) { GtkSearchBar *bar =
   --  GTK_SEARCH_BAR (user_data); return gtk_search_bar_handle_event (bar,
   --  event); }
   --  static void create_toplevel (void) { GtkWidget *window = gtk_window_new
   --  (GTK_WINDOW_TOPLEVEL); GtkWindow *search_bar = gtk_search_bar_new ();
   --  // Add more widgets to the window...
   --  g_signal_connect (window, "key-press-event", G_CALLBACK
   --  (on_key_press_event), search_bar); } ]|
   --  Since: gtk+ 3.10
   --  "event": a Gdk.Event.Gdk_Event containing key press events

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Search_Mode_Enabled_Property : constant Glib.Properties.Property_Boolean;

   Show_Close_Button_Property : constant Glib.Properties.Property_Boolean;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Search_Bar_Record, Gtk_Search_Bar);
   function "+"
     (Widget : access Gtk_Search_Bar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Search_Bar
   renames Implements_Gtk_Buildable.To_Object;

private
   Show_Close_Button_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-close-button");
   Search_Mode_Enabled_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("search-mode-enabled");
end Gtk.Search_Bar;
