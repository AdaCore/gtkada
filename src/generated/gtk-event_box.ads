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
--  The Gtk.Event_Box.Gtk_Event_Box widget is a subclass of Gtk.Bin.Gtk_Bin
--  which also has its own window. It is useful since it allows you to catch
--  events for widgets which do not have their own window.
--
--  </description>
--  <group>Layout Containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;

package Gtk.Event_Box is

   type Gtk_Event_Box_Record is new Gtk_Bin_Record with null record;
   type Gtk_Event_Box is access all Gtk_Event_Box_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Event_Box : out Gtk_Event_Box);
   procedure Initialize
      (Event_Box : not null access Gtk_Event_Box_Record'Class);
   --  Create a new box.
   --  The box's child can then be set using the Gtk.Container.Add function.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Event_Box_New return Gtk_Event_Box;
   --  Create a new box.
   --  The box's child can then be set using the Gtk.Container.Add function.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_event_box_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Above_Child
      (Event_Box : not null access Gtk_Event_Box_Record) return Boolean;
   --  Returns whether the event box window is above or below the windows of
   --  its child. See Gtk.Event_Box.Set_Above_Child for details.
   --  Since: gtk+ 2.4

   procedure Set_Above_Child
      (Event_Box   : not null access Gtk_Event_Box_Record;
       Above_Child : Boolean);
   --  Set whether the event box window is positioned above the windows of its
   --  child, as opposed to below it. If the window is above, all events inside
   --  the event box will go to the event box. If the window is below, events
   --  in windows of child widgets will first got to that widget, and then to
   --  its parents.
   --  The default is to keep the window below the child.
   --  Since: gtk+ 2.4
   --  "above_child": True if the event box window is above its child

   function Get_Visible_Window
      (Event_Box : not null access Gtk_Event_Box_Record) return Boolean;
   --  Returns whether the event box has a visible window. See
   --  Gtk.Event_Box.Set_Visible_Window for details.
   --  Since: gtk+ 2.4

   procedure Set_Visible_Window
      (Event_Box      : not null access Gtk_Event_Box_Record;
       Visible_Window : Boolean);
   --  Set whether the event box uses a visible or invisible child window. The
   --  default is to use visible windows.
   --  In an invisible window event box, the window that the event box creates
   --  is a Gdk.Input_Only window, which means that it is invisible and only
   --  serves to receive events.
   --  A visible window event box creates a visible (Gdk.Input_Output) window
   --  that acts as the parent window for all the widgets contained in the
   --  event box.
   --  You should generally make your event box invisible if you just want to
   --  trap events. Creating a visible window may cause artifacts that are
   --  visible to the user, especially if the user is using a theme with
   --  gradients or pixmaps.
   --  The main reason to create a non input-only event box is if you want to
   --  set the background to a different color or draw on it.
   --  There is one unexpected issue for an invisible event box that has its
   --  window below the child. (See Gtk.Event_Box.Set_Above_Child.) Since the
   --  input-only window is not an ancestor window of any windows that
   --  descendent widgets of the event box create, events on these windows
   --  aren't propagated up by the windowing system, but only by GTK+. The
   --  practical effect of this is if an event isn't in the event mask for the
   --  descendant window (see Gtk.Widget.Add_Events), it won't be received by
   --  the event box.
   --  This problem doesn't occur for visible event boxes, because in that
   --  case, the event box window is actually the ancestor of the descendant
   --  windows, not just at the same place on the screen.
   --  Since: gtk+ 2.4
   --  "visible_window": True to make the event box have a visible window

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Above_Child_Property : constant Glib.Properties.Property_Boolean;

   Visible_Window_Property : constant Glib.Properties.Property_Boolean;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Event_Box_Record, Gtk_Event_Box);
   function "+"
     (Widget : access Gtk_Event_Box_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Event_Box
   renames Implements_Gtk_Buildable.To_Object;

private
   Visible_Window_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-window");
   Above_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("above-child");
end Gtk.Event_Box;
