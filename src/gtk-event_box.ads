-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  This widget is a container that catches events for its child when its
--  child does not have its own window (like a Gtk_Scrolled_Window or a
--  Gtk_Label for instance).
--  Some widgets in GtkAda do not have their own window, and thus can not
--  directly get events from the server. The Gtk_Event_Box widget can be
--  used to force its child to receive events anyway.
--
--  For instance, this widget is used internally in a Gtk_Combo_Box so that
--  the application can change the cursor when the mouse is in the popup
--  window. In that case, it contains a frame, that itself contains the
--  scrolled window of the popup.
--
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Layout containers</group>

with Glib.Properties;
with Gtk.Bin;

package Gtk.Event_Box is

   type Gtk_Event_Box_Record is new Gtk.Bin.Gtk_Bin_Record with private;
   type Gtk_Event_Box is access all Gtk_Event_Box_Record'Class;

   procedure Gtk_New (Event_Box : out Gtk_Event_Box);
   --  Create a new box.
   --  The box's child can then be set using the Gtk.Container.Add function.

   procedure Initialize (Event_Box : access Gtk_Event_Box_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Event_Box.

   procedure Set_Visible_Window
      (Event_Box : access Gtk_Event_Box_Record;
       Visible_Window : Boolean);
   function Get_Visible_Window
      (Event_Box : access Gtk_Event_Box_Record) return Boolean;
   --  Set whether the event box uses a visible or invisible child window. The
   --  default is to use visible windows
   --  Except if you want to explicitly change the background, or explicitly
   --  draw on it, you should make the event box invisible.

   procedure Set_Above_Child
      (Event_Box : access Gtk_Event_Box_Record;
       Above_Child : Boolean);
   function Get_Above_Child
      (Event_Box : access Gtk_Event_Box_Record) return Boolean;
   --  Set whether the event box window is positioned above the windows of its
   --  child, as opposed to below it. If the window is above, all events inside
   --  the event box will go to the event box. If the window is below, events
   --  in windows of child widgets will first go to that widget, and then to
   --  its parent. The default is to keep the window below the child.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Above_Child_Property
   --  Type:  Boolean
   --  Descr: Whether the event-trapping window of the eventbox is above the
   --         window of the child widget as opposed to below it.
   --
   --  Name:  Visible_Window_Property
   --  Type:  Boolean
   --  Descr: Whether the event box is visible, as opposed to invisible and
   --         only used to trap events.
   --
   --  </properties>

   Above_Child_Property    : constant Glib.Properties.Property_Boolean;
   Visible_Window_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Event_Box_Record is new Gtk.Bin.Gtk_Bin_Record with null record;

   Above_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("above-child");
   Visible_Window_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-window");

   pragma Import (C, Get_Type, "gtk_event_box_get_type");
end Gtk.Event_Box;
