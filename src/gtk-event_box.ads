-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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
--  <c_version>1.2.6</c_version>

with Gtk.Object;
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

   ----------------------------
   -- Support for GATE/DGATE --
   ----------------------------

   procedure Generate (N : in Node_Ptr; File : in File_Type);
   --  Gate internal function

   procedure Generate (Event_Box : in out Object.Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Event_Box_Record is new Gtk.Bin.Gtk_Bin_Record with null record;
   pragma Import (C, Get_Type, "gtk_event_box_get_type");
end Gtk.Event_Box;
