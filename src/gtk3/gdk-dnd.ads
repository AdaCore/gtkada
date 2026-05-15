------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

--  <group>Gdk, the low-level API</group>
with Glib; use Glib;
with Gdk.Drag_Contexts; use Gdk.Drag_Contexts;
with Gdk.Types; use Gdk.Types;
with Gdk.Screen;
with Gtk;
with Gtk.Target_List;

package Gdk.Dnd is

   type Gdk_Atom_Array is array (Natural range <>) of Gdk.Types.Gdk_Atom;
   function Get_Targets (Context : Drag_Context) return Gdk_Atom_Array;
   --  Return the list of targets supported by this context.

   subtype Drag_Action is Gdk.Drag_Contexts.Gdk_Drag_Action;
   subtype Drag_Protocol is Gdk.Drag_Contexts.Gdk_Drag_Protocol;

   Action_Any     : constant Drag_Action := 2 ** 8 - 1;

   procedure Drag_Status
     (Context : Drag_Context;
      Action  : Drag_Action;
      Time    : Guint32);

   procedure Drop_Reply
     (Context : Drag_Context;
      Ok      : Boolean;
      Time    : Guint32);

   procedure Drop_Finish
     (Context : Drag_Context;
      Success : Boolean;
      Time    : Guint32);
   --  Clean up from the drag, and display snapback, if necessary.

   function Drag_Get_Selection (Context : Drag_Context) return Gdk_Atom;

   function Drag_Begin
     (Window  : Gdk.Gdk_Window;
      Targets : Gtk.Target_List.Gtk_Target_List) return Drag_Context;

   function Drag_Context_Get_Protocol
     (Context  : Drag_Context;
      Protocol : Drag_Protocol) return Drag_Protocol;
   --  Return which drag protocol is recognized by a given context.

   procedure Drag_Find_Window_For_Screen
     (Context     : Drag_Context;
      Drag_Window : Gdk.Gdk_Window;
      Screen      : Gdk.Screen.Gdk_Screen;
      X_Root      : Gint;
      Y_Root      : Gint;
      Dest_Window : Gdk.Gdk_Window;
      Protocol    : Drag_Protocol);

   function Drag_Motion
     (Context          : Drag_Context;
      Dest_Window      : Gdk.Gdk_Window;
      Protocol         : Drag_Protocol;
      X_Root           : Gint;
      Y_Root           : Gint;
      Suggested_Action : Drag_Action;
      Possible_Actions : Drag_Action;
      Time             : Guint32) return Boolean;

   procedure Drag_Drop (Context : Drag_Context; Time : Guint32);

   procedure Drag_Abort (Context : Drag_Context; Time : Guint32);

end Gdk.Dnd;
