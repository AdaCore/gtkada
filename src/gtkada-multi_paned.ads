-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2003 ACT-Europe                     --
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

with Glib;
with Gdk.GC;
with Gtk.Enums;
with Gtk.Fixed;
with Gtk.Widget;
with Gdk.Window;

--  ??? Do we want opaque resizing


--  This widget implements a multi-paned widget, similar to the standard
--  Gtk_Paned widget, but which can contain several children side to side.
--
--  This widget can mix vertical and horizontal splits

package Gtkada.Multi_Paned is
   type Gtkada_Multi_Paned_Record is new Gtk.Fixed.Gtk_Fixed_Record
     with private;
   type Gtkada_Multi_Paned is access all Gtkada_Multi_Paned_Record'Class;

   procedure Gtk_New (Win : out Gtkada_Multi_Paned);
   procedure Initialize (Win : access Gtkada_Multi_Paned_Record'Class);
   --  Create a new paned window.

   procedure Add_Child
     (Win        : access Gtkada_Multi_Paned_Record;
      New_Child  : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Add new child, splitting as needed (in the direction of the main pane).
   --  This should be used when there is no child yet

   procedure Split
     (Win         : access Gtkada_Multi_Paned_Record;
      Ref_Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      New_Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation : Gtk.Enums.Gtk_Orientation;
      Child_Percent : Float := 0.5;
      After       : Boolean := True);
   --  Split the pane containing Ref_Widget, and add New_Child
   --  in the new pane (on the right or at the bottom if After is True, on the
   --  left or at the top if After is False).
   --  Child_Percent indicates what proportion of the area currently occupied
   --  by Ref_Widget will now be occupied by New_Child. The area occupied by
   --  Ref_Child is reduced accordingly.

private
   type Resize_Handle is record
      Position : Gtk.Widget.Gtk_Allocation;
      Win      : Gdk.Window.Gdk_Window;
      Percent  : Float;
   end record;

   type Handles_Array is array (Natural range <>) of Resize_Handle;
   type Handles_Array_Access is access Handles_Array;

   type Child_Description;
   type Child_Description_Access is access Child_Description;
   type Child_Description (Is_Widget : Boolean) is record
      Parent : Child_Description_Access;
      Next   : Child_Description_Access;
      case Is_Widget is
         when True  =>
            Widget      : Gtk.Widget.Gtk_Widget;
         when False =>
            Orientation : Gtk.Enums.Gtk_Orientation;
            First_Child : Child_Description_Access;
            Handles     : Handles_Array_Access;
      end case;
   end record;

   type Gtkada_Multi_Paned_Record is new Gtk.Fixed.Gtk_Fixed_Record with record
      Children    : Child_Description_Access;
      GC          : Gdk.GC.Gdk_GC;

      Selected_Handle_Parent : Child_Description_Access;
      Selected_Handle_Index  : Natural;
      Selected_Handle_Pos    : Gtk.Widget.Gtk_Allocation;
      Anim_Offset            : Glib.Allocation_Int;
      --  Temporary variables, used while resizing windows

   end record;

end Gtkada.Multi_Paned;
