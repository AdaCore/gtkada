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
--  The Gtk_Handle_Box widget allows a portion of a window to be "torn off".
--  It is a bin widget which displays its child and a handle that the user can
--  drag to tear off a separate window (the float window) containing the child
--  widget. A thin ghost is drawn in the original location of the handlebox. By
--  dragging the separate window back to its original location, it can be
--  reattached.
--
--  When reattaching, the ghost and float window, must be aligned along one of
--  the edges, the snap edge. This either can be specified by the application
--  programmer explicitely, or GtkAda will pick a reasonable default based on
--  the handle position.
--
--  To make detaching and reattaching the handlebox as minimally confusing as
--  possible to the user, it is important to set the snap edge so that the snap
--  edge does not move when the handlebox is detached. For instance, if the
--  handlebox is packed at the bottom of a Vbox, then when the handlebox is
--  detached, the bottom edge of the handlebox's allocation will remain fixed
--  as the height of the handlebox shrinks, so the snap edge should be set to
--  Pos_Bottom.
--  </description>
--  <c_version>1.3.4</c_version>

with Gtk.Bin;
with Gtk.Enums;

package Gtk.Handle_Box is

   type Gtk_Handle_Box_Record is new Gtk.Bin.Gtk_Bin_Record with private;
   type Gtk_Handle_Box is access all Gtk_Handle_Box_Record'Class;

   procedure Gtk_New (Handle_Box : out Gtk_Handle_Box);
   --  Create a new Handle_Box.

   procedure Initialize (Handle_Box : access Gtk_Handle_Box_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Dialog.

   procedure Set_Shadow_Type
     (Handle_Box : access Gtk_Handle_Box_Record;
      Typ        : Enums.Gtk_Shadow_Type);
   --  Set the type of shadow to be drawn around the border of the Handle_Box.

   procedure Set_Handle_Position
     (Handle_Box : access  Gtk_Handle_Box_Record;
      Position   : Enums.Gtk_Position_Type);
   --  Set the side of the Handle_Box where the handle is drawn.

   procedure Set_Snap_Edge
     (Handle_Box : access  Gtk_Handle_Box_Record;
      Edge       : Enums.Gtk_Position_Type);
   --  Set the snap edge of a Handle_Box.
   --  The snap edge is the edge of the detached child that must be aligned
   --  with the corresponding edge of the "ghost" left behind when the child
   --  was detached to reattach the torn-off window. Usually, the snap edge
   --  should be chosen so that it stays in the same place on the screen when
   --  the Handle_Box is torn off.
   --
   --  If the snap edge is not set, then an appropriate value will be guessed
   --  from the handle position. If the handle position is Pos_Right or
   --  Pos_Left, then the snap edge will be Pos_Top, otherwise it will be
   --  Pos_Left.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Shadow_Property
   --    Type:  Gtk_Shadow_Type
   --    Flags: read-write
   --    Descr: Appearance of the shadow that surrounds the container.
   --    See also: Set_Shadow_Type
   --
   --  - Name:  Handle_Position_Property
   --    Type:  Gtk_Position_Type
   --    Flags: read-write
   --    Descr: Position of the handle relative to the child widget.
   --    See also: Set_Handle_Position
   --
   --  - Name:  Snap_Edge_Property
   --    Type:  Gtk_Position_Type
   --    Flags: read-write
   --    Descr: Side of the handlebox that's lined up with the docking point
   --           to dock the handlebox.
   --    See also: Set_Snap_Edge
   --
   --  </properties>

   Shadow_Property          : constant Gtk.Enums.Property_Gtk_Shadow_Type;
   Handle_Position_Property : constant Gtk.Enums.Property_Gtk_Position_Type;
   Snap_Edge_Property       : constant Gtk.Enums.Property_Gtk_Position_Type;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --   - "child-attached"
   --     procedure Handler
   --       (Handle_Box : access Gtk_Handle_Box_Record'Class;
   --        Widget     : access Gtk_Widget_Record'Class);
   --
   --  Emitted when the contents of the Handle_Box are reattached to the main
   --  window.
   --  Widget is the child widget of the Handle_Box. (this argument provides no
   --  extra information and is here only for backwards-compatibility)
   --
   --   - "child-detached"
   --     procedure Handler
   --       (Handle_Box : access Gtk_Handle_Box_Record'Class;
   --        Widget     : access Gtk_Widget_Record'Class);
   --
   --  Emitted when the contents of the Handle_Box are detached from the main
   --  window. See "child-attached" for drtails on the parameters.
   --  </signals>

private
   type Gtk_Handle_Box_Record is new Gtk.Bin.Gtk_Bin_Record with null record;

   Shadow_Property          : constant Gtk.Enums.Property_Gtk_Shadow_Type :=
     Gtk.Enums.Build ("shadow");
   Handle_Position_Property : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("handle_position");
   Snap_Edge_Property       : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("snap_edge");

   pragma Import (C, Get_Type, "gtk_handle_box_get_type");
end Gtk.Handle_Box;
