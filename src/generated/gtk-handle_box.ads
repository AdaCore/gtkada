------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
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

pragma Ada_05;
--  <description>
--  The Gtk.Handle_Box.Gtk_Handle_Box widget allows a portion of a window to
--  be "torn off". It is a bin widget which displays its child and a handle
--  that the user can drag to tear off a separate window (the 'float window')
--  containing the child widget. A thin 'ghost' is drawn in the original
--  location of the handlebox. By dragging the separate window back to its
--  original location, it can be reattached.
--
--  When reattaching, the ghost and float window, must be aligned along one of
--  the edges, the 'snap edge'. This either can be specified by the application
--  programmer explicitely, or GTK+ will pick a reasonable default based on the
--  handle position.
--
--  To make detaching and reattaching the handlebox as minimally confusing as
--  possible to the user, it is important to set the snap edge so that the snap
--  edge does not move when the handlebox is deattached. For instance, if the
--  handlebox is packed at the bottom of a VBox, then when the handlebox is
--  detached, the bottom edge of the handlebox's allocation will remain fixed
--  as the height of the handlebox shrinks, so the snap edge should be set to
--  Gtk.Enums.Pos_Bottom.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Handle_Box is

   type Gtk_Handle_Box_Record is new Gtk_Bin_Record with null record;
   type Gtk_Handle_Box is access all Gtk_Handle_Box_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Handle_Box : out Gtk_Handle_Box);
   procedure Initialize (Handle_Box : access Gtk_Handle_Box_Record'Class);
   --  Create a new handle box.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_handle_box_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child_Detached
      (Handle_Box : not null access Gtk_Handle_Box_Record) return Boolean;
   --  Whether the handlebox's child is currently detached.
   --  Since: gtk+ 2.14

   function Get_Handle_Position
      (Handle_Box : not null access Gtk_Handle_Box_Record)
       return Gtk.Enums.Gtk_Position_Type;
   procedure Set_Handle_Position
      (Handle_Box : not null access Gtk_Handle_Box_Record;
       Position   : Gtk.Enums.Gtk_Position_Type);
   --  Sets the side of the handlebox where the handle is drawn.
   --  "position": the side of the handlebox where the handle should be drawn.

   function Get_Shadow_Type
      (Handle_Box : not null access Gtk_Handle_Box_Record)
       return Gtk.Enums.Gtk_Shadow_Type;
   procedure Set_Shadow_Type
      (Handle_Box : not null access Gtk_Handle_Box_Record;
       The_Type   : Gtk.Enums.Gtk_Shadow_Type);
   --  Sets the type of shadow to be drawn around the border of the handle
   --  box.
   --  "type": the shadow type.

   function Get_Snap_Edge
      (Handle_Box : not null access Gtk_Handle_Box_Record)
       return Gtk.Enums.Gtk_Position_Type;
   procedure Set_Snap_Edge
      (Handle_Box : not null access Gtk_Handle_Box_Record;
       Edge       : Gtk.Enums.Gtk_Position_Type);
   --  Sets the snap edge of a handlebox. The snap edge is the edge of the
   --  detached child that must be aligned with the corresponding edge of the
   --  "ghost" left behind when the child was detached to reattach the torn-off
   --  window. Usually, the snap edge should be chosen so that it stays in the
   --  same place on the screen when the handlebox is torn off.
   --  If the snap edge is not set, then an appropriate value will be guessed
   --  from the handle position. If the handle position is Gtk.Enums.Pos_Right
   --  or Gtk.Enums.Pos_Left, then the snap edge will be Gtk.Enums.Pos_Top,
   --  otherwise it will be Gtk.Enums.Pos_Left.
   --  "edge": the snap edge, or -1 to unset the value; in which case GTK+
   --  will try to guess an appropriate value in the future.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Handle_Box_Record, Gtk_Handle_Box);
   function "+"
     (Widget : access Gtk_Handle_Box_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Handle_Box
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Child_Detached_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Handle_Position_Property
   --  Type: Gtk.Enums.Gtk_Position_Type
   --  Flags: read-write
   --
   --  Name: Shadow_Type_Property
   --  Type: Gtk.Enums.Gtk_Shadow_Type
   --  Flags: read-write
   --
   --  Name: Snap_Edge_Property
   --  Type: Gtk.Enums.Gtk_Position_Type
   --  Flags: read-write
   --
   --  Name: Snap_Edge_Set_Property
   --  Type: Boolean
   --  Flags: read-write

   Child_Detached_Property : constant Glib.Properties.Property_Boolean;
   Handle_Position_Property : constant Gtk.Enums.Property_Gtk_Position_Type;
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type;
   Snap_Edge_Property : constant Gtk.Enums.Property_Gtk_Position_Type;
   Snap_Edge_Set_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "child-attached"
   --     procedure Handler
   --       (Self   : access Gtk_Handle_Box_Record'Class;
   --        Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --    --  "widget": the child widget of the handlebox. (this argument provides no
   --    --  extra information and is here only for backwards-compatibility)
   --  This signal is emitted when the contents of the handlebox are
   --  reattached to the main window.
   --
   --  "child-detached"
   --     procedure Handler
   --       (Self   : access Gtk_Handle_Box_Record'Class;
   --        Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --    --  "widget": the child widget of the handlebox. (this argument provides no
   --    --  extra information and is here only for backwards-compatibility)
   --  This signal is emitted when the contents of the handlebox are detached
   --  from the main window.

   Signal_Child_Attached : constant Glib.Signal_Name := "child-attached";
   Signal_Child_Detached : constant Glib.Signal_Name := "child-detached";

private
   Child_Detached_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("child-detached");
   Handle_Position_Property : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("handle-position");
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type :=
     Gtk.Enums.Build ("shadow-type");
   Snap_Edge_Property : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("snap-edge");
   Snap_Edge_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("snap-edge-set");
end Gtk.Handle_Box;
