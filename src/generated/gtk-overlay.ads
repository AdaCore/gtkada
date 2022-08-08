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
--  GtkOverlay is a container which contains a single main child, on top of
--  which it can place "overlay" widgets. The position of each overlay widget
--  is determined by its Gtk.Widget.Gtk_Widget:halign and
--  Gtk.Widget.Gtk_Widget:valign properties. E.g. a widget with both alignments
--  set to Gtk.Widget.Align_Start will be placed at the top left corner of the
--  GtkOverlay container, whereas an overlay with halign set to
--  Gtk.Widget.Align_Center and valign set to Gtk.Widget.Align_End will be
--  placed a the bottom edge of the GtkOverlay, horizontally centered. The
--  position can be adjusted by setting the margin properties of the child to
--  non-zero values.
--
--  More complicated placement of overlays is possible by connecting to the
--  Gtk.Overlay.Gtk_Overlay::get-child-position signal.
--
--  An overlay's minimum and natural sizes are those of its main child. The
--  sizes of overlay children are not considered when measuring these preferred
--  sizes.
--
--  # GtkOverlay as GtkBuildable
--
--  The GtkOverlay implementation of the GtkBuildable interface supports
--  placing a child as an overlay by specifying "overlay" as the "type"
--  attribute of a `<child>` element.
--
--  # CSS nodes
--
--  GtkOverlay has a single CSS node with the name "overlay". Overlay children
--  whose alignments cause them to be positioned at an edge get the style
--  classes ".left", ".right", ".top", and/or ".bottom" according to their
--  position.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Rectangle; use Gdk.Rectangle;
with Glib;          use Glib;
with Glib.Object;   use Glib.Object;
with Glib.Types;    use Glib.Types;
with Gtk.Bin;       use Gtk.Bin;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Overlay is

   type Gtk_Overlay_Record is new Gtk_Bin_Record with null record;
   type Gtk_Overlay is access all Gtk_Overlay_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Overlay);
   procedure Initialize (Self : not null access Gtk_Overlay_Record'Class);
   --  Creates a new Gtk.Overlay.Gtk_Overlay.
   --  Since: gtk+ 3.2
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Overlay_New return Gtk_Overlay;
   --  Creates a new Gtk.Overlay.Gtk_Overlay.
   --  Since: gtk+ 3.2

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_overlay_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Overlay
      (Self   : not null access Gtk_Overlay_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds Widget to Overlay.
   --  The widget will be stacked on top of the main widget added with
   --  Gtk.Container.Add.
   --  The position at which Widget is placed is determined from its
   --  Gtk.Widget.Gtk_Widget:halign and Gtk.Widget.Gtk_Widget:valign
   --  properties.
   --  Since: gtk+ 3.2
   --  "widget": a Gtk.Widget.Gtk_Widget to be added to the container

   function Get_Overlay_Pass_Through
      (Self   : not null access Gtk_Overlay_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Convenience function to get the value of the
   --  Gtk.Overlay.Gtk_Overlay:pass-through child property for Widget.
   --  Since: gtk+ 3.18
   --  "widget": an overlay child of Gtk.Overlay.Gtk_Overlay

   procedure Set_Overlay_Pass_Through
      (Self         : not null access Gtk_Overlay_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Pass_Through : Boolean);
   --  Convenience function to set the value of the
   --  Gtk.Overlay.Gtk_Overlay:pass-through child property for Widget.
   --  Since: gtk+ 3.18
   --  "widget": an overlay child of Gtk.Overlay.Gtk_Overlay
   --  "pass_through": whether the child should pass the input through

   procedure Reorder_Overlay
      (Self  : not null access Gtk_Overlay_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Index : Glib.Gint);
   --  Moves Child to a new Index in the list of Overlay children. The list
   --  contains overlays in the order that these were added to Overlay by
   --  default. See also Gtk.Overlay.Gtk_Overlay:index.
   --  A widget's index in the Overlay children list determines which order
   --  the children are drawn if they overlap. The first child is drawn at the
   --  bottom. It also affects the default focus chain order.
   --  Since: gtk+ 3.18
   --  "child": the overlaid Gtk.Widget.Gtk_Widget to move
   --  "index_": the new index for Child in the list of overlay children of
   --  Overlay, starting from 0. If negative, indicates the end of the list

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean is not null access function
     (Self       : access Gtk_Overlay_Record'Class;
      Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Allocation : access Gdk.Rectangle.Gdk_Rectangle)
   return Boolean;

   type Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean is not null access function
     (Self       : access Glib.Object.GObject_Record'Class;
      Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Allocation : access Gdk.Rectangle.Gdk_Rectangle)
   return Boolean;

   Signal_Get_Child_Position : constant Glib.Signal_Name := "get-child-position";
   procedure On_Get_Child_Position
      (Self  : not null access Gtk_Overlay_Record;
       Call  : Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean;
       After : Boolean := False);
   procedure On_Get_Child_Position
      (Self  : not null access Gtk_Overlay_Record;
       Call  : Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::get-child-position signal is emitted to determine the position
   --  and size of any overlay child widgets. A handler for this signal should
   --  fill Allocation with the desired position and size for Widget, relative
   --  to the 'main' child of Overlay.
   --
   --  The default handler for this signal uses the Widget's halign and valign
   --  properties to determine the position and gives the widget its natural
   --  size (except that an alignment of Gtk.Widget.Align_Fill will cause the
   --  overlay to be full-width/height). If the main child is a
   --  Gtk.Scrolled_Window.Gtk_Scrolled_Window, the overlays are placed
   --  relative to its contents.
   -- 
   --  Callback parameters:
   --    --  "widget": the child widget to position
   --    --  "allocation": return location for the allocation
   --    --  Returns True if the Allocation has been filled

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Overlay_Record, Gtk_Overlay);
   function "+"
     (Widget : access Gtk_Overlay_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Overlay
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Overlay;
