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
--  A Gtk_Layout is a widget that can have an almost infinite size, without
--  occupying a lot of memory. Its children can be located anywhere within
--  it, but will only appear on the screen if the visible area of the layout
--  contains them.
--  Just like a Gtk_Viewport, its visible area is indicated by two
--  Gtk_Adjustment widgets, and thus a Gtk_Layout can be put as is in a
--  Gtk_Scrolled_Window.
--  In fact, due to a bug in gtk+1.2.6, you will have to do put in a scrolled
--  window. This of course will be fixed in later versions.
--  As for Gtk_Fixed containers, the children can be located anywhere in the
--  layout (no automatic organization is done). But, as opposed to Gtk_Fixed
--  widgets, a Gtk_Layout does not try to resize itself to show all its
--  children.
--  </description>
--  <c_version>1.2.6</c_version>

with Gdk.Window;
with Gtk.Adjustment;
with Gtk.Container;
with Gtk.Widget;
with Gtk.Object;

package Gtk.Layout is

   type Gtk_Layout_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Layout is access all Gtk_Layout_Record'Class;

   procedure Gtk_New
     (Layout      : out Gtk_Layout;
      Hadjustment : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadjustment : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment);
   --  Create new layout.
   --  You can either give an explicit couple of adjustments, that will
   --  indicate the current visible area. If you don't specify any, they will
   --  be created automatically by GtkAda, which is the usual way to do.
   --  The Layout does not occupy any area on the screen, and you have to
   --  explicitly specify one with Set_Size below.

   procedure Initialize
     (Layout      : access Gtk_Layout_Record'Class;
      Hadjustment : Gtk.Adjustment.Gtk_Adjustment;
      Vadjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Layout.

   procedure Put (Layout : access Gtk_Layout_Record;
                  Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                  X      : in     Gint;
                  Y      : in     Gint);
   --  Insert a new child in the layout.
   --  The child will be displayed on the screen only if at least part of
   --  it intersects the visible area of the layout.
   --  The layout does not resize itself to automatically show the widget.

   procedure Move (Layout : access Gtk_Layout_Record;
                   Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                   X      : in     Gint;
                   Y      : in     Gint);
   --  Move a child of the layout.
   --  Nothing is done if Widget is not already a child of Layout.

   function Get_Bin_Window (Widget : access Gtk_Layout_Record)
                           return Gdk.Window.Gdk_Window;
   --  Return the window associated with the layout.
   --  You should use this one rather than Gtk.Widget.Get_Window.

   procedure Set_Size (Layout : access Gtk_Layout_Record;
                       Width  : in     Guint;
                       Height : in     Guint);
   --  Specify an absolute size for the layout.
   --  This is not the size on the screen, but the internal size of the widget.
   --  The screen's size can be set with Gtk.Widget.Set_Usize.

   function Get_Hadjustment (Layout : access Gtk_Layout_Record)
                            return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the adjustment that indicate the horizontal visual area
   --  of the layout.
   --  You generally do not have to modify the value of this adustment
   --  yourself. This is done automatically by GtkAda when the layout has
   --  been put in a Gtk_Scrolled_Window.

   function Get_Vadjustment (Layout : access Gtk_Layout_Record)
                            return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the adjustment that indicate the vertical visual area
   --  of the layout.
   --  You generally do not have to modify the value of this adustment
   --  yourself. This is done automatically by GtkAda when the layout has
   --  been put in a Gtk_Scrolled_Window.

   procedure Set_Hadjustment (Layout     : access Gtk_Layout_Record;
                              Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Specify a new adjustment to use for the horizontal visual area.

   procedure Set_Vadjustment (Layout     : access Gtk_Layout_Record;
                              Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Specify a new adjustment to use for the vertical visual area.

   procedure Freeze (Layout : access Gtk_Layout_Record);
   --  Temporarily freezes the refreshing of the layout on the screen.
   --  This should be used before doing long modifications to the children
   --  to speed up the update.
   --  Note that each call to Freeze increments an internal counter, and the
   --  widget will only be thawed when the counter reaches 0;

   procedure Thaw (Layout : access Gtk_Layout_Record);
   --  Force an immediate update of the layout.
   --  Every time a child is modified, the screen is refreshed.
   --  This is the opposite of Freeze.
   --  This subprogram decrements the internal counter.

   function Get_Xoffset (Layout : access Gtk_Layout_Record) return Guint;
   --  Get the current offset of the top-left corner.

   function Get_Yoffset (Layout : access Gtk_Layout_Record) return Guint;
   --  Get the current offset of the top-left corner.

   function Get_Width (Layout : access Gtk_Layout_Record) return Guint;
   --  Get the width in pixels of the layout.

   function Get_Height (Layout : access Gtk_Layout_Record) return Guint;
   --  Get the height in pixels of the layout.

   ----------------------------
   -- Support for GATE/DGATE --
   ----------------------------

   procedure Generate (N : in Node_Ptr; File : in File_Type);
   --  Gate internal function

   procedure Generate (Layout : in out Object.Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "set_scroll_adjustments"
   --    procedure Handler (Layout : access Gtk_Layout_Record'Class;
   --                       Hadj   : in Gtk.Adjustment.Gtk_Adjustment;
   --                       Vadj   : in Gtk.Adjustment.Gtk_Adjustment);
   --
   --    Emitted whenever at least one of the adjustment of the layout is
   --    changed.
   --  </signals>
private
   type Gtk_Layout_Record is new Gtk.Container.Gtk_Container_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_layout_get_type");
end Gtk.Layout;
