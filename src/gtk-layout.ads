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
--  A Gtk_Layout is a widget that can have an almost infinite size, without
--  occupying a lot of memory. Its children can be located anywhere within
--  it, but will only appear on the screen if the visible area of the layout
--  contains them.
--  Just like a Gtk_Viewport, its visible area is indicated by two
--  Gtk_Adjustment widgets, and thus a Gtk_Layout can be put as is in a
--  Gtk_Scrolled_Window.
--  As for Gtk_Fixed containers, the children can be located anywhere in the
--  layout (no automatic organization is done). But, as opposed to Gtk_Fixed
--  widgets, a Gtk_Layout does not try to resize itself to show all its
--  children.
--
--  Starting from GtkAda 2.0, you have to call Set_Size and specify the maximum
--  size of the layout, otherwise children added with Put outside the size
--  defined for the layout will never be visible.
--  One way to do this is to systematically call Set_Size before calling Put,
--  and make sure you specify a size big enough for the layout.
--
--  </description>
--  <c_version>2.8.17</c_version>

with Glib.Properties;
with Gdk.Window;
with Gtk.Adjustment;
with Gtk.Container;
with Gtk.Widget;

package Gtk.Layout is

   type Gtk_Layout_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Layout is access all Gtk_Layout_Record'Class;

   procedure Gtk_New
     (Layout      : out Gtk_Layout;
      Hadjustment : Adjustment.Gtk_Adjustment := null;
      Vadjustment : Adjustment.Gtk_Adjustment := null);
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

   procedure Put
     (Layout : access Gtk_Layout_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : Gint;
      Y      : Gint);
   --  Insert a new child in the layout.
   --  The child will be displayed on the screen only if at least part of
   --  it intersects the visible area of the layout.
   --  The layout does not resize itself to automatically show the widget.
   --  You also need to call Set_Size, if the size you initially defined is
   --  smaller than (X, Y), or the child will never be visible even if the
   --  layout is scrolled.

   procedure Move
     (Layout : access Gtk_Layout_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : Gint;
      Y      : Gint);
   --  Move a child of the layout.
   --  Nothing is done if Widget is not already a child of Layout.

   function Get_Bin_Window
     (Widget : access Gtk_Layout_Record) return Gdk.Window.Gdk_Window;
   --  Return the window associated with the layout.
   --  You should use this one rather than Gtk.Widget.Get_Window.

   procedure Set_Size
     (Layout : access Gtk_Layout_Record;
      Width  : Guint;
      Height : Guint);
   procedure Get_Size
     (Layout : access Gtk_Layout_Record;
      Width  : out Guint;
      Height : out Guint);
   --  Specify an absolute size for the layout.
   --  This is not the size on the screen, but the internal size of the widget.
   --  The screen's size can be set with Gtk.Widget.Set_Usize.

   procedure Set_Hadjustment
     (Layout     : access Gtk_Layout_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   function Get_Hadjustment
     (Layout : access Gtk_Layout_Record) return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the adjustment that indicate the horizontal visual area
   --  of the layout.
   --  You generally do not have to modify the value of this adjustment
   --  yourself, since this is done automatically when the layout has
   --  been put in a Gtk_Scrolled_Window.

   procedure Set_Vadjustment
     (Layout     : access Gtk_Layout_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   function Get_Vadjustment
     (Layout : access Gtk_Layout_Record) return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the adjustment that indicate the vertical visual area
   --  of the layout.
   --  You generally do not have to modify the value of this adjustment
   --  yourself, since this is done automatically when the layout has
   --  been put in a Gtk_Scrolled_Window.

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   function Get_Width (Layout : access Gtk_Layout_Record) return Guint;
   pragma Obsolescent;  --  Get_Width
   --  Deprecated, only provided for compatibility, see Get_Size

   function Get_Height (Layout : access Gtk_Layout_Record) return Guint;
   --  pragma Obsolescent;  --  Get_Height
   --  Deprecated, only provided for compatibility, see Get_Size

   procedure Freeze (Layout : access Gtk_Layout_Record);
   pragma Obsolescent;  --  Freeze
   --  Deprecated, only provided for compatibility.

   procedure Thaw (Layout : access Gtk_Layout_Record);
   pragma Obsolescent;  --  Thaw
   --  Deprecated, only provided for compatibility.

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Hadjustment_Property
   --    Type:  Gtk_Adjustment_Record'Class
   --    Flags: read-write
   --    Descr: The GtkAdjustment for the horizontal position.
   --    See also: Set_Hadjustment and Get_Hadjustment
   --
   --  - Name:  Vadjustment_Property
   --    Type:  Gtk_Adjustment_Record'Class
   --    Flags: read-write
   --    Descr: The GtkAdjustment for the vertical position.
   --    See also: Set_Vadjustment and Get_Vadjustment
   --
   --  - Name:  Width_Property
   --    Type:  Guint
   --    Flags: read-write
   --    Descr: The width of the layout.
   --    See also: Set_Size and Get_Width
   --
   --  - Name:  Height_Property
   --    Type:  Guint
   --    Flags: read-write
   --    Descr: The height of the layout.
   --    See also: Set_Size and Get_Height
   --
   --  </properties>

   Hadjustment_Property : constant Glib.Properties.Property_Object;
   Vadjustment_Property : constant Glib.Properties.Property_Object;
   Width_Property       : constant Glib.Properties.Property_Uint;
   Height_Property      : constant Glib.Properties.Property_Uint;

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

   Signal_Set_Scroll_Adjustments : constant String := "set_scroll_adjustments";

private
   type Gtk_Layout_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

   Hadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("hadjustment");
   Vadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("vadjustment");
   Width_Property       : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("width");
   Height_Property      : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("height");

   pragma Import (C, Get_Type, "gtk_layout_get_type");
end Gtk.Layout;
