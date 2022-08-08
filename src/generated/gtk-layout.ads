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
--  Gtk.Layout.Gtk_Layout is similar to Gtk.Drawing_Area.Gtk_Drawing_Area in
--  that it's a "blank slate" and doesn't do anything except paint a blank
--  background by default. It's different in that it supports scrolling
--  natively due to implementing Gtk.Scrollable.Gtk_Scrollable, and can contain
--  child widgets since it's a Gtk.Container.Gtk_Container.
--
--  If you just want to draw, a Gtk.Drawing_Area.Gtk_Drawing_Area is a better
--  choice since it has lower overhead. If you just need to position child
--  widgets at specific points, then Gtk.Fixed.Gtk_Fixed provides that
--  functionality on its own.
--
--  When handling expose events on a Gtk.Layout.Gtk_Layout, you must draw to
--  the Gdk.Gdk_Window returned by Gtk.Layout.Get_Bin_Window, rather than to
--  the one returned by Gtk.Widget.Get_Window as you would for a
--  Gtk.Drawing_Area.Gtk_Drawing_Area.
--
--  </description>
--  <description>
--  A Gtk_Layout is a widget that can have an almost infinite size, without
--  occupying a lot of memory. Its children can be located anywhere within it,
--  but will only appear on the screen if the visible area of the layout
--  contains them. Just like a Gtk_Viewport, its visible area is indicated by
--  two Gtk_Adjustment widgets, and thus a Gtk_Layout can be put as is in a
--  Gtk_Scrolled_Window. As for Gtk_Fixed containers, the children can be
--  located anywhere in the layout (no automatic organization is done). But, as
--  opposed to Gtk_Fixed widgets, a Gtk_Layout does not try to resize itself to
--  show all its children.
--
--  Starting from GtkAda 2.0, you have to call Set_Size and specify the
--  maximum size of the layout, otherwise children added with Put outside the
--  size defined for the layout will never be visible. One way to do this is to
--  systematically call Set_Size before calling Put, and make sure you specify
--  a size big enough for the layout.
--
--  </description>
--  <screenshot>gtk-layout</screenshot>
--  <group>Layout containers</group>
--  <testgtk>create_layout.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk;             use Gdk;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Scrollable;  use Gtk.Scrollable;
with Gtk.Style;       use Gtk.Style;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Layout is

   type Gtk_Layout_Record is new Gtk_Container_Record with null record;
   type Gtk_Layout is access all Gtk_Layout_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Layout      : out Gtk_Layout;
       Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null);
   procedure Initialize
      (Layout      : not null access Gtk_Layout_Record'Class;
       Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null);
   --  Creates a new Gtk.Layout.Gtk_Layout. Unless you have a specific
   --  adjustment you'd like the layout to use for scrolling, pass null for
   --  Hadjustment and Vadjustment.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "hadjustment": horizontal scroll adjustment, or null
   --  "vadjustment": vertical scroll adjustment, or null

   function Gtk_Layout_New
      (Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null)
       return Gtk_Layout;
   --  Creates a new Gtk.Layout.Gtk_Layout. Unless you have a specific
   --  adjustment you'd like the layout to use for scrolling, pass null for
   --  Hadjustment and Vadjustment.
   --  "hadjustment": horizontal scroll adjustment, or null
   --  "vadjustment": vertical scroll adjustment, or null

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_layout_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Bin_Window
      (Layout : not null access Gtk_Layout_Record) return Gdk.Gdk_Window;
   --  Retrieve the bin window of the layout used for drawing operations.
   --  Since: gtk+ 2.14

   procedure Get_Size
      (Layout : not null access Gtk_Layout_Record;
       Width  : out Guint;
       Height : out Guint);
   --  Gets the size that has been set on the layout, and that determines the
   --  total extents of the layout's scrollbar area. See gtk_layout_set_size
   --  ().
   --  "width": location to store the width set on Layout, or null
   --  "height": location to store the height set on Layout, or null

   procedure Set_Size
      (Layout : not null access Gtk_Layout_Record;
       Width  : Guint;
       Height : Guint);
   --  Sets the size of the scrollable area of the layout.
   --  "width": width of entire scrollable area
   --  "height": height of entire scrollable area

   procedure Move
      (Layout       : not null access Gtk_Layout_Record;
       Child_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       X            : Glib.Gint;
       Y            : Glib.Gint);
   --  Moves a current child of Layout to a new position.
   --  "child_widget": a current child of Layout
   --  "x": X position to move to
   --  "y": Y position to move to

   procedure Put
      (Layout       : not null access Gtk_Layout_Record;
       Child_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       X            : Glib.Gint;
       Y            : Glib.Gint);
   --  The child will be displayed on the screen only if at least part of it
   --  intersects the visible area of the layout. The layout does not resize
   --  itself to automatically show the widget. You also need to call Set_Size,
   --  if the size you initially defined is smaller than (X, Y), or the child
   --  will never be visible even if the layout is scrolled.
   --  "child_widget": child widget
   --  "x": X position of child widget
   --  "y": Y position of child widget

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Border
      (Self   : not null access Gtk_Layout_Record;
       Border : access Gtk.Style.Gtk_Border) return Boolean;

   function Get_Hadjustment
      (Self : not null access Gtk_Layout_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Hadjustment
      (Self        : not null access Gtk_Layout_Record;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Get_Hscroll_Policy
      (Self : not null access Gtk_Layout_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy;

   procedure Set_Hscroll_Policy
      (Self   : not null access Gtk_Layout_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);

   function Get_Vadjustment
      (Self : not null access Gtk_Layout_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Vadjustment
      (Self        : not null access Gtk_Layout_Record;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Get_Vscroll_Policy
      (Self : not null access Gtk_Layout_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy;

   procedure Set_Vscroll_Policy
      (Self   : not null access Gtk_Layout_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Height_Property : constant Glib.Properties.Property_Uint;

   Width_Property : constant Glib.Properties.Property_Uint;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Scrollable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Layout_Record, Gtk_Layout);
   function "+"
     (Widget : access Gtk_Layout_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Layout
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Scrollable is new Glib.Types.Implements
     (Gtk.Scrollable.Gtk_Scrollable, Gtk_Layout_Record, Gtk_Layout);
   function "+"
     (Widget : access Gtk_Layout_Record'Class)
   return Gtk.Scrollable.Gtk_Scrollable
   renames Implements_Gtk_Scrollable.To_Interface;
   function "-"
     (Interf : Gtk.Scrollable.Gtk_Scrollable)
   return Gtk_Layout
   renames Implements_Gtk_Scrollable.To_Object;

private
   Width_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("width");
   Height_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("height");
end Gtk.Layout;
