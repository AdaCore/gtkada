-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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
--  Gtk_Scrolled_Window is a Gtk_Bin child: it's a container the accepts a
--  single child widget. Gtk_Scrolled_Window adds scrollbars to the child
--  widget.
--
--  The scrolled window can work in two ways. Some widgets have native
--  scrolling support; these widgets have "slots" for Gtk_Adjustment objects.
--  The scrolled window installs Gtk_Adjustment objects in the child window's
--  slots using the "set_scroll_adjustments" signal (Conceptually, these
--  widgets implement a "Scrollable" interface).
--
--  The second way to use the scrolled window is useful with widgets that lack
--  the "set_scroll_adjustments" signal. The Gtk_Viewport widget acts as a
--  proxy, implementing scrollability for child widgets that lack their own
--  scrolling capabilities.
--
--  If a widget has native scrolling abilities, it can be added to the
--  Gtk_Scrolled_Window with Gtk.Container.Add. If a widget does not, you must
--  first add the widget to a Gtk_Viewport, then add the Gtk_Viewport to the
--  scrolled window. The convenience function Add_With_Viewport does exactly
--  this, so you can ignore the presence of the viewport.
--
--  If you want to create your own new widget type that can be inserted
--  directly into a scrolled_window, you need to specify a signal for
--  Set_Scroll_Adjustments in the call to Gtk.Object.Initialize_Class_Record.
--
--  </description>
--  <c_version>1.3.11</c_version>

with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Bin;
with Gtk.Enums;
with Gtk.Widget;

package Gtk.Scrolled_Window is

   type Gtk_Scrolled_Window_Record is new Bin.Gtk_Bin_Record with private;
   type Gtk_Scrolled_Window is access all Gtk_Scrolled_Window_Record'Class;

   procedure Gtk_New
     (Scrolled_Window : out Gtk_Scrolled_Window;
      Hadjustment     : Gtk_Adjustment := Null_Adjustment;
      Vadjustment     : Gtk_Adjustment := Null_Adjustment);
   --  Create a new scrolled window.
   --  The two arguments are the scrolled window's horizontal and vertical
   --  adjustments; these will be shared with the scrollbars and the child
   --  widget to keep the bars in sync with the child. Usually you want to use
   --  the default value Null_Adjustment for the adjustments, which will cause
   --  the scrolled window to create them for you.

   procedure Initialize
     (Scrolled_Window : access Gtk_Scrolled_Window_Record'Class;
      Hadjustment     : Gtk_Adjustment := Null_Adjustment;
      Vadjustment     : Gtk_Adjustment := Null_Adjustment);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Scrolled_Window.

   procedure Set_Hadjustment
     (Scrolled_Window : access Gtk_Scrolled_Window_Record;
      Hadjustment     : Gtk_Adjustment);
   --  Set the Gtk_Adjustment for the horizontal scrollbar.

   procedure Set_Vadjustment
     (Scrolled_Window : access Gtk_Scrolled_Window_Record;
      Vadjustment     : Gtk_Adjustment);
   --  Set the Gtk_Adjustment for the vertical scrollbar.

   function Get_Hadjustment
     (Scrolled_Window : access Gtk_Scrolled_Window_Record)
      return Gtk_Adjustment;
   --  Return the horizontal scrollbar's adjustment.
   --  This adjustment is used to connect the horizontal scrollbar to the child
   --  widget's horizontal scroll functionality.

   function Get_Vadjustment
     (Scrolled_Window : access Gtk_Scrolled_Window_Record)
      return Gtk_Adjustment;
   --  Return the vertical scrollbar's adjustment.
   --  This adjustment is used to connect the vertical scrollbar to the child
   --  widget's vertical scroll functionality.

   procedure Set_Policy
     (Scrolled_Window    : access Gtk_Scrolled_Window_Record;
      H_Scrollbar_Policy : Enums.Gtk_Policy_Type;
      V_Scrollbar_Policy : Enums.Gtk_Policy_Type);
   --  Set the scrollbar policy for the horizontal and vertical scrollbars.
   --  It determines when the scrollbar should appear; it is a value
   --  from the Gtk_Policy_Type enumeration. If Policy_Always, the scrollbar is
   --  always present; if Policy_Never, the scrollbar is never present; if
   --  Policy_Automatic, the scrollbar is present only if needed (that is, if
   --  the slider part of the bar would be smaller than the trough - the
   --  display is larger than the page size).

   procedure Get_Policy
     (Scrolled_Window    : access Gtk_Scrolled_Window_Record;
      H_Scrollbar_Policy : out Enums.Gtk_Policy_Type;
      V_Scrollbar_Policy : out Enums.Gtk_Policy_Type);
   --  Return the scrollbar policy for the horizontal and vertical scrollbars.

   procedure Set_Placement
     (Scrolled_Window  : access Gtk_Scrolled_Window_Record;
      Window_Placement : Gtk.Enums.Gtk_Corner_Type);
   --  Determine the location of the widget with respect to the scrollbars.
   --  The default is Corner_Top_Left.

   function Get_Placement
     (Scrolled_Window  : access Gtk_Scrolled_Window_Record)
      return Gtk.Enums.Gtk_Corner_Type;
   --  Return the location of the widget with respect to the scrollbars.

   procedure Set_Shadow_Type
     (Scrolled_Window : access Gtk_Scrolled_Window_Record;
      Shadow_Type     : Gtk.Enums.Gtk_Shadow_Type);
   --  Change the type of shadow drawn around the contents of Scrolled_Window.

   function Get_Shadow_Type
     (Scrolled_Window : access Gtk_Scrolled_Window_Record)
      return Gtk.Enums.Gtk_Shadow_Type;
   --  Return the type of shadow drawn around the contents of Scrolled_Window.

   procedure Add_With_Viewport
     (Scrolled_Window : access Gtk_Scrolled_Window_Record;
      Child           : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Used to add children without native scrolling capabilities.
   --  This is simply a convenience function; it is equivalent to adding the
   --  unscrollable child to a viewport, then adding the viewport to the
   --  scrolled window. If a child has native scrolling, use Gtk.Container.Add
   --  instead of this function.
   --
   --  The viewport scrolls the child by moving its Gdk_Window, and takes the
   --  size of the child to be the size of its toplevel Gdk_Window. This will
   --  be very wrong for most widgets that support native scrolling; for
   --  example, if you add a Gtk_Clist with a viewport, the whole widget will
   --  scroll, including the column headings. Thus Gtk_Clist supports scrolling
   --  already, and should not be used with the GtkViewport proxy.
   --
   --  A widget supports scrolling natively if it contains a valid
   --  "set_scroll_adjustments" signal.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Scrolled_Window_Record is new Bin.Gtk_Bin_Record with null record;

   pragma Import (C, Get_Type, "gtk_scrolled_window_get_type");
end Gtk.Scrolled_Window;
