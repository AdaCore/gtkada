------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Represents a widget tooltip.
--
--  Basic tooltips can be realized simply by using
--  [methodGtk.Widget.set_tooltip_text] or
--  [methodGtk.Widget.set_tooltip_markup] without any explicit tooltip object.
--
--  When you need a tooltip with a little more fancy contents, like adding an
--  image, or you want the tooltip to have different contents per `GtkTreeView`
--  row or cell, you will have to do a little more work:
--
--  - Set the [propertyGtk.Widget:has-tooltip] property to True. This will
--  make GTK monitor the widget for motion and related events which are needed
--  to determine when and where to show a tooltip.
--
--  - Connect to the [signalGtk.Widget::query-tooltip] signal. This signal
--  will be emitted when a tooltip is supposed to be shown. One of the
--  arguments passed to the signal handler is a `GtkTooltip` object. This is
--  the object that we are about to display as a tooltip, and can be
--  manipulated in your callback using functions like
--  [methodGtk.Tooltip.set_icon]. There are functions for setting the tooltip's
--  markup, setting an image from a named icon, or even putting in a custom
--  widget.
--
--  - Return True from your ::query-tooltip handler. This causes the tooltip
--  to be show. If you return False, it will not be shown.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Paintable; use Gdk.Paintable;
with Gdk.Rectangle; use Gdk.Rectangle;
with Glib;          use Glib;
with Glib.G_Icon;   use Glib.G_Icon;
with Glib.Object;   use Glib.Object;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Tooltip is

   type Gtk_Tooltip_Record is new GObject_Record with null record;
   type Gtk_Tooltip is access all Gtk_Tooltip_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tooltip_get_type");

   -------------
   -- Methods --
   -------------

   procedure Set_Custom
      (Self          : not null access Gtk_Tooltip_Record;
       Custom_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Replaces the widget packed into the tooltip with Custom_Widget.
   --  Custom_Widget does not get destroyed when the tooltip goes away. By
   --  default a box with a `GtkImage` and `GtkLabel` is embedded in the
   --  tooltip, which can be configured using Gtk.Tooltip.Set_Markup and
   --  Gtk.Tooltip.Set_Icon.
   --  @param Custom_Widget a `GtkWidget`, or null to unset the old custom
   --  widget.

   procedure Set_Icon
      (Self      : not null access Gtk_Tooltip_Record;
       Paintable : Gdk.Paintable.Gdk_Paintable);
   --  Sets the icon of the tooltip (which is in front of the text) to be
   --  Paintable. If Paintable is null, the image will be hidden.
   --  @param Paintable a `GdkPaintable`

   procedure Set_Icon_From_Gicon
      (Self   : not null access Gtk_Tooltip_Record;
       G_Icon : Glib.G_Icon.G_Icon);
   --  Sets the icon of the tooltip (which is in front of the text) to be the
   --  icon indicated by Gicon with the size indicated by Size. If Gicon is
   --  null, the image will be hidden.
   --  @param G_Icon a `GIcon` representing the icon

   procedure Set_Icon_From_Icon_Name
      (Self      : not null access Gtk_Tooltip_Record;
       Icon_Name : UTF8_String := "");
   --  Sets the icon of the tooltip (which is in front of the text) to be the
   --  icon indicated by Icon_Name with the size indicated by Size. If
   --  Icon_Name is null, the image will be hidden.
   --  @param Icon_Name an icon name

   procedure Set_Markup
      (Self   : not null access Gtk_Tooltip_Record;
       Markup : UTF8_String := "");
   --  Sets the text of the tooltip to be Markup.
   --  The string must be marked up with Pango markup. If Markup is null, the
   --  label will be hidden.
   --  @param Markup a string with Pango markup or NLL

   procedure Set_Text
      (Self : not null access Gtk_Tooltip_Record;
       Text : UTF8_String := "");
   --  Sets the text of the tooltip to be Text.
   --  If Text is null, the label will be hidden. See also
   --  [methodGtk.Tooltip.set_markup].
   --  @param Text a text string

   procedure Set_Tip_Area
      (Self : not null access Gtk_Tooltip_Record;
       Rect : Gdk.Rectangle.Gdk_Rectangle);
   --  Sets the area of the widget, where the contents of this tooltip apply,
   --  to be Rect (in widget coordinates). This is especially useful for
   --  properly setting tooltips on `GtkTreeView` rows and cells,
   --  `GtkIconViews`, etc.
   --  For setting tooltips on `GtkTreeView`, please refer to the convenience
   --  functions for this: gtk_tree_view_set_tooltip_row and
   --  gtk_tree_view_set_tooltip_cell.
   --  @param Rect a `GdkRectangle`

end Gtk.Tooltip;
