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
--  Basic tooltips can be realized simply by using Gtk.Widget.Set_Tooltip_Text
--  or Gtk.Widget.Set_Tooltip_Markup without any explicit tooltip object.
--
--  When you need a tooltip with a little more fancy contents, like adding an
--  image, or you want the tooltip to have different contents per
--  Gtk.Tree_View.Gtk_Tree_View row or cell, you will have to do a little more
--  work:
--
--  - Set the Gtk.Widget.Gtk_Widget:has-tooltip property to True, this will
--  make GTK+ monitor the widget for motion and related events which are needed
--  to determine when and where to show a tooltip.
--
--  - Connect to the Gtk.Widget.Gtk_Widget::query-tooltip signal. This signal
--  will be emitted when a tooltip is supposed to be shown. One of the
--  arguments passed to the signal handler is a GtkTooltip object. This is the
--  object that we are about to display as a tooltip, and can be manipulated in
--  your callback using functions like Gtk.Tooltip.Set_Icon. There are
--  functions for setting the tooltip's markup, setting an image from a named
--  icon, or even putting in a custom widget.
--
--  Return True from your query-tooltip handler. This causes the tooltip to be
--  show. If you return False, it will not be shown.
--
--  In the probably rare case where you want to have even more control over
--  the tooltip that is about to be shown, you can set your own
--  Gtk.Window.Gtk_Window which will be used as tooltip window. This works as
--  follows:
--
--  - Set Gtk.Widget.Gtk_Widget:has-tooltip and connect to
--  Gtk.Widget.Gtk_Widget::query-tooltip as before. Use
--  Gtk.Widget.Set_Tooltip_Window to set a Gtk.Window.Gtk_Window created by you
--  as tooltip window.
--
--  - In the Gtk.Widget.Gtk_Widget::query-tooltip callback you can access your
--  window using Gtk.Widget.Get_Tooltip_Window and manipulate as you wish. The
--  semantics of the return value are exactly as before, return True to show
--  the window, False to not show it.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Display;   use Gdk.Display;
with Gdk.Pixbuf;    use Gdk.Pixbuf;
with Gdk.Rectangle; use Gdk.Rectangle;
with Glib;          use Glib;
with Glib.G_Icon;   use Glib.G_Icon;
with Glib.Object;   use Glib.Object;
with Gtk.Enums;     use Gtk.Enums;
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
   --  default a box with a Gtk.Image.Gtk_Image and Gtk.Label.Gtk_Label is
   --  embedded in the tooltip, which can be configured using
   --  Gtk.Tooltip.Set_Markup and Gtk.Tooltip.Set_Icon.
   --  Since: gtk+ 2.12
   --  "custom_widget": a Gtk.Widget.Gtk_Widget, or null to unset the old
   --  custom widget.

   procedure Set_Icon
      (Self   : not null access Gtk_Tooltip_Record;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Sets the icon of the tooltip (which is in front of the text) to be
   --  Pixbuf. If Pixbuf is null, the image will be hidden.
   --  Since: gtk+ 2.12
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf, or null

   procedure Set_Icon_From_Gicon
      (Self   : not null access Gtk_Tooltip_Record;
       G_Icon : Glib.G_Icon.G_Icon;
       Size   : Gtk.Enums.Gtk_Icon_Size);
   --  Sets the icon of the tooltip (which is in front of the text) to be the
   --  icon indicated by Gicon with the size indicated by Size. If Gicon is
   --  null, the image will be hidden.
   --  Since: gtk+ 2.20
   --  "gicon": a Glib.G_Icon.G_Icon representing the icon, or null
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   procedure Set_Icon_From_Icon_Name
      (Self      : not null access Gtk_Tooltip_Record;
       Icon_Name : UTF8_String := "";
       Size      : Gtk.Enums.Gtk_Icon_Size);
   --  Sets the icon of the tooltip (which is in front of the text) to be the
   --  icon indicated by Icon_Name with the size indicated by Size. If
   --  Icon_Name is null, the image will be hidden.
   --  Since: gtk+ 2.14
   --  "icon_name": an icon name, or null
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   procedure Set_Icon_From_Stock
      (Self     : not null access Gtk_Tooltip_Record;
       Stock_Id : UTF8_String := "";
       Size     : Gtk.Enums.Gtk_Icon_Size);
   pragma Obsolescent (Set_Icon_From_Stock);
   --  Sets the icon of the tooltip (which is in front of the text) to be the
   --  stock item indicated by Stock_Id with the size indicated by Size. If
   --  Stock_Id is null, the image will be hidden.
   --  Since: gtk+ 2.12
   --  Deprecated since 3.10, 1
   --  "stock_id": a stock id, or null
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   procedure Set_Markup
      (Self   : not null access Gtk_Tooltip_Record;
       Markup : UTF8_String := "");
   --  Sets the text of the tooltip to be Markup, which is marked up with the
   --  [Pango text markup language][PangoMarkupFormat]. If Markup is null, the
   --  label will be hidden.
   --  Since: gtk+ 2.12
   --  "markup": a markup string (see [Pango markup
   --  format][PangoMarkupFormat]) or null

   procedure Set_Text
      (Self : not null access Gtk_Tooltip_Record;
       Text : UTF8_String := "");
   --  Sets the text of the tooltip to be Text. If Text is null, the label
   --  will be hidden. See also Gtk.Tooltip.Set_Markup.
   --  Since: gtk+ 2.12
   --  "text": a text string or null

   procedure Set_Tip_Area
      (Self : not null access Gtk_Tooltip_Record;
       Rect : Gdk.Rectangle.Gdk_Rectangle);
   --  Sets the area of the widget, where the contents of this tooltip apply,
   --  to be Rect (in widget coordinates). This is especially useful for
   --  properly setting tooltips on Gtk.Tree_View.Gtk_Tree_View rows and cells,
   --  Gtk_Icon_Views, etc.
   --  For setting tooltips on Gtk.Tree_View.Gtk_Tree_View, please refer to
   --  the convenience functions for this: Gtk.Tree_View.Set_Tooltip_Row and
   --  Gtk.Tree_View.Set_Tooltip_Cell.
   --  Since: gtk+ 2.12
   --  "rect": a Gdk.Rectangle.Gdk_Rectangle

   ---------------
   -- Functions --
   ---------------

   procedure Trigger_Tooltip_Query
      (Display : not null access Gdk.Display.Gdk_Display_Record'Class);
   --  Triggers a new tooltip query on Display, in order to update the current
   --  visible tooltip, or to show/hide the current tooltip. This function is
   --  useful to call when, for example, the state of the widget changed by a
   --  key press.
   --  Since: gtk+ 2.12
   --  "display": a Gdk.Display.Gdk_Display

end Gtk.Tooltip;
