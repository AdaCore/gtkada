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

--  An interface for widgets that can act as the root of a widget hierarchy.
--
--  The root widget takes care of providing the connection to the windowing
--  system and manages layout, drawing and event delivery for its widget
--  hierarchy.
--
--  The obvious example of a `GtkRoot` is `GtkWindow`.
--
--  To get the display to which a `GtkRoot` belongs, use
--  [methodGtk.Root.get_display].
--
--  `GtkRoot` also maintains the location of keyboard focus inside its widget
--  hierarchy, with [methodGtk.Root.set_focus] and [methodGtk.Root.get_focus].

pragma Warnings (Off, "*is already use-visible*");
with Gdk;        use Gdk;
with Glib;       use Glib;
with Glib.Types; use Glib.Types;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Root is

   type Gtk_Root is new Glib.Types.GType_Interface;
   Null_Gtk_Root : constant Gtk_Root;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_root_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Display (Self : Gtk_Root) return Gdk.Gdk_Display;
   --  Returns the display that this `GtkRoot` is on.
   --  @return the display of Root

   function Get_Focus (Self : Gtk_Root) return Gtk.Widget.Gtk_Widget;
   --  Retrieves the current focused widget within the root.
   --  Note that this is the widget that would have the focus if the root is
   --  active; if the root is not focused then `gtk_widget_has_focus (widget)`
   --  will be False for the widget.
   --  @return the currently focused widget

   procedure Set_Focus
      (Self  : Gtk_Root;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  If Focus is not the current focus widget, and is focusable, sets it as
   --  the focus widget for the root.
   --  If Focus is null, unsets the focus widget for the root.
   --  To set the focus to a particular widget in the root, it is usually more
   --  convenient to use [methodGtk.Widget.grab_focus] instead of this
   --  function.
   --  @param Focus widget to be the new focus widget, or null to unset the
   --  focus widget

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Root"

   function "+" (W : Gtk_Root) return Gtk_Root;
   pragma Inline ("+");

private

   Null_Gtk_Root : constant Gtk_Root :=
      Gtk_Root (Glib.Types.Null_Interface);
end Gtk.Root;
