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
--  GtkOffscreenWindow is strictly intended to be used for obtaining snapshots
--  of widgets that are not part of a normal widget hierarchy. Since
--  Gtk.Offscreen_Window.Gtk_Offscreen_Window is a toplevel widget you cannot
--  obtain snapshots of a full window with it since you cannot pack a toplevel
--  widget in another toplevel.
--
--  The idea is to take a widget and manually set the state of it, add it to a
--  GtkOffscreenWindow and then retrieve the snapshot as a cairo_surface_t or
--  Gdk.Pixbuf.Gdk_Pixbuf.
--
--  GtkOffscreenWindow derives from Gtk.Window.Gtk_Window only as an
--  implementation detail. Applications should not use any API specific to
--  Gtk.Window.Gtk_Window to operate on this object. It should be treated as a
--  Gtk.Bin.Gtk_Bin that has no parent widget.
--
--  When contained offscreen widgets are redrawn, GtkOffscreenWindow will emit
--  a Gtk.Widget.Gtk_Widget::damage-event signal.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;         use Cairo;
with Gdk.Pixbuf;    use Gdk.Pixbuf;
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Window;    use Gtk.Window;

package Gtk.Offscreen_Window is

   type Gtk_Offscreen_Window_Record is new Gtk_Window_Record with null record;
   type Gtk_Offscreen_Window is access all Gtk_Offscreen_Window_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Offscreen_Window);
   procedure Initialize
      (Self : not null access Gtk_Offscreen_Window_Record'Class);
   --  Creates a toplevel container widget that is used to retrieve snapshots
   --  of widgets without showing them on the screen.
   --  Since: gtk+ 2.20
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Offscreen_Window_New return Gtk_Offscreen_Window;
   --  Creates a toplevel container widget that is used to retrieve snapshots
   --  of widgets without showing them on the screen.
   --  Since: gtk+ 2.20

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_offscreen_window_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Pixbuf
      (Self : not null access Gtk_Offscreen_Window_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Retrieves a snapshot of the contained widget in the form of a
   --  Gdk.Pixbuf.Gdk_Pixbuf. This is a new pixbuf with a reference count of 1,
   --  and the application should unreference it once it is no longer needed.
   --  Since: gtk+ 2.20

   function Get_Surface
      (Self : not null access Gtk_Offscreen_Window_Record)
       return Cairo.Cairo_Surface;
   --  Retrieves a snapshot of the contained widget in the form of a
   --  cairo_surface_t. If you need to keep this around over window resizes
   --  then you should add a reference to it.
   --  Since: gtk+ 2.20

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Offscreen_Window_Record, Gtk_Offscreen_Window);
   function "+"
     (Widget : access Gtk_Offscreen_Window_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Offscreen_Window
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Offscreen_Window;
