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

--  An interface for widgets that have their own [classGdk.Surface].
--
--  The obvious example of a `GtkNative` is `GtkWindow`.
--
--  Every widget that is not itself a `GtkNative` is contained in one, and you
--  can get it with [methodGtk.Widget.get_native].
--
--  To get the surface of a `GtkNative`, use [methodGtk.Native.get_surface].
--  It is also possible to find the `GtkNative` to which a surface belongs,
--  with [funcGtk.Native.get_for_surface].
--
--  In addition to a [classGdk.Surface], a `GtkNative` also provides a
--  [classGsk.Renderer] for rendering on that surface. To get the renderer, use
--  [methodGtk.Native.get_renderer].

pragma Warnings (Off, "*is already use-visible*");
with Gdk;        use Gdk;
with Glib;       use Glib;
with Glib.Types; use Glib.Types;

package Gtk.Native is

   type Gtk_Native is new Glib.Types.GType_Interface;
   Null_Gtk_Native : constant Gtk_Native;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_native_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Surface (Self : Gtk_Native) return Gdk.Gdk_Surface;
   --  Returns the surface of this `GtkNative`.
   --  @return the surface of Self

   procedure Get_Surface_Transform
      (Self : Gtk_Native;
       X    : out Gdouble;
       Y    : out Gdouble);
   pragma Import (C, Get_Surface_Transform, "gtk_native_get_surface_transform");
   --  Retrieves the surface transform of Self.
   --  This is the translation from Self's surface coordinates into Self's
   --  widget coordinates.
   --  @param X return location for the x coordinate
   --  @param Y return location for the y coordinate

   procedure Realize (Self : Gtk_Native);
   pragma Import (C, Realize, "gtk_native_realize");
   --  Realizes a `GtkNative`.
   --  This should only be used by subclasses.

   procedure Unrealize (Self : Gtk_Native);
   pragma Import (C, Unrealize, "gtk_native_unrealize");
   --  Unrealizes a `GtkNative`.
   --  This should only be used by subclasses.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Native"

   function "+" (W : Gtk_Native) return Gtk_Native;
   pragma Inline ("+");

private

   Null_Gtk_Native : constant Gtk_Native :=
      Gtk_Native (Glib.Types.Null_Interface);
end Gtk.Native;
