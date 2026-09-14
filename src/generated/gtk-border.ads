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

--  Specifies a border around a rectangular area.
--
--  Each side can have a different width.

pragma Warnings (Off, "*is already use-visible*");
with Glib; use Glib;

package Gtk.Border is

   type Gtk_Border is record
      Left : Gint16;
      Right : Gint16;
      Top : Gint16;
      Bottom : Gint16;
   end record;
   pragma Convention (C, Gtk_Border);
   --  Specifies a border around a rectangular area.
   --
   --  Each side can have a different width.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_border_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Self : not null access Gtk_Border) return not null access Gtk_Border;
   pragma Import (C, Copy, "gtk_border_copy");
   --  Copies a `GtkBorder`.

   procedure Free (Self : not null access Gtk_Border);
   pragma Import (C, Free, "gtk_border_free");
   --  Frees a `GtkBorder`.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Gtk_Border) return Gtk_Border;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   procedure Gtk_New (Self : out Gtk_Border);
   --  Allocates a new `GtkBorder` struct and initializes its elements to
   --  zero.

   function Gtk_Border_New return not null access Gtk_Border;
   pragma Import (C, Gtk_Border_New, "gtk_border_new");
   --  Allocates a new `GtkBorder` struct and initializes its elements to
   --  zero.

end Gtk.Border;
