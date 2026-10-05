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

--  A `GtkEntryBuffer` that locks the underlying memory to prevent it from
--  being swapped to disk.
--
--  `GtkPasswordEntry` uses a `GtkPasswordEntryBuffer`.

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Gtk.Entry_Buffer; use Gtk.Entry_Buffer;

package Gtk.Password_Entry_Buffer is

   type Gtk_Password_Entry_Buffer_Record is new Gtk_Entry_Buffer_Record with null record;
   type Gtk_Password_Entry_Buffer is access all Gtk_Password_Entry_Buffer_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Password_Entry_Buffer);
   procedure Initialize
      (Self : not null access Gtk_Password_Entry_Buffer_Record'Class);
   --  Creates a new `GtkEntryBuffer` using secure memory allocations.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Password_Entry_Buffer_New return Gtk_Password_Entry_Buffer;
   --  Creates a new `GtkEntryBuffer` using secure memory allocations.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_password_entry_buffer_get_type");

end Gtk.Password_Entry_Buffer;
