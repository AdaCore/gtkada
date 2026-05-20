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

--  A layout manager for widgets with a single child.
--
--  `GtkBinLayout` will stack each child of a widget on top of each other,
--  using the [propertyGtk.Widget:hexpand], [propertyGtk.Widget:vexpand],
--  [propertyGtk.Widget:halign], and [propertyGtk.Widget:valign] properties of
--  each child to determine where they should be positioned.
--
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Gtk.Layout_Manager; use Gtk.Layout_Manager;

package Gtk.Bin_Layout is

   type Gtk_Bin_Layout_Record is new Gtk_Layout_Manager_Record with null record;
   type Gtk_Bin_Layout is access all Gtk_Bin_Layout_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Bin_Layout);
   procedure Initialize (Self : not null access Gtk_Bin_Layout_Record'Class);
   --  Creates a new `GtkBinLayout` instance.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Bin_Layout_New return Gtk_Bin_Layout;
   --  Creates a new `GtkBinLayout` instance.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_bin_layout_get_type");

end Gtk.Bin_Layout;
