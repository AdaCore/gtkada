------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       ----                     Copyright (C) 1998-2012, AdaCore                     --
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

with System;
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Item is

   -----------------------
   -- Local Subprograms --
   -----------------------

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Item_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   -----------------
   -- Item_Select --
   -----------------

   procedure Item_Select (Item : access Gtk_Item_Record) is
      procedure Internal (Item : System.Address);
      pragma Import (C, Internal, "gtk_item_select");

   begin
      Internal (Get_Object (Item));
   end Item_Select;

   -------------------
   -- Item_Deselect --
   -------------------

   procedure Item_Deselect (Item : access Gtk_Item_Record) is
      procedure Internal (Item : System.Address);
      pragma Import (C, Internal, "gtk_item_deselect");

   begin
      Internal (Get_Object (Item));
   end Item_Deselect;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Item : access Gtk_Item_Record) is
      procedure Internal (Item : System.Address);
      pragma Import (C, Internal, "gtk_item_toggle");

   begin
      Internal (Get_Object (Item));
   end Toggle;

end Gtk.Item;
