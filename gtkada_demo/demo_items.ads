------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                    Copyright (C) 1998-2026, AdaCore                      --
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

--  The item type behind the demo selector.
--
--  One Demo_Item represents either a category or a demo, in the column view
--  which lists all available demos.

with Ada.Strings.Unbounded;

with Glib;
with Glib.Object;
with Gtk.Frame;

package Demo_Items is

   type Demo_Function is
     access procedure (Frame : access Gtk.Frame.Gtk_Frame_Record'Class);

   type Help_Function is access function return String;

   type Demo_Item_Record is new Glib.Object.GObject_Record with record
      Title : Ada.Strings.Unbounded.Unbounded_String;
      --  The text shown in the selector

      Run : Demo_Function := null;
      --  The actual demo. Null for a category row.

      Help : Help_Function := null;
      --  The demo's help text, using the "@b...@B" markup.
   end record;
   type Demo_Item is access all Demo_Item_Record'Class;

   function Get_Type return Glib.GType;
   --  The GType registered for Demo_Item_Record. Needed by the list store.

   procedure Gtk_New
     (Self  : out Demo_Item;
      Title : String;
      Run   : Demo_Function := null;
      Help  : Help_Function := null);
   --  A demo row. If Run is null, this is a category row.

   function To_Demo_Item (Object : Glib.Object.GObject) return Demo_Item;
   --  Object seen as a Demo_Item.

   function Title_Of (Object : Glib.Object.GObject) return String;
   --  The title Object would show in the selector, or "" if it is not a
   --  Demo_Item.

end Demo_Items;
