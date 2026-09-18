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

--  The catalogue of demos shown in the selector.
--
--  This is the one place anyone has to edit to add a demo: append an entry
--  to the Demos table in the body, naming its full "Category/Title" path.
--  The categories are derived from those paths, so no second list has to be
--  kept in step.

with Glib.List_Model;
with Glib.Object;

package Demo_Registry is

   function Root_Model return Glib.List_Model.Glist_Model;
   --  The top level of the selector tree: one Demo_Items.Demo_Item per
   --  category, plus one per uncategorised demo, in alphabetical order.
   --
   --  Transfer full, to suit Gtk.Tree_List_Model's constructor: the registry
   --  builds the store on the first call and hands over its own reference,
   --  so this is meant to be called once.

   function Children_Of
     (Item : Glib.Object.GObject) return Glib.List_Model.Glist_Model;
   --  The create-child-model function for Gtk.Tree_List_Model: the demos of
   --  the category Item stands for, or Null_Glist_Model when Item is a leaf
   --  -- which is what tells the tree that the row can never have children,
   --  and so must be drawn without an expander arrow.
   --
   --  Transfer full, as the callback demands; the registry keeps the store
   --  for the next expansion, so the reference returned is an extra one.

end Demo_Registry;
