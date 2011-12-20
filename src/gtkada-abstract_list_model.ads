------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Gtk.Tree_Model;
with Gtkada.Abstract_Tree_Model;

package Gtkada.Abstract_List_Model is

   type Gtk_Abstract_List_Model_Record is
   abstract new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record
   with private;

   type Gtk_Abstract_List_Model is
      access all Gtk_Abstract_List_Model_Record'Class;

   procedure Initialize (Self : access Gtk_Abstract_List_Model_Record'Class);

   function Get_Flags
     (Self : access Gtk_Abstract_List_Model_Record)
      return Gtk.Tree_Model.Tree_Model_Flags;
   --  Return a set of flags supported by this interface.

   function Has_Child
     (Self : access Gtk_Abstract_List_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Return True if Iter has children, False otherwise.

   function Children
     (Self   : access Gtk_Abstract_List_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Returns the first child of Parent.

   function Parent
     (Self  : access Gtk_Abstract_List_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Returns the parent of Child. For the list it always returns Null_Iter.

private

   type Gtk_Abstract_List_Model_Record is
   abstract new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record
   with null record;

end Gtkada.Abstract_List_Model;
