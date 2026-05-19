------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

package Gtk.Tree_Model.Utils is

   function Init_Tree_Iter
     (Stamp       : Glib.Gint;
      User_Data_1 : System.Address := System.Null_Address;
      User_Data_2 : System.Address := System.Null_Address;
      User_Data_3 : System.Address := System.Null_Address)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Is_Null (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Returns True if specified iterator is null (its internal stamp equals
   --  to zero).

   function Is_Valid
     (Self  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Stamp : Glib.Gint) return Boolean;
   --  Returns True if specified iterator is null iterator or its internal
   --  stamp is equal to the specified stamp.

   function Get_Stamp (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint;

   function Get_User_Data_1
     (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return System.Address;

   function Get_User_Data_2
     (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return System.Address;

   function Get_User_Data_3
     (Self : Gtk.Tree_Model.Gtk_Tree_Iter) return System.Address;

end Gtk.Tree_Model.Utils;
