------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
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
--
--  This packages provides the implementation of a generic single-linked
--  list.
--  One instantiation is found in Gtk.Widget.Widget_Slist for a list of
--  widgets.
--
--  See the documentation of Glib.Glist for more information, it provides
--  the same API as this package.
--  Single linked lists are traversed the same way as double-linked lists,
--  even though most subprograms are less efficient than their
--  double-linked counterparts.
--
--  </description>
--  <c_version>1.2.6</c_version>
--  <group>Glib, the general-purpose library</group>

with System;

package Glib.GSlist is

   --  <doc_ignore>

   generic
      type Gpointer (<>) is private;
      with function Convert (P : Gpointer) return System.Address is <>;
      with function Convert (S : System.Address) return Gpointer is <>;
   package Generic_SList is

      type GSlist is private;
      Null_List : constant GSlist;

      procedure Alloc (List : out GSlist);
      procedure Append (List : in out GSlist;
                        Data : Gpointer);
      function Concat (List1 : GSlist;
                       List2 : GSlist)
                       return GSlist;
      procedure Insert (List : in out GSlist;
                        Data : Gpointer;
                        Position : Gint);
      function Find (List : GSlist;
                     Data : Gpointer)
                     return GSlist;
      procedure Free (List : in out GSlist);
      function Get_Data (List : GSlist) return Gpointer;

      function Get_Data_Address (List : GSlist) return System.Address;
      --  Return directly the System.Address contained in the C list.
      --  This is used mainly internally in GtkAda to implement String lists,
      --  and you should not have to use this subprogram yourself.

      function Index (List : GSlist;
                      Data : Gpointer)
                      return Gint;
      function Last (List : GSlist)
                     return GSlist;
      function Length (List : GSlist)
                       return Guint;
      procedure List_Reverse (List : in out GSlist);
      function Next (List : GSlist)
                     return GSlist;
      function Nth (List : GSlist;
                    N    : Guint)
                    return GSlist;
      function Nth_Data (List : GSlist;
                         N : Guint)
                         return Gpointer;
      function Position (List : GSlist;
                         Link : GSlist)
                         return Gint;
      procedure Prepend (List : in out GSlist;
                         Data : Gpointer);
      procedure Remove (List : in out GSlist;
                        Data : Gpointer);
      procedure Remove_Link (List : in out GSlist;
                             Link : GSlist);
      function Get_Object (Obj : GSlist) return System.Address;
      pragma Inline (Get_Object);
      procedure Set_Object (Obj    : in out GSlist;
                            Value  :        System.Address);
      pragma Inline (Set_Object);
   private

      type GSlist is
         record
            Ptr : System.Address := System.Null_Address;
         end record;
      Null_List : constant GSlist := (Ptr => System.Null_Address);
   end Generic_SList;

   --  </doc_ignore>

end Glib.GSlist;
