-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------


with System;

package Glib.Glist is

   generic
      type Gpointer (<>) is private;
      with function Convert (P : Gpointer) return System.Address is <>;
      with function Convert (S : System.Address) return Gpointer is <>;
   package Generic_List is

      type Glist is private;
      Null_List : constant Glist;

      procedure Alloc (List : out Glist);
      procedure Append (List : in out Glist;
                        Data : in Gpointer);
      function Concat (List1 : in Glist;
                       List2 : in Glist)
                       return Glist;
      procedure Insert (List : in out Glist;
                        Data : in Gpointer;
                        Position : in Gint);
      function Find (List : in Glist;
                     Data : in Gpointer)
                     return Glist;
      function First (List : in Glist)
                      return Glist;
      procedure Free (List : in out Glist);

      function Get_Data (List : in Glist) return Gpointer;
      function Get_Data_Address (List : in Glist) return System.Address;
      --  These two functions are basically the same, except that the first
      --  one first converts the System.Address to Gpointer.
      --  The second one is only useful if you need access directly to the
      --  C pointer (e.g for String lists)

      function Get_Gpointer (List : in Glist) return Gpointer;
      --
      --  Sometimes, the data is not stored in the "data" field
      --  of each cell, but rather at each cell. In such cases,
      --  to retrieve the address of the data, we need to return
      --  the address of the cell itself, insted of the address
      --  pointed to by data.
      --
      --  Ex: the GtkCTree row_list.

      function Index (List : in Glist;
                      Data : in Gpointer)
                      return Gint;
      function Last (List : in Glist)
                     return Glist;
      function Length (List : in Glist)
                       return Guint;
      procedure List_Reverse (List : in out Glist);
      function Next (List : in Glist)
                     return Glist;
      function Nth (List : in Glist;
                    N    : in Guint)
                    return Glist;
      function Nth_Data (List : in Glist;
                         N : in Guint)
                         return Gpointer;
      function Position (List : in Glist;
                         Link : in Glist)
                         return Gint;
      procedure Prepend (List : in out Glist;
                         Data : in Gpointer);
      function Prev (List : in Glist)
                     return Glist;
      procedure Remove (List : in out Glist;
                        Data : in Gpointer);
      procedure Remove_Link (List : in out Glist;
                             Link : in Glist);


      function Is_Created (List : in Glist) return Boolean;

      --  For Internal use only.
      --
      function Get_Object (Obj : in Glist)
                           return System.Address;
      pragma Inline (Get_Object);

      procedure Set_Object (Obj    : in out Glist;
                            Value  : in     System.Address);
      pragma Inline (Set_Object);

   private

      type Glist is
         record
            Ptr : System.Address := System.Null_Address;
         end record;
      Null_List : constant Glist := (Ptr => System.Null_Address);
   end Generic_List;

end Glib.Glist;




