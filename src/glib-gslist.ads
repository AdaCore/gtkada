-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------


with System;

package Glib.GSlist is

   generic
      type Gpointer (<>) is private;
      with function Convert (P : Gpointer) return System.Address is <>;
      with function Convert (S : System.Address) return Gpointer is <>;
   package Generic_SList is

      type GSlist is private;
      Null_List : constant GSlist;

      procedure Alloc (List : out GSlist);
      procedure Append (List : in out GSlist;
                        Data : in Gpointer);
      function Concat (List1 : in GSlist;
                       List2 : in GSlist)
                       return GSlist;
      procedure Insert (List : in out GSlist;
                        Data : in Gpointer;
                        Position : in Gint);
      function Find (List : in GSlist;
                     Data : in Gpointer)
                     return GSlist;
      procedure Free (List : in out GSlist);
      function Get_Data (List : in GSlist)
                         return Gpointer;
      function Index (List : in GSlist;
                      Data : in Gpointer)
                      return Gint;
      function Last (List : in GSlist)
                     return GSlist;
      function Length (List : in GSlist)
                       return Guint;
      procedure List_Reverse (List : in out GSlist);
      function Next (List : in GSlist)
                     return GSlist;
      function Nth (List : in GSlist;
                    N    : in Guint)
                    return GSlist;
      function Nth_Data (List : in GSlist;
                         N : in Guint)
                         return Gpointer;
      function Position (List : in GSlist;
                         Link : in GSlist)
                         return Gint;
      procedure Prepend (List : in out GSlist;
                         Data : in Gpointer);
      procedure Remove (List : in out GSlist;
                        Data : in Gpointer);
      procedure Remove_Link (List : in out GSlist;
                             Link : in GSlist);
      function Get_Object (Obj : in GSlist)
                           return System.Address;
      pragma Inline (Get_Object);
      procedure Set_Object (Obj    : in out GSlist;
                            Value  : in     System.Address);
      pragma Inline (Set_Object);
   private

      type GSlist is
         record
            Ptr : System.Address := System.Null_Address;
         end record;
      Null_List : constant GSlist := (Ptr => System.Null_Address);
   end Generic_SList;
   --  mapping: Alloc glib.h g_slist_alloc
   --  mapping: Append glib.h g_slist_append
   --  mapping: Concat glib.h g_slist_concat
   --  mapping: Find glib.h g_slist_find
   --  mapping: First glib.h g_slist_first
   --  mapping: Free glib.h g_slist_free
   --  mapping: Index glib.h g_slist_index
   --  mapping: Insert glib.h g_slist_insert
   --  mapping: Last glib.h g_slist_last
   --  mapping: Length glib.h g_slist_length
   --  mapping: List_Reverse glib.h g_slist_reverse
   --  mapping: NOT_IMPLEMENTED glib.h g_slist_foreach
   --  mapping: NOT_IMPLEMENTED glib.h g_slist_free_1
   --  mapping: NOT_IMPLEMENTED glib.h g_slist_previous(list)
   --  mapping: Nth glib.h g_slist_nth
   --  mapping: Nth_Data glib.h g_slist_nth_data
   --  mapping: Position glib.h g_slist_position
   --  mapping: Prepend glib.h g_slist_prepend
   --  mapping: Remove glib.h g_slist_remove
   --  mapping: Remove_Link glib.h g_slist_remove_link

end Glib.GSlist;




