
with System;

package Glib.Glist is

   generic
      type Gpointer is private;
   package Generic_List is

      type Glist is private;

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
      function Index (List : in Glist;
                      Data : in Gpointer)
                      return Gint;
      function Last (List : in Glist)
                     return Glist;
      function Length (List : in Glist)
                       return Guint;
      procedure List_Reverse (List : in out Glist);
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
      procedure Remove (List : in out Glist;
                        Data : in Gpointer);
      procedure Remove_Link (List : in out Glist;
                             Link : in Glist);
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
   end Generic_List;

   --  mapping: Alloc glib.h g_list_alloc
   --  mapping: Append glib.h g_list_append
   --  mapping: Concat glib.h g_list_concat
   --  mapping: Find glib.h g_list_find
   --  mapping: First glib.h g_list_first
   --  mapping: Free glib.h g_list_free
   --  mapping: Index glib.h g_list_index
   --  mapping: Insert glib.h g_list_insert
   --  mapping: Last glib.h g_list_last
   --  mapping: Length glib.h g_list_length
   --  mapping: List_Reverse glib.h g_list_reverse
   --  mapping: NOT_IMPLEMENTED glib.h g_list_foreach
   --  mapping: NOT_IMPLEMENTED glib.h g_list_free_1
   --  mapping: NOT_IMPLEMENTED glib.h g_list_previous(list)
   --  mapping: Nth glib.h g_list_nth
   --  mapping: Nth_Data glib.h g_list_nth_data
   --  mapping: Position glib.h g_list_position
   --  mapping: Prepend glib.h g_list_prepend
   --  mapping: Remove glib.h g_list_remove
   --  mapping: Remove_Link glib.h g_list_remove_link

end Glib.Glist;
