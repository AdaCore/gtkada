
with Gtk; use Gtk;
with System;

package Glib.Glist is

   generic
--      type Data_Type is limited private;
      type Gpointer is private; --  access all Data_Type;
   package Generic_List is

      type Glist is private;

      procedure Alloc (List : out Glist);
      --  mapping: Alloc glib.h g_list_alloc

      procedure Append (List : in out Glist;
                        Data : in Gpointer);
      --  mapping: Append glib.h g_list_append

      function Concat (List1 : in Glist;
                       List2 : in Glist)
                       return Glist;
      --  mapping: Concat glib.h g_list_concat

      procedure Insert (List : in out Glist;
                        Data : in Gpointer;
                        Position : in Gint);
      --  mapping: Insert glib.h g_list_insert

      function Find (List : in Glist;
                     Data : in Gpointer)
                     return Glist;
      --  mapping: Find glib.h g_list_find

      function First (List : in Glist)
                      return Glist;
      --  mapping: First glib.h g_list_first

      procedure Free (List : in out Glist);
      --  mapping: Free glib.h g_list_free

      function Get_Object (Obj : in Glist)
                           return System.Address;
      pragma Inline (Get_Object);

      function Index (List : in Glist;
                      Data : in Gpointer)
                      return Gint;
      --  mapping: Index glib.h g_list_index

      function Last (List : in Glist)
                     return Glist;
      --  mapping: Last glib.h g_list_last

      function Length (List : in Glist)
                       return Guint;
      --  mapping: Length glib.h g_list_length

      procedure List_Reverse (List : in out Glist);
      --  mapping: List_Reverse glib.h g_list_reverse

      function Nth (List : in Glist;
                    N    : in Guint)
                    return Glist;
      --  mapping: Nth glib.h g_list_nth

      function Nth_Data (List : in Glist;
                         N : in Guint)
                         return Gpointer;
      --  mapping: Nth_Data glib.h g_list_nth_data

      function Position (List : in Glist;
                         Link : in Glist)
                         return Gint;
      --  mapping: Position glib.h g_list_position

      procedure Prepend (List : in out Glist;
                         Data : in Gpointer);
      --  mapping: Prepend glib.h g_list_prepend

      procedure Remove (List : in out Glist;
                        Data : in Gpointer);
      --  mapping: Remove glib.h g_list_remove

      procedure Remove_Link (List : in out Glist;
                             Link : in Glist);
      --  mapping: Remove_Link glib.h g_list_remove_link

      procedure Set_Object (Obj    : in out Glist;
                            Value  : in     System.Address);
      pragma Inline (Set_Object);

   private

      type Glist is
         record
            Ptr : System.Address := System.Null_Address;
         end record;

   end Generic_List;

   --  mapping: NOT_IMPLEMENTED glib.h g_list_free_1
   --  mapping: NOT_IMPLEMENTED glib.h g_list_foreach
   --  mapping: NOT_IMPLEMENTED glib.h g_list_previous(list)

end Glib.Glist;
