
with Gtk; use Gtk;

generic
   type Data_Type is private;
package Glib.GList is

   type GList is private;
   type Gpointer is access all Data_Type;

   procedure Alloc (List : out GList);
   --  mapping: Alloc glib.h g_list_alloc

   procedure Append (List : in out GList;
                     Data : in Gpointer);
   --  mapping: Append glib.h g_list_append

   function Concat (List1 : in GList;
                    List2 : in GList)
                    return GList;
   --  mapping: Concat glib.h g_list_concat

   procedure Insert (List : in out GList;
                     Data : in Gpointer;
                     Position : in GInt);
   --  mapping: Insert glib.h g_list_insert

   function Find (List : in GList;
                  Data : in Gpointer)
                  return GList;
   --  mapping: Find glib.h g_list_find

   function First (List : in GList)
                   return GList;
   --  mapping: First glib.h g_list_first

   procedure Free (List : in out GList);
   --  mapping: Free glib.h g_list_free

   function Index (List : in GList;
                   Data : in Gpointer)
                   return GInt;
   --  mapping: Index glib.h g_list_index

   function Last (List : in GList)
                  return GList;
   --  mapping: Last glib.h g_list_last

   function Length (List : in GList)
                    return GUint;
   --  mapping: Length glib.h g_list_length

   function Nth (List : in GList;
                 N    : in GUint)
                 return GList;
   --  mapping: Nth glib.h g_list_nth

   function Nth_Data (List : in GList;
                      N : in GUint)
                      return Gpointer;
   --  mapping: Nth_Data glib.h g_list_nth_data

   function Position (List : in GList;
                      Link : in GList)
                      return GInt;
   --  mapping: Position glib.h g_list_position

   procedure Prepend (List : in out GList;
                      Data : in Gpointer);
   --  mapping: Prepend glib.h g_list_prepend

   procedure Remove (List : in out GList;
                     Data : in Gpointer);
   --  mapping: Remove glib.h g_list_remove

   procedure Remove_Link (List : in out GList;
                          Link : in GList);
   --  mapping: Remove_Link glib.h g_list_remove_link

   procedure List_Reverse (List : in out GList);
   --  mapping: List_Reverse glib.h g_list_reverse

   --  mapping: NOT_IMPLEMENTED glib.h g_list_free_1
   --  mapping: NOT_IMPLEMENTED glib.h g_list_foreach
   --  mapping: NOT_IMPLEMENTED glib.h g_list_previous(list)

private

   type GList is tagged
      record
         Ptr : System.Address := System.Null_Address;
      end record;

   function Get_Object (Obj : in Object'Class)
                        return System.Address;
   pragma Inline (Get_Object);

   procedure Set_Object (Obj    : in out Object'Class;
                         Value  : in     System.Address);
   pragma Inline (Set_Object);

end Glib.GList;
