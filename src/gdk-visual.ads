with Glib; use Glib;
with Glib.Glist;
with Gdk.Types;


package Gdk.Visual is

   type Gdk_Visual is new Root_Type with null record;
   --  This type is not private because we need the full declaration
   --  to instanciate Glib.Glist.Generic_List with it.

   type Gdk_Visual_Type_Array is array (Natural range <>)
     of Types.Gdk_Visual_Type;


   function Get_Best_Depth return Gint;
   --  mapping: Get_Best_Depth gdk.h gdk_visual_get_best_depth

   function Get_Best_Type return Types.Gdk_Visual_Type;
   --  mapping: Get_Best_Type gdk.h gdk_visual_get_best_type

   procedure Get_System (Visual : out Gdk_Visual'Class);
   --  mapping: Get_System gdk.h gdk_visual_get_system

   procedure Get_Best (Visual : out Gdk_Visual'Class);
   --  mapping: Get_Best gdk.h gdk_visual_get_best

   procedure Get_Best (Visual :    out Gdk_Visual'Class;
                       Depth  : in     Gint);
   --  mapping: Get_Best gdk.h gdk_visual_get_best_with_depth

   procedure Get_Best (Visual      :    out Gdk_Visual'Class;
                       Visual_Type : in     Types.Gdk_Visual_Type);
   --  mapping: Get_Best gdk.h gdk_visual_get_best_with_type

   procedure Get_Best (Visual      :    out Gdk_Visual'Class;
                       Depth       : in     Gint;
                       Visual_Type : in     Types.Gdk_Visual_Type);
   --  mapping: Get_Best gdk.h gdk_visual_get_best_with_both

   function Query_Depths return Gint_Array;
   --  mapping: Query_Depths gdk.h gdk_query_depths

   function Query_Visual_Types return Gdk_Visual_Type_Array;
   --  mapping: Query_Visual_Types gdk.h gdk_query_visual_types



   function Convert (V : in Gdk_Visual'Class) return System.Address;
   function Convert (V : in System.Address) return Gdk_Visual'Class;

   package Gdk_Visual_List is new Glib.Glist.Generic_List (Gdk_Visual'Class);

   function List_Visuals return Gdk_Visual_List.Glist;
   --  mapping: List_Visuals gdk.h gdk_list_visuals


private

   pragma Import (C, Get_Best_Depth, "gdk_visual_get_best_depth");
   pragma Import (C, Get_Best_Type, "gdk_visual_get_best_type");

   --  probably not needed.
   --
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_visual_ref
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_visual_unref

end Gdk.Visual;
