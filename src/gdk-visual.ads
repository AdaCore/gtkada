-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
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
