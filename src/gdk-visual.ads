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

with Glib; use Glib;
with Glib.Glist;
with Gdk.Types;


package Gdk.Visual is

   type Gdk_Visual is new Root_Type with null record;
   --  This type is not private because we need the full declaration
   --  to instanciate Glib.Glist.Generic_List with it.

   Null_Visual : constant Gdk_Visual;

   type Gdk_Visual_Type_Array is array (Natural range <>)
     of Types.Gdk_Visual_Type;


   function Get_Best_Depth return Gint;

   function Get_Best_Type return Types.Gdk_Visual_Type;

   procedure Get_System (Visual : out Gdk_Visual);

   procedure Get_Best (Visual : out Gdk_Visual);

   procedure Get_Best (Visual :    out Gdk_Visual;
                       Depth  : in     Gint);

   procedure Get_Best (Visual      :    out Gdk_Visual;
                       Visual_Type : in     Types.Gdk_Visual_Type);

   procedure Get_Best (Visual      :    out Gdk_Visual;
                       Depth       : in     Gint;
                       Visual_Type : in     Types.Gdk_Visual_Type);

   function Query_Depths return Gint_Array;

   function Query_Visual_Types return Gdk_Visual_Type_Array;



   function Convert (V : in Gdk_Visual'Class) return System.Address;
   function Convert (V : in System.Address) return Gdk_Visual'Class;

   package Gdk_Visual_List is new Glib.Glist.Generic_List (Gdk_Visual'Class);

   function List_Visuals return Gdk_Visual_List.Glist;


private

   Null_Visual : constant Gdk_Visual := (Ptr => System.Null_Address);

   pragma Import (C, Get_Best_Depth, "gdk_visual_get_best_depth");
   pragma Import (C, Get_Best_Type, "gdk_visual_get_best_type");

end Gdk.Visual;
