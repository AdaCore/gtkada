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

with Interfaces.C.Pointers;

package body Gdk.Visual is


   type Aliased_Gint_Array is array (Natural range <>) of aliased Gint;

   package Gint_Ptr is new Interfaces.C.Pointers
     (Index => Natural, Element => Gint, Element_Array => Aliased_Gint_Array,
      Default_Terminator => 0);


   type Aliased_Visual_Type_Array is array (Natural range <>)
     of aliased Types.Gdk_Visual_Type;

   package Visual_Type_Ptr is new Interfaces.C.Pointers
     (Index => Natural, Element => Types.Gdk_Visual_Type,
      Element_Array => Aliased_Visual_Type_Array,
      Default_Terminator => Types.Static_Gray);


   ----------------
   --  Get_Best  --
   ----------------

   procedure Get_Best (Visual : out Gdk_Visual'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_visual_get_system");
   begin
      Set_Object (Visual, Internal);
   end Get_Best;


   ----------------
   --  Get_Best  --
   ----------------

   procedure Get_Best (Visual :    out Gdk_Visual'Class;
                       Depth  : in     Gint) is
      function Internal (Depth : in Gint) return System.Address;
      pragma Import (C, Internal, "gdk_visual_get_best_with_depth");
   begin
      Set_Object (Visual, Internal (Depth));
   end Get_Best;


   ----------------
   --  Get_Best  --
   ----------------

   procedure Get_Best (Visual      :    out Gdk_Visual'Class;
                       Visual_Type : in     Types.Gdk_Visual_Type) is
      function Internal (Visual_Type : in Types.Gdk_Visual_Type)
                         return System.Address;
      pragma Import (C, Internal, "gdk_visual_get_best_with_type");
   begin
      Set_Object (Visual, Internal (Visual_Type));
   end Get_Best;


   ----------------
   --  Get_Best  --
   ----------------

   procedure Get_Best (Visual      :    out Gdk_Visual'Class;
                       Depth       : in     Gint;
                       Visual_Type : in     Types.Gdk_Visual_Type) is
      function Internal (Depth       : in Gint;
                         Visual_Type : in Types.Gdk_Visual_Type)
                         return System.Address;
      pragma Import (C, Internal, "gdk_visual_get_best_with_both");
   begin
      Set_Object (Visual, Internal (Depth, Visual_Type));
   end Get_Best;


   ------------------
   --  Get_System  --
   ------------------

   procedure Get_System (Visual : out Gdk_Visual'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_visual_get_system");
   begin
      Set_Object (Visual, Internal);
   end Get_System;


   --------------------
   --  List_Visuals  --
   --------------------

   function Convert (V : in Gdk_Visual'Class) return System.Address is
   begin
      return Get_Object (V);
   end Convert;

   function Convert (V : in System.Address) return Gdk_Visual'Class is
      Result : Gdk_Visual;
   begin
      Set_Object (Result, V);
      return Result;
   end Convert;

   function List_Visuals return Gdk_Visual_List.Glist is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_list_visuals");
      Result : Gdk_Visual_List.Glist;
   begin
      Gdk_Visual_List.Set_Object (Result, Internal);
      return Result;
   end List_Visuals;


   --------------------
   --  Query_Depths  --
   --------------------

   function Query_Depths return Gint_Array is
      procedure Internal (Depths : out Gint_Ptr.Pointer; Count : out Gint);
      pragma Import (C, Internal, "gdk_query_depths");

      Internal_Result : Gint_Ptr.Pointer;
      Count : Gint;

   begin

      Internal (Internal_Result, Count);

      declare
         Temp : constant Aliased_Gint_Array :=
           Gint_Ptr.Value (Ref => Internal_Result,
                           Length => Interfaces.C.ptrdiff_t (Count));
         Result : Gint_Array (1 .. Temp'Length);
      begin

         for Index in Temp'Range loop
            Result (Result'First + Index - Temp'First) := Temp (Index);
         end loop;

         return Result;

      end;

   end Query_Depths;


   --------------------------
   --  Query_Visual_Types  --
   --------------------------

   function Query_Visual_Types return Gdk_Visual_Type_Array is
      procedure Internal (Visual_Type : out Visual_Type_Ptr.Pointer;
                          Count : out Gint);
      pragma Import (C, Internal, "gdk_query_visual_types");

      Internal_Result : Visual_Type_Ptr.Pointer;
      Count : Gint;

   begin

      Internal (Internal_Result, Count);

      declare
         Temp : constant Aliased_Visual_Type_Array :=
           Visual_Type_Ptr.Value (Ref => Internal_Result,
                                  Length => Interfaces.C.ptrdiff_t (Count));
         Result : Gdk_Visual_Type_Array (1 .. Temp'Length);
      begin

         for Index in Temp'Range loop
            Result (Result'First + Index - Temp'First) := Temp (Index);
         end loop;

         return Result;

      end;

   end Query_Visual_Types;


end Gdk.Visual;
