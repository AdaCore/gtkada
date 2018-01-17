------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
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

with Interfaces.C.Pointers;

package body Gdk.Visual is

   type Aliased_Gint_Array is array (Natural range <>) of aliased Gint;

   package Gint_Ptr is new Interfaces.C.Pointers
     (Index => Natural, Element => Gint, Element_Array => Aliased_Gint_Array,
      Default_Terminator => 0);

   type Aliased_Visual_Type_Array is array (Natural range <>)
     of aliased Gdk_Visual_Type;

   package Visual_Type_Ptr is new Interfaces.C.Pointers
     (Index => Natural, Element => Gdk_Visual_Type,
      Element_Array => Aliased_Visual_Type_Array,
      Default_Terminator => Visual_Static_Gray);

   --------------
   -- Get_Best --
   --------------

   function Get_Best return Gdk_Visual is
      function Internal return Gdk_Visual;
      pragma Import (C, Internal, "gdk_visual_get_best");
   begin
      return Internal;
   end Get_Best;

   procedure Get_Best (Visual : out Gdk_Visual) is
   begin
      Visual := Get_Best;
   end Get_Best;

   function Get_Best (Depth : Gint) return Gdk_Visual is
      function Internal (Depth : Gint) return Gdk_Visual;
      pragma Import (C, Internal, "gdk_visual_get_best_with_depth");

   begin
      return Internal (Depth);
   end Get_Best;

   function Get_Best (Visual_Type : Gdk_Visual_Type) return Gdk_Visual is
      function Internal (Visual_Type : Gdk_Visual_Type) return Gdk_Visual;
      pragma Import (C, Internal, "gdk_visual_get_best_with_type");

   begin
      return Internal (Visual_Type);
   end Get_Best;

   function Get_Best
     (Depth : Gint; Visual_Type : Gdk_Visual_Type) return Gdk_Visual
   is
      function Internal
        (Depth : Gint; Visual_Type : Gdk_Visual_Type) return Gdk_Visual;
      pragma Import (C, Internal, "gdk_visual_get_best_with_both");

   begin
      return Internal (Depth, Visual_Type);
   end Get_Best;

   ------------------
   -- List_Visuals --
   ------------------

   function List_Visuals return Gdk_Visual_List.Glist is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_list_visuals");
      Result : Gdk_Visual_List.Glist;
   begin
      Gdk_Visual_List.Set_Object (Result, Internal);
      return Result;
   end List_Visuals;

   ------------------
   -- Query_Depths --
   ------------------

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

   ------------------------
   -- Query_Visual_Types --
   ------------------------

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
