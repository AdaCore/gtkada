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

package body Gdk.Rectangle is

   ---------------
   -- Intersect --
   ---------------

   procedure Intersect (Src1      : in     Gdk_Rectangle;
                        Src2      : in     Gdk_Rectangle;
                        Dest      :    out Gdk_Rectangle;
                        Intersect :    out Boolean) is
      function Internal (Src1, Src2, Dest : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_rectangle_intersect");
   begin
      Intersect := To_Boolean (Internal (Src1'Address,
                                         Src2'Address,
                                         Dest'Address));
   end Intersect;

   -------------
   --  Union  --
   -------------

   procedure Union (Src1 : in     Gdk_Rectangle;
                    Src2 : in     Gdk_Rectangle;
                    Dest :    out Gdk_Rectangle) is
      procedure Internal (Src1, Src2 : in System.Address;
                          Dest       : in System.Address);
      pragma Import (C, Internal, "gdk_rectangle_union");
   begin
      Internal (Src1'Address, Src2'Address, Dest'Address);
   end Union;


   ----------------------------------------------------------------------

   function Ada_Gdk_Rectangle_Size return Guint;
   pragma Import (C, Ada_Gdk_Rectangle_Size);
begin
   pragma Assert (Gdk_Rectangle'Size / 8 = Ada_Gdk_Rectangle_Size);
   null;
end Gdk.Rectangle;
