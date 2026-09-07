------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  A size.

pragma Warnings (Off, "*is already use-visible*");
with Glib;         use Glib;
with Interfaces.C; use Interfaces.C;

package Graphene.Size is

   type Graphene_Size_T is record
      Width : Interfaces.C.C_float;
      Height : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Graphene_Size_T);

   function From_Object_Free (B : access Graphene_Size_T) return Graphene_Size_T;
   pragma Inline (From_Object_Free);
   --  A size.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_size_get_type");

   -------------
   -- Methods --
   -------------

   function Equal
      (Self : not null access Graphene_Size_T;
       B    : not null access Graphene_Size_T) return Boolean;
   --  Checks whether the two give Graphene.Size.Graphene_Size_T are equal.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Size.Graphene_Size_T
   --  @return `true` if the sizes are equal

   procedure Free (Self : not null access Graphene_Size_T);
   pragma Import (C, Free, "graphene_size_free");
   --  Frees the resources allocated by graphene_size_alloc.
   --  Since: gtk+ 1.0

   function Init
      (Self   : not null access Graphene_Size_T;
       Width  : Interfaces.C.C_float;
       Height : Interfaces.C.C_float) return access Graphene_Size_T;
   pragma Import (C, Init, "graphene_size_init");
   --  Initializes a Graphene.Size.Graphene_Size_T using the given Width and
   --  Height.
   --  Since: gtk+ 1.0
   --  @param Width the width
   --  @param Height the height
   --  @return the initialized Graphene.Size.Graphene_Size_T

   function Init_From_Size
      (Self : not null access Graphene_Size_T;
       Src  : not null access Graphene_Size_T) return access Graphene_Size_T;
   pragma Import (C, Init_From_Size, "graphene_size_init_from_size");
   --  Initializes a Graphene.Size.Graphene_Size_T using the width and height
   --  of the given Src.
   --  Since: gtk+ 1.0
   --  @param Src a Graphene.Size.Graphene_Size_T
   --  @return the initialized Graphene.Size.Graphene_Size_T

   procedure Interpolate
      (Self   : not null access Graphene_Size_T;
       B      : not null access Graphene_Size_T;
       Factor : Gdouble;
       Res    : not null access Graphene_Size_T);
   pragma Import (C, Interpolate, "graphene_size_interpolate");
   --  Linearly interpolates the two given Graphene.Size.Graphene_Size_T using
   --  the given interpolation Factor.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Size.Graphene_Size_T
   --  @param Factor the linear interpolation factor
   --  @param Res return location for the interpolated size

   procedure Scale
      (Self   : not null access Graphene_Size_T;
       Factor : Interfaces.C.C_float;
       Res    : not null access Graphene_Size_T);
   pragma Import (C, Scale, "graphene_size_scale");
   --  Scales the components of a Graphene.Size.Graphene_Size_T using the
   --  given Factor.
   --  Since: gtk+ 1.0
   --  @param Factor the scaling factor
   --  @param Res return location for the scaled size

   ---------------
   -- Functions --
   ---------------

   function Zero return access constant Graphene_Size_T;
   pragma Import (C, Zero, "graphene_size_zero");
   --  A constant pointer to a zero Graphene.Size.Graphene_Size_T, useful for
   --  equality checks and interpolations.
   --  Since: gtk+ 1.0
   --  @return a constant size

end Graphene.Size;
