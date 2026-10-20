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

--  Collects the parameters that are needed when stroking a path.
--
--  <gtkada_demo>create_paths.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;        use Cairo;
with Glib;         use Glib;
with Gsk.Enums;    use Gsk.Enums;
with Interfaces.C; use Interfaces.C;

package Gsk.Stroke is

   type Gsk_Stroke is new Glib.C_Boxed with null record;
   Null_Gsk_Stroke : constant Gsk_Stroke;

   function From_Object (Object : System.Address) return Gsk_Stroke;
   function From_Object_Free (B : access Gsk_Stroke'Class) return Gsk_Stroke;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gsk_Stroke;
       Line_Width : Interfaces.C.C_float);
   --  Creates a new `GskStroke` with the given Line_Width.
   --  Since: gtk+ 4.14
   --  @param Line_Width line width of the stroke. Must be > 0

   function Gsk_Stroke_New
      (Line_Width : Interfaces.C.C_float) return Gsk_Stroke;
   --  Creates a new `GskStroke` with the given Line_Width.
   --  Since: gtk+ 4.14
   --  @param Line_Width line width of the stroke. Must be > 0

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_stroke_get_type");

   -------------
   -- Methods --
   -------------

   function Copy (Self : Gsk_Stroke) return Gsk_Stroke;
   --  Creates a copy of a `GskStroke`.
   --  Since: gtk+ 4.14
   --  @return a new `GskStroke`. Use [methodGsk.Stroke.free] to free it. Has
   --  transfer-ownership='full'.

   procedure Free (Self : Gsk_Stroke);
   --  Frees a `GskStroke`.
   --  Since: gtk+ 4.14

   function Get_Dash_Offset (Self : Gsk_Stroke) return Interfaces.C.C_float;
   --  Gets the dash offset.
   --  Since: gtk+ 4.14
   --  @return the dash offset

   procedure Set_Dash_Offset
      (Self   : Gsk_Stroke;
       Offset : Interfaces.C.C_float);
   --  Sets the offset into the dash pattern where dashing should begin.
   --  This is an offset into the length of the path, not an index into the
   --  array values of the dash array.
   --  See [methodGsk.Stroke.set_dash] for more details on dashing.
   --  Since: gtk+ 4.14
   --  @param Offset offset into the dash pattern

   function Get_Line_Cap (Self : Gsk_Stroke) return Gsk.Enums.Gsk_Line_Cap;
   --  Gets the line cap used.
   --  See [enumGsk.LineCap] for details.
   --  Since: gtk+ 4.14
   --  @return the line cap

   procedure Set_Line_Cap
      (Self     : Gsk_Stroke;
       Line_Cap : Gsk.Enums.Gsk_Line_Cap);
   --  Sets the line cap to be used when stroking.
   --  See [enumGsk.LineCap] for details.
   --  Since: gtk+ 4.14
   --  @param Line_Cap the line cap

   function Get_Line_Join (Self : Gsk_Stroke) return Gsk.Enums.Gsk_Line_Join;
   --  Gets the line join used.
   --  See [enumGsk.LineJoin] for details.
   --  Since: gtk+ 4.14
   --  @return the line join

   procedure Set_Line_Join
      (Self      : Gsk_Stroke;
       Line_Join : Gsk.Enums.Gsk_Line_Join);
   --  Sets the line join to be used when stroking.
   --  See [enumGsk.LineJoin] for details.
   --  Since: gtk+ 4.14
   --  @param Line_Join the line join to use

   function Get_Line_Width (Self : Gsk_Stroke) return Interfaces.C.C_float;
   --  Gets the line width used.
   --  Since: gtk+ 4.14
   --  @return the line width

   procedure Set_Line_Width
      (Self       : Gsk_Stroke;
       Line_Width : Interfaces.C.C_float);
   --  Sets the line width to be used when stroking.
   --  The line width must be >= 0.
   --  Since: gtk+ 4.14
   --  @param Line_Width width of the line in pixels

   function Get_Miter_Limit (Self : Gsk_Stroke) return Interfaces.C.C_float;
   --  Gets the miter limit.
   --  Since: gtk+ 4.14
   --  @return the miter limit

   procedure Set_Miter_Limit
      (Self  : Gsk_Stroke;
       Limit : Interfaces.C.C_float);
   --  Sets the miter limit to be used when stroking.
   --  The miter limit is the distance from the corner where sharp turns of
   --  joins get cut off.
   --  The limit is specfied in units of line width and must be non-negative.
   --  For joins of type [enumGsk.LineJoin.miter] that exceed the miter limit,
   --  the join gets rendered as if it was of type [enumGsk.LineJoin.bevel].
   --  Since: gtk+ 4.14
   --  @param Limit the miter limit

   procedure To_Cairo (Self : Gsk_Stroke; Cr : Cairo.Cairo_Context);
   --  A helper function that sets the stroke parameters of a cairo context
   --  from a `GskStroke`.
   --  Since: gtk+ 4.14
   --  @param Cr the cairo context to configure

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Dash_Array is array (Natural range <>) of Interfaces.C.C_float;
   pragma Convention (C, Dash_Array);

   procedure Set_Dash (Self : Gsk_Stroke; Dash : Dash_Array);

   function Get_Dash (Self : Gsk_Stroke) return Dash_Array;
   --  Returns a copy; it stays valid after modifying or freeing Self.

   ---------------
   -- Functions --
   ---------------

   function Equal
      (Stroke1 : Gsk_Stroke;
       Stroke2 : Gsk_Stroke) return Boolean;
   --  Checks if two strokes are identical.
   --  Since: gtk+ 4.14
   --  @param Stroke1 the first stroke
   --  @param Stroke2 the second stroke
   --  @return true if the two strokes are equal, false otherwise

private
   Null_Gsk_Stroke : constant Gsk_Stroke :=
      (Glib.C_Boxed with null record);

end Gsk.Stroke;
