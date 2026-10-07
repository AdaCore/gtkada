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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Gsk.Transform is

   function From_Object_Free
     (B : access Gsk_Transform'Class) return Gsk_Transform
   is
      Result : constant Gsk_Transform := Gsk_Transform (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gsk_Transform is
      S : Gsk_Transform;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -----------------------
   -- Gsk_Transform_New --
   -----------------------

   function Gsk_Transform_New return Gsk_Transform is
      function Internal return System.Address;
      pragma Import (C, Internal, "gsk_transform_new");
      Self : Gsk_Transform;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Gsk_Transform_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gsk_Transform) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gsk_transform_new");
   begin
      Self.Set_Object (Internal);
   end Gtk_New;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self   : Gsk_Transform;
       Second : Gsk_Transform) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Second : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_transform_equal");
   begin
      return Internal (Get_Object (Self), Get_Object (Second)) /= 0;
   end Equal;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category
      (Self : Gsk_Transform) return Gsk.Enums.Gsk_Transform_Category
   is
      function Internal
         (Self : System.Address) return Gsk.Enums.Gsk_Transform_Category;
      pragma Import (C, Internal, "gsk_transform_get_category");
   begin
      return Internal (Get_Object (Self));
   end Get_Category;

   ------------
   -- Invert --
   ------------

   function Invert (Self : Gsk_Transform) return Gsk_Transform is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_transform_invert");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Invert;

   ------------
   -- Matrix --
   ------------

   function Matrix
      (Self   : Gsk_Transform;
       Matrix : not null access Graphene.Matrix.Graphene_Matrix_T)
       return Gsk_Transform
   is
      function Internal
         (Self   : System.Address;
          Matrix : access Graphene.Matrix.Graphene_Matrix_T)
          return System.Address;
      pragma Import (C, Internal, "gsk_transform_matrix");
   begin
      return From_Object (Internal (Get_Object (Self), Matrix));
   end Matrix;

   ---------------
   -- Matrix_2D --
   ---------------

   function Matrix_2D
      (Self : Gsk_Transform;
       Xx   : Interfaces.C.C_float;
       Yx   : Interfaces.C.C_float;
       Xy   : Interfaces.C.C_float;
       Yy   : Interfaces.C.C_float;
       Dx   : Interfaces.C.C_float;
       Dy   : Interfaces.C.C_float) return Gsk_Transform
   is
      function Internal
         (Self : System.Address;
          Xx   : Interfaces.C.C_float;
          Yx   : Interfaces.C.C_float;
          Xy   : Interfaces.C.C_float;
          Yy   : Interfaces.C.C_float;
          Dx   : Interfaces.C.C_float;
          Dy   : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_transform_matrix_2d");
   begin
      return From_Object (Internal (Get_Object (Self), Xx, Yx, Xy, Yy, Dx, Dy));
   end Matrix_2D;

   -----------------
   -- Perspective --
   -----------------

   function Perspective
      (Self  : Gsk_Transform;
       Depth : Interfaces.C.C_float) return Gsk_Transform
   is
      function Internal
         (Self  : System.Address;
          Depth : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_transform_perspective");
   begin
      return From_Object (Internal (Get_Object (Self), Depth));
   end Perspective;

   -----------
   -- Print --
   -----------

   procedure Print (Self : Gsk_Transform; String : Glib.String.Gstring) is
      procedure Internal
         (Self   : System.Address;
          String : Glib.String.Gstring);
      pragma Import (C, Internal, "gsk_transform_print");
   begin
      Internal (Get_Object (Self), String);
   end Print;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gsk_Transform) return Gsk_Transform is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_transform_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   ------------
   -- Rotate --
   ------------

   function Rotate
      (Self  : Gsk_Transform;
       Angle : Interfaces.C.C_float) return Gsk_Transform
   is
      function Internal
         (Self  : System.Address;
          Angle : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_transform_rotate");
   begin
      return From_Object (Internal (Get_Object (Self), Angle));
   end Rotate;

   ---------------
   -- Rotate_3D --
   ---------------

   function Rotate_3D
      (Self  : Gsk_Transform;
       Angle : Interfaces.C.C_float;
       Axis  : not null access Graphene.Vec3.Graphene_Vec3_T)
       return Gsk_Transform
   is
      function Internal
         (Self  : System.Address;
          Angle : Interfaces.C.C_float;
          Axis  : access Graphene.Vec3.Graphene_Vec3_T)
          return System.Address;
      pragma Import (C, Internal, "gsk_transform_rotate_3d");
   begin
      return From_Object (Internal (Get_Object (Self), Angle, Axis));
   end Rotate_3D;

   -----------
   -- Scale --
   -----------

   function Scale
      (Self     : Gsk_Transform;
       Factor_X : Interfaces.C.C_float;
       Factor_Y : Interfaces.C.C_float) return Gsk_Transform
   is
      function Internal
         (Self     : System.Address;
          Factor_X : Interfaces.C.C_float;
          Factor_Y : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_transform_scale");
   begin
      return From_Object (Internal (Get_Object (Self), Factor_X, Factor_Y));
   end Scale;

   --------------
   -- Scale_3D --
   --------------

   function Scale_3D
      (Self     : Gsk_Transform;
       Factor_X : Interfaces.C.C_float;
       Factor_Y : Interfaces.C.C_float;
       Factor_Z : Interfaces.C.C_float) return Gsk_Transform
   is
      function Internal
         (Self     : System.Address;
          Factor_X : Interfaces.C.C_float;
          Factor_Y : Interfaces.C.C_float;
          Factor_Z : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_transform_scale_3d");
   begin
      return From_Object (Internal (Get_Object (Self), Factor_X, Factor_Y, Factor_Z));
   end Scale_3D;

   ----------
   -- Skew --
   ----------

   function Skew
      (Self   : Gsk_Transform;
       Skew_X : Interfaces.C.C_float;
       Skew_Y : Interfaces.C.C_float) return Gsk_Transform
   is
      function Internal
         (Self   : System.Address;
          Skew_X : Interfaces.C.C_float;
          Skew_Y : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_transform_skew");
   begin
      return From_Object (Internal (Get_Object (Self), Skew_X, Skew_Y));
   end Skew;

   -----------
   -- To_2D --
   -----------

   procedure To_2D
      (Self   : Gsk_Transform;
       Out_Xx : out Interfaces.C.C_float;
       Out_Yx : out Interfaces.C.C_float;
       Out_Xy : out Interfaces.C.C_float;
       Out_Yy : out Interfaces.C.C_float;
       Out_Dx : out Interfaces.C.C_float;
       Out_Dy : out Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          Out_Xx : out Interfaces.C.C_float;
          Out_Yx : out Interfaces.C.C_float;
          Out_Xy : out Interfaces.C.C_float;
          Out_Yy : out Interfaces.C.C_float;
          Out_Dx : out Interfaces.C.C_float;
          Out_Dy : out Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_transform_to_2d");
   begin
      Internal (Get_Object (Self), Out_Xx, Out_Yx, Out_Xy, Out_Yy, Out_Dx, Out_Dy);
   end To_2D;

   ----------------------
   -- To_2D_Components --
   ----------------------

   procedure To_2D_Components
      (Self        : Gsk_Transform;
       Out_Skew_X  : out Interfaces.C.C_float;
       Out_Skew_Y  : out Interfaces.C.C_float;
       Out_Scale_X : out Interfaces.C.C_float;
       Out_Scale_Y : out Interfaces.C.C_float;
       Out_Angle   : out Interfaces.C.C_float;
       Out_Dx      : out Interfaces.C.C_float;
       Out_Dy      : out Interfaces.C.C_float)
   is
      procedure Internal
         (Self        : System.Address;
          Out_Skew_X  : out Interfaces.C.C_float;
          Out_Skew_Y  : out Interfaces.C.C_float;
          Out_Scale_X : out Interfaces.C.C_float;
          Out_Scale_Y : out Interfaces.C.C_float;
          Out_Angle   : out Interfaces.C.C_float;
          Out_Dx      : out Interfaces.C.C_float;
          Out_Dy      : out Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_transform_to_2d_components");
   begin
      Internal (Get_Object (Self), Out_Skew_X, Out_Skew_Y, Out_Scale_X, Out_Scale_Y, Out_Angle, Out_Dx, Out_Dy);
   end To_2D_Components;

   ---------------
   -- To_Affine --
   ---------------

   procedure To_Affine
      (Self        : Gsk_Transform;
       Out_Scale_X : out Interfaces.C.C_float;
       Out_Scale_Y : out Interfaces.C.C_float;
       Out_Dx      : out Interfaces.C.C_float;
       Out_Dy      : out Interfaces.C.C_float)
   is
      procedure Internal
         (Self        : System.Address;
          Out_Scale_X : out Interfaces.C.C_float;
          Out_Scale_Y : out Interfaces.C.C_float;
          Out_Dx      : out Interfaces.C.C_float;
          Out_Dy      : out Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_transform_to_affine");
   begin
      Internal (Get_Object (Self), Out_Scale_X, Out_Scale_Y, Out_Dx, Out_Dy);
   end To_Affine;

   ---------------
   -- To_Matrix --
   ---------------

   procedure To_Matrix
      (Self       : Gsk_Transform;
       Out_Matrix : not null access Graphene.Matrix.Graphene_Matrix_T)
   is
      procedure Internal
         (Self       : System.Address;
          Out_Matrix : not null access Graphene.Matrix.Graphene_Matrix_T);
      pragma Import (C, Internal, "gsk_transform_to_matrix");
   begin
      Internal (Get_Object (Self), Out_Matrix);
   end To_Matrix;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Gsk_Transform) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gsk_transform_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end To_String;

   ------------------
   -- To_Translate --
   ------------------

   procedure To_Translate
      (Self   : Gsk_Transform;
       Out_Dx : out Interfaces.C.C_float;
       Out_Dy : out Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          Out_Dx : out Interfaces.C.C_float;
          Out_Dy : out Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_transform_to_translate");
   begin
      Internal (Get_Object (Self), Out_Dx, Out_Dy);
   end To_Translate;

   ---------------
   -- Transform --
   ---------------

   function Transform
      (Self  : Gsk_Transform;
       Other : Gsk_Transform) return Gsk_Transform
   is
      function Internal
         (Self  : System.Address;
          Other : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_transform_transform");
   begin
      return From_Object (Internal (Get_Object (Self), Get_Object (Other)));
   end Transform;

   ----------------------
   -- Transform_Bounds --
   ----------------------

   procedure Transform_Bounds
      (Self     : Gsk_Transform;
       Rect     : not null access Graphene.Rect.Graphene_Rect_T;
       Out_Rect : not null access Graphene.Rect.Graphene_Rect_T)
   is
      procedure Internal
         (Self     : System.Address;
          Rect     : not null access Graphene.Rect.Graphene_Rect_T;
          Out_Rect : not null access Graphene.Rect.Graphene_Rect_T);
      pragma Import (C, Internal, "gsk_transform_transform_bounds");
   begin
      Internal (Get_Object (Self), Rect, Out_Rect);
   end Transform_Bounds;

   ---------------------
   -- Transform_Point --
   ---------------------

   procedure Transform_Point
      (Self      : Gsk_Transform;
       Point     : not null access Graphene.Point.Graphene_Point_T;
       Out_Point : not null access Graphene.Point.Graphene_Point_T)
   is
      procedure Internal
         (Self      : System.Address;
          Point     : not null access Graphene.Point.Graphene_Point_T;
          Out_Point : not null access Graphene.Point.Graphene_Point_T);
      pragma Import (C, Internal, "gsk_transform_transform_point");
   begin
      Internal (Get_Object (Self), Point, Out_Point);
   end Transform_Point;

   ---------------
   -- Translate --
   ---------------

   function Translate
      (Self  : Gsk_Transform;
       Point : not null access Graphene.Point.Graphene_Point_T)
       return Gsk_Transform
   is
      function Internal
         (Self  : System.Address;
          Point : access Graphene.Point.Graphene_Point_T)
          return System.Address;
      pragma Import (C, Internal, "gsk_transform_translate");
   begin
      return From_Object (Internal (Get_Object (Self), Point));
   end Translate;

   ------------------
   -- Translate_3D --
   ------------------

   function Translate_3D
      (Self  : Gsk_Transform;
       Point : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Gsk_Transform
   is
      function Internal
         (Self  : System.Address;
          Point : access Graphene.Point3d.Graphene_Point3D_T)
          return System.Address;
      pragma Import (C, Internal, "gsk_transform_translate_3d");
   begin
      return From_Object (Internal (Get_Object (Self), Point));
   end Translate_3D;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gsk_Transform) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gsk_transform_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

   -----------
   -- Parse --
   -----------

   function Parse
      (String        : UTF8_String;
       Out_Transform : out Gsk_Transform) return Boolean
   is
      function Internal
         (String            : Gtkada.Types.Chars_Ptr;
          Acc_Out_Transform : access System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_transform_parse");
      Acc_Out_Transform     : aliased Gsk_Transform;
      Tmp_String            : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Acc_Out_Transform : aliased System.Address;
      Tmp_Return            : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_String, Tmp_Acc_Out_Transform'Access);
      Acc_Out_Transform := From_Object (Tmp_Acc_Out_Transform);
      Out_Transform := Acc_Out_Transform;
      Free (Tmp_String);
      return Tmp_Return /= 0;
   end Parse;

end Gsk.Transform;
