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
with Glib.Object; use Glib.Object;

package body Gsk.Path_Builder is

   function From_Object_Free
     (B : access Gsk_Path_Builder'Class) return Gsk_Path_Builder
   is
      Result : constant Gsk_Path_Builder := Gsk_Path_Builder (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gsk_Path_Builder is
      S : Gsk_Path_Builder;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   procedure Add_Cairo_Path (Self : Gsk_Path_Builder; Path : Cairo.Cairo_Path_Access) is
      procedure Internal (Self : System.Address; Path : Cairo.Cairo_Path_Access);
      pragma Import (C, Internal, "gsk_path_builder_add_cairo_path");
   begin
      Internal (Get_Object (Self), Path);
   end Add_Cairo_Path;

   function Get_Current_Point (Self : Gsk_Path_Builder)
   return Graphene.Point.Graphene_Point_T is
      function Internal (Self : System.Address)
      return access constant Graphene.Point.Graphene_Point_T;
      pragma Import (C, Internal, "gsk_path_builder_get_current_point");
   begin
      return Internal (Get_Object (Self)).all;
   end Get_Current_Point;

   --------------------------
   -- Gsk_Path_Builder_New --
   --------------------------

   function Gsk_Path_Builder_New return Gsk_Path_Builder is
      function Internal return System.Address;
      pragma Import (C, Internal, "gsk_path_builder_new");
      Self : Gsk_Path_Builder;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Gsk_Path_Builder_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gsk_Path_Builder) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gsk_path_builder_new");
   begin
      Self.Set_Object (Internal);
   end Gtk_New;

   ----------------
   -- Add_Circle --
   ----------------

   procedure Add_Circle
      (Self   : Gsk_Path_Builder;
       Center : not null access Graphene.Point.Graphene_Point_T;
       Radius : Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          Center : not null access Graphene.Point.Graphene_Point_T;
          Radius : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_add_circle");
   begin
      Internal (Get_Object (Self), Center, Radius);
   end Add_Circle;

   ----------------
   -- Add_Layout --
   ----------------

   procedure Add_Layout
      (Self   : Gsk_Path_Builder;
       Layout : not null access Pango.Layout.Pango_Layout_Record'Class)
   is
      procedure Internal (Self : System.Address; Layout : System.Address);
      pragma Import (C, Internal, "gsk_path_builder_add_layout");
   begin
      Internal (Get_Object (Self), Get_Object (Layout));
   end Add_Layout;

   --------------
   -- Add_Path --
   --------------

   procedure Add_Path (Self : Gsk_Path_Builder; Path : Gsk.Path.Gsk_Path) is
      procedure Internal (Self : System.Address; Path : System.Address);
      pragma Import (C, Internal, "gsk_path_builder_add_path");
   begin
      Internal (Get_Object (Self), Get_Object (Path));
   end Add_Path;

   --------------
   -- Add_Rect --
   --------------

   procedure Add_Rect
      (Self : Gsk_Path_Builder;
       Rect : not null access Graphene.Rect.Graphene_Rect_T)
   is
      procedure Internal
         (Self : System.Address;
          Rect : not null access Graphene.Rect.Graphene_Rect_T);
      pragma Import (C, Internal, "gsk_path_builder_add_rect");
   begin
      Internal (Get_Object (Self), Rect);
   end Add_Rect;

   ----------------------
   -- Add_Reverse_Path --
   ----------------------

   procedure Add_Reverse_Path
      (Self : Gsk_Path_Builder;
       Path : Gsk.Path.Gsk_Path)
   is
      procedure Internal (Self : System.Address; Path : System.Address);
      pragma Import (C, Internal, "gsk_path_builder_add_reverse_path");
   begin
      Internal (Get_Object (Self), Get_Object (Path));
   end Add_Reverse_Path;

   -----------------
   -- Add_Segment --
   -----------------

   procedure Add_Segment
      (Self    : Gsk_Path_Builder;
       Path    : Gsk.Path.Gsk_Path;
       Start   : not null access Gsk.Path_Point.Gsk_Path_Point;
       The_End : not null access Gsk.Path_Point.Gsk_Path_Point)
   is
      procedure Internal
         (Self    : System.Address;
          Path    : System.Address;
          Start   : not null access Gsk.Path_Point.Gsk_Path_Point;
          The_End : not null access Gsk.Path_Point.Gsk_Path_Point);
      pragma Import (C, Internal, "gsk_path_builder_add_segment");
   begin
      Internal (Get_Object (Self), Get_Object (Path), Start, The_End);
   end Add_Segment;

   ------------
   -- Arc_To --
   ------------

   procedure Arc_To
      (Self : Gsk_Path_Builder;
       X1   : Interfaces.C.C_float;
       Y1   : Interfaces.C.C_float;
       X2   : Interfaces.C.C_float;
       Y2   : Interfaces.C.C_float)
   is
      procedure Internal
         (Self : System.Address;
          X1   : Interfaces.C.C_float;
          Y1   : Interfaces.C.C_float;
          X2   : Interfaces.C.C_float;
          Y2   : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_arc_to");
   begin
      Internal (Get_Object (Self), X1, Y1, X2, Y2);
   end Arc_To;

   -----------
   -- Close --
   -----------

   procedure Close (Self : Gsk_Path_Builder) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gsk_path_builder_close");
   begin
      Internal (Get_Object (Self));
   end Close;

   --------------
   -- Conic_To --
   --------------

   procedure Conic_To
      (Self   : Gsk_Path_Builder;
       X1     : Interfaces.C.C_float;
       Y1     : Interfaces.C.C_float;
       X2     : Interfaces.C.C_float;
       Y2     : Interfaces.C.C_float;
       Weight : Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          X1     : Interfaces.C.C_float;
          Y1     : Interfaces.C.C_float;
          X2     : Interfaces.C.C_float;
          Y2     : Interfaces.C.C_float;
          Weight : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_conic_to");
   begin
      Internal (Get_Object (Self), X1, Y1, X2, Y2, Weight);
   end Conic_To;

   --------------
   -- Cubic_To --
   --------------

   procedure Cubic_To
      (Self : Gsk_Path_Builder;
       X1   : Interfaces.C.C_float;
       Y1   : Interfaces.C.C_float;
       X2   : Interfaces.C.C_float;
       Y2   : Interfaces.C.C_float;
       X3   : Interfaces.C.C_float;
       Y3   : Interfaces.C.C_float)
   is
      procedure Internal
         (Self : System.Address;
          X1   : Interfaces.C.C_float;
          Y1   : Interfaces.C.C_float;
          X2   : Interfaces.C.C_float;
          Y2   : Interfaces.C.C_float;
          X3   : Interfaces.C.C_float;
          Y3   : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_cubic_to");
   begin
      Internal (Get_Object (Self), X1, Y1, X2, Y2, X3, Y3);
   end Cubic_To;

   -----------------
   -- Html_Arc_To --
   -----------------

   procedure Html_Arc_To
      (Self   : Gsk_Path_Builder;
       X1     : Interfaces.C.C_float;
       Y1     : Interfaces.C.C_float;
       X2     : Interfaces.C.C_float;
       Y2     : Interfaces.C.C_float;
       Radius : Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          X1     : Interfaces.C.C_float;
          Y1     : Interfaces.C.C_float;
          X2     : Interfaces.C.C_float;
          Y2     : Interfaces.C.C_float;
          Radius : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_html_arc_to");
   begin
      Internal (Get_Object (Self), X1, Y1, X2, Y2, Radius);
   end Html_Arc_To;

   -------------
   -- Line_To --
   -------------

   procedure Line_To
      (Self : Gsk_Path_Builder;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float)
   is
      procedure Internal
         (Self : System.Address;
          X    : Interfaces.C.C_float;
          Y    : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_line_to");
   begin
      Internal (Get_Object (Self), X, Y);
   end Line_To;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
      (Self : Gsk_Path_Builder;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float)
   is
      procedure Internal
         (Self : System.Address;
          X    : Interfaces.C.C_float;
          Y    : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_move_to");
   begin
      Internal (Get_Object (Self), X, Y);
   end Move_To;

   -------------
   -- Quad_To --
   -------------

   procedure Quad_To
      (Self : Gsk_Path_Builder;
       X1   : Interfaces.C.C_float;
       Y1   : Interfaces.C.C_float;
       X2   : Interfaces.C.C_float;
       Y2   : Interfaces.C.C_float)
   is
      procedure Internal
         (Self : System.Address;
          X1   : Interfaces.C.C_float;
          Y1   : Interfaces.C.C_float;
          X2   : Interfaces.C.C_float;
          Y2   : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_quad_to");
   begin
      Internal (Get_Object (Self), X1, Y1, X2, Y2);
   end Quad_To;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gsk_Path_Builder) return Gsk_Path_Builder is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_path_builder_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   ----------------
   -- Rel_Arc_To --
   ----------------

   procedure Rel_Arc_To
      (Self : Gsk_Path_Builder;
       X1   : Interfaces.C.C_float;
       Y1   : Interfaces.C.C_float;
       X2   : Interfaces.C.C_float;
       Y2   : Interfaces.C.C_float)
   is
      procedure Internal
         (Self : System.Address;
          X1   : Interfaces.C.C_float;
          Y1   : Interfaces.C.C_float;
          X2   : Interfaces.C.C_float;
          Y2   : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_rel_arc_to");
   begin
      Internal (Get_Object (Self), X1, Y1, X2, Y2);
   end Rel_Arc_To;

   ------------------
   -- Rel_Conic_To --
   ------------------

   procedure Rel_Conic_To
      (Self   : Gsk_Path_Builder;
       X1     : Interfaces.C.C_float;
       Y1     : Interfaces.C.C_float;
       X2     : Interfaces.C.C_float;
       Y2     : Interfaces.C.C_float;
       Weight : Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          X1     : Interfaces.C.C_float;
          Y1     : Interfaces.C.C_float;
          X2     : Interfaces.C.C_float;
          Y2     : Interfaces.C.C_float;
          Weight : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_rel_conic_to");
   begin
      Internal (Get_Object (Self), X1, Y1, X2, Y2, Weight);
   end Rel_Conic_To;

   ------------------
   -- Rel_Cubic_To --
   ------------------

   procedure Rel_Cubic_To
      (Self : Gsk_Path_Builder;
       X1   : Interfaces.C.C_float;
       Y1   : Interfaces.C.C_float;
       X2   : Interfaces.C.C_float;
       Y2   : Interfaces.C.C_float;
       X3   : Interfaces.C.C_float;
       Y3   : Interfaces.C.C_float)
   is
      procedure Internal
         (Self : System.Address;
          X1   : Interfaces.C.C_float;
          Y1   : Interfaces.C.C_float;
          X2   : Interfaces.C.C_float;
          Y2   : Interfaces.C.C_float;
          X3   : Interfaces.C.C_float;
          Y3   : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_rel_cubic_to");
   begin
      Internal (Get_Object (Self), X1, Y1, X2, Y2, X3, Y3);
   end Rel_Cubic_To;

   ---------------------
   -- Rel_Html_Arc_To --
   ---------------------

   procedure Rel_Html_Arc_To
      (Self   : Gsk_Path_Builder;
       X1     : Interfaces.C.C_float;
       Y1     : Interfaces.C.C_float;
       X2     : Interfaces.C.C_float;
       Y2     : Interfaces.C.C_float;
       Radius : Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          X1     : Interfaces.C.C_float;
          Y1     : Interfaces.C.C_float;
          X2     : Interfaces.C.C_float;
          Y2     : Interfaces.C.C_float;
          Radius : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_rel_html_arc_to");
   begin
      Internal (Get_Object (Self), X1, Y1, X2, Y2, Radius);
   end Rel_Html_Arc_To;

   -----------------
   -- Rel_Line_To --
   -----------------

   procedure Rel_Line_To
      (Self : Gsk_Path_Builder;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float)
   is
      procedure Internal
         (Self : System.Address;
          X    : Interfaces.C.C_float;
          Y    : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_rel_line_to");
   begin
      Internal (Get_Object (Self), X, Y);
   end Rel_Line_To;

   -----------------
   -- Rel_Move_To --
   -----------------

   procedure Rel_Move_To
      (Self : Gsk_Path_Builder;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float)
   is
      procedure Internal
         (Self : System.Address;
          X    : Interfaces.C.C_float;
          Y    : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_rel_move_to");
   begin
      Internal (Get_Object (Self), X, Y);
   end Rel_Move_To;

   -----------------
   -- Rel_Quad_To --
   -----------------

   procedure Rel_Quad_To
      (Self : Gsk_Path_Builder;
       X1   : Interfaces.C.C_float;
       Y1   : Interfaces.C.C_float;
       X2   : Interfaces.C.C_float;
       Y2   : Interfaces.C.C_float)
   is
      procedure Internal
         (Self : System.Address;
          X1   : Interfaces.C.C_float;
          Y1   : Interfaces.C.C_float;
          X2   : Interfaces.C.C_float;
          Y2   : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_rel_quad_to");
   begin
      Internal (Get_Object (Self), X1, Y1, X2, Y2);
   end Rel_Quad_To;

   --------------------
   -- Rel_Svg_Arc_To --
   --------------------

   procedure Rel_Svg_Arc_To
      (Self            : Gsk_Path_Builder;
       Rx              : Interfaces.C.C_float;
       Ry              : Interfaces.C.C_float;
       X_Axis_Rotation : Interfaces.C.C_float;
       Large_Arc       : Boolean;
       Positive_Sweep  : Boolean;
       X               : Interfaces.C.C_float;
       Y               : Interfaces.C.C_float)
   is
      procedure Internal
         (Self            : System.Address;
          Rx              : Interfaces.C.C_float;
          Ry              : Interfaces.C.C_float;
          X_Axis_Rotation : Interfaces.C.C_float;
          Large_Arc       : Glib.Gboolean;
          Positive_Sweep  : Glib.Gboolean;
          X               : Interfaces.C.C_float;
          Y               : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_rel_svg_arc_to");
   begin
      Internal (Get_Object (Self), Rx, Ry, X_Axis_Rotation, Boolean'Pos (Large_Arc), Boolean'Pos (Positive_Sweep), X, Y);
   end Rel_Svg_Arc_To;

   ----------------
   -- Svg_Arc_To --
   ----------------

   procedure Svg_Arc_To
      (Self            : Gsk_Path_Builder;
       Rx              : Interfaces.C.C_float;
       Ry              : Interfaces.C.C_float;
       X_Axis_Rotation : Interfaces.C.C_float;
       Large_Arc       : Boolean;
       Positive_Sweep  : Boolean;
       X               : Interfaces.C.C_float;
       Y               : Interfaces.C.C_float)
   is
      procedure Internal
         (Self            : System.Address;
          Rx              : Interfaces.C.C_float;
          Ry              : Interfaces.C.C_float;
          X_Axis_Rotation : Interfaces.C.C_float;
          Large_Arc       : Glib.Gboolean;
          Positive_Sweep  : Glib.Gboolean;
          X               : Interfaces.C.C_float;
          Y               : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_path_builder_svg_arc_to");
   begin
      Internal (Get_Object (Self), Rx, Ry, X_Axis_Rotation, Boolean'Pos (Large_Arc), Boolean'Pos (Positive_Sweep), X, Y);
   end Svg_Arc_To;

   -------------
   -- To_Path --
   -------------

   function To_Path (Self : Gsk_Path_Builder) return Gsk.Path.Gsk_Path is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_path_builder_to_path");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end To_Path;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gsk_Path_Builder) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gsk_path_builder_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gsk.Path_Builder;
