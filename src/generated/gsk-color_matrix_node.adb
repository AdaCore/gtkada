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

package body Gsk.Color_Matrix_Node is

   -------------------------------
   -- Gsk_Color_Matrix_Node_New --
   -------------------------------

   function Gsk_Color_Matrix_Node_New
      (Child        : Gsk.Render_Node.Gsk_Render_Node;
       Color_Matrix : not null access Graphene.Matrix.Graphene_Matrix_T;
       Color_Offset : not null access Graphene.Vec4.Graphene_Vec4_T)
       return Gsk_Color_Matrix_Node
   is
      Self : constant Gsk_Color_Matrix_Node := new Gsk_Color_Matrix_Node_Record;
   begin
      Gsk.Color_Matrix_Node.Initialize (Self, Child, Color_Matrix, Color_Offset);
      return Self;
   end Gsk_Color_Matrix_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self         : out Gsk_Color_Matrix_Node;
       Child        : Gsk.Render_Node.Gsk_Render_Node;
       Color_Matrix : not null access Graphene.Matrix.Graphene_Matrix_T;
       Color_Offset : not null access Graphene.Vec4.Graphene_Vec4_T)
   is
   begin
      Self := new Gsk_Color_Matrix_Node_Record;
      Gsk.Color_Matrix_Node.Initialize (Self, Child, Color_Matrix, Color_Offset);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self         : not null access Gsk_Color_Matrix_Node_Record'Class;
       Child        : Gsk.Render_Node.Gsk_Render_Node;
       Color_Matrix : not null access Graphene.Matrix.Graphene_Matrix_T;
       Color_Offset : not null access Graphene.Vec4.Graphene_Vec4_T)
   is
      function Internal
         (Child        : System.Address;
          Color_Matrix : access Graphene.Matrix.Graphene_Matrix_T;
          Color_Offset : access Graphene.Vec4.Graphene_Vec4_T)
          return System.Address;
      pragma Import (C, Internal, "gsk_color_matrix_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Child), Color_Matrix, Color_Offset));
      end if;
   end Initialize;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : Gsk_Color_Matrix_Node) return Gsk.Render_Node.Gsk_Render_Node
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_color_matrix_node_get_child");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Child;

   ----------------------
   -- Get_Color_Matrix --
   ----------------------

   function Get_Color_Matrix
      (Self : Gsk_Color_Matrix_Node)
       return access constant Graphene.Matrix.Graphene_Matrix_T
   is
      function Internal
         (Self : System.Address)
          return access constant Graphene.Matrix.Graphene_Matrix_T;
      pragma Import (C, Internal, "gsk_color_matrix_node_get_color_matrix");
   begin
      return Internal (Get_Object (Self));
   end Get_Color_Matrix;

   ----------------------
   -- Get_Color_Offset --
   ----------------------

   function Get_Color_Offset
      (Self : Gsk_Color_Matrix_Node)
       return access constant Graphene.Vec4.Graphene_Vec4_T
   is
      function Internal
         (Self : System.Address)
          return access constant Graphene.Vec4.Graphene_Vec4_T;
      pragma Import (C, Internal, "gsk_color_matrix_node_get_color_offset");
   begin
      return Internal (Get_Object (Self));
   end Get_Color_Offset;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Color_Matrix_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Color_Matrix_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Color_Matrix_Node;
