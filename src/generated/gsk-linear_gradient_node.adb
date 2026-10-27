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
with Interfaces.C.Pointers;

package body Gsk.Linear_Gradient_Node is

   ----------------------------------
   -- Gsk_Linear_Gradient_Node_New --
   ----------------------------------

   function Gsk_Linear_Gradient_Node_New
     (Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Start       : not null access Graphene.Point.Graphene_Point_T;
      The_End     : not null access Graphene.Point.Graphene_Point_T;
      Color_Stops : Gsk_Color_Stop_Array) return Gsk_Linear_Gradient_Node
   is
      Self : constant Gsk_Linear_Gradient_Node := new Gsk_Linear_Gradient_Node_Record;
   begin
      Gsk.Linear_Gradient_Node.Initialize (Self, Bounds, Start, The_End, Color_Stops);
      return Self;
   end Gsk_Linear_Gradient_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self        : out Gsk_Linear_Gradient_Node;
      Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Start       : not null access Graphene.Point.Graphene_Point_T;
      The_End     : not null access Graphene.Point.Graphene_Point_T;
      Color_Stops : Gsk_Color_Stop_Array)
   is
   begin
      Self := new Gsk_Linear_Gradient_Node_Record;
      Gsk.Linear_Gradient_Node.Initialize (Self, Bounds, Start, The_End, Color_Stops);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : not null access Gsk_Linear_Gradient_Node_Record'Class;
      Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Start       : not null access Graphene.Point.Graphene_Point_T;
      The_End     : not null access Graphene.Point.Graphene_Point_T;
      Color_Stops : Gsk_Color_Stop_Array)
   is
      function Internal
        (Bounds        : access Graphene.Rect.Graphene_Rect_T;
         Start         : access Graphene.Point.Graphene_Point_T;
         The_End       : access Graphene.Point.Graphene_Point_T;
         Color_Stops   : System.Address;
         N_Color_Stops : Gsize) return System.Address;
      pragma Import (C, Internal, "gsk_linear_gradient_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Bounds, Start, The_End, Color_Stops'Address, Gsize (Color_Stops'Length)));
      end if;
   end Initialize;

   ---------------------
   -- Get_Color_Stops --
   ---------------------

   function Get_Color_Stops
      (Self : Gsk_Linear_Gradient_Node)
       return Gsk.Render_Node.Gsk_Color_Stop_Array
   is
      use Interfaces.C;
      type Aliased_Array is array
        (Positive range <>) of aliased Gsk.Render_Node.Gsk_Color_Stop
        with Convention => C;
      package P is new Interfaces.C.Pointers
        (Positive, Gsk.Render_Node.Gsk_Color_Stop,
         Aliased_Array, (0.0, (others => 0.0)));

      function Internal
         (Self    : System.Address;
          N_Stops : access Gsize)
          return P.Pointer;
      pragma Import (C, Internal, "gsk_linear_gradient_node_get_color_stops");
      Acc_N_Stops : aliased Gsize;
      Tmp_Return  : P.Pointer;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_N_Stops'Access);
      declare
         Result : constant Gsk.Render_Node.Gsk_Color_Stop_Array :=
           Gsk.Render_Node.Gsk_Color_Stop_Array
             (P.Value (Tmp_Return, ptrdiff_t (Acc_N_Stops)));
      begin
         return Result;
      end;
   end Get_Color_Stops;

   -------------
   -- Get_End --
   -------------

   function Get_End
      (Self : Gsk_Linear_Gradient_Node)
       return access constant Graphene.Point.Graphene_Point_T
   is
      function Internal
         (Self : System.Address)
          return access constant Graphene.Point.Graphene_Point_T;
      pragma Import (C, Internal, "gsk_linear_gradient_node_get_end");
   begin
      return Internal (Get_Object (Self));
   end Get_End;

   -----------------------
   -- Get_N_Color_Stops --
   -----------------------

   function Get_N_Color_Stops (Self : Gsk_Linear_Gradient_Node) return Gsize is
      function Internal (Self : System.Address) return Gsize;
      pragma Import (C, Internal, "gsk_linear_gradient_node_get_n_color_stops");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Color_Stops;

   ---------------
   -- Get_Start --
   ---------------

   function Get_Start
      (Self : Gsk_Linear_Gradient_Node)
       return access constant Graphene.Point.Graphene_Point_T
   is
      function Internal
         (Self : System.Address)
          return access constant Graphene.Point.Graphene_Point_T;
      pragma Import (C, Internal, "gsk_linear_gradient_node_get_start");
   begin
      return Internal (Get_Object (Self));
   end Get_Start;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Linear_Gradient_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Linear_Gradient_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Linear_Gradient_Node;
