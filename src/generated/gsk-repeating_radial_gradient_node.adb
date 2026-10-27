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

package body Gsk.Repeating_Radial_Gradient_Node is

   --------------------------------------------
   -- Gsk_Repeating_Radial_Gradient_Node_New --
   --------------------------------------------

   function Gsk_Repeating_Radial_Gradient_Node_New
     (Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Center      : not null access Graphene.Point.Graphene_Point_T;
      Hradius     : Interfaces.C.C_float;
      Vradius     : Interfaces.C.C_float;
      Start       : Interfaces.C.C_float;
      The_End     : Interfaces.C.C_float;
      Color_Stops : Gsk.Render_Node.Gsk_Color_Stop_Array)
   return Gsk_Repeating_Radial_Gradient_Node
   is
      Self : constant Gsk_Repeating_Radial_Gradient_Node := new Gsk_Repeating_Radial_Gradient_Node_Record;
   begin
      Gsk.Repeating_Radial_Gradient_Node.Initialize (Self, Bounds, Center, Hradius, Vradius, Start, The_End, Color_Stops);
      return Self;
   end Gsk_Repeating_Radial_Gradient_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self        : out Gsk_Repeating_Radial_Gradient_Node;
      Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Center      : not null access Graphene.Point.Graphene_Point_T;
      Hradius     : Interfaces.C.C_float;
      Vradius     : Interfaces.C.C_float;
      Start       : Interfaces.C.C_float;
      The_End     : Interfaces.C.C_float;
      Color_Stops : Gsk.Render_Node.Gsk_Color_Stop_Array) is
   begin
      Self := new Gsk_Repeating_Radial_Gradient_Node_Record;
      Gsk.Repeating_Radial_Gradient_Node.Initialize (Self, Bounds, Center, Hradius, Vradius, Start, The_End, Color_Stops);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : not null access Gsk_Repeating_Radial_Gradient_Node_Record'Class;
      Bounds      : not null access Graphene.Rect.Graphene_Rect_T;
      Center      : not null access Graphene.Point.Graphene_Point_T;
      Hradius     : Interfaces.C.C_float;
      Vradius     : Interfaces.C.C_float;
      Start       : Interfaces.C.C_float;
      The_End     : Interfaces.C.C_float;
      Color_Stops : Gsk.Render_Node.Gsk_Color_Stop_Array)
   is
      function Internal
        (Bounds        : access Graphene.Rect.Graphene_Rect_T;
         Center        : access Graphene.Point.Graphene_Point_T;
         Hradius       : Interfaces.C.C_float;
         Vradius       : Interfaces.C.C_float;
         Start         : Interfaces.C.C_float;
         The_End       : Interfaces.C.C_float;
         Color_Stops   : System.Address;
         N_Color_Stops : Gsize) return System.Address;
      pragma Import (C, Internal, "gsk_repeating_radial_gradient_node_new");
   begin
      if not Self.Is_Created then
         Set_Object
           (Self,
            Internal (Bounds, Center, Hradius, Vradius, Start, The_End, Color_Stops'Address, Gsize (Color_Stops'Length)));
      end if;
   end Initialize;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Repeating_Radial_Gradient_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Repeating_Radial_Gradient_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Repeating_Radial_Gradient_Node;
