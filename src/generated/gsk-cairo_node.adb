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

package body Gsk.Cairo_Node is

   ------------------------
   -- Gsk_Cairo_Node_New --
   ------------------------

   function Gsk_Cairo_Node_New
      (Bounds : not null access Graphene.Rect.Graphene_Rect_T)
       return Gsk_Cairo_Node
   is
      Self : constant Gsk_Cairo_Node := new Gsk_Cairo_Node_Record;
   begin
      Gsk.Cairo_Node.Initialize (Self, Bounds);
      return Self;
   end Gsk_Cairo_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gsk_Cairo_Node;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
   is
   begin
      Self := new Gsk_Cairo_Node_Record;
      Gsk.Cairo_Node.Initialize (Self, Bounds);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gsk_Cairo_Node_Record'Class;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
   is
      function Internal
         (Bounds : access Graphene.Rect.Graphene_Rect_T)
          return System.Address;
      pragma Import (C, Internal, "gsk_cairo_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Bounds));
      end if;
   end Initialize;

   ----------------------
   -- Get_Draw_Context --
   ----------------------

   function Get_Draw_Context
      (Self : Gsk_Cairo_Node) return Cairo.Cairo_Context
   is
      function Internal (Self : System.Address) return Cairo.Cairo_Context;
      pragma Import (C, Internal, "gsk_cairo_node_get_draw_context");
   begin
      return Internal (Get_Object (Self));
   end Get_Draw_Context;

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface (Self : Gsk_Cairo_Node) return Cairo.Cairo_Surface is
      function Internal (Self : System.Address) return Cairo.Cairo_Surface;
      pragma Import (C, Internal, "gsk_cairo_node_get_surface");
   begin
      return Internal (Get_Object (Self));
   end Get_Surface;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Cairo_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Cairo_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Cairo_Node;
