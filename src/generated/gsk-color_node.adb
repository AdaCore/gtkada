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

package body Gsk.Color_Node is

   ------------------------
   -- Gsk_Color_Node_New --
   ------------------------

   function Gsk_Color_Node_New
      (Rgba   : not null access Gdk.RGBA.Gdk_RGBA;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
       return Gsk_Color_Node
   is
      Self : constant Gsk_Color_Node := new Gsk_Color_Node_Record;
   begin
      Gsk.Color_Node.Initialize (Self, Rgba, Bounds);
      return Self;
   end Gsk_Color_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gsk_Color_Node;
       Rgba   : not null access Gdk.RGBA.Gdk_RGBA;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
   is
   begin
      Self := new Gsk_Color_Node_Record;
      Gsk.Color_Node.Initialize (Self, Rgba, Bounds);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gsk_Color_Node_Record'Class;
       Rgba   : not null access Gdk.RGBA.Gdk_RGBA;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
   is
      function Internal
         (Rgba   : access Gdk.RGBA.Gdk_RGBA;
          Bounds : access Graphene.Rect.Graphene_Rect_T)
          return System.Address;
      pragma Import (C, Internal, "gsk_color_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Rgba, Bounds));
      end if;
   end Initialize;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color
      (Self : Gsk_Color_Node) return access constant Gdk.RGBA.Gdk_RGBA
   is
      function Internal
         (Self : System.Address) return access constant Gdk.RGBA.Gdk_RGBA;
      pragma Import (C, Internal, "gsk_color_node_get_color");
   begin
      return Internal (Get_Object (Self));
   end Get_Color;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Color_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Color_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Color_Node;
