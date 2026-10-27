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

package body Gsk.Inset_Shadow_Node is

   -------------------------------
   -- Gsk_Inset_Shadow_Node_New --
   -------------------------------

   function Gsk_Inset_Shadow_Node_New
      (Outline     : not null access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
       Color       : not null access Gdk.RGBA.Gdk_RGBA;
       Dx          : Interfaces.C.C_float;
       Dy          : Interfaces.C.C_float;
       Spread      : Interfaces.C.C_float;
       Blur_Radius : Interfaces.C.C_float) return Gsk_Inset_Shadow_Node
   is
      Self : constant Gsk_Inset_Shadow_Node := new Gsk_Inset_Shadow_Node_Record;
   begin
      Gsk.Inset_Shadow_Node.Initialize (Self, Outline, Color, Dx, Dy, Spread, Blur_Radius);
      return Self;
   end Gsk_Inset_Shadow_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self        : out Gsk_Inset_Shadow_Node;
       Outline     : not null access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
       Color       : not null access Gdk.RGBA.Gdk_RGBA;
       Dx          : Interfaces.C.C_float;
       Dy          : Interfaces.C.C_float;
       Spread      : Interfaces.C.C_float;
       Blur_Radius : Interfaces.C.C_float)
   is
   begin
      Self := new Gsk_Inset_Shadow_Node_Record;
      Gsk.Inset_Shadow_Node.Initialize (Self, Outline, Color, Dx, Dy, Spread, Blur_Radius);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self        : not null access Gsk_Inset_Shadow_Node_Record'Class;
       Outline     : not null access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
       Color       : not null access Gdk.RGBA.Gdk_RGBA;
       Dx          : Interfaces.C.C_float;
       Dy          : Interfaces.C.C_float;
       Spread      : Interfaces.C.C_float;
       Blur_Radius : Interfaces.C.C_float)
   is
      function Internal
         (Outline     : access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
          Color       : access Gdk.RGBA.Gdk_RGBA;
          Dx          : Interfaces.C.C_float;
          Dy          : Interfaces.C.C_float;
          Spread      : Interfaces.C.C_float;
          Blur_Radius : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_inset_shadow_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Outline, Color, Dx, Dy, Spread, Blur_Radius));
      end if;
   end Initialize;

   ---------------------
   -- Get_Blur_Radius --
   ---------------------

   function Get_Blur_Radius
      (Self : Gsk_Inset_Shadow_Node) return Interfaces.C.C_float
   is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_inset_shadow_node_get_blur_radius");
   begin
      return Internal (Get_Object (Self));
   end Get_Blur_Radius;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color
      (Self : Gsk_Inset_Shadow_Node)
       return access constant Gdk.RGBA.Gdk_RGBA
   is
      function Internal
         (Self : System.Address) return access constant Gdk.RGBA.Gdk_RGBA;
      pragma Import (C, Internal, "gsk_inset_shadow_node_get_color");
   begin
      return Internal (Get_Object (Self));
   end Get_Color;

   ------------
   -- Get_Dx --
   ------------

   function Get_Dx
      (Self : Gsk_Inset_Shadow_Node) return Interfaces.C.C_float
   is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_inset_shadow_node_get_dx");
   begin
      return Internal (Get_Object (Self));
   end Get_Dx;

   ------------
   -- Get_Dy --
   ------------

   function Get_Dy
      (Self : Gsk_Inset_Shadow_Node) return Interfaces.C.C_float
   is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_inset_shadow_node_get_dy");
   begin
      return Internal (Get_Object (Self));
   end Get_Dy;

   -----------------
   -- Get_Outline --
   -----------------

   function Get_Outline
      (Self : Gsk_Inset_Shadow_Node)
       return Gsk.Rounded_Rect.Gsk_Rounded_Rect
   is
      function Internal
         (Self : System.Address)
          return access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      pragma Import (C, Internal, "gsk_inset_shadow_node_get_outline");
   begin
      return Internal (Get_Object (Self)).all;
   end Get_Outline;

   ----------------
   -- Get_Spread --
   ----------------

   function Get_Spread
      (Self : Gsk_Inset_Shadow_Node) return Interfaces.C.C_float
   is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_inset_shadow_node_get_spread");
   begin
      return Internal (Get_Object (Self));
   end Get_Spread;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Inset_Shadow_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Inset_Shadow_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Inset_Shadow_Node;
