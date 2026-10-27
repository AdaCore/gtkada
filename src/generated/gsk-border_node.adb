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

package body Gsk.Border_Node is

   -------------------------
   -- Gsk_Border_Node_New --
   -------------------------

   function Gsk_Border_Node_New
      (Outline      : Gsk.Rounded_Rect.Gsk_Rounded_Rect;
       Border_Width : Gfloat_Array4;
       Border_Color : Gdk_Rgba_Array4) return Gsk_Border_Node
   is
      Self : constant Gsk_Border_Node := new Gsk_Border_Node_Record;
   begin
      Gsk.Border_Node.Initialize (Self, Outline, Border_Width, Border_Color);
      return Self;
   end Gsk_Border_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self         : out Gsk_Border_Node;
       Outline      : Gsk.Rounded_Rect.Gsk_Rounded_Rect;
       Border_Width : Gfloat_Array4;
       Border_Color : Gdk_Rgba_Array4)
   is
   begin
      Self := new Gsk_Border_Node_Record;
      Gsk.Border_Node.Initialize (Self, Outline, Border_Width, Border_Color);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self         : not null access Gsk_Border_Node_Record'Class;
       Outline      : Gsk.Rounded_Rect.Gsk_Rounded_Rect;
       Border_Width : Gfloat_Array4;
       Border_Color : Gdk_Rgba_Array4)
   is
      function Internal
         (Outline      : Gsk.Rounded_Rect.Gsk_Rounded_Rect;
          Border_Width : Gfloat_Array4;
          Border_Color : Gdk_Rgba_Array4) return System.Address;
      pragma Import (C, Internal, "gsk_border_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Outline, Border_Width, Border_Color));
      end if;
   end Initialize;

   ----------------
   -- Get_Colors --
   ----------------

   function Get_Colors (Self : Gsk_Border_Node) return Gdk_Rgba_Array4 is
      use Interfaces.C;
      type Aliased_Gdk_Rgba_Array is
        array (Positive range <>) of aliased Gdk.RGBA.Gdk_RGBA
        with Convention => C;
      package P is new Interfaces.C.Pointers
        (Positive, Gdk.RGBA.Gdk_RGBA, Aliased_Gdk_Rgba_Array, (others => 0.0));

      function Internal (Self : System.Address) return P.Pointer;
      pragma Import (C, Internal, "gsk_border_node_get_colors");
      Acc_Result : P.Pointer := null;
      Result     : Gdk_Rgba_Array4;
   begin
      Acc_Result := Internal (Get_Object (Self));
      Result := Gdk_Rgba_Array4 (P.Value (Acc_Result, 4));
      return Result;
   end Get_Colors;

   -----------------
   -- Get_Outline --
   -----------------

   function Get_Outline
      (Self : Gsk_Border_Node) return Gsk.Rounded_Rect.Gsk_Rounded_Rect
   is
      function Internal
         (Self : System.Address)
          return access Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      pragma Import (C, Internal, "gsk_border_node_get_outline");
   begin
      return Internal (Get_Object (Self)).all;
   end Get_Outline;

   ----------------
   -- Get_Widths --
   ----------------

   function Get_Widths (Self : Gsk_Border_Node) return Gfloat_Array4 is
      use Interfaces.C;
      type Aliased_Gfloat_Array is
        array (Positive range <>) of aliased Gfloat
        with Convention => C;
      package P is new Interfaces.C.Pointers
        (Positive, Gfloat, Aliased_Gfloat_Array, 0.0);

      function Internal (Self : System.Address) return P.Pointer;
      pragma Import (C, Internal, "gsk_border_node_get_widths");
      Acc_Result : P.Pointer := null;
      Result     : Gfloat_Array4;
   begin
      Acc_Result := Internal (Get_Object (Self));
      Result := Gfloat_Array4 (P.Value (Acc_Result, 4));
      return Result;
   end Get_Widths;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Border_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Border_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Border_Node;
