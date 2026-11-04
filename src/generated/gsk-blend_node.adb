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

package body Gsk.Blend_Node is

   ------------------------
   -- Gsk_Blend_Node_New --
   ------------------------

   function Gsk_Blend_Node_New
      (Bottom     : Gsk.Render_Node.Gsk_Render_Node;
       Top        : Gsk.Render_Node.Gsk_Render_Node;
       Blend_Mode : Gsk.Enums.Gsk_Blend_Mode) return Gsk_Blend_Node
   is
      Self : constant Gsk_Blend_Node := new Gsk_Blend_Node_Record;
   begin
      Gsk.Blend_Node.Initialize (Self, Bottom, Top, Blend_Mode);
      return Self;
   end Gsk_Blend_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self       : out Gsk_Blend_Node;
       Bottom     : Gsk.Render_Node.Gsk_Render_Node;
       Top        : Gsk.Render_Node.Gsk_Render_Node;
       Blend_Mode : Gsk.Enums.Gsk_Blend_Mode)
   is
   begin
      Self := new Gsk_Blend_Node_Record;
      Gsk.Blend_Node.Initialize (Self, Bottom, Top, Blend_Mode);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self       : not null access Gsk_Blend_Node_Record'Class;
       Bottom     : Gsk.Render_Node.Gsk_Render_Node;
       Top        : Gsk.Render_Node.Gsk_Render_Node;
       Blend_Mode : Gsk.Enums.Gsk_Blend_Mode)
   is
      function Internal
         (Bottom     : System.Address;
          Top        : System.Address;
          Blend_Mode : Gsk.Enums.Gsk_Blend_Mode) return System.Address;
      pragma Import (C, Internal, "gsk_blend_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Bottom), Get_Object (Top), Blend_Mode));
      end if;
   end Initialize;

   --------------------
   -- Get_Blend_Mode --
   --------------------

   function Get_Blend_Mode
      (Self : Gsk_Blend_Node) return Gsk.Enums.Gsk_Blend_Mode
   is
      function Internal
         (Self : System.Address) return Gsk.Enums.Gsk_Blend_Mode;
      pragma Import (C, Internal, "gsk_blend_node_get_blend_mode");
   begin
      return Internal (Get_Object (Self));
   end Get_Blend_Mode;

   ----------------------
   -- Get_Bottom_Child --
   ----------------------

   function Get_Bottom_Child
      (Self : Gsk_Blend_Node) return Gsk.Render_Node.Gsk_Render_Node
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_blend_node_get_bottom_child");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Bottom_Child;

   -------------------
   -- Get_Top_Child --
   -------------------

   function Get_Top_Child
      (Self : Gsk_Blend_Node) return Gsk.Render_Node.Gsk_Render_Node
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_blend_node_get_top_child");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Top_Child;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Blend_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Blend_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Blend_Node;
