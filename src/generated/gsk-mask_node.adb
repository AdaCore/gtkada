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

package body Gsk.Mask_Node is

   -----------------------
   -- Gsk_Mask_Node_New --
   -----------------------

   function Gsk_Mask_Node_New
      (Source    : Gsk.Render_Node.Gsk_Render_Node;
       Mask      : Gsk.Render_Node.Gsk_Render_Node;
       Mask_Mode : Gsk.Enums.Gsk_Mask_Mode) return Gsk_Mask_Node
   is
      Self : constant Gsk_Mask_Node := new Gsk_Mask_Node_Record;
   begin
      Gsk.Mask_Node.Initialize (Self, Source, Mask, Mask_Mode);
      return Self;
   end Gsk_Mask_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self      : out Gsk_Mask_Node;
       Source    : Gsk.Render_Node.Gsk_Render_Node;
       Mask      : Gsk.Render_Node.Gsk_Render_Node;
       Mask_Mode : Gsk.Enums.Gsk_Mask_Mode)
   is
   begin
      Self := new Gsk_Mask_Node_Record;
      Gsk.Mask_Node.Initialize (Self, Source, Mask, Mask_Mode);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self      : not null access Gsk_Mask_Node_Record'Class;
       Source    : Gsk.Render_Node.Gsk_Render_Node;
       Mask      : Gsk.Render_Node.Gsk_Render_Node;
       Mask_Mode : Gsk.Enums.Gsk_Mask_Mode)
   is
      function Internal
         (Source    : System.Address;
          Mask      : System.Address;
          Mask_Mode : Gsk.Enums.Gsk_Mask_Mode) return System.Address;
      pragma Import (C, Internal, "gsk_mask_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Source), Get_Object (Mask), Mask_Mode));
      end if;
   end Initialize;

   --------------
   -- Get_Mask --
   --------------

   function Get_Mask
      (Self : Gsk_Mask_Node) return Gsk.Render_Node.Gsk_Render_Node
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_mask_node_get_mask");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Mask;

   -------------------
   -- Get_Mask_Mode --
   -------------------

   function Get_Mask_Mode
      (Self : Gsk_Mask_Node) return Gsk.Enums.Gsk_Mask_Mode
   is
      function Internal
         (Self : System.Address) return Gsk.Enums.Gsk_Mask_Mode;
      pragma Import (C, Internal, "gsk_mask_node_get_mask_mode");
   begin
      return Internal (Get_Object (Self));
   end Get_Mask_Mode;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source
      (Self : Gsk_Mask_Node) return Gsk.Render_Node.Gsk_Render_Node
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_mask_node_get_source");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Source;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Mask_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Mask_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Mask_Node;
