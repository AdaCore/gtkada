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

package body Gsk.Paste_Node is

   ------------------------
   -- Gsk_Paste_Node_New --
   ------------------------

   function Gsk_Paste_Node_New
      (Bounds : not null access Graphene.Rect.Graphene_Rect_T;
       Depth  : Gsize) return Gsk_Paste_Node
   is
      Self : constant Gsk_Paste_Node := new Gsk_Paste_Node_Record;
   begin
      Gsk.Paste_Node.Initialize (Self, Bounds, Depth);
      return Self;
   end Gsk_Paste_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gsk_Paste_Node;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T;
       Depth  : Gsize)
   is
   begin
      Self := new Gsk_Paste_Node_Record;
      Gsk.Paste_Node.Initialize (Self, Bounds, Depth);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gsk_Paste_Node_Record'Class;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T;
       Depth  : Gsize)
   is
      function Internal
         (Bounds : access Graphene.Rect.Graphene_Rect_T;
          Depth  : Gsize) return System.Address;
      pragma Import (C, Internal, "gsk_paste_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Bounds, Depth));
      end if;
   end Initialize;

   ---------------
   -- Get_Depth --
   ---------------

   function Get_Depth (Self : Gsk_Paste_Node) return Gsize is
      function Internal (Self : System.Address) return Gsize;
      pragma Import (C, Internal, "gsk_paste_node_get_depth");
   begin
      return Internal (Get_Object (Self));
   end Get_Depth;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Paste_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Paste_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Paste_Node;
