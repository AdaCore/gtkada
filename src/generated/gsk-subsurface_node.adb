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

package body Gsk.Subsurface_Node is

   -----------------------------
   -- Gsk_Subsurface_Node_New --
   -----------------------------

   function Gsk_Subsurface_Node_New
      (Child      : Gsk.Render_Node.Gsk_Render_Node;
       Subsurface : System.Address) return Gsk_Subsurface_Node
   is
      Self : constant Gsk_Subsurface_Node := new Gsk_Subsurface_Node_Record;
   begin
      Gsk.Subsurface_Node.Initialize (Self, Child, Subsurface);
      return Self;
   end Gsk_Subsurface_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self       : out Gsk_Subsurface_Node;
       Child      : Gsk.Render_Node.Gsk_Render_Node;
       Subsurface : System.Address)
   is
   begin
      Self := new Gsk_Subsurface_Node_Record;
      Gsk.Subsurface_Node.Initialize (Self, Child, Subsurface);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self       : not null access Gsk_Subsurface_Node_Record'Class;
       Child      : Gsk.Render_Node.Gsk_Render_Node;
       Subsurface : System.Address)
   is
      function Internal
         (Child      : System.Address;
          Subsurface : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_subsurface_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Child), Subsurface));
      end if;
   end Initialize;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : Gsk_Subsurface_Node) return Gsk.Render_Node.Gsk_Render_Node
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_subsurface_node_get_child");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Child;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Subsurface_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Subsurface_Node_Record;
   begin
      return Result;
   end Create;

   --------------------
   -- Get_Subsurface --
   --------------------

   function Get_Subsurface
      (Node : Gsk_Subsurface_Node) return System.Address
   is
      function Internal (Node : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_subsurface_node_get_subsurface");
   begin
      return Internal (Get_Object (Node));
   end Get_Subsurface;

end Gsk.Subsurface_Node;
