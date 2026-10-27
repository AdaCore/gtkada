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

package body Gsk.Component_Transfer_Node is

   -------------------------------------
   -- Gsk_Component_Transfer_Node_New --
   -------------------------------------

   function Gsk_Component_Transfer_Node_New
      (Child : Gsk.Render_Node.Gsk_Render_Node;
       R     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       G     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       B     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       A     : Gsk.Component_Transfer.Gsk_Component_Transfer)
       return Gsk_Component_Transfer_Node
   is
      Self : constant Gsk_Component_Transfer_Node := new Gsk_Component_Transfer_Node_Record;
   begin
      Gsk.Component_Transfer_Node.Initialize (Self, Child, R, G, B, A);
      return Self;
   end Gsk_Component_Transfer_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self  : out Gsk_Component_Transfer_Node;
       Child : Gsk.Render_Node.Gsk_Render_Node;
       R     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       G     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       B     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       A     : Gsk.Component_Transfer.Gsk_Component_Transfer)
   is
   begin
      Self := new Gsk_Component_Transfer_Node_Record;
      Gsk.Component_Transfer_Node.Initialize (Self, Child, R, G, B, A);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self  : not null access Gsk_Component_Transfer_Node_Record'Class;
       Child : Gsk.Render_Node.Gsk_Render_Node;
       R     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       G     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       B     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       A     : Gsk.Component_Transfer.Gsk_Component_Transfer)
   is
      function Internal
         (Child : System.Address;
          R     : System.Address;
          G     : System.Address;
          B     : System.Address;
          A     : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Child), Get_Object (R), Get_Object (G), Get_Object (B), Get_Object (A)));
      end if;
   end Initialize;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : Gsk_Component_Transfer_Node)
       return Gsk.Render_Node.Gsk_Render_Node
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_node_get_child");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Child;

   ------------------
   -- Get_Transfer --
   ------------------

   function Get_Transfer
      (Self      : Gsk_Component_Transfer_Node;
       Component : Gdk.Enums.Gdk_Color_Channel)
       return Gsk.Component_Transfer.Gsk_Component_Transfer
   is
      function Internal
         (Self      : System.Address;
          Component : Gdk.Enums.Gdk_Color_Channel) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_node_get_transfer");
   begin
      return From_Object (Internal (Get_Object (Self), Component));
   end Get_Transfer;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Component_Transfer_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Component_Transfer_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Component_Transfer_Node;
