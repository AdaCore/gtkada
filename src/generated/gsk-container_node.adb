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

package body Gsk.Container_Node is

   ----------------------------
   -- Gsk_Container_Node_New --
   ----------------------------

   function Gsk_Container_Node_New
     (Children : Gsk_Render_Node_Array) return Gsk_Container_Node
   is
      Self : constant Gsk_Container_Node := new Gsk_Container_Node_Record;
   begin
      Gsk.Container_Node.Initialize (Self, Children);
      return Self;
   end Gsk_Container_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self     : out Gsk_Container_Node;
      Children : Gsk.Render_Node.Gsk_Render_Node_Array) is
   begin
      Self := new Gsk_Container_Node_Record;
      Gsk.Container_Node.Initialize (Self, Children);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : not null access Gsk_Container_Node_Record'Class;
      Children : Gsk.Render_Node.Gsk_Render_Node_Array)
   is
      function Internal
        (Children   : System.Address;
         N_Children : Guint) return System.Address;
      pragma Import (C, Internal, "gsk_container_node_new");
   begin
      if not Self.Is_Created then
         declare
            Ch : Glib.Gpointer_Array (Children'Range);
         begin
            for Index in Children'Range loop
               Ch (Index) := Get_Object (Children (Index));
            end loop;
            Set_Object (Self, Internal (Ch'Address, Guint (Children'Length)));
         end;
      end if;
   end Initialize;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : Gsk_Container_Node;
       Idx  : Guint) return Gsk.Render_Node.Gsk_Render_Node
   is
      function Internal
         (Self : System.Address;
          Idx  : Guint) return System.Address;
      pragma Import (C, Internal, "gsk_container_node_get_child");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self), Idx));
   end Get_Child;

   --------------------
   -- Get_N_Children --
   --------------------

   function Get_N_Children (Self : Gsk_Container_Node) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gsk_container_node_get_n_children");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Children;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Container_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Container_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Container_Node;
