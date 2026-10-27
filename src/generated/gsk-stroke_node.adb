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

package body Gsk.Stroke_Node is

   -------------------------
   -- Gsk_Stroke_Node_New --
   -------------------------

   function Gsk_Stroke_Node_New
      (Child  : Gsk.Render_Node.Gsk_Render_Node;
       Path   : Gsk.Path.Gsk_Path;
       Stroke : Gsk.Stroke.Gsk_Stroke) return Gsk_Stroke_Node
   is
      Self : constant Gsk_Stroke_Node := new Gsk_Stroke_Node_Record;
   begin
      Gsk.Stroke_Node.Initialize (Self, Child, Path, Stroke);
      return Self;
   end Gsk_Stroke_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gsk_Stroke_Node;
       Child  : Gsk.Render_Node.Gsk_Render_Node;
       Path   : Gsk.Path.Gsk_Path;
       Stroke : Gsk.Stroke.Gsk_Stroke)
   is
   begin
      Self := new Gsk_Stroke_Node_Record;
      Gsk.Stroke_Node.Initialize (Self, Child, Path, Stroke);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gsk_Stroke_Node_Record'Class;
       Child  : Gsk.Render_Node.Gsk_Render_Node;
       Path   : Gsk.Path.Gsk_Path;
       Stroke : Gsk.Stroke.Gsk_Stroke)
   is
      function Internal
         (Child  : System.Address;
          Path   : System.Address;
          Stroke : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_stroke_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Child), Get_Object (Path), Get_Object (Stroke)));
      end if;
   end Initialize;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : Gsk_Stroke_Node) return Gsk.Render_Node.Gsk_Render_Node
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_stroke_node_get_child");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Child;

   --------------
   -- Get_Path --
   --------------

   function Get_Path (Self : Gsk_Stroke_Node) return Gsk.Path.Gsk_Path is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_stroke_node_get_path");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Path;

   ----------------
   -- Get_Stroke --
   ----------------

   function Get_Stroke (Self : Gsk_Stroke_Node) return Gsk.Stroke.Gsk_Stroke is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_stroke_node_get_stroke");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Stroke;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Stroke_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Stroke_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Stroke_Node;
