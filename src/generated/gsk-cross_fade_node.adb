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

package body Gsk.Cross_Fade_Node is

   -----------------------------
   -- Gsk_Cross_Fade_Node_New --
   -----------------------------

   function Gsk_Cross_Fade_Node_New
      (Start    : Gsk.Render_Node.Gsk_Render_Node;
       The_End  : Gsk.Render_Node.Gsk_Render_Node;
       Progress : Interfaces.C.C_float) return Gsk_Cross_Fade_Node
   is
      Self : constant Gsk_Cross_Fade_Node := new Gsk_Cross_Fade_Node_Record;
   begin
      Gsk.Cross_Fade_Node.Initialize (Self, Start, The_End, Progress);
      return Self;
   end Gsk_Cross_Fade_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self     : out Gsk_Cross_Fade_Node;
       Start    : Gsk.Render_Node.Gsk_Render_Node;
       The_End  : Gsk.Render_Node.Gsk_Render_Node;
       Progress : Interfaces.C.C_float)
   is
   begin
      Self := new Gsk_Cross_Fade_Node_Record;
      Gsk.Cross_Fade_Node.Initialize (Self, Start, The_End, Progress);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self     : not null access Gsk_Cross_Fade_Node_Record'Class;
       Start    : Gsk.Render_Node.Gsk_Render_Node;
       The_End  : Gsk.Render_Node.Gsk_Render_Node;
       Progress : Interfaces.C.C_float)
   is
      function Internal
         (Start    : System.Address;
          The_End  : System.Address;
          Progress : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_cross_fade_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Start), Get_Object (The_End), Progress));
      end if;
   end Initialize;

   -------------------
   -- Get_End_Child --
   -------------------

   function Get_End_Child
      (Self : Gsk_Cross_Fade_Node) return Gsk.Render_Node.Gsk_Render_Node
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_cross_fade_node_get_end_child");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_End_Child;

   ------------------
   -- Get_Progress --
   ------------------

   function Get_Progress
      (Self : Gsk_Cross_Fade_Node) return Interfaces.C.C_float
   is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_cross_fade_node_get_progress");
   begin
      return Internal (Get_Object (Self));
   end Get_Progress;

   ---------------------
   -- Get_Start_Child --
   ---------------------

   function Get_Start_Child
      (Self : Gsk_Cross_Fade_Node) return Gsk.Render_Node.Gsk_Render_Node
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_cross_fade_node_get_start_child");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Start_Child;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Cross_Fade_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Cross_Fade_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Cross_Fade_Node;
