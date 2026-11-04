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
with Glib.Object; use Glib.Object;

package body Gsk.Glshader_Node is

   ---------------------------
   -- Gsk_Glshader_Node_New --
   ---------------------------

   function Gsk_Glshader_Node_New
     (Shader   : not null access Gsk.Glshader.Gsk_Glshader_Record'Class;
      Bounds   : not null access Graphene.Rect.Graphene_Rect_T;
      Args     : Glib.Bytes.Gbytes;
      Children : Gsk.Render_Node.Gsk_Render_Node_Array)
   return Gsk_Glshader_Node
   is
      Self : constant Gsk_Glshader_Node := new Gsk_Glshader_Node_Record;
   begin
      Gsk.Glshader_Node.Initialize (Self, Shader, Bounds, Args, Children);
      return Self;
   end Gsk_Glshader_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self     : out Gsk_Glshader_Node;
      Shader   : not null access Gsk.Glshader.Gsk_Glshader_Record'Class;
      Bounds   : not null access Graphene.Rect.Graphene_Rect_T;
      Args     : Glib.Bytes.Gbytes;
      Children : Gsk.Render_Node.Gsk_Render_Node_Array) is
   begin
      Self := new Gsk_Glshader_Node_Record;
      Gsk.Glshader_Node.Initialize (Self, Shader, Bounds, Args, Children);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : not null access Gsk_Glshader_Node_Record'Class;
      Shader   : not null access Gsk.Glshader.Gsk_Glshader_Record'Class;
      Bounds   : not null access Graphene.Rect.Graphene_Rect_T;
      Args     : Glib.Bytes.Gbytes;
      Children : Gsk.Render_Node.Gsk_Render_Node_Array)
   is
      function Internal
        (Shader     : System.Address;
         Bounds     : access Graphene.Rect.Graphene_Rect_T;
         Args       : System.Address;
         Children   : System.Address;
         N_Children : Guint) return System.Address;
      pragma Import (C, Internal, "gsk_gl_shader_node_new");
   begin
      if not Self.Is_Created then
         declare
            Ch : Glib.Gpointer_Array (Children'Range);
         begin
            for Index in Children'Range loop
               Ch (Index) := Get_Object (Children (Index));
            end loop;
            Set_Object (Self, Internal (Get_Object (Shader), Bounds, Get_Object (Args), Ch'Address, Guint (Children'Length)));
         end;
      end if;
   end Initialize;

   ----------------
   -- Get_Shader --
   ----------------

   function Get_Shader
      (Self : Gsk_Glshader_Node) return Gsk.Glshader.Gsk_Glshader
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_gl_shader_node_get_shader");
      Stub_Gsk_Glshader : Gsk.Glshader.Gsk_Glshader_Record;
   begin
      return Gsk.Glshader.Gsk_Glshader (Get_User_Data (Internal (Get_Object (Self)), Stub_Gsk_Glshader));
   end Get_Shader;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Glshader_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Glshader_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Glshader_Node;
