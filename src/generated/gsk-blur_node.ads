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

--  A render node applying a blur effect to its single child.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Gsk.Render_Node; use Gsk.Render_Node;
with Interfaces.C;    use Interfaces.C;

package Gsk.Blur_Node is

   type Gsk_Blur_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Blur_Node is access all Gsk_Blur_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gsk_Blur_Node;
       Child  : Gsk.Render_Node.Gsk_Render_Node;
       Radius : Interfaces.C.C_float);
   procedure Initialize
      (Self   : not null access Gsk_Blur_Node_Record'Class;
       Child  : Gsk.Render_Node.Gsk_Render_Node;
       Radius : Interfaces.C.C_float);
   --  Creates a render node that blurs the child.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Child the child node to blur
   --  @param Radius the blur radius. Must be positive

   function Gsk_Blur_Node_New
      (Child  : Gsk.Render_Node.Gsk_Render_Node;
       Radius : Interfaces.C.C_float) return Gsk_Blur_Node;
   --  Creates a render node that blurs the child.
   --  @param Child the child node to blur
   --  @param Radius the blur radius. Must be positive

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_blur_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : Gsk_Blur_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Retrieves the child `GskRenderNode` of the blur Node.
   --  @return the blurred child node. Has transfer-ownership='none'.

   function Get_Radius (Self : Gsk_Blur_Node) return Interfaces.C.C_float;
   --  Retrieves the blur radius of the Node.
   --  @return the blur radius

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Blur_Node_Record;

private
   for Gsk_Blur_Node_Record'External_Tag use "GskBlurNode";
end Gsk.Blur_Node;
