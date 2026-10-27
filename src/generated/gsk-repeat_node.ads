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

--  A render node repeating its single child node.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Graphene.Rect;   use Graphene.Rect;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Repeat_Node is

   type Gsk_Repeat_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Repeat_Node is access all Gsk_Repeat_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self         : out Gsk_Repeat_Node;
       Bounds       : not null access Graphene.Rect.Graphene_Rect_T;
       Child        : Gsk.Render_Node.Gsk_Render_Node;
       Child_Bounds : access Graphene.Rect.Graphene_Rect_T);
   procedure Initialize
      (Self         : not null access Gsk_Repeat_Node_Record'Class;
       Bounds       : not null access Graphene.Rect.Graphene_Rect_T;
       Child        : Gsk.Render_Node.Gsk_Render_Node;
       Child_Bounds : access Graphene.Rect.Graphene_Rect_T);
   --  Creates a `GskRenderNode` that will repeat the drawing of Child across
   --  the given Bounds.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Bounds The bounds of the area to be painted
   --  @param Child The child to repeat
   --  @param Child_Bounds The area of the child to repeat or null to use the
   --  child's bounds

   function Gsk_Repeat_Node_New
      (Bounds       : not null access Graphene.Rect.Graphene_Rect_T;
       Child        : Gsk.Render_Node.Gsk_Render_Node;
       Child_Bounds : access Graphene.Rect.Graphene_Rect_T)
       return Gsk_Repeat_Node;
   --  Creates a `GskRenderNode` that will repeat the drawing of Child across
   --  the given Bounds.
   --  @param Bounds The bounds of the area to be painted
   --  @param Child The child to repeat
   --  @param Child_Bounds The area of the child to repeat or null to use the
   --  child's bounds

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_repeat_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : Gsk_Repeat_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Retrieves the child of Node.
   --  @return a `GskRenderNode`. Has transfer-ownership='none'.

   function Get_Child_Bounds
      (Self : Gsk_Repeat_Node)
       return access constant Graphene.Rect.Graphene_Rect_T;
   --  Retrieves the bounding rectangle of the child of Node.
   --  @return a bounding rectangle

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Repeat_Node_Record;

private
   for Gsk_Repeat_Node_Record'External_Tag use "GskRepeatNode";
end Gsk.Repeat_Node;
