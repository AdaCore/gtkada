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

--  A render node that potentially diverts a part of the scene graph to a
--  subsurface.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Subsurface_Node is

   type Gsk_Subsurface_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Subsurface_Node is access all Gsk_Subsurface_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gsk_Subsurface_Node;
       Child      : Gsk.Render_Node.Gsk_Render_Node;
       Subsurface : System.Address);
   procedure Initialize
      (Self       : not null access Gsk_Subsurface_Node_Record'Class;
       Child      : Gsk.Render_Node.Gsk_Render_Node;
       Subsurface : System.Address);
   --  Creates a `GskRenderNode` that will possibly divert the child node to a
   --  subsurface.
   --  Note: Since subsurfaces are currently private, these nodes cannot
   --  currently be created outside of GTK. See
   --  [GtkGraphicsOffload](../gtk4/class.GraphicsOffload.html).
   --  Since: gtk+ 4.14
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Child The child to divert to a subsurface
   --  @param Subsurface the subsurface to use

   function Gsk_Subsurface_Node_New
      (Child      : Gsk.Render_Node.Gsk_Render_Node;
       Subsurface : System.Address) return Gsk_Subsurface_Node;
   --  Creates a `GskRenderNode` that will possibly divert the child node to a
   --  subsurface.
   --  Note: Since subsurfaces are currently private, these nodes cannot
   --  currently be created outside of GTK. See
   --  [GtkGraphicsOffload](../gtk4/class.GraphicsOffload.html).
   --  Since: gtk+ 4.14
   --  @param Child The child to divert to a subsurface
   --  @param Subsurface the subsurface to use

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_subsurface_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : Gsk_Subsurface_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Gets the child node that is getting drawn by the given Node.
   --  Since: gtk+ 4.14
   --  @return the child `GskRenderNode`. Has transfer-ownership='none'.

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Subsurface_Node_Record;

   ---------------
   -- Functions --
   ---------------

   function Get_Subsurface
      (Node : Gsk_Subsurface_Node) return System.Address;
   --  Gets the subsurface that was set on this node
   --  Since: gtk+ 4.14
   --  @param Node a subsurface `GskRenderNode`
   --  @return the subsurface

private
   for Gsk_Subsurface_Node_Record'External_Tag use "GskSubsurfaceNode";
end Gsk.Subsurface_Node;
