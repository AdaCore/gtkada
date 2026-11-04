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

--  A render node applying a rounded rectangle clip to its single child.

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Gsk.Render_Node;  use Gsk.Render_Node;
with Gsk.Rounded_Rect; use Gsk.Rounded_Rect;

package Gsk.Rounded_Clip_Node is

   type Gsk_Rounded_Clip_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Rounded_Clip_Node is access all Gsk_Rounded_Clip_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self  : out Gsk_Rounded_Clip_Node;
       Child : Gsk.Render_Node.Gsk_Render_Node;
       Clip  : Gsk.Rounded_Rect.Gsk_Rounded_Rect);
   procedure Initialize
      (Self  : not null access Gsk_Rounded_Clip_Node_Record'Class;
       Child : Gsk.Render_Node.Gsk_Render_Node;
       Clip  : Gsk.Rounded_Rect.Gsk_Rounded_Rect);
   --  Creates a `GskRenderNode` that will clip the Child to the area given by
   --  Clip.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Child The node to draw
   --  @param Clip The clip to apply

   function Gsk_Rounded_Clip_Node_New
      (Child : Gsk.Render_Node.Gsk_Render_Node;
       Clip  : Gsk.Rounded_Rect.Gsk_Rounded_Rect)
       return Gsk_Rounded_Clip_Node;
   --  Creates a `GskRenderNode` that will clip the Child to the area given by
   --  Clip.
   --  @param Child The node to draw
   --  @param Clip The clip to apply

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_rounded_clip_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : Gsk_Rounded_Clip_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Gets the child node that is getting clipped by the given Node.
   --  @return The child that is getting clipped. Has
   --  transfer-ownership='none'.

   function Get_Clip
      (Self : Gsk_Rounded_Clip_Node)
       return Gsk.Rounded_Rect.Gsk_Rounded_Rect;
   --  Retrieves the rounded rectangle used to clip the contents of the Node.
   --  @return a rounded rectangle

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Rounded_Clip_Node_Record;

private
   for Gsk_Rounded_Clip_Node_Record'External_Tag use "GskRoundedClipNode";
end Gsk.Rounded_Clip_Node;
