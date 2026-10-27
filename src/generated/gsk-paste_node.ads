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

--  A render node for a paste.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Graphene.Rect;   use Graphene.Rect;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Paste_Node is

   type Gsk_Paste_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Paste_Node is access all Gsk_Paste_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gsk_Paste_Node;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T;
       Depth  : Gsize);
   procedure Initialize
      (Self   : not null access Gsk_Paste_Node_Record'Class;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T;
       Depth  : Gsize);
   --  Creates a `GskRenderNode` that will paste copied contents.
   --  Since: gtk+ 4.22
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Bounds the rectangle to render the paste into
   --  @param Depth the index of which copy to paste. This will usually be 0.

   function Gsk_Paste_Node_New
      (Bounds : not null access Graphene.Rect.Graphene_Rect_T;
       Depth  : Gsize) return Gsk_Paste_Node;
   --  Creates a `GskRenderNode` that will paste copied contents.
   --  Since: gtk+ 4.22
   --  @param Bounds the rectangle to render the paste into
   --  @param Depth the index of which copy to paste. This will usually be 0.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_paste_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Depth (Self : Gsk_Paste_Node) return Gsize;
   --  Retrieves the index of the copy that should be pasted.
   --  Since: gtk+ 4.22
   --  @return the index of the copy to paste.

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Paste_Node_Record;

private
   for Gsk_Paste_Node_Record'External_Tag use "GskPasteNode";
end Gsk.Paste_Node;
