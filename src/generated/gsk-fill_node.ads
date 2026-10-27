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

--  A render node filling the area given by [structGsk.Path] and
--  [enumGsk.FillRule] with the child node.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Gsk.Enums;       use Gsk.Enums;
with Gsk.Path;        use Gsk.Path;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Fill_Node is

   type Gsk_Fill_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Fill_Node is access all Gsk_Fill_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self      : out Gsk_Fill_Node;
       Child     : Gsk.Render_Node.Gsk_Render_Node;
       Path      : Gsk.Path.Gsk_Path;
       Fill_Rule : Gsk.Enums.Gsk_Fill_Rule);
   procedure Initialize
      (Self      : not null access Gsk_Fill_Node_Record'Class;
       Child     : Gsk.Render_Node.Gsk_Render_Node;
       Path      : Gsk.Path.Gsk_Path;
       Fill_Rule : Gsk.Enums.Gsk_Fill_Rule);
   --  Creates a `GskRenderNode` that will fill the Child in the area given by
   --  Path and Fill_Rule.
   --  Since: gtk+ 4.14
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Child The node to fill the area with
   --  @param Path The path describing the area to fill
   --  @param Fill_Rule The fill rule to use

   function Gsk_Fill_Node_New
      (Child     : Gsk.Render_Node.Gsk_Render_Node;
       Path      : Gsk.Path.Gsk_Path;
       Fill_Rule : Gsk.Enums.Gsk_Fill_Rule) return Gsk_Fill_Node;
   --  Creates a `GskRenderNode` that will fill the Child in the area given by
   --  Path and Fill_Rule.
   --  Since: gtk+ 4.14
   --  @param Child The node to fill the area with
   --  @param Path The path describing the area to fill
   --  @param Fill_Rule The fill rule to use

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_fill_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : Gsk_Fill_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Gets the child node that is getting drawn by the given Node.
   --  Since: gtk+ 4.14
   --  @return The child that is getting drawn. Has transfer-ownership='none'.

   function Get_Fill_Rule
      (Self : Gsk_Fill_Node) return Gsk.Enums.Gsk_Fill_Rule;
   --  Retrieves the fill rule used to determine how the path is filled.
   --  Since: gtk+ 4.14
   --  @return a `GskFillRule`

   function Get_Path (Self : Gsk_Fill_Node) return Gsk.Path.Gsk_Path;
   --  Retrieves the path used to describe the area filled with the contents
   --  of the Node.
   --  Since: gtk+ 4.14
   --  @return a `GskPath`. Has transfer-ownership='none'.

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Fill_Node_Record;

private
   for Gsk_Fill_Node_Record'External_Tag use "GskFillNode";
end Gsk.Fill_Node;
