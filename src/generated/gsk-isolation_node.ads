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

--  A render node that isolates its child from surrounding rendernodes.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Gsk.Enums;       use Gsk.Enums;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Isolation_Node is

   type Gsk_Isolation_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Isolation_Node is access all Gsk_Isolation_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gsk_Isolation_Node;
       Child      : Gsk.Render_Node.Gsk_Render_Node;
       Isolations : Gsk.Enums.Gsk_Isolation);
   procedure Initialize
      (Self       : not null access Gsk_Isolation_Node_Record'Class;
       Child      : Gsk.Render_Node.Gsk_Render_Node;
       Isolations : Gsk.Enums.Gsk_Isolation);
   --  Creates a `GskRenderNode` that isolates the drawing operations of the
   --  child from surrounding ones.
   --  You can express "everything but these flags" in a forward compatible
   --  way by using bit math: `GSK_ISOLATION_ALL & ~(GSK_ISOLATION_BACKGROUND |
   --  GSK_ISOLATION_COPY_PASTE)` will isolate everything but background and
   --  copy/paste.
   --  For the available isolations, see [flagsGsk.Isolation].
   --  Since: gtk+ 4.22
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Child The child
   --  @param Isolations features to isolate

   function Gsk_Isolation_Node_New
      (Child      : Gsk.Render_Node.Gsk_Render_Node;
       Isolations : Gsk.Enums.Gsk_Isolation) return Gsk_Isolation_Node;
   --  Creates a `GskRenderNode` that isolates the drawing operations of the
   --  child from surrounding ones.
   --  You can express "everything but these flags" in a forward compatible
   --  way by using bit math: `GSK_ISOLATION_ALL & ~(GSK_ISOLATION_BACKGROUND |
   --  GSK_ISOLATION_COPY_PASTE)` will isolate everything but background and
   --  copy/paste.
   --  For the available isolations, see [flagsGsk.Isolation].
   --  Since: gtk+ 4.22
   --  @param Child The child
   --  @param Isolations features to isolate

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_isolation_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : Gsk_Isolation_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Gets the child node that is getting drawn by the given Node.
   --  Since: gtk+ 4.22
   --  @return the child `GskRenderNode`. Has transfer-ownership='none'.

   function Get_Isolations
      (Self : Gsk_Isolation_Node) return Gsk.Enums.Gsk_Isolation;
   --  Gets the isolation features that are enforced by this node.
   --  Since: gtk+ 4.22
   --  @return the isolation features

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Isolation_Node_Record;

private
   for Gsk_Isolation_Node_Record'External_Tag use "GskIsolationNode";
end Gsk.Isolation_Node;
