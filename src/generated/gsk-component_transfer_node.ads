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

--  A render node for applying a `GskComponentTransfer` for each color
--  component of the child node.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Enums;              use Gdk.Enums;
with Glib;                   use Glib;
with Gsk.Component_Transfer; use Gsk.Component_Transfer;
with Gsk.Render_Node;        use Gsk.Render_Node;

package Gsk.Component_Transfer_Node is

   type Gsk_Component_Transfer_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Component_Transfer_Node is access all Gsk_Component_Transfer_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self  : out Gsk_Component_Transfer_Node;
       Child : Gsk.Render_Node.Gsk_Render_Node;
       R     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       G     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       B     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       A     : Gsk.Component_Transfer.Gsk_Component_Transfer);
   procedure Initialize
      (Self  : not null access Gsk_Component_Transfer_Node_Record'Class;
       Child : Gsk.Render_Node.Gsk_Render_Node;
       R     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       G     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       B     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       A     : Gsk.Component_Transfer.Gsk_Component_Transfer);
   --  Creates a render node that will apply component transfers to a child
   --  node.
   --  Since: gtk+ 4.20
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Child The child to apply component transfers to
   --  @param R transfer for the red component
   --  @param G transfer for the green component
   --  @param B transfer for the blue component
   --  @param A transfer for the alpha component

   function Gsk_Component_Transfer_Node_New
      (Child : Gsk.Render_Node.Gsk_Render_Node;
       R     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       G     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       B     : Gsk.Component_Transfer.Gsk_Component_Transfer;
       A     : Gsk.Component_Transfer.Gsk_Component_Transfer)
       return Gsk_Component_Transfer_Node;
   --  Creates a render node that will apply component transfers to a child
   --  node.
   --  Since: gtk+ 4.20
   --  @param Child The child to apply component transfers to
   --  @param R transfer for the red component
   --  @param G transfer for the green component
   --  @param B transfer for the blue component
   --  @param A transfer for the alpha component

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_component_transfer_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : Gsk_Component_Transfer_Node)
       return Gsk.Render_Node.Gsk_Render_Node;
   --  Gets the child node that is getting drawn by the given Node.
   --  Since: gtk+ 4.20
   --  @return the child `GskRenderNode`. Has transfer-ownership='none'.

   function Get_Transfer
      (Self      : Gsk_Component_Transfer_Node;
       Component : Gdk.Enums.Gdk_Color_Channel)
       return Gsk.Component_Transfer.Gsk_Component_Transfer;
   --  Gets the component transfer for one of the components.
   --  Since: gtk+ 4.20
   --  @param Component the component to get the transfer for
   --  @return the `GskComponentTransfer`. Has transfer-ownership='none'.

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Component_Transfer_Node_Record;

private
   for Gsk_Component_Transfer_Node_Record'External_Tag use "GskComponentTransferNode";
end Gsk.Component_Transfer_Node;
