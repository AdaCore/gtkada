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

--  A render node drawing one or more shadows behind its single child node.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;        use Gdk.RGBA;
with Glib;            use Glib;
with Gsk.Render_Node; use Gsk.Render_Node;
with Interfaces.C;    use Interfaces.C;

package Gsk.Shadow_Node is

   type Gsk_Shadow_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Shadow_Node is access all Gsk_Shadow_Node_Record'Class;

   type Gsk_Shadow is record
      Color : Gdk.RGBA.Gdk_RGBA;
      Dx : Interfaces.C.C_float;
      Dy : Interfaces.C.C_float;
      Radius : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Gsk_Shadow);
   --  The shadow parameters in a shadow node.

   type Gsk_Shadow_Array is array (Natural range <>) of Gsk_Shadow
   with Convention => C;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_shadow_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : Gsk_Shadow_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Retrieves the child `GskRenderNode` of the shadow Node.
   --  @return the child render node. Has transfer-ownership='none'.

   function Get_N_Shadows (Self : Gsk_Shadow_Node) return Gsize;
   --  Retrieves the number of shadows in the Node.
   --  @return the number of shadows.

   function Get_Shadow (Self : Gsk_Shadow_Node; I : Gsize) return Gsk_Shadow;
   --  Retrieves the shadow data at the given index I.
   --  @param I the given index
   --  @return the shadow data

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Gsk_Shadow) return Gsk_Shadow;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   procedure Gtk_New
     (Self    : out Gsk_Shadow_Node;
      Child   : Gsk.Render_Node.Gsk_Render_Node;
      Shadows : Gsk_Shadow_Array);
   procedure Initialize
     (Self    : not null access Gsk_Shadow_Node_Record'Class;
      Child   : Gsk.Render_Node.Gsk_Render_Node;
      Shadows : Gsk_Shadow_Array);
   --  Creates a `GskRenderNode` that will draw a Child with the given Shadows
   --  below it.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Child The node to draw
   --  @param Shadows The shadows to apply

   function Gsk_Shadow_Node_New
     (Child   : Gsk.Render_Node.Gsk_Render_Node;
      Shadows : Gsk_Shadow_Array) return Gsk_Shadow_Node;
   --  Creates a `GskRenderNode` that will draw a Child with the given Shadows
   --  below it.
   --  @param Child The node to draw
   --  @param Shadows The shadows to apply

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Shadow_Node_Record;

private
   for Gsk_Shadow_Node_Record'External_Tag use "GskShadowNode";
end Gsk.Shadow_Node;
