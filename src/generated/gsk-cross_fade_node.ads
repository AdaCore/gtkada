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

--  A render node cross fading between two child nodes.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Gsk.Render_Node; use Gsk.Render_Node;
with Interfaces.C;    use Interfaces.C;

package Gsk.Cross_Fade_Node is

   type Gsk_Cross_Fade_Node_Record is new Gsk_Render_Node_Record with null record;
   type Gsk_Cross_Fade_Node is access all Gsk_Cross_Fade_Node_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self     : out Gsk_Cross_Fade_Node;
       Start    : Gsk.Render_Node.Gsk_Render_Node;
       The_End  : Gsk.Render_Node.Gsk_Render_Node;
       Progress : Interfaces.C.C_float);
   procedure Initialize
      (Self     : not null access Gsk_Cross_Fade_Node_Record'Class;
       Start    : Gsk.Render_Node.Gsk_Render_Node;
       The_End  : Gsk.Render_Node.Gsk_Render_Node;
       Progress : Interfaces.C.C_float);
   --  Creates a `GskRenderNode` that will do a cross-fade between Start and
   --  End.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Start The start node to be drawn
   --  @param The_End The node to be cross_fadeed onto the Start node
   --  @param Progress How far the fade has progressed from start to end. The
   --  value will be clamped to the range [0 ... 1]

   function Gsk_Cross_Fade_Node_New
      (Start    : Gsk.Render_Node.Gsk_Render_Node;
       The_End  : Gsk.Render_Node.Gsk_Render_Node;
       Progress : Interfaces.C.C_float) return Gsk_Cross_Fade_Node;
   --  Creates a `GskRenderNode` that will do a cross-fade between Start and
   --  End.
   --  @param Start The start node to be drawn
   --  @param The_End The node to be cross_fadeed onto the Start node
   --  @param Progress How far the fade has progressed from start to end. The
   --  value will be clamped to the range [0 ... 1]

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_cross_fade_node_get_type");

   -------------
   -- Methods --
   -------------

   function Get_End_Child
      (Self : Gsk_Cross_Fade_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Retrieves the child `GskRenderNode` at the end of the cross-fade.
   --  @return a `GskRenderNode`. Has transfer-ownership='none'.

   function Get_Progress
      (Self : Gsk_Cross_Fade_Node) return Interfaces.C.C_float;
   --  Retrieves the progress value of the cross fade.
   --  @return the progress value, between 0 and 1

   function Get_Start_Child
      (Self : Gsk_Cross_Fade_Node) return Gsk.Render_Node.Gsk_Render_Node;
   --  Retrieves the child `GskRenderNode` at the beginning of the cross-fade.
   --  @return a `GskRenderNode`. Has transfer-ownership='none'.

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Cross_Fade_Node_Record;

private
   for Gsk_Cross_Fade_Node_Record'External_Tag use "GskCrossFadeNode";
end Gsk.Cross_Fade_Node;
