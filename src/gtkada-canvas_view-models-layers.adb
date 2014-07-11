------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2014, AdaCore                          --
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

with Ada.Containers.Hashed_Maps;
with Glib.Graphs;         use Glib.Graphs;
with Glib.Graphs.Layouts;

package body Gtkada.Canvas_View.Models.Layers is

   type Canvas_Vertex is new Vertex with record
      Item  : Abstract_Item;
   end record;
   type Canvas_Vertex_Access is access all Canvas_Vertex'Class;

   procedure Get_Size (V : Vertex_Access; Width, Height : out Gdouble);
   procedure Set_Position (V : Vertex_Access; X, Y : Gdouble);
   package Graph_Layouts is new Glib.Graphs.Layouts
     (Get_Size     => Get_Size,
      Set_Position => Set_Position);

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size (V : Vertex_Access; Width, Height : out Gdouble) is
      B : constant Model_Rectangle :=
        Canvas_Vertex_Access (V).Item.Model_Bounding_Box;
   begin
      Width  := B.Width;
      Height := B.Height;
   end Get_Size;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (V : Vertex_Access; X, Y : Gdouble) is
   begin
      Canvas_Vertex_Access (V).Item.Set_Position ((X, Y));
   end Set_Position;

   ------------
   -- Layout --
   ------------

   procedure Layout
     (Self                 : not null access Canvas_Model_Record'Class;
      Horizontal           : Boolean := True;
      Space_Between_Items  : Gdouble := 10.0;
      Space_Between_Layers : Gdouble := 20.0)
   is
      G : Graph;

      type Item_Info is record
         Vertex : Vertex_Access;
         --  The vertex or edge representing this item in the graph
      end record;
      package Items_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Abstract_Item,
         Element_Type    => Item_Info,
         Hash            => Gtkada.Canvas_View.Hash,
         Equivalent_Keys => "=");
      use Items_Maps;

      Items : Items_Maps.Map;

      procedure On_Item (It : not null access Abstract_Item_Record'Class);
      procedure On_Link (It : not null access Abstract_Item_Record'Class);

      procedure On_Item (It : not null access Abstract_Item_Record'Class) is
         V : constant Vertex_Access := new Canvas_Vertex'
           (Vertex with Item => Abstract_Item (It));
      begin
         Add_Vertex (G, V);
         Items.Include (Abstract_Item (It), Item_Info'(Vertex => V));
      end On_Item;

      procedure On_Link (It : not null access Abstract_Item_Record'Class) is
         V1, V2 : Vertex_Access;
      begin
         if It.all not in Canvas_Link_Record'Class then
            --  custom edges unsupported, since we don't know their head or
            --  tail
            return;
         end if;

         --  Ignore link-to-link
         if Canvas_Link (It).From.Is_Link
           or else Canvas_Link (It).To.Is_Link
         then
            return;
         end if;

         V1 := Items.Element (Canvas_Link (It).From.Get_Toplevel_Item).Vertex;
         V2 := Items.Element (Canvas_Link (It).To.Get_Toplevel_Item).Vertex;
         Add_Edge (G, V1, V2);
      end On_Link;

   begin
      Set_Directed (G, True);
      Self.For_Each_Item (On_Item'Access, Filter => Kind_Item);
      Self.For_Each_Item (On_Link'Access, Filter => Kind_Link);
      Graph_Layouts.Layer_Layout
        (G,
         Horizontal           => Horizontal,
         Space_Between_Layers => Space_Between_Layers,
         Space_Between_Items  => Space_Between_Items);
      Destroy (G);
      Self.Refresh_Layout;  --  recompute the links, and refresh views
   end Layout;

end Gtkada.Canvas_View.Models.Layers;
