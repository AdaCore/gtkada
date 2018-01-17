------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Glib.Graphs;               use Glib.Graphs;
with Glib.Graphs.Layouts;
with Gtkada.Canvas_View.Views;  use Gtkada.Canvas_View.Views;

package body Gtkada.Canvas_View.Models.Layers is

   type Canvas_Vertex is new Vertex with record
      Item  : Abstract_Item;
      View  : Canvas_View;
   end record;
   type Canvas_Vertex_Access is access all Canvas_Vertex'Class;

   type Canvas_Edge is new Edge with record
      Item  : Canvas_Link;
   end record;
   type Canvas_Edge_Access is access all Canvas_Edge'Class;

   procedure Get_Size (V : Vertex_Access; Width, Height : out Gdouble);
   procedure Set_Position (V : Vertex_Access; X, Y : Gdouble);

   package Graph_Layouts is new Glib.Graphs.Layouts
     (Get_Size     => Get_Size,
      Set_Position => Set_Position);

   type Canvas_Dummy_Vertex is new Graph_Layouts.Base_Dummy_Vertex with record
      Pos   : Gtkada.Style.Point;
   end record;

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
      V2 : Canvas_Vertex_Access;
   begin
      if V.all in Canvas_Vertex'Class then
         V2 := Canvas_Vertex_Access (V);

         if V2.View /= null
           and then V2.Item.Position /= No_Position
         then
            Animate_Position (V2.Item, (X, Y)).Start (V2.View);
         else
            V2.Item.Set_Position ((X, Y));
         end if;
      else
         Canvas_Dummy_Vertex (V.all).Pos := (X, Y);
      end if;
   end Set_Position;

   ------------
   -- Layout --
   ------------

   procedure Layout
     (Self                 : not null access Canvas_Model_Record'Class;
      View                 : access Canvas_View_Record'Class := null;
      Horizontal           : Boolean := True;
      Add_Waypoints        : Boolean := False;
      Space_Between_Items  : Gdouble := 10.0;
      Space_Between_Layers : Gdouble := 20.0)
   is
      procedure Replaced_With_Dummy_Vertices
        (Replaced_Edge : Edge_Access;
         Dummies       : Vertices_Array);

      package Layered_Layouts is new Graph_Layouts.Layered_Layouts
        (Dummy_Vertex                 => Canvas_Dummy_Vertex,
         Replaced_With_Dummy_Vertices => Replaced_With_Dummy_Vertices);

      type Long_Edge (Size : Natural) is record
         Edge : Canvas_Link;
         Dummies : Vertices_Array (1 .. Size);
         --  Set when the edge was split into smaller edges with dummy
         --  vertices. This is used to create the waypoints for long edges.
      end record;
      package Long_Edge_Lists
         is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Long_Edge);
      use Long_Edge_Lists;

      Long_Edges : Long_Edge_Lists.List;

      ----------------------------------
      -- Replaced_With_Dummy_Vertices --
      ----------------------------------

      procedure Replaced_With_Dummy_Vertices
        (Replaced_Edge : Edge_Access;
         Dummies       : Vertices_Array)
      is
         E : constant Canvas_Edge_Access := Canvas_Edge_Access (Replaced_Edge);
      begin
         if Add_Waypoints then
            Long_Edges.Append
              (Long_Edge'(Size => Dummies'Length,
                          Edge => E.Item,
                          Dummies => Dummies));
         end if;
      end Replaced_With_Dummy_Vertices;

      package Items_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Abstract_Item,
         Element_Type    => Vertex_Access,
         Hash            => Gtkada.Canvas_View.Hash,
         Equivalent_Keys => "=");
      use Items_Maps;

      G     : Graph;
      Items : Items_Maps.Map;

      procedure On_Item (It : not null access Abstract_Item_Record'Class);
      procedure On_Link (It : not null access Abstract_Item_Record'Class);

      procedure On_Item (It : not null access Abstract_Item_Record'Class) is
         V : constant Vertex_Access := new Canvas_Vertex'
           (Vertex with
            Item => Abstract_Item (It),
            View => Canvas_View (View));
      begin
         Add_Vertex (G, V);
         Items.Include (Abstract_Item (It), V);
      end On_Item;

      procedure On_Link (It : not null access Abstract_Item_Record'Class) is
         V1, V2 : Vertex_Access;
         E      : Canvas_Edge_Access;
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

         --  Remove existing waypoints
         Canvas_Link (It).Set_Waypoints ((1 .. 0 => <>));

         V1 := Items.Element (Canvas_Link (It).From.Get_Toplevel_Item);
         V2 := Items.Element (Canvas_Link (It).To.Get_Toplevel_Item);
         E := new Canvas_Edge;
         E.Item := Canvas_Link (It);
         Add_Edge (G, E, V1, V2);
      end On_Link;

      C : Long_Edge_Lists.Cursor;

   begin
      Set_Directed (G, True);
      Self.For_Each_Item (On_Item'Access, Filter => Kind_Item);
      Self.For_Each_Item (On_Link'Access, Filter => Kind_Link);
      Layered_Layouts.Layout
        (G,
         Horizontal           => Horizontal,
         Space_Between_Layers => Space_Between_Layers,
         Space_Between_Items  => Space_Between_Items);

      if Add_Waypoints then
         C := Long_Edges.First;
         while Has_Element (C) loop
            declare
               E : constant Long_Edge := Element (C);
               WP : Item_Point_Array (E.Dummies'Range);
            begin
               for D in WP'Range loop
                  WP (D) := Canvas_Dummy_Vertex (E.Dummies (D).all).Pos;
               end loop;

               E.Edge.Set_Waypoints (WP);
            end;

            Next (C);
         end loop;
      end if;

      Destroy (G);
      Self.Refresh_Layout;  --  recompute the links, and refresh views
   end Layout;

end Gtkada.Canvas_View.Models.Layers;
