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
with Gdk.RGBA;            use Gdk.RGBA;
with Glib.Graphs;         use Glib.Graphs;
with Glib.Graphs.Layouts;

package body Gtkada.Canvas_View.Models.Layers is

   Debug_Show_Dummies : constant Boolean := False;
   --  If true, modifies the model to show the dummy vertices. This is useful
   --  to debug the layout algorithm.
   --  In this case, the model must be a List_Canvas_Model_Record'Class

   type Canvas_Vertex is new Vertex with record
      Item  : Abstract_Item;
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
      Item  : Abstract_Item;
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
   begin
      if V.all in Canvas_Vertex'Class then
         Canvas_Vertex_Access (V).Item.Set_Position ((X, Y));
      elsif Debug_Show_Dummies then
         Canvas_Dummy_Vertex (V.all).Item.Set_Position ((X, Y));
      end if;
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
      procedure Replaced_With_Dummy_Vertices
        (Replaced_Edge : Edge_Access;
         Dummies       : Vertices_Array);

      package Layered_Layouts is new Graph_Layouts.Layered_Layouts
        (Dummy_Vertex                 => Canvas_Dummy_Vertex,
         Replaced_With_Dummy_Vertices => Replaced_With_Dummy_Vertices);

      ----------------------------------
      -- Replaced_With_Dummy_Vertices --
      ----------------------------------

      procedure Replaced_With_Dummy_Vertices
        (Replaced_Edge : Edge_Access;
         Dummies       : Vertices_Array)
      is
         Style, R_Style, Edge_Style : Drawing_Style;
         It    : Abstract_Item;
         Prev  : Abstract_Item;
         Link  : Canvas_Link;
      begin
         if Debug_Show_Dummies then
            Style := Gtk_New (Stroke => Null_RGBA, Line_Width => 1.0);
            R_Style := Canvas_Edge_Access (Replaced_Edge).Item.Get_Style;
            Edge_Style := Gtk_New
              (Stroke => R_Style.Get_Stroke,
               Arrow_From  => No_Arrow_Style,
               Arrow_To    => No_Arrow_Style,
               Symbol_From => No_Symbol,
               Symbol_To   => No_Symbol);

            Prev := Canvas_Vertex_Access (Get_Src (Replaced_Edge)).Item;

            for D in Dummies'Range loop
               It := Abstract_Item
                 (Gtk_New_Ellipse (Style, Width => 1.0, Height => 1.0));
               Canvas_Dummy_Vertex (Dummies (D).all).Item := It;
               List_Canvas_Model (Self).Add (It);

               Link := Gtk_New
                 (Prev, It,
                  Style       => Edge_Style,
                  Anchor_From => (0.5, 0.5, No_Clipping, 0.0),
                  Anchor_To   => (0.5, 0.5, No_Clipping, 0.0));
               List_Canvas_Model (Self).Add (Link);

               Prev := It;
            end loop;

            It := Canvas_Vertex_Access (Get_Dest (Replaced_Edge)).Item;
            Link := Gtk_New
              (Prev, It,
               Style       => R_Style,
               Anchor_From => (0.5, 0.5, No_Clipping, 0.0),
               Anchor_To   => (0.5, 0.5, No_Clipping, 0.0));
            List_Canvas_Model (Self).Add (Link);

            Self.Remove (Canvas_Edge_Access (Replaced_Edge).Item);
            Canvas_Edge_Access (Replaced_Edge).Item := null;  -- just in case
         end if;
      end Replaced_With_Dummy_Vertices;

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

      G     : Graph;
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

         V1 := Items.Element (Canvas_Link (It).From.Get_Toplevel_Item).Vertex;
         V2 := Items.Element (Canvas_Link (It).To.Get_Toplevel_Item).Vertex;
         E := new Canvas_Edge;
         E.Item := Canvas_Link (It);
         Add_Edge (G, E, V1, V2);
      end On_Link;

   begin
      Set_Directed (G, True);
      Self.For_Each_Item (On_Item'Access, Filter => Kind_Item);
      Self.For_Each_Item (On_Link'Access, Filter => Kind_Link);
      Layered_Layouts.Layout
        (G,
         Horizontal           => Horizontal,
         Space_Between_Layers => Space_Between_Layers,
         Space_Between_Items  => Space_Between_Items);
      Destroy (G);
      Self.Refresh_Layout;  --  recompute the links, and refresh views
   end Layout;

end Gtkada.Canvas_View.Models.Layers;
