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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Gtkada.Canvas_View.Rtrees is

   package Box_Lists is new Ada.Containers.Doubly_Linked_Lists (Box_Access);

   function Choose_Leaf_Node
      (Self : Rtree; Rect : Model_Rectangle) return Box_Access;
   --  Choose the best node to insert Rect into, starting at the root.
   --  It never returns a leaf node, only a node that accepts children.

   function Least_Enlargement
      (Nodes : Box_Array; Rect : Model_Rectangle)
      return not null Box_Access;
   --  Returns the node from Nodes that would require the least enlargement to
   --  contain Rect.

   procedure Linear_Pick_Seeds
      (Width, Height : Gdouble;
       Nodes : Box_Array;
       Node1, Node2 : out Box_Access);
   --  Select, among the children of a box (Nodes), the two that are less
   --  likely to be in the same parent after a split. This uses the linear
   --  search proposed in the original paper on R-Trees.
   --  Width and Height are the total dimensions of the box.

   procedure Internal_Find
      (Self : Rtree;
       Rect : Model_Rectangle;
       Callback : not null access procedure (Node : Box_Access));
   --  Calls Callback for each item in the given area.

   procedure Add_Child (Self : Box_Access; Child : Box_Access);
   --  Add a new child. This doesn't update the bounding boxes or ensures that
   --  the number of children is kept below the threshold.

   procedure Recompute_Bounding_Box (Self : Box_Access);
   --  Recompute the tightest bounding box for all children of Self.

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child (Self : Box_Access; Child : Box_Access) is
   begin
      for C in Self.Children'Range loop
         if Self.Children (C) = null then
            Self.Children (C) := Child;
            Child.Parent := Self;
            return;
         end if;
      end loop;
   end Add_Child;

   ----------------------------
   -- Recompute_Bounding_Box --
   ----------------------------

   procedure Recompute_Bounding_Box (Self : Box_Access) is
      C : Box_Access;
      P : Box_Access := Self;
   begin
      while P /= null loop
         C := P.Children (P.Children'First);
         if C = null then
            P.Rect := (0.0, 0.0, 0.0, 0.0);
         else
            P.Rect := C.Rect;

            for Child in P.Children'First + 1 .. P.Children'Last loop
               C := P.Children (Child);
               exit when C = null;
               Union (P.Rect, C.Rect);
            end loop;
         end if;

         P := P.Parent;
      end loop;
   end Recompute_Bounding_Box;

   -----------------------
   -- Least_Enlargement --
   -----------------------

   function Least_Enlargement
      (Nodes : Box_Array; Rect : Model_Rectangle)
      return not null Box_Access
   is
      Best_Choice : Box_Access;
      Best_Choice_Enlarge : Gdouble := Gdouble'Last;
      Ltree       : Box_Access;
      New_Width, New_Height, Enlarge, Old_Ratio : Gdouble;
   begin
      for C in Nodes'Range loop
         Ltree := Nodes (C);
         exit when Ltree = null;

         Old_Ratio := Ltree.Rect.Width * Ltree.Rect.Height;
         New_Width := Gdouble'Max
            (Ltree.Rect.X + Ltree.Rect.Width, Rect.X + Rect.Width)
            - Gdouble'Min (Ltree.Rect.X, Rect.X);
         New_Height := Gdouble'Max
            (Ltree.Rect.Y + Ltree.Rect.Height, Rect.Y + Rect.Height)
            - Gdouble'Min (Ltree.Rect.Y, Rect.Y);
         Enlarge := abs (New_Width * New_Height - Old_Ratio);

         if Enlarge < Best_Choice_Enlarge then
            Best_Choice := Ltree;
            Best_Choice_Enlarge := Enlarge;
         end if;
      end loop;
      return Best_Choice;
   end Least_Enlargement;

   ----------------------
   -- Choose_Leaf_Node --
   ----------------------

   function Choose_Leaf_Node
      (Self : Rtree; Rect : Model_Rectangle) return Box_Access
   is
      Best_Choice : Box_Access := Self.Root;
      C           : Box_Access;
   begin
      --  Stop when we can go no further down in the tree (nodes below are
      --  leaves)

      loop
         C := Best_Choice.Children (Best_Choice.Children'First);
         exit when C = null or else C.Object /= null;

         Best_Choice := Least_Enlargement (Best_Choice.Children, Rect);
      end loop;
      return Best_Choice;
   end Choose_Leaf_Node;

   -----------------------
   -- Linear_Pick_Seeds --
   -----------------------

   procedure Linear_Pick_Seeds
      (Width, Height : Gdouble;
       Nodes : Box_Array;
       Node1, Node2 : out Box_Access)
   is
      --  Find extreme rectangles along all dimensions.
      --  Along each dimensions, find the entry whose rectangle has the
      --  highest low side, and the one with the lowest high-side. Record
      --  the separations.

      X_High : Box_Access := Nodes (Nodes'First);  --  highest low x
      X_Low  : Box_Access := X_High;
      Highest_Low_X : Gdouble := X_Low.Rect.X;
      Lowest_High_X : Gdouble := X_High.Rect.X + X_High.Rect.Width;

      Y_High : Box_Access := Nodes (Nodes'First);  --  highest low y
      Y_Low  : Box_Access := Y_High;
      Highest_Low_Y : Gdouble := Y_Low.Rect.Y;
      Lowest_High_Y : Gdouble := Y_High.Rect.Y + Y_High.Rect.Height;

      N : Box_Access;
      Candidate_X, Candidate_Y : Gdouble;
   begin
      for C in Nodes'First + 1 .. Nodes'Last loop
         N := Nodes (C);
         exit when N = null;

         if N.Rect.X >= Highest_Low_X then
            X_Low := N;
            Highest_Low_X := N.Rect.X;
         elsif N.Rect.X + N.Rect.Width <= Lowest_High_X then
            X_High := N;
            Lowest_High_X := N.Rect.X + N.Rect.Width;
         end if;

         if N.Rect.Y >= Highest_Low_Y then
            Y_Low := N;
            Highest_Low_Y := N.Rect.Y;
         elsif N.Rect.Y + N.Rect.Height <= Lowest_High_Y then
            Y_High := N;
            Lowest_High_Y := N.Rect.Y + N.Rect.Height;
         end if;
      end loop;

      --  Adjust the shape of the rectangle cluster
      --  Normalize the separations by dividing by the width of the entire
      --  set along the corresponding dimensions.

      Candidate_X := abs (Lowest_High_X - Highest_Low_X) / Width;
      Candidate_Y := abs (Lowest_High_Y - Highest_Low_Y) / Height;

      --  Select the most extreme pair (the pair with the greatest normalized
      --  separation along any dimensions)

      if Candidate_X > Candidate_Y
         and then X_Low /= X_High
      then
         Node1 := X_Low;
         Node2 := X_High;
      elsif Y_Low /= Y_High then
         Node1 := Y_Low;
         Node2 := Y_High;
      else
         --  One node encapsulates all the others.
         Node1 := X_Low;
         for C in Nodes'Range loop
            if Nodes (C) /= Node1 then
               Node2 := Nodes (C);
               exit;
            end if;
         end loop;
      end if;
   end Linear_Pick_Seeds;

   -------------------
   -- Internal_Find --
   -------------------

   procedure Internal_Find
      (Self : Rtree;
       Rect : Model_Rectangle;
       Callback : not null access procedure (Node : Box_Access))
   is
      use Box_Lists;
      To_Analyze : Box_Lists.List;
      Current, C : Box_Access;
   begin
      --  The implementation is non-recursive to improve efficiency

      if Self.Root /= null then
         To_Analyze.Append (Box_Access'(Self.Root));
         while not To_Analyze.Is_Empty loop
            Current := To_Analyze.First_Element;
            To_Analyze.Delete_First;

            for Child in Current.Children'Range loop
               C := Current.Children (Child);
               exit when C = null;

               if Rect = No_Rectangle
                  or else Intersects (Rect, C.Rect)
               then
                  if C.Object /= null then
                     Callback (C);
                  else
                     To_Analyze.Append (C);
                  end if;
               end if;
            end loop;
         end loop;
      end if;
   end Internal_Find;

   ----------
   -- Find --
   ----------

   function Find
      (Self : Rtree; Rect : Model_Rectangle) return Items_Lists.List
   is
      use Items_Lists;
      Results    : Items_Lists.List;
      procedure Append (Node : Box_Access);
      procedure Append (Node : Box_Access) is
      begin
         Results.Append (Node.Object);
      end Append;
   begin
      Internal_Find (Self, Rect, Append'Access);
      return Results;
   end Find;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Self : in out Rtree;
       Item : not null access Abstract_Item_Record'Class)
   is
      Child : constant Box_Access := new Box'
         (Max_Children_Plus_1 => 0,
          Rect         => Item.Model_Bounding_Box,
          Object       => Abstract_Item (Item),
          others       => <>);
      Parent, P, P2 : Box_Access;
      N1, N2        : Box_Access;
      New_Parent    : Box_Access;
      Old_Root      : Box_Access;
   begin
      if Self.Root = null then
         Self.Root := new Box'
            (Max_Children_Plus_1 => Self.Max_Children + 1,
             others => <>);
      end if;

      if Self.Root.Children (Self.Root.Children'First) = null then
         --  Initial insertion in an empty tree
         Add_Child (Self.Root, Child);
         Recompute_Bounding_Box (Self.Root);

      else
         --  Compute the best node to insert the new child. The returned node
         --  has leaves as children (i.e. they don't themselves contain nodes)

         Parent := Choose_Leaf_Node (Self, Child.Rect);
         Add_Child (Parent, Child);

         --  Walk up the tree and resize the bounding boxes as needed

         P := Parent;
         while P /= null loop
            Union (P.Rect, Child.Rect);
            P := P.Parent;
         end loop;

         --  Now split the nodes as needed when they are full: starting with
         --  the new parent A, we check if it has too many children. If yes,
         --  its parent will have one more child B. The children of A are then
         --  shared between A and B, where the algorithm tries to minimize the
         --  area of both A and B.
         --  Parent might now have too many children as well, so we go up the
         --  tree and normalize the nodes (we might eventually have to create
         --  a new root). This ensures a balanced tree.

         P := Parent;
         while P /= null and then P.Children (P.Children'Last) /= null loop
            Linear_Pick_Seeds
               (Width  => P.Rect.Width,
                Height => P.Rect.Height,
                Nodes  => P.Children,
                Node1  => N1,
                Node2  => N2);

            New_Parent := new Box'
               (Max_Children_Plus_1 => Self.Max_Children + 1,
                Rect                => N2.Rect,
                others              => <>);
            Add_Child (New_Parent, N2);

            declare
               Nodes : constant Box_Array := P.Children;
            begin
               P.Children := (1 => N1, others => null);
               P.Rect := N1.Rect;

               for C in Nodes'Range loop
                  exit when Nodes (C) = null;
                  if Nodes (C) /= N1 and then Nodes (C) /= N2 then
                     P2 := Least_Enlargement ((P, New_Parent), Nodes (C).Rect);
                     Add_Child (P2, Nodes (C));
                     Union (P2.Rect, Nodes (C).Rect);
                  end if;
               end loop;
            end;

            --  If we are splitting the root node, we need to create a new
            --  root

            if P.Parent = null then
               Old_Root := Self.Root;
               Self.Root := new Box'
                  (Max_Children_Plus_1 => Self.Max_Children + 1,
                   Rect => Old_Root.Rect,
                   others => <>);
               Add_Child (Self.Root, Old_Root);
               Add_Child (Self.Root, New_Parent);
               Union (Self.Root.Rect, New_Parent.Rect);
            else
               Add_Child (P.Parent, New_Parent);
            end if;

            P := P.Parent;
         end loop;
      end if;
   end Insert;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Rtree) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Box'Class, Box_Access);
      procedure Recurse (B : in out Box_Access);
      procedure Recurse (B : in out Box_Access) is
      begin
         for C in B.Children'Range loop
            exit when B.Children (C) = null;
            Recurse (B.Children (C));
         end loop;
         Unchecked_Free (B);
      end Recurse;
   begin
      if Self.Root /= null then
         Recurse (Self.Root);
      end if;
   end Clear;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Rtree) return Boolean is
   begin
      return Self.Root = null;
   end Is_Empty;

   ----------------
   -- Dump_Debug --
   ----------------

   procedure Dump_Debug (Self : Rtree) is
      procedure Internal (B : Box_Access; Prefix : String);
      procedure Internal (B : Box_Access; Prefix : String) is
      begin
         if B.Object /= null then
            Put_Line
               (Prefix & "[leaf "
                & Gdouble'Image (B.Rect.X) & Gdouble'Image (B.Rect.Y)
                & Gdouble'Image (B.Rect.Width) & 'x'
                & Gdouble'Image (B.Rect.Height)
                & ']');
         else
            Put_Line
               (Prefix & "["
                & Gdouble'Image (B.Rect.X) & Gdouble'Image (B.Rect.Y)
                & Gdouble'Image (B.Rect.Width) & 'x'
                & Gdouble'Image (B.Rect.Height));
            for C in B.Children'Range loop
               exit when B.Children (C) = null;
               Internal (B.Children (C), Prefix & "  ");
            end loop;
            Put_Line (Prefix & "]");
         end if;
      end Internal;
   begin
      if Self.Root /= null then
         Internal (Self.Root, "");
      end if;
   end Dump_Debug;

   ------------------
   -- Bounding_Box --
   ------------------

   function Bounding_Box (Self : Rtree) return Model_Rectangle is
   begin
      if Self.Root = null then
         return (0.0, 0.0, 0.0, 0.0);
      else
         return Self.Root.Rect;
      end if;
   end Bounding_Box;

   ---------------------
   -- For_Each_Object --
   ---------------------

   procedure For_Each_Object
      (Self     : Rtree;
       Callback : not null access procedure
          (Item : not null access Abstract_Item_Record'Class);
       In_Area  : Model_Rectangle := No_Rectangle)
   is
      procedure Append (Node : Box_Access);
      pragma Inline (Append);

      procedure Append (Node : Box_Access) is
      begin
         Callback (Node.Object);
      end Append;
   begin
      Internal_Find (Self, In_Area, Append'Access);
   end For_Each_Object;

end Gtkada.Canvas_View.Rtrees;
