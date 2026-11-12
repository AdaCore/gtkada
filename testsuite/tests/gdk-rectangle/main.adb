--  Exercise Gdk.Rectangle point containment, equality, intersection, and the
--  temporary-wrapper ownership helper. There is no matching C test to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Gdk.Rectangle;
with Glib;
with Glib.Test;

procedure Main is
   use Glib.Test;
   use type Glib.Gint;

   procedure Test_Contains_Point with Convention => C;
   procedure Test_Equal with Convention => C;
   procedure Test_Intersect with Convention => C;
   procedure Test_From_Object_Free with Convention => C;

   --  From_Object_Free releases its wrapper with g_free, so allocate the
   --  record through GLib and view that aligned storage as an Ada record.
   type Rectangle_Access is access all Gdk.Rectangle.Gdk_Rectangle;
   function To_Rectangle_Access is new Ada.Unchecked_Conversion
     (System.Address, Rectangle_Access);

   --------------------------
   -- Test_Contains_Point --
   --------------------------

   --  Rectangle coordinates include the top and left edges and exclude the
   --  right and bottom edges.
   procedure Test_Contains_Point is
      Rect : constant Gdk.Rectangle.Gdk_Rectangle :=
        (X => 10, Y => 20, Width => 30, Height => 40);
   begin
      Assert_True (Gdk.Rectangle.Contains_Point (Rect, 10, 20));
      Assert_True (Gdk.Rectangle.Contains_Point (Rect, 39, 59));
      Assert_False (Gdk.Rectangle.Contains_Point (Rect, 40, 30));
      Assert_False (Gdk.Rectangle.Contains_Point (Rect, 30, 60));
   end Test_Contains_Point;

   ----------------
   -- Test_Equal --
   ----------------

   --  Equal accepts identical values and detects a change to each field.
   procedure Test_Equal is
      Rect : constant Gdk.Rectangle.Gdk_Rectangle :=
        (X => 10, Y => 20, Width => 30, Height => 40);
   begin
      Assert_True (Gdk.Rectangle.Equal (Rect, Rect));
      Assert_False
        (Gdk.Rectangle.Equal (Rect, (X => 11, Y => 20, Width => 30, Height => 40)));
      Assert_False
        (Gdk.Rectangle.Equal (Rect, (X => 10, Y => 21, Width => 30, Height => 40)));
      Assert_False
        (Gdk.Rectangle.Equal (Rect, (X => 10, Y => 20, Width => 31, Height => 40)));
      Assert_False
        (Gdk.Rectangle.Equal (Rect, (X => 10, Y => 20, Width => 30, Height => 41)));
   end Test_Equal;

   --------------------
   -- Test_Intersect --
   --------------------

   --  Check the computed overlap and the no-overlap result. The API leaves
   --  the destination origin undefined when the rectangles do not intersect.
   procedure Test_Intersect is
      First : constant Gdk.Rectangle.Gdk_Rectangle :=
        (X => 10, Y => 20, Width => 30, Height => 40);
      Overlapping : constant Gdk.Rectangle.Gdk_Rectangle :=
        (X => 25, Y => 35, Width => 30, Height => 40);
      Disjoint : constant Gdk.Rectangle.Gdk_Rectangle :=
        (X => 100, Y => 100, Width => 5, Height => 5);
      Area : Gdk.Rectangle.Gdk_Rectangle;
      Intersects : Boolean;
   begin
      Gdk.Rectangle.Intersect (First, Overlapping, Area, Intersects);
      Assert_True (Intersects);
      Assert_Cmpint_Eq (Area.X, 25);
      Assert_Cmpint_Eq (Area.Y, 35);
      Assert_Cmpint_Eq (Area.Width, 15);
      Assert_Cmpint_Eq (Area.Height, 25);

      Gdk.Rectangle.Intersect (First, Disjoint, Area, Intersects);
      Assert_False (Intersects);
      Assert_Cmpint_Eq (Area.Width, 0);
      Assert_Cmpint_Eq (Area.Height, 0);
   end Test_Intersect;

   --------------------------
   -- Test_From_Object_Free --
   --------------------------

   --  Conversion copies all four fields and consumes the GLib-owned wrapper.
   procedure Test_From_Object_Free is
      function G_Malloc (Size : Glib.Gsize) return System.Address;
      pragma Import (C, G_Malloc, "g_malloc");

      Value : constant Gdk.Rectangle.Gdk_Rectangle :=
        (X => -7, Y => 12, Width => 34, Height => 56);
      Bytes : constant Glib.Gsize :=
        Glib.Gsize
          ((Gdk.Rectangle.Gdk_Rectangle'Size + System.Storage_Unit - 1)
           / System.Storage_Unit);
      Wrapper : constant Rectangle_Access :=
        To_Rectangle_Access (G_Malloc (Bytes));
      Converted : Gdk.Rectangle.Gdk_Rectangle;
   begin
      Wrapper.all := Value;
      Converted := Gdk.Rectangle.From_Object_Free (Wrapper);
      Assert_Cmpint_Eq (Converted.X, Value.X);
      Assert_Cmpint_Eq (Converted.Y, Value.Y);
      Assert_Cmpint_Eq (Converted.Width, Value.Width);
      Assert_Cmpint_Eq (Converted.Height, Value.Height);
   end Test_From_Object_Free;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gdk/rectangle/contains-point",
      Test_Contains_Point'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk/rectangle/equal", Test_Equal'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk/rectangle/intersect", Test_Intersect'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk/rectangle/from-object-free",
      Test_From_Object_Free'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
