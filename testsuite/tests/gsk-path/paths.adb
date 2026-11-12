--  Exercise Gsk.Path's string, bounds, point traversal, and foreach APIs.
--  GTK's C testsuite has no standalone GskPath test to port; this extends the
--  existing Ada coverage for parsing, measurements, strokes, and rendering.
--  It also checks Gsk.Stroke ownership and Cairo conversion; no focused C
--  testsuite case for gsk_stroke_* exists to port.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Interfaces.C;     use Interfaces.C;
with System;
with Glib;             use Glib;
with Glib.Test;        use Glib.Test;
with Glib.Object;
with Glib.String;      use Glib.String;
with Glib.Types;
with Graphene.Point;   use Graphene.Point;
with Graphene.Vec2;
with Graphene.Rect;    use Graphene.Rect;
with Gsk.Enums;        use Gsk.Enums;
with Gsk.Path;         use Gsk.Path;
with Gsk.Path_Builder; use Gsk.Path_Builder;
with Gsk.Path_Measure; use Gsk.Path_Measure;
with Gsk.Path_Point;   use Gsk.Path_Point;
with Gsk.Rounded_Rect; use Gsk.Rounded_Rect;
with Gsk.Stroke;       use Gsk.Stroke;
with Gdk.Paintable;
with Gdk.RGBA;
with Gtk.Main;
with Gtk.Snapshot;     use Gtk.Snapshot;
with Pango.Font;
with Pango.Layout;
with Gtk.Label;
with Cairo;
with Cairo.Image_Surface;

procedure Paths is
   use type System.Address;
   use type Cairo.Cairo_Line_Cap;
   use type Cairo.Cairo_Line_Join;
   use type Pango.Font.Pango_Font_Description;

   --  From_Object_Free frees its wrapper with g_free, so its temporary Ada
   --  record must be allocated by GLib and viewed through an access type.
   type Stroke_Access is access all Gsk_Stroke;
   function To_Stroke_Access is new Ada.Unchecked_Conversion
     (System.Address, Stroke_Access);

   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   --  Cairo.Get_Dash allocates a copy with Ada's allocator; release it after
   --  checking the dash values returned by Gsk.Stroke.To_Cairo.
   procedure Free_Dashes is new Ada.Unchecked_Deallocation
     (Cairo.Dash_Array, Cairo.Dash_Array_Access);

   procedure Near (Actual, Expected : C_float) is
   begin
      Assert_Cmpfloat_With_Epsilon
        (Gdouble (Actual), Gdouble (Expected), 0.01);
   end Near;

   --  Compare the builder's copied current point without exposing its pointer.
   procedure Assert_Current_Point
     (Builder : Gsk_Path_Builder; X, Y : C_float) is
      Point : constant Graphene_Point_T := Builder.Get_Current_Point;
   begin
      Near (Point.X, X);
      Near (Point.Y, Y);
   end Assert_Current_Point;

   procedure Test_Parse with
     Convention => C;
   procedure Test_Parse is
      Path      : constant Gsk_Path        :=
        Parse
          ("M 0 0 L 100 0 L 100 100 L 0 100 Z M 25 25 L 75 25 L 75 75 L 25 75 Z");
      Roundtrip : constant Gsk_Path        := Parse (Path.To_String);
      Bounds    : aliased Graphene_Rect_T;
      Point     : aliased Graphene_Point_T := (50.0, 50.0);
      Empty     : Gsk_Path;
      Builder   : Gsk_Path_Builder;
   begin
      Assert_Nonnull (Get_Object (Path));
      Assert_True (Path.Equal (Roundtrip));
      Assert_True (Path.Get_Bounds (Bounds'Access));
      Near (Bounds.Size.Width, 100.0);
      Near (Bounds.Size.Height, 100.0);
      Assert_True (Path.In_Fill (Point'Access, Gsk_Fill_Rule_Winding));
      Assert_False (Path.In_Fill (Point'Access, Gsk_Fill_Rule_Even_Odd));
      Assert_Null (Get_Object (Parse ("invalid SVG")));
      Gtk_New (Builder);
      Empty := Builder.To_Path;
      Assert_True (Empty.Is_Empty);
      declare
         Measure : constant Gsk_Path_Measure := Gsk_Path_Measure_New (Empty);
         Point   : aliased Gsk_Path_Point;
      begin
         Near (Measure.Get_Length, 0.0);
         Assert_False (Measure.Get_Point (0.0, Point'Access));
         Measure.Unref;
      end;
      Empty.Unref;
      Builder.Unref;
      Roundtrip.Unref;
      Path.Unref;
   end Test_Parse;

   --  The callback overload with user data uses a closure to keep the Ada
   --  value alive while C iterates. These globals let the callback verify
   --  the transferred value and stop at a predictable segment when asked.
   package User_Data_Foreach is new Gsk_Path_Foreach_User_Data (Natural);
   User_Data_Visits : Natural := 0;
   Stop_At_First    : Boolean := False;

   function Visit_With_User_Data
     (Op        : Gsk_Path_Operation;
      Points    : Graphene_Point_Array;
      Weight    : C_float;
      User_Data : Natural) return Boolean;
   function Visit_With_User_Data
     (Op        : Gsk_Path_Operation;
      Points    : Graphene_Point_Array;
      Weight    : C_float;
      User_Data : Natural) return Boolean
   is
      pragma Unreferenced (Op, Weight);
   begin
      User_Data_Visits := User_Data_Visits + 1;
      Assert_True (User_Data = 42);
      Assert_True (Points'Length > 0);
      return not Stop_At_First;
   end Visit_With_User_Data;

   --  Check the path-point APIs at both vertices and contour boundaries, plus
   --  the false results for empty paths and points with no adjacent vertex.
   procedure Test_Path_Queries with Convention => C;
   procedure Test_Path_Queries is
      Builder : Gsk_Path_Builder;
      Empty   : Gsk_Path;
      Path    : constant Gsk_Path := Parse ("M 0 0 L 10 0 L 10 10");
      Closed  : constant Gsk_Path := Parse ("M 0 0 L 10 0 L 10 10 Z");
      Stroke  : constant Gsk_Stroke := Gsk_Stroke_New (2.0);
      Bounds  : aliased Graphene_Rect_T;
      Start   : aliased Gsk_Path_Point;
      Finish  : aliased Gsk_Path_Point;
      Position : Graphene_Point_T;
      Output   : Gstring := Gstring_New;
      Held     : Gsk_Path;
      Has_Next : Boolean;
      Has_Previous : Boolean;
      Steps    : Natural;
   begin
      --  A nonempty path has both fast and tight bounds, and a stroke has
      --  measurable bounds as well.
      Assert_True (Path.Get_Bounds (Bounds'Access));
      Assert_True (Bounds.Size.Width >= 10.0);
      Assert_True (Path.Get_Tight_Bounds (Bounds'Access));
      Assert_True (Bounds.Size.Width >= 10.0);
      Assert_True (Path.Get_Stroke_Bounds (Stroke, Bounds'Access));
      Assert_True (Bounds.Size.Width >= 10.0);

      --  Start and end points can be traversed in either direction. At each
      --  end of the traversal the API reports False instead of inventing a
      --  point. The path point is opaque, so only query positions directly.
      Assert_True (Path.Get_Start_Point (Start'Access));
      Assert_True (Path.Get_End_Point (Finish'Access));
      Get_Position (Start'Access, Path, Position);
      Near (Position.X, 0.0);
      Near (Position.Y, 0.0);
      Get_Position (Finish'Access, Path, Position);
      Near (Position.X, 10.0);
      Near (Position.Y, 10.0);
      Has_Next := True;
      Steps := 0;
      while Has_Next and Steps < 8 loop
         Has_Next := Path.Get_Next (Start);
         Steps := Steps + 1;
      end loop;
      Assert_False (Has_Next);
      Assert_True (Steps > 1);
      Has_Previous := True;
      Steps := 0;
      while Has_Previous and Steps < 8 loop
         Has_Previous := Path.Get_Previous (Finish);
         Steps := Steps + 1;
      end loop;
      Assert_False (Has_Previous);
      Assert_True (Steps > 1);
      Assert_False (Path.Is_Closed);
      Assert_True (Closed.Is_Closed);

      --  Print writes the same canonical representation used by To_String.
      Path.Print (Output);
      declare
         Printed : constant UTF8_String := Free_And_Steal (Output);
      begin
         Assert_Cmpstr_Eq (Printed, Path.To_String);
      end;

      --  Ref returns an additional owned reference to the same path.
      Held := Path.Ref;
      Assert_True (Path.Equal (Held));
      Held.Unref;

      --  Empty paths have no bounds or endpoints; their bounds are reset to
      --  zero by all three bounds queries.
      Gtk_New (Builder);
      Empty := Builder.To_Path;
      Builder.Unref;
      Assert_False (Empty.Get_Bounds (Bounds'Access));
      Assert_False (Empty.Get_Tight_Bounds (Bounds'Access));
      Assert_False (Empty.Get_Stroke_Bounds (Stroke, Bounds'Access));
      Assert_True (Bounds.Size.Width = 0.0);
      Assert_True (Bounds.Size.Height = 0.0);
      Assert_False (Empty.Get_Start_Point (Start'Access));
      Assert_False (Empty.Get_End_Point (Finish'Access));

      --  Exercise the user-data callback through full iteration, then stop
      --  on its first call to cover the callback's early-termination result.
      User_Data_Visits := 0;
      Stop_At_First := False;
      Assert_True
        (User_Data_Foreach.Foreach
           (Path, Gsk_Path_Foreach_Allow_Cubic,
            Visit_With_User_Data'Unrestricted_Access, 42));
      Assert_True (User_Data_Visits > 1);
      User_Data_Visits := 0;
      Stop_At_First := True;
      Assert_False
        (User_Data_Foreach.Foreach
           (Path, Gsk_Path_Foreach_Allow_Cubic,
            Visit_With_User_Data'Unrestricted_Access, 42));
      Assert_True (User_Data_Visits = 1);

      Stroke.Free;
      Empty.Unref;
      Closed.Unref;
      Path.Unref;
   end Test_Path_Queries;

   procedure Test_Measure with
     Convention => C;
   procedure Test_Measure is
      Builder                : Gsk_Path_Builder;
      Path, Segment          : Gsk_Path;
      Measure                : Gsk_Path_Measure;
      Tolerance_Function     : Gsk_Path_Measure;
      Tolerance_Procedure    : Gsk_Path_Measure;
      Held_Measure           : Gsk_Path_Measure;
      Transfer_Measure       : Gsk_Path_Measure;
      Start, Finish, Closest : aliased Gsk_Path_Point;
      Position               : Graphene_Point_T;
      Query                  : aliased Graphene_Point_T := (50.0, 7.0);
      Distance               : aliased C_float;
      Copied                 : Gsk_Path_Point_Access;
      Tangent                : aliased Graphene.Vec2.Graphene_Vec2_T;
      function C_Size return Gsize;
      pragma Import (C, C_Size, "gtkada_test_path_point_size");
      function C_Alignment return Gsize;
      pragma Import (C, C_Alignment, "gtkada_test_path_point_alignment");
      function G_Malloc (Size : Gsize) return System.Address;
      pragma Import (C, G_Malloc, "g_malloc");
      type Measure_Access is access all Gsk_Path_Measure;
      function To_Measure_Access is new Ada.Unchecked_Conversion
        (System.Address, Measure_Access);
      Wrapper_Bytes : constant Gsize :=
        Gsize ((Gsk_Path_Measure'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
   begin
      Assert_True (Gsize (Gsk_Path_Point'Size / System.Storage_Unit) = C_Size);
      Assert_True (Gsize (Gsk_Path_Point'Alignment) = C_Alignment);
      Gtk_New (Builder);
      Builder.Move_To (0.0, 0.0);
      Builder.Line_To (100.0, 0.0);
      Position := Builder.Get_Current_Point;
      Near (Position.X, 100.0);
      Path := Builder.To_Path;
      Builder.Unref;
      Tolerance_Function :=
        Gsk_Path_Measure_New_With_Tolerance (Path, 0.25);
      Gtk_New (Measure, Path);
      Near (Measure.Get_Length, 100.0);
      --  Get_Path is borrowed; inspect it while its measure remains alive.
      Assert_True (Measure.Get_Path.Equal (Path));
      --  Both tolerance constructor forms retain the requested tolerance and
      --  measure the same straight segment with its exact length.
      Gtk_New_With_Tolerance (Tolerance_Procedure, Path, 0.25);
      Near (Tolerance_Function.Get_Tolerance, 0.25);
      Near (Tolerance_Procedure.Get_Tolerance, 0.25);
      Near (Tolerance_Function.Get_Length, 100.0);
      Near (Tolerance_Procedure.Get_Length, 100.0);
      Tolerance_Function.Unref;
      Tolerance_Procedure.Unref;
      --  Ref returns an independently owned reference to the same measure.
      Held_Measure := Measure.Ref;
      Near (Held_Measure.Get_Length, 100.0);
      Held_Measure.Unref;
      Assert_True (Measure.Get_Point (20.0, Start'Access));
      Assert_True (Measure.Get_Point (80.0, Finish'Access));
      Get_Position (Finish'Access, Path, Position);
      Near (Position.X, 80.0);
      Near (Position.Y, 0.0);
      Near (Get_Distance (Finish'Access, Measure), 80.0);
      Near (Get_Rotation (Finish'Access, Path, Gsk_Path_To_End), 0.0);
      Get_Tangent (Finish'Access, Path, Gsk_Path_To_End, Tangent);
      Near (Graphene.Vec2.Get_X (Tangent'Access), 1.0);
      Near (Graphene.Vec2.Get_Y (Tangent'Access), 0.0);
      Near (Get_Curvature (Finish'Access, Path, Gsk_Path_To_End), 0.0);
      Assert_True (Compare (Start'Access, Finish'Access) < 0);
      Copied := Copy (Finish'Access);
      Assert_True (Equal (Copied, Finish'Access));
      Free (Copied);
      Assert_True
        (Path.Get_Closest_Point
           (Query'Access, 10.0, Closest'Access, Distance'Access));
      Near (Distance, 7.0);
      Get_Position (Closest'Access, Path, Position);
      Near (Position.X, 50.0);
      Assert_False
        (Path.Get_Closest_Point (Query'Access, 1.0, Closest'Access));
      Gtk_New (Builder);
      Builder.Add_Segment (Path, Start'Access, Finish'Access);
      Segment := Builder.To_Path;
      Builder.Unref;
      declare
         Submeasure : constant Gsk_Path_Measure :=
           Gsk_Path_Measure_New (Segment);
      begin
         Near (Submeasure.Get_Length, 60.0);
         Submeasure.Unref;
      end;
      Segment.Unref;

      --  From_Object_Free consumes a wrapper allocated with GLib's allocator.
      --  Move the one native reference into that wrapper to avoid double-unref.
      declare
         Wrapper : constant Measure_Access :=
           To_Measure_Access (G_Malloc (Wrapper_Bytes));
         Converted : Gsk_Path_Measure;
      begin
         Transfer_Measure := Gsk_Path_Measure_New (Path);
         Wrapper.all := Transfer_Measure;
         Transfer_Measure.Set_Object (System.Null_Address);
         Converted := From_Object_Free (Wrapper);
         Near (Converted.Get_Length, 100.0);
         Converted.Unref;
      end;
      Measure.Unref;
      Path.Unref;
   end Test_Measure;

   procedure Test_Stroke with
     Convention => C;
   procedure Test_Stroke is
      Stroke : constant Gsk_Stroke          := Gsk_Stroke_New (8.0);
      Other  : Gsk_Stroke;
      Procedure_Stroke : Gsk_Stroke;
      Dashes : constant Dash_Array (4 .. 6) := (12.0, 4.0, 2.0);
      Path   : constant Gsk_Path            := Parse ("M 0 0 L 100 0");
      Bounds : aliased Graphene_Rect_T;
   begin
      --  Exercise the procedure constructor and the width accessors as well
      --  as the function constructor used below.
      Gtk_New (Procedure_Stroke, 6.0);
      Near (Procedure_Stroke.Get_Line_Width, 6.0);
      Procedure_Stroke.Set_Line_Width (7.0);
      Near (Procedure_Stroke.Get_Line_Width, 7.0);
      Procedure_Stroke.Free;

      Stroke.Set_Dash (Dashes);
      Assert_True (Stroke.Get_Dash = Dashes);
      Stroke.Set_Dash_Offset (3.0);
      Stroke.Set_Line_Cap (Gsk_Line_Cap_Round);
      Stroke.Set_Line_Join (Gsk_Line_Join_Bevel);
      Stroke.Set_Miter_Limit (5.0);
      Other := Stroke.Copy;
      Assert_True (Gsk.Stroke.Equal (Stroke, Other));
      Near (Other.Get_Dash_Offset, 3.0);
      Assert_True (Other.Get_Line_Cap = Gsk_Line_Cap_Round);
      Assert_True (Other.Get_Line_Join = Gsk_Line_Join_Bevel);
      Near (Other.Get_Miter_Limit, 5.0);

      --  Configure a memory-backed Cairo context and verify that the helper
      --  transfers the stroke parameters, including its dash offset.
      declare
         Surface : Cairo.Cairo_Surface := Cairo.Image_Surface.Create
           (Cairo.Image_Surface.Cairo_Format_ARGB32, 20, 20);
         Cr : Cairo.Cairo_Context := Cairo.Create (Surface);
         Cairo_Dashes : Cairo.Dash_Array_Access;
         Cairo_Offset : Glib.Gdouble;
      begin
         Stroke.To_Cairo (Cr);
         Assert_Cmpfloat_With_Epsilon
           (Cairo.Get_Line_Width (Cr), 8.0, 0.01);
         Assert_True
           (Cairo.Get_Line_Cap (Cr) = Cairo.Cairo_Line_Cap_Round);
         Assert_True
           (Cairo.Get_Line_Join (Cr) = Cairo.Cairo_Line_Join_Bevel);
         Assert_Cmpfloat_With_Epsilon
           (Cairo.Get_Miter_Limit (Cr), 5.0, 0.01);
         Assert_Cmpint_Eq (Cairo.Get_Dash_Count (Cr), 3);
         Cairo.Get_Dash (Cr, Cairo_Dashes, Cairo_Offset);
         Assert_Cmpfloat_With_Epsilon (Cairo_Offset, 3.0, 0.01);
         Assert_Cmpfloat_With_Epsilon (Cairo_Dashes (1), 12.0, 0.01);
         Assert_Cmpfloat_With_Epsilon (Cairo_Dashes (2), 4.0, 0.01);
         Assert_Cmpfloat_With_Epsilon (Cairo_Dashes (3), 2.0, 0.01);
         Free_Dashes (Cairo_Dashes);
         Cairo.Destroy (Cr);
         Cairo.Surface_Destroy (Surface);
      end;

      --  The helper copies the boxed handle and frees only the temporary
      --  record, so give it an independently owned stroke value to convert.
      declare
         Bytes : constant size_t :=
           size_t ((Gsk_Stroke'Size + System.Storage_Unit - 1)
                   / System.Storage_Unit);
         Wrapper : constant Stroke_Access :=
           To_Stroke_Access (G_Malloc (Bytes));
         Converted : Gsk_Stroke;
      begin
         Wrapper.all := Stroke.Copy;
         Converted := From_Object_Free (Wrapper);
         Near (Converted.Get_Line_Width, 8.0);
         Assert_True (Gsk.Stroke.Equal (Stroke, Converted));
         Converted.Free;
      end;

      Stroke.Set_Dash ((1 .. 0 => 0.0));
      Assert_True (Stroke.Get_Dash'Length = 0);
      Assert_True (Other.Get_Dash = Dashes);
      Assert_True (Path.Get_Stroke_Bounds (Stroke, Bounds'Access));
      Assert_True (Bounds.Size.Height >= 8.0);
      Other.Free;
      Stroke.Free;
      Path.Unref;
   end Test_Stroke;

   Operation_Count : Natural := 0;
   function Visit
     (Op     : Gsk_Path_Operation;
      Points : Graphene_Point_Array;
      Weight : C_float)
      return Boolean;
   function Visit
     (Op     : Gsk_Path_Operation;
      Points : Graphene_Point_Array;
      Weight : C_float)
      return Boolean
   is
      pragma Unreferenced (Weight);
   begin
      Operation_Count := Operation_Count + 1;
      if Op = Gsk_Path_Cubic then
         Assert_True (Points'Length = 4);
         Near (Points (Points'Last).X, 100.0);
      end if;
      return True;
   end Visit;

   procedure Test_Build_Render with
     Convention => C;
   procedure Test_Build_Render is
      Builder    : Gsk_Path_Builder;
      Path       : Gsk_Path;
      Stroke     : constant Gsk_Stroke               := Gsk_Stroke_New (4.0);
      Label      : Gtk.Label.Gtk_Label;
      Layout     : Pango.Layout.Pango_Layout;
      Font       : Pango.Font.Pango_Font_Description :=
        Pango.Font.From_String ("Sans 30");
      Bounds     : aliased Graphene_Rect_T := ((0.0, 0.0), (200.0, 100.0));
      Color      : constant Gdk.RGBA.Gdk_RGBA        := (0.2, 0.4, 0.8, 1.0);
      Snap       : Gtk_Snapshot;
      Paintable  : Gdk.Paintable.Gdk_Paintable;
      Surface    : Cairo.Cairo_Surface               :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32, 200, 100);
      Cr         : Cairo.Cairo_Context               := Cairo.Create (Surface);
      Cairo_Path : Cairo.Cairo_Path_Access;
   begin
      Path := Parse ("M 0 0 C 20 40 80 40 100 0");
      Assert_True
        (Path.Foreach
           (Gsk_Path_Foreach_Allow_Cubic, Visit'Unrestricted_Access));
      Assert_True (Operation_Count = 2);
      Path.To_Cairo (Cr);
      Cairo_Path := Cairo.Copy_Path (Cr);
      Gtk_New (Builder);
      Builder.Add_Cairo_Path (Cairo_Path);
      Cairo.Path_Destroy (Cairo_Path);
      Path.Unref;
      Path := Builder.To_Path;
      Assert_False (Path.Is_Empty);
      Path.Unref;
      Builder.Unref;
      Gtk.Label.Gtk_New (Label, "Path");
      Label.Ref_Sink;
      Layout := Label.Create_Pango_Layout ("Path");
      Layout.Set_Font_Description (Font);
      Gtk_New (Builder);
      Builder.Add_Layout (Layout);
      Path := Builder.To_Path;
      Builder.Unref;
      Assert_False (Path.Is_Empty);
      for Clip in Boolean loop
         Gtk_New (Snap);
         if Clip then
            Snap.Push_Fill (Path, Gsk_Fill_Rule_Winding);
            Snap.Append_Color (Color, Bounds'Access);
            Snap.Pop;
            Snap.Push_Stroke (Path, Stroke);
            Snap.Append_Color (Color, Bounds'Access);
            Snap.Pop;
         else
            Snap.Append_Fill (Path, Gsk_Fill_Rule_Winding, Color);
            Snap.Append_Stroke (Path, Stroke, Color);
         end if;
         Paintable := Snap.To_Paintable (null);
         Assert_Nonnull (System.Address (Paintable));
         Glib.Types.To_Object (Glib.Types.GType_Interface (Paintable)).Unref;
         Snap.Unref;
      end loop;
      Path.Unref;
      Stroke.Free;
      Layout.Unref;
      Label.Unref;
      Pango.Font.Free (Font);
      Cairo.Destroy (Cr);
      Cairo.Surface_Destroy (Surface);
   end Test_Build_Render;

   --  Exercise both constructors and ownership conversions, then verify that
   --  every absolute and relative drawing command updates the path as stated.
   procedure Test_Builder_Curves with Convention => C;
   procedure Test_Builder_Curves is
      function G_Malloc (Size : Gsize) return System.Address;
      pragma Import (C, G_Malloc, "g_malloc");
      type Builder_Access is access all Gsk_Path_Builder;
      function To_Builder_Access is new Ada.Unchecked_Conversion
        (System.Address, Builder_Access);

      Builder       : Gsk_Path_Builder;
      Function_Builder : Gsk_Path_Builder;
      Ref_Builder   : Gsk_Path_Builder;
      Converted     : Gsk_Path_Builder;
      Borrowed      : Gsk_Path_Builder;
      Path          : Gsk_Path;
      Wrapper_Bytes : constant Gsize :=
        Gsize ((Gsk_Path_Builder'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
      Wrapper       : constant Builder_Access :=
        To_Builder_Access (G_Malloc (Wrapper_Bytes));
   begin
      --  The function and procedure constructors both create usable builders.
      Function_Builder := Gsk_Path_Builder_New;
      Gtk_New (Builder);
      Ref_Builder := Builder.Ref;
      Borrowed := From_Object (Get_Object (Builder));
      Assert_True (Get_Object (Borrowed) = Get_Object (Builder));
      Assert_True (Get_Object (Ref_Builder) = Get_Object (Builder));

      --  From_Object_Free frees only the GLib-allocated wrapper record; the
      --  original builder remains the single owner of this native reference.
      Wrapper.all := Builder;
      Converted := From_Object_Free (Wrapper);
      Assert_True (Get_Object (Converted) = Get_Object (Builder));

      --  Absolute commands establish and then advance the current point.
      Builder.Move_To (0.0, 0.0);
      Assert_Current_Point (Builder, 0.0, 0.0);
      Builder.Line_To (10.0, 0.0);
      Assert_Current_Point (Builder, 10.0, 0.0);
      Builder.Quad_To (12.0, 2.0, 14.0, 0.0);
      Assert_Current_Point (Builder, 14.0, 0.0);
      Builder.Cubic_To (16.0, 2.0, 18.0, 2.0, 20.0, 0.0);
      Assert_Current_Point (Builder, 20.0, 0.0);
      Builder.Conic_To (22.0, 2.0, 24.0, 0.0, 1.0);
      Assert_Current_Point (Builder, 24.0, 0.0);
      Builder.Svg_Arc_To (8.0, 8.0, 0.0, False, True, 40.0, 0.0);
      Assert_Current_Point (Builder, 40.0, 0.0);
      Builder.Arc_To (45.0, 0.0, 45.0, 5.0);
      Assert_Current_Point (Builder, 45.0, 5.0);
      Builder.Html_Arc_To (50.0, 5.0, 50.0, 10.0, 2.0);
      Builder.Close;
      Path := Builder.To_Path;
      Assert_False (Path.Is_Empty);
      Assert_True (Path.Is_Closed);
      Path.Unref;

      --  Relative commands use offsets from the current point, including
      --  relative arc endpoints; positive conic weights are required by GSK.
      Builder.Move_To (0.0, 0.0);
      Builder.Rel_Move_To (1.0, 2.0);
      Assert_Current_Point (Builder, 1.0, 2.0);
      Builder.Rel_Line_To (2.0, 1.0);
      Assert_Current_Point (Builder, 3.0, 3.0);
      Builder.Rel_Quad_To (1.0, 2.0, 3.0, 0.0);
      Assert_Current_Point (Builder, 6.0, 3.0);
      Builder.Rel_Cubic_To (1.0, 1.0, 2.0, 2.0, 4.0, 0.0);
      Assert_Current_Point (Builder, 10.0, 3.0);
      Builder.Rel_Conic_To (1.0, 2.0, 3.0, 0.0, 1.0);
      Assert_Current_Point (Builder, 13.0, 3.0);
      Builder.Rel_Svg_Arc_To (6.0, 6.0, 0.0, False, True, 10.0, 0.0);
      Assert_Current_Point (Builder, 23.0, 3.0);
      Builder.Rel_Arc_To (5.0, 0.0, 5.0, 5.0);
      Assert_Current_Point (Builder, 28.0, 8.0);
      Builder.Rel_Html_Arc_To (5.0, 0.0, 5.0, 5.0, 2.0);
      Builder.Close;
      Path := Builder.To_Path;
      Assert_False (Path.Is_Empty);
      Path.Unref;

      --  To_Path resets the builder, and Ref owns an additional native ref.
      Path := Builder.To_Path;
      Assert_True (Path.Is_Empty);
      Path.Unref;
      Function_Builder.Unref;
      Ref_Builder.Unref;
      Builder.Unref;
   end Test_Builder_Curves;

   --  The predefined-shape methods append complete contours without changing
   --  the current point; path composition and segment extraction are checked
   --  through the resulting bounds and contour endpoints.
   procedure Test_Builder_Contours with Convention => C;
   procedure Test_Builder_Contours is
      Builder : Gsk_Path_Builder;
      Source  : constant Gsk_Path := Parse ("M 0 0 L 10 0 L 10 10 Z");
      Start   : aliased Gsk_Path_Point;
      Finish  : aliased Gsk_Path_Point;
      Center  : aliased Graphene_Point_T := (20.0, 20.0);
      Rect    : aliased Graphene_Rect_T := ((30.0, 0.0), (10.0, 8.0));
      Bounds  : aliased Graphene_Rect_T;
      Rounded : aliased Gsk_Rounded_Rect;
      Path    : Gsk_Path;
   begin
      --  Use endpoints from a real path when extracting its open segment.
      Assert_True (Source.Get_Start_Point (Start'Access));
      Assert_True (Source.Get_End_Point (Finish'Access));
      Rounded := Gsk.Rounded_Rect.Init_From_Rect
        (Rounded'Access, Rect'Access, 2.0).all;

      Gtk_New (Builder);
      Add_Circle (Builder, Center'Access, 5.0);
      Add_Rect (Builder, Rect'Access);
      Add_Rounded_Rect (Builder, Rounded);
      Add_Path (Builder, Source);
      Add_Reverse_Path (Builder, Source);
      Add_Segment (Builder, Source, Start'Access, Finish'Access);
      Path := Builder.To_Path;
      Assert_False (Path.Is_Empty);
      Assert_True (Path.Get_Bounds (Bounds'Access));
      Assert_True (Bounds.Size.Width >= 40.0);
      Assert_True (Bounds.Size.Height >= 25.0);
      Path.Unref;
      Builder.Unref;
      Source.Unref;
   end Test_Builder_Contours;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/gsk/path/parse-fill", Test_Parse'Unrestricted_Access);
   Add_Func ("/gsk/path/queries", Test_Path_Queries'Unrestricted_Access);
   Add_Func ("/gsk/path/measure-segment", Test_Measure'Unrestricted_Access);
   Add_Func ("/gsk/path/stroke", Test_Stroke'Unrestricted_Access);
   Add_Func ("/gsk/path/build-render", Test_Build_Render'Unrestricted_Access);
   Add_Func ("/gsk/path/builder-curves", Test_Builder_Curves'Unrestricted_Access);
   Add_Func ("/gsk/path/builder-contours", Test_Builder_Contours'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Paths;
