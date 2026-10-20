with Ada.Command_Line;
with Interfaces.C;     use Interfaces.C;
with System;
with Glib;             use Glib;
with Glib.Test;        use Glib.Test;
with Glib.Object;
with Glib.Types;
with Graphene.Point;   use Graphene.Point;
with Graphene.Vec2;
with Graphene.Rect;    use Graphene.Rect;
with Gsk.Enums;        use Gsk.Enums;
with Gsk.Path;         use Gsk.Path;
with Gsk.Path_Builder; use Gsk.Path_Builder;
with Gsk.Path_Measure; use Gsk.Path_Measure;
with Gsk.Path_Point;   use Gsk.Path_Point;
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
   use type Pango.Font.Pango_Font_Description;
   procedure Near (Actual, Expected : C_float) is
   begin
      Assert_Cmpfloat_With_Epsilon
        (Gdouble (Actual), Gdouble (Expected), 0.01);
   end Near;

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

   procedure Test_Measure with
     Convention => C;
   procedure Test_Measure is
      Builder                : Gsk_Path_Builder;
      Path, Segment          : Gsk_Path;
      Measure                : Gsk_Path_Measure;
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
      Gtk_New (Measure, Path);
      Near (Measure.Get_Length, 100.0);
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
      Measure.Unref;
      Path.Unref;
   end Test_Measure;

   procedure Test_Stroke with
     Convention => C;
   procedure Test_Stroke is
      Stroke : constant Gsk_Stroke          := Gsk_Stroke_New (8.0);
      Other  : Gsk_Stroke;
      Dashes : constant Dash_Array (4 .. 6) := (12.0, 4.0, 2.0);
      Path   : constant Gsk_Path            := Parse ("M 0 0 L 100 0");
      Bounds : aliased Graphene_Rect_T;
   begin
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
     (Op       : Gsk_Path_Operation;
      Points   : access constant Graphene_Point_T;
      N_Points : Gsize; Weight : C_float; Data : System.Address)
      return Gboolean with
     Convention => C;
   function Visit
     (Op       : Gsk_Path_Operation;
      Points   : access constant Graphene_Point_T;
      N_Points : Gsize; Weight : C_float; Data : System.Address)
      return Gboolean
   is
      pragma Unreferenced (Weight, Data);
      type Point_Buffer is array (0 .. 3) of Graphene_Point_T;
      Buffer : Point_Buffer with
        Import, Address => Points.all'Address;
   begin
      Operation_Count := Operation_Count + 1;
      if Op = Gsk_Path_Cubic then
         Assert_True (N_Points = 4);
         Near (Buffer (3).X, 100.0);
      end if;
      return 1;
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
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/gsk/path/parse-fill", Test_Parse'Unrestricted_Access);
   Add_Func ("/gsk/path/measure-segment", Test_Measure'Unrestricted_Access);
   Add_Func ("/gsk/path/stroke", Test_Stroke'Unrestricted_Access);
   Add_Func ("/gsk/path/build-render", Test_Build_Render'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Paths;
