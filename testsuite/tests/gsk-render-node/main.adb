--  Exercise the public Gsk.Render_Node methods with colour, container, and
--  gradient nodes. There is no focused C render-node test to port; symbolic.c
--  only constructs and unreferences a colour node as part of a larger test.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Cairo;
with Cairo.Image_Surface;
with Gdk.RGBA;
with Glib;
with Glib.Bytes;
with Glib.Error;
with Glib.Test;
with Graphene.Point;
with Graphene.Rect;
with Interfaces.C;
with Gsk.Color_Node;
with Gsk.Container_Node;
with Gsk.Border_Node;
with Gsk.Conic_Gradient_Node;
with Gsk.Enums;
with Gsk.Linear_Gradient_Node;
with Gsk.Radial_Gradient_Node;
with Gsk.Repeating_Linear_Gradient_Node;
with Gsk.Repeating_Radial_Gradient_Node;
with Gsk.Rounded_Rect;
with Gsk.Render_Node;
with Gtk.Main;
with System;

procedure Main is
   use Glib.Test;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Glib.Error.GError;
   use type Glib.Gsize;
   use type Cairo.Cairo_Status;
   use type Ada.Directories.File_Size;
   use type System.Address;

   --  The Ada wrapper record owns its GSK reference, so deallocating a wrapper
   --  runs Finalize and releases the underlying immutable render node.
   procedure Free_Render_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);
   procedure Free_Border_Node is new Ada.Unchecked_Deallocation
     (Gsk.Border_Node.Gsk_Border_Node_Record'Class,
      Gsk.Border_Node.Gsk_Border_Node);
   procedure Free_Radial_Node is new Ada.Unchecked_Deallocation
     (Gsk.Radial_Gradient_Node.Gsk_Radial_Gradient_Node_Record'Class,
      Gsk.Radial_Gradient_Node.Gsk_Radial_Gradient_Node);
   procedure Free_Linear_Node is new Ada.Unchecked_Deallocation
     (Gsk.Linear_Gradient_Node.Gsk_Linear_Gradient_Node_Record'Class,
      Gsk.Linear_Gradient_Node.Gsk_Linear_Gradient_Node);
   procedure Free_Repeating_Linear_Node is new Ada.Unchecked_Deallocation
     (Gsk.Repeating_Linear_Gradient_Node.Gsk_Repeating_Linear_Gradient_Node_Record'Class,
      Gsk.Repeating_Linear_Gradient_Node.Gsk_Repeating_Linear_Gradient_Node);
   procedure Free_Repeating_Radial_Node is new Ada.Unchecked_Deallocation
     (Gsk.Repeating_Radial_Gradient_Node.Gsk_Repeating_Radial_Gradient_Node_Record'Class,
      Gsk.Repeating_Radial_Gradient_Node.Gsk_Repeating_Radial_Gradient_Node);
   procedure Free_Conic_Node is new Ada.Unchecked_Deallocation
     (Gsk.Conic_Gradient_Node.Gsk_Conic_Gradient_Node_Record'Class,
      Gsk.Conic_Gradient_Node.Gsk_Conic_Gradient_Node);

   Parse_Error_Called : Boolean := False;
   Parse_Start        : Gsk.Render_Node.Gsk_Parse_Location;
   Parse_End          : Gsk.Render_Node.Gsk_Parse_Location;
   Parse_Error_Text   : Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.Null_Unbounded_String;

   procedure On_Parse_Error
     (Start   : Gsk.Render_Node.Gsk_Parse_Location;
      The_End : Gsk.Render_Node.Gsk_Parse_Location;
      Error   : Glib.Error.GError);

   procedure Test_Render_Node with Convention => C;
   procedure Test_Border_Node with Convention => C;
   procedure Test_Conic_Gradient_Node with Convention => C;
   procedure Test_Linear_Gradient_Node with Convention => C;
   procedure Test_Radial_Gradient_Node with Convention => C;
   procedure Test_Repeating_Linear_Gradient_Node with Convention => C;
   procedure Test_Repeating_Radial_Gradient_Node with Convention => C;
   procedure Test_Deserialize_Errors with Convention => C;
   procedure Test_Children with Convention => C;
   procedure Test_Internal_Conversions with Convention => C;

   --  These GtkAda conversion helpers release their input with g_free, so
   --  provide records allocated by GLib's matching C allocator.
   function G_Malloc (Size : Glib.Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   type Parse_Location_Access is
     access all Gsk.Render_Node.Gsk_Parse_Location;
   type Color_Stop_Access is access all Gsk.Render_Node.Gsk_Color_Stop;
   function To_Parse_Location_Access is new Ada.Unchecked_Conversion
     (System.Address, Parse_Location_Access);
   function To_Color_Stop_Access is new Ada.Unchecked_Conversion
     (System.Address, Color_Stop_Access);

   --  GSK's CSS-like node format recovers by returning a node for bad input;
   --  use a recognized node with an invalid color to exercise its error hook.
   function As_Bytes (Text : String) return Glib.Guint8_Array is
      Result : Glib.Guint8_Array (1 .. Text'Length);
   begin
      for Index in Text'Range loop
         Result (Index - Text'First + 1) :=
           Glib.Guint8 (Character'Pos (Text (Index)));
      end loop;
      return Result;
   end As_Bytes;

   --  Save the diagnostic in the callback; the test checks it after GTK
   --  returns from deserialization, when no callback-owned data is retained.
   procedure On_Parse_Error
     (Start   : Gsk.Render_Node.Gsk_Parse_Location;
      The_End : Gsk.Render_Node.Gsk_Parse_Location;
      Error   : Glib.Error.GError)
   is
   begin
      Parse_Error_Called := True;
      Parse_Start := Start;
      Parse_End := The_End;
      if Error /= null then
         Parse_Error_Text := Ada.Strings.Unbounded.To_Unbounded_String
           (Glib.Error.Get_Message (Error));
      end if;
   end On_Parse_Error;

   --  Check the node accessors and render a node to an in-memory Cairo target.
   procedure Test_Render_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 3.0, Y => 4.0), (Width => 32.0, Height => 24.0));
      Color : aliased Gdk.RGBA.Gdk_RGBA :=
        (Red => 0.25, Green => 0.5, Blue => 0.75, Alpha => 1.0);
      Faint_Color : aliased Gdk.RGBA.Gdk_RGBA :=
        (Red => 0.25, Green => 0.5, Blue => 0.75, Alpha => 0.5);
      Node : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node
          (Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access));
      Procedure_Node : Gsk.Color_Node.Gsk_Color_Node;
      Faint_Node : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node
          (Gsk.Color_Node.Gsk_Color_Node_New
             (Faint_Color'Access, Bounds'Access));
      Actual_Bounds : aliased Graphene.Rect.Graphene_Rect_T;
      Opaque_Bounds : aliased Graphene.Rect.Graphene_Rect_T;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Surface : Cairo.Cairo_Surface :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32, 40, 32);
      Context : Cairo.Cairo_Context := Cairo.Create (Surface);
      Output_File : constant String := Ada.Directories.Compose
        (Ada.Directories.Current_Directory, "gsk-render-node.dump");
      Error : Glib.Error.GError;
   begin
      --  A default wrapper is uncreated; Set_Object is used only with a live
      --  node address here, and is reset before this stack wrapper finalizes.
      declare
         Dummy : aliased Gsk.Render_Node.Dummy_Gsk_Render_Node_Record;
      begin
         Assert_False (Gsk.Render_Node.Is_Created (Dummy'Access));
         Gsk.Render_Node.Set_Object (Dummy'Access, Gsk.Render_Node.Get_Object (Node));
         Assert_True (Gsk.Render_Node.Is_Created (Dummy'Access));
         Assert_True
           (Gsk.Render_Node.Get_Object (Dummy'Access) =
              Gsk.Render_Node.Get_Object (Node));
         Gsk.Render_Node.Set_Object (Dummy'Access, System.Null_Address);
         Assert_False (Gsk.Render_Node.Is_Created (Dummy'Access));
      end;
      Assert_True
        (Gsk.Render_Node.Get_Object (null) = System.Null_Address);

      --  A colour node reports its exact bounds and node kind. Opaque bounds
      --  are available for alpha 1 and absent when the colour is translucent.
      Assert_True (Gsk.Render_Node.Is_Created (Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Node) = Gsk.Enums.Gsk_Color_Node);
      Gsk.Render_Node.Get_Bounds (Node, Actual_Bounds'Access);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Bounds.Origin.X), 3.0, 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Bounds.Origin.Y), 4.0, 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Bounds.Size.Width), 32.0, 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Bounds.Size.Height), 24.0, 0.001);
      Assert_True
        (Gsk.Render_Node.Get_Opaque_Rect (Node, Opaque_Bounds'Access));
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Opaque_Bounds.Size.Width), 32.0, 0.001);
      Assert_False
        (Gsk.Render_Node.Get_Opaque_Rect (Faint_Node, Opaque_Bounds'Access));

      --  Gtk_New is the procedure-style color-node constructor; check its
      --  stored bounds and color so the test covers the values it receives.
      Gsk.Color_Node.Gtk_New
        (Procedure_Node, Faint_Color'Access, Bounds'Access);
      --  A created object must have the concrete color-node render type.
      Assert_True (Gsk.Render_Node.Is_Created (Procedure_Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Procedure_Node)) =
             Gsk.Enums.Gsk_Color_Node);
      Gsk.Render_Node.Get_Bounds
        (Gsk.Render_Node.Gsk_Render_Node (Procedure_Node),
         Actual_Bounds'Access);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Bounds.Origin.X), 3.0, 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Bounds.Size.Height), 24.0, 0.001);
      --  Get_Color borrows the immutable color stored in this live node.
      declare
         Actual_Color : constant access constant Gdk.RGBA.Gdk_RGBA :=
           Gsk.Color_Node.Get_Color (Procedure_Node);
      begin
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual_Color.Red), 0.25, 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual_Color.Green), 0.5, 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual_Color.Blue), 0.75, 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual_Color.Alpha), 0.5, 0.001);
      end;

      --  Draw to memory so this test never needs a real display or renderer.
      Gsk.Render_Node.Draw (Node, Context);
      Assert_True (Cairo.Status (Context) = Cairo.Cairo_Status_Success);

      --  A serialized node can be deserialized through the null-callback path.
      Serialized := Gsk.Render_Node.Serialize (Node);
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) = Gsk.Enums.Gsk_Color_Node);

      --  Successful writes are checked and removed in the test directory.
      if Ada.Directories.Exists (Output_File) then
         Ada.Directories.Delete_File (Output_File);
      end if;
      Assert_True
        (Gsk.Render_Node.Write_To_File (Node, Output_File, Error));
      Assert_True (Error = null);
      Assert_True (Ada.Directories.Exists (Output_File));
      Assert_True (Ada.Directories.Size (Output_File) > 0);
      Ada.Directories.Delete_File (Output_File);

      --  A directory cannot be replaced by the serialized file; this checks
      --  the recoverable error result without relying on permissions or paths.
      Assert_False
        (Gsk.Render_Node.Write_To_File
           (Node, Ada.Directories.Current_Directory, Error));
      Assert_True (Error /= null);
      Assert_True (Glib.Error.Get_Message (Error)'Length > 0);
      Glib.Error.Error_Free (Error);

      --  Destroy Cairo handles before releasing nodes so all resources have
      --  deterministic lifetimes independent of Ada heap finalization order.
      Cairo.Destroy (Context);
      Cairo.Surface_Destroy (Surface);
      Free_Render_Node (Restored);
      Free_Render_Node (Faint_Node);
      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Procedure_Node));
      Free_Render_Node (Node);
   end Test_Render_Node;

   --  Check the four independently stored sides, plus the rounded outline.
   procedure Check_Border_Properties
     (Node : Gsk.Border_Node.Gsk_Border_Node;
      Expected_Widths : Gsk.Border_Node.Gfloat_Array4;
      Expected_Colors : Gsk.Border_Node.Gdk_Rgba_Array4;
      Expected_Outline : Gsk.Rounded_Rect.Gsk_Rounded_Rect)
   is
      use type Gdk.RGBA.Gdk_RGBA;

      Actual_Widths : constant Gsk.Border_Node.Gfloat_Array4 :=
        Gsk.Border_Node.Get_Widths (Node);
      Actual_Colors : constant Gsk.Border_Node.Gdk_Rgba_Array4 :=
        Gsk.Border_Node.Get_Colors (Node);
      Actual_Outline : constant Gsk.Rounded_Rect.Gsk_Rounded_Rect :=
        Gsk.Border_Node.Get_Outline (Node);
   begin
      for Side in Expected_Widths'Range loop
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual_Widths (Side)),
            Glib.Gdouble (Expected_Widths (Side)), 0.001);
         --  Exact equality is appropriate: these RGBA values are copied
         --  unchanged into and back out of the immutable render node.
         Assert_True (Actual_Colors (Side) = Expected_Colors (Side));
      end loop;

      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Outline.Bounds.Origin.X),
         Glib.Gdouble (Expected_Outline.Bounds.Origin.X), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Outline.Bounds.Origin.Y),
         Glib.Gdouble (Expected_Outline.Bounds.Origin.Y), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Outline.Bounds.Size.Width),
         Glib.Gdouble (Expected_Outline.Bounds.Size.Width), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Outline.Bounds.Size.Height),
         Glib.Gdouble (Expected_Outline.Bounds.Size.Height), 0.001);
   end Check_Border_Properties;

   --  Exercise both Ada constructors, the initialization guard, all three
   --  accessors, and the render-node factory used when deserializing a node.
   procedure Test_Border_Node is
      use type Gsk.Render_Node.Gsk_Render_Node;

      Outline : constant Gsk.Rounded_Rect.Gsk_Rounded_Rect :=
        (Bounds => ((X => 1.0, Y => 2.0),
                    (Width => 40.0, Height => 30.0)),
         Corner => (others => (Width => 5.0, Height => 5.0)));
      Widths : constant Gsk.Border_Node.Gfloat_Array4 :=
        (1.0, 2.0, 3.0, 4.0);
      Colors : constant Gsk.Border_Node.Gdk_Rgba_Array4 :=
        ((Red => 1.0, Green => 0.0, Blue => 0.0, Alpha => 0.25),
         (Red => 0.0, Green => 1.0, Blue => 0.0, Alpha => 0.5),
         (Red => 0.0, Green => 0.0, Blue => 1.0, Alpha => 0.75),
         (Red => 0.25, Green => 0.5, Blue => 0.75, Alpha => 1.0));
      Other_Widths : constant Gsk.Border_Node.Gfloat_Array4 :=
        (8.0, 7.0, 6.0, 5.0);
      Other_Colors : constant Gsk.Border_Node.Gdk_Rgba_Array4 :=
        (others => (Red => 0.0, Green => 0.0, Blue => 0.0, Alpha => 1.0));
      Node : Gsk.Border_Node.Gsk_Border_Node :=
        Gsk.Border_Node.Gsk_Border_Node_New (Outline, Widths, Colors);
      Gtk_Node : Gsk.Border_Node.Gsk_Border_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Border : Gsk.Border_Node.Gsk_Border_Node;
   begin
      Check_Border_Properties (Node, Widths, Colors, Outline);

      --  Initialize is intentionally a no-op once the wrapper owns a node.
      Gsk.Border_Node.Initialize
        (Node, Outline, Other_Widths, Other_Colors);
      Check_Border_Properties (Node, Widths, Colors, Outline);

      --  Gtk_New follows the procedure-style constructor path and preserves
      --  all side-specific widths and colors too.
      Gsk.Border_Node.Gtk_New (Gtk_Node, Outline, Widths, Colors);
      Check_Border_Properties (Gtk_Node, Widths, Colors, Outline);

      --  Deserialization dispatches through the node's external Ada tag, so
      --  it also exercises Gsk.Border_Node.Create.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) = Gsk.Enums.Gsk_Border_Node);
      Restored_Border := Gsk.Border_Node.Gsk_Border_Node (Restored);
      Check_Border_Properties (Restored_Border, Widths, Colors, Outline);

      --  The controlled render-node wrappers release their references when
      --  deallocated; clear the typed alias before freeing the base wrapper.
      Restored_Border := null;
      Free_Render_Node (Restored);
      Free_Border_Node (Node);
      Free_Border_Node (Gtk_Node);
   end Test_Border_Node;

   --  Check the endpoints and copied color stops exposed by a linear node.
   procedure Check_Linear_Properties
     (Node : Gsk.Linear_Gradient_Node.Gsk_Linear_Gradient_Node;
      Start : Graphene.Point.Graphene_Point_T;
      The_End : Graphene.Point.Graphene_Point_T;
      Stops : Gsk.Render_Node.Gsk_Color_Stop_Array)
   is
      Actual_Start : constant access constant Graphene.Point.Graphene_Point_T :=
        Gsk.Linear_Gradient_Node.Get_Start (Node);
      Actual_End : constant access constant Graphene.Point.Graphene_Point_T :=
        Gsk.Linear_Gradient_Node.Get_End (Node);
      --  Copying the returned C slice also checks GtkAda's array conversion.
      Actual_Stops : constant Gsk.Render_Node.Gsk_Color_Stop_Array :=
        Gsk.Linear_Gradient_Node.Get_Color_Stops (Node);
   begin
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Start.X), Glib.Gdouble (Start.X), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Start.Y), Glib.Gdouble (Start.Y), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_End.X), Glib.Gdouble (The_End.X), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_End.Y), Glib.Gdouble (The_End.Y), 0.001);

      Assert_True
        (Gsk.Linear_Gradient_Node.Get_N_Color_Stops (Node) =
           Glib.Gsize (Stops'Length));
      Assert_True (Actual_Stops'Length = Stops'Length);
      for Offset in 0 .. Stops'Length - 1 loop
         declare
            Actual : constant Gsk.Render_Node.Gsk_Color_Stop :=
              Actual_Stops (Actual_Stops'First + Offset);
            Expected : constant Gsk.Render_Node.Gsk_Color_Stop :=
              Stops (Stops'First + Offset);
         begin
            --  GTK copies these C float values into the immutable render node.
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Offset), Glib.Gdouble (Expected.Offset),
               0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Color.Red),
               Glib.Gdouble (Expected.Color.Red), 0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Color.Green),
               Glib.Gdouble (Expected.Color.Green), 0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Color.Blue),
               Glib.Gdouble (Expected.Color.Blue), 0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Color.Alpha),
               Glib.Gdouble (Expected.Color.Alpha), 0.001);
         end;
      end loop;
   end Check_Linear_Properties;

   --  Exercise both Ada constructors, the no-op Initialize guard, each
   --  accessor, and the factory used when deserializing a render node.
   procedure Test_Linear_Gradient_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 48.0, Height => 36.0));
      Start : aliased Graphene.Point.Graphene_Point_T := (X => 5.0, Y => 7.0);
      The_End : aliased Graphene.Point.Graphene_Point_T :=
        (X => 42.0, Y => 28.0);
      Other_Start : aliased Graphene.Point.Graphene_Point_T :=
        (X => 9.0, Y => 11.0);
      Other_End : aliased Graphene.Point.Graphene_Point_T :=
        (X => 31.0, Y => 25.0);
      --  Distinct offsets and color channels verify stop order and all fields.
      Stops : constant Gsk.Render_Node.Gsk_Color_Stop_Array (0 .. 2) :=
        (0 => (Offset => 0.0,
               Color => (Red => 1.0, Green => 0.0, Blue => 0.0, Alpha => 1.0)),
         1 => (Offset => 0.4,
               Color => (Red => 0.1, Green => 0.5, Blue => 0.8, Alpha => 0.75)),
         2 => (Offset => 1.0,
               Color => (Red => 0.0, Green => 0.25, Blue => 0.75, Alpha => 0.5)));
      Other_Stops : constant Gsk.Render_Node.Gsk_Color_Stop_Array (0 .. 1) :=
        (others => (Offset => 0.5,
                    Color => (Red => 0.0, Green => 0.0, Blue => 0.0, Alpha => 1.0)));
      Node : Gsk.Linear_Gradient_Node.Gsk_Linear_Gradient_Node :=
        Gsk.Linear_Gradient_Node.Gsk_Linear_Gradient_Node_New
          (Bounds'Access, Start'Access, The_End'Access, Stops);
      Gtk_Node : Gsk.Linear_Gradient_Node.Gsk_Linear_Gradient_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Linear : Gsk.Linear_Gradient_Node.Gsk_Linear_Gradient_Node;
   begin
      Check_Linear_Properties (Node, Start, The_End, Stops);

      --  Initialize must preserve the original immutable node after creation.
      Gsk.Linear_Gradient_Node.Initialize
        (Node, Bounds'Access, Other_Start'Access, Other_End'Access,
         Other_Stops);
      Check_Linear_Properties (Node, Start, The_End, Stops);

      --  Gtk_New follows the procedure-style constructor path.
      Gsk.Linear_Gradient_Node.Gtk_New
        (Gtk_Node, Bounds'Access, Start'Access, The_End'Access, Stops);
      Check_Linear_Properties (Gtk_Node, Start, The_End, Stops);

      --  Deserializing a linear node dispatches through its Ada Create hook.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
           Gsk.Enums.Gsk_Linear_Gradient_Node);
      Restored_Linear :=
        Gsk.Linear_Gradient_Node.Gsk_Linear_Gradient_Node (Restored);
      Check_Linear_Properties (Restored_Linear, Start, The_End, Stops);

      --  The typed alias and base wrapper share ownership of the same object.
      Restored_Linear := null;
      Free_Render_Node (Restored);
      Free_Linear_Node (Gtk_Node);
      Free_Linear_Node (Node);
   end Test_Linear_Gradient_Node;

   --  Repeating gradients expose no endpoint or stop getters, so check their
   --  node type and render bounds while exercising all available constructors.
   procedure Test_Repeating_Linear_Gradient_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 48.0, Height => 36.0));
      Start : aliased Graphene.Point.Graphene_Point_T := (X => 5.0, Y => 7.0);
      The_End : aliased Graphene.Point.Graphene_Point_T :=
        (X => 42.0, Y => 28.0);
      Other_Start : aliased Graphene.Point.Graphene_Point_T :=
        (X => 9.0, Y => 11.0);
      Other_End : aliased Graphene.Point.Graphene_Point_T :=
        (X => 31.0, Y => 25.0);
      --  Use increasing offsets with endpoint stops, as required by GSK.
      Stops : constant Gsk.Render_Node.Gsk_Color_Stop_Array (0 .. 2) :=
        (0 => (Offset => 0.0,
               Color => (Red => 1.0, Green => 0.0, Blue => 0.0, Alpha => 1.0)),
         1 => (Offset => 0.5,
               Color => (Red => 0.1, Green => 0.5, Blue => 0.8, Alpha => 0.75)),
         2 => (Offset => 1.0,
               Color => (Red => 0.0, Green => 0.25, Blue => 0.75, Alpha => 0.5)));
      Node : Gsk.Repeating_Linear_Gradient_Node.Gsk_Repeating_Linear_Gradient_Node :=
        Gsk.Repeating_Linear_Gradient_Node.Gsk_Repeating_Linear_Gradient_Node_New
          (Bounds'Access, Start'Access, The_End'Access, Stops);
      Gtk_Node : Gsk.Repeating_Linear_Gradient_Node.Gsk_Repeating_Linear_Gradient_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Repeating : Gsk.Repeating_Linear_Gradient_Node.Gsk_Repeating_Linear_Gradient_Node;

      --  Bounds are the generic observable property for this node subtype.
      procedure Assert_Node_Bounds
        (Candidate : Gsk.Render_Node.Gsk_Render_Node) is
         Actual : aliased Graphene.Rect.Graphene_Rect_T;
      begin
         Gsk.Render_Node.Get_Bounds (Candidate, Actual'Access);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual.Origin.X), Glib.Gdouble (Bounds.Origin.X), 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual.Origin.Y), Glib.Gdouble (Bounds.Origin.Y), 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual.Size.Width), Glib.Gdouble (Bounds.Size.Width), 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual.Size.Height), Glib.Gdouble (Bounds.Size.Height), 0.001);
      end Assert_Node_Bounds;
   begin
      --  The function constructor creates the C node and the Ada wrapper.
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Gsk.Render_Node.Gsk_Render_Node (Node)) =
           Gsk.Enums.Gsk_Repeating_Linear_Gradient_Node);
      Assert_Node_Bounds (Gsk.Render_Node.Gsk_Render_Node (Node));

      --  Initialize must leave an already-created immutable node untouched.
      Gsk.Repeating_Linear_Gradient_Node.Initialize
        (Node, Bounds'Access, Other_Start'Access, Other_End'Access, Stops);
      Assert_Node_Bounds (Gsk.Render_Node.Gsk_Render_Node (Node));

      --  Gtk_New exercises the procedure constructor path independently.
      Gsk.Repeating_Linear_Gradient_Node.Gtk_New
        (Gtk_Node, Bounds'Access, Start'Access, The_End'Access, Stops);
      Assert_Node_Bounds (Gsk.Render_Node.Gsk_Render_Node (Gtk_Node));

      --  Deserialization reconstructs the Ada subclass through its Create hook.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
           Gsk.Enums.Gsk_Repeating_Linear_Gradient_Node);
      --  This class conversion verifies the deserialized object has its Ada tag.
      Restored_Repeating :=
        Gsk.Repeating_Linear_Gradient_Node.Gsk_Repeating_Linear_Gradient_Node
          (Restored);
      Assert_Node_Bounds (Gsk.Render_Node.Gsk_Render_Node (Restored_Repeating));

      --  Clear the typed alias before releasing the shared base wrapper.
      Restored_Repeating := null;
      Free_Render_Node (Restored);
      Free_Repeating_Linear_Node (Gtk_Node);
      Free_Repeating_Linear_Node (Node);
   end Test_Repeating_Linear_Gradient_Node;

   --  Repeating radial nodes have no geometry getters beyond render bounds.
   --  Exercise both constructors, immutable initialization, and deserialization.
   procedure Test_Repeating_Radial_Gradient_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 48.0, Height => 36.0));
      Other_Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 12.0, Y => 14.0), (Width => 8.0, Height => 6.0));
      Center : aliased Graphene.Point.Graphene_Point_T := (X => 8.0, Y => 9.0);
      Other_Center : aliased Graphene.Point.Graphene_Point_T :=
        (X => 16.0, Y => 18.0);
      --  Increasing endpoint stops satisfy GSK's gradient requirements.
      Stops : constant Gsk.Render_Node.Gsk_Color_Stop_Array (0 .. 1) :=
        (0 => (Offset => 0.0,
               Color => (Red => 1.0, Green => 0.1, Blue => 0.0, Alpha => 1.0)),
         1 => (Offset => 1.0,
               Color => (Red => 0.0, Green => 0.4, Blue => 0.8, Alpha => 0.75)));
      Node : Gsk.Repeating_Radial_Gradient_Node.Gsk_Repeating_Radial_Gradient_Node :=
        Gsk.Repeating_Radial_Gradient_Node.Gsk_Repeating_Radial_Gradient_Node_New
          (Bounds'Access, Center'Access, 18.0, 12.0, 0.2, 0.9, Stops);
      Gtk_Node : Gsk.Repeating_Radial_Gradient_Node.Gsk_Repeating_Radial_Gradient_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Repeating :
        Gsk.Repeating_Radial_Gradient_Node.Gsk_Repeating_Radial_Gradient_Node;

      --  Bounds are the shared observable property for this node subtype.
      procedure Assert_Node_Bounds
        (Candidate : Gsk.Render_Node.Gsk_Render_Node;
         Expected : Graphene.Rect.Graphene_Rect_T)
      is
         Actual : aliased Graphene.Rect.Graphene_Rect_T;
      begin
         Gsk.Render_Node.Get_Bounds (Candidate, Actual'Access);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual.Origin.X), Glib.Gdouble (Expected.Origin.X), 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual.Origin.Y), Glib.Gdouble (Expected.Origin.Y), 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual.Size.Width), Glib.Gdouble (Expected.Size.Width), 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Actual.Size.Height), Glib.Gdouble (Expected.Size.Height), 0.001);
      end Assert_Node_Bounds;
   begin
      --  The function constructor forwards all gradient parameters to GSK.
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Gsk.Render_Node.Gsk_Render_Node (Node)) =
           Gsk.Enums.Gsk_Repeating_Radial_Gradient_Node);
      Assert_Node_Bounds (Gsk.Render_Node.Gsk_Render_Node (Node), Bounds);

      --  Initialize must preserve an already-created immutable render node.
      Gsk.Repeating_Radial_Gradient_Node.Initialize
        (Node, Other_Bounds'Access, Other_Center'Access, 2.0, 4.0, 0.0, 1.0,
         Stops);
      Assert_Node_Bounds (Gsk.Render_Node.Gsk_Render_Node (Node), Bounds);

      --  Gtk_New exercises the procedure constructor independently.
      Gsk.Repeating_Radial_Gradient_Node.Gtk_New
        (Gtk_Node, Bounds'Access, Center'Access, 18.0, 12.0, 0.2, 0.9, Stops);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Gsk.Render_Node.Gsk_Render_Node (Gtk_Node)) =
           Gsk.Enums.Gsk_Repeating_Radial_Gradient_Node);
      Assert_Node_Bounds (Gsk.Render_Node.Gsk_Render_Node (Gtk_Node), Bounds);

      --  Deserialization invokes the generated Ada Create factory hook.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
           Gsk.Enums.Gsk_Repeating_Radial_Gradient_Node);
      Restored_Repeating :=
        Gsk.Repeating_Radial_Gradient_Node.Gsk_Repeating_Radial_Gradient_Node
          (Restored);
      Assert_Node_Bounds (Gsk.Render_Node.Gsk_Render_Node (Restored_Repeating), Bounds);

      --  The typed alias shares ownership with the restored base wrapper.
      Restored_Repeating := null;
      Free_Render_Node (Restored);
      Free_Repeating_Radial_Node (Gtk_Node);
      Free_Repeating_Radial_Node (Node);
      Glib.Bytes.Unref (Serialized);
   end Test_Repeating_Radial_Gradient_Node;

   --  Check that the radial node retains its center, radii, percentages, and
   --  ordered color stops through the generated getters.
   procedure Check_Radial_Properties
     (Node : Gsk.Radial_Gradient_Node.Gsk_Radial_Gradient_Node;
      Center : Graphene.Point.Graphene_Point_T;
      Expected_Hradius : Interfaces.C.C_float;
      Expected_Vradius : Interfaces.C.C_float;
      Expected_Start : Interfaces.C.C_float;
      Expected_End : Interfaces.C.C_float;
      Stops : Gsk.Render_Node.Gsk_Color_Stop_Array)
   is
      use type Interfaces.C.C_float;
      Actual_Center : constant access constant Graphene.Point.Graphene_Point_T :=
        Gsk.Radial_Gradient_Node.Get_Center (Node);
      --  Copying the returned C slice also checks GtkAda's array conversion.
      Actual_Stops : constant Gsk.Render_Node.Gsk_Color_Stop_Array :=
        Gsk.Radial_Gradient_Node.Get_Color_Stops (Node);
   begin
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Center.X), Glib.Gdouble (Center.X), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Center.Y), Glib.Gdouble (Center.Y), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Radial_Gradient_Node.Get_Hradius (Node)),
         Glib.Gdouble (Expected_Hradius), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Radial_Gradient_Node.Get_Vradius (Node)),
         Glib.Gdouble (Expected_Vradius), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Radial_Gradient_Node.Get_Start (Node)),
         Glib.Gdouble (Expected_Start), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Radial_Gradient_Node.Get_End (Node)),
         Glib.Gdouble (Expected_End), 0.001);

      Assert_True
        (Gsk.Radial_Gradient_Node.Get_N_Color_Stops (Node) =
           Glib.Gsize (Stops'Length));
      Assert_True (Actual_Stops'Length = Stops'Length);
      for Offset in 0 .. Stops'Length - 1 loop
         declare
            Actual : constant Gsk.Render_Node.Gsk_Color_Stop :=
              Actual_Stops (Actual_Stops'First + Offset);
            Expected : constant Gsk.Render_Node.Gsk_Color_Stop :=
              Stops (Stops'First + Offset);
         begin
            --  GTK copies these C float values into the immutable render node.
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Offset), Glib.Gdouble (Expected.Offset),
               0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Color.Red),
               Glib.Gdouble (Expected.Color.Red), 0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Color.Green),
               Glib.Gdouble (Expected.Color.Green), 0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Color.Blue),
               Glib.Gdouble (Expected.Color.Blue), 0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Color.Alpha),
               Glib.Gdouble (Expected.Color.Alpha), 0.001);
         end;
      end loop;
   end Check_Radial_Properties;

   --  Exercise both Ada constructors, the no-op Initialize guard, each
   --  radial accessor, and the factory used when deserializing a render node.
   procedure Test_Radial_Gradient_Node is
      --  The non-origin bounds and off-center point make geometry checks
      --  sensitive to accidentally using the bounds origin as the center.
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 48.0, Height => 36.0));
      Center : aliased Graphene.Point.Graphene_Point_T := (X => 12.0, Y => 15.0);
      Other_Center : aliased Graphene.Point.Graphene_Point_T :=
        (X => 20.0, Y => 24.0);
      --  Distinct offsets and color channels verify both stop order and data.
      Stops : constant Gsk.Render_Node.Gsk_Color_Stop_Array (0 .. 1) :=
        (0 => (Offset => 0.0,
               Color => (Red => 1.0, Green => 0.0, Blue => 0.0, Alpha => 1.0)),
         1 => (Offset => 1.0,
               Color => (Red => 0.0, Green => 0.25, Blue => 0.75, Alpha => 0.5)));
      Other_Stops : constant Gsk.Render_Node.Gsk_Color_Stop_Array (0 .. 1) :=
        (others => (Offset => 0.5,
                    Color => (Red => 0.0, Green => 0.0, Blue => 0.0, Alpha => 1.0)));
      Node : Gsk.Radial_Gradient_Node.Gsk_Radial_Gradient_Node :=
        Gsk.Radial_Gradient_Node.Gsk_Radial_Gradient_Node_New
          (Bounds'Access, Center'Access, 18.0, 12.0, 0.2, 0.9, Stops);
      Gtk_Node : Gsk.Radial_Gradient_Node.Gsk_Radial_Gradient_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Radial : Gsk.Radial_Gradient_Node.Gsk_Radial_Gradient_Node;
   begin
      Check_Radial_Properties (Node, Center, 18.0, 12.0, 0.2, 0.9, Stops);

      --  Initialize must preserve the original object after its first call.
      Gsk.Radial_Gradient_Node.Initialize
        (Node, Bounds'Access, Other_Center'Access, 2.0, 4.0, 0.0, 1.0,
         Other_Stops);
      Check_Radial_Properties (Node, Center, 18.0, 12.0, 0.2, 0.9, Stops);

      --  The procedure constructor follows the same initialized-object path.
      Gsk.Radial_Gradient_Node.Gtk_New
        (Gtk_Node, Bounds'Access, Center'Access, 18.0, 12.0, 0.2, 0.9, Stops);
      Check_Radial_Properties
        (Gtk_Node, Center, 18.0, 12.0, 0.2, 0.9, Stops);

      --  Deserializing a radial node dispatches through its Ada Create hook.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
           Gsk.Enums.Gsk_Radial_Gradient_Node);
      Restored_Radial :=
        Gsk.Radial_Gradient_Node.Gsk_Radial_Gradient_Node (Restored);
      --  Serialization stores the two circle radii; deserialization exposes
      --  their canonical values through the legacy radius/percentage getters.
      Check_Radial_Properties
        (Restored_Radial, Center, 16.2, 10.8, 0.2222222, 1.0, Stops);

      --  Clear the typed alias before freeing the base wrapper that owns it;
      --  the two access values refer to the same controlled Ada wrapper.
      Restored_Radial := null;
      Free_Render_Node (Restored);
      Free_Radial_Node (Gtk_Node);
      Free_Radial_Node (Node);
   end Test_Radial_Gradient_Node;

   --  Check the geometry, rotation, and copied stops exposed by a conic node.
   procedure Check_Conic_Properties
     (Node : Gsk.Conic_Gradient_Node.Gsk_Conic_Gradient_Node;
      Center : Graphene.Point.Graphene_Point_T;
      Expected_Rotation : Interfaces.C.C_float;
      Expected_Angle : Interfaces.C.C_float;
      Stops : Gsk.Render_Node.Gsk_Color_Stop_Array)
   is
      Actual_Center : constant access constant Graphene.Point.Graphene_Point_T :=
        Gsk.Conic_Gradient_Node.Get_Center (Node);
      Actual_Stops : constant Gsk.Render_Node.Gsk_Color_Stop_Array :=
        Gsk.Conic_Gradient_Node.Get_Color_Stops (Node);
   begin
      --  The returned center is borrowed, so inspect it while the node lives.
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Center.X), Glib.Gdouble (Center.X), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual_Center.Y), Glib.Gdouble (Center.Y), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Conic_Gradient_Node.Get_Rotation (Node)),
         Glib.Gdouble (Expected_Rotation), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Conic_Gradient_Node.Get_Angle (Node)),
         Glib.Gdouble (Expected_Angle), 0.001);

      --  Both the separate count and copied slice must match the input stops.
      Assert_True
        (Gsk.Conic_Gradient_Node.Get_N_Color_Stops (Node) =
           Glib.Gsize (Stops'Length));
      Assert_True (Actual_Stops'Length = Stops'Length);
      for Offset in 0 .. Stops'Length - 1 loop
         declare
            Actual : constant Gsk.Render_Node.Gsk_Color_Stop :=
              Actual_Stops (Actual_Stops'First + Offset);
            Expected : constant Gsk.Render_Node.Gsk_Color_Stop :=
              Stops (Stops'First + Offset);
         begin
            --  The stop data is copied into the immutable node without change.
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Offset), Glib.Gdouble (Expected.Offset),
               0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Color.Red),
               Glib.Gdouble (Expected.Color.Red), 0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Color.Green),
               Glib.Gdouble (Expected.Color.Green), 0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Color.Blue),
               Glib.Gdouble (Expected.Color.Blue), 0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual.Color.Alpha),
               Glib.Gdouble (Expected.Color.Alpha), 0.001);
         end;
      end loop;
   end Check_Conic_Properties;

   --  Exercise both constructors, Initialize's no-op guard, all conic getters,
   --  and the render-node factory used when a conic node is deserialized.
   procedure Test_Conic_Gradient_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 48.0, Height => 36.0));
      Center : aliased Graphene.Point.Graphene_Point_T :=
        (X => 12.0, Y => 15.0);
      Other_Center : aliased Graphene.Point.Graphene_Point_T :=
        (X => 20.0, Y => 24.0);
      --  Different offsets and channel values make order and copying visible.
      Stops : constant Gsk.Render_Node.Gsk_Color_Stop_Array (0 .. 1) :=
        (0 => (Offset => 0.0,
               Color => (Red => 1.0, Green => 0.0, Blue => 0.0, Alpha => 1.0)),
         1 => (Offset => 1.0,
               Color => (Red => 0.0, Green => 0.25, Blue => 0.75, Alpha => 0.5)));
      Other_Stops : constant Gsk.Render_Node.Gsk_Color_Stop_Array (0 .. 1) :=
        (others => (Offset => 0.5,
                    Color => (Red => 0.0, Green => 0.0, Blue => 0.0, Alpha => 1.0)));
      Node : Gsk.Conic_Gradient_Node.Gsk_Conic_Gradient_Node :=
        Gsk.Conic_Gradient_Node.Gsk_Conic_Gradient_Node_New
          (Bounds'Access, Center'Access, 30.0, Stops);
      Gtk_Node : Gsk.Conic_Gradient_Node.Gsk_Conic_Gradient_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
   begin
      --  Rotation is in degrees; GTK reports the CSS angle in radians.
      Check_Conic_Properties (Node, Center, 30.0, 1.0471976, Stops);

      --  Initialize must preserve the original data after the first call.
      Gsk.Conic_Gradient_Node.Initialize
        (Node, Bounds'Access, Other_Center'Access, 180.0, Other_Stops);
      Check_Conic_Properties (Node, Center, 30.0, 1.0471976, Stops);

      --  Gtk_New follows the procedure-style constructor path as well.
      Gsk.Conic_Gradient_Node.Gtk_New
        (Gtk_Node, Bounds'Access, Center'Access, 30.0, Stops);
      Check_Conic_Properties (Gtk_Node, Center, 30.0, 1.0471976, Stops);

      --  Deserialization dispatches through the conic node's Ada Create hook.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      --  This node-kind check confirms dispatch to the conic Ada factory.
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
           Gsk.Enums.Gsk_Conic_Gradient_Node);

      Free_Render_Node (Restored);
      Free_Conic_Node (Gtk_Node);
      Free_Conic_Node (Node);
   end Test_Conic_Gradient_Node;

   --  Both callback choices return the parser's recovered node, while the
   --  callback form also verifies the location and diagnostic handoff.
   procedure Test_Deserialize_Errors is
      Bad_Text : constant String :=
        "color { color: bogus; bounds: 0 0 10 10; }";
      Bad_Bytes : constant Glib.Bytes.Gbytes :=
        Glib.Bytes.Gbytes_New
          (As_Bytes (Bad_Text), Glib.Gsize (Bad_Text'Length));
      Rejected : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Rejected := Gsk.Render_Node.Deserialize (Bad_Bytes, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Rejected));
      Free_Render_Node (Rejected);

      Parse_Error_Called := False;
      Parse_Error_Text := Ada.Strings.Unbounded.Null_Unbounded_String;
      Rejected := Gsk.Render_Node.Deserialize
        (Bad_Bytes, On_Parse_Error'Unrestricted_Access);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Rejected));
      Assert_True (Parse_Error_Called);
      Assert_True (Parse_End.Bytes >= Parse_Start.Bytes);
      Assert_True (Ada.Strings.Unbounded.Length (Parse_Error_Text) > 0);
      Free_Render_Node (Rejected);
   end Test_Deserialize_Errors;

   --  Container children exercise the returned array and the none-ownership
   --  conversion, which takes a reference for each Ada wrapper.
   procedure Test_Children is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 8.0, Height => 8.0));
      First_Color : aliased Gdk.RGBA.Gdk_RGBA := (1.0, 0.0, 0.0, 1.0);
      Second_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.0, 1.0, 0.0, 1.0);
      First : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node
          (Gsk.Color_Node.Gsk_Color_Node_New
             (First_Color'Access, Bounds'Access));
      Second : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node
          (Gsk.Color_Node.Gsk_Color_Node_New
             (Second_Color'Access, Bounds'Access));
      Children : Gsk.Render_Node.Gsk_Render_Node_Array (1 .. 2);
      Parent : Gsk.Render_Node.Gsk_Render_Node;
      Direct_Parent : Gsk.Container_Node.Gsk_Container_Node;
      Direct_First : Gsk.Render_Node.Gsk_Render_Node;
      Direct_Second : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Children := (First, Second);
      Parent := Gsk.Render_Node.Gsk_Render_Node
        (Gsk.Container_Node.Gsk_Container_Node_New (Children));
      declare
         Actual : Gsk.Render_Node.Gsk_Render_Node_Array :=
           Gsk.Render_Node.Get_Children (Parent);
      begin
         Assert_True (Actual'Length = 2);
         Assert_True
           (Gsk.Render_Node.Get_Node_Type (Actual (Actual'First)) =
              Gsk.Enums.Gsk_Color_Node);
         Assert_True
           (Gsk.Render_Node.Get_Node_Type (Actual (Actual'Last)) =
              Gsk.Enums.Gsk_Color_Node);
         Free_Render_Node (Actual (Actual'First));
         Free_Render_Node (Actual (Actual'Last));
      end;

      --  Exercise Gtk_New and the container-specific count accessor too.
      Gsk.Container_Node.Gtk_New (Direct_Parent, Children);
      Assert_Cmpuint_Eq
        (Gsk.Container_Node.Get_N_Children (Direct_Parent), 2);

      --  GSK child indexes are zero-based, even though Children starts at 1.
      Direct_First := Gsk.Container_Node.Get_Child (Direct_Parent, 0);
      Direct_Second := Gsk.Container_Node.Get_Child (Direct_Parent, 1);
      Assert_True
        (Gsk.Render_Node.Get_Object (Direct_First) =
           Gsk.Render_Node.Get_Object (First));
      Assert_True
        (Gsk.Render_Node.Get_Object (Direct_Second) =
           Gsk.Render_Node.Get_Object (Second));

      --  Get_Child returns owning Ada wrappers for borrowed C pointers.
      Free_Render_Node (Direct_First);
      Free_Render_Node (Direct_Second);
      declare
         Direct_Parent_As_Render : Gsk.Render_Node.Gsk_Render_Node :=
           Gsk.Render_Node.Gsk_Render_Node (Direct_Parent);
      begin
         Free_Render_Node (Direct_Parent_As_Render);
      end;
      Free_Render_Node (Parent);
      Free_Render_Node (First);
      Free_Render_Node (Second);
   end Test_Children;

   --  Verify the internal record copy/free helpers and the null ownership
   --  conversion. The records live in GLib-allocated memory, as their free
   --  routines require, and are consumed by From_Object_Free.
   procedure Test_Internal_Conversions is
      Address : aliased System.Address := System.Null_Address;
      Dummy : aliased Gsk.Render_Node.Dummy_Gsk_Render_Node_Record :=
        Gsk.Render_Node.Create (Address'Access);
      Location_Ptr : constant Parse_Location_Access :=
        To_Parse_Location_Access
          (G_Malloc
             (Glib.Gsize
                (Gsk.Render_Node.Gsk_Parse_Location'Size /
                   System.Storage_Unit)));
      Stop_Ptr : constant Color_Stop_Access :=
        To_Color_Stop_Access
          (G_Malloc
             (Glib.Gsize
                (Gsk.Render_Node.Gsk_Color_Stop'Size /
                   System.Storage_Unit)));
   begin
      --  Create's dummy implementation ignores the address and yields an
      --  uncreated record; a null C object converts to a null node reference.
      Assert_False (Gsk.Render_Node.Is_Created (Dummy'Access));
      Assert_True
        (Gsk.Render_Node.From_Object_Full_Ownership (System.Null_Address) =
           null);

      Location_Ptr.all :=
        (Bytes => 7, Chars => 6, Lines => 2, Line_Bytes => 3, Line_Chars => 3);
      declare
         Location : constant Gsk.Render_Node.Gsk_Parse_Location :=
           Gsk.Render_Node.From_Object_Free (Location_Ptr);
      begin
         Assert_True (Location.Bytes = 7 and Location.Chars = 6);
         Assert_True (Location.Lines = 2 and Location.Line_Bytes = 3);
         Assert_True (Location.Line_Chars = 3);
      end;

      Stop_Ptr.all :=
        (Offset => 0.75,
         Color => (Red => 0.1, Green => 0.3, Blue => 0.7, Alpha => 1.0));
      declare
         Stop : constant Gsk.Render_Node.Gsk_Color_Stop :=
           Gsk.Render_Node.From_Object_Free (Stop_Ptr);
      begin
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Stop.Offset), 0.75, 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Stop.Color.Red), 0.1, 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Stop.Color.Green), 0.3, 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Stop.Color.Blue), 0.7, 0.001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Stop.Color.Alpha), 1.0, 0.001);
      end;
   end Test_Internal_Conversions;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gsk/render-node/accessors-io", Test_Render_Node'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/render-node/border-node", Test_Border_Node'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/render-node/conic-gradient-node",
      Test_Conic_Gradient_Node'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/render-node/linear-gradient-node",
      Test_Linear_Gradient_Node'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/render-node/radial-gradient-node",
      Test_Radial_Gradient_Node'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/render-node/repeating-linear-gradient-node",
      Test_Repeating_Linear_Gradient_Node'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/render-node/repeating-radial-gradient-node",
      Test_Repeating_Radial_Gradient_Node'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/render-node/deserialize-errors",
      Test_Deserialize_Errors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/render-node/children", Test_Children'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/render-node/internal-conversions",
      Test_Internal_Conversions'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
