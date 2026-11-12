--  Exercises Gsk.Text_Node construction and every public text-node getter.
--  GTK has no C testsuite case dedicated to gsk_text_node, so these checks
--  verify its documented behavior directly through the Ada binding.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;
with System;

with Glib;          use Glib;
with Glib.Object;
with Glib.Test;     use Glib.Test;
with Gdk.RGBA;
with Gsk.Enums;
with Gsk.Render_Node;
with Gsk.Text_Node;
with Graphene.Point;
with Gtk.Label;
with Gtk.Main;
with Gtk.Snapshot;
with Pango;
with Pango.Font;
with Pango.Glyph_String;
with Pango.Layout;

procedure Main is

   use type Glib.GType;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Pango.Font.Pango_Font;
   use type Pango.Pango_Glyph;
   use type System.Address;

   --  Pango allocates this C array when Set_Size is called; this conversion
   --  lets the test copy the shaped glyph record into its first element.
   package Glyph_Info_Conversions is new
     System.Address_To_Access_Conversions
       (Pango.Glyph_String.Pango_Glyph_Info);

   --  Render nodes are controlled Ada wrappers; deallocation finalizes the
   --  wrapper and releases its owned GskRenderNode reference.
   procedure Free_Render_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Text_Node
   with Convention => C;

   --  GTK creates a valid text node from a one-character Pango layout. Its
   --  glyph record and font then provide safe inputs for the public
   --  Gsk.Text_Node constructors.
   procedure Test_Text_Node is
      Label         : constant Gtk.Label.Gtk_Label :=
        Gtk.Label.Gtk_Label_New ("text node font source");
      Layout        : Pango.Layout.Pango_Layout;
      Snapshot      : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Description   : Pango.Font.Pango_Font_Description :=
        Pango.Font.From_String ("Sans 12");
      Initial_Color : constant Gdk.RGBA.Gdk_RGBA :=
        (Red => 0.2, Green => 0.4, Blue => 0.6, Alpha => 1.0);
      Other_Color   : aliased Gdk.RGBA.Gdk_RGBA :=
        (Red => 0.9, Green => 0.1, Blue => 0.3, Alpha => 0.5);
      Other_Offset  : aliased Graphene.Point.Graphene_Point_T :=
        (x => 30.0, y => 40.0);
      Source_Node   : Gsk.Render_Node.Gsk_Render_Node;
      Text_Node     : Gsk.Text_Node.Gsk_Text_Node;
      Function_Node : Gsk.Text_Node.Gsk_Text_Node;
      Procedure_Node : Gsk.Text_Node.Gsk_Text_Node;
      Font          : Pango.Font.Pango_Font;
      Glyph_Info    : Pango.Glyph_String.Pango_Glyph_Info_Array (0 .. 0);
      Glyph_String  : access Pango.Glyph_String.Pango_Glyph_String;
      Color         : aliased Gdk.RGBA.Gdk_RGBA;
      Offset        : aliased Graphene.Point.Graphene_Point_T;
   begin
      Label.Ref_Sink;
      Layout := Label.Create_Pango_Layout ("A");
      Layout.Set_Font_Description (Description);
      Pango.Font.Free (Description);

      --  Append_Layout produces the text node GTK itself uses for a shaped
      --  glyph run; no renderer or mapped window is required.
      Snapshot.Append_Layout (Layout, Initial_Color);
      Source_Node := Snapshot.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Source_Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Source_Node) = Gsk.Enums.Gsk_Text_Node);
      Text_Node := Gsk.Text_Node.Gsk_Text_Node (Source_Node);

      --  The single ASCII character shapes to one glyph. Copy its shaped
      --  information into a Pango-owned glyph string with matching cluster
      --  storage before passing it to either constructor.
      Font := Gsk.Text_Node.Get_Font (Text_Node);
      Glyph_Info := Gsk.Text_Node.Get_Glyphs (Text_Node);
      Assert_True (Font /= null);
      Assert_True (Glyph_Info'Length = 1);
      Assert_Cmpuint_Eq (Gsk.Text_Node.Get_Num_Glyphs (Text_Node), 1);
      Glyph_String := Pango.Glyph_String.Pango_Glyph_String_New;
      Pango.Glyph_String.Set_Size (Glyph_String, 1);
      Glyph_Info_Conversions.To_Pointer (Glyph_String.Glyphs).all :=
        Glyph_Info (Glyph_Info'First);
      Glyph_String.Log_Clusters.all := 0;

      --  Preserve the baseline and color reported by GTK's source node.
      Color := Gsk.Text_Node.Get_Color (Text_Node).all;
      Offset := Gsk.Text_Node.Get_Offset (Text_Node).all;

      --  Exercise both Ada constructors with glyph data obtained from a real
      --  layout and verify that each returns a usable text render node.
      Function_Node := Gsk.Text_Node.Gsk_Text_Node_New
        (Font, Glyph_String, Color'Access, Offset'Access);
      Gsk.Text_Node.Gtk_New
        (Procedure_Node, Font, Glyph_String, Color'Access, Offset'Access);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Function_Node));
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Procedure_Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Function_Node)) =
           Gsk.Enums.Gsk_Text_Node);
      Assert_True (Gsk.Text_Node.Get_Type /= Gsk.Render_Node.Get_Type);

      --  Check values returned by the getters; Get_Font is borrowed from the
      --  render node and is not unref'ed separately.
      declare
         Read_Color : constant access constant Gdk.RGBA.Gdk_RGBA :=
           Gsk.Text_Node.Get_Color (Function_Node);
         Read_Font  : constant Pango.Font.Pango_Font :=
           Gsk.Text_Node.Get_Font (Function_Node);
         Read_Glyphs : constant Pango.Glyph_String.Pango_Glyph_Info_Array :=
           Gsk.Text_Node.Get_Glyphs (Function_Node);
         Read_Offset : constant access constant Graphene.Point.Graphene_Point_T :=
           Gsk.Text_Node.Get_Offset (Function_Node);
      begin
         Assert_Cmpfloat_With_Epsilon (Gdouble (Read_Color.Red), 0.2, 0.0001);
         Assert_Cmpfloat_With_Epsilon (Gdouble (Read_Color.Green), 0.4, 0.0001);
         Assert_Cmpfloat_With_Epsilon (Gdouble (Read_Color.Blue), 0.6, 0.0001);
         Assert_True (Read_Font /= null);
         Assert_True
           (Glib.Object.Get_Object (Read_Font) =
              Glib.Object.Get_Object (Font));
         Assert_Cmpfloat_With_Epsilon
           (Gdouble (Read_Offset.x), Gdouble (Offset.x), 0.0001);
         Assert_Cmpfloat_With_Epsilon
           (Gdouble (Read_Offset.y), Gdouble (Offset.y), 0.0001);
         Assert_Cmpuint_Eq (Gsk.Text_Node.Get_Num_Glyphs (Function_Node), 1);
         Assert_True (Read_Glyphs'Length = 1);
         Assert_True (Read_Glyphs (Read_Glyphs'First).Glyph = Glyph_Info (0).Glyph);
         Assert_False (Gsk.Text_Node.Has_Color_Glyphs (Function_Node));
      end;

      --  Initialize on an existing node must leave its original values intact.
      --  The dummy glyph string is unused because the call is a no-op.
      declare
         Dummy_Glyphs : aliased Pango.Glyph_String.Pango_Glyph_String :=
           (Num_Glyphs => 0, Glyphs => System.Null_Address,
            Log_Clusters => null, Space => 0);
      begin
         Procedure_Node.Initialize
           (Font, Dummy_Glyphs'Access, Other_Color'Access, Other_Offset'Access);
      end;
      Assert_Cmpfloat_With_Epsilon
        (Gdouble (Gsk.Text_Node.Get_Color (Procedure_Node).Red), 0.2, 0.0001);
      Assert_Cmpfloat_With_Epsilon
        (Gdouble (Gsk.Text_Node.Get_Offset (Procedure_Node).x),
         Gdouble (Offset.x), 0.0001);

      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Function_Node));
      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Procedure_Node));
      Pango.Glyph_String.Free (Glyph_String);
      Free_Render_Node (Source_Node);
      Layout.Unref;
      Label.Unref;
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
   end Test_Text_Node;

begin
   Glib.Test.Init;
   --  GTK initializes the Pango font map used to shape the test layout.
   Gtk.Main.Init;
   Glib.Test.Add_Func ("/gsk/text-node", Test_Text_Node'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
