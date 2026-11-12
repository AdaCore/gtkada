--  Exercise Pango.Glyph_String creation, geometry, coordinate conversion,
--  Pango.Item allocation, and GtkAda's wrapper ownership helpers. No matching
--  C test exists to port for either unit.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;      use Glib;
with Glib.Object;
with Glib.Test; use Glib.Test;
with Gtk.Label; use Gtk.Label;
with Gtk.Main;
with Pango;
with Pango.Context;
with Pango.Font;
with Pango.Glyph_String; use Pango.Glyph_String;
with Pango.Item;

procedure Main is
   use type System.Address;
   use type Pango.Font.Pango_Font;

   --  These constrained overlays view the C arrays allocated by Set_Size.
   type Three_Glyphs is array (0 .. 2) of Pango_Glyph_Info;
   pragma Convention (C, Three_Glyphs);
   type Three_Clusters is array (0 .. 2) of Glib.Gint;
   pragma Convention (C, Three_Clusters);
   type Glyph_Array_Access is access all Three_Glyphs;
   type Cluster_Array_Access is access all Three_Clusters;
   function To_Glyph_Array is new Ada.Unchecked_Conversion
     (System.Address, Glyph_Array_Access);
   function To_Cluster_Array is new Ada.Unchecked_Conversion
     (System.Address, Cluster_Array_Access);

   --  From_Object_Free uses g_free for its temporary record allocation.
   type Vis_Attr_Access is access all Pango_Glyph_Vis_Attr;
   type Geometry_Access is access all Pango_Glyph_Geometry;
   type Info_Access is access all Pango_Glyph_Info;
   type String_Access is access all Pango_Glyph_String;
   type Analysis_Access is access all Pango.Item.Pango_Analysis;
   type Item_Access is access all Pango.Item.Pango_Item;
   function To_Vis_Attr is new Ada.Unchecked_Conversion
     (System.Address, Vis_Attr_Access);
   function To_Geometry is new Ada.Unchecked_Conversion
     (System.Address, Geometry_Access);
   function To_Info is new Ada.Unchecked_Conversion
     (System.Address, Info_Access);
   function To_String is new Ada.Unchecked_Conversion
     (System.Address, String_Access);
   function To_Analysis is new Ada.Unchecked_Conversion
     (System.Address, Analysis_Access);
   function To_Item is new Ada.Unchecked_Conversion
     (System.Address, Item_Access);

   function G_Malloc (Size : Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Test_Glyph_String with Convention => C;
   procedure Test_Wrapper_Conversions with Convention => C;
   procedure Test_Pango_Item with Convention => C;

   --  This case covers every public operation implemented in the unit body.
   --  Set up an owned GTK Pango context and a concrete font for extent calls.
   procedure Test_Glyph_String is
      Label       : Gtk_Label;
      Context     : Pango.Context.Pango_Context;
      Description : Pango.Font.Pango_Font_Description;
      Font        : Pango.Font.Pango_Font;
      Glyphs      : access Pango_Glyph_String := Pango_Glyph_String_New;
      Copy_Glyphs : access Pango_Glyph_String;
      Widths      : Gint_Array (0 .. 2);
      --  Coordinate conversion needs a font and a left-to-right run level.
      Analysis    : aliased Pango.Item.Pango_Analysis :=
        (Shape_Engine => System.Null_Address,
         Lang_Engine  => System.Null_Address,
         Font         => System.Null_Address,
         Level        => 0,
         Gravity      => 0,
         Flags        => 0,
         Script       => 0,
         Language     => System.Null_Address,
         Extra_Attrs  => System.Null_Address);
      Ink         : Pango.Pango_Rectangle;
      Logical     : Pango.Pango_Rectangle;
      Range_Ink   : Pango.Pango_Rectangle;
      Range_Logical : Pango.Pango_Rectangle;
      X_Pos       : Gint;
      Index       : Gint;
      Trailing    : Gint;
   begin
      Label := Gtk_Label_New ("");
      --  Sink the label's floating reference so it can be released explicitly.
      Glib.Object.Ref_Sink (Glib.Object.GObject (Label));
      Context := Label.Get_Pango_Context;
      Description := Pango.Font.From_String ("Sans 12");
      Font := Context.Load_Font (Description);
      Pango.Font.Free (Description);
      Assert_True (Font /= null);
      Analysis.Font := Glib.Object.Get_Object (Font);

      --  A newly allocated glyph string starts empty; Set_Size owns storage
      --  for both glyph data and log-cluster indices.
      Assert_True (Glyphs /= null);
      Assert_Cmpint_Eq (Glyphs.Num_Glyphs, 0);
      Set_Size (Glyphs, 3);
      Assert_Cmpint_Eq (Glyphs.Num_Glyphs, 3);

      --  Use three one-character clusters with known advances so the results
      --  of width distribution and x/index conversion are deterministic.
      To_Glyph_Array (Glyphs.Glyphs).all :=
        (0 => (Glyph => Pango.Pango_Glyph (1),
               Geometry => (Width => 300, X_Offset => 0, Y_Offset => 0),
               Attr => (Is_Cluster_Start => 1)),
         1 => (Glyph => Pango.Pango_Glyph (2),
               Geometry => (Width => 200, X_Offset => 0, Y_Offset => 0),
               Attr => (Is_Cluster_Start => 1)),
         2 => (Glyph => Pango.Pango_Glyph (3),
               Geometry => (Width => 400, X_Offset => 0, Y_Offset => 0),
               Attr => (Is_Cluster_Start => 1)));
      To_Cluster_Array (Glyphs.Log_Clusters.all'Address).all :=
        (0 => 0, 1 => 1, 2 => 2);

      --  The fast width query and the full extents API agree on logical width.
      Assert_Cmpint_Eq (Get_Width (Glyphs), 900);
      Extents (Glyphs, Font, Ink, Logical);
      Assert_Cmpint_Eq (Logical.Width, 900);
      Assert_Cmpint_Gt (Logical.Height, 0);
      Extents_Range (Glyphs, 1, 2, Font, Range_Ink, Range_Logical);
      Assert_Cmpint_Eq (Range_Logical.Width, 200);

      --  One glyph per character leaves each character with its full advance.
      Get_Logical_Widths (Glyphs, "abc", 3, 0, Widths);
      Assert_Cmpint_Eq (Widths (0), 300);
      Assert_Cmpint_Eq (Widths (1), 200);
      Assert_Cmpint_Eq (Widths (2), 400);

      --  Both edges of the second character map to its advance boundaries.
      Index_To_X (Glyphs, "abc", 3, Analysis'Access, 1, False, X_Pos);
      Assert_Cmpint_Eq (X_Pos, 300);
      Index_To_X (Glyphs, "abc", 3, Analysis'Access, 1, True, X_Pos);
      Assert_Cmpint_Eq (X_Pos, 500);
      X_To_Index (Glyphs, "abc", 3, Analysis'Access, 350, Index, Trailing);
      Assert_Cmpint_Eq (Index, 1);
      Assert_Cmpint_Eq (Trailing, 0);

      --  Copy duplicates the glyph storage; freeing either value is independent.
      Copy_Glyphs := Copy (Glyphs);
      Assert_True (Copy_Glyphs /= null);
      Assert_Cmpint_Eq (Get_Width (Copy_Glyphs), Get_Width (Glyphs));
      Free (Copy_Glyphs);
      Free (Glyphs);
      Glib.Object.Unref (Glib.Object.GObject (Font));
      Glib.Object.Unref (Glib.Object.GObject (Label));
   end Test_Glyph_String;

   --  Check each GtkAda helper returns a value copy while freeing only its
   --  temporary wrapper allocated by GLib's allocator.
   procedure Test_Wrapper_Conversions is
      --  Allocate enough C storage for each record, including its padding.
      Vis_Bytes : constant Gsize :=
        Gsize ((Pango_Glyph_Vis_Attr'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
      Geometry_Bytes : constant Gsize :=
        Gsize ((Pango_Glyph_Geometry'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
      Info_Bytes : constant Gsize :=
        Gsize ((Pango_Glyph_Info'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
      String_Bytes : constant Gsize :=
        Gsize ((Pango_Glyph_String'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
      Vis_Wrapper : constant Vis_Attr_Access :=
        To_Vis_Attr (G_Malloc (Vis_Bytes));
      Geometry_Wrapper : constant Geometry_Access :=
        To_Geometry (G_Malloc (Geometry_Bytes));
      --  These wrappers are distinct because each conversion consumes memory.
      Info_Wrapper : constant Info_Access := To_Info (G_Malloc (Info_Bytes));
      String_Wrapper : constant String_Access :=
        To_String (G_Malloc (String_Bytes));
      Vis_Value : Pango_Glyph_Vis_Attr;
      Geometry_Value : Pango_Glyph_Geometry;
      Info_Value : Pango_Glyph_Info;
      String_Value : Pango_Glyph_String;
   begin
      Vis_Wrapper.all := (Is_Cluster_Start => 1);
      Vis_Value := From_Object_Free (Vis_Wrapper);
      Assert_Cmpuint_Eq (Vis_Value.Is_Cluster_Start, 1);

      Geometry_Wrapper.all := (Width => 64, X_Offset => 2, Y_Offset => -3);
      Geometry_Value := From_Object_Free (Geometry_Wrapper);
      Assert_Cmpint_Eq (Gint (Geometry_Value.Width), 64);
      Assert_Cmpint_Eq (Gint (Geometry_Value.X_Offset), 2);
      Assert_Cmpint_Eq (Gint (Geometry_Value.Y_Offset), -3);

      Info_Wrapper.all :=
        (Glyph => Pango.Pango_Glyph (7),
         Geometry => (Width => 128, X_Offset => 4, Y_Offset => 5),
         Attr => (Is_Cluster_Start => 1));
      Info_Value := From_Object_Free (Info_Wrapper);
      Assert_Cmpuint_Eq (Guint (Info_Value.Glyph), 7);
      Assert_Cmpint_Eq (Gint (Info_Value.Geometry.Width), 128);

      --  Null nested pointers isolate the record copy from owned glyph storage.
      String_Wrapper.all :=
        (Num_Glyphs => 2,
         Glyphs => System.Null_Address,
         Log_Clusters => null,
         Space => 2);
      String_Value := From_Object_Free (String_Wrapper);
      Assert_Cmpint_Eq (String_Value.Num_Glyphs, 2);
      Assert_Cmpint_Eq (String_Value.Space, 2);
   end Test_Wrapper_Conversions;

   --  Check the generated Pango.Item constructor and both record conversion
   --  helpers. The temporary wrappers use g_malloc because the helpers release
   --  their wrapper storage with g_free; their nested pointers stay null so
   --  the returned shallow record copies own no external allocations.
   procedure Test_Pango_Item is
      Analysis_Bytes : constant Gsize :=
        Gsize ((Pango.Item.Pango_Analysis'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
      Item_Bytes : constant Gsize :=
        Gsize ((Pango.Item.Pango_Item'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
      Analysis_Wrapper : constant Analysis_Access :=
        To_Analysis (G_Malloc (Analysis_Bytes));
      Item_Wrapper : constant Item_Access := To_Item (G_Malloc (Item_Bytes));
      Analysis_Value : Pango.Item.Pango_Analysis;
      Item_Value : Pango.Item.Pango_Item;
      New_Item : access Pango.Item.Pango_Item :=
        Pango.Item.Pango_Item_New;
   begin
      --  Pango initializes a new item to an empty range with default analysis.
      Assert_True (New_Item /= null);
      Assert_Cmpint_Eq (New_Item.Offset, 0);
      Assert_Cmpint_Eq (New_Item.Length, 0);
      Assert_Cmpint_Eq (New_Item.Num_Chars, 0);
      Assert_True (New_Item.Analysis.Font = System.Null_Address);
      Pango.Item.Free (New_Item);

      --  Conversion returns a value copy and frees only the GLib wrapper.
      Analysis_Wrapper.all :=
        (Shape_Engine => System.Null_Address,
         Lang_Engine  => System.Null_Address,
         Font         => System.Null_Address,
         Level        => 2,
         Gravity      => 1,
         Flags        => 3,
         Script       => 4,
         Language     => System.Null_Address,
         Extra_Attrs  => System.Null_Address);
      Analysis_Value := Pango.Item.From_Object_Free (Analysis_Wrapper);
      Assert_Cmpuint_Eq (Guint (Analysis_Value.Level), 2);
      Assert_Cmpuint_Eq (Guint (Analysis_Value.Gravity), 1);
      Assert_Cmpuint_Eq (Guint (Analysis_Value.Flags), 3);
      Assert_Cmpuint_Eq (Guint (Analysis_Value.Script), 4);

      --  Null nested addresses make this record copy safe to inspect and drop.
      Item_Wrapper.all :=
        (Offset   => 7,
         Length   => 11,
         Num_Chars => 5,
         Analysis => Analysis_Value);
      Item_Value := Pango.Item.From_Object_Free (Item_Wrapper);
      Assert_Cmpint_Eq (Item_Value.Offset, 7);
      Assert_Cmpint_Eq (Item_Value.Length, 11);
      Assert_Cmpint_Eq (Item_Value.Num_Chars, 5);
      Assert_Cmpuint_Eq (Guint (Item_Value.Analysis.Level), 2);
      Assert_True (Item_Value.Analysis.Extra_Attrs = System.Null_Address);
   end Test_Pango_Item;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/pango-glyph-string/geometry-and-indexing",
      Test_Glyph_String'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-glyph-string/wrapper-conversions",
      Test_Wrapper_Conversions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-item/new-and-wrapper-conversions",
      Test_Pango_Item'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
