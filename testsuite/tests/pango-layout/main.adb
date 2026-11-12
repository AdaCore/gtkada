--  Exercises the Pango.Layout bindings and Pango.Enums.To_Pixels using a
--  context supplied by GTK.
--  No standalone C test covers PangoLayout; this test complements the small
--  pango_layout_get_text/get_attributes checks in testsuite/c_tests/label.c.

with Ada.Command_Line;

with Glib;      use Glib;
with Glib.Object;
with Glib.Test; use Glib.Test;
with Gtk.Label; use Gtk.Label;
with Gtk.Main;
with Pango;
with Pango.Attributes;
with Pango.Context;
with Pango.Enums;
with Pango.Font;
with Pango.Layout; use Pango.Layout;
with Pango.Tabs;
with Gtkada.Bindings;
with Gtkada.Types;

procedure Main is

   use type Pango.Attributes.Pango_Attr_List;
   use type Pango.Context.Pango_Context;
   use type Pango.Font.Pango_Font_Description;
   use type Pango.Layout.Pango_Layout;
   use type Pango.Layout.Pango_Layout_Line;
   use type Pango.Tabs.Pango_Tab_Array;
   use type Pango.Enums.Alignment;
   use type Pango.Enums.Wrap_Mode;
   use type Pango.Tabs.Pango_Tab_Align;

   Two_Lines : constant UTF8_String := "ab" & ASCII.LF & "cd";

   procedure Test_Construction_And_Properties with Convention => C;
   procedure Test_Text_And_Geometry with Convention => C;
   procedure Test_Iterator with Convention => C;
   procedure Test_Pango_Unit_Conversion with Convention => C;

   --  A label gives the layout a backend-ready Pango context without relying
   --  on a display server beyond the test runner's normal Xvfb setup.
   procedure Create_Layout
     (Label   : out Gtk_Label;
      Context : out Pango.Context.Pango_Context;
      Layout  : out Pango_Layout) is
   begin
      Label := Gtk_Label_New ("");
      --  Keep an owned reference so the individual test can release the label
      --  without triggering GTK's floating-reference warning.
      Glib.Object.Ref_Sink (Glib.Object.GObject (Label));
      Context := Label.Get_Pango_Context;
      Layout := Pango_Layout_New (Context);
   end Create_Layout;

   --  Check both the function constructor and the output-parameter constructor.
   --  Setting each property and reading it back checks the public contract.
   procedure Test_Construction_And_Properties is
      Label       : Gtk_Label;
      Context     : Pango.Context.Pango_Context;
      Layout      : Pango_Layout;
      Function_Copy : Pango_Layout;
      Out_Layout  : Pango_Layout;
      Manual      : constant Pango_Layout := new Pango_Layout_Record;
      Attrs       : Pango.Attributes.Pango_Attr_List;
      Tabs        : Pango.Tabs.Pango_Tab_Array;
      Read_Tabs   : Pango.Tabs.Pango_Tab_Array;
      Desc        : Pango.Font.Pango_Font_Description;
      Read_Desc   : Pango.Font.Pango_Font_Description;
      Alignment   : Pango.Tabs.Pango_Tab_Align;
      Location    : Gint;
   begin
      Create_Layout (Label, Context, Layout);

      --  The runtime type IDs identify the layout, boxed iterator, and line
      --  types registered by the linked Pango library.
      Assert_True (Pango.Layout.Get_Type /= 0);
      Assert_True (Pango.Layout.Get_Type_Layout_Iter /= 0);
      Assert_True (Pango.Layout.Get_Type_Layout_Line /= 0);
      Assert_True (Layout.Get_Context = Context);
      Assert_True
        (Layout.Get_Attributes = Pango.Attributes.Null_Pango_Attr_List);
      Assert_True (Layout.Get_Tabs = Pango.Tabs.Null_Pango_Tab_Array);

      --  A manually allocated record is initially uncreated; the constructor
      --  associates it with the context and a repeated call leaves it intact.
      --  Initialize is idempotent once the GObject has been created.
      Initialize (Manual, Context);
      Initialize (Manual, Context);
      Assert_True (Manual.Get_Context = Context);

      Gdk_New (Out_Layout, Context);
      Assert_True (Out_Layout.Get_Context = Context);

      --  A copied layout owns independent text and settings.
      Layout.Set_Text ("original");
      Function_Copy := Layout.Copy;
      Assert_Cmpstr_Eq (Function_Copy.Get_Text, "original");
      Function_Copy.Set_Text ("copy");
      Assert_Cmpstr_Eq (Layout.Get_Text, "original");

      --  These options round-trip enum, boolean, and integer settings through
      --  Pango's native layout object.
      Layout.Set_Alignment (Pango.Enums.Pango_Align_Center);
      Assert_True (Layout.Get_Alignment = Pango.Enums.Pango_Align_Center);
      Layout.Set_Auto_Dir (False);
      Assert_False (Layout.Get_Auto_Dir);
      Layout.Set_Ellipsize (Ellipsize_End);
      Assert_True (Layout.Get_Ellipsize = Ellipsize_End);
      Layout.Set_Height (-2);
      Assert_Cmpint_Eq (Layout.Get_Height, -2);
      Layout.Set_Indent (128);
      Assert_Cmpint_Eq (Layout.Get_Indent, 128);
      Layout.Set_Justify (True);
      Assert_True (Layout.Get_Justify);
      Layout.Set_Single_Paragraph_Mode (True);
      Assert_True (Layout.Get_Single_Paragraph_Mode);
      Layout.Set_Spacing (64);
      Assert_Cmpint_Eq (Layout.Get_Spacing, 64);
      Layout.Set_Width (4096);
      Assert_Cmpint_Eq (Layout.Get_Width, 4096);
      Layout.Set_Wrap (Pango.Enums.Pango_Wrap_Char);
      Assert_True (Layout.Get_Wrap = Pango.Enums.Pango_Wrap_Char);

      --  Pango copies the attribute list and tab array into the layout.
      Attrs := Pango.Attributes.Pango_Attr_List_New;
      Layout.Set_Attributes (Attrs);
      Assert_True
        (Layout.Get_Attributes /= Pango.Attributes.Null_Pango_Attr_List);
      Pango.Attributes.Unref (Attrs);

      Tabs := Pango.Tabs.Pango_Tab_Array_New
        (Initial_Size => 1, Positions_In_Pixels => True);
      Pango.Tabs.Set_Tab (Tabs, 0, Pango.Tabs.Pango_Tab_Left, 24);
      Layout.Set_Tabs (Tabs);
      Read_Tabs := Layout.Get_Tabs;
      --  Get_Tabs returns an owned copy, so inspect and free it separately.
      Assert_Cmpint_Eq (Pango.Tabs.Get_Size (Read_Tabs), 1);
      Assert_True (Pango.Tabs.Get_Positions_In_Pixels (Read_Tabs));
      Pango.Tabs.Get_Tab
        (Read_Tabs, 0, Alignment, Location);
      Assert_True (Alignment = Pango.Tabs.Pango_Tab_Left);
      Assert_Cmpint_Eq (Location, 24);
      Pango.Tabs.Free (Tabs);
      Pango.Tabs.Free (Read_Tabs);

      --  The layout takes its own copy of a font description.
      Desc := Pango.Font.From_String ("Sans 12");
      Layout.Set_Font_Description (Desc);
      Pango.Font.Free (Desc);
      Read_Desc := Layout.Get_Font_Description;
      Assert_True (Read_Desc /= null);
      Assert_Cmpstr_Eq (Pango.Font.Get_Family (Read_Desc), "Sans");

      --  A context change invalidates layout calculations and is safe to call.
      Context.Set_Base_Dir (Pango.Enums.Pango_Direction_Ltr);
      Layout.Context_Changed;
      Assert_Cmpstr_Eq (Layout.Get_Text, "original");

      Glib.Object.Unref (Glib.Object.GObject (Label));
   end Test_Construction_And_Properties;

   --  Verify markup parsing and exercise the coordinate conversion and extent
   --  routines with ordinary ASCII text, where byte indices equal character
   --  indices and expected results are stable.
   procedure Test_Text_And_Geometry is
      Label       : Gtk_Label;
      Context     : Pango.Context.Pango_Context;
      Layout      : Pango_Layout;
      Attrs       : Pango.Attributes.Pango_Attr_List;
      Strong      : Pango.Pango_Rectangle;
      Weak        : Pango.Pango_Rectangle;
      Ink         : Pango.Pango_Rectangle;
      Logical     : Pango.Pango_Rectangle;
      Position    : Pango.Pango_Rectangle;
      Width       : Gint;
      Height      : Gint;
      Pixel_Width : Gint;
      Pixel_Height : Gint;
      Line        : Gint;
      X_Pos       : Gint;
      Index       : Gint;
      Trailing    : Gint;
      New_Index   : Gint;
      New_Trailing : Gint;
      Accel       : Gunichar;
      Exact       : Boolean;
      Raw_Text    : Gtkada.Types.Chars_Ptr;
   begin
      Create_Layout (Label, Context, Layout);
      Layout.Set_Text (Two_Lines);
      Assert_Cmpstr_Eq (Layout.Get_Text, Two_Lines);
      --  This overload exposes the C-owned string directly; converting it
      --  validates both the overloaded binding and its unchanged text value.
      Raw_Text := Pango.Layout.Get_Text (Layout);
      Assert_Cmpstr_Eq
        (Gtkada.Bindings.Value_Allowing_Null (Raw_Text), Two_Lines);
      Assert_Cmpint_Eq (Layout.Get_Character_Count, 5);
      Assert_Cmpint_Ge (Layout.Get_Baseline, 0);
      Assert_Cmpint_Eq (Layout.Get_Line_Count, 2);
      Assert_Cmpint_Ge (Layout.Get_Unknown_Glyphs_Count, 0);
      Assert_Cmpuint_Ge (Layout.Get_Serial, 1);

      --  Pango-unit extents and Get_Size describe the same logical bounds;
      --  pixel extents and pixel size report the scaled device dimensions.
      Layout.Get_Extents (Ink, Logical);
      Assert_Cmpint_Gt (Logical.Width, 0);
      Assert_Cmpint_Gt (Logical.Height, 0);
      Layout.Get_Size (Width, Height);
      Assert_Cmpint_Eq (Width, Logical.Width);
      Assert_Cmpint_Eq (Height, Logical.Height);
      Layout.Get_Pixel_Extents (Ink, Logical);
      Assert_Cmpint_Gt (Logical.Width, 0);
      Layout.Get_Pixel_Size (Pixel_Width, Pixel_Height);
      Assert_Cmpint_Gt (Pixel_Width, 0);
      Assert_Cmpint_Gt (Pixel_Height, 0);

      Layout.Get_Cursor_Pos (1, Strong, Weak);
      Assert_Cmpint_Eq (Strong.Width, 0);
      Assert_Cmpint_Eq (Weak.Width, 0);
      --  Index conversions use UTF-8 byte offsets; this ASCII sample keeps
      --  those offsets equal to the familiar character positions.
      Layout.Index_To_Pos (0, Position);
      Assert_Cmpint_Gt (Position.Width, 0);
      Layout.Index_To_Line_X (0, False, Line, X_Pos);
      Assert_Cmpint_Eq (Line, 0);
      Assert_Cmpint_Ge (X_Pos, 0);
      Layout.Xy_To_Index
        (Position.X + Position.Width / 2,
         Position.Y + Position.Height / 2,
         Index, Trailing, Exact);
      Assert_Cmpint_Ge (Index, 0);
      Assert_Cmpint_Le (Index, 5);
      Assert_Cmpint_Ge (Trailing, 0);
      Layout.Move_Cursor_Visually
        (Strong => True, Old_Index => 0, Old_Trailing => 0, Direction => 1,
         New_Index => New_Index, New_Trailing => New_Trailing);
      Assert_Cmpint_Ge (New_Index, 0);
      Assert_Cmpint_Ge (New_Trailing, 0);

      --  Markup setters replace the displayed text; the accelerator overload
      --  returns the first marked character as a Unicode code point.
      Layout.Set_Markup ("<b>bold</b>");
      Assert_Cmpstr_Eq (Layout.Get_Text, "bold");
      Layout.Set_Markup_With_Accel
        ("_Hello", -1, Gunichar (Character'Pos ('_')), Accel);
      Assert_Cmpuint_Eq (Guint (Accel), Guint (Character'Pos ('H')));
      Assert_Cmpstr_Eq (Layout.Get_Text, "Hello");

      --  Force wrapping and ellipsization with a deliberately tiny width.
      Layout.Set_Text ("a long line that cannot fit in one pixel");
      Layout.Set_Single_Paragraph_Mode (False);
      Layout.Set_Width (1);
      Layout.Set_Ellipsize (Ellipsize_None);
      --  With a narrow positive width, this text must wrap; ellipsizing then
      --  replaces the clipped tail with the configured ellipsis.
      Assert_True (Layout.Is_Wrapped);
      Layout.Set_Ellipsize (Ellipsize_End);
      Assert_True (Layout.Is_Ellipsized);

      --  Clearing optional layout data restores the Pango defaults.
      Layout.Set_Attributes (Pango.Attributes.Null_Pango_Attr_List);
      Attrs := Layout.Get_Attributes;
      Assert_True (Attrs = Pango.Attributes.Null_Pango_Attr_List);
      Layout.Set_Tabs (Pango.Tabs.Null_Pango_Tab_Array);
      Assert_True (Layout.Get_Tabs = Pango.Tabs.Null_Pango_Tab_Array);
      Glib.Object.Unref (Glib.Object.GObject (Label));
   end Test_Text_And_Geometry;

   --  An explicit line break gives the iterator multiple lines and makes all
   --  four movement methods observable without depending on fonts or display
   --  metrics.
   procedure Test_Iterator is
      Label        : Gtk_Label;
      Context      : Pango.Context.Pango_Context;
      Layout       : Pango_Layout;
      Iter         : Pango_Layout_Iter;
      Iter_Copy    : Pango_Layout_Iter;
      Ink          : Pango.Pango_Rectangle;
      Logical      : Pango.Pango_Rectangle;
      Char_Rect    : Pango.Pango_Rectangle;
      Y0           : Gint;
      Y1           : Gint;
   begin
      Create_Layout (Label, Context, Layout);
      Layout.Set_Text (Two_Lines);
      Iter := Layout.Get_Iter;

      --  The iterator starts at the first character and each extent routine
      --  reports coordinates in the layout's Pango-unit space.
      Assert_False (Iter.At_Last_Line);
      Assert_Cmpint_Eq (Iter.Get_Index, 0);
      Assert_True (Iter.Get_Layout = Layout);
      Assert_Cmpint_Gt (Iter.Get_Baseline, 0);
      Iter.Get_Char_Extents (Char_Rect);
      Assert_Cmpint_Gt (Char_Rect.Height, 0);
      Iter.Get_Cluster_Extents (Ink, Logical);
      Assert_Cmpint_Gt (Logical.Height, 0);
      Iter.Get_Layout_Extents (Ink, Logical);
      Assert_Cmpint_Gt (Logical.Width, 0);
      Iter.Get_Line_Extents (Ink, Logical);
      Assert_Cmpint_Gt (Logical.Height, 0);
      Iter.Get_Line_Yrange (Y0, Y1);
      Assert_Cmpint_Gt (Y1, Y0);
      Iter.Get_Run_Extents (Ink, Logical);
      Assert_Cmpint_Gt (Logical.Height, 0);
      Assert_True (Iter.Get_Line = Iter.Get_Line_Readonly);
      Assert_True (Layout.Get_Line (0) = Layout.Get_Line_Readonly (0));

      --  Copy creates an independently owned iterator at the same position.
      Iter_Copy := Iter.Copy;
      Assert_Cmpint_Eq (Iter_Copy.Get_Index, Iter.Get_Index);
      Free (Iter_Copy);

      --  Each advance returns True while movement is possible, then False at
      --  the end. Fresh iterators keep the paths independent.
      Assert_True (Iter.Next_Char);
      while Iter.Next_Char loop
         null;
      end loop;
      Assert_False (Iter.Next_Char);
      Free (Iter);

      Iter := Layout.Get_Iter;
      Assert_True (Iter.Next_Cluster);
      while Iter.Next_Cluster loop
         null;
      end loop;
      Assert_False (Iter.Next_Cluster);
      Free (Iter);

      Iter := Layout.Get_Iter;
      Assert_True (Iter.Next_Line);
      Assert_True (Iter.At_Last_Line);
      Assert_False (Iter.Next_Line);
      Free (Iter);

      Iter := Layout.Get_Iter;
      Assert_True (Iter.Next_Run);
      while Iter.Next_Run loop
         null;
      end loop;
      Assert_False (Iter.Next_Run);
      Free (Iter);
      Glib.Object.Unref (Glib.Object.GObject (Label));
   end Test_Iterator;

   --  Pango units use 1024 units per pixel. Check nearest-pixel rounding on
   --  either side of zero and verify that zero follows the nonpositive path.
   procedure Test_Pango_Unit_Conversion is
   begin
      Assert_Cmpint_Eq (Pango.Enums.To_Pixels (512), 1);
      Assert_Cmpint_Eq (Pango.Enums.To_Pixels (-512), -1);
      Assert_Cmpint_Eq (Pango.Enums.To_Pixels (0), 0);
   end Test_Pango_Unit_Conversion;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/pango-layout/construction-properties",
      Test_Construction_And_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-layout/text-geometry",
      Test_Text_And_Geometry'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-layout/iterator",
      Test_Iterator'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-enums/to-pixels",
      Test_Pango_Unit_Conversion'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
