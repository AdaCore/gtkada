--  Exercises the Pango.Font accessors bound by this branch, and in
--  particular the nullable instance parameter of pango_font_get_font_map:
--  the C function is documented to return NULL when handed a NULL font, so
--  the binding must accept a null access value rather than reject it with
--  "not null access".

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;       use Glib;
with Glib.Test;  use Glib.Test;
with Glib.Object;
with Gtk.Label;  use Gtk.Label;
with Gtk.Main;
with Gtk.Window; use Gtk.Window;
with Pango.Context;
with Pango.Coverage;
with Pango.Enums;
with Pango.Font; use Pango.Font;
with Pango.Font_Face;
with Pango.Font_Family;
with Pango.Font_Map;
with Pango.Font_Metrics;
with Pango.Language;

procedure Main is

   use type System.Address;
   use type Glib.Gdouble;
   use type Glib.Gint;
   use type Pango.Enums.Style;
   use type Pango.Enums.Variant;
   use type Pango.Enums.Weight;
   use type Pango.Enums.Stretch;
   use type Pango.Enums.Coverage_Level;
   use type Pango.Font_Face.Pango_Font_Face;
   use type Pango.Font_Family.Pango_Font_Family;
   use type Pango.Coverage.Pango_Coverage;
   use type Pango.Font_Map.Pango_Font_Map;

   --  From_Object_Free releases its temporary record with g_free, so give it
   --  a wrapper allocated by GLib and store the already-owned metrics value.
   type Font_Metrics_Access is access all
     Pango.Font_Metrics.Pango_Font_Metrics;
   function To_Font_Metrics_Access is new Ada.Unchecked_Conversion
     (System.Address, Font_Metrics_Access);

   function G_Malloc (Size : Glib.Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Test_Null_Font
   with Convention => C;

   procedure Test_Descriptions
   with Convention => C;

   procedure Test_Font_Map
   with Convention => C;

   procedure Test_Font_Queries
   with Convention => C;

   --------------------
   -- Test_Null_Font --
   --------------------

   --  pango_font_get_font_map (NULL) returns NULL.

   procedure Test_Null_Font is
   begin
      Assert_True (Pango.Font.Get_Font_Map (null) = null);
   end Test_Null_Font;

   ------------------------
   -- Test_Descriptions --
   ------------------------

   --  Exercise the owned description constructors, conversions, helpers,
   --  and merge operations while checking their resulting values.

   procedure Test_Descriptions is
      From_Function : Pango_Font_Description :=
        Pango_Font_Description_New;
      From_Procedure : Pango_Font_Description;
      Description : Pango_Font_Description;
      Other       : Pango_Font_Description;
      Copy_Desc   : Pango_Font_Description;
   begin
      --  Both constructors return a usable description that Free clears.
      Gdk_New (From_Procedure);
      Set_Family (From_Function, "Noto Sans");
      Set_Family (From_Procedure, "Noto Sans");
      Assert_True (Get_Family (From_Function) = "Noto Sans");
      Assert_True (Get_Family (From_Procedure) = "Noto Sans");

      --  The address adapter used by boxed GObject properties exposes the
      --  description record itself, and the normal-size helper formats style
      --  and weight names as "Normal".
      Assert_True
        (To_Address (From_Function, System.Null_Address) /=
           System.Null_Address);
      Assert_True (Get_Style_As_String (From_Function) = "Normal");
      Assert_True (Get_Weight_As_String (From_Function) = "Normal");

      --  The Ada convenience constructor maps enum choices through Pango's
      --  font-description parser. Keep the non-normal weight with the normal
      --  variant because this helper currently joins those two tokens when
      --  both are non-normal.
      Description := To_Font_Description
        (Family_Name => "Sans",
         Style       => Pango.Enums.Pango_Style_Italic,
         Variant     => Pango.Enums.Pango_Variant_Normal,
         Weight      => Pango.Enums.Pango_Weight_Bold,
         Stretch     => Pango.Enums.Pango_Stretch_Condensed,
         Size        => 12);
      Assert_True (Get_Family (Description) = "Sans");
      Assert_True (Get_Style (Description) = Pango.Enums.Pango_Style_Italic);
      Assert_True (Get_Variant (Description) = Pango.Enums.Pango_Variant_Normal);
      Assert_True (Get_Weight (Description) = Pango.Enums.Pango_Weight_Bold);
      Assert_True
        (Get_Stretch (Description) = Pango.Enums.Pango_Stretch_Condensed);
      Assert_True (Get_Style_As_String (Description) = "Italic ");
      Assert_True (Get_Weight_As_String (Description) = "Bold      ");

      --  Small-caps parses correctly when weight is left at its default.
      Copy_Desc := To_Font_Description
        (Family_Name => "Sans",
         Variant     => Pango.Enums.Pango_Variant_Small_Caps);
      Assert_True
        (Get_Variant (Copy_Desc) = Pango.Enums.Pango_Variant_Small_Caps);
      Free (Copy_Desc);

      --  A point size is relative; Set_Absolute_Size changes the size mode.
      Assert_False (Get_Size_Is_Absolute (Description));
      Set_Absolute_Size (Description, 12.0 * 1024.0);
      Assert_True (Get_Size_Is_Absolute (Description));

      --  The copied family and variation setters round-trip their input.
      Set_Family (Description, "Noto Serif");
      Assert_True (Get_Family (Description) = "Noto Serif");
      Set_Variations (Description, "wght=500");
      Assert_True (Get_Variations (Description) = "wght=500");

      --  String and filename forms are allocated strings with meaningful
      --  content; copies compare equal, and a parsed filename round-trips.
      Assert_True (To_String (Description)'Length > 0);
      Assert_True (To_Filename (Description)'Length > 0);
      Copy_Desc := Copy (Description);
      Assert_True (Equal (Description, Copy_Desc));
      Free (Copy_Desc);
      Copy_Desc := Copy_Static (Description);
      Assert_True (Equal (Description, Copy_Desc));
      Free (Copy_Desc);

      --  Better_Match accepts an exact candidate when there is no old match.
      Assert_True (Better_Match (Description, null, Description));

      --  Merge without replacement keeps the base family and size while
      --  filling unset attributes; replacement then copies all source fields.
      Free (Description);
      Description := From_String ("Sans 12");
      Other := From_String ("Serif Italic 18");
      Merge (Description, Other, Replace_Existing => False);
      Assert_True (Get_Family (Description) = "Sans");
      Merge (Description, Other, Replace_Existing => True);
      Assert_True (Get_Family (Description) = "Serif");
      Free (Description);

      Description := From_String ("Sans 12");
      Merge_Static (Description, Other, Replace_Existing => False);
      Assert_True (Get_Family (Description) = "Sans");
      Merge_Static (Description, Other, Replace_Existing => True);
      Assert_True (Get_Family (Description) = "Serif");

      Free (Other);
      Free (Description);
      Free (From_Function);
      Free (From_Procedure);
      Assert_True (From_Function = null);
      Assert_True (From_Procedure = null);
   end Test_Descriptions;

   -------------------
   -- Test_Font_Map --
   -------------------

   --  A font loaded from a context reports back the context's font map.

   procedure Test_Font_Map is
      Window  : Gtk_Window;
      Label   : Gtk_Label;
      Context : Pango.Context.Pango_Context;
      Desc    : Pango_Font_Description;
      Font    : Pango_Font;
   begin
      Window := Gtk_Window_New;
      Gtk_New (Label, "hello");
      Window.Set_Child (Label);

      Context := Label.Get_Pango_Context;

      Desc := From_String ("Sans 12");
      Font := Context.Load_Font (Desc);
      Free (Desc);

      Assert_True (Font /= null);
      Assert_True (Pango.Font.Get_Font_Map (Font) = Context.Get_Font_Map);

      Window.Destroy;
   end Test_Font_Map;

   ------------------------
   -- Test_Font_Queries --
   ------------------------

   --  Query a real font from a GTK Pango context. This exercises wrapper
   --  conversions for its descriptions, face, coverage, metrics, and map.

   procedure Test_Font_Queries is
      Window       : Gtk_Window;
      Label        : Gtk_Label;
      Context      : Pango.Context.Pango_Context;
      Description  : Pango_Font_Description := From_String ("Sans 12");
      Font         : Pango_Font;
      Face         : Pango.Font_Face.Pango_Font_Face;
      Face_Family  : Pango.Font_Family.Pango_Font_Family;
      Face_Desc    : Pango_Font_Description;
      Font_Desc    : Pango_Font_Description;
      Absolute_Desc : Pango_Font_Description;
      Language     : constant Pango.Language.Pango_Language :=
        Pango.Language.From_String ("en");
      Coverage     : Pango.Coverage.Pango_Coverage;
      Metrics      : Pango.Font_Metrics.Pango_Font_Metrics;
      Metrics_Ref  : Pango.Font_Metrics.Pango_Font_Metrics;
      Ink          : Pango.Pango_Rectangle;
      Logical      : Pango.Pango_Rectangle;
   begin
      Window := Gtk_Window_New;
      Gtk_New (Label, "font queries");
      Window.Set_Child (Label);
      Context := Label.Get_Pango_Context;
      Font := Context.Load_Font (Description);
      Free (Description);

      Assert_True (Font /= null);
      Assert_True (Pango.Font.Get_Font_Map (Font) = Context.Get_Font_Map);
      Assert_True
        (Pango.Font.Has_Char
           (Font, Glib.Gunichar (Character'Pos ('A'))));

      --  Both description calls return independently owned structures; the
      --  absolute-size form records a device-unit size.
      Font_Desc := Pango.Font.Describe (Font);
      Absolute_Desc := Pango.Font.Describe_With_Absolute_Size (Font);
      Assert_True (Get_Family (Font_Desc)'Length > 0);
      Assert_True (Get_Size_Is_Absolute (Absolute_Desc));
      Free (Font_Desc);
      Free (Absolute_Desc);

      --  A font face is borrowed from the font, while coverage is returned
      --  with an owned reference that the caller releases with Unref.
      Face := Pango.Font.Get_Face (Font);
      Assert_True (Face /= null);
      --  The face belongs to a family, has a displayable style name, and the
      --  regular Sans face is not synthesized from another face.
      Face_Family := Face.Get_Family;
      Assert_True (Face_Family /= null);
      Assert_True (Pango.Font_Family.Get_Name (Face_Family)'Length > 0);
      Assert_True (Face.Get_Face_Name'Length > 0);
      Assert_False (Face.Is_Synthesized);

      --  Sans is scalable, so Pango reports no discrete bitmap sizes.
      declare
         Face_Sizes : constant Glib.Gint_Array := Face.List_Sizes;
      begin
         Assert_True (Face_Sizes'Length = 0);
      end;

      Face_Desc := Face.Describe;
      Assert_True (Get_Family (Face_Desc)'Length > 0);
      Free (Face_Desc);

      Coverage := Pango.Font.Get_Coverage (Font, Language);
      Assert_True (Coverage /= null);
      Assert_True
        (Coverage.Get (Character'Pos ('A')) /=
           Pango.Enums.Pango_Coverage_None);
      Pango.Coverage.Unref (Coverage);

      --  Metrics are owned boxed values. Check each documented measurement
      --  for a named font language, then confirm Ref preserves every value.
      Metrics := Pango.Font.Get_Metrics (Font, Language);
      declare
         Char_Width : constant Glib.Gint :=
           Pango.Font_Metrics.Get_Approximate_Char_Width (Metrics);
         Digit_Width : constant Glib.Gint :=
           Pango.Font_Metrics.Get_Approximate_Digit_Width (Metrics);
         Ascent : constant Glib.Gint :=
           Pango.Font_Metrics.Get_Ascent (Metrics);
         Descent : constant Glib.Gint :=
           Pango.Font_Metrics.Get_Descent (Metrics);
         Height : constant Glib.Gint :=
           Pango.Font_Metrics.Get_Height (Metrics);
         Strike_Position : constant Glib.Gint :=
           Pango.Font_Metrics.Get_Strikethrough_Position (Metrics);
         Strike_Thickness : constant Glib.Gint :=
           Pango.Font_Metrics.Get_Strikethrough_Thickness (Metrics);
         Underline_Position : constant Glib.Gint :=
           Pango.Font_Metrics.Get_Underline_Position (Metrics);
         Underline_Thickness : constant Glib.Gint :=
           Pango.Font_Metrics.Get_Underline_Thickness (Metrics);
      begin
         --  A loaded font provides positive widths and ascent, plus positive
         --  thicknesses for both decoration styles.
         Assert_True (Char_Width > 0);
         Assert_True (Digit_Width > 0);
         Assert_True (Ascent > 0);
         Assert_True (Descent >= 0);
         --  A backend may omit line height; otherwise it spans ascent and descent.
         Assert_True (Height = 0 or else Height >= Ascent + Descent);
         Assert_True (Strike_Position > 0);
         Assert_True (Strike_Thickness > 0);
         Assert_True (Underline_Position <= 0);
         Assert_True (Underline_Thickness > 0);

         --  Ref returns another owned reference to the same metrics object;
         --  equal getter results confirm both handles remain usable.
         Metrics_Ref := Pango.Font_Metrics.Ref (Metrics);
         Assert_True
           (Pango.Font_Metrics.Get_Approximate_Char_Width (Metrics_Ref) =
              Char_Width);
         Assert_True
           (Pango.Font_Metrics.Get_Approximate_Digit_Width (Metrics_Ref) =
              Digit_Width);
         Assert_True (Pango.Font_Metrics.Get_Ascent (Metrics_Ref) = Ascent);
         Assert_True (Pango.Font_Metrics.Get_Descent (Metrics_Ref) = Descent);
         Assert_True (Pango.Font_Metrics.Get_Height (Metrics_Ref) = Height);
         Assert_True
           (Pango.Font_Metrics.Get_Strikethrough_Position (Metrics_Ref) =
              Strike_Position);
         Assert_True
           (Pango.Font_Metrics.Get_Strikethrough_Thickness (Metrics_Ref) =
              Strike_Thickness);
         Assert_True
           (Pango.Font_Metrics.Get_Underline_Position (Metrics_Ref) =
              Underline_Position);
         Assert_True
           (Pango.Font_Metrics.Get_Underline_Thickness (Metrics_Ref) =
              Underline_Thickness);
      end;
      Pango.Font_Metrics.Unref (Metrics);
      Pango.Font_Metrics.Unref (Metrics_Ref);

      --  The generated conversion helper copies the boxed pointer from a
      --  GLib-allocated Ada wrapper and frees that wrapper, not the metrics.
      --  The converted value retains the owned reference, which is released
      --  once below after the check.
      Metrics := Pango.Font.Get_Metrics (Font);
      declare
         Wrapper_Bytes : constant Glib.Gsize :=
           Glib.Gsize
             ((Pango.Font_Metrics.Pango_Font_Metrics'Size
                + System.Storage_Unit - 1)
              / System.Storage_Unit);
         Wrapper : constant Font_Metrics_Access :=
           To_Font_Metrics_Access (G_Malloc (Wrapper_Bytes));
         Converted : Pango.Font_Metrics.Pango_Font_Metrics;
      begin
         Wrapper.all := Metrics;
         Converted := Pango.Font_Metrics.From_Object_Free (Wrapper);
         Assert_True (Pango.Font_Metrics.Get_Ascent (Converted) > 0);
         Pango.Font_Metrics.Unref (Converted);
      end;

      --  Glyph extents return rectangles in Pango units. Glyph one is the
      --  font's missing-glyph box, which has a non-negative logical width.
      Pango.Font.Get_Glyph_Extents
        (Font, Pango.Pango_Glyph (1), Ink, Logical);
      Assert_True (Logical.Width >= 0);
      Assert_True (Logical.Height >= 0);

      Glib.Object.Unref (Glib.Object.GObject (Font));
      Window.Destroy;
   end Test_Font_Queries;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/pango-font/null-font", Test_Null_Font'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-font/descriptions", Test_Descriptions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-font/font-map", Test_Font_Map'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-font/font-queries", Test_Font_Queries'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
