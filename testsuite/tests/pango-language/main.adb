--  Exercise Pango.Language conversion, samples, scripts, and language-range
--  matching. No registered C test case directly covers these operations.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;      use Glib;
with Glib.Test; use Glib.Test;
with Pango.Language; use Pango.Language;

procedure Main is
   use type Glib.Gint;

   procedure Test_Language_Values
   with Convention => C;

   procedure Test_Wrapper_Conversion
   with Convention => C;

   --  From_Object_Free releases its wrapper with g_free, so allocate that
   --  temporary wrapper with GLib's allocator instead of Ada's allocator.
   type Language_Access is access all Pango_Language;
   function To_Language_Access is new Ada.Unchecked_Conversion
     (System.Address, Language_Access);

   --------------------------
   -- Test_Language_Values --
   --------------------------

   --  Canonical tags round-trip, and the English sample and script metadata
   --  demonstrate the language-specific queries on a known tag.
   procedure Test_Language_Values is
      English : constant Pango_Language := From_String ("EN_us");
      Scripts : constant Pango_Script_Array := Get_Scripts (English);
      Unknown : constant Pango_Language := From_String ("xx");
      No_Scripts : constant Pango_Script_Array := Get_Scripts (Unknown);
      Default : constant Pango_Language := Get_Default;
   begin
      Assert_Cmpstr_Eq (To_String (English), "en-us");
      Assert_True (Get_Sample_String (English)'Length > 0);
      Assert_Cmpuint_Gt (Guint (Scripts'Length), 0);
      Assert_True (Scripts (Scripts'First) = Pango_Script_Latin);
      Assert_True (Includes_Script (English, Pango_Script_Latin));
      Assert_False (Includes_Script (English, Pango_Script_Cyrillic));

      --  "xx" is the documented placeholder for an unrecognized tag: it has
      --  no known script list, so Pango conservatively accepts any script.
      Assert_Cmpuint_Eq (Guint (No_Scripts'Length), 0);
      Assert_True (Includes_Script (Unknown, Pango_Script_Cyrillic));
      Assert_True (Get_Sample_String (Unknown)'Length > 0);

      --  Pango accepts exact tags, subtags, and wildcard ranges, but does not
      --  confuse a longer unrelated tag with a matching prefix.
      Assert_True (Matches (English, "en-us"));
      Assert_True (Matches (English, "en"));
      Assert_True (Matches (English, "fr, en"));
      Assert_True (Matches (English, "*"));
      Assert_False (Matches (English, "fr de"));
      Assert_False (Matches (English, "en-uk"));

      --  The environment's default language is valid even when the process
      --  locale is the portable C locale.
      Assert_True (To_String (Default)'Length > 0);
   end Test_Language_Values;

   -----------------------------
   -- Test_Wrapper_Conversion --
   -----------------------------

   --  The generated conversion copies a borrowed Pango language handle and
   --  frees only the temporary Ada record allocated through GLib.
   procedure Test_Wrapper_Conversion is
      function G_Malloc (Size : Gsize) return System.Address;
      pragma Import (C, G_Malloc, "g_malloc");

      Original : constant Pango_Language := From_String ("fr");
      Converted : Pango_Language;
      Wrapper_Bytes : constant Gsize :=
        Gsize ((Pango_Language'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
      Wrapper : constant Language_Access :=
        To_Language_Access (G_Malloc (Wrapper_Bytes));
   begin
      Wrapper.all.Set_Object (Get_Object (Original));
      Converted := From_Object_Free (Wrapper);
      Assert_Cmpstr_Eq (To_String (Converted), "fr");
      --  The language handle is interned and borrowed; only the wrapper is
      --  freed by From_Object_Free, and the handle itself must not be freed.
   end Test_Wrapper_Conversion;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/pango-language/values-and-metadata",
      Test_Language_Values'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-language/wrapper-conversion",
      Test_Wrapper_Conversion'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
