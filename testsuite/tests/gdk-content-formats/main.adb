--  Exercise constructors, queries, set operations, conversions and string
--  round-tripping for Gdk.Content_Formats. No display or C test is needed.

with Ada.Command_Line;

with GNAT.Strings;
with Glib;                 use Glib;
with Glib.String;          use Glib.String;
with Glib.Test;            use Glib.Test;
with Gdk.Content_Formats;  use Gdk.Content_Formats;
with Gtkada.Types;
with System;

procedure Main is
   use type Glib.GType;
   use type System.Address;

   Mime_A : constant String := "application/x-gtkada-a";
   Mime_B : constant String := "application/x-gtkada-b";

   procedure Free_List (Items : in out GNAT.Strings.String_List);
   procedure Test_Mime_Formats with Convention => C;
   procedure Test_Gtype_Formats with Convention => C;

   --  GNAT.Strings.String_List owns each allocated element returned by the
   --  binding, so release elements after checking the list.
   procedure Free_List (Items : in out GNAT.Strings.String_List) is
   begin
      for Index in Items'Range loop
         GNAT.Strings.Free (Items (Index));
      end loop;
   end Free_List;

   --  MIME-format checks cover both constructor styles, list access, matching,
   --  union, reference counting, printing and parsing.
   procedure Test_Mime_Formats is
      Input : GNAT.Strings.String_List (1 .. 2) :=
        (1 => new String'(Mime_A), 2 => new String'(Mime_B));
      Formats : Gdk_Content_Formats :=
        Gdk_Content_Formats_New (Input, Guint (Input'Length));
      Count : aliased Gsize := 0;
      Got : GNAT.Strings.String_List :=
        Formats.Get_Mime_Types (Count'Access);
      Without_Count : GNAT.Strings.String_List := Formats.Get_Mime_Types;
      Other_Input : GNAT.Strings.String_List (1 .. 2) :=
        (1 => new String'(Mime_B),
         2 => new String'("application/x-gtkada-c"));
      Other : Gdk_Content_Formats :=
        Gdk_Content_Formats_New (Other_Input, Guint (Other_Input'Length));
      --  Union consumes its first argument, so pass an extra reference and
      --  keep Formats available for the rest of this test.
      Combined : Gdk_Content_Formats := Union (Ref (Formats), Other);
      Empty_Input : constant GNAT.Strings.String_List (1 .. 0) :=
        (others => null);
      Empty : Gdk_Content_Formats := Gdk_Content_Formats_New (Empty_Input, 0);
      By_Procedure : Gdk_Content_Formats;
      Held : Gdk_Content_Formats;
      Printed : Gstring := Gstring_New;
      Parsed : Gdk_Content_Formats;
   begin
      Assert_False (Formats.Is_Empty);
      declare
         Present : constant Boolean := Formats.Contain_Mime_Type (Mime_A);
      begin
         Assert_True (Present);
      end;
      Assert_True (Formats.Contain_Mime_Type (Mime_B));
      Assert_False (Formats.Contain_Mime_Type ("application/x-missing"));

      --  The optional count and omitted-count forms return the same MIME set.
      Assert_Cmpuint_Eq (Guint (Count), 2);
      Assert_Cmpstr_Eq (Got (Got'First).all, Mime_A);
      Assert_Cmpstr_Eq (Got (Got'First + 1).all, Mime_B);
      Assert_Cmpuint_Eq (Guint (Without_Count'Length), 2);
      Free_List (Got);
      Free_List (Without_Count);

      --  The out-parameter constructor must produce the same advertised type.
      declare
         One_Mime : GNAT.Strings.String_List (1 .. 1) :=
           (1 => new String'(Mime_A));
      begin
         Gdk_New (By_Procedure, One_Mime, 1);
         Free_List (One_Mime);
      end;
      Assert_True (By_Procedure.Contain_Mime_Type (Mime_A));

      --  The first common MIME type follows the order of the first set.
      Assert_True (Match (Formats, Other));
      Assert_False (Match (Formats, Empty));
      Assert_Cmpstr_Eq (Match_Mime_Type (Formats, Other), Mime_B);
      Assert_Cmpstr_Eq (Match_Mime_Type (Formats, Empty), "");
      Assert_True (Combined.Contain_Mime_Type (Mime_A));
      Assert_True (Combined.Contain_Mime_Type (Mime_B));
      Assert_True
        (Combined.Contain_Mime_Type ("application/x-gtkada-c"));
      Assert_True (Empty.Is_Empty);

      --  Ref returns another owned reference to the same immutable boxed value.
      Held := Ref (Formats);
      Assert_True (Held.Get_Object = Formats.Get_Object);
      Unref (Held);

      declare
         Rendered : constant UTF8_String := To_String (Formats);
      begin
         Assert_True (Rendered'Length > 0);
         Print (Formats, Printed);
         Assert_Cmpstr_Eq (Gtkada.Types.Value (Printed.Str), Rendered);
         Parsed := Parse (Rendered);
         Assert_True (Match (Parsed, Formats));
      end;

      --  The serializer/deserializer helpers preserve the original format,
      --  regardless of which conversions are registered by this GTK build.
      declare
         A : Gdk_Content_Formats := Union_Deserialize_Gtypes (Ref (Formats));
         B : Gdk_Content_Formats := Union_Deserialize_Mime_Types (Ref (Formats));
         C : Gdk_Content_Formats := Union_Serialize_Gtypes (Ref (Formats));
         D : Gdk_Content_Formats := Union_Serialize_Mime_Types (Ref (Formats));
      begin
         Assert_True (A.Contain_Mime_Type (Mime_A));
         Assert_True (B.Contain_Mime_Type (Mime_A));
         Assert_True (C.Contain_Mime_Type (Mime_A));
         Assert_True (D.Contain_Mime_Type (Mime_A));
         Unref (A);
         Unref (B);
         Unref (C);
         Unref (D);
      end;

      declare
         Ignored : constant UTF8_String := Free (Printed, True);
         pragma Unreferenced (Ignored);
      begin
         null;
      end;
      Unref (Parsed);
      Unref (Combined);
      Unref (Empty);
      Unref (By_Procedure);
      Unref (Other);
      Unref (Formats);
      --  Constructor inputs remain GNAT-owned; the C constructor copies them.
      Free_List (Input);
      Free_List (Other_Input);
   end Test_Mime_Formats;

   --  GType checks exercise both constructor styles, membership, intersection
   --  selection and the conversion helpers on in-process formats.
   procedure Test_Gtype_Formats is
      Formats : Gdk_Content_Formats :=
        Gdk_Content_Formats_New_For_Gtype (GType_String);
      Procedure_Built : Gdk_Content_Formats;
      Other : Gdk_Content_Formats :=
        Gdk_Content_Formats_New_For_Gtype (GType_Int);
      Mixed : Gdk_Content_Formats := Union (Ref (Formats), Other);
      Serializer_Mime : GNAT.Strings.String_List (1 .. 1) :=
        (1 => new String'("text/plain"));
      Mime_Formats : Gdk_Content_Formats :=
        Gdk_Content_Formats_New (Serializer_Mime, 1);
      Deserialized : Gdk_Content_Formats :=
        Union_Deserialize_Gtypes (Ref (Mime_Formats));
      Serialized : Gdk_Content_Formats :=
        Union_Serialize_Mime_Types (Ref (Formats));
   begin
      Free_List (Serializer_Mime);
      Assert_True (Gdk.Content_Formats.Get_Type /= GType_Invalid);
      Assert_False (Formats.Is_Empty);
      Assert_True (Formats.Contain_Gtype (GType_String));
      Assert_False (Formats.Contain_Gtype (GType_Int));

      Gdk_New_For_Gtype (Procedure_Built, GType_Int);
      Assert_True (Procedure_Built.Contain_Gtype (GType_Int));

      --  The match methods return the first common type or the invalid type.
      Assert_Cmpuint_Eq (Guint (Match_Gtype (Formats, Mixed)),
                         Guint (GType_String));
      Assert_Cmpuint_Eq (Guint (Match_Gtype (Formats, Other)), 0);
      Assert_True (Match (Formats, Mixed));
      Assert_False (Match (Formats, Other));
      Assert_True (Mixed.Contain_Gtype (GType_String));
      Assert_True (Mixed.Contain_Gtype (GType_Int));

      --  A registered text serializer can add the string GType; the input MIME
      --  and GType remain present even when a build registers no conversion.
      Assert_True (Mime_Formats.Contain_Mime_Type ("text/plain"));
      Assert_True (Deserialized.Contain_Mime_Type ("text/plain"));
      Assert_True (Serialized.Contain_Gtype (GType_String));

      --  Each transformation consumes its extra reference; returned formats
      --  and the original values below are released independently.
      Unref (Serialized);
      Unref (Deserialized);
      Unref (Mime_Formats);
      Unref (Mixed);
      Unref (Other);
      Unref (Procedure_Built);
      Unref (Formats);
   end Test_Gtype_Formats;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gdk/content-formats/mime", Test_Mime_Formats'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk/content-formats/gtype", Test_Gtype_Formats'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
