--  Exercise Gtk.Css_Section constructors, location accessors, parent and
--  source accessors, reference counting, and both textual representations.

with Ada.Command_Line;

with Glib;        use Glib;
with Glib.Bytes;  use Glib.Bytes;
with Glib.GFile;  use Glib.GFile;
with Glib.Object;
with Glib.String; use Glib.String;
with Glib.Test;   use Glib.Test;
with Glib.Types;

with Gtk.Css_Section; use Gtk.Css_Section;
with Gtk.Main;

with Gtkada.Types;
with System;
with System.Address_To_Access_Conversions;

procedure Main is
   use type Glib.GType;
   use type System.Address;

   subtype Css_File is Glib.GFile.Gfile;

   --  These internal conversions expect a GLib-allocated wrapper pointer,
   --  which they copy and release with g_free.
   function G_Malloc (Size : Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   package Section_Conversion is new
     System.Address_To_Access_Conversions (Gtk_Css_Section);
   package Location_Conversion is new
     System.Address_To_Access_Conversions (Gtk_Css_Location);

   Css_Text : constant UTF8_String := "button { color: blue; }";

   --  The two constructors and both location queries should preserve all
   --  five counters, including the byte and character offsets within a line.
   Start : constant Gtk_Css_Location :=
     (Bytes => 2, Chars => 2, Lines => 1, Line_Bytes => 2, Line_Chars => 2);
   Finish : constant Gtk_Css_Location :=
     (Bytes => 18, Chars => 18, Lines => 1, Line_Bytes => 18, Line_Chars => 18);

   procedure Assert_Location
     (Actual   : Gtk_Css_Location;
      Expected : Gtk_Css_Location);
   function To_Bytes (Text : String) return Guint8_Array;
   procedure Release (File : Css_File);
   procedure Test_Sections with Convention => C;

   --  Compare every field so a partially copied C record is detected.
   procedure Assert_Location
     (Actual   : Gtk_Css_Location;
      Expected : Gtk_Css_Location) is
   begin
      Assert_Cmpuint_Eq (Guint (Actual.Bytes), Guint (Expected.Bytes));
      Assert_Cmpuint_Eq (Guint (Actual.Chars), Guint (Expected.Chars));
      Assert_Cmpuint_Eq (Guint (Actual.Lines), Guint (Expected.Lines));
      Assert_Cmpuint_Eq
        (Guint (Actual.Line_Bytes), Guint (Expected.Line_Bytes));
      Assert_Cmpuint_Eq
        (Guint (Actual.Line_Chars), Guint (Expected.Line_Chars));
   end Assert_Location;

   --  GBytes receives a byte-for-byte copy of this Ada string.
   function To_Bytes (Text : String) return Guint8_Array is
      Result : Guint8_Array (1 .. Text'Length);
      Index  : Positive := Result'First;
   begin
      for Character of Text loop
         Result (Index) := Guint8 (Standard.Character'Pos (Character));
         Index := Index + 1;
      end loop;
      return Result;
   end To_Bytes;

   --  GFile is an interface here, so release its underlying GObject reference.
   procedure Release (File : Css_File) is
   begin
      if File /= Null_Gfile then
         Glib.Object.Unref
           (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
      end if;
   end Release;

   --  Verify the GtkCssSection API with real immutable source bytes and a
   --  stable file identifier; no disk file or display content is required.
   procedure Test_Sections is
      File  : constant Css_File := New_For_Path ("css-section.css");
      Data  : constant Guint8_Array := To_Bytes (Css_Text);
      Bytes : constant Gbytes := Gbytes_New (Data, Gsize (Data'Length));
      By_Function : Gtk_Css_Section :=
        Gtk_Css_Section_New (File, Start, Finish);
      By_Procedure : Gtk_Css_Section;
      From_Bytes : Gtk_Css_Section :=
        Gtk_Css_Section_New_With_Bytes (File, Bytes, Start, Finish);
      Bytes_By_Procedure : Gtk_Css_Section;
      Parent : Gtk_Css_Section;
      Held : Gtk_Css_Section;
      Printed : Gstring := Gstring_New;
   begin
      Assert_True (Gtk.Css_Section.Get_Type /= 0);

      --  Both constructor styles return the requested locations and file.
      Gtk_New (By_Procedure, File, Start, Finish);
      Assert_Location (By_Function.Get_Start_Location, Start);
      Assert_Location (By_Function.Get_End_Location, Finish);
      Assert_Location (By_Procedure.Get_Start_Location, Start);
      Assert_Location (By_Procedure.Get_End_Location, Finish);
      Assert_True (By_Function.Get_File /= Null_Gfile);
      Assert_True (By_Procedure.Get_File /= Null_Gfile);

      --  A directly constructed document section has no parent or byte source.
      Parent := By_Function.Get_Parent;
      Assert_True (Parent.Get_Object = System.Null_Address);
      Assert_True
        (By_Function.Get_Bytes.Get_Object = System.Null_Address);

      --  The byte constructors retain the supplied GBytes as their source.
      Gtk_New_With_Bytes
        (Bytes_By_Procedure, File, Bytes, Start, Finish);
      Assert_Location (From_Bytes.Get_Start_Location, Start);
      Assert_Location (From_Bytes.Get_End_Location, Finish);
      Assert_True
        (From_Bytes.Get_Bytes.Get_Object = Bytes.Get_Object);
      Assert_True
        (Bytes_By_Procedure.Get_Bytes.Get_Object = Bytes.Get_Object);
      Assert_True (From_Bytes.Get_File /= Null_Gfile);

      --  Print appends to GString, while To_String returns the same form.
      Print (From_Bytes, Printed);
      declare
         Printed_Text : constant UTF8_String := Gtkada.Types.Value (Printed.Str);
         Rendered     : constant UTF8_String := From_Bytes.To_String;
      begin
         Assert_Cmpstr_Eq (Printed_Text, Rendered);
         Assert_True (Rendered'Length > 0);
      end;

      --  The generated From_Object_Free helpers copy their values and free
      --  GLib-allocated wrapper memory; model that ownership contract exactly.
      declare
         Section_Address : constant System.Address :=
           G_Malloc (Gsize (Gtk_Css_Section'Size / System.Storage_Unit));
         Section_Pointer : constant Section_Conversion.Object_Pointer :=
           Section_Conversion.To_Pointer (Section_Address);
         Converted_Section : Gtk_Css_Section;
         Location_Address : constant System.Address :=
           G_Malloc (Gsize (Gtk_Css_Location'Size / System.Storage_Unit));
         Location_Pointer : constant Location_Conversion.Object_Pointer :=
           Location_Conversion.To_Pointer (Location_Address);
         Converted_Location : Gtk_Css_Location;
      begin
         Section_Pointer.all := By_Function;
         Converted_Section :=
           Gtk.Css_Section.From_Object_Free (Section_Pointer);
         Assert_True
           (Converted_Section.Get_Object = By_Function.Get_Object);

         Location_Pointer.all := Start;
         Converted_Location :=
           Gtk.Css_Section.From_Object_Free (Location_Pointer);
         Assert_Location (Converted_Location, Start);
      end;

      --  Ref creates an additional owned reference; two Unrefs balance it.
      Held := Ref (From_Bytes);
      Assert_True (Held.Get_Object = From_Bytes.Get_Object);
      Unref (Held);

      declare
         Ignored : constant UTF8_String := Free (Printed, True);
         pragma Unreferenced (Ignored);
      begin
         null;
      end;
      Unref (Bytes_By_Procedure);
      Unref (From_Bytes);
      Unref (By_Procedure);
      Unref (By_Function);
      Unref (Bytes);
      Release (File);
   end Test_Sections;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/css-section/constructors-and-accessors",
      Test_Sections'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
