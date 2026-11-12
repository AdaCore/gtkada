--  Exercise the public Glib.Variant constructors, type operations, accessors,
--  containers, parser, serialization, and reference-counting helpers.
--  The checkout has no focused g_variant_* C test to port, so these cases
--  check the documented GLib behavior directly through the Ada bindings.

with Ada.Command_Line;
with GNAT.Strings;
with System;

with Glib;          use Glib;
with Glib.Bytes;
with Glib.Error;
with Glib.String;
with Glib.Test;     use Glib.Test;
with Glib.Variant;  use Glib.Variant;

procedure Main is
   type Gvariant_Access is access all Gvariant;

   use type Glib.Bytes.Gbytes;
   use type Glib.Error.GError;
   use type Glib.GQuark;
   use type Glib.GType;
   use type Glib.Gint16;
   use type Glib.Gint32;
   use type Glib.Gint64;
   use type Glib.Guint8;
   use type Glib.Guint16;
   use type Glib.Guint32;
   use type Glib.Guint64;
   use type Glib.Gsize;
   use type Glib.Gdouble;
   use type GNAT.Strings.String_Access;
   use type Gvariant;
   use type Gvariant_Type;
   use type Gvariant_Iter;
   use type System.Address;

   Empty_List : constant GNAT.Strings.String_List (1 .. 0) := (others => null);

   procedure Scalar_Constructors with Convention => C;
   procedure String_Constructors with Convention => C;
   procedure String_Vector_And_Types with Convention => C;
   procedure Containers_And_Text with Convention => C;
   procedure Serialized_And_Refs with Convention => C;

   --  GNAT.Strings lists returned by Dup_* and the string-vector accessors
   --  own each element, so release every allocated string after checking it.
   procedure Free_List (Items : in out GNAT.Strings.String_List);

   --  This hook exists only to release the iterator allocated by Iter_New;
   --  GtkAda currently exposes iteration but not g_variant_iter_free.
   procedure Free_Iter (Iter : Gvariant_Iter);
   pragma Import (C, Free_Iter, "g_variant_iter_free");

   procedure Free_List (Items : in out GNAT.Strings.String_List) is
   begin
      for Item of Items loop
         if Item /= null then
            GNAT.Strings.Free (Item);
         end if;
      end loop;
   end Free_List;

   -------------------------
   -- Scalar_Constructors --
   -------------------------

   procedure Scalar_Constructors is
      V : Gvariant;
   begin
      --  Exercise procedure and function forms for the scalar constructors.
      G_New_Boolean (V, True);
      Assert_True (Get_Boolean (V));
      Unref (V);
      V := Gvariant_New_Boolean (False);
      Assert_False (Get_Boolean (V));
      Assert_True (Classify (V) = Class_Boolean);
      Unref (V);

      G_New_Byte (V, 201);
      Assert_True (Get_Byte (V) = 201);
      Unref (V);
      V := Gvariant_New_Byte (17);
      Assert_True (Get_Byte (V) = 17);
      Unref (V);

      G_New_Double (V, 3.5);
      Assert_True (Get_Double (V) = 3.5);
      Unref (V);
      V := Gvariant_New_Double (-2.25);
      Assert_True (Get_Double (V) = -2.25);
      Unref (V);

      G_New_Handle (V, 73);
      Assert_True (Get_Handle (V) = 73);
      Unref (V);
      V := Gvariant_New_Handle (-4);
      Assert_True (Get_Handle (V) = -4);
      Unref (V);

      G_New_Int16 (V, -1234);
      Assert_True (Get_Int16 (V) = -1234);
      Unref (V);
      V := Gvariant_New_Int16 (2345);
      Assert_True (Get_Int16 (V) = 2345);
      Unref (V);

      G_New_Int32 (V, -123456);
      Assert_True (Get_Int32 (V) = -123456);
      Unref (V);
      V := Gvariant_New_Int32 (234567);
      Assert_True (Get_Int32 (V) = 234567);
      Unref (V);

      G_New_Int64 (V, -123456789012);
      Assert_True (Get_Int64 (V) = -123456789012);
      Unref (V);
      V := Gvariant_New_Int64 (234567890123);
      Assert_True (Get_Int64 (V) = 234567890123);
      Unref (V);

      G_New_Uint16 (V, 54321);
      Assert_True (Get_Uint16 (V) = 54321);
      Unref (V);
      V := Gvariant_New_Uint16 (12345);
      Assert_True (Get_Uint16 (V) = 12345);
      Unref (V);

      G_New_Uint32 (V, 3456789012);
      Assert_True (Get_Uint32 (V) = 3456789012);
      Unref (V);
      V := Gvariant_New_Uint32 (1234567890);
      Assert_True (Get_Uint32 (V) = 1234567890);
      Unref (V);

      G_New_Uint64 (V, 1234567890123);
      Assert_True (Get_Uint64 (V) = 1234567890123);
      Unref (V);
      V := Gvariant_New_Uint64 (9876543210123);
      Assert_True (Get_Uint64 (V) = 9876543210123);
      Unref (V);
   end Scalar_Constructors;

   ----------------------
   -- String_Constructors --
   ----------------------

   procedure String_Constructors is
      V      : Gvariant;
      Length : aliased Gsize := 0;
      Bytes  : constant Guint8_Array := (1 => 65, 2 => 66, 3 => 0);
   begin
      --  String accessors report byte lengths and preserve the UTF-8 value.
      G_New_String (V, "hello");
      Assert_Cmpstr_Eq (Get_String (V, Length'Access), "hello");
      Assert_True (Length = 5);
      Assert_Cmpstr_Eq (Dup_String (V, Length'Access), "hello");
      Assert_Cmpstr_Eq (Print (V, False), "'hello'");
      Assert_True (Get_Size (V) > 0);
      Unref (V);

      V := Gvariant_New_String ("function");
      Assert_Cmpstr_Eq (Get_String (V), "function");
      Unref (V);
      --  Object-path and signature values are strings with distinct types.
      G_New_Object_Path (V, "/org/example/Widget");
      Assert_Cmpstr_Eq (Get_String (V), "/org/example/Widget");
      Assert_Cmpstr_Eq (Get_Type_String (V), "o");
      Unref (V);
      V := Gvariant_New_Object_Path ("/");
      Assert_Cmpstr_Eq (Get_String (V), "/");
      Unref (V);
      G_New_Signature (V, "a{sv}");
      Assert_Cmpstr_Eq (Get_String (V), "a{sv}");
      Assert_Cmpstr_Eq (Get_Type_String (V), "g");
      Unref (V);
      V := Gvariant_New_Signature ("s");
      Assert_Cmpstr_Eq (Get_String (V), "s");
      Unref (V);

      --  A byte string retains embedded bytes, with its final NUL included.
      G_New_Bytestring (V, Bytes);
      Assert_Cmpstr_Eq (Get_Type_String (V), "ay");
      Assert_True (Get_Size (V) = 3);
      Unref (V);
      V := Gvariant_New_Bytestring (Bytes);
      Assert_True (Get_Size (V) = 3);
      Unref (V);

      Assert_True (Is_Object_Path ("/org/example"));
      Assert_False (Is_Object_Path ("org/example"));
      Assert_True (Is_Signature ("a{sv}"));
      Assert_False (Is_Signature ("not-a-signature"));
   end String_Constructors;

   ---------------------------
   -- String_Vector_And_Types --
   ---------------------------

   procedure String_Vector_And_Types is
      Source : GNAT.Strings.String_List (1 .. 2) :=
        (1 => new String'("/one"), 2 => new String'("/two"));
      Paths  : GNAT.Strings.String_List (1 .. 2) :=
        (1 => new String'("/three"), 2 => new String'("/four"));
      Bytes  : GNAT.Strings.String_List (1 .. 2) :=
        (1 => new String'("alpha"), 2 => new String'("beta"));
      Byte_Source : GNAT.Strings.String_List (1 .. 2) :=
        (1 => new String'("red"), 2 => new String'("blue"));
      More_Words : GNAT.Strings.String_List (1 .. 2) :=
        (1 => new String'("left"), 2 => new String'("right"));
      V      : Gvariant;
      Count  : aliased Gsize := 0;
      T      : Gvariant_Type;
      A      : Gvariant_Type;
      Dict_Type : Gvariant_Type;
      Key_T  : Gvariant_Type;
      Val_T  : Gvariant_Type;
      Part    : Gvariant_Type;
   begin
      --  The constructors copy input lists, so the temporary Ada strings can
      --  be freed once each GVariant has been created.
      G_New_Objv (V, Source, 2);
      Free_List (Source);
      declare
         Items : GNAT.Strings.String_List := Dup_Objv (V, Count'Access);
      begin
         Assert_True (Count = 2);
         Assert_Cmpstr_Eq (Items (Items'Last).all, "/two");
         Free_List (Items);
      end;
      Unref (V);

      V := Gvariant_New_Objv (Paths, 2);
      Free_List (Paths);
      declare
         Items : GNAT.Strings.String_List := Dup_Objv (V);
      begin
         Assert_Cmpstr_Eq (Items (Items'First).all, "/three");
         Free_List (Items);
      end;
      Unref (V);

      G_New_Strv (V, Bytes, 2);
      Free_List (Bytes);
      declare
         Items : GNAT.Strings.String_List := Dup_Strv (V, Count'Access);
      begin
         Assert_True (Count = 2);
         Assert_Cmpstr_Eq (Items (Items'First).all, "alpha");
         Free_List (Items);
      end;
      Unref (V);

      V := Gvariant_New_Strv (More_Words, 2);
      Free_List (More_Words);
      declare
         Items : GNAT.Strings.String_List := Dup_Strv (V, Count'Access);
      begin
         Assert_True (Count = 2);
         Assert_Cmpstr_Eq (Items (Items'Last).all, "right");
         Free_List (Items);
      end;
      Unref (V);

      --  Byte-string arrays use the same string-list representation, with
      --  copies returned by both accessors.
      V := Gvariant_New_Bytestring_Array (Byte_Source, 2);
      Free_List (Byte_Source);
      declare
         Items : GNAT.Strings.String_List := Dup_Bytestring_Array (V, Count'Access);
      begin
         Assert_True (Count = 2);
         Assert_Cmpstr_Eq (Items (Items'Last).all, "blue");
         Free_List (Items);
      end;
      Unref (V);
      G_New_Bytestring_Array (V, Empty_List, 0);
      declare
         Items : GNAT.Strings.String_List := Dup_Bytestring_Array (V, Count'Access);
      begin
         Assert_True (Count = 0 and then Items'Length = 0);
         Free_List (Items);
      end;
      Unref (V);

      --  Type predicates and element traversal work on definite and
      --  indefinite type strings; owned type objects are explicitly freed.
      T := Gvariant_Type_New ("a{sv}");
      Assert_True (Is_Array (T) and then Is_Container (T));
      Assert_True (Is_Definite (T) and then not Is_Basic (T));
      Assert_Cmpstr_Eq (Dup_String (T), "a{sv}");
      Assert_True (Get_String_Length (T) = 5);
      A := Element (T);
      Assert_True (Is_Dict_Entry (A));
      Assert_True (N_Items (A) = 2);
      Key_T := Key (A);
      Val_T := Value (A);
      Assert_True (Is_Basic (Key_T));
      Assert_True (Is_Variant (Val_T));
      Free (T);

      T := Gvariant_Type_New ("(sib)");
      Assert_True (Is_Tuple (T) and then N_Items (T) = 3);
      Part := First (T);
      Assert_Cmpstr_Eq (Dup_String (Part), "s");
      Part := Next (Part);
      Assert_Cmpstr_Eq (Dup_String (Part), "i");
      Part := Next (Part);
      Assert_Cmpstr_Eq (Dup_String (Part), "b");
      Assert_True (Next (Part) = null);
      Free (T);

      T := Gvariant_Type_New ("a*");
      Assert_True (Is_Array (T) and then not Is_Definite (T));
      A := Gvariant_Type_New ("a*");
      Assert_True (Is_Subtype_Of (T, A));
      Free (T);
      Free (A);
      T := Gvariant_Type_New ("ms");
      Assert_True (Is_Maybe (T) and then Is_Container (T));
      Free (T);

      --  Both dictionary-entry type constructor forms report the same type.
      Key_T := Gvariant_Type_New ("s");
      Val_T := Gvariant_Type_New ("i");
      G_New_Dict_Entry (Dict_Type, Key_T, Val_T);
      Assert_Cmpstr_Eq (Dup_String (Dict_Type), "{si}");
      Free (Dict_Type);
      Dict_Type := Gvariant_Type_New_Dict_Entry (Key_T, Val_T);
      Assert_Cmpstr_Eq (Dup_String (Dict_Type), "{si}");
      Free (Dict_Type);
      Free (Key_T);
      Free (Val_T);

      --  Value constructors preserve the key/value types and child values.
      declare
         Key_Value  : Gvariant := Gvariant_New_String ("key");
         Int_Value  : Gvariant := Gvariant_New_Int32 (19);
         Pair       : Gvariant;
         Pair_Child : Gvariant;
      begin
         G_New_Dict_Entry (Pair, Key_Value, Int_Value);
         Assert_True (N_Children (Pair) = 2);
         Pair_Child := Get_Child_Value (Pair, 0);
         Assert_Cmpstr_Eq (Get_String (Pair_Child), "key");
         Unref (Pair_Child);
         Unref (Pair);

         Pair := Gvariant_New_Dict_Entry
           (Gvariant_New_String ("other"), Gvariant_New_Boolean (True));
         Assert_True (N_Children (Pair) = 2);
         Pair_Child := Get_Child_Value (Pair, 1);
         Assert_True (Get_Boolean (Pair_Child));
         Unref (Pair_Child);
         Unref (Pair);
      end;

      --  A value-derived type is borrowed and must not be freed separately.
      V := Gvariant_New_Int32 (42);
      T := Get_Type (V);
      Assert_Cmpstr_Eq (Get_Type_String (V), "i");
      Assert_True (Is_Of_Type (V, T));
      Assert_True (Is_Definite (T));
      Unref (V);

      G_New (T, "i");
      Assert_Cmpstr_Eq (Dup_String (T), "i");
      Free (T);
   end String_Vector_And_Types;

   -------------------------
   -- Parsed_And_Containers --
   -------------------------

   procedure Containers_And_Text is
      Child   : Gvariant;
      Wrapped : Gvariant;
      Inner   : Gvariant;
      Boxed   : Gvariant;
      Pair    : Gvariant;
      Key_Value : Gvariant;
      Int_Value : Gvariant;
      Maybe_Type : Gvariant_Type := Gvariant_Type_New ("ms");
      Maybe_Bytes : Glib.Bytes.Gbytes;
      Maybe_Value : Gvariant;
      Iter    : Gvariant_Iter;
      Rendered : Glib.String.Gstring;
   begin
      --  A variant boxes one value; child access and unboxing each return an
      --  owned reference to the contained value.
      Inner := Gvariant_New_Int32 (91);
      Inner := Ref_Sink (Inner);
      G_New_Variant (Wrapped, Inner);
      Assert_True (Is_Container (Wrapped));
      Assert_True (N_Children (Wrapped) = 1);
      Child := Get_Child_Value (Wrapped, 0);
      Assert_True (Is_Of_Type (Child, Gvariant_Type_Int32));
      Unref (Child);
      Child := Get_Variant (Wrapped);
      Assert_True (Get_Int32 (Child) = 91);
      Unref (Child);
      Unref (Wrapped);

      Boxed := Gvariant_New_Variant (Inner);
      Child := Get_Variant (Boxed);
      Assert_True (Get_Int32 (Child) = 91);
      Unref (Child);
      Unref (Boxed);
      Unref (Inner);

      --  The nothing value of a maybe serializes to zero bytes. Build it from
      --  GBytes so Get_Maybe is checked without the parser, whose current
      --  end-pointer binding cannot safely be used.
      declare
         Empty_Data   : constant Guint8_Array (1 .. 0) := (others => 0);
      begin
         Maybe_Bytes := Glib.Bytes.Gbytes_New (Empty_Data, 0);
         Maybe_Value := Gvariant_New_From_Bytes (Maybe_Type, Maybe_Bytes, False);
         Assert_True (Get_Maybe (Maybe_Value) = Null_Gvariant);
         Unref (Maybe_Value);
         Glib.Bytes.Unref (Maybe_Bytes);
      end;
      Free (Maybe_Type);

      --  A dictionary entry can be created directly, so this also exercises
      --  child extraction and iteration without relying on the parser.
      Key_Value := Gvariant_New_String ("key");
      Int_Value := Gvariant_New_Int32 (19);
      G_New_Dict_Entry (Pair, Key_Value, Int_Value);
      Assert_True (Is_Container (Pair));
      Assert_True (N_Children (Pair) = 2);
      Child := Get_Child_Value (Pair, 0);
      Assert_Cmpstr_Eq (Get_String (Child), "key");
      Unref (Child);
      Child := Get_Child_Value (Pair, 1);
      Assert_True (Get_Int32 (Child) = 19);
      Unref (Child);

      --  Iter_New allocates C iterator storage. Init resets that storage and
      --  Next_Value returns references that each need an Unref.
      Iter := Iter_New (Pair);
      Assert_True (Init (Iter, Pair) = 2);
      Child := Next_Value (Iter);
      Assert_Cmpstr_Eq (Get_String (Child), "key");
      Unref (Child);
      Child := Next_Value (Iter);
      Assert_True (Get_Int32 (Child) = 19);
      Unref (Child);
      Child := Next_Value (Iter);
      Assert_True (Child = Null_Gvariant);
      Free_Iter (Iter);

      --  Error quarks are stable identifiers and can be queried without
      --  manufacturing a parser error.
      Assert_True (Parse_Error_Quark /= 0);
      Assert_True (Parser_Get_Error_Quark /= 0);

      --  Printing returns the GVariant text form; Print_String appends it to
      --  the caller's GString, which is freed after its contents are checked.
      Assert_True (Print (Pair, False)'Length > 0);
      Assert_True (Print (Pair, True)'Length > 0);
      Rendered := Glib.String.Gstring_New ("prefix:");
      Rendered := Print_String (Pair, Rendered, False);
      declare
         Text : constant UTF8_String := Glib.String.Free (Rendered, False);
      begin
         Assert_True (Text'Length > 7);
      end;

      Unref (Pair);
   end Containers_And_Text;

   --------------------------
   -- Serialized_And_Refs --
   --------------------------

   procedure Serialized_And_Refs is
      V       : Gvariant;
      Copy    : Gvariant;
      Normal  : Gvariant;
      Swapped : Gvariant;
      Bytes   : Glib.Bytes.Gbytes;
      Data_Bytes : Glib.Bytes.Gbytes;
      Round   : Gvariant;
      Buffer  : aliased Guint8_Array (1 .. 4);
      Type_I  : constant Gvariant_Type := Gvariant_Type_Int32;
      Address_Copy : Gvariant;
   begin
      V := Gvariant_New_Int32 (1234567);
      Assert_True (Is_Floating (V));
      Assert_True (Check_Format_String (V, "i", True));
      Assert_True (Is_Container (V) = False);
      Assert_True (Is_Normal_Form (V));

      --  From_Object wraps the same underlying value without taking a new
      --  reference; only the original V is released below.
      Address_Copy := From_Object (V.Get_Object);
      Assert_True (Get_Int32 (Address_Copy) = 1234567);
      Assert_True (V.Get_Object /= System.Null_Address);
      Assert_True (Hash (V.Get_Object) > 0);

      --  From_Object_Free consumes the Ada wrapper allocation but leaves the
      --  wrapped C GVariant reference owned by the returned value.
      declare
         Heap_Value : Gvariant_Access :=
           new Gvariant'(Gvariant_New_Int32 (314));
         Converted : constant Gvariant := From_Object_Free (Heap_Value);
      begin
         Assert_True (Get_Int32 (Converted) = 314);
         Unref (Converted);
      end;

      Normal := Take_Ref (Get_Normal_Form (V));
      Assert_True (not Is_Floating (Normal));
      Assert_True (Get_Int32 (Normal) = 1234567);
      Unref (Normal);

      Swapped := Byteswap (V);
      Copy := Byteswap (Swapped);
      Assert_True (Get_Int32 (Copy) = 1234567);
      Unref (Copy);
      Unref (Swapped);

      --  Store serializes into caller-owned storage. Recreating a variant
      --  from GBytes checks both constructor overloads and their type handling.
      Assert_True (Get_Size (V) = Buffer'Length);
      Store (V, Buffer'Address);
      Bytes := Glib.Bytes.Gbytes_New (Buffer, Buffer'Length);
      Data_Bytes := Get_Data_As_Bytes (V);
      Assert_True (Glib.Bytes.Get_Size (Data_Bytes) = Buffer'Length);
      Glib.Bytes.Unref (Data_Bytes);
      Round := Gvariant_New_From_Bytes (Type_I, Bytes, True);
      Assert_True (Get_Int32 (Round) = 1234567);
      Unref (Round);
      G_New_From_Bytes (Round, Type_I, Bytes, True);
      Assert_True (Get_Int32 (Round) = 1234567);
      Unref (Round);
      Glib.Bytes.Unref (Bytes);

      --  Reference helpers preserve values while changing floating ownership.
      Copy := Ref_Sink (V);
      Assert_True (not Is_Floating (Copy));
      Unref (Copy);
      V := Gvariant_New_Int32 (88);
      Copy := Ref_Sink (V);
      Normal := Ref (Copy);
      Assert_True (Get_Int32 (Normal) = 88);
      Unref (Normal);
      Unref (Copy);

      --  Type-string helpers validate exact type expressions without using
      --  the parser's unsupported end-pointer binding.
      Assert_True (String_Is_Valid ("a{sv}"));
      Assert_False (String_Is_Valid ("invalid"));
      Assert_True (String_Get_Depth ("a{sv}") > 0);

   end Serialized_And_Refs;

begin
   Glib.Test.Init;
   Add_Func ("/glib-variant/scalars", Scalar_Constructors'Unrestricted_Access);
   Add_Func ("/glib-variant/strings", String_Constructors'Unrestricted_Access);
   Add_Func
     ("/glib-variant/string-vectors-and-types",
      String_Vector_And_Types'Unrestricted_Access);
   Add_Func
     ("/glib-variant/containers-and-text",
      Containers_And_Text'Unrestricted_Access);
   Add_Func
     ("/glib-variant/serialization-and-refs",
      Serialized_And_Refs'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
