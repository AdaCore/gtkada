--  Exercises the Pango attribute constructors, list operations, callback
--  filtering, and GtkAda's proxy conversion helpers. The filter case ports
--  the relevant behavior of test_pango_attributes in testsuite/c_tests/builder.c.

with Ada.Command_Line;
with System;

with Glib.Test; use Glib.Test;
with Pango.Attributes; use Pango.Attributes;

procedure Main is
   use type System.Address;
   use type Pango_Attr_List;
   use type Pango_Attribute;

   --  These access types let the tests exercise the GtkAda helpers that
   --  consume heap-allocated Ada wrappers returned by C conversions.
   type Attribute_Access is access all Pango_Attribute;
   type Attr_List_Access is access all Pango_Attr_List;
   type Attr_Iterator_Access is access all Pango_Attr_Iterator;

   function Keep_Attribute (Attribute : Pango_Attribute) return Boolean;

   procedure Test_Attribute_Values
   with Convention => C;

   procedure Test_List_Operations
   with Convention => C;

   procedure Test_Conversions
   with Convention => C;

   ---------------------
   -- Keep_Attribute --
   ---------------------

   --  The callback selects each valid attribute, as the C test's callback
   --  does, while the assertions check that Pango returned a filtered list.
   function Keep_Attribute (Attribute : Pango_Attribute) return Boolean is
   begin
      return Attribute /= null;
   end Keep_Attribute;

   --------------------------
   -- Test_Attribute_Values --
   --------------------------

   --  Attribute equality compares values, and Copy creates an independently
   --  destroyable attribute with the same value.
   procedure Test_Attribute_Values is
      Family       : constant Pango_Attribute := Attr_Family_New ("Noto Sans");
      Same_Family  : constant Pango_Attribute := Attr_Family_New ("Noto Sans");
      Other_Family : constant Pango_Attribute := Attr_Family_New ("Serif");
      Family_Copy  : constant Pango_Attribute := Copy (Family);
      Strike_On    : constant Pango_Attribute := Attr_Strikethrough_New (True);
      Strike_Copy  : constant Pango_Attribute := Attr_Strikethrough_New (True);
      Strike_Off   : constant Pango_Attribute := Attr_Strikethrough_New (False);
   begin
      Assert_True (Equal (Family, Same_Family));
      Assert_False (Equal (Family, Other_Family));
      Assert_True (Equal (Family, Family_Copy));
      Assert_True (Equal (Strike_On, Strike_Copy));
      Assert_False (Equal (Strike_On, Strike_Off));

      Destroy (Family);
      Destroy (Same_Family);
      Destroy (Other_Family);
      Destroy (Family_Copy);
      Destroy (Strike_On);
      Destroy (Strike_Copy);
      Destroy (Strike_Off);
   end Test_Attribute_Values;

   --------------------------
   -- Test_List_Operations --
   --------------------------

   --  Exercise constructors, insertion/replacement, filtering, copying,
   --  references, splicing, index updates, and iterator lifetime.
   procedure Test_List_Operations is
      Output_List : Pango_Attr_List;
      List        : Pango_Attr_List;
      Other       : Pango_Attr_List;
      Copy_List   : Pango_Attr_List;
      Shared_List : Pango_Attr_List;
      Filtered    : Pango_Attr_List;
      Iterator    : Pango_Attr_Iterator;
   begin
      --  Both constructor forms produce a non-null list that the caller owns.
      Gdk_New (Output_List);
      Assert_True (Output_List /= Null_Pango_Attr_List);
      Unref (Output_List);

      List := Pango_Attr_List_New;
      Assert_True (List /= Null_Pango_Attr_List);

      --  Insert_Before and Insert add owned attributes; Change replaces the
      --  existing family attribute with a new value of the same type.
      Insert (List, Attr_Family_New ("Noto Sans"));
      Insert_Before (List, Attr_Strikethrough_New (True));
      Change (List, Attr_Family_New ("Serif"));

      --  This callback filter is the core of the C builder test port: Pango
      --  invokes it for the list's attributes and returns the selected list.
      Filtered := Filter (List, Keep_Attribute'Unrestricted_Access);
      Assert_True (Filtered /= Null_Pango_Attr_List);
      Unref (Filtered);

      --  Copy owns a separate list. Ref returns the same list with one extra
      --  reference, so both references must be released below.
      Copy_List := Copy (List);
      Assert_True (Copy_List /= Null_Pango_Attr_List);
      Assert_True (Copy_List /= List);
      Shared_List := Ref (List);
      Assert_True (Shared_List = List);

      --  Get_Iterator returns a separately allocated iterator that must be
      --  destroyed before the list can be modified or released.
      Iterator := Get_Iterator (Copy_List);
      Assert_True (Iterator /= null);
      Destroy (Iterator);

      --  Splice inserts attributes from another list, and Update adjusts the
      --  byte ranges after an edit to the text.
      Other := Pango_Attr_List_New;
      Insert (Other, Attr_Family_New ("Monospace"));
      Splice (List, Other, Pos => 2, Len => 4);
      Update (List, Pos => 1, Remove => 1, Add => 2);

      Unref (Other);
      Unref (Copy_List);
      Unref (Shared_List);
      Unref (List);
   end Test_List_Operations;

   ----------------------
   -- Test_Conversions --
   ----------------------

   --  Verify public address conversions and consume heap wrappers through
   --  From_Object_Free, releasing each underlying C value exactly once.
   procedure Test_Conversions is
      Original       : constant Pango_Attribute := Attr_Family_New ("Serif");
      Round_Trip     : constant Pango_Attribute := Convert (Convert (Original));
      Heap_Attribute : Attribute_Access;
      Converted_Attr : Pango_Attribute;
      Expected_Attr  : constant Pango_Attribute :=
        Attr_Family_New ("Monospace");
      Heap_List      : Attr_List_Access;
      Converted_List : Pango_Attr_List;
      List           : Pango_Attr_List;
      Heap_Iterator  : Attr_Iterator_Access;
      Converted_Iter : Pango_Attr_Iterator;
   begin
      Assert_True (Equal (Original, Round_Trip));
      Destroy (Original);

      --  From_Object_Free frees only the Ada wrapper allocation; the returned
      --  Pango attribute still owns its C allocation and is destroyed below.
      Heap_Attribute := new Pango_Attribute'(Attr_Family_New ("Monospace"));
      Converted_Attr := From_Object_Free (Heap_Attribute);
      Assert_True (Equal (Converted_Attr, Expected_Attr));
      Destroy (Converted_Attr);
      Destroy (Expected_Attr);

      --  The boxed list helper follows the same ownership convention and
      --  returns the list reference, which is then unreferenced once.
      Heap_List := new Pango_Attr_List'(Pango_Attr_List_New);
      Converted_List := From_Object_Free (Heap_List);
      Assert_True (Converted_List /= Null_Pango_Attr_List);
      Unref (Converted_List);

      --  Iterator conversion consumes its wrapper allocation; Destroy releases
      --  the native iterator returned by the list.
      List := Pango_Attr_List_New;
      Heap_Iterator := new Pango_Attr_Iterator'(Get_Iterator (List));
      Converted_Iter := From_Object_Free (Heap_Iterator);
      Assert_True (Converted_Iter /= null);
      Destroy (Converted_Iter);
      Unref (List);
   end Test_Conversions;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/pango-attributes/attribute-values",
      Test_Attribute_Values'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-attributes/list-operations",
      Test_List_Operations'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-attributes/conversions",
      Test_Conversions'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
