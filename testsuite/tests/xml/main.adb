--  Glib.XML tests for buffer and file parsing, tree operations, attributes,
--  escaping, copying, equality, freeing, and printing. There is no C test
--  counterpart for this GtkAda-only XML package.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Glib;
with Glib.Test; use Glib.Test;
with Glib.XML;

procedure Main is
   package XML is new Glib.XML (Integer);
   use type XML.Node_Ptr;
   use type Glib.String_Ptr;

   use Ada.Strings.Unbounded;

   Freed_Data : Natural := 0;
   Roundtrip_File : constant String := "xml-roundtrip-test.xml";
   Invalid_File   : constant String := "missing-xml-dir/output.xml";

   procedure Test_Parse_And_Search
   with Convention => C;
   procedure Test_Tree_Operations
   with Convention => C;
   procedure Test_Attributes_And_Protection
   with Convention => C;
   procedure Test_Copy_Equality_And_Free
   with Convention => C;
   procedure Test_Print_And_File_Parse
   with Convention => C;
   procedure Test_Encoding_And_Parse_Errors
   with Convention => C;

   function New_Node (Tag : String; Data : Integer := 0)
      return XML.Node_Ptr;
   procedure Free_Data (Data : in out Integer);
   function Read_Text (Name : String) return String;

   --------------
   -- New_Node --
   --------------

   function New_Node (Tag : String; Data : Integer := 0)
      return XML.Node_Ptr
   is
   begin
      return new XML.Node'
        (Tag           => new String'(Tag),
         Attributes    => null,
         Value         => null,
         Parent        => null,
         Child         => null,
         Next          => null,
         Specific_Data => Data);
   end New_Node;

   ---------------
   -- Free_Data --
   ---------------

   procedure Free_Data (Data : in out Integer) is
   begin
      --  Track that the optional callback runs once for each freed node.
      Data := Data + 1;
      Freed_Data := Freed_Data + 1;
   end Free_Data;

   ---------------
   -- Read_Text --
   ---------------

   function Read_Text (Name : String) return String is
      File   : Ada.Text_IO.File_Type;
      Result : Unbounded_String;
   begin
      --  Print writes a line at a time; restore the line endings so the
      --  resulting string can be checked for the XML entity spellings.
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Name);
      while not Ada.Text_IO.End_Of_File (File) loop
         Append (Result, Ada.Text_IO.Get_Line (File));
         Append (Result, ASCII.LF);
      end loop;
      Ada.Text_IO.Close (File);
      return To_String (Result);
   end Read_Text;

   --------------------------
   -- Test_Parse_And_Search --
   --------------------------

   procedure Test_Parse_And_Search is
      Source : constant String :=
        "<?xml version=""1.0""?> " & ASCII.LF & ASCII.HT & ASCII.CR
        & "<!--comment--><!DOCTYPE root>"
        & "<root id=""a&amp;b"" quoted='x&quot;y' plain=word>"
        & "<text>one &lt; two &amp; three &gt; four &quot;quoted&quot;"
        & " &apos;single&apos;</text><item key='first'/>"
        & "<field>value</field><explicit></explicit><empty />"
        & "<group><leaf>deep</leaf></group>"
        & "</root>";
      Root : XML.Node_Ptr;
      Second : XML.Node_Ptr;
      Item : XML.Node_Ptr;
      Group : XML.Node_Ptr;
   begin
      --  Exercise comments and a DOCTYPE, mixed attribute quoting, entity
      --  decoding, text values, nested children and both empty-tag spellings.
      Root := XML.Parse_Buffer (Source);
      Assert_True (Root /= null);
      Assert_Cmpstr_Eq (Root.Tag.all, "root");
      Assert_Cmpstr_Eq (XML.Get_Attribute (Root, "id"), "a&b");
      Assert_Cmpstr_Eq (XML.Get_Attribute (Root, "quoted"), "x""y");
      Assert_Cmpstr_Eq (XML.Get_Attribute (Root, "plain"), "word");
      Assert_Cmpstr_Eq
        (XML.Get_Attribute (Root, "absent", "fallback"), "fallback");
      Assert_Cmpstr_Eq
        (XML.Get_Attribute (null, "absent", "fallback"), "fallback");
      Assert_Cmpuint_Eq (Glib.Guint (XML.Children_Count (Root)), 6);

      Assert_Cmpstr_Eq
        (XML.Get_Field (Root, "text").all,
         "one < two & three > four ""quoted"" 'single'");
      Assert_True (XML.Get_Field (Root, "absent") = null);

      Item := XML.Find_Tag (Root.Child, "item");
      Assert_True (Item /= null);
      Assert_Cmpstr_Eq (XML.Get_Attribute (Item, "key"), "first");
      Assert_True (XML.Find_Tag (Item.Next, "group") /= null);
      Assert_True (XML.Find_Tag (null, "root") = null);

      Assert_True
        (XML.Find_Tag_With_Attribute (Root.Child, "item", "key") = Item);
      Assert_True
        (XML.Find_Tag_With_Attribute
           (Root.Child, "item", "key", "first") = Item);
      Assert_True
        (XML.Find_Tag_With_Attribute
           (Root.Child, "item", "key", "wrong") = null);
      Assert_True
        (XML.Find_Tag_With_Attribute
           (Root.Child, "item", "missing") = null);

      Group := XML.Find_Tag (Root.Child, "group");
      Assert_True (Group /= null and then Group.Child /= null);
      Assert_Cmpstr_Eq (Group.Child.Parent.Tag.all, "group");
      Assert_Cmpstr_Eq (Group.Child.Value.all, "deep");
      Assert_Cmpstr_Eq (XML.Find_Tag (Root.Child, "explicit").Value.all, "");
      Second := XML.Parse_Buffer (Source);
      Assert_True (XML.Is_Equal (Root, Second));
      XML.Free (Second);

      XML.Free (Root);
      Assert_True (Root = null);
   end Test_Parse_And_Search;

   --------------------------
   -- Test_Tree_Operations --
   --------------------------

   procedure Test_Tree_Operations is
      Root : XML.Node_Ptr := New_Node ("root");
      First : XML.Node_Ptr := New_Node ("first");
      Middle : XML.Node_Ptr := New_Node ("middle");
      Last : XML.Node_Ptr := New_Node ("last");
      Front : XML.Node_Ptr := New_Node ("front");
      Empty_Root : XML.Node_Ptr := New_Node ("empty");
   begin
      --  Prepending and appending both maintain the parent and sibling links.
      Assert_Cmpuint_Eq (Glib.Guint (XML.Children_Count (null)), 0);
      Assert_Cmpuint_Eq (Glib.Guint (XML.Children_Count (Root)), 0);
      XML.Add_Child (Root, First);
      XML.Add_Child (Root, Middle, Append => True);
      XML.Add_Child (Root, Last, Append => True);
      XML.Add_Child (Root, Front);
      Assert_True (Root.Child = Front);
      Assert_True (Front.Next = First and then First.Next = Middle);
      Assert_True (Middle.Next = Last and then Last.Next = null);
      Assert_True (Front.Parent = Root and then Last.Parent = Root);
      Assert_Cmpuint_Eq (Glib.Guint (XML.Children_Count (Root)), 4);

      --  Free detaches a first, middle and final sibling before releasing it.
      XML.Free (Middle);
      Assert_True (First.Next = Last);
      Assert_True (Root.Child = Front);
      XML.Free (Front);
      Assert_True (Root.Child = First);
      XML.Free (Last);
      Assert_True (First.Next = null);

      --  The append-to-empty branch initializes Child directly.
      XML.Add_Child (Empty_Root, New_Node ("only"), Append => True);
      Assert_Cmpuint_Eq (Glib.Guint (XML.Children_Count (Empty_Root)), 1);
      Assert_True (Empty_Root.Child.Parent = Empty_Root);

      XML.Free (Root);
      XML.Free (Empty_Root);
   end Test_Tree_Operations;

   ---------------------------------------
   -- Test_Attributes_And_Protection --
   ---------------------------------------

   procedure Test_Attributes_And_Protection is
      Root : XML.Node_Ptr := XML.Parse_Buffer
        ("<?xml?><entry first='1' change='old' keep='yes'/>");
      Bare : XML.Node_Ptr := New_Node ("bare");
      Escaped : constant String := XML.Protect
        ("<>&'""" & Character'Val (1) & " café");
      Bad_Byte : constant String := (1 => Character'Val (16#FF#));
   begin
      --  Updating an existing key replaces it; new keys are prepended, and
      --  values are escaped before they are stored in the raw attribute list.
      Assert_True (Root /= null);
      XML.Set_Attribute (Root, "change", "new & <value>");
      Assert_Cmpstr_Eq (XML.Get_Attribute (Root, "change"), "new & <value>");
      XML.Set_Attribute (Root, "added", "'quoted'");
      Assert_Cmpstr_Eq (XML.Get_Attribute (Root, "added"), "'quoted'");
      Assert_Cmpstr_Eq (XML.Get_Attribute (Root, "first"), "1");
      Assert_Cmpstr_Eq (XML.Get_Attribute (Root, "keep"), "yes");
      Assert_Cmpstr_Eq
        (XML.Get_Attribute (Bare, "missing", "fallback"), "fallback");
      XML.Set_Attribute (Bare, "first", "created");
      Assert_Cmpstr_Eq (XML.Get_Attribute (Bare, "first"), "created");

      --  Protect handles XML delimiters, a control character, valid UTF-8 and
      --  invalid UTF-8 (which is preserved byte-for-byte by this API).
      Assert_Cmpstr_Eq
        (Escaped, "&lt;&gt;&amp;&apos;&quot;&#1; café");
      Assert_Cmpstr_Eq (XML.Protect (Bad_Byte), Bad_Byte);
      Assert_Cmpstr_Eq (XML.Protect (""), "");
      XML.Free (Root);
      XML.Free (Bare);
   end Test_Attributes_And_Protection;

   ----------------------------------
   -- Test_Copy_Equality_And_Free --
   ----------------------------------

   procedure Test_Copy_Equality_And_Free is
      Root : XML.Node_Ptr := XML.Parse_Buffer
        ("<?xml?><root a='one'><left>value</left><right b='two'/></root>");
      Copy : XML.Node_Ptr;
      Plain : XML.Node_Ptr := New_Node ("plain", 4);
      Other : XML.Node_Ptr;
      Null_Tag : XML.Node_Ptr := New_Node ("ignored");
      Null_Fields : XML.Node_Ptr := New_Node ("ignored");
   begin
      --  Deep_Copy copies a subtree, its attributes and values, but does not
      --  copy the source node's sibling; the copy owns separate allocations.
      Assert_True (XML.Deep_Copy (null) = null);
      Copy := XML.Deep_Copy (Root);
      Assert_True (XML.Is_Equal (Root, Copy));
      Assert_True (Copy.Parent = null and then Copy.Next = null);
      Assert_True (Copy.Child.Parent = Copy);
      Assert_Cmpstr_Eq (Copy.Child.Next.Parent.Tag.all, "root");
      XML.Free (Root);
      Assert_Cmpstr_Eq (Copy.Child.Value.all, "value");

      --  Equality also handles null roots and distinguishes every stored
      --  field, including recursive child and sibling lists.
      Assert_True (XML.Is_Equal (null, null));
      Assert_True (not XML.Is_Equal (null, Copy));
      Assert_True (not XML.Is_Equal (Copy, null));
      Other := New_Node ("plain", 99);
      Assert_True (XML.Is_Equal (Plain, Other));
      XML.Free (Other);
      Other := New_Node ("other");
      Assert_True (not XML.Is_Equal (Plain, Other));
      XML.Free (Other);

      XML.Free (Null_Tag.Tag);
      Null_Tag.Tag := null;
      Null_Fields := New_Node ("ignored");
      XML.Free (Null_Fields.Tag);
      Null_Fields.Tag := null;
      Assert_True (XML.Is_Equal (Null_Tag, Null_Fields));
      Other := New_Node ("plain");
      Assert_True (not XML.Is_Equal (Null_Tag, Other));
      Assert_True (not XML.Is_Equal (Other, Null_Tag));
      XML.Free (Other);

      Other := New_Node ("plain");
      Other.Attributes := new String'("a=1");
      Assert_True (not XML.Is_Equal (Plain, Other));
      Assert_True (not XML.Is_Equal (Other, Plain));
      declare
         Peer : XML.Node_Ptr := New_Node ("plain");
      begin
         Peer.Attributes := new String'("a=2");
         Assert_True (not XML.Is_Equal (Other, Peer));
         XML.Free (Peer);
      end;
      XML.Free (Other);
      Other := New_Node ("plain");
      Other.Value := new String'("value");
      Assert_True (not XML.Is_Equal (Plain, Other));
      Assert_True (not XML.Is_Equal (Other, Plain));
      declare
         Peer : XML.Node_Ptr := New_Node ("plain");
      begin
         Peer.Value := new String'("different");
         Assert_True (not XML.Is_Equal (Other, Peer));
         XML.Free (Peer);
      end;
      XML.Free (Other);
      Other := New_Node ("plain");
      Other.Child := New_Node ("child");
      Other.Child.Parent := Other;
      Assert_True (not XML.Is_Equal (Plain, Other));
      Assert_True (not XML.Is_Equal (Other, Plain));
      XML.Free (Other);
      Other := New_Node ("plain");
      Other.Next := New_Node ("sibling");
      Assert_True (not XML.Is_Equal (Plain, Other));
      Assert_True (not XML.Is_Equal (Other, Plain));
      XML.Free (Other);

      --  Non-null child and sibling lists are compared recursively too.
      Other := New_Node ("parent");
      Other.Child := New_Node ("left-child");
      Other.Child.Parent := Other;
      declare
         Peer : XML.Node_Ptr := New_Node ("parent");
      begin
         Peer.Child := New_Node ("right-child");
         Peer.Child.Parent := Peer;
         Assert_True (not XML.Is_Equal (Other, Peer));
         XML.Free (Peer);
      end;
      XML.Free (Other);
      Other := New_Node ("sibling-list");
      Other.Next := New_Node ("left-sibling");
      declare
         Peer : XML.Node_Ptr := New_Node ("sibling-list");
      begin
         Peer.Next := New_Node ("right-sibling");
         Assert_True (not XML.Is_Equal (Other, Peer));
         XML.Free (Peer);
      end;
      XML.Free (Other);

      --  The callback is applied recursively to children and the node itself.
      Freed_Data := 0;
      XML.Free (Copy, Free_Data'Access);
      Assert_True (Copy = null);
      Assert_Cmpuint_Eq (Glib.Guint (Freed_Data), 3);
      XML.Free (Plain);
      XML.Free (Null_Tag);
      XML.Free (Null_Fields);
   end Test_Copy_Equality_And_Free;

   ----------------------------------
   -- Test_Print_And_File_Parse --
   ----------------------------------

   procedure Test_Print_And_File_Parse is
      Root : XML.Node_Ptr := XML.Parse_Buffer
        ("<?xml?><root><text>placeholder</text><empty /></root>");
      Text_Node : XML.Node_Ptr;
      Read_Back : XML.Node_Ptr;
      Success : Boolean;
      Control : XML.Node_Ptr := New_Node ("control");
      Null_Value : XML.Node_Ptr := New_Node ("null-value");
      Slash_Tag : XML.Node_Ptr := New_Node ("empty/");
   begin
      --  Printing to a file escapes text, preserves the tree, and the file
      --  parser reads the result back. A missing directory exercises failure.
      Assert_True (Root /= null);
      Text_Node := XML.Find_Tag (Root.Child, "text");
      Assert_True (Text_Node /= null);
      XML.Free (Text_Node.Value);
      Text_Node.Value := new String'("a < b & c > d ""quoted"" 'single'");
      XML.Set_Attribute (Root, "version", "one & two");
      XML.Print (Root, Roundtrip_File, Success);
      Assert_True (Success);
      Assert_True
        (Ada.Strings.Fixed.Index (Read_Text (Roundtrip_File),
                                  "a &lt; b &amp; c &gt; d "
                                  & "&quot;quoted&quot; &apos;single&apos;")
         /= 0);
      Read_Back := XML.Parse (Roundtrip_File);
      Assert_True (Read_Back /= null);
      Assert_True (XML.Is_Equal (Root, Read_Back));
      XML.Free (Read_Back);

      XML.Print (Root, Invalid_File, Success);
      Assert_True (not Success);

      --  Exercise the default overload's standard-output path as well.
      XML.Print (Root);

      --  XML's printer serializes control bytes as numeric character refs.
      Control.Value := new String'(1 => Character'Val (1));
      XML.Print (Control, Roundtrip_File, Success);
      Assert_True (Success);
      Assert_True
        (Ada.Strings.Fixed.Index (Read_Text (Roundtrip_File), "&#1;") /= 0);

      --  Hand-built nodes cover the legacy trailing-slash representation and
      --  the distinct null-value case; the parser normalizes slash tags.
      XML.Print (Slash_Tag, Roundtrip_File, Success);
      Assert_True (Success);
      Assert_True
        (Ada.Strings.Fixed.Index (Read_Text (Roundtrip_File), "<empty/>") /= 0);
      XML.Print (Null_Value, Roundtrip_File, Success);
      Assert_True (Success);
      Assert_True
        (Ada.Strings.Fixed.Index
           (Read_Text (Roundtrip_File), "<null-value />") /= 0);

      XML.Free (Control);
      XML.Free (Null_Value);
      XML.Free (Slash_Tag);
      XML.Free (Root);
      Ada.Directories.Delete_File (Roundtrip_File);
   end Test_Print_And_File_Parse;

   -------------------------------------
   -- Test_Encoding_And_Parse_Errors --
   -------------------------------------

   procedure Test_Encoding_And_Parse_Errors is
      Latin_1 : constant String :=
        "<?xml version=""1.0"" encoding=""ISO-8859-1""?>"
        & "<root>caf" & Character'Val (16#E9#) & "</root>";
      No_Closing_Quote : constant String :=
        "<?xml version=""1.0"" encoding=""ISO-8859-1?>" & "<root/>";
      Root : XML.Node_Ptr;
   begin
      --  Explicit legacy encodings are converted to UTF-8. A missing closing
      --  encoding quote falls back to parsing the buffer as supplied.
      Root := XML.Parse_Buffer (Latin_1);
      Assert_True (Root /= null);
      Assert_Cmpstr_Eq (Root.Value.all, "café");
      XML.Free (Root);

      Root := XML.Parse_Buffer (No_Closing_Quote);
      Assert_True (Root /= null);
      XML.Free (Root);
      XML.Free (Root);
      Assert_True (XML.Parse_Buffer ("short") = null);
      Assert_True
        (XML.Parse_Buffer ("<?xml?><") = null);
      Assert_True (XML.Parse ("no-such-xml-file.xml") = null);
   end Test_Encoding_And_Parse_Errors;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/xml/parse-and-search",
                       Test_Parse_And_Search'Unrestricted_Access);
   Glib.Test.Add_Func ("/xml/tree-operations",
                       Test_Tree_Operations'Unrestricted_Access);
   Glib.Test.Add_Func ("/xml/attributes-and-protection",
                       Test_Attributes_And_Protection'Unrestricted_Access);
   Glib.Test.Add_Func ("/xml/copy-equality-and-free",
                       Test_Copy_Equality_And_Free'Unrestricted_Access);
   Glib.Test.Add_Func ("/xml/print-and-file-parse",
                       Test_Print_And_File_Parse'Unrestricted_Access);
   Glib.Test.Add_Func ("/xml/encoding-and-errors",
                       Test_Encoding_And_Parse_Errors'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
