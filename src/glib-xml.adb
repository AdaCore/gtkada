-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1999-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2004 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Direct_IO;
with Glib.Convert;  use Glib.Convert;
with Glib.Error;    use Glib.Error;
with Glib.Messages; use Glib.Messages;

package body Glib.XML is

   package Dir is new Ada.Direct_IO (Character);

   procedure Skip_Blanks (Buf : String; Index : in out Natural);
   --  Skip blanks, LF and CR, starting at Index. Index is updated to the
   --  new position (first non blank or EOF)

   function Get_Node (Buf : String; Index : access Natural) return Node_Ptr;
   --  The main parse routine. Starting at Index.all, Index.all is updated
   --  on return. Return the node starting at Buf (Index.all) which will
   --  also contain all the children and subchildren.

   procedure Get_Buf
     (Buf        : String;
      Index      : in out Natural;
      Terminator : Character;
      S          : out String_Ptr);
   --  On return, S will contain the String starting at Buf (Index) and
   --  terminating before the first 'Terminator' character. Index will also
   --  point to the next non blank character.
   --  The special XML '&' characters are translated appropriately in S.
   --  S is set to null if Terminator wasn't found in Buf.

   procedure Extract_Attrib
     (Tag        : in out String_Ptr;
      Attributes : out String_Ptr;
      Empty_Node : out Boolean);
   --  Extract the attributes as a string, if the tag contains blanks ' '
   --  On return, Tag is unchanged and Attributes contains the string
   --  If the last character in Tag is '/' then the node is empty and
   --  Empty_Node is set to True.

   procedure Get_Next_Word
     (Buf     : String;
      Index   : in out Natural;
      Word    : out String_Ptr);
   --  extract the next textual word from Buf and return it.
   --  return null if no word left.
   --  The special XML '&' characters are translated appropriately in S.

   function Translate (S : String) return String;
   --  Translate S by replacing the XML '&' special characters by the
   --  actual ASCII character.
   --  This function currently handles:
   --   - &quot;
   --   - &gt;
   --   - &lt;
   --   - &amp;
   --   - &apos;

   -----------------
   -- Skip_Blanks --
   -----------------

   procedure Skip_Blanks (Buf : String; Index : in out Natural) is
   begin
      while Index < Buf'Last and then
        (Buf (Index) = ' '  or else Buf (Index) = ASCII.LF
          or else Buf (Index) = ASCII.HT
          or else Buf (Index) = ASCII.CR)
      loop
         Index := Index + 1;
      end loop;
   end Skip_Blanks;

   -------------
   -- Get_Buf --
   -------------

   procedure Get_Buf
     (Buf        : String;
      Index      : in out Natural;
      Terminator : Character;
      S          : out String_Ptr)
   is
      Start : constant Natural := Index;

   begin
      while Index <= Buf'Last
        and then Buf (Index) /= Terminator
      loop
         Index := Index + 1;
      end loop;

      if Index > Buf'Last then
         S := null;

      else
         S := new String'(Translate (Buf (Start .. Index - 1)));
         Index := Index + 1;

         if Index < Buf'Last then
            Skip_Blanks (Buf, Index);
         end if;
      end if;
   end Get_Buf;

   ------------------------
   -- Extract_Attributes --
   ------------------------

   procedure Extract_Attrib
     (Tag        : in out String_Ptr;
      Attributes : out String_Ptr;
      Empty_Node : out Boolean)
   is
      Index             : Natural := Tag'First;
      Index_Last_Of_Tag : Natural;
      S                 : String_Ptr;

   begin
      --  First decide if the node is empty

      if Tag (Tag'Last) = '/' then
         Empty_Node := True;
      else
         Empty_Node := False;
      end if;

      while Index <= Tag'Last and then
        not
          (Tag (Index) = ' '  or else Tag (Index) = ASCII.LF
           or else Tag (Index) = ASCII.HT
           or else Tag (Index) = ASCII.CR)
      loop
         Index := Index + 1;
      end loop;

      Index_Last_Of_Tag := Index - 1;
      Skip_Blanks (Tag.all, Index);

      if Index <= Tag'Last then
         if Empty_Node then
            Attributes := new String'(Tag (Index .. Tag'Last - 1));
         else
            Attributes := new String'(Tag (Index .. Tag'Last));
         end if;

         S := new String'(Tag (Tag'First .. Index_Last_Of_Tag));
         Free (Tag);
         Tag := S;
      end if;
   end Extract_Attrib;

   -------------------
   -- Get_Next_Word --
   -------------------

   procedure Get_Next_Word
     (Buf     : String;
      Index   : in out Natural;
      Word    : out String_Ptr)
   is
      Terminator : Character := ' ';
   begin
      Skip_Blanks (Buf, Index);

      if Buf (Index) = ''' or Buf (Index) = '"' then
         --  If the word starts with a quotation mark, then read until
         --  the closing mark

         Terminator := Buf (Index);
         Index := Index + 1;
         Get_Buf (Buf, Index, Terminator, Word);

      else
         --  For a normal word, scan up to either a blank, or a '='

         declare
            Start_Index : constant Natural := Index;
         begin
            while Index <= Buf'Last
              and then Buf (Index) /= ' '
              and then Buf (Index) /= '='
            loop
               Index := Index + 1;
            end loop;

            Word := new String'(Translate (Buf (Start_Index .. Index - 1)));
         end;
      end if;

      if Index < Buf'Last then
         Skip_Blanks (Buf, Index);
      end if;
   end Get_Next_Word;

   ---------------
   -- Translate --
   ---------------

   function Translate (S : String) return String is
      Str       : String (1 .. S'Length);
      Start, J  : Positive;
      Index     : Positive := S'First;
      In_String : Boolean  := False;

   begin
      if S'Length = 0 then
         return S;
      else
         J := Str'First;

         loop
            if In_String or else S (Index) /= '&' then
               Str (J) := S (Index);
            else
               Index := Index + 1;
               Start := Index;

               while S (Index) /= ';' loop
                  Index := Index + 1;
                  pragma Assert (Index <= S'Last);
               end loop;

               if S (Start .. Index - 1) = "quot" then
                  Str (J) := '"';
               elsif S (Start .. Index - 1) = "gt" then
                  Str (J) := '>';
               elsif S (Start .. Index - 1) = "lt" then
                  Str (J) := '<';
               elsif S (Start .. Index - 1) = "amp" then
                  Str (J) := '&';
               elsif S (Start .. Index - 1) = "apos" then
                  Str (J) := ''';
               end if;
            end if;

            exit when Index = S'Last;

            if S (Index) = '"' then
               In_String := not In_String;
            end if;

            Index := Index + 1;
            J     := J + 1;
         end loop;

         return Str (1 .. J);
      end if;
   end Translate;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute
     (N : in Node_Ptr;
      Attribute_Name : in UTF8_String;
      Default        : in UTF8_String := "") return UTF8_String
   is
      Index      : Natural;
      Key, Value : String_Ptr;

   begin
      if N = null or else N.Attributes = null then
         return Default;
      end if;

      Index := N.Attributes'First;
      while Index < N.Attributes'Last loop
         Get_Next_Word (N.Attributes.all, Index, Key);
         Get_Buf (N.Attributes.all, Index, '=', Value);
         Free (Value);
         Get_Next_Word (N.Attributes.all, Index, Value);

         if Attribute_Name = Key.all then
            exit;
         else
            Free (Key);
            Free (Value);
         end if;
      end loop;

      Free (Key);

      if Value = null then
         return Default;
      else
         declare
            V : constant String := Value.all;
         begin
            Free (Value);

            return V (V'First .. V'Last);
         end;
      end if;
   end Get_Attribute;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (N : Node_Ptr; Attribute_Name, Attribute_Value : UTF8_String)
   is
      Index, Tmp : Natural;
      Key, Value : String_Ptr;
      Atts       : String_Ptr;
      Str        : constant String :=
        Attribute_Name & "=""" & Translate (Attribute_Value) & """ ";

   begin
      if N.Attributes /= null then
         Index := N.Attributes'First;
         --  First remove any definition of the attribute in the current list
         while Index < N.Attributes'Last loop
            Tmp := Index;
            Get_Next_Word (N.Attributes.all, Index, Key);
            Get_Buf (N.Attributes.all, Index, '=', Value);
            Get_Next_Word (N.Attributes.all, Index, Value);

            Free (Value);

            if Attribute_Name = Key.all then
               Atts := new String'
                 (Str
                  & N.Attributes (N.Attributes'First .. Tmp - 1)
                  & N.Attributes (Index .. N.Attributes'Last));
               Free (N.Attributes);
               N.Attributes := Atts;
               Free (Key);
               return;
            end if;

            Free (Key);
         end loop;

         Atts := new String'(Str & N.Attributes.all);
         Free (N.Attributes);
         N.Attributes := Atts;

      else
         N.Attributes := new String'(Str);
      end if;
   end Set_Attribute;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (N : Node_Ptr; Child : Node_Ptr; Append : Boolean := False)
   is
      Tmp : Node_Ptr;
   begin
      if Append then
         if N.Child = null then
            N.Child := Child;
         else
            Tmp := N.Child;
            while Tmp.Next /= null loop
               Tmp := Tmp.Next;
            end loop;
            Tmp.Next := Child;
         end if;
      else
         Child.Next := N.Child;
         Child.Parent := N;
         N.Child := Child;
      end if;
   end Add_Child;

   --------------
   -- Get_Node --
   --------------

   function Get_Node (Buf : String; Index : access Natural) return Node_Ptr is
      N : constant Node_Ptr := new Node;
      Q : Node_Ptr;
      S : String_Ptr;
      Empty_Node : Boolean;
      Last_Child : Node_Ptr;

   begin
      pragma Assert (Buf (Index.all) = '<');
      Index.all := Index.all + 1;
      Get_Buf (Buf, Index.all, '>', N.Tag);

      --  Check to see whether it is a comment, !DOCTYPE, or the like:

      if N.Tag (N.Tag'First) = '!' then
         return Get_Node (Buf, Index);
      else
         --  Here we have to deal with the attributes of the form
         --  <tag attrib='xyyzy'>

         Extract_Attrib (N.Tag, N.Attributes, Empty_Node);

         --  it is possible to have a child-less node that has the form
         --  <tag /> or <tag attrib='xyyzy'/>

         if Empty_Node then
            N.Value := new String'("");
         else
            if Buf (Index.all) = '<' then
               if Buf (Index.all + 1) = '/' then
                  --  No value contained on this node

                  N.Value := new String'("");
                  Index.all := Index.all + 1;

               else
                  --  Parse the children

                  Add_Child (N, Get_Node (Buf, Index));
                  Last_Child := N.Child;
                  pragma Assert (Buf (Index.all) = '<');

                  while Buf (Index.all + 1) /= '/' loop
                     Q := Last_Child;
                     Q.Next := Get_Node (Buf, Index);
                     Q.Next.Parent := N;
                     Last_Child := Q.Next;
                     pragma Assert (Buf (Index.all) = '<');
                  end loop;

                  Index.all := Index.all + 1;
               end if;

            else
               --  Get the value of this node

               Get_Buf (Buf, Index.all, '<', N.Value);
            end if;

            pragma Assert (Buf (Index.all) = '/');
            Index.all := Index.all + 1;
            Get_Buf (Buf, Index.all, '>', S);
            pragma Assert (N.Tag.all = S.all);
            Free (S);
         end if;

         return N;
      end if;
   end Get_Node;

   -------------
   -- Protect --
   -------------

   function Protect (S : String) return String is
      Length : Natural := 0;
   begin
      for J in S'Range loop
         case S (J) is
            when '<' => Length := Length + 4;
            when '>' => Length := Length + 4;
            when '&' => Length := Length + 5;
            when ''' => Length := Length + 6;
            when '"' => Length := Length + 6;
            when others => Length := Length + 1;
         end case;
      end loop;

      declare
         Result : String (1 .. Length);
         Index : Integer := 1;
      begin
         for J in S'Range loop
            case S (J) is
               when '<' =>
                  Result (Index .. Index + 3) := "&lt;";
                  Index := Index + 4;
               when '>' =>
                  Result (Index .. Index + 3) := "&gt;";
                  Index := Index + 4;
               when '&' =>
                  Result (Index .. Index + 4) := "&amp;";
                  Index := Index + 5;
               when ''' =>
                  Result (Index .. Index + 5) := "&apos;";
                  Index := Index + 6;
               when '"' =>
                  Result (Index .. Index + 5) := "&quot;";
                  Index := Index + 6;
               when others =>
                  Result (Index) := S (J);
                  Index := Index + 1;
            end case;
         end loop;

         return Result;
      end;
   end Protect;

   -----------
   -- Print --
   -----------

   procedure Print (N : Node_Ptr; File_Name : String := "") is
      File : File_Type;

      procedure Do_Indent (Indent : Natural);
      --  Print a string made of Indent blank characters.

      procedure Print_String (S : String);
      --  Print S to File, after replacing the special '<', '>',
      --  '"', '&' and ''' characters.

      procedure Print_Node (N : Node_Ptr; Indent : Natural);
      --  Write a node and its children to File

      ---------------
      -- Do_Indent --
      ---------------

      procedure Do_Indent (Indent : Natural) is
      begin
         Put ((1 .. Indent => ' '));
      end Do_Indent;

      ------------------
      -- Print_String --
      ------------------

      procedure Print_String (S : String) is
      begin
         for J in S'Range loop
            case S (J) is
               when '<' => Put ("&lt;");
               when '>' => Put ("&gt;");
               when '&' => Put ("&amp;");
               when ''' => Put ("&apos;");
               when '"' => Put ("&quot;");
               when others => Put (S (J));
            end case;
         end loop;
      end Print_String;

      ----------------
      -- Print_Node --
      ----------------

      procedure Print_Node (N : Node_Ptr; Indent : Natural) is
         P : Node_Ptr;
      begin
         Do_Indent (Indent);
         Put ("<" & N.Tag.all);

         if N.Attributes /= null then
            Put (" " & N.Attributes.all);
         end if;

         if N.Child /= null then
            Put_Line (">");
            P := N.Child;
            while P /= null loop
               Print_Node (P, Indent + 2);
               P := P.Next;
            end loop;

            Do_Indent (Indent);
            Put_Line ("</" & N.Tag.all & ">");

         elsif N.Value = null
           or else N.Value.all = ""
         then
            --  The following handles the difference between what you got
            --  when you parsed <tag/> vs. <tag />.
            if N.Tag (N.Tag'Last) = '/' then
               Put_Line (">");
            else
               Put_Line (" />");
            end if;
         else
            Put (">");
            Print_String (N.Value.all);
            Put_Line ("</" & N.Tag.all & ">");
         end if;
      end Print_Node;

   begin
      if File_Name /= "" then
         Create (File, Out_File, File_Name);
         Set_Output (File);
      end if;
      Put_Line ("<?xml version=""1.0""?>");
      Print_Node (N, 0);

      if File_Name /= "" then
         Close (File);
         Set_Output (Standard_Output);
      end if;
   end Print;

   -----------
   -- Parse --
   -----------

   function Parse (File : String) return Node_Ptr is

      procedure Fast_Read (The_File : in String;
                           Buf      : in out String_Ptr);
      --  Read Buf'length characters in The_File and store it in Buf.
      --  This procedure performs a single call to Read, so it is supposed to
      --  be more efficient than the previous implementation (read character
      --  by character).

      procedure Fast_Read (The_File : in String;
                           Buf      : in out String_Ptr) is
         type Fixed_String is new String (Buf'Range);

         package Dir_Fast is new Ada.Direct_IO (Fixed_String);
         use Dir_Fast;

         F : Dir_Fast.File_Type;

      begin
         Dir_Fast.Open (F, In_File, The_File);
         Dir_Fast.Read (F, Fixed_String (Buf.all));
         Dir_Fast.Close (F);
      exception
         when others =>
            Free (Buf);
            Dir_Fast.Close (F);
            raise;
      end Fast_Read;

      use Dir;

      F           : Dir.File_Type;
      Buf         : String_Ptr;
      Result      : Node_Ptr;

   begin
      begin
         Open (F, In_File, File);
         Buf := new String (1 .. Natural (Size (F)));
         Close (F);
      exception
         when others =>
            Free (Buf);
            Close (F);
            raise;
      end;

      if Buf'Length = 0 then
         Free (Buf);
         return null;
      end if;

      Fast_Read (File, Buf);
      Result := Parse_Buffer (Buf.all);
      Free (Buf);
      return Result;
   end Parse;

   ------------------
   -- Parse_Buffer --
   ------------------

   function Parse_Buffer (Buffer : UTF8_String) return Node_Ptr is
      Index       : aliased Natural := 2;
      XML_Version : String_Ptr;
      Encoding    : Integer;
      Encoding_Last : Integer;
      Result        : Node_Ptr;
   begin
      Get_Buf (Buffer, Index, '>', XML_Version);
      if XML_Version = null then
         return null;
      else
         --  Check the encoding specified for that file
         Encoding := Ada.Strings.Fixed.Index (XML_Version.all, "encoding");

         if Encoding /= 0 then
            while Encoding <= XML_Version'Last
              and then XML_Version (Encoding) /= '"'
            loop
               Encoding := Encoding + 1;
            end loop;

            Encoding := Encoding + 1;
            Encoding_Last := Encoding + 1;

            while Encoding_Last <= XML_Version'Last
              and then XML_Version (Encoding_Last) /= '"'
            loop
               Encoding_Last := Encoding_Last + 1;
            end loop;

            if Encoding_Last <= XML_Version'Last then
               declare
                  Error       : aliased GError;
                  Utf8_Buffer : constant String := Glib.Convert.Convert
                    (Buffer,
                     To_Codeset => "UTF-8",
                     From_Codeset =>
                       XML_Version (Encoding .. Encoding_Last - 1),
                     Error => Error'Unchecked_Access);
               begin
                  if Utf8_Buffer /= "" then
                     Result := Get_Node (Utf8_Buffer, Index'Unchecked_Access);
                  else
                     Glib.Messages.Log
                       ("Glib", Log_Level_Warning, Get_Message (Error));
                     Error_Free (Error);
                  end if;
               end;
            else
               Result := Get_Node (Buffer, Index'Unchecked_Access);
            end if;
         else
            Result := Get_Node (Buffer, Index'Unchecked_Access);
         end if;

         Free (XML_Version);
         return Result;
      end if;
   end Parse_Buffer;

   --------------
   -- Find_Tag --
   --------------

   function Find_Tag (N : Node_Ptr; Tag : UTF8_String) return Node_Ptr is
      P : Node_Ptr := N;

   begin
      while P /= null loop
         if P.Tag.all = Tag then
            return P;
         end if;

         P := P.Next;
      end loop;

      return null;
   end Find_Tag;

   -----------------------------
   -- Find_Tag_With_Attribute --
   -----------------------------

   function Find_Tag_With_Attribute
     (N : Node_Ptr;
      Tag : UTF8_String;
      Key : UTF8_String;
      Value : UTF8_String := "") return Node_Ptr
   is
      P : Node_Ptr := N;
   begin
      while P /= null loop
         if P.Tag.all = Tag then
            declare
               The_Value : constant String := Get_Attribute (P, Key);
            begin
               if The_Value /= "" then
                  if Value = "" or The_Value = Value then
                     --  if Value is not given when calling the
                     --  the function only the Key need to match
                     return P;
                  end if;
               end if;
            end;
         end if;
         P := P.Next;
      end loop;

      return null;
   end Find_Tag_With_Attribute;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field (N : Node_Ptr; Field : UTF8_String) return String_Ptr is
      P : constant Node_Ptr := Find_Tag (N.Child, Field);

   begin
      if P /= null then
         return P.Value;
      else
         return null;
      end if;
   end Get_Field;

   ----------
   -- Free --
   ----------

   procedure Free
     (N : in out Node_Ptr; Free_Data : Free_Specific_Data := null)
   is
      procedure Free_Node (N : in out Node_Ptr);
      --  Free the memory for a node, but doesn't remove it from its parent

      procedure Unchecked_Free is new Unchecked_Deallocation (Node, Node_Ptr);

      ---------------
      -- Free_Node --
      ---------------

      procedure Free_Node (N : in out Node_Ptr) is
         Child : Node_Ptr := N.Child;
         Next  : Node_Ptr;

      begin
         Free (N.Tag);
         Free (N.Attributes);
         Free (N.Value);

         if Free_Data /= null then
            Free_Data (N.Specific_Data);
         end if;

         --  Free all the children
         while Child /= null loop
            Next := Child.Next;
            Free_Node (Child);
            Child := Next;
         end loop;

         Unchecked_Free (N);
      end Free_Node;

      Child    : Node_Ptr;
      Previous : Node_Ptr;

   begin
      if N = null then
         return;
      end if;

      if N.Parent /= null then
         Child := N.Parent.Child;

         --  Remove the node from its parent
         while Child /= null and then Child /= N loop
            Previous := Child;
            Child := Child.Next;
         end loop;

         if Previous = null then
            N.Parent.Child := N.Next;
         else
            Previous.Next := N.Next;
         end if;
      end if;

      --  Free the memory occupied by the node
      Free_Node (N);
   end Free;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy (N : Node_Ptr) return Node_Ptr is
      function Deep_Copy_Internal
        (N : Node_Ptr; Parent : Node_Ptr := null) return Node_Ptr;
      --  Internal version of Deep_Copy. Returns a deep copy of N, whose
      --  parent should be Parent.

      function Deep_Copy_Internal
        (N : Node_Ptr; Parent : Node_Ptr := null) return Node_Ptr
      is
         Attr  : String_Ptr;
         Value : String_Ptr;

         New_N : Node_Ptr;
      begin
         if N = null then
            return null;
         else
            if N.Attributes /= null then
               Attr := new String'(N.Attributes.all);
            end if;

            if N.Value /= null then
               Value := new String'(N.Value.all);
            end if;

            New_N := new Node'
              (Tag => new String'(N.Tag.all),
               Attributes => Attr,
               Value => Value,
               Parent => Parent,
               Child => null,
               Next => Deep_Copy_Internal (N.Next, Parent => Parent),
               Specific_Data => N.Specific_Data);

            New_N.Child := Deep_Copy_Internal (N.Child, Parent => New_N);

            return New_N;
         end if;
      end Deep_Copy_Internal;

   begin
      return Deep_Copy_Internal (N);
   end Deep_Copy;

end Glib.XML;
