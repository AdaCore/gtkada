-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                      Copyright (C) 1999                           --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
with Ada.Direct_IO;

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

   procedure Extract_Attrib
     (Tag        : in out String_Ptr;
      Attributes : out String_Ptr;
      Empty_Node : out Boolean);
   --  Extract the attributes as a string, if the tag contains blanks ' '
   --  On return, Tag is unchanged and Attributes contains the string
   --  If the last character in Tag is '/' then the node is empty and
   --  Empty_Node is set to True

   procedure Get_Next_Word
     (Buf     : String;
      Index   : in out Natural;
      Word    : out String_Ptr);
   --  extract the next textual word from Buf and return it.
   --  return null if no word left

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
      Start : Natural := Index;

   begin
      while Buf (Index) /= Terminator loop
         Index := Index + 1;
      end loop;

      S := new String' (Buf (Start .. Index - 1));
      Index := Index + 1;

      if Index < Buf'Last then
         Skip_Blanks (Buf, Index);
      end if;
   end Get_Buf;

   ------------------------
   -- Extract_Attributes --
   ------------------------

   procedure Extract_Attrib
     (Tag : in out String_Ptr;
      Attributes : out String_Ptr;
      Empty_Node : out Boolean)
   is
      Index : Natural := Tag'First;
      Index_Last_Of_Tag : Natural;
      S : String_Ptr;

   begin
      --  First decide if the node is empty

      if Tag.all (Tag.all'Last) = '/' then
         Empty_Node := True;
      else
         Empty_Node := False;
      end if;

      while Index <= Tag.all'Last and then
        not
          (Tag.all (Index) = ' '  or else Tag.all (Index) = ASCII.LF
           or else Tag.all (Index) = ASCII.HT
           or else Tag.all (Index) = ASCII.CR)
      loop
         Index := Index + 1;
      end loop;

      Index_Last_Of_Tag := Index - 1;
      Skip_Blanks (Tag.all, Index);

      if Index <= Tag.all'Last then
         if Empty_Node then
            Attributes := new String' (Tag (Index .. Tag'Last - 1));
         else
            Attributes := new String' (Tag (Index .. Tag'Last));
         end if;

         S := new String' (Tag.all (Tag.all'First .. Index_Last_Of_Tag));
         Free (Tag);
         Tag := S;
      end if;
   end Extract_Attrib;

   --------------------
   --  Get_Next_Word --
   --------------------

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
            while Buf (Index) /= ' ' and
              Buf (Index) /= '=' loop
               Index := Index + 1;
            end loop;

            Word := new String' (Buf (Start_Index .. Index - 1));
         end;
      end if;

      if Index < Buf'Last then
         Skip_Blanks (Buf, Index);
      end if;
   end Get_Next_Word;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute
     (N : in Node_Ptr;
      Attribute_Name : in String) return String_Ptr
   is
      Index : Natural := N.Attributes.all'First;
      Key, Value : String_Ptr;

   begin
      while Index < N.Attributes.all'Last loop
         Get_Next_Word (N.Attributes.all, Index, Key);
         Get_Buf (N.Attributes.all, Index, '=', Value);
         Get_Next_Word (N.Attributes.all, Index, Value);

         if Attribute_Name = Key.all then
            exit;
         else
            Free (Key);
            Free (Value);
         end if;
      end loop;
      return Value;
   end Get_Attribute;

   --------------
   -- Get_Node --
   --------------

   function Get_Node (Buf : String; Index : access Natural) return Node_Ptr is
      N : Node_Ptr := new Node;
      Q : Node_Ptr;
      S : String_Ptr;
      Index_Save : Natural;
      Empty_Node : Boolean;

   begin
      pragma Assert (Buf (Index.all) = '<');
      Index.all := Index.all + 1;
      Index_Save := Index.all;
      Get_Buf (Buf, Index.all, '>', N.Tag);

      --  Here we have to deal with the attributes of the form
      --  <tag attrib='xyyzy'>

      Extract_Attrib (N.Tag, N.Attributes, Empty_Node);

      --  it is possible to have a child-less node that has the form
      --  <tag /> or <tag attrib='xyyzy'/>

      if Empty_Node then
         N.Value := new String' ("");
      else
         if Buf (Index.all) = '<' then
            if Buf (Index.all + 1) = '/' then

               --  No value contained on this node

               N.Value := new String' ("");
               Index.all := Index.all + 1;

            else

               --  Parse the children

               N.Child := Get_Node (Buf, Index);
               N.Child.Parent := N;
               N.Last_Child := N.Child;
               pragma Assert (Buf (Index.all) = '<');

               while Buf (Index.all + 1) /= '/' loop
                  Q := N.Last_Child;
                  Q.Next := Get_Node (Buf, Index);
                  Q.Next.Parent := N;
                  N.Last_Child := Q.Next;
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
      end if;

      return N;
   end Get_Node;

   -----------
   -- Print --
   -----------

   procedure Print (N : Node_Ptr; Indent : Natural := 0) is
      P : Node_Ptr;

      procedure Do_Indent (Indent : Natural);

      procedure Do_Indent (Indent : Natural) is
      begin
         for J in 1 .. Indent loop
            Put (' ');
         end loop;
      end Do_Indent;

   begin
      Do_Indent (Indent);
      Put ("<" & N.Tag.all);

      if N.Attributes /= null then
         Put (" " & N.Attributes.all);
      end if;

      if N.Child /= null then
         Put_Line (">");
         Print (N.Child, Indent + 2);
         P := N.Child.Next;

         while P /= null loop
            Print (P, Indent + 2);
            P := P.Next;
         end loop;

         Do_Indent (Indent);
         Put_Line ("</" & N.Tag.all & ">");

      else
         if N.Value.all = "" then
            Put_Line ("/>");
         else
            Put (">");
            Put (N.Value.all);
            Put_Line ("</" & N.Tag.all & ">");
         end if;
      end if;
   end Print;

   -----------
   -- Parse --
   -----------

   function Parse (File : String) return Node_Ptr is

      procedure Fast_Read (The_File : in String;
                           Buf      : in String_Ptr);
      --  Read Buf'length characters in The_File and store it in Buf.
      --  This procedure performs a single call to Read, so it is supposed to
      --  be more efficient than the previous implementation (read character
      --  by character).

      procedure Fast_Read (The_File : in String;
                           Buf      : in String_Ptr) is
         type Fixed_String is new String (Buf'Range);

         package Dir_Fast is new Ada.Direct_IO (Fixed_String);
         use Dir_Fast;

         F : Dir_Fast.File_Type;

      begin
         Dir_Fast.Open (F, In_File, The_File);
         Dir_Fast.Read (F, Fixed_String (Buf.all));
         Dir_Fast.Close (F);
      end Fast_Read;

      use Dir;

      Index       : aliased Natural := 2;
      F           : Dir.File_Type;
      Buf         : String_Ptr;
      XML_Version : String_Ptr;

   begin
      Open (F, In_File, File);
      Buf := new String (1 .. Natural (Size (F)));
      Close (F);
      Fast_Read (File, Buf);
      Get_Buf (Buf.all, Index, '>', XML_Version);
      return Get_Node (Buf.all, Index'Unchecked_Access);
   end Parse;

   --------------
   -- Find_Tag --
   --------------

   function Find_Tag (N : Node_Ptr; Tag : String) return Node_Ptr is
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

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field (N : Node_Ptr; Field : String) return String_Ptr is
      P : Node_Ptr := Find_Tag (N.Child, Field);

   begin
      if P /= null then
         return P.Value;
      else
         return null;
      end if;
   end Get_Field;

end Glib.XML;
