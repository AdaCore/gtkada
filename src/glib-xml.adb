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

   --------------
   -- Get_Node --
   --------------

   function Get_Node (Buf : String; Index : access Natural) return Node_Ptr is
      N : Node_Ptr := new Node;
      P, Q : Node_Ptr;
      S : String_Ptr;

   begin
      pragma Assert (Buf (Index.all) = '<');
      Index.all := Index.all + 1;
      Get_Buf (Buf, Index.all, '>', N.Tag);

      if Buf (Index.all) = '<' then
         if Buf (Index.all + 1) = '/' then

            --  No value contained on this node

            N.Value := new String' ("");
            Index.all := Index.all + 1;

         else

            --  Parse the children

            N.Child := Get_Node (Buf, Index);
            N.Child.Parent := N;
            pragma Assert (Buf (Index.all) = '<');

            while Buf (Index.all + 1) /= '/' loop
               Q := N.Child;
               P := Q.Next;

               while P /= null loop
                  Q := P;
                  P := P.Next;
               end loop;

               Q.Next := Get_Node (Buf, Index);
               Q.Next.Parent := N;
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
      Put ("<" & N.Tag.all & ">");

      if N.Child /= null then
         New_Line;
         Print (N.Child, Indent + 2);
         P := N.Child.Next;

         while P /= null loop
            Print (P, Indent + 2);
            P := P.Next;
         end loop;

         Do_Indent (Indent);
         Put_Line ("</" & N.Tag.all & ">");

      else
         Put (N.Value.all);
         Put_Line ("</" & N.Tag.all & ">");
      end if;

   end Print;

   -----------
   -- Parse --
   -----------

   function Parse (File : String) return Node_Ptr is

      use Dir;

      Index       : aliased Natural := 2;
      F           : Dir.File_Type;
      Buf         : String_Ptr;
      XML_Version : String_Ptr;

   begin
      Open (F, In_File, File);
      Buf := new String (1 .. Natural (Size (F)));

      for J in 1 .. Natural (Size (F)) loop
         Read (F, Buf (J));
      end loop;

      Close (F);

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
