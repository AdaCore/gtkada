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

--  <description>
--
--  This package provides a simple minded XML parser to be used with
--  Gate.
--
--  </description>

with Unchecked_Deallocation;

generic
   type XML_Specific_Data is private;
   --  The type of the extra data that can be attached to each node of the
   --  XML tree. See for instance the package Glib.Glade.

package Glib.XML is

   --  <doc_ignore>
   procedure Free is new Unchecked_Deallocation (String, String_Ptr);
   --  </doc_ignore>

   type Node;
   type Node_Ptr is access all Node;
   --  Pointer to a node of the XML tree.

   type Node is record
      Tag   : String_Ptr;
      --  The name of this node

      Attributes   : String_Ptr;
      --  The attributes of this node

      Value : String_Ptr;
      --  The value, or null is not relevant

      Parent : Node_Ptr;
      --  The parent of this Node.

      Child : Node_Ptr;
      --  The first Child of this Node. The next child is Child.Next

      Next  : Node_Ptr;
      --  Next "brother" node.

      Specific_Data : XML_Specific_Data;
      --  Use to store data specific to each implementation (e.g a boolean
      --  indicating whether this node has been accessed)
   end record;
   --  A node of the XML tree.
   --  Each time a tag is found in the XML file, a new node is created, that
   --  points to its parent, its children and its siblings (nodes at the same
   --  level in the tree and with the same parent).

   function Parse (File : String) return Node_Ptr;
   --  Parse File and return the first node representing the XML file.

   procedure Print (N : Node_Ptr; Indent : Natural := 0);
   --  Simple print procedure. Print the whole tree starting with N.

   function Find_Tag (N : Node_Ptr; Tag : String) return Node_Ptr;
   --  Find a tag Tag in N and its brothers.

   function Get_Field (N : Node_Ptr; Field : String) return String_Ptr;
   --  Return the value of the field 'Field' if present in the children of N.
   --  Return null otherwise.

   function Get_Attribute
     (N : in Node_Ptr;
      Attribute_Name : in String) return String_Ptr;
   --  Return the value of the attribute 'Attribute_Name' if present.
   --  Return null otherwise.

   function Find_Top (N : Node_Ptr) return Node_Ptr;
   -- Given any node in the tree, return the root of the tree.


end Glib.XML;
