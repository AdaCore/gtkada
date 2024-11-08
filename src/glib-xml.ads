------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--
--  This package provides a simple minded XML parser to be used with
--  Gate.
--
--  </description>
--  <group>Glib, the general-purpose library</group>

with Ada.Unchecked_Deallocation;

generic
   type XML_Specific_Data is private;
   --  The type of the extra data that can be attached to each node of the
   --  XML tree. See for instance the package Glib.Glade.

package Glib.XML is

   --  <doc_ignore>
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Ptr);
   --  </doc_ignore>

   type Node;
   type Node_Ptr is access all Node;
   --  Pointer to a node of the XML tree.

   type Node is record
      Tag   : String_Ptr;
      --  The name of this node. This is utf8-encoded

      Attributes   : String_Ptr;
      --  The attributes of this node. This is utf8-encoded

      Value : String_Ptr;
      --  The value, or null is not relevant. This is utf8-encoded

      Parent : Node_Ptr;
      --  The parent of this Node.

      Child : Node_Ptr;
      --  The first Child of this Node. The next child is Child.Next

      Next  : Node_Ptr;
      --  Next sibling node.

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

   function Parse_Buffer (Buffer : UTF8_String) return Node_Ptr;
   --  Parse a given Buffer in memory and return the first node representing
   --  the XML contents.

   procedure Print (N : Node_Ptr; File_Name : String := "");
   --  Write the tree starting with N into a file File_Name. The generated
   --  file is valid XML, and can be parsed with the Parse function.
   --  If File_Name is the empty string, then the tree is printed on the
   --  standard output

   procedure Print
     (N         : Node_Ptr;
      File_Name : String;
      Success   : out Boolean);
   --  Same as above, with Success reporting the success of the operation.

   function Protect (S : String) return String;
   --  Return a copy of S modified so that it is a valid XML value

   function Find_Tag (N : Node_Ptr; Tag : UTF8_String) return Node_Ptr;
   --  Find a tag Tag in N and its brothers.

   function Get_Field (N : Node_Ptr; Field : UTF8_String) return String_Ptr;
   --  Return the value of the field 'Field' if present in the children of N.
   --  Return null otherwise.
   --  Do not free the returned value.

   function Is_Equal (Node1, Node2 : Node_Ptr) return Boolean;
   --  Compare two XML nodes recursively, and returns True if they are equal.
   --  Casing in attributes is relevant. Order of attributes is also
   --  relevant.

   procedure Add_Child
     (N : Node_Ptr; Child : Node_Ptr; Append : Boolean := False);
   --  Add a new child to a node.
   --  If Append is true, the child is added at the end of the current list of
   --  children.

   function Children_Count (N : Node_Ptr) return Natural;
   --  Return the number of child nodes

   function Deep_Copy (N : Node_Ptr) return Node_Ptr;
   --  Return a deep copy of the tree starting with N. N can then be freed
   --  without affecting the copy.

   type Free_Specific_Data is access
     procedure (Data : in out XML_Specific_Data);

   procedure Free
     (N : in out Node_Ptr; Free_Data : Free_Specific_Data := null);
   --  Free the memory allocated for a node and its children.
   --  It also disconnects N from its parent.
   --  If Free_Data is not null, it is used to free the memory occupied by
   --  the Specific_Data for each node.

   function Get_Attribute
     (N              : Node_Ptr;
      Attribute_Name : UTF8_String;
      Default        : UTF8_String := "") return UTF8_String;
   --  Return the value of the attribute 'Attribute_Name' if present.
   --  Special XML characters have already been interpreted in the result
   --  string.
   --  Return Default otherwise.

   procedure Set_Attribute
     (N : Node_Ptr; Attribute_Name, Attribute_Value : UTF8_String);
   --  Create a new attribute, or replace an existing one. The attribute value
   --  is automatically protected for special XML characters

   function Find_Tag_With_Attribute
     (N     : Node_Ptr;
      Tag   : UTF8_String;
      Key   : UTF8_String;
      Value : UTF8_String := "")
     return Node_Ptr;
   --  Find a tag Tag in N that has a given key (and value if given).

end Glib.XML;
