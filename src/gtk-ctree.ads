-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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
--  This widget is similar to GtkClist but it displays a tree with expandable
--  nodes instead of a simple list. Gtk_Tree is a more flexible tree widget
--  (it can have arbitrary widgets in the tree cells), but it is less efficient
--  and is limited to 32768 pixels.
--
--  </description>
--  <c_version>1.2.7</c_version>

with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Pixmap;
with Glib; use Glib;
with Glib.Glist;
with Glib.Gnodes;
with Gtk.Object;
with Gtk.Clist;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style;
with Gtkada.Types; use Gtkada.Types;
with Interfaces.C.Strings;
with Unchecked_Conversion;

package Gtk.Ctree is
   pragma Elaborate_Body;

   type Gtk_Ctree_Record is new Gtk.Clist.Gtk_Clist_Record with private;
   type Gtk_Ctree is access all Gtk_Ctree_Record'Class;

   type Gtk_Ctree_Row is new Gtk.Clist.Gtk_Clist_Row;

   type Gtk_Ctree_Node is new Gdk.C_Proxy;
   Null_Ctree_Node : constant Gtk_Ctree_Node := null;

   --  <doc_ignore>
   package Row_List is new Glib.Glist.Generic_List (Gtk_Ctree_Row);

   function Convert is new Unchecked_Conversion
     (Gtk_Ctree_Node, System.Address);
   function Convert is new Unchecked_Conversion
     (System.Address, Gtk_Ctree_Node);
   package Node_List is new Glib.Glist.Generic_List (Gtk_Ctree_Node);
   --  </doc_ignore>

   -----------------------------------
   -- Creation, insertion, deletion --
   -----------------------------------

   procedure Gtk_New (Widget      :    out Gtk_Ctree;
                      Columns     : in     Gint;
                      Tree_Column : in     Gint := 0);
   --  Create a ctree with Columns columns.
   --  Tree_Column indicates in which column the tree will be displayed.

   procedure Initialize (Widget      : access Gtk_Ctree_Record'Class;
                         Columns     : in     Gint;
                         Tree_Column : in     Gint := 0);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gtk_New (Widget      :    out Gtk_Ctree;
                      Titles      : in     Chars_Ptr_Array;
                      Tree_Column : in     Gint := 0);
   --  Create a ctree with Titles'Length columns.
   --  Titles gives the title of each column.
   --  Tree_Column indicates in which column the tree will be displayed.

   procedure Initialize (Widget      : access Gtk_Ctree_Record'Class;
                         Titles      : in     Chars_Ptr_Array;
                         Tree_Column : in     Gint := 0);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Ctree.

   --  <description>
   --  Elements inside a Gtk_Ctree are not ordered from the top to the bottom
   --  as is the case for Gtk_Clist. Instead, they are put in the ctree by
   --  indicating where in the tree they should be placed. The position of an
   --  element (called a node) is defined by a parent node and a sibling node.
   --  The node will be attached in the parent subtree, on top of the sibling
   --  node.
   --  </description>

   function Insert_Node (Ctree         : access Gtk_Ctree_Record;
                         Parent        : in     Gtk_Ctree_Node;
                         Sibling       : in     Gtk_Ctree_Node;
                         Text          : in     Chars_Ptr_Array;
                         Spacing       : in     Guint8;
                         Pixmap_Closed : in     Gdk.Pixmap.Gdk_Pixmap;
                         Mask_Closed   : in     Gdk.Bitmap.Gdk_Bitmap;
                         Pixmap_Opened : in     Gdk.Pixmap.Gdk_Pixmap;
                         Mask_Opened   : in     Gdk.Bitmap.Gdk_Bitmap;
                         Is_Leaf       : in     Boolean;
                         Expanded      : in     Boolean)
                         return                 Gtk_Ctree_Node;
   --  Insert a new node in the Ctree.
   --  Parent is the parent node. If null, the new node is part of the root.
   --  The new node will be inserted right on top of Sibling. If Sibling is
   --  null, then it will be the first node in the subtree.
   --  Text contains the text for each cell of the node. Note that Insert_Node
   --  expects the length of the Text parameter to be equal to the number of
   --  columns of the Ctree.
   --  Spacing is the number of pixels between the lines of the tree and the
   --  text in the same column.
   --  If Is_Leaf is True, then the node won't contain any subtree. If False,
   --  the newly created node can be used as the Parent for further node
   --  creation. In this case, Expanded indicates whether the subtree
   --  associated with this node should be initially visible.
   --  In addition to the "+" or "-" sign indicating whether the subtree is
   --  expanded or not, it is possible to put a pixmap giving this information.
   --  Pixmap_Closed and Mask_Closed represent the image and the mask used when
   --  the subtree is closed; similarly, Pixmap_Opened and Mask_Opened
   --  represent the image and the mask used when the subtree is opened.

   procedure Remove_Node (Ctree : access Gtk_Ctree_Record;
                          Node  : in     Gtk_Ctree_Node);
   --  Remove Node from Ctree.

   -------------------------------------------
   -- Tree, Node and Row basic manipulation --
   -------------------------------------------

   function Get_Tree_Column (Widget : access Gtk.Ctree.Gtk_Ctree_Record'Class)
                             return          Gint;

   function Get_Node_List (Ctree : access Gtk_Ctree_Record)
                          return         Node_List.Glist;
   --   Extract the nodes with Node_List.Get_Gpointer

   function Get_Row_List (Ctree : access Gtk_Ctree_Record)
                          return         Row_List.Glist;

   function Get_Selection (Widget : access Gtk_Ctree_Record)
                          return Node_List.Glist;
   --   Extract the nodes with Node_List.Get_Data

   function Node_Get_Row (Node : in Gtk_Ctree_Node) return Gtk_Ctree_Row;

   function Row_Get_Children (Row : in Gtk_Ctree_Row) return Gtk_Ctree_Node;

   function Row_Get_Expanded (Row : in Gtk_Ctree_Row) return Boolean;
   --  Expanded can also be retrieved via Get_Node_Info
   --  This function is just a quick accessor

   function Row_Get_Is_Leaf (Row : in Gtk_Ctree_Row) return Boolean;
   --  Is_Leaf can also be retrieved via Get_Node_Info
   --  This function is just a quick accessor

   function Row_Get_Parent (Row : in Gtk_Ctree_Row) return Gtk_Ctree_Node;

   function Row_Get_Sibling (Row : in Gtk_Ctree_Row) return Gtk_Ctree_Node;

   function Is_Created (Node : in Gtk_Ctree_Node) return Boolean;
   --  Return True if Node is different from Null_Ctree_Node

   -----------------------------------------
   -- Querying / finding tree information --
   -----------------------------------------

   function Is_Viewable
     (Ctree  : access Gtk_Ctree_Record;
      Node   : in     Gtk_Ctree_Node)
      return Boolean;
   --  Return True if Node is viewable.
   --  A Node is viewable if all the trees and subtrees containing it are
   --  expanded.

   function Last
     (Ctree  : access Gtk_Ctree_Record;
      Node   : in     Gtk_Ctree_Node)
      return Gtk_Ctree_Node;

   function Find_Node_Ptr
     (Ctree     : access Gtk_Ctree_Record;
      Ctree_Row : in     Gtk_Ctree_Row)
      return Gtk_Ctree_Node;

   function Node_Nth (Ctree  : access Gtk_Ctree_Record;
                      Row    : in     Guint)
                      return          Gtk_Ctree_Node;

   function Find (Ctree : access Gtk_Ctree_Record;
                  Node  : in     Gtk_Ctree_Node;
                  Child : in     Gtk_Ctree_Node) return Boolean;

   function Is_Ancestor (Ctree  : access Gtk_Ctree_Record;
                         Node   : in     Gtk_Ctree_Node;
                         Child  : in     Gtk_Ctree_Node)
                         return          Boolean;

   function Is_Hot_Spot (Ctree  : access Gtk_Ctree_Record;
                         X      : in     Gint;
                         Y      : in     Gint)
                         return          Boolean;

   ------------------------------------------------------
   -- Tree signals: move, expand, collapse, (un)select --
   ------------------------------------------------------

   procedure Move (Ctree       : access Gtk_Ctree_Record;
                   Node        : in     Gtk_Ctree_Node;
                   New_Parent  : in     Gtk_Ctree_Node;
                   New_Sibling : in     Gtk_Ctree_Node);
   --  Move a node in a Ctree.
   --  After its creation, a node can be moved.
   --  New_Parent points to the new parent node that will contain Node.
   --  If null, Node will be attached to the root.
   --  New_Sibling indicates under which node Node will be inserted.
   --  If New_Sibling is null, the new node will be the lowest in its branch.

   procedure Expand (Ctree : access Gtk_Ctree_Record;
                     Node  : in     Gtk_Ctree_Node);
   --  Expand the first level of the subtree associated with Node.

   procedure Expand_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null);
   --  Expand the whole subtree associated with Node.

   procedure Expand_To_Depth
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null;
      Depth : in     Gint);
   --  Expand the subtree associated with Node and its descendants until Depth
   --  levels of subtrees have been reached.

   procedure Collapse (Ctree : access Gtk_Ctree_Record;
                       Node  : in     Gtk_Ctree_Node);
   --  Collapse the first level of the subtree associated with Node.

   procedure Collapse_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null);
   --  Collapse the whole subtree associated with Node.

   procedure Collapse_To_Depth
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null;
      Depth : in     Gint);
   --  Collapse the subtree associated with Node and its descendants until
   --  Depth levels of subtrees have been reached.

   procedure Toggle_Expansion (Ctree : access Gtk_Ctree_Record;
                               Node  : in     Gtk_Ctree_Node);
   --  Change the state of the Ctree from expanded to collapsed and the other
   --  way around on one level.

   procedure Toggle_Expansion_Recursive (Ctree : access Gtk_Ctree_Record;
                                         Node  : in     Gtk_Ctree_Node);
   --  Change the state of the Ctree from expanded to collapsed and the other
   --  way around for the whole subtree.

   procedure Gtk_Select (Ctree : access  Gtk_Ctree_Record;
                         Node  : in      Gtk_Ctree_Node);
   --  Select a specified Node, and only this one.

   procedure Select_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null);
   --  Select a specified Node, and its whole subtree.

   procedure Unselect (Ctree : access Gtk_Ctree_Record;
                       Node  : in     Gtk_Ctree_Node);
   --  Unselect a specified Node, and only this one.

   procedure Unselect_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null);
   --  Unselect a specified Node, and its whole subtree.

   procedure Real_Select_Recursive (Ctree : access Gtk_Ctree_Record;
                                    Node  : in     Gtk_Ctree_Node;
                                    State : in     Gint);

   ------------------------------------
   -- Analogs of Gtk_Clist functions --
   ------------------------------------

   procedure Node_Set_Text (Ctree  : access Gtk_Ctree_Record;
                            Node   : in     Gtk_Ctree_Node;
                            Column : in     Gint;
                            Text   : in     String);
   --  Set the cell's text, replacing its current contents.
   --  This changes the type of the cell to Cell_Text. The pixmap (if any)
   --  will no longer be displayed.

   procedure Node_Get_Text (Ctree   : access Gtk_Ctree_Record;
                            Node    : in     Gtk_Ctree_Node;
                            Column  : in     Gint;
                            Text    :    out Interfaces.C.Strings.chars_ptr;
                            Success :    out Boolean);
   --  Return the text contained in cell.
   --  The type of the cell should be either Cell_Text or Cell_Pixtext.
   --  If there was a problem, a null-length string is returned and Success is
   --  set to False.
   --  The problem might appear in case the Node or the Column are
   --  invalid, or if the cell does not contain any text.

   procedure Node_Set_Pixmap (Ctree  : access Gtk_Ctree_Record;
                              Node   : in     Gtk_Ctree_Node;
                              Column : in     Gint;
                              Pixmap : in     Gdk.Pixmap.Gdk_Pixmap;
                              Mask   : in     Gdk.Bitmap.Gdk_Bitmap);
   --  Set the cell's pixmap, replacing its current contents.
   --  The type of the cell becomes Cell_Pixmap, and the text is no longer
   --  displayed.

   procedure Node_Get_Pixmap (Ctree   : access Gtk_Ctree_Record;
                              Node    : in     Gtk_Ctree_Node;
                              Column  : in     Gint;
                              Pixmap  :    out Gdk.Pixmap.Gdk_Pixmap;
                              Mask    :    out Gdk.Bitmap.Gdk_Bitmap;
                              Success :    out Boolean);
   --  Return the Pixmap contained in a cell.
   --  The type of the cell should be Cell_Pixmap.
   --  The result is meaningful only if Success is True. If the Cell did not
   --  contain a pixmap, Success is set to False.

   procedure Node_Set_Pixtext (Ctree   : access Gtk_Ctree_Record;
                               Node    : in     Gtk_Ctree_Node;
                               Column  : in     Gint;
                               Text    : in     String;
                               Spacing : in     Guint8;
                               Pixmap  : in     Gdk.Pixmap.Gdk_Pixmap;
                               Mask    : in     Gdk.Bitmap.Gdk_Bitmap);
   --  Set both the Text and the Pixmap for the cell.
   --  Replace its current contents. The type of the cell becomes Cell_Pixtext,
   --  and both the text and the pixmap are displayed.

   procedure Node_Get_Pixtext (Ctree   : access Gtk_Ctree_Record;
                               Node    : in     Gtk_Ctree_Node;
                               Column  : in     Gint;
                               Text    :    out Interfaces.C.Strings.chars_ptr;
                               Spacing :    out Guint8;
                               Pixmap  :    out Gdk.Pixmap.Gdk_Pixmap;
                               Mask    :    out Gdk.Bitmap.Gdk_Bitmap;
                               Success :    out Boolean);
   --  Return the Text and the Pixmap for the cell.
   --  The result is not meaningful if Success is False.

   procedure Node_Set_Shift (Ctree      : access Gtk_Ctree_Record;
                             Node       : in     Gtk_Ctree_Node;
                             Column     : in     Gint;
                             Vertical   : in     Gint;
                             Horizontal : in     Gint);
   --  Set a horizontal and vertical shift for drawing the content of the cell.
   --  Both shifts can be either positive or negative.
   --  This is particularly useful for indenting items in a columns.

   procedure Set_Node_Info (Ctree         : access Gtk_Ctree_Record;
                            Node          : in     Gtk_Ctree_Node;
                            Text          : in     String;
                            Spacing       : in     Guint8;
                            Pixmap_Closed : in     Gdk.Pixmap.Gdk_Pixmap;
                            Mask_Closed   : in     Gdk.Bitmap.Gdk_Bitmap;
                            Pixmap_Opened : in     Gdk.Pixmap.Gdk_Pixmap;
                            Mask_Opened   : in     Gdk.Bitmap.Gdk_Bitmap;
                            Is_Leaf       : in     Boolean;
                            Expanded      : in     Boolean);

   procedure Get_Node_Info
     (Ctree         : access Gtk_Ctree_Record;
      Node          : in     Gtk_Ctree_Node;
      Text          :    out Interfaces.C.Strings.chars_ptr;
      Spacing       :    out Guint8;
      Pixmap_Closed :    out Gdk.Pixmap.Gdk_Pixmap;
      Mask_Closed   :    out Gdk.Bitmap.Gdk_Bitmap;
      Pixmap_Opened :    out Gdk.Pixmap.Gdk_Pixmap;
      Mask_Opened   :    out Gdk.Bitmap.Gdk_Bitmap;
      Is_Leaf       :    out Boolean;
      Expanded      :    out Boolean;
      Success       :    out Boolean);

   procedure Node_Set_Selectable (Ctree      : access Gtk_Ctree_Record;
                                  Node       : in     Gtk_Ctree_Node;
                                  Selectable : in     Boolean := True);
   --  Indicate whether the Node can be selected or not.
   --  The default value is True.

   function Node_Get_Selectable (Ctree  : access Gtk_Ctree_Record;
                                 Node   : in     Gtk_Ctree_Node)
                                 return          Boolean;
   --  Return the selectable status of the Node.

   procedure Node_Set_Row_Style (Ctree : access Gtk_Ctree_Record;
                                 Node  : in     Gtk_Ctree_Node;
                                 Style : in     Gtk.Style.Gtk_Style);
   --  Set the default style for the cells in the Node.
   --  This can be overriden for each cell with Node_Set_Cell_Style.

   function Node_Get_Row_Style (Ctree  : access Gtk_Ctree_Record;
                                Node   : in     Gtk_Ctree_Node)
                                return          Gtk.Style.Gtk_Style;
   --  Return the default style used for the Node.

   procedure Node_Set_Cell_Style (Ctree  : access Gtk_Ctree_Record;
                                  Node   : in     Gtk_Ctree_Node;
                                  Column : in     Gint;
                                  Style  : in     Gtk.Style.Gtk_Style);
   --  Set the style (font, color, ...) used for the cell.
   --  This overrides the Node's style.

   function Node_Get_Cell_Style (Ctree  : access Gtk_Ctree_Record;
                                 Node   : in     Gtk_Ctree_Node;
                                 Column : in     Gint)
                                 return          Gtk.Style.Gtk_Style;
   --  Return the style of the cell.

   procedure Node_Set_Foreground (Ctree : access Gtk_Ctree_Record;
                                  Node  : in     Gtk_Ctree_Node;
                                  Color : in     Gdk.Color.Gdk_Color);
   --  Set the foreground color for the Node.
   --  The color must already be allocated.
   --  If no such Node exists in the tree, nothing is done.

   procedure Node_Set_Background (Ctree : access Gtk_Ctree_Record;
                                  Node  : in     Gtk_Ctree_Node;
                                  Color : in     Gdk.Color.Gdk_Color);
   --  Set the background color for the Node.
   --  The color must already be allocated.
   --  If no such Node exists in the tree, nothing is done.

   function Node_Get_Cell_Type (Ctree  : access Gtk_Ctree_Record;
                                Node   : in     Gtk_Ctree_Node;
                                Column : in     Gint)
                                return          Gtk_Cell_Type;
   --  Return the type of the cell at Node/Column.
   --  This indicates which of the functions Node_Get_Text. Node_Get_Pixmap,
   --  etc. should be used with this cell.

   procedure Node_Moveto (Ctree     : access Gtk_Ctree_Record;
                          Node      : in     Gtk_Ctree_Node;
                          Column    : in     Gint;
                          Row_Align : in     Gfloat := 0.5;
                          Col_Align : in     Gfloat := 0.5);
   --  Make a Node visible.
   --  Column indicates which column of the Node should be visible, if not
   --  all columns can be displayed.
   --  Row_Align and Col_Align are parameters between 0.0 and 1.0, and
   --  specify how the Node and the Column will be centered in the Ctree
   --  window. 0.0 means a Node on the top, and a Column on the left.

   function Node_Is_Visible
     (Ctree  : access Gtk_Ctree_Record;
      Node   : in     Gtk_Ctree_Node)
      return Gtk_Visibility;
   --  Indicate the visibility of a Node.
   --  Return Visibility_None if the Node is not visible in the Ctree window;
   --  Visibility_Partial if the Node is partially visible; Visibility_Full
   --  if the Node is entirely visible.
   --  This function ignores the fact that Node is in an expanded or collapsed
   --  subtree.

   ------------------------------
   -- Ctree specific functions --
   ------------------------------

   procedure Set_Indent (Ctree  : access Gtk_Ctree_Record;
                         Indent : in     Gint);

   function Get_Indent (Widget : access Gtk.Ctree.Gtk_Ctree_Record'Class)
     return Gint;

   procedure Set_Spacing (Ctree   : access Gtk_Ctree_Record;
                          Spacing : in     Gint);

   function Get_Spacing (Widget : access Gtk.Ctree.Gtk_Ctree_Record'Class)
     return Gint;

   procedure Set_Show_Stub (Ctree     : access Gtk_Ctree_Record;
                            Show_Stub : in     Boolean);

   function Get_Show_Stub (Ctree : access Gtk_Ctree_Record) return Boolean;

   procedure Set_Line_Style (Ctree      : access Gtk_Ctree_Record;
                             Line_Style : in     Gtk_Ctree_Line_Style);

   function Get_Line_Style (Ctree : access Gtk_Ctree_Record)
                            return         Gtk_Ctree_Line_Style;

   procedure Set_Expander_Style
     (Ctree          : access Gtk_Ctree_Record;
      Expander_Style : in     Gtk_Ctree_Expander_Style);

   function Get_Expander_Style (Ctree : access Gtk_Ctree_Record)
                                return         Gtk_Ctree_Expander_Style;

   type Gtk_Ctree_Compare_Drag_Func is access
     function (Ctree        : in Gtk_Ctree;
               Source_Node  : in Gtk_Ctree_Node;
               New_Parent   : in Gtk_Ctree_Node;
               New_Sibling  : in Gtk_Ctree_Node) return Boolean;

   procedure Set_Drag_Compare_Func
     (Ctree    : access Gtk_Ctree_Record;
      Cmp_Func : in     Gtk_Ctree_Compare_Drag_Func);

   ----------------------------
   -- Tree sorting functions --
   ----------------------------

   procedure Sort_Node (Ctree : access Gtk_Ctree_Record;
                        Node  : in     Gtk_Ctree_Node);

   procedure Sort_Recursive (Ctree : access Gtk_Ctree_Record;
                             Node  : in     Gtk_Ctree_Node);

   --------------------------
   -- Ctree_Gnode handling --
   --------------------------

   generic
      type Data_Type (<>) is private;
   package Ctree_Gnode is

      type Data_Type_Access is access all Data_Type;

      type Gtk_Ctree_Gnode_Func is access
        function (Ctree : access Gtk_Ctree_Record'Class;
                  Depth : in     Guint;
                  Gnode : in     Glib.Gnodes.Gnode;
                  Cnode : in     Gtk_Ctree_Node;
                  Data  : in     Data_Type_Access) return Boolean;


      function Export_To_Gnode (Ctree   : access Gtk_Ctree_Record'Class;
                                Parent  : in     Glib.Gnodes.Gnode;
                                Sibling : in     Glib.Gnodes.Gnode;
                                Node    : in     Gtk_Ctree_Node;
                                Func    : in     Gtk_Ctree_Gnode_Func;
                                Data    : in     Data_Type_Access)
        return Glib.Gnodes.Gnode;

      function Insert_Gnode (Ctree   : access Gtk_Ctree_Record'Class;
                             Parent  : in     Glib.Gnodes.Gnode;
                             Sibling : in     Glib.Gnodes.Gnode;
                             Node    : in     Gtk_Ctree_Node;
                             Func    : in     Gtk_Ctree_Gnode_Func;
                             Data    : in     Data_Type_Access)
        return Gtk_Ctree_Node;

   end Ctree_Gnode;

   -----------------------
   -- Row_Data handling --
   -----------------------

   generic
      type Data_Type (<>) is private;
   package Row_Data is

      type Data_Type_Access is access all Data_Type;

      procedure Node_Set_Row_Data (Ctree : access Gtk_Ctree_Record'Class;
                                   Node  : in     Gtk_Ctree_Node;
                                   Data  : in     Data_Type);
      --  Associate a Data with a Node.

      function Node_Get_Row_Data
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node)
         return Data_Type;
      --  Retrieve a data associated with a Node.
      --  Error Handling:
      --  Gtkada.Types.Data_Error is raised when trying to retrieve
      --  the data from a Node for which no data has been set
      --  (using Node_Set_Row_Data).

      function Find_By_Row_Data
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node;
         Data  : in     Data_Type)
         return Gtk_Ctree_Node;
      --  Find the first node containing a specified Data.
      --  Node is the starting point of the search. If null, the search will
      --  start from the root.
      --  Return the first Node whose associated data is Data, null if none
      --  can be found.

      function Find_All_By_Row_Data
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node;
         Data  : in     Data_Type)
         return Node_List.Glist;
      --  Find all nodes containing a specified Data.
      --  Node is the starting point of the search. If null, the search will
      --  start from the root.

      type Gcompare_Func is access
        function (A, B : in Data_Type) return Boolean;
      --  Function used to compare data types in Find_By_Row_Data_Custom and
      --  Find_All_By_Row_Data_Custom.

      function Find_By_Row_Data_Custom
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node;
         Data  : in     Data_Type;
         Func  : in     Gcompare_Func)
         return Gtk_Ctree_Node;
      --  Find the first node containing a specified Data.
      --  Similar to Find_By_Row_Data but Func is used to allow a more flexible
      --  (user defined) method to compare two nodes.

      function Find_All_By_Row_Data_Custom
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node;
         Data  : in     Data_Type;
         Func  : in     Gcompare_Func) return Node_List.Glist;
      --  Find all the nodes containing a specified Data.
      --  Similar to Find_All_By_Row_Data but Func is used to allow a more
      --  flexible (user defined) method to compare two nodes.

      type Gtk_Ctree_Func is access
        procedure (Ctree : access Gtk_Ctree_Record'Class;
                   Node  : in     Gtk_Ctree_Node;
                   Data  : in     Data_Type_Access);
      --  Function used by Post/Pre_Recursive functions below.

      procedure Post_Recursive (Ctree : access Gtk_Ctree_Record'Class;
                                Node  : in     Gtk_Ctree_Node;
                                Func  : in     Gtk_Ctree_Func;
                                Data  : in     Data_Type_Access);
      --  Apply Func to each node of a subtree.
      --  Node designates the root of the subtree.
      --  Data will be passed as a parameter to Func.
      --  This procedure will first apply Func to the children nodes.

      procedure Post_Recursive_To_Depth (Ctree : access Gtk_Ctree_Record'Class;
                                         Node  : in     Gtk_Ctree_Node;
                                         Depth : in     Gint;
                                         Func  : in     Gtk_Ctree_Func;
                                         Data  : in     Data_Type_Access);
      --  Apply Func to each node of a subtree until a specified Depth.
      --  Node designates the root of the subtree.
      --  Data will be passed as a parameter to Func.
      --  This function is similar to Post_Recursive except that it
      --  stop at a specified subtree depth.

      procedure Pre_Recursive (Ctree : access Gtk_Ctree_Record'Class;
                               Node  : in     Gtk_Ctree_Node;
                               Func  : in     Gtk_Ctree_Func;
                               Data  : in     Data_Type_Access);
      --  Apply Func to each node of a subtree.
      --  Similar to Post_Recursive but will apply Func to the parent before
      --  applying it to its children.

      procedure Pre_Recursive_To_Depth (Ctree : access Gtk_Ctree_Record'Class;
                                        Node  : in     Gtk_Ctree_Node;
                                        Depth : in     Gint;
                                        Func  : in     Gtk_Ctree_Func;
                                        Data  : in     Data_Type_Access);
      --  Apply Func to each node of a subtree until a specific Depth.
      --  Similar to Post_Recursive_To_Depth but will apply Func to the parent
      --  before applying it to its children.


   private

      --  <doc_ignore>
      function Default_Gcompare_Func (A, B : in Data_Type) return Boolean;
      --
      --  This function needs to be declared in the spec, although it is
      --  only used in the body. Otherwise, the compiler does not allow
      --  to apply the 'Access attribute to it.
      --  </doc_ignore>

   end Row_Data;
   --
   --  The previous package implements the Row_Data stuff.
   --  !! Warning !! No type verification is made to check if you are
   --  using the appropriate function Get. This is your own responsability

   ----------------------------
   -- Support for Gate/Dgate --
   ----------------------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);
   --  Gate internal function

   procedure Generate (Ctree : in out Gtk.Object.Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "tree_select_row"
   --    procedure Handler (Ctree  : access Gtk_Ctree_Record'Class;
   --                       Node   : Gtk_Ctree_Node;
   --                       Column : Gint);
   --
   --    Emitted to request the selection of a node.
   --    Column is the column number where the user clicked.
   --
   --  - "tree_unselect_row"
   --    procedure Handler (Ctree  : access Gtk_Ctree_Record'Class;
   --                       Node   : Gtk_Ctree_Node;
   --                       Column : Gint);
   --
   --    Emitted to request the unselection of a node.
   --
   --  - "tree_expand"
   --    procedure Handler (Ctree  : access Gtk_Clist_Record'Class;
   --                       Node   : Gtk_Ctree_Node);
   --
   --    Emitted when the subtree associated with a Node is expanded.
   --
   --  - "tree_collapse"
   --    procedure Handler (Ctree  : access Gtk_Clist_Record'Class;
   --                       Node   : Gtk_Ctree_Node);
   --
   --    Emitted when the subtree associated with a Node is collapsed.
   --
   --  - "tree_move"
   --    procedure Handler (Ctree       : access Gtk_Clist_Record'Class;
   --                       Node        : Gtk_Ctree_Node);
   --                       New_Parent  : Gtk_Ctree_Node);
   --                       New_Sibling : Gtk_Ctree_Node);
   --
   --    Emitted when a Node is moved (e.g its parent and/or its sibling
   --    changed).
   --
   --  </signals>
private

   type Gtk_Ctree_Record is new Gtk.Clist.Gtk_Clist_Record with null record;

   pragma Import (C, Get_Type, "gtk_ctree_get_type");
   pragma Import (C, Node_Get_Row, "ada_ctree_node_get_row");
   pragma Import (C, Row_Get_Children, "ada_ctree_row_get_children");
   pragma Import (C, Row_Get_Parent, "ada_ctree_row_get_parent");
   pragma Import (C, Row_Get_Sibling, "ada_ctree_row_get_sibling");

end Gtk.Ctree;
