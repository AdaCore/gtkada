-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

package Gtk.Ctree is
   pragma Elaborate_Body;

   type Gtk_Ctree_Record is new Gtk.Clist.Gtk_Clist_Record with private;
   type Gtk_Ctree is access all Gtk_Ctree_Record'Class;

   type Gtk_Ctree_Row is new Gtk.Clist.Gtk_Clist_Row;

   type Gtk_Ctree_Node is new Object_Type;
   Null_Ctree_Node : constant Gtk_Ctree_Node;

   package Row_List is new Glib.Glist.Generic_List (Gtk_Ctree_Row);

   function Convert (C : in Gtk_Ctree_Node) return System.Address;
   function Convert (W : System.Address) return Gtk_Ctree_Node;
   package Node_List is new Glib.Glist.Generic_List (Gtk_Ctree_Node);
   --
   --  Note: To extract the Gtk_Ctree_Node, use Node_List.Get_Gpointer,
   --        not Node_List.Get_Data.


   type Gtk_Ctree_Compare_Drag_Func is access
     function (Ctree        : in Gtk_Ctree;
               Source_Node  : in Gtk_Ctree_Node;
               New_Parent   : in Gtk_Ctree_Node;
               New_Sibling  : in Gtk_Ctree_Node) return Boolean;


   procedure Collapse (Ctree : access Gtk_Ctree_Record;
                       Node  : in     Gtk_Ctree_Node);

   procedure Collapse_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := Null_Ctree_Node);

   procedure Collapse_To_Depth
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := Null_Ctree_Node;
      Depth : in     Gint);

   procedure Expand (Ctree : access Gtk_Ctree_Record;
                     Node  : in     Gtk_Ctree_Node);

   procedure Expand_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := Null_Ctree_Node);

   procedure Expand_To_Depth
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := Null_Ctree_Node;
      Depth : in     Gint);

   function Find (Ctree : access Gtk_Ctree_Record;
                  Node  : in     Gtk_Ctree_Node;
                  Child : in     Gtk_Ctree_Node) return Boolean;

   function Find_Node_Ptr (Ctree     : access Gtk_Ctree_Record;
                           Ctree_Row : in     Gtk_Ctree_Row)
     return Gtk_Ctree_Node;

   function Get_Expander_Style (Ctree : access Gtk_Ctree_Record)
                                return         Gtk_Ctree_Expander_Style;

   function Get_Line_Style (Ctree : access Gtk_Ctree_Record)
                            return         Gtk_Ctree_Line_Style;

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

   function Get_Node_List (Ctree : access Gtk_Ctree_Record)
                           return         Node_List.Glist;

   function Get_Row_List (Ctree : access Gtk_Ctree_Record)
                          return         Row_List.Glist;

   function Get_Selection (Widget : access Gtk_Ctree_Record)
                           return Node_List.Glist;

   function Get_Show_Stub (Ctree : access Gtk_Ctree_Record) return Boolean;

   function Get_Tree_Column (Widget : access Gtk.Ctree.Gtk_Ctree_Record'Class)
                             return          Gint;

   function Get_Tree_Indent (Widget : access Gtk.Ctree.Gtk_Ctree_Record'Class)
                             return          Gint;

   function Get_Tree_Spacing (Widget : access Gtk.Ctree.Gtk_Ctree_Record'Class)
                              return          Gint;

   procedure Gtk_New (Widget      :    out Gtk_Ctree;
                      Titles      : in     Chars_Ptr_Array;
                      Tree_Column : in     Gint := 0);
   procedure Initialize (Widget      : access Gtk_Ctree_Record'Class;
                         Titles      : in     Chars_Ptr_Array;
                         Tree_Column : in     Gint := 0);

   procedure Gtk_New (Widget      :    out Gtk_Ctree;
                      Columns     : in     Gint;
                      Tree_Column : in     Gint := 0);
   procedure Initialize (Widget      : access Gtk_Ctree_Record'Class;
                         Columns     : in     Gint;
                         Tree_Column : in     Gint := 0);

   procedure Gtk_Select (Ctree : access  Gtk_Ctree_Record;
                         Node  : in      Gtk_Ctree_Node);

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
   --  Note: Insert_Node expects the length of the Text parameter
   --        to be equal to the number of columns of the Ctree...

   function Is_Ancestor (Ctree  : access Gtk_Ctree_Record;
                         Node   : in     Gtk_Ctree_Node;
                         Child  : in     Gtk_Ctree_Node)
                         return          Boolean;

   function Is_Hot_Spot (Ctree  : access Gtk_Ctree_Record;
                         X      : in     Gint;
                         Y      : in     Gint)
                         return          Boolean;

   function Is_Viewable (Ctree  : access Gtk_Ctree_Record;
                         Node   : in     Gtk_Ctree_Node)
                         return          Boolean;

   function Last (Ctree  : access Gtk_Ctree_Record;
                  Node   : in     Gtk_Ctree_Node)
                  return          Gtk_Ctree_Node;

   procedure Move (Ctree       : access Gtk_Ctree_Record;
                   Node        : in     Gtk_Ctree_Node;
                   New_Parent  : in     Gtk_Ctree_Node;
                   New_Sibling : in     Gtk_Ctree_Node);

   function Node_Get_Cell_Style (Ctree  : access Gtk_Ctree_Record;
                                 Node   : in     Gtk_Ctree_Node;
                                 Column : in     Gint)
                                 return          Gtk.Style.Gtk_Style;

   function Node_Get_Cell_Type (Ctree  : access Gtk_Ctree_Record;
                                Node   : in     Gtk_Ctree_Node;
                                Column : in     Gint)
                                return          Gtk_Cell_Type;

   procedure Node_Get_Pixmap (Ctree   : access Gtk_Ctree_Record;
                              Node    : in     Gtk_Ctree_Node;
                              Column  : in     Gint;
                              Pixmap  :    out Gdk.Pixmap.Gdk_Pixmap;
                              Mask    :    out Gdk.Bitmap.Gdk_Bitmap;
                              Success :    out Boolean);

   procedure Node_Get_Pixtext (Ctree   : access Gtk_Ctree_Record;
                               Node    : in     Gtk_Ctree_Node;
                               Column  : in     Gint;
                               Text    :    out Interfaces.C.Strings.chars_ptr;
                               Spacing :    out Guint8;
                               Pixmap  :    out Gdk.Pixmap.Gdk_Pixmap;
                               Mask    :    out Gdk.Bitmap.Gdk_Bitmap;
                               Success :    out Boolean);

   function Node_Get_Row (Node : in Gtk_Ctree_Node) return Gtk_Ctree_Row;

   function Node_Get_Row_Style (Ctree  : access Gtk_Ctree_Record;
                                Node   : in     Gtk_Ctree_Node)
                                return          Gtk.Style.Gtk_Style;

   function Node_Get_Selectable (Ctree  : access Gtk_Ctree_Record;
                                 Node   : in     Gtk_Ctree_Node)
                                 return          Boolean;

   procedure Node_Get_Text (Ctree   : access Gtk_Ctree_Record;
                            Node    : in     Gtk_Ctree_Node;
                            Column  : in     Gint;
                            Text    :    out Interfaces.C.Strings.chars_ptr;
                            Success :    out Boolean);

   function Node_Is_Visible (Ctree  : access Gtk_Ctree_Record;
                             Node   : in     Gtk_Ctree_Node)
                             return          Gtk_Visibility;

   procedure Node_Moveto (Ctree     : access Gtk_Ctree_Record;
                          Node      : in     Gtk_Ctree_Node;
                          Column    : in     Gint;
                          Row_Align : in     Gfloat;
                          Col_Align : in     Gfloat);

   function Node_Nth (Ctree  : access Gtk_Ctree_Record;
                      Row    : in     Guint)
                      return          Gtk_Ctree_Node;

   procedure Node_Set_Background (Ctree : access Gtk_Ctree_Record;
                                  Node  : in     Gtk_Ctree_Node;
                                  Color : in     Gdk.Color.Gdk_Color);

   procedure Node_Set_Cell_Style (Ctree  : access Gtk_Ctree_Record;
                                  Node   : in     Gtk_Ctree_Node;
                                  Column : in     Gint;
                                  Style  : in     Gtk.Style.Gtk_Style);

   procedure Node_Set_Foreground (Ctree : access Gtk_Ctree_Record;
                                  Node  : in     Gtk_Ctree_Node;
                                  Color : in     Gdk.Color.Gdk_Color);

   procedure Node_Set_Pixmap (Ctree  : access Gtk_Ctree_Record;
                              Node   : in     Gtk_Ctree_Node;
                              Column : in     Gint;
                              Pixmap : in     Gdk.Pixmap.Gdk_Pixmap;
                              Mask   : in     Gdk.Bitmap.Gdk_Bitmap);

   procedure Node_Set_Pixtext (Ctree   : access Gtk_Ctree_Record;
                               Node    : in     Gtk_Ctree_Node;
                               Column  : in     Gint;
                               Text    : in     String;
                               Spacing : in     Guint8;
                               Pixmap  : in     Gdk.Pixmap.Gdk_Pixmap;
                               Mask    : in     Gdk.Bitmap.Gdk_Bitmap);

   procedure Node_Set_Row_Style (Ctree : access Gtk_Ctree_Record;
                                 Node  : in     Gtk_Ctree_Node;
                                 Style : in     Gtk.Style.Gtk_Style);

   procedure Node_Set_Selectable (Ctree      : access Gtk_Ctree_Record;
                                  Node       : in     Gtk_Ctree_Node;
                                  Selectable : in     Boolean);

   procedure Node_Set_Shift (Ctree      : access Gtk_Ctree_Record;
                             Node       : in     Gtk_Ctree_Node;
                             Column     : in     Gint;
                             Vertical   : in     Gint;
                             Horizontal : in     Gint);

   procedure Node_Set_Text (Ctree  : access Gtk_Ctree_Record;
                            Node   : in     Gtk_Ctree_Node;
                            Column : in     Gint;
                            Text   : in     String);

   procedure Real_Select_Recursive (Ctree : access Gtk_Ctree_Record;
                                    Node  : in     Gtk_Ctree_Node;
                                    State : in     Gint);

   procedure Remove_Node (Ctree : access Gtk_Ctree_Record;
                          Node  : in     Gtk_Ctree_Node);

   function Row_Get_Children (Row : in Gtk_Ctree_Row) return Gtk_Ctree_Node;

   function Row_Get_Expanded (Row : in Gtk_Ctree_Row) return Boolean;
   --  Expanded can also be retrieved via Get_Node_Info
   --  This function is just a quick accessor

   function Row_Get_Is_Leaf (Row : in Gtk_Ctree_Row) return Boolean;
   --  Is_Leaf can also be retrieved via Get_Node_Info
   --  This function is just a quick accessor

   function Row_Get_Parent (Row : in Gtk_Ctree_Row) return Gtk_Ctree_Node;

   function Row_Get_Sibling (Row : in Gtk_Ctree_Row) return Gtk_Ctree_Node;

   procedure Select_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := Null_Ctree_Node);

   procedure Set_Compare_Drag_Func
     (Ctree    : access Gtk_Ctree_Record;
      Cmp_Func : in     Gtk_Ctree_Compare_Drag_Func);

   procedure Set_Expander_Style
     (Ctree          : access Gtk_Ctree_Record;
      Expander_Style : in     Gtk_Ctree_Expander_Style);

   procedure Set_Indent (Ctree  : access Gtk_Ctree_Record;
                         Indent : in     Gint);

   procedure Set_Line_Style (Ctree      : access Gtk_Ctree_Record;
                             Line_Style : in     Gtk_Ctree_Line_Style);

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

   procedure Set_Show_Stub (Ctree     : access Gtk_Ctree_Record;
                            Show_Stub : in     Boolean);

   procedure Set_Spacing (Ctree   : access Gtk_Ctree_Record;
                          Spacing : in     Gint);

   procedure Sort_Node (Ctree : access Gtk_Ctree_Record;
                        Node  : in     Gtk_Ctree_Node);

   procedure Sort_Recursive (Ctree : access Gtk_Ctree_Record;
                             Node  : in     Gtk_Ctree_Node);

   procedure Toggle_Expansion (Ctree : access Gtk_Ctree_Record;
                               Node  : in     Gtk_Ctree_Node);

   procedure Toggle_Expansion_Recursive (Ctree : access Gtk_Ctree_Record;
                                         Node  : in     Gtk_Ctree_Node);

   procedure Unselect (Ctree : access Gtk_Ctree_Record;
                       Node  : in     Gtk_Ctree_Node);

   procedure Unselect_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := Null_Ctree_Node);


   -------------------
   --  Ctree_Gnode  --
   -------------------

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


   ---------------
   -- Row_Data --
   ---------------

   generic
      type Data_Type (<>) is private;
   package Row_Data is

      type Data_Type_Access is access all Data_Type;

      type Gtk_Ctree_Func is access
        procedure (Ctree : access Gtk_Ctree_Record'Class;
                   Node  : in     Gtk_Ctree_Node;
                   Data  : in     Data_Type_Access);

      type Gcompare_Func is access
        function (A, B : in Data_Type) return Boolean;

      function Find_By_Row_Data (Ctree : access Gtk_Ctree_Record'Class;
                                 Node  : in     Gtk_Ctree_Node;
                                 Data  : in     Data_Type)
        return Gtk_Ctree_Node;
      --
      --  Note : This function does not map 'gtk_ctree_find_by_row_data'.
      --         Instead, it is using Find_By_Row_Data_Custom with the
      --         comparison function being the Data_Type equality operator.


      function Find_All_By_Row_Data (Ctree : access Gtk_Ctree_Record'Class;
                                     Node  : in     Gtk_Ctree_Node;
                                     Data  : in     Data_Type)
        return Node_List.Glist;
      --
      --  Note : This function does not map 'gtk_ctree_find_all_by_row_data'.
      --         Instead, it is using Find_All_By_Row_Data_Custom with the
      --         comparison function being the Data_Type equality operator.

      function Find_By_Row_Data_Custom (Ctree : access Gtk_Ctree_Record'Class;
                                        Node  : in     Gtk_Ctree_Node;
                                        Data  : in     Data_Type;
                                        Func  : in     Gcompare_Func)
        return Gtk_Ctree_Node;

      function Find_All_By_Row_Data_Custom
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node;
         Data  : in     Data_Type;
         Func  : in     Gcompare_Func) return Node_List.Glist;

      function Node_Get_Row_Data (Ctree : access Gtk_Ctree_Record'Class;
                                  Node  : in     Gtk_Ctree_Node)
        return Data_Type;
      --
      --  Error Handling:
      --  Gtkada.Types.Data_Error is raised when trying to retrieve
      --  the data from a Node for which no data has been set
      --  (use Node_Set_Row_Data).

      procedure Node_Set_Row_Data (Ctree : access Gtk_Ctree_Record'Class;
                                   Node  : in     Gtk_Ctree_Node;
                                   Data  : in     Data_Type);
      --  maps gtk_ctree_node_set_row_data_full

      procedure Post_Recursive (Ctree : access Gtk_Ctree_Record'Class;
                                Node  : in     Gtk_Ctree_Node;
                                Func  : in     Gtk_Ctree_Func;
                                Data  : in     Data_Type_Access);

      procedure Post_Recursive_To_Depth (Ctree : access Gtk_Ctree_Record'Class;
                                         Node  : in     Gtk_Ctree_Node;
                                         Depth : in     Gint;
                                         Func  : in     Gtk_Ctree_Func;
                                         Data  : in     Data_Type_Access);

      procedure Pre_Recursive (Ctree : access Gtk_Ctree_Record'Class;
                               Node  : in     Gtk_Ctree_Node;
                               Func  : in     Gtk_Ctree_Func;
                               Data  : in     Data_Type_Access);

      procedure Pre_Recursive_To_Depth (Ctree : access Gtk_Ctree_Record'Class;
                                        Node  : in     Gtk_Ctree_Node;
                                        Depth : in     Gint;
                                        Func  : in     Gtk_Ctree_Func;
                                        Data  : in     Data_Type_Access);


   private

      function Default_Gcompare_Func (A, B : in Data_Type) return Boolean;
      --
      --  This function needs to be declared in the spec, although it is
      --  only used in the body. Otherwise, the compiler does not allow
      --  to apply the 'Access attribute to it.

   end Row_Data;
   --
   --  The previous package implements the Row_Data stuff.
   --  !! Warning !! No type verification is made to check if you are
   --  using the appropriate function Get. This is your own responsability

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);
   --  Gate internal function
 
   procedure Generate (Ctree : in out Gtk.Object.Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function

private

   type Gtk_Ctree_Record is new Gtk.Clist.Gtk_Clist_Record with null record;

   Null_Ctree_Node : constant Gtk_Ctree_Node :=
     Gtk_Ctree_Node (Null_Object_Type);

end Gtk.Ctree;
