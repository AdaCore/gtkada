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

with Glib;                use Glib;
with Gdk;                 use Gdk;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Label;           use Gtk.Label;
with Gtk.Radio_Button;    use Gtk.Radio_Button;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Spin_Button;     use Gtk.Spin_Button;
with Gtk.Tree;            use Gtk.Tree;
with Gtk.Tree_Item;       use Gtk.Tree_Item;
with Gtk.Widget;          use Gtk.Widget;
with Gtk;                 use Gtk;
with Common;              use Common;

package body Create_Tree is

   Default_Number_Of_Item  : Gfloat := 3.0;
   Default_Recursion_Level : Gfloat := 3.0;

   Tree_Area : Gtk_Box;

   --  This is the mapping for sTreeSampleSelection
   Single_Button       : Gtk_Radio_Button;
   Browse_Button       : Gtk_Radio_Button;
   Multiple_Button     : Gtk_Radio_Button;
   Mode_Group          : Widget_SList.GSlist;
   Draw_Line_Button    : Gtk_Check_Button;
   View_Line_Button    : Gtk_Check_Button;
   No_Root_Item_Button : Gtk_Check_Button;
   Nb_Item_Spinner     : Gtk_Spin_Button;
   Recursion_Spinner   : Gtk_Spin_Button;

   type My_Tree_Record is new Gtk_Tree_Record with record
      Nb_Item_Add    : Guint;
      Add_Button     : Gtk_Button;
      Remove_Button  : Gtk_Button;
      Subtree_Button : Gtk_Button;
   end record;
   type My_Tree is access all My_Tree_Record'Class;

   package Tree_Cb is new Handlers.Callback (My_Tree_Record);

   package To_Tree is new Unchecked_Cast
     (Gtk_Tree_Record, Gtk_Tree);
   package From_Tree is new Unchecked_Cast
     (Gtk_Tree_Item_Record, Gtk_Tree_Item);


   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Tree@B is to @bGtk_Ctree@B what @bGtk_List@B is to"
        & " @bGtk_Clist@B: this is a more flexible version, since items in"
        & " a tree can contain any kind of widget, including pixmaps."
        & ASCII.LF
        & "On the other hand, it is a little bit slower than a @bGtk_Ctree@B"
        & " and can contain only a limited number of items.";
   end Help;

   ---------------------
   -- Cb_Tree_Changed --
   ---------------------

   procedure Cb_Tree_Changed (Tree : access My_Tree_Record'Class) is
      use Widget_List;
      Selected_List : Widget_List.Glist;
      Nb_Selected   : Guint;
   begin
      Selected_List := Get_Selection (Tree);
      Nb_Selected   := Length (Selected_List);

      if Nb_Selected = 0 then
         Set_Sensitive (Tree.Add_Button,
                        Get_Children (Tree) = Null_List);
         Set_Sensitive (Tree.Remove_Button, False);
         Set_Sensitive (Tree.Subtree_Button, False);
      else
         Set_Sensitive (Tree.Add_Button, Nb_Selected = 1);
         Set_Sensitive (Tree.Remove_Button, True);
         Set_Sensitive (Tree.Subtree_Button, Nb_Selected = 1);
      end if;
   end Cb_Tree_Changed;

   ---------------------
   -- Cb_Add_New_Item --
   ---------------------

   procedure Cb_Add_New_Item (Tree : access My_Tree_Record'Class) is
      use Widget_List;
      Selected_List : Widget_List.Glist;
      Selected_Item : Gtk_Tree_Item;
      Subtree       : Gtk_Tree;
      Item_New      : Gtk_Tree_Item;
   begin
      Selected_List := Get_Selection (Tree);

      if Selected_List = Null_List then
         Subtree := Gtk_Tree (Tree);
      else
         Selected_Item := From_Tree.Convert (Get_Data (Selected_List));
         Subtree := Get_Subtree (Selected_Item);
         if Subtree = null then
            Gtk_New (Subtree);
            Set_Subtree (Selected_Item, Subtree);
         end if;
      end if;

      Gtk_New (Item_New, "Item add" & Guint'Image (Tree.Nb_Item_Add));
      Append (Subtree, Item_New);
      Show (Item_New);
      Tree.Nb_Item_Add := Tree.Nb_Item_Add + 1;
   end Cb_Add_New_Item;

   --------------------
   -- Cb_Remove_Item --
   --------------------

   procedure Cb_Remove_Item (Tree : access My_Tree_Record'Class) is
      use Widget_List;
      Selected_List : Widget_List.Glist;
      Clear_List    : Widget_List.Glist;
   begin
      Selected_List := Get_Selection (Tree);
      while Selected_List /= Null_List loop
         Prepend (Clear_List, Get_Data (Selected_List));
         Selected_List := Next (Selected_List);
      end loop;

      List_Reverse (Clear_List);
      Remove_Items (Tree, Clear_List);
      Free (Clear_List);
   end Cb_Remove_Item;

   -----------------------
   -- Cb_Remove_Subtree --
   -----------------------

   procedure Cb_Remove_Subtree (Tree : access My_Tree_Record'Class) is
      use Widget_List;
      Selected_List : Widget_List.Glist;
      Item          : Gtk_Tree_Item;
   begin
      Selected_List := Get_Selection (Tree);
      if Selected_List /= Null_List then
        Item := From_Tree.Convert (Get_Data (Selected_List));
        if Is_Created (Item.all) then
           Remove_Subtree (Item);
        end if;
      end if;
   end Cb_Remove_Subtree;

   --------------------
   -- Create_Subtree --
   --------------------

   procedure Create_Subtree (Item                : in Gtk_Tree_Item;
                             Level               : in Guint;
                             Nb_Item_Max         : in Gint;
                             Recursion_Level_Max : in Gint)
   is
      Item_SubTree : Gtk_Tree;
      No_Root_Item : Boolean;
      Item_New     : Gtk_Tree_Item;
      The_Level    : Guint := Level;
   begin
      if Level = Guint (Recursion_Level_Max) then
         return;
      end if;

      if Level = -1 then
         The_Level := 0;
         Item_Subtree := To_Tree.Convert (Item);
         No_Root_Item := True;
      else
         declare
            Tree : Gtk_Tree;
         begin
            Gtk_New (Tree);
            Item_Subtree := Tree;
            No_Root_Item := False;
         end;
      end if;

      for Nb_Item in 0 .. Nb_Item_Max - 1 loop
         Gtk_New (Item_New,
                  "item" & Guint'Image (The_Level)
                  & "-" & Gint'Image (Nb_Item));
         Append (Item_Subtree, Item_New);
         Create_Subtree (Item_New, The_Level + 1, Nb_Item_Max,
                         Recursion_Level_Max);
         Show (Item_New);
      end loop;

      if not No_Root_Item then
         Set_Subtree (Item, Item_Subtree);
      end if;
   end Create_Subtree;

   ------------------------
   -- Create_Tree_Sample --
   ------------------------

   procedure Create_Tree_Sample (Selection_Mode : in Gtk_Selection_Mode;
                                 Draw_Line      : in Boolean;
                                 View_Line      : in Boolean;
                                 No_Root_Item   : in Boolean;
                                 Nb_Item_Max    : in Gint;
                                 Recursion_Level_Max : in Gint)
   is
      Box2         : Gtk_Box;
      Scrolled     : Gtk_Scrolled_Window;
      Root_Tree    : My_Tree;
      Root_Item    : Gtk_Tree_Item;
      use Gtk.Widget.Widget_List;
      List         : Gtk.Widget.Widget_List.Glist;

   begin
      List := Children (Tree_Area);
      while Length (List) /= 0 loop
         Remove (Tree_Area, Get_Data (List));
         List := Next (List);
      end loop;

      Gtk_New_Vbox (Box2, False, 0);
      Pack_Start (Tree_Area, Box2, True, True, 0);
      Set_Border_Width (Box2, 5);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Box2, Scrolled, True, True, 0);
      Set_Usize (Scrolled, 200, 200);

      --  Create root tree widget
      Root_Tree := new My_Tree_Record;
      Initialize (Root_Tree);
      Root_Tree.Nb_Item_Add := 0;
      Tree_Cb.Object_Connect
        (Root_Tree, "selection_changed",
         Tree_Cb.To_Marshaller (Cb_Tree_Changed'Access),
         Slot_Object => Root_Tree);
      Add_With_Viewport (Scrolled, Root_Tree);
      Set_Selection_Mode (Root_Tree, Selection_Mode);
      Set_View_Lines (Root_Tree, Draw_Line);
      if View_Line then
         Set_View_Mode (Root_Tree, Gtk.Enums.Tree_View_Line);
      else
         Set_View_Mode (Root_Tree, Gtk.Enums.Tree_View_Item);
      end if;

      if No_Root_Item then
         Root_Item := From_Tree.Convert (Root_Tree);
      else
         Gtk_New (Root_Item, "root item");
         Append (Root_Tree, Root_Item);
      end if;

      Create_Subtree (Root_Item, - Boolean'Pos (No_Root_Item), Nb_Item_Max,
                      Recursion_Level_Max);

      Gtk_New_Vbox (Box2, False, 0);
      Pack_Start (Tree_Area, Box2, False, False, 0);
      Set_Border_Width (Box2, 5);

      Gtk_New (Root_Tree.Add_Button, "Add Item");
      Set_Sensitive (Root_Tree.Add_Button, False);
      Tree_Cb.Object_Connect
        (Root_Tree.Add_Button, "clicked",
         Tree_Cb.To_Marshaller (Cb_Add_New_Item'Access),
         Slot_Object => Root_Tree);
      Pack_Start (Box2, Root_Tree.Add_Button, True, True, 0);

      Gtk_New (Root_Tree.Remove_Button, "Remove Item(s)");
      Set_Sensitive (Root_Tree.Remove_Button, False);
      Tree_Cb.Object_Connect
        (Root_Tree.Remove_Button, "clicked",
         Tree_Cb.To_Marshaller (Cb_Remove_Item'Access),
         Slot_Object => Root_Tree);
      Pack_Start (Box2, Root_Tree.Remove_Button, True, True, 0);

      Gtk_New (Root_Tree.Subtree_Button, "Remove Subtree");
      Set_Sensitive (Root_Tree.Subtree_Button, False);
      Tree_Cb.Object_Connect
        (Root_Tree.Subtree_Button, "clicked",
         Tree_Cb.To_Marshaller (Cb_Remove_Subtree'Access),
         Slot_Object => Root_Tree);
      Pack_Start (Box2, Root_Tree.Subtree_Button, True, True, 0);

      Show_All (Tree_Area);
   end Create_Tree_Sample;

   --------------------
   -- Cb_Create_Tree --
   --------------------

   procedure Cb_Create_Tree (Button : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Button);
      Selection_Mode  : Gtk_Selection_Mode := Selection_Single;
      View_Line       : Boolean;
      Draw_Line       : Boolean;
      No_Root_Item    : Boolean;
      Nb_Item         : Gint;
      Recursion_Level : Gint;
   begin
      --  Get Selection Mode Choice
      if Get_Active (Single_Button) then
         Selection_Mode := Selection_Single;
      elsif Get_Active (Browse_Button) then
         Selection_Mode := Selection_Browse;
      else
         Selection_Mode := Selection_Multiple;
      end if;

      --  Get options choice
      Draw_Line    := Get_Active (Draw_Line_Button);
      View_Line    := Get_Active (View_Line_Button);
      No_Root_Item := Get_Active (No_Root_Item_Button);

      --  Get levels
      Nb_Item         := Get_Value_As_Int (Nb_Item_Spinner);
      Recursion_Level := Get_Value_As_Int (Recursion_Spinner);

      Create_Tree_Sample (Selection_Mode, Draw_Line, View_Line,
                          No_Root_Item, Nb_Item, Recursion_Level);
   end Cb_Create_Tree;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1,
        Box2,
        Box3,
        Box4,
        Box5   : Gtk_Box;
      Button   : Gtk_Button;
      Frame2   : Gtk_Frame;
      Adj      : Gtk_Adjustment;
      Label    : Gtk_Label;

   begin
      Set_Label (Frame, "Tree");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 5);
      Pack_Start (Box1, Box2, False, False, 0);
      Set_Border_Width (Box2, 5);

      Gtk_New_Hbox (Box3, False, 5);
      Pack_Start (Box2, Box3, False, False, 0);

      --  Create selection mode frame
      Gtk_New (Frame2, "Selection Mode");
      Pack_Start (Box3, Frame2, False, False, 0);

      Gtk_New_Vbox (Box4, False, 0);
      Add (Frame2, Box4);
      Set_Border_Width (Box4, 5);

      Gtk_New (Single_Button, Widget_Slist.Null_List, "SINGLE");
      Pack_Start (Box4, Single_Button, False, False, 0);

      Gtk_New (Browse_Button, Group (Single_Button), "BROWSE");
      Pack_Start (Box4, Browse_Button, False, False, 0);

      Gtk_New (Multiple_Button, Group (Browse_Button), "MULTIPLE");
      Pack_Start (Box4, Multiple_Button, False, False, 0);

      Mode_Group := Group (Multiple_Button);

      --  Create option mode frame
      Gtk_New (Frame2, "Options");
      Pack_Start (Box3, Frame2, False, False, 0);

      Gtk_New_Vbox (Box4, False, 0);
      Add (Frame2, Box4);
      Set_Border_Width (Box4, 5);

      Gtk_New (Draw_Line_Button, "Draw Line");
      Pack_Start (Box4, Draw_Line_Button, False, False, 0);
      Set_Active (Draw_Line_Button, True);

      Gtk_New (View_Line_Button, "View line mode");
      Pack_Start (Box4, View_Line_Button, False, False, 0);
      Set_Active (View_Line_Button, True);

      Gtk_New (No_Root_Item_Button, "Without Root Item");
      Pack_Start (Box4, No_Root_Item_Button, False, False, 0);
      Set_Active (No_Root_Item_Button, True);

      --  Create recursion parameter
      Gtk_New (Frame2, "Size parameters");
      Pack_Start (Box2, Frame2, False, False, 0);

      Gtk_New_Hbox (Box4, False, 5);
      Add (Frame2, Box4);
      Set_Border_Width (Box4, 5);

         --  Create number of item spin button
      Gtk_New_Hbox (Box5, False, 5);
      Pack_Start (Box4, Box5, False, False, 0);

      Gtk_New (Label, "Number of Item");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box5, Label, False, True, 0);

      Gtk_New (Adj, Default_Number_Of_Item, 1.0, 255.0, 1.0, 5.0, 0.0);
      Gtk_New (Nb_Item_Spinner, Adj, 0.0, 0);
      Pack_Start (Box5, Nb_Item_Spinner, False, True, 0);

      --  Create recursion level spin button
      Gtk_New_Hbox (Box5, False, 5);
      Pack_Start (Box4, Box5, False, False, 0);

      Gtk_New (Label, "Depth level");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box5, Label, False, True, 0);

      Gtk_New (Adj, Default_Recursion_Level, 0.0, 255.0, 1.0, 5.0, 0.0);
      Gtk_New (Recursion_Spinner, Adj, 0.0, 0);
      Pack_Start (Box5, Recursion_Spinner, False, False, 0);

      Gtk_New (Button, "Create Tree Sample");
      Pack_Start (Box1, Button, False, False, 0);
      Widget_Handler.Object_Connect
        (Button, "clicked",
         Widget_Handler.To_Marshaller (Cb_Create_Tree'Access),
         Slot_Object => Button);

      Gtk_New_Vbox (Tree_Area, Homogeneous => False, Spacing => 0);
      Pack_Start (Box1, Tree_Area, False, False, 0);

      Show_All (Frame);
   end Run;

end Create_Tree;
