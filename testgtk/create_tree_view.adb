------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with Ada.Exceptions;

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Gtk;                      use Gtk;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Cell_Renderer_Combo;  use Gtk.Cell_Renderer_Combo;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Sortable;        use Gtk.Tree_Sortable;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Handlers;             use Gtk.Handlers;
with Pango.Font;               use Pango.Font;

package body Create_Tree_View is

   package Object_Callback is new Gtk.Handlers.Callback (GObject_Record);

   Text_Column       : constant := 0;
   Strike_Column     : constant := 1;
   Editable_Column   : constant := 2;
   Active_Column     : constant := 3;
   Foreground_Column : constant := 4;
   Font_Column       : constant := 5;
   Combo_Model_Column  : constant := 6;
   Combo_Column_Column  : constant := 7;

   Base_Font, Small_Font : Pango_Font_Description;

   function Add_Line
     (Model    : access Gtk_Tree_Store_Record'Class;
      Text     : String;
      Striken  : Boolean := False;
      Editable : Boolean := False;
      Active   : Boolean := False;
      Parent   : Gtk_Tree_Iter := Null_Iter) return Gtk_Tree_Iter;
   --  Insert a new line in the tree, as a child of Parent (or as a root node
   --  if Parent is Null_Iter.

   procedure Edited_Callback
     (Model  : access GObject_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when the user clicks on a toggle button. This is used to reflect
   --  (or not) the change in the model.

   procedure Text_Edited_Callback
     (Model  : access GObject_Record'Class;
      Params : Glib.Values.GValues);
   --  Same when a text cell is edited

   function Custom_Sort
     (Model : Gtk_Tree_Model;
      A, B  : Gtk_Tree_Iter) return Gint;
   --  Our own customer sort function for the tree

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Tree_View@B is a widget used to display a number of"
        & " lines, organized either as a simple list or into a tree"
        & " hierarchy." & ASCII.LF
        & "Each line can contain multiple resizable column, each of which"
        & " can contain pixmaps, texts, or both. The columns can be sorted"
        & " interactively by the user by clicking on the column header."
        & ASCII.LF
        & "Like the @bGtk_Text_View@B, this widget is based on the model-"
        & "view-controller paradigm: data is stored in a non graphical"
        & " object (a @bGtk_Tree_Model@B), which is then associated with"
        & " one or many view. The model provides subprograms for inserting"
        & " or removing lines, as well as for traversing an existing tree."
        & ASCII.LF
        & "Cells in the tree can be defined as editable, as shown in this"
        & " example: in this case, the user needs to double click on the"
        & " cell, and an entry widget is then displayed in which the text"
        & " can be modified"
        & ASCII.LF
        & "The first column is sortable in this example. By default, gtk+"
        & " would use an alphabetical order on a text column, but here we have"
        & " defined our own sorting algorithm (striken first, then others,"
        & " and alphabetical within)";
   end Help;

   -----------------
   -- Custom_Sort --
   -----------------

   function Custom_Sort
     (Model : Gtk_Tree_Model;
      A, B  : Gtk_Tree_Iter) return Gint
   is
      Text_A : constant String := Get_String (Model, A, Text_Column);
      Text_B : constant String := Get_String (Model, B, Text_Column);
      A_Not_Striken : constant Boolean :=
        Text_A'Length > 11
        and then Text_A (Text_A'Last - 10 .. Text_A'Last) = "not striken";
      B_Not_Striken : constant Boolean :=
        Text_B'Length > 11
        and then Text_B (Text_B'Last - 10 .. Text_B'Last) = "not striken";
   begin
      if not A_Not_Striken and then B_Not_Striken then
         return -1;  --  A first, B second
      elsif A_Not_Striken and then not B_Not_Striken then
         return 1;   --  B first, A second
      elsif Text_A < Text_B then
         return -1;  --  A first
      elsif Text_A = Text_B then
         return 0;   --  Same
      else
         return 1;   --  B first
      end if;
   end Custom_Sort;

   --------------
   -- Add_Line --
   --------------

   function Add_Line
     (Model    : access Gtk_Tree_Store_Record'Class;
      Text     : String;
      Striken  : Boolean := False;
      Editable : Boolean := False;
      Active   : Boolean := False;
      Parent   : Gtk_Tree_Iter := Null_Iter) return Gtk_Tree_Iter
   is
      Example1 : constant Boolean := True;

      Iter : Gtk_Tree_Iter;
      Values : GValue_Array (Text_Column .. Combo_Column_Column);

      Combo_Model : Gtk_List_Store;
      Combo_Iter  : Gtk_Tree_Iter;
   begin
      Append (Model, Iter, Parent);

      --  Create a Tree_Store to contain the combo box values for this
      --  row.
      Gtk_New (Combo_Model, (0 => GType_String));
      for Index in 1 .. Text'Length loop
         Append (Combo_Model, Combo_Iter);
         Set (Combo_Model, Combo_Iter, 0,
              Text (Text'First .. Text'Last + 1 - Index));
      end loop;

      if Example1 then
         --  First approach: set an GValue_Array. This requires more code, but
         --  is more efficient in practice since gtk+ does not have to traverse
         --  the tree for each cell, only once for each row

         Init_Set_String (Values (Text_Column), Text);
         Init_Set_Boolean (Values (Strike_Column), Striken);
         Init_Set_Boolean (Values (Editable_Column), Editable);
         Init_Set_Boolean (Values (Active_Column), Active);
         if Editable then
            Init_Set_String (Values (Foreground_Column), "red");
            Pango.Font.Desc_Properties.Set_Value
               (Values (Font_Column), Small_Font);
         else
            Init_Set_String (Values (Foreground_Column), "black");
            Pango.Font.Desc_Properties.Set_Value
               (Values (Font_Column), Base_Font);
         end if;

         Init (Values (Combo_Model_Column), Gtk.List_Store.Get_Type);
         Set_Object (Values (Combo_Model_Column), GObject (Combo_Model));

         Init_Set_Int (Values (Combo_Column_Column), 0);

         Model.Set (Iter, Values);

      else
         --  Second approach: set each cell individually. This is somewhat
         --  easier to code, but might be slower.

         Set (Model, Iter, Text_Column, Text);
         Set (Model, Iter, Strike_Column, Striken);
         Set (Model, Iter, Active_Column, Active);
         Set (Model, Iter, Editable_Column, Editable);
         if Editable then
            Set (Model, Iter, Foreground_Column, "red");
         else
            Set (Model, Iter, Foreground_Column, "black");
         end if;

         Set (Model, Iter, Combo_Model_Column, GObject (Combo_Model));
         Set (Model, Iter, Combo_Column_Column, 0);
      end if;

      return Iter;
   end Add_Line;

   ---------------------
   -- Edited_Callback --
   ---------------------

   procedure Edited_Callback
     (Model  : access GObject_Record'Class;
      Params : Glib.Values.GValues)
   is
      M           : constant Gtk_Tree_Store := Gtk_Tree_Store (Model);
      Path_String : constant String := Get_String (Nth (Params, 1));
      Iter        : constant Gtk_Tree_Iter :=
                      Get_Iter_From_String (M, Path_String);
      Old_Value   : Boolean;
   begin
      Old_Value := Get_Boolean (M, Iter, Active_Column);
      Set (M, Iter, Active_Column, not Old_Value);
   end Edited_Callback;

   --------------------------
   -- Text_Edited_Callback --
   --------------------------

   procedure Text_Edited_Callback
     (Model  : access GObject_Record'Class;
      Params : Glib.Values.GValues)
   is
      M           : constant Gtk_Tree_Store := Gtk_Tree_Store (Model);
      Path_String : constant String := Get_String (Nth (Params, 1));
      Text_Value  : constant GValue := Nth (Params, 2);
      Iter        : constant Gtk_Tree_Iter :=
                      Get_Iter_From_String (M, Path_String);
   begin
      Set_Value (M, Iter, Text_Column, Text_Value);
   end Text_Edited_Callback;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Model    : Gtk_Tree_Store;
      Tree     : Gtk_Tree_View;
      Scrolled : Gtk_Scrolled_Window;
      Col      : Gtk_Tree_View_Column;
      Num      : Gint;
      Text_Render   : Gtk_Cell_Renderer_Text;
      Toggle_Render : Gtk_Cell_Renderer_Toggle;
      Parent, Iter  : Gtk_Tree_Iter;
      Value         : Glib.Values.GValue;

      Combo         : Gtk_Cell_Renderer_Combo;
      pragma Unreferenced (Num);
      pragma Warnings (Off, Iter);

   begin
      --  Create the model that contains the actual data. In this example,
      --  we create this as a tree, although it could also be a simple list.
      --
      --  Each data is in fact a line in the graphical widget, and can contain
      --  multiple columns, not all of which are visible at any time. Some can
      --  be used as flags to indicate how the rendering should be done.

      Gtk_New (Model,
               (Text_Column       => GType_String,
                Strike_Column     => GType_Boolean,
                Editable_Column   => GType_Boolean,
                Active_Column     => GType_Boolean,
                Foreground_Column => GType_String,
                Font_Column       => Pango.Font.Get_Type,
                Combo_Model_Column  => Gtk.List_Store.Get_Type,
                Combo_Column_Column => GType_Int));

      --  Create the view: it shows two columns, the first contains some text,
      --  the second contains a toggle button. In each column, a renderer is
      --  used to display the data graphically. In the future, it will be
      --  possible to create your own renderers with GtkAda. For now, we simply
      --  use two of the predefined renderers. The list of possible attributes
      --  for these is defined in their respective packages.

      Gtk_New (Tree, +Model);
      Set_Grid_Lines (Tree, Grid_Lines_Vertical);
      Set_Enable_Tree_Lines (Tree, True);
      Set_Rubber_Banding (Tree, True);
      Set_Mode (Get_Selection (Tree), Selection_Multiple);

      Gtk_New (Text_Render);
      Gtk_New (Toggle_Render);
      Gtk_New (Combo);

      Gtk_New (Col);
      Col.Set_Resizable (True);
      Num := Append_Column (Tree, Col);
      Set_Sort_Column_Id (Col, Text_Column);
      Set_Title (Col, "First column");
      Pack_Start (Col, Text_Render, True);
      --  Set_Sizing (Col, Tree_View_Column_Autosize);
      Add_Attribute (Col, Text_Render, "text", Text_Column);
      Add_Attribute (Col, Text_Render, "strikethrough", Strike_Column);
      Add_Attribute (Col, Text_Render, "foreground", Foreground_Column);
      Add_Attribute (Col, Text_Render, "font-desc", Font_Column);

      Gtk_New (Col);
      Col.Set_Resizable (True);
      Set_Sort_Column_Id (Col, -1);  --  unsortable
      Num := Append_Column (Tree, Col);
      Set_Title (Col, "Second column");
      Pack_Start (Col, Toggle_Render, False);
      Add_Attribute (Col, Toggle_Render, "active", Active_Column);
      Add_Attribute (Col, Toggle_Render, "activatable", Editable_Column);

      Gtk_New (Col);
      Col.Set_Resizable (True);
      Set_Sort_Column_Id (Col, -1);  --  unsortable
      Num := Append_Column (Tree, Col);
      Set_Title (Col, "Third column");
      Pack_Start (Col, Combo, False);
      Add_Attribute (Col, Combo, "editable", Editable_Column);
      Add_Attribute (Col, Combo, "model", Combo_Model_Column);
      Add_Attribute (Col, Combo, "text-column", Combo_Column_Column);

      Base_Font := From_String ("sans 16px");
      Small_Font := From_String ("sans 12px");

      --  Make the tree sortable (see also calls to Set_Sort_Column_Id above).
      --  The default sort is alphabetical. For the fun of it, we implement
      --  our own sorting algorithm here.

      Set_Headers_Clickable (Tree, True);
      Set_Sort_Func (+Model, Text_Column, Custom_Sort'Access);

      --  By default, the toggle renderer doesn't react to clicks, ie the user
      --  cannot interactively change the value of the radio button. This needs
      --  a special callback for the "edited" signal.
      --  The same applies for text renderers.
      --
      --  In both cases, the callback should be used for validation of the
      --  input.

      Object_Callback.Object_Connect
        (Toggle_Render, "toggled", Edited_Callback'Access,
         Slot_Object => Model);
      Object_Callback.Object_Connect
        (Text_Render, "edited", Text_Edited_Callback'Access,
         Slot_Object => Model);

      --  Insert some data in the tree

      Parent := Null_Iter;

      for Count in 1 .. 30 loop
         Iter := Add_Line
           (Model, "not-editable, not striken", Parent => Parent);
         Iter := Add_Line
           (Model, "editable, not striken",
            Editable => True, Parent => Parent);
         Iter := Add_Line
           (Model, "editable, striken",
            Editable => True, Striken => True, Parent => Parent);
         Iter := Add_Line
           (Model, "not-editable, striken", Striken => True, Parent => Parent);

         Iter := Add_Line
           (Model, "not-editable, not striken",
            Active => True, Parent => Parent);
         Iter := Add_Line
           (Model, "editable, not striken",
            Editable => True, Active => True, Parent => Parent);
         Iter := Add_Line
           (Model, "editable, striken",
            Editable => True, Striken => True, Active => True,
            Parent => Parent);
         Iter := Add_Line
           (Model, "not-editable, striken",
            Striken => True, Active => True, Parent => Parent);

         Parent := Iter;
      end loop;

      --  Test Gtk_Tree_Store.remove
      begin
         Parent := Model.Get_Iter_First;
         Model.Append (Iter => Iter, Parent => Parent);
         Model.Set (Iter, 0, "1");
         Model.Append (Iter => Iter, Parent => Parent);
         Model.Set (Iter, 0, "2");
         Model.Append (Iter => Iter, Parent => Parent);
         Model.Set
           (Iter, 0, "if You see this, Gtk_Tree_Store.remove works incorrect");

         Iter := Model.Nth_Child (Parent, 0);
         Model.Remove (Iter); -- Iter must be set on the 2 after remove

         --  Was iterator set on second node?
         Model.Get_Value (Iter, 0, Value);
         if Glib.Values.Get_String (Value) /= "2" then
            raise Constraint_Error with "Iterator not set on the next node";
         end if;

         Model.Remove (Iter); -- Iter must be set on the warning after remove
         Model.Remove (Iter); -- remove last test node

         --  Was iterator set to null?
         if Iter /= Null_Iter then
            raise Constraint_Error with
              "Iterator not set to null afer last node";
         end if;

         Iter := Add_Line
           (Model, "Gtk_Tree_Store.remove works correctly",
            Editable => False, Striken => False, Active => True,
            Parent => Null_Iter);

      exception
         when E : others =>
            Iter := Add_Line
              (Model, "Test for Gtk_Tree_Store.remove raised:" &
                 Ada.Exceptions.Exception_Information (E),
               Editable => False, Striken => False, Active => True,
               Parent => Null_Iter);
      end;

      --  Insert the view in the frame

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Always, Policy_Always);
      Add (Scrolled, Tree);

      Show_All (Scrolled);
      Add (Frame, Scrolled);
   end Run;

end Create_Tree_View;
