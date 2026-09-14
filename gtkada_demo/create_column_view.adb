------------------------------------------------------------------------------
--               GtkAda - Ada binding for the Gimp Toolkit                  --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

with GNAT.Strings;

with Glib;            use Glib;
with Glib.List_Model; use Glib.List_Model;
with Glib.Object;     use Glib.Object;

with Gtk.Column_View;              use Gtk.Column_View;
with Gtk.Column_View_Column;       use Gtk.Column_View_Column;
with Gtk.Custom_Sorter;            use Gtk.Custom_Sorter;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Expression;
with Gtk.Frame;                    use Gtk.Frame;
with Gtk.Label;                    use Gtk.Label;
with Gtk.List_Item;                use Gtk.List_Item;
with Gtk.Property_Expression;      use Gtk.Property_Expression;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Selection_Model;
with Gtk.Signal_List_Item_Factory; use Gtk.Signal_List_Item_Factory;
with Gtk.Single_Selection;         use Gtk.Single_Selection;
with Gtk.Sort_List_Model;          use Gtk.Sort_List_Model;
with Gtk.Sorter;                   use Gtk.Sorter;
with Gtk.String_List;              use Gtk.String_List;
with Gtk.String_Object;            use Gtk.String_Object;
with Gtk.String_Sorter;            use Gtk.String_Sorter;
with Gtk.Widget;                   use Gtk.Widget;

package body Create_Column_View is

   --  Note that this unit deliberately never writes the bare package name
   --  String_List: with both Gtk and Gtk.Enums use-visible, the simple name
   --  is ambiguous (Gtk.Enums.String_List is an unrelated GList
   --  instantiation). The type name Gtk_String_List is unambiguous.

   Languages : aliased GNAT.Strings.String_List :=
     (new String'("Ada"),
      new String'("C"),
      new String'("Haskell"),
      new String'("OCaml"),
      new String'("Python"),
      new String'("Rust"),
      new String'("SPARK"),
      new String'("Smalltalk"));

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_Column_View@B presents a @bGlist_Model@B as a table: one "
        & "row per item, and one @bGtk_Column_View_Column@B per field."
        & ASCII.LF
        & ASCII.LF
        & "Each column is given a @bGtk_List_Item_Factory@B, "
        & "which manufactures the widget shown in every cell of that column. "
        & ASCII.LF
        & ASCII.LF
        & "Each column is given a @bGtk_Sorter@B."
        & ASCII.LF
        & "Click a column header to sort by it."
        & ASCII.LF;
   end Help;

   procedure Setup_Cell
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure Bind_Language
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   procedure Bind_Letters
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);

   function Compare_Lengths
     (A, B : not null access GObject_Record'Class) return Gint;
   pragma Convention (C, Compare_Lengths);

   function Cell_Label
     (Item : not null access Gtk_List_Item_Record'Class) return Gtk_Label;
   function Item_String
     (Item : not null access Gtk_List_Item_Record'Class) return String;

   ----------------
   -- Cell_Label --
   ----------------

   function Cell_Label
     (Item : not null access Gtk_List_Item_Record'Class) return Gtk_Label
   is (Gtk_Label (Item.Get_Child));

   ----------------
   -- Item_String --
   ----------------

   function Item_String
     (Item : not null access Gtk_List_Item_Record'Class) return String
   is
      --  Get_Item returns a GObject, which for a Gtk_String_List model is
      --  always a Gtk_String_Object. It is null while the item is unbound.
      Obj : constant GObject := Item.Get_Item;
   begin
      if Obj = null then
         return "";
      else
         return Gtk_String_Object (Obj).Get_String;
      end if;
   end Item_String;

   ----------------
   -- Setup_Cell --
   ----------------

   procedure Setup_Cell
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Label : Gtk_Label;
   begin
      Gtk.Label.Gtk_New (Label, "");
      Label.Set_Xalign (0.0);
      Item.Set_Child (Label);
   end Setup_Cell;

   -------------------
   -- Bind_Language --
   -------------------

   procedure Bind_Language
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Cell_Label (Item).Set_Text (Item_String (Item));
   end Bind_Language;

   ------------------
   -- Bind_Letters --
   ------------------

   procedure Bind_Letters
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Length : constant Natural := Item_String (Item)'Length;
   begin
      --  bind is the place for per-item computation: it runs afresh every
      --  time a recycled cell is pointed at another item.
      Cell_Label (Item).Set_Text (Length'Img);
   end Bind_Letters;

   ---------------------
   -- Compare_Lengths --
   ---------------------

   function Compare_Lengths
     (A, B : not null access GObject_Record'Class) return Gint
   is
      Len_A : constant Natural := Gtk_String_Object (A).Get_String'Length;
      Len_B : constant Natural := Gtk_String_Object (B).Get_String'Length;
   begin
      return Gint (Len_A) - Gint (Len_B);
   end Compare_Lengths;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk_Frame_Record'Class) is
      Strings     : Gtk_String_List;
      View        : Gtk_Column_View;
      Language    : Gtk_Column_View_Column;
      Letters     : Gtk_Column_View_Column;
      Factory     : Gtk_Signal_List_Item_Factory;
      By_Name     : Gtk_String_Sorter;
      By_Length   : Gtk_Custom_Sorter;
      View_Sorter : Gtk_Sorter;
      Sorted      : Gtk_Sort_List_Model;
      Scrolled    : Gtk_Scrolled_Window;
   begin
      Frame.Set_Label ("Column View");
      Frame.Set_Label_Align (0.5);

      Gtk.String_List.Gtk_New (Strings, Languages);

      --  The view is built without a model: the model can only be created
      --  once the columns are in place, since it is the view's own sorter
      --  that drives it.
      Gtk.Column_View.Gtk_New
        (View, Gtk.Selection_Model.Null_Gtk_Selection_Model);
      View.Set_Show_Column_Separators (True);
      View.Set_Show_Row_Separators (True);
      View.Set_Reorderable (True);

      --  A column with no factory shows empty cells, so every column gets
      --  one. Handlers are shared: the two columns differ only in bind.
      Gtk.Signal_List_Item_Factory.Gtk_New (Factory);
      Factory.On_Setup (Setup_Cell'Access);
      Factory.On_Bind (Bind_Language'Access);

      Gtk.Column_View_Column.Gtk_New (Language, "Language", Factory);
      Language.Set_Expand (True);
      By_Name :=
        Gtk_String_Sorter_New
          (Gtk.Expression.Gtk_Expression
             (Gtk_Property_Expression_New
                (Gtk.String_Object.Get_Type, null, "string")));
      Language.Set_Sorter (By_Name);
      View.Append_Column (Language);

      Gtk.Signal_List_Item_Factory.Gtk_New (Factory);
      Factory.On_Setup (Setup_Cell'Access);
      Factory.On_Bind (Bind_Letters'Access);

      Gtk.Column_View_Column.Gtk_New (Letters, "Letters", Factory);
      --  A property expression cannot reach the length of the string, so
      --  this column sorts through a comparison function instead.
      By_Length := Gtk_Custom_Sorter_New (Compare_Lengths'Access);
      Letters.Set_Sorter (By_Length);
      View.Append_Column (Letters);

      --  Get_Sorter is transfer-none whereas Gtk_Sort_List_Model_New is
      --  transfer-full on its sorter, so the reference has to be taken
      --  explicitly here. Without it the sorter would be finalised along
      --  with the sort model, and the demo would crash.
      View_Sorter := View.Get_Sorter;
      Ref (View_Sorter);

      Sorted := Gtk_Sort_List_Model_New (+Strings, View_Sorter);
      View.Set_Model (+Gtk_Single_Selection_New (+Sorted));

      --  Only now that a sort model is in place does this have any effect.
      View.Sort_By_Column (Language, Sort_Ascending);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Scrolled.Set_Child (View);
      Scrolled.Set_Margin_Top (10);
      Scrolled.Set_Margin_Bottom (10);
      Scrolled.Set_Margin_Start (10);
      Scrolled.Set_Margin_End (10);
      Frame.Set_Child (Scrolled);
   end Run;

end Create_Column_View;
