------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
--  Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo renders text in a cell
--  like Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text from which it is
--  derived. But while Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text offers a
--  simple entry to edit the text,
--  Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo offers a
--  Gtk.Combo_Box.Gtk_Combo_Box widget to edit the text. The values to display
--  in the combo box are taken from the tree model specified in the
--  Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo:model property.
--
--  The combo cell renderer takes care of adding a text cell renderer to the
--  combo box and sets it to display the column specified by its
--  Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo:text-column property.
--  Further properties of the combo box can be set in a handler for the
--  Gtk.Cell_Renderer.Gtk_Cell_Renderer::editing-started signal.
--
--  The Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo cell renderer was
--  added in GTK+ 2.6.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;
with Glib.Properties;        use Glib.Properties;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Tree_Model;         use Gtk.Tree_Model;

package Gtk.Cell_Renderer_Combo is

   type Gtk_Cell_Renderer_Combo_Record is new Gtk_Cell_Renderer_Text_Record with null record;
   type Gtk_Cell_Renderer_Combo is access all Gtk_Cell_Renderer_Combo_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Combo);
   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Combo_Record'Class);
   --  Creates a new Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo. Adjust
   --  how text is drawn using object properties. Object properties can be set
   --  globally (with g_object_set). Also, with
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column, you can bind a property to a
   --  value in a Gtk.Tree_Model.Gtk_Tree_Model. For example, you can bind the
   --  "text" property on the cell renderer to a string value in the model,
   --  thus rendering a different string in each row of the
   --  Gtk.Tree_View.Gtk_Tree_View.
   --  Since: gtk+ 2.6
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Cell_Renderer_Combo_New return Gtk_Cell_Renderer_Combo;
   --  Creates a new Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo. Adjust
   --  how text is drawn using object properties. Object properties can be set
   --  globally (with g_object_set). Also, with
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column, you can bind a property to a
   --  value in a Gtk.Tree_Model.Gtk_Tree_Model. For example, you can bind the
   --  "text" property on the cell renderer to a string value in the model,
   --  thus rendering a different string in each row of the
   --  Gtk.Tree_View.Gtk_Tree_View.
   --  Since: gtk+ 2.6

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_renderer_combo_get_type");

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Has_Entry_Property : constant Glib.Properties.Property_Boolean;
   --  If True, the cell renderer will include an entry and allow to enter
   --  values other than the ones in the popup list.

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Tree_Model.Gtk_Tree_Model
   --  Holds a tree model containing the possible values for the combo box.
   --  Use the text_column property to specify the column holding the values.

   Text_Column_Property : constant Glib.Properties.Property_Int;
   --  Specifies the model column which holds the possible values for the
   --  combo box.
   --
   --  Note that this refers to the model specified in the model property, not
   --  the model backing the tree view to which this cell renderer is attached.
   --
   --  Gtk.Cell_Renderer_Combo.Gtk_Cell_Renderer_Combo automatically adds a
   --  text cell renderer for this column to its combo box.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Cell_Renderer_Combo_UTF8_String_Gtk_Tree_Iter_Void is not null access procedure
     (Self        : access Gtk_Cell_Renderer_Combo_Record'Class;
      Path_String : UTF8_String;
      New_Iter    : Gtk.Tree_Model.Gtk_Tree_Iter);

   type Cb_GObject_UTF8_String_Gtk_Tree_Iter_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Path_String : UTF8_String;
      New_Iter    : Gtk.Tree_Model.Gtk_Tree_Iter);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Cell_Renderer_Combo_Record;
       Call  : Cb_Gtk_Cell_Renderer_Combo_UTF8_String_Gtk_Tree_Iter_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Cell_Renderer_Combo_Record;
       Call  : Cb_GObject_UTF8_String_Gtk_Tree_Iter_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted each time after the user selected an item in the
   --  combo box, either by using the mouse or the arrow keys. Contrary to
   --  GtkComboBox, GtkCellRendererCombo::changed is not emitted for changes
   --  made to a selected item in the entry. The argument New_Iter corresponds
   --  to the newly selected item in the combo box and it is relative to the
   --  GtkTreeModel set via the model property on GtkCellRendererCombo.
   --
   --  Note that as soon as you change the model displayed in the tree view,
   --  the tree view will immediately cease the editing operating. This means
   --  that you most probably want to refrain from changing the model until the
   --  combo cell renderer emits the edited or editing_canceled signal.
   -- 
   --  Callback parameters:
   --    --  "path_string": a string of the path identifying the edited cell
   --    --  (relative to the tree view model)
   --    --  "new_iter": the new iter selected in the combo box (relative to the
   --    --  combo box model)

private
   Text_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("text-column");
   Model_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("model");
   Has_Entry_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-entry");
end Gtk.Cell_Renderer_Combo;
