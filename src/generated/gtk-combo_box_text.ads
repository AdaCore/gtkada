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
--  A GtkComboBoxText is a simple variant of Gtk.Combo_Box.Gtk_Combo_Box that
--  hides the model-view complexity for simple text-only use cases.
--
--  To create a GtkComboBoxText, use Gtk.Combo_Box_Text.Gtk_New or
--  Gtk.Combo_Box_Text.Gtk_New_With_Entry.
--
--  You can add items to a GtkComboBoxText with
--  Gtk.Combo_Box_Text.Append_Text, Gtk.Combo_Box_Text.Insert_Text or
--  Gtk.Combo_Box_Text.Prepend_Text and remove options with
--  Gtk.Combo_Box_Text.Remove.
--
--  If the GtkComboBoxText contains an entry (via the "has-entry" property),
--  its contents can be retrieved using Gtk.Combo_Box_Text.Get_Active_Text. The
--  entry itself can be accessed by calling Gtk.Bin.Get_Child on the combo box.
--
--  You should not call Gtk.Combo_Box.Set_Model or attempt to pack more cells
--  into this combo box via its GtkCellLayout interface.
--
--  # GtkComboBoxText as GtkBuildable
--
--  The GtkComboBoxText implementation of the GtkBuildable interface supports
--  adding items directly using the <items> element and specifying <item>
--  elements for each item. Each <item> element can specify the "id"
--  corresponding to the appended text and also supports the regular
--  translation attributes "translatable", "context" and "comments".
--
--  Here is a UI definition fragment specifying GtkComboBoxText items: |[
--  <object class="GtkComboBoxText"> <items> <item translatable="yes"
--  id="factory">Factory</item> <item translatable="yes" id="home">Home</item>
--  <item translatable="yes" id="subway">Subway</item> </items> </object> ]|
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> combobox ╰── box.linked ├── entry.combo ├──
--  button.combo ╰── window.popup ]|
--
--  GtkComboBoxText has a single CSS node with name combobox. It adds the
--  style class .combo to the main CSS nodes of its entry and button children,
--  and the .linked class to the node of its internal box.
--
--  </description>
--  <group>Numeric/Text Data Entry</group>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;         use Gdk.Event;
with Glib;              use Glib;
with Glib.Types;        use Glib.Types;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Cell_Editable; use Gtk.Cell_Editable;
with Gtk.Cell_Layout;   use Gtk.Cell_Layout;
with Gtk.Cell_Renderer; use Gtk.Cell_Renderer;
with Gtk.Combo_Box;     use Gtk.Combo_Box;
with Gtk.Tree_Model;    use Gtk.Tree_Model;

package Gtk.Combo_Box_Text is

   type Gtk_Combo_Box_Text_Record is new Gtk_Combo_Box_Record with null record;
   type Gtk_Combo_Box_Text is access all Gtk_Combo_Box_Text_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Cell_Layout_Data_Func is access procedure
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  A function which should set the value of Cell_Layout's cell renderer(s)
   --  as appropriate.
   --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
   --  "cell": the cell renderer whose value is to be set
   --  "tree_model": the model
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to set the
   --  value for

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Combo_Box_Text);
   procedure Initialize
      (Self : not null access Gtk_Combo_Box_Text_Record'Class);
   --  Creates a new Gtk.Combo_Box_Text.Gtk_Combo_Box_Text, which is a
   --  Gtk.Combo_Box.Gtk_Combo_Box just displaying strings.
   --  Since: gtk+ 2.24
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Combo_Box_Text_New return Gtk_Combo_Box_Text;
   --  Creates a new Gtk.Combo_Box_Text.Gtk_Combo_Box_Text, which is a
   --  Gtk.Combo_Box.Gtk_Combo_Box just displaying strings.
   --  Since: gtk+ 2.24

   procedure Gtk_New_With_Entry (Self : out Gtk_Combo_Box_Text);
   procedure Initialize_With_Entry
      (Self : not null access Gtk_Combo_Box_Text_Record'Class);
   --  Creates a new Gtk.Combo_Box_Text.Gtk_Combo_Box_Text, which is a
   --  Gtk.Combo_Box.Gtk_Combo_Box just displaying strings. The combo box
   --  created by this function has an entry.
   --  Since: gtk+ 2.24
   --  Initialize_With_Entry does nothing if the object was already created
   --  with another call to Initialize* or G_New.

   function Gtk_Combo_Box_Text_New_With_Entry return Gtk_Combo_Box_Text;
   --  Creates a new Gtk.Combo_Box_Text.Gtk_Combo_Box_Text, which is a
   --  Gtk.Combo_Box.Gtk_Combo_Box just displaying strings. The combo box
   --  created by this function has an entry.
   --  Since: gtk+ 2.24

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_combo_box_text_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append
      (Self : not null access Gtk_Combo_Box_Text_Record;
       Id   : UTF8_String := "";
       Text : UTF8_String);
   --  Appends Text to the list of strings stored in Combo_Box. If Id is
   --  non-null then it is used as the ID of the row.
   --  This is the same as calling Gtk.Combo_Box_Text.Insert with a position
   --  of -1.
   --  Since: gtk+ 2.24
   --  "id": a string ID for this value, or null
   --  "text": A string

   procedure Append_Text
      (Self : not null access Gtk_Combo_Box_Text_Record;
       Text : UTF8_String);
   --  Appends Text to the list of strings stored in Combo_Box.
   --  This is the same as calling Gtk.Combo_Box_Text.Insert_Text with a
   --  position of -1.
   --  Since: gtk+ 2.24
   --  "text": A string

   function Get_Active_Text
      (Self : not null access Gtk_Combo_Box_Text_Record) return UTF8_String;
   --  Returns the currently active string in Combo_Box, or null if none is
   --  selected. If Combo_Box contains an entry, this function will return its
   --  contents (which will not necessarily be an item from the list).
   --  Since: gtk+ 2.24

   procedure Insert
      (Self     : not null access Gtk_Combo_Box_Text_Record;
       Position : Glib.Gint;
       Id       : UTF8_String := "";
       Text     : UTF8_String);
   --  Inserts Text at Position in the list of strings stored in Combo_Box. If
   --  Id is non-null then it is used as the ID of the row. See
   --  Gtk.Combo_Box.Gtk_Combo_Box:id-column.
   --  If Position is negative then Text is appended.
   --  Since: gtk+ 3.0
   --  "position": An index to insert Text
   --  "id": a string ID for this value, or null
   --  "text": A string to display

   procedure Insert_Text
      (Self     : not null access Gtk_Combo_Box_Text_Record;
       Position : Glib.Gint;
       Text     : UTF8_String);
   --  Inserts Text at Position in the list of strings stored in Combo_Box.
   --  If Position is negative then Text is appended.
   --  This is the same as calling Gtk.Combo_Box_Text.Insert with a null ID
   --  string.
   --  Since: gtk+ 2.24
   --  "position": An index to insert Text
   --  "text": A string

   procedure Prepend
      (Self : not null access Gtk_Combo_Box_Text_Record;
       Id   : UTF8_String := "";
       Text : UTF8_String);
   --  Prepends Text to the list of strings stored in Combo_Box. If Id is
   --  non-null then it is used as the ID of the row.
   --  This is the same as calling Gtk.Combo_Box_Text.Insert with a position
   --  of 0.
   --  Since: gtk+ 2.24
   --  "id": a string ID for this value, or null
   --  "text": a string

   procedure Prepend_Text
      (Self : not null access Gtk_Combo_Box_Text_Record;
       Text : UTF8_String);
   --  Prepends Text to the list of strings stored in Combo_Box.
   --  This is the same as calling Gtk.Combo_Box_Text.Insert_Text with a
   --  position of 0.
   --  Since: gtk+ 2.24
   --  "text": A string

   procedure Remove
      (Self     : not null access Gtk_Combo_Box_Text_Record;
       Position : Glib.Gint);
   --  Removes the string at Position from Combo_Box.
   --  Since: gtk+ 2.24
   --  "position": Index of the item to remove

   procedure Remove_All (Self : not null access Gtk_Combo_Box_Text_Record);
   --  Removes all the text entries from the combo box.
   --  Since: gtk+ 3.0

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func        : Gtk_Cell_Layout_Data_Func);
   --  Sets the Gtk_Cell_Layout_Data_Func to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Since: gtk+ 2.4
   --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  "func": the Gtk_Cell_Layout_Data_Func to use, or null

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Cell_Data_Func_User_Data is

      type Gtk_Cell_Layout_Data_Func is access procedure
        (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
         Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data        : User_Data_Type);
      --  A function which should set the value of Cell_Layout's cell renderer(s)
      --  as appropriate.
      --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
      --  "cell": the cell renderer whose value is to be set
      --  "tree_model": the model
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to set the
      --  value for
      --  "data": user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

      procedure Set_Cell_Data_Func
         (Cell_Layout : not null access Gtk.Combo_Box_Text.Gtk_Combo_Box_Text_Record'Class;
          Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func        : Gtk_Cell_Layout_Data_Func;
          Func_Data   : User_Data_Type);
      --  Sets the Gtk_Cell_Layout_Data_Func to use for Cell_Layout.
      --  This function is used instead of the standard attributes mapping for
      --  setting the column value, and should set the value of Cell_Layout's
      --  cell renderer(s) as appropriate.
      --  Func may be null to remove a previously set function.
      --  Since: gtk+ 2.4
      --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
      --  "func": the Gtk_Cell_Layout_Data_Func to use, or null
      --  "func_data": user data for Func

   end Set_Cell_Data_Func_User_Data;

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Editing_Done
      (Cell_Editable : not null access Gtk_Combo_Box_Text_Record);

   procedure Remove_Widget
      (Cell_Editable : not null access Gtk_Combo_Box_Text_Record);

   procedure Start_Editing
      (Cell_Editable : not null access Gtk_Combo_Box_Text_Record;
       Event         : Gdk.Event.Gdk_Event);

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Glib.Gint);

   procedure Clear (Cell_Layout : not null access Gtk_Combo_Box_Text_Record);

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);

   function Get_Cells
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Pack_Start
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Reorder
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Glib.Gint);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "CellEditable"
   --
   --  - "CellLayout"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Combo_Box_Text_Record, Gtk_Combo_Box_Text);
   function "+"
     (Widget : access Gtk_Combo_Box_Text_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Combo_Box_Text
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Cell_Editable is new Glib.Types.Implements
     (Gtk.Cell_Editable.Gtk_Cell_Editable, Gtk_Combo_Box_Text_Record, Gtk_Combo_Box_Text);
   function "+"
     (Widget : access Gtk_Combo_Box_Text_Record'Class)
   return Gtk.Cell_Editable.Gtk_Cell_Editable
   renames Implements_Gtk_Cell_Editable.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Editable.Gtk_Cell_Editable)
   return Gtk_Combo_Box_Text
   renames Implements_Gtk_Cell_Editable.To_Object;

   package Implements_Gtk_Cell_Layout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Combo_Box_Text_Record, Gtk_Combo_Box_Text);
   function "+"
     (Widget : access Gtk_Combo_Box_Text_Record'Class)
   return Gtk.Cell_Layout.Gtk_Cell_Layout
   renames Implements_Gtk_Cell_Layout.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Layout.Gtk_Cell_Layout)
   return Gtk_Combo_Box_Text
   renames Implements_Gtk_Cell_Layout.To_Object;

end Gtk.Combo_Box_Text;
