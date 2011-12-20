------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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
--  A Gtk_Combo_Box_Entry is a widget that allows the user to choose from a
--  list of valid choices or enter a different value. It is very similar to
--  Gtk_Combo_Box, but it displays the selected value in an entry to allow
--  modifying it.
--
--  In contrast to a Gtk_Combo_Box, the underlying model of a
--  Gtk_Combo_Box_Entry must always have a text column (see Set_Text_Column),
--  and the entry will show the content of the text column in the selected row.
--  To get the text from the entry, use Gtk.Combo_Box.Get_Active_Text.
--
--  The changed signal will be emitted while typing into a Gtk_Combo_Box_Entry,
--  as well as when selecting an item from the Gtk_Combo_Box_Entry's list. Use
--  Gtk.Combo_Box.Get_Active or Gtk.Combo_Box.Get_Active_Iter to discover
--  whether an item was actually selected from the list.
--
--  Connect to the activate signal of the Gtk_Entry (use Gtk.Bin.Get_Child) to
--  detect when the user actually finishes entering text.
--
--  The convenience API to construct simple text-only Gtk_Combo_Box can also be
--  used with Gtk_Combo_Box_Entry which have been constructed with
--  Gtk_New_Text.
--  </description>
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>
--  <screenshot>gtk-combo_box_entry</screenshot>

with Glib.Properties;
with Glib.Types;
with Gtk.Cell_Editable;
with Gtk.Cell_Layout;
with Gtk.Combo_Box;
with Gtk.Tree_Model;

package Gtk.Combo_Box_Entry is

   type Gtk_Combo_Box_Entry_Record is new Gtk.Combo_Box.Gtk_Combo_Box_Record
      with null record;
   type Gtk_Combo_Box_Entry is access all Gtk_Combo_Box_Entry_Record'Class;

   procedure Gtk_New    (Combo : out Gtk_Combo_Box_Entry);
   procedure Initialize (Combo : access Gtk_Combo_Box_Entry_Record'Class);
   --  Creates a new combo entry which has a Gtk.Gentry.Gtk_Entry as child.
   --  After construction, you should set a model using Gtk.Combo_Box.Set_Model
   --  and text_column using Set_Text_Column.

   procedure Gtk_New_Text    (Combo : out Gtk_Combo_Box_Entry);
   procedure Initialize_Text (Combo : access Gtk_Combo_Box_Entry_Record'Class);
   --  Convenience function which constructs a new editable text combo box,
   --  which is an entry just displaying strings. If you use this function to
   --  create a text combo box, you should only manipulate its data source with
   --  the convenience functions Gtk.Combo_Box.Append_Text,
   --  Gtk.Combo_Box.Insert_Text,...

   procedure Gtk_New_With_Model
     (Combo       : out Gtk_Combo_Box_Entry;
      Model       : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Text_Column : Gint);
   procedure Initialize_With_Model
     (Combo       : access Gtk_Combo_Box_Entry_Record'Class;
      Model       : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Text_Column : Gint);
   --  Creates a new entry which has a Gtk_Entry as child and a list of strings
   --  as popup. You can get the Gtk_Entry using Gtk.Bin.Get_Child. To add and
   --  remove strings from the list, just modify Model

   function Get_Type return Glib.GType;
   --  Returns the internal value used for Gtk_Combo_Box_Entry widgets

   procedure Set_Text_Column
     (Entry_Box : access Gtk_Combo_Box_Entry_Record; Text_Column : Gint);
   function Get_Text_Column
     (Entry_Box : access Gtk_Combo_Box_Entry_Record)  return Gint;
   --  Sets the model column which Entry_Box should use to get strings from
   --  to be Text_Column.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Cell_Layout"
   --    This interface should be used to add new renderers to the view, to
   --    render various columns of the model
   --  - "Gtk_Cell_Editable"
   --    This interface should be used to edit the contents of a tree model
   --    cell

   package Implements_Cell_Layout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Combo_Box_Entry_Record,
      Gtk_Combo_Box_Entry);
   function "+"
     (Box : access Gtk_Combo_Box_Entry_Record'Class)
      return Gtk.Cell_Layout.Gtk_Cell_Layout
      renames Implements_Cell_Layout.To_Interface;
   function "-"
     (Layout : Gtk.Cell_Layout.Gtk_Cell_Layout)
      return Gtk_Combo_Box_Entry
      renames Implements_Cell_Layout.To_Object;
   --  Converts to and from the Gtk_Cell_Layout interface

   package Implements_Cell_Editable is new Glib.Types.Implements
     (Gtk.Cell_Editable.Gtk_Cell_Editable,
      Gtk_Combo_Box_Entry_Record, Gtk_Combo_Box_Entry);
   function "+"
     (Box : access Gtk_Combo_Box_Entry_Record'Class)
      return Gtk.Cell_Editable.Gtk_Cell_Editable
      renames Implements_Cell_Editable.To_Interface;
   function "-"
     (Editable : Gtk.Cell_Editable.Gtk_Cell_Editable)
      return Gtk_Combo_Box_Entry
      renames Implements_Cell_Editable.To_Object;
   --  Converts to and from the Gtk_Cell_Editable interface

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.

   --  <properties>
   --  Name:  Text_Column_Property
   --  Type:  Int
   --  Descr: A column in the data source model to get the strings from
   --  </properties>

   Text_Column_Property : constant Glib.Properties.Property_Int;

private
   Text_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("text-column");

   pragma Import (C, Get_Type, "gtk_combo_box_entry_get_type");
end Gtk.Combo_Box_Entry;
