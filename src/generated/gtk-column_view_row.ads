------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Configures how rows are displayed in a [classGtk.ColumnView].
--
--  It is not used to set the widgets displayed in the individual cells. For
--  that see [methodGtkcolumnviewcolumn.set_factory] and
--  [classGtkcolumnviewcell].

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gtk.Column_View_Row is

   type Gtk_Column_View_Row_Record is new GObject_Record with null record;
   type Gtk_Column_View_Row is access all Gtk_Column_View_Row_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_column_view_row_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Accessible_Description
      (Self : not null access Gtk_Column_View_Row_Record) return UTF8_String;
   --  Gets the accessible description of Self.
   --  Since: gtk+ 4.12
   --  @return the accessible description

   procedure Set_Accessible_Description
      (Self        : not null access Gtk_Column_View_Row_Record;
       Description : UTF8_String);
   --  Sets the accessible description for the row, which may be used by e.g.
   --  screen readers.
   --  Since: gtk+ 4.12
   --  @param Description the description

   function Get_Accessible_Label
      (Self : not null access Gtk_Column_View_Row_Record) return UTF8_String;
   --  Gets the accessible label of Self.
   --  Since: gtk+ 4.12
   --  @return the accessible label

   procedure Set_Accessible_Label
      (Self  : not null access Gtk_Column_View_Row_Record;
       Label : UTF8_String);
   --  Sets the accessible label for the row, which may be used by e.g. screen
   --  readers.
   --  Since: gtk+ 4.12
   --  @param Label the label

   function Get_Activatable
      (Self : not null access Gtk_Column_View_Row_Record) return Boolean;
   --  Checks if the row has been set to be activatable via
   --  Gtk.Column_View_Row.Set_Activatable.
   --  Since: gtk+ 4.12
   --  @return True if the row is activatable

   procedure Set_Activatable
      (Self        : not null access Gtk_Column_View_Row_Record;
       Activatable : Boolean);
   --  Sets Self to be activatable.
   --  If a row is activatable, double-clicking on the row, using the Return
   --  key or calling Gtk.Widget.Activate will activate the row. Activating
   --  instructs the containing columnview to emit the
   --  [signalGtk.ColumnView::activate] signal.
   --  By default, row are activatable.
   --  Since: gtk+ 4.12
   --  @param Activatable if the row should be activatable

   function Get_Focusable
      (Self : not null access Gtk_Column_View_Row_Record) return Boolean;
   --  Checks if a row item has been set to be focusable via
   --  Gtk.Column_View_Row.Set_Focusable.
   --  Since: gtk+ 4.12
   --  @return True if the row is focusable

   procedure Set_Focusable
      (Self      : not null access Gtk_Column_View_Row_Record;
       Focusable : Boolean);
   --  Sets Self to be focusable.
   --  If a row is focusable, it can be focused using the keyboard. This works
   --  similar to [methodGtk.Widget.set_focusable].
   --  Note that if row are not focusable, the contents of cells can still be
   --  focused if they are focusable.
   --  By default, rows are focusable.
   --  Since: gtk+ 4.12
   --  @param Focusable if the row should be focusable

   function Get_Item
      (Self : not null access Gtk_Column_View_Row_Record)
       return System.Address;
   --  Gets the model item that associated with Self.
   --  If Self is unbound, this function returns null.
   --  Since: gtk+ 4.12
   --  @return The item displayed

   function Get_Position
      (Self : not null access Gtk_Column_View_Row_Record) return Guint;
   --  Gets the position in the model that Self currently displays.
   --  If Self is unbound, GTK_INVALID_LIST_POSITION is returned.
   --  Since: gtk+ 4.12
   --  @return The position of this row

   function Get_Selectable
      (Self : not null access Gtk_Column_View_Row_Record) return Boolean;
   --  Checks if the row has been set to be selectable via
   --  Gtk.Column_View_Row.Set_Selectable.
   --  Do not confuse this function with
   --  [methodGtk.ColumnViewRow.get_selected].
   --  Since: gtk+ 4.12
   --  @return True if the row is selectable

   procedure Set_Selectable
      (Self       : not null access Gtk_Column_View_Row_Record;
       Selectable : Boolean);
   --  Sets Self to be selectable.
   --  If a row is selectable, clicking on the row or using the keyboard will
   --  try to select or unselect the row. Whether this succeeds is up to the
   --  model to determine, as it is managing the selected state.
   --  Note that this means that making a row non-selectable has no influence
   --  on the selected state at all. A non-selectable row may still be
   --  selected.
   --  By default, rows are selectable.
   --  Since: gtk+ 4.12
   --  @param Selectable if the row should be selectable

   function Get_Selected
      (Self : not null access Gtk_Column_View_Row_Record) return Boolean;
   --  Checks if the item is selected that this row corresponds to.
   --  The selected state is maintained by the list widget and its model and
   --  cannot be set otherwise.
   --  Since: gtk+ 4.12
   --  @return True if the item is selected.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accessible_Description_Property : constant Glib.Properties.Property_String;
   --  The accessible description to set on the row.

   Accessible_Label_Property : constant Glib.Properties.Property_String;
   --  The accessible label to set on the row.

   Activatable_Property : constant Glib.Properties.Property_Boolean;
   --  If the row can be activated by the user.

   Focusable_Property : constant Glib.Properties.Property_Boolean;
   --  If the row can be focused with the keyboard.

   Item_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Object.GObject
   --  The item for this row.

   Position_Property : constant Glib.Properties.Property_Uint;
   --  Position of the row.

   Selectable_Property : constant Glib.Properties.Property_Boolean;
   --  If the row can be selected by the user.

   Selected_Property : constant Glib.Properties.Property_Boolean;
   --  If the item in the row is currently selected.

private
   Selected_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("selected");
   Selectable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("selectable");
   Position_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("position");
   Item_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("item");
   Focusable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focusable");
   Activatable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activatable");
   Accessible_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("accessible-label");
   Accessible_Description_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("accessible-description");
end Gtk.Column_View_Row;
