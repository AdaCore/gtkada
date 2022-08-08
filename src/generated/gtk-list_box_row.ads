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


pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Glist;      use Glib.Glist;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Glib.Variant;    use Glib.Variant;
with Gtk.Actionable;  use Gtk.Actionable;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.List_Box_Row is

   type Gtk_List_Box_Row_Record is new Gtk_Bin_Record with null record;
   type Gtk_List_Box_Row is access all Gtk_List_Box_Row_Record'Class;

   function Convert (R : Gtk.List_Box_Row.Gtk_List_Box_Row) return System.Address;
   function Convert (R : System.Address) return Gtk.List_Box_Row.Gtk_List_Box_Row;
   package List_Box_Row_List is new Generic_List (Gtk.List_Box_Row.Gtk_List_Box_Row);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_List_Box_Row);
   procedure Initialize
      (Self : not null access Gtk_List_Box_Row_Record'Class);
   --  Creates a new Gtk.List_Box_Row.Gtk_List_Box_Row, to be used as a child
   --  of a Gtk.List_Box.Gtk_List_Box.
   --  Since: gtk+ 3.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_List_Box_Row_New return Gtk_List_Box_Row;
   --  Creates a new Gtk.List_Box_Row.Gtk_List_Box_Row, to be used as a child
   --  of a Gtk.List_Box.Gtk_List_Box.
   --  Since: gtk+ 3.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_list_box_row_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed (Self : not null access Gtk_List_Box_Row_Record);
   --  Marks Row as changed, causing any state that depends on this to be
   --  updated. This affects sorting, filtering and headers.
   --  Note that calls to this method must be in sync with the data used for
   --  the row functions. For instance, if the list is mirroring some external
   --  data set, and *two* rows changed in the external data set then when you
   --  call Gtk.List_Box_Row.Changed on the first row the sort function must
   --  only read the new data for the first of the two changed rows, otherwise
   --  the resorting of the rows will be wrong.
   --  This generally means that if you don't fully control the data model you
   --  have to duplicate the data that affects the listbox row functions into
   --  the row widgets themselves. Another alternative is to call
   --  Gtk.List_Box.Invalidate_Sort on any model change, but that is more
   --  expensive.
   --  Since: gtk+ 3.10

   function Get_Activatable
      (Self : not null access Gtk_List_Box_Row_Record) return Boolean;
   --  Gets the value of the Gtk.List_Box_Row.Gtk_List_Box_Row:activatable
   --  property for this row.
   --  Since: gtk+ 3.14

   procedure Set_Activatable
      (Self        : not null access Gtk_List_Box_Row_Record;
       Activatable : Boolean);
   --  Set the Gtk.List_Box_Row.Gtk_List_Box_Row:activatable property for this
   --  row.
   --  Since: gtk+ 3.14
   --  "activatable": True to mark the row as activatable

   function Get_Header
      (Self : not null access Gtk_List_Box_Row_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the current header of the Row. This can be used in a
   --  Gtk_List_Box_Update_Header_Func to see if there is a header set already,
   --  and if so to update the state of it.
   --  Since: gtk+ 3.10

   procedure Set_Header
      (Self   : not null access Gtk_List_Box_Row_Record;
       Header : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the current header of the Row. This is only allowed to be called
   --  from a Gtk_List_Box_Update_Header_Func. It will replace any existing
   --  header in the row, and be shown in front of the row in the listbox.
   --  Since: gtk+ 3.10
   --  "header": the header, or null

   function Get_Index
      (Self : not null access Gtk_List_Box_Row_Record) return Glib.Gint;
   --  Gets the current index of the Row in its Gtk.List_Box.Gtk_List_Box
   --  container.
   --  Since: gtk+ 3.10

   function Get_Selectable
      (Self : not null access Gtk_List_Box_Row_Record) return Boolean;
   --  Gets the value of the Gtk.List_Box_Row.Gtk_List_Box_Row:selectable
   --  property for this row.
   --  Since: gtk+ 3.14

   procedure Set_Selectable
      (Self       : not null access Gtk_List_Box_Row_Record;
       Selectable : Boolean);
   --  Set the Gtk.List_Box_Row.Gtk_List_Box_Row:selectable property for this
   --  row.
   --  Since: gtk+ 3.14
   --  "selectable": True to mark the row as selectable

   function Is_Selected
      (Self : not null access Gtk_List_Box_Row_Record) return Boolean;
   --  Returns whether the child is currently selected in its
   --  Gtk.List_Box.Gtk_List_Box container.
   --  Since: gtk+ 3.14

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_List_Box_Row_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_List_Box_Row_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_List_Box_Row_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_List_Box_Row_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_List_Box_Row_Record;
       Detailed_Action_Name : UTF8_String);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Activatable_Property : constant Glib.Properties.Property_Boolean;
   --  The property determines whether the
   --  Gtk.List_Box.Gtk_List_Box::row-activated signal will be emitted for this
   --  row.

   Selectable_Property : constant Glib.Properties.Property_Boolean;
   --  The property determines whether this row can be selected.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_List_Box_Row_Void is not null access procedure
     (Self : access Gtk_List_Box_Row_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_List_Box_Row_Record;
       Call  : Cb_Gtk_List_Box_Row_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_List_Box_Row_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This is a keybinding signal, which will cause this row to be activated.
   --
   --  If you want to be notified when the user activates a row (by key or
   --  not), use the Gtk.List_Box.Gtk_List_Box::row-activated signal on the
   --  row's parent Gtk.List_Box.Gtk_List_Box.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Actionable"
   --
   --  - "Buildable"

   package Implements_Gtk_Actionable is new Glib.Types.Implements
     (Gtk.Actionable.Gtk_Actionable, Gtk_List_Box_Row_Record, Gtk_List_Box_Row);
   function "+"
     (Widget : access Gtk_List_Box_Row_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_List_Box_Row
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_List_Box_Row_Record, Gtk_List_Box_Row);
   function "+"
     (Widget : access Gtk_List_Box_Row_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_List_Box_Row
   renames Implements_Gtk_Buildable.To_Object;

private
   Selectable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("selectable");
   Activatable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activatable");
end Gtk.List_Box_Row;
