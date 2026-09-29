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

--  The kind of widget that can be added to a `GtkListBox`.
--
--  [classGtk.ListBox] will automatically wrap its children in a
--  `GtkListboxRow` when necessary.
--
--  <group>Trees and Lists</group>
--  <gtkada_demo>create_list_box_controls.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Glist;            use Glib.Glist;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.List_Box_Row is

   type Gtk_List_Box_Row_Record is new Gtk_Widget_Record with null record;
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
   --  Creates a new `GtkListBoxRow`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_List_Box_Row_New return Gtk_List_Box_Row;
   --  Creates a new `GtkListBoxRow`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_list_box_row_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed (Self : not null access Gtk_List_Box_Row_Record);
   --  Marks Row as changed, causing any state that depends on this to be
   --  updated.
   --  This affects sorting, filtering and headers.
   --  Note that calls to this method must be in sync with the data used for
   --  the row functions. For instance, if the list is mirroring some external
   --  data set, and *two* rows changed in the external data set then when you
   --  call Gtk.List_Box_Row.Changed on the first row the sort function must
   --  only read the new data for the first of the two changed rows, otherwise
   --  the resorting of the rows will be wrong.
   --  This generally means that if you don't fully control the data model you
   --  have to duplicate the data that affects the listbox row functions into
   --  the row widgets themselves. Another alternative is to call
   --  [methodGtk.ListBox.invalidate_sort] on any model change, but that is
   --  more expensive.

   function Get_Activatable
      (Self : not null access Gtk_List_Box_Row_Record) return Boolean;
   --  Gets whether the row is activatable.
   --  @return True if the row is activatable

   procedure Set_Activatable
      (Self        : not null access Gtk_List_Box_Row_Record;
       Activatable : Boolean);
   --  Set whether the row is activatable.
   --  @param Activatable True to mark the row as activatable

   function Get_Child
      (Self : not null access Gtk_List_Box_Row_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Row.
   --  @return the child widget of Row. Has transfer-ownership='none'.

   procedure Set_Child
      (Self  : not null access Gtk_List_Box_Row_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Self.
   --  @param Child the child widget

   function Get_Header
      (Self : not null access Gtk_List_Box_Row_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the current header of the Row.
   --  This can be used in a [callbackGtk.ListBoxUpdateHeaderFunc] to see if
   --  there is a header set already, and if so to update the state of it.
   --  @return the current header. Has transfer-ownership='none'.

   procedure Set_Header
      (Self   : not null access Gtk_List_Box_Row_Record;
       Header : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the current header of the Row.
   --  This is only allowed to be called from a
   --  [callbackGtk.ListBoxUpdateHeaderFunc]. It will replace any existing
   --  header in the row, and be shown in front of the row in the listbox.
   --  @param Header the header

   function Get_Index
      (Self : not null access Gtk_List_Box_Row_Record) return Glib.Gint;
   --  Gets the current index of the Row in its `GtkListBox` container.
   --  @return the index of the Row, or -1 if the Row is not in a listbox

   function Get_Selectable
      (Self : not null access Gtk_List_Box_Row_Record) return Boolean;
   --  Gets whether the row can be selected.
   --  @return True if the row is selectable

   procedure Set_Selectable
      (Self       : not null access Gtk_List_Box_Row_Record;
       Selectable : Boolean);
   --  Set whether the row can be selected.
   --  @param Selectable True to mark the row as selectable

   function Is_Selected
      (Self : not null access Gtk_List_Box_Row_Record) return Boolean;
   --  Returns whether the child is currently selected in its `GtkListBox`
   --  container.
   --  @return True if Row is selected

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_List_Box_Row_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_List_Box_Row_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_List_Box_Row_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_List_Box_Row_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_List_Box_Row_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_List_Box_Row_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_List_Box_Row_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_List_Box_Row_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_List_Box_Row_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_List_Box_Row_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_List_Box_Row_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_List_Box_Row_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_List_Box_Row_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_List_Box_Row_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_List_Box_Row_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Activatable_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the ::row-activated signal will be emitted for this
   --  row.

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Selectable_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether this row can be selected.

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
   --  not), use the [signalGtk.ListBox::row-activated] signal on the row's
   --  parent `GtkListBox`.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_List_Box_Row_Record, Gtk_List_Box_Row);
   function "+"
     (Widget : access Gtk_List_Box_Row_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_List_Box_Row
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_List_Box_Row_Record, Gtk_List_Box_Row);
   function "+"
     (Widget : access Gtk_List_Box_Row_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_List_Box_Row
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Selectable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("selectable");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
   Activatable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activatable");
end Gtk.List_Box_Row;
