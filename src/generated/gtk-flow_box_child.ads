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

--  The kind of widget that can be added to a `GtkFlowBox`.
--
--  [classGtk.FlowBox] will automatically wrap its children in a
--  `GtkFlowBoxChild` when necessary.

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

package Gtk.Flow_Box_Child is

   type Gtk_Flow_Box_Child_Record is new Gtk_Widget_Record with null record;
   type Gtk_Flow_Box_Child is access all Gtk_Flow_Box_Child_Record'Class;

   function Convert (R : Gtk.Flow_Box_Child.Gtk_Flow_Box_Child) return System.Address;
   function Convert (R : System.Address) return Gtk.Flow_Box_Child.Gtk_Flow_Box_Child;
   package Flow_Box_Child_List is new Generic_List (Gtk.Flow_Box_Child.Gtk_Flow_Box_Child);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Flow_Box_Child);
   procedure Initialize
      (Self : not null access Gtk_Flow_Box_Child_Record'Class);
   --  Creates a new `GtkFlowBoxChild`.
   --  This should only be used as a child of a `GtkFlowBox`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Flow_Box_Child_New return Gtk_Flow_Box_Child;
   --  Creates a new `GtkFlowBoxChild`.
   --  This should only be used as a child of a `GtkFlowBox`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_flow_box_child_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed (Self : not null access Gtk_Flow_Box_Child_Record);
   --  Marks Child as changed, causing any state that depends on this to be
   --  updated.
   --  This affects sorting and filtering.
   --  Note that calls to this method must be in sync with the data used for
   --  the sorting and filtering functions. For instance, if the list is
   --  mirroring some external data set, and *two* children changed in the
   --  external data set when you call Gtk.Flow_Box_Child.Changed on the first
   --  child, the sort function must only read the new data for the first of
   --  the two changed children, otherwise the resorting of the children will
   --  be wrong.
   --  This generally means that if you don't fully control the data model,
   --  you have to duplicate the data that affects the sorting and filtering
   --  functions into the widgets themselves.
   --  Another alternative is to call [methodGtk.FlowBox.invalidate_sort] on
   --  any model change, but that is more expensive.

   function Get_Child
      (Self : not null access Gtk_Flow_Box_Child_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Self.
   --  @return the child widget of Self. Has transfer-ownership='none'.

   procedure Set_Child
      (Self  : not null access Gtk_Flow_Box_Child_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Self.
   --  @param Child the child widget

   function Get_Index
      (Self : not null access Gtk_Flow_Box_Child_Record) return Glib.Gint;
   --  Gets the current index of the Child in its `GtkFlowBox` container.
   --  @return the index of the Child, or -1 if the Child is not in a flow box

   function Is_Selected
      (Self : not null access Gtk_Flow_Box_Child_Record) return Boolean;
   --  Returns whether the Child is currently selected in its `GtkFlowBox`
   --  container.
   --  @return True if Child is selected

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Flow_Box_Child_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Flow_Box_Child_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Flow_Box_Child_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Flow_Box_Child_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Flow_Box_Child_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Flow_Box_Child_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Flow_Box_Child_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Flow_Box_Child_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Flow_Box_Child_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Flow_Box_Child_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Flow_Box_Child_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Flow_Box_Child_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Flow_Box_Child_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Flow_Box_Child_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Flow_Box_Child_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Flow_Box_Child_Void is not null access procedure
     (Self : access Gtk_Flow_Box_Child_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Flow_Box_Child_Record;
       Call  : Cb_Gtk_Flow_Box_Child_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Flow_Box_Child_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user activates a child widget in a `GtkFlowBox`.
   --
   --  This can happen either by clicking or double-clicking, or via a
   --  keybinding.
   --
   --  This is a [keybinding signal](class.SignalAction.html), but it can be
   --  used by applications for their own purposes.
   --
   --  The default bindings are <kbd>Space</kbd> and <kbd>Enter</kbd>.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Flow_Box_Child_Record, Gtk_Flow_Box_Child);
   function "+"
     (Widget : access Gtk_Flow_Box_Child_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Flow_Box_Child
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Flow_Box_Child_Record, Gtk_Flow_Box_Child);
   function "+"
     (Widget : access Gtk_Flow_Box_Child_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Flow_Box_Child
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Flow_Box_Child_Record, Gtk_Flow_Box_Child);
   function "+"
     (Widget : access Gtk_Flow_Box_Child_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Flow_Box_Child
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Flow_Box_Child;
