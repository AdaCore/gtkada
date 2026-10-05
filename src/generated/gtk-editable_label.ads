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

--  Allows users to edit the displayed text by switching to an "edit mode".
--
--  <picture> <source srcset="editable-label-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example
--  GtkEditableLabel" src="editable-label.png"> </picture>
--  `GtkEditableLabel` does not have API of its own, but it implements the
--  [ifaceGtk.Editable] interface.
--
--  The default bindings for activating the edit mode is to click or press the
--  Enter key. The default bindings for leaving the edit mode are the Enter key
--  (to save the results) or the Escape key (to cancel the editing).
--
--  # Shortcuts and Gestures
--
--  `GtkEditableLabel` supports the following keyboard shortcuts:
--
--  - <kbd>Enter</kbd> starts editing. - <kbd>Escape</kbd> stops editing.
--
--  # Actions
--
--  `GtkEditableLabel` defines a set of built-in actions:
--
--  - `editing.starts` switches the widget into editing mode. - `editing.stop`
--  switches the widget out of editing mode.
--
--  # CSS nodes
--
--  ``` editablelabel[.editing] ╰── stack ├── label ╰── text ```
--
--  `GtkEditableLabel` has a main node with the name editablelabel. When the
--  entry is in editing mode, it gets the .editing style class.
--
--  For all the subnodes added to the text node in various situations, see
--  [classGtk.Text].

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Editable;          use Gtk.Editable;
with Gtk.Widget;            use Gtk.Widget;
with Interfaces.C;          use Interfaces.C;

package Gtk.Editable_Label is

   type Gtk_Editable_Label_Record is new Gtk_Widget_Record with null record;
   type Gtk_Editable_Label is access all Gtk_Editable_Label_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Editable_Label; Str : UTF8_String);
   procedure Initialize
      (Self : not null access Gtk_Editable_Label_Record'Class;
       Str  : UTF8_String);
   --  Creates a new `GtkEditableLabel` widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Str the text for the label

   function Gtk_Editable_Label_New
      (Str : UTF8_String) return Gtk_Editable_Label;
   --  Creates a new `GtkEditableLabel` widget.
   --  @param Str the text for the label

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_editable_label_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Editing
      (Self : not null access Gtk_Editable_Label_Record) return Boolean;
   --  Returns whether the label is currently in "editing mode".
   --  @return True if Self is currently in editing mode

   procedure Start_Editing
      (Self : not null access Gtk_Editable_Label_Record);
   --  Switches the label into "editing mode".

   procedure Stop_Editing
      (Self   : not null access Gtk_Editable_Label_Record;
       Commit : Boolean);
   --  Switches the label out of "editing mode".
   --  If Commit is True, the resulting text is kept as the
   --  [propertyGtk.Editable:text] property value, otherwise the resulting text
   --  is discarded and the label will keep its previous
   --  [propertyGtk.Editable:text] property value.
   --  @param Commit whether to set the edited text on the label

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Editable_Label_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Editable_Label_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Editable_Label_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Editable_Label_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Editable_Label_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Editable_Label_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Editable_Label_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Editable_Label_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Editable_Label_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Editable_Label_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Editable_Label_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Editable_Label_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Editable_Label_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Editable_Label_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Editable_Label_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Delegate_Get_Accessible_Platform_State
      (Self  : not null access Gtk_Editable_Label_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Delete_Selection
      (Self : not null access Gtk_Editable_Label_Record);

   procedure Delete_Text
      (Self      : not null access Gtk_Editable_Label_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);

   procedure Finish_Delegate
      (Self : not null access Gtk_Editable_Label_Record);

   function Get_Alignment
      (Self : not null access Gtk_Editable_Label_Record)
       return Interfaces.C.C_float;

   procedure Set_Alignment
      (Self   : not null access Gtk_Editable_Label_Record;
       Xalign : Interfaces.C.C_float);

   function Get_Chars
      (Self      : not null access Gtk_Editable_Label_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String;

   function Get_Delegate
      (Self : not null access Gtk_Editable_Label_Record)
       return Gtk.Editable.Gtk_Editable;

   function Get_Editable
      (Self : not null access Gtk_Editable_Label_Record) return Boolean;

   procedure Set_Editable
      (Self        : not null access Gtk_Editable_Label_Record;
       Is_Editable : Boolean);

   function Get_Enable_Undo
      (Self : not null access Gtk_Editable_Label_Record) return Boolean;

   procedure Set_Enable_Undo
      (Self        : not null access Gtk_Editable_Label_Record;
       Enable_Undo : Boolean);

   function Get_Max_Width_Chars
      (Self : not null access Gtk_Editable_Label_Record) return Glib.Gint;

   procedure Set_Max_Width_Chars
      (Self    : not null access Gtk_Editable_Label_Record;
       N_Chars : Glib.Gint);

   function Get_Position
      (Self : not null access Gtk_Editable_Label_Record) return Glib.Gint;

   procedure Set_Position
      (Self     : not null access Gtk_Editable_Label_Record;
       Position : Glib.Gint);

   procedure Get_Selection_Bounds
      (Self          : not null access Gtk_Editable_Label_Record;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean);

   function Get_Text
      (Self : not null access Gtk_Editable_Label_Record) return UTF8_String;

   procedure Set_Text
      (Self : not null access Gtk_Editable_Label_Record;
       Text : UTF8_String);

   function Get_Width_Chars
      (Self : not null access Gtk_Editable_Label_Record) return Glib.Gint;

   procedure Set_Width_Chars
      (Self    : not null access Gtk_Editable_Label_Record;
       N_Chars : Glib.Gint);

   procedure Init_Delegate
      (Self : not null access Gtk_Editable_Label_Record);

   procedure Insert_Text
      (Self     : not null access Gtk_Editable_Label_Record;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Position : in out Glib.Gint);

   procedure Select_Region
      (Self      : not null access Gtk_Editable_Label_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Editing_Property : constant Glib.Properties.Property_Boolean;
   --  This property is True while the widget is in edit mode.

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
   --
   --  - "Gtk.Editable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Editable_Label_Record, Gtk_Editable_Label);
   function "+"
     (Widget : access Gtk_Editable_Label_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Editable_Label
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Editable_Label_Record, Gtk_Editable_Label);
   function "+"
     (Widget : access Gtk_Editable_Label_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Editable_Label
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Editable_Label_Record, Gtk_Editable_Label);
   function "+"
     (Widget : access Gtk_Editable_Label_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Editable_Label
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Editable is new Glib.Types.Implements
     (Gtk.Editable.Gtk_Editable, Gtk_Editable_Label_Record, Gtk_Editable_Label);
   function "+"
     (Widget : access Gtk_Editable_Label_Record'Class)
   return Gtk.Editable.Gtk_Editable
   renames Implements_Gtk_Editable.To_Interface;
   function "-"
     (Interf : Gtk.Editable.Gtk_Editable)
   return Gtk_Editable_Label
   renames Implements_Gtk_Editable.To_Object;

private
   Editing_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editing");
end Gtk.Editable_Label;
