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

--  Surrounds its child with a decorative frame and an optional label.
--
--  <picture> <source srcset="frame-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkFrame" src="frame.png"> </picture>
--  If present, the label is drawn inside the top edge of the frame. The
--  horizontal position of the label can be controlled with
--  [methodGtk.Frame.set_label_align].
--
--  `GtkFrame` clips its child. You can use this to add rounded corners to
--  widgets, but be aware that it also cuts off shadows.
--
--  # GtkFrame as GtkBuildable
--
--  An example of a UI definition fragment with GtkFrame:
--
--  ```xml <object class="GtkFrame"> <property name="label-widget"> <object
--  class="GtkLabel" id="frame_label"/> </property> <property name="child">
--  <object class="GtkEntry" id="frame_content"/> </property> </object> ```
--
--  # CSS nodes
--
--  ``` frame ├── <label widget> ╰── <child> ```
--
--  `GtkFrame` has a main CSS node with name "frame", which is used to draw
--  the visible border. You can set the appearance of the border using CSS
--  properties like "border-style" on this node.
--
--  # Accessibility
--
--  `GtkFrame` uses the [enumGtk.AccessibleRole.group] role.
--
--  This is a very convenient widget to visually group related widgets (like
--  groups of buttons for instance), possibly with a title to explain the
--  purpose of this group.
--
--  A Gtk_Frame has only one child, so you have to put a container like for
--  instance a Gtk_Box inside if you want the frame to surround multiple
--  widgets.
--
--  <screenshot>gtk-frame</screenshot>
--  <group>Ornaments</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;
with Interfaces.C;          use Interfaces.C;

package Gtk.Frame is

   type Gtk_Frame_Record is new Gtk_Widget_Record with null record;
   type Gtk_Frame is access all Gtk_Frame_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Frame; Label : UTF8_String := "");
   procedure Initialize
      (Self  : not null access Gtk_Frame_Record'Class;
       Label : UTF8_String := "");
   --  Creates a new `GtkFrame`, with optional label Label.
   --  If Label is null, the label is omitted.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Label the text to use as the label of the frame

   function Gtk_Frame_New (Label : UTF8_String := "") return Gtk_Frame;
   --  Creates a new `GtkFrame`, with optional label Label.
   --  If Label is null, the label is omitted.
   --  @param Label the text to use as the label of the frame

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_frame_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : not null access Gtk_Frame_Record) return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Frame.
   --  @return the child widget of Frame

   procedure Set_Child
      (Self  : not null access Gtk_Frame_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Frame.
   --  @param Child the child widget

   function Get_Label
      (Self : not null access Gtk_Frame_Record) return UTF8_String;
   --  Returns the frame labels text.
   --  If the frame's label widget is not a `GtkLabel`, null is returned.
   --  @return the text in the label, or null if there was no label widget or
   --  the label widget was not a `GtkLabel`. This string is owned by GTK and
   --  must not be modified or freed.

   procedure Set_Label
      (Self  : not null access Gtk_Frame_Record;
       Label : UTF8_String := "");
   --  Creates a new `GtkLabel` with the Label and sets it as the frame's
   --  label widget.
   --  @param Label the text to use as the label of the frame

   function Get_Label_Align
      (Self : not null access Gtk_Frame_Record) return Interfaces.C.C_float;
   --  Retrieves the X alignment of the frame's label.
   --  @return the frames X alignment

   procedure Set_Label_Align
      (Self   : not null access Gtk_Frame_Record;
       Xalign : Interfaces.C.C_float);
   --  Sets the X alignment of the frame widget's label.
   --  The default value for a newly created frame is 0.0.
   --  @param Xalign The position of the label along the top edge of the
   --  widget. A value of 0.0 represents left alignment; 1.0 represents right
   --  alignment.

   function Get_Label_Widget
      (Self : not null access Gtk_Frame_Record) return Gtk.Widget.Gtk_Widget;
   --  Retrieves the label widget for the frame.
   --  @return the label widget

   procedure Set_Label_Widget
      (Self         : not null access Gtk_Frame_Record;
       Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the label widget for the frame.
   --  This is the widget that will appear embedded in the top edge of the
   --  frame as a title.
   --  @param Label_Widget the new label widget

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Announce
      (Self     : not null access Gtk_Frame_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Frame_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Frame_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Frame_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Frame_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Frame_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Frame_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Frame_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Frame_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Frame_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Frame_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Frame_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Frame_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Frame_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Frame_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Label_Property : constant Glib.Properties.Property_String;
   --  Text of the frame's label.

   Label_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  Widget to display in place of the usual frame label.

   Label_Xalign_Property : constant Glib.Properties.Property_Float;
   --  The horizontal alignment of the label.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Accessible"
   --
   --  - "ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Frame_Record, Gtk_Frame);
   function "+"
     (Widget : access Gtk_Frame_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Frame
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Frame_Record, Gtk_Frame);
   function "+"
     (Widget : access Gtk_Frame_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Frame
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Label_Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("label-xalign");
   Label_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("label-widget");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Frame;
