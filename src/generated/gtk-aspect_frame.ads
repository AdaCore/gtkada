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

--  Preserves the aspect ratio of its child.
--
--  The frame can respect the aspect ratio of the child widget, or use its own
--  aspect ratio.
--
--  # CSS nodes
--
--  `GtkAspectFrame` uses a CSS node with name `aspectframe`.
--
--  # Accessibility
--
--  Until GTK 4.10, `GtkAspectFrame` used the [enumGtk.AccessibleRole.group]
--  role.
--
--  Starting from GTK 4.12, `GtkAspectFrame` uses the
--  [enumGtk.AccessibleRole.generic] role.
--
--  A Gtk_Aspect_Frame keeps its one child at a fixed aspect ratio (width
--  divided by height) however much the frame itself is resized, using the
--  Xalign and Yalign properties to place the child in any space left over.
--  With Obey_Child set, the ratio is taken from the child's own size request
--  instead of from Ratio.
--
--  Unlike Gtk_Frame it draws no border and has no label.
--
--  <group>Ornaments</group>
--  <gtkada_demo>create_aspect_frame.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;
with Interfaces.C;          use Interfaces.C;

package Gtk.Aspect_Frame is

   type Gtk_Aspect_Frame_Record is new Gtk_Widget_Record with null record;
   type Gtk_Aspect_Frame is access all Gtk_Aspect_Frame_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gtk_Aspect_Frame;
       Xalign     : Interfaces.C.C_float;
       Yalign     : Interfaces.C.C_float;
       Ratio      : Interfaces.C.C_float;
       Obey_Child : Boolean);
   procedure Initialize
      (Self       : not null access Gtk_Aspect_Frame_Record'Class;
       Xalign     : Interfaces.C.C_float;
       Yalign     : Interfaces.C.C_float;
       Ratio      : Interfaces.C.C_float;
       Obey_Child : Boolean);
   --  Create a new `GtkAspectFrame`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Xalign Horizontal alignment of the child within the parent.
   --  Ranges from 0.0 (left aligned) to 1.0 (right aligned)
   --  @param Yalign Vertical alignment of the child within the parent. Ranges
   --  from 0.0 (top aligned) to 1.0 (bottom aligned)
   --  @param Ratio The desired aspect ratio.
   --  @param Obey_Child If True, Ratio is ignored, and the aspect ratio is
   --  taken from the requistion of the child.

   function Gtk_Aspect_Frame_New
      (Xalign     : Interfaces.C.C_float;
       Yalign     : Interfaces.C.C_float;
       Ratio      : Interfaces.C.C_float;
       Obey_Child : Boolean) return Gtk_Aspect_Frame;
   --  Create a new `GtkAspectFrame`.
   --  @param Xalign Horizontal alignment of the child within the parent.
   --  Ranges from 0.0 (left aligned) to 1.0 (right aligned)
   --  @param Yalign Vertical alignment of the child within the parent. Ranges
   --  from 0.0 (top aligned) to 1.0 (bottom aligned)
   --  @param Ratio The desired aspect ratio.
   --  @param Obey_Child If True, Ratio is ignored, and the aspect ratio is
   --  taken from the requistion of the child.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_aspect_frame_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : not null access Gtk_Aspect_Frame_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Self.
   --  @return the child widget of Self. Has transfer-ownership='none'.

   procedure Set_Child
      (Self  : not null access Gtk_Aspect_Frame_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Self.
   --  @param Child the child widget

   function Get_Obey_Child
      (Self : not null access Gtk_Aspect_Frame_Record) return Boolean;
   --  Returns whether the child's size request should override the set aspect
   --  ratio of the `GtkAspectFrame`.
   --  @return whether to obey the child's size request

   procedure Set_Obey_Child
      (Self       : not null access Gtk_Aspect_Frame_Record;
       Obey_Child : Boolean);
   --  Sets whether the aspect ratio of the child's size request should
   --  override the set aspect ratio of the `GtkAspectFrame`.
   --  @param Obey_Child If True, Ratio is ignored, and the aspect ratio is
   --  taken from the requisition of the child.

   function Get_Ratio
      (Self : not null access Gtk_Aspect_Frame_Record)
       return Interfaces.C.C_float;
   --  Returns the desired aspect ratio of the child.
   --  @return the desired aspect ratio

   procedure Set_Ratio
      (Self  : not null access Gtk_Aspect_Frame_Record;
       Ratio : Interfaces.C.C_float);
   --  Sets the desired aspect ratio of the child.
   --  @param Ratio aspect ratio of the child

   function Get_Xalign
      (Self : not null access Gtk_Aspect_Frame_Record)
       return Interfaces.C.C_float;
   --  Returns the horizontal alignment of the child within the allocation of
   --  the `GtkAspectFrame`.
   --  @return the horizontal alignment

   procedure Set_Xalign
      (Self   : not null access Gtk_Aspect_Frame_Record;
       Xalign : Interfaces.C.C_float);
   --  Sets the horizontal alignment of the child within the allocation of the
   --  `GtkAspectFrame`.
   --  @param Xalign horizontal alignment, from 0.0 (left aligned) to 1.0
   --  (right aligned)

   function Get_Yalign
      (Self : not null access Gtk_Aspect_Frame_Record)
       return Interfaces.C.C_float;
   --  Returns the vertical alignment of the child within the allocation of
   --  the `GtkAspectFrame`.
   --  @return the vertical alignment

   procedure Set_Yalign
      (Self   : not null access Gtk_Aspect_Frame_Record;
       Yalign : Interfaces.C.C_float);
   --  Sets the vertical alignment of the child within the allocation of the
   --  `GtkAspectFrame`.
   --  @param Yalign horizontal alignment, from 0.0 (top aligned) to 1.0
   --  (bottom aligned)

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Aspect_Frame_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Aspect_Frame_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Aspect_Frame_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Aspect_Frame_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Aspect_Frame_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Aspect_Frame_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Aspect_Frame_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Aspect_Frame_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Aspect_Frame_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Aspect_Frame_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Aspect_Frame_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Aspect_Frame_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Aspect_Frame_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Aspect_Frame_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Aspect_Frame_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Obey_Child_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `GtkAspectFrame` should use the aspect ratio of its child.

   Ratio_Property : constant Glib.Properties.Property_Float;
   --  The aspect ratio to be used by the `GtkAspectFrame`.
   --
   --  This property is only used if [propertyGtk.AspectFrame:obey-child] is
   --  set to False.

   Xalign_Property : constant Glib.Properties.Property_Float;
   --  The horizontal alignment of the child.

   Yalign_Property : constant Glib.Properties.Property_Float;
   --  The vertical alignment of the child.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Aspect_Frame_Record, Gtk_Aspect_Frame);
   function "+"
     (Widget : access Gtk_Aspect_Frame_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Aspect_Frame
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Aspect_Frame_Record, Gtk_Aspect_Frame);
   function "+"
     (Widget : access Gtk_Aspect_Frame_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Aspect_Frame
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Aspect_Frame_Record, Gtk_Aspect_Frame);
   function "+"
     (Widget : access Gtk_Aspect_Frame_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Aspect_Frame
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Ratio_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("ratio");
   Obey_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("obey-child");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Aspect_Frame;
