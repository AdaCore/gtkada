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

--  Places its child widgets at fixed positions and with fixed sizes.
--
--  `GtkFixed` performs no automatic layout management.
--
--  For most applications, you should not use this container! It keeps you
--  from having to learn about the other GTK containers, but it results in
--  broken applications. With `GtkFixed`, the following things will result in
--  truncated text, overlapping widgets, and other display bugs:
--
--  - Themes, which may change widget sizes.
--
--  - Fonts other than the one you used to write the app will of course change
--  the size of widgets containing text; keep in mind that users may use a
--  larger font because of difficulty reading the default, or they may be using
--  a different OS that provides different fonts.
--
--  - Translation of text into other languages changes its size. Also, display
--  of non-English text will use a different font in many cases.
--
--  In addition, `GtkFixed` does not pay attention to text direction and thus
--  may produce unwanted results if your app is run under right-to-left
--  languages such as Hebrew or Arabic. That is: normally GTK will order
--  containers appropriately for the text direction, e.g. to put labels to the
--  right of the thing they label when using an RTL language, but it can't do
--  that with `GtkFixed`. So if you need to reorder widgets depending on the
--  text direction, you would need to manually detect it and adjust child
--  positions accordingly.
--
--  Finally, fixed positioning makes it kind of annoying to add/remove UI
--  elements, since you have to reposition all the other elements. This is a
--  long-term maintenance problem for your application.
--
--  If you know none of these things are an issue for your application, and
--  prefer the simplicity of `GtkFixed`, by all means use the widget. But you
--  should be aware of the tradeoffs.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Types;            use Glib.Types;
with Gsk.Transform;         use Gsk.Transform;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Fixed is

   type Gtk_Fixed_Record is new Gtk_Widget_Record with null record;
   type Gtk_Fixed is access all Gtk_Fixed_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Fixed);
   procedure Initialize (Self : not null access Gtk_Fixed_Record'Class);
   --  Creates a new `GtkFixed`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Fixed_New return Gtk_Fixed;
   --  Creates a new `GtkFixed`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_fixed_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Child_Position
      (Self   : not null access Gtk_Fixed_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : out Gdouble;
       Y      : out Gdouble);
   --  Retrieves the translation transformation of the given child `GtkWidget`
   --  in the `GtkFixed`.
   --  See also: [methodGtk.Fixed.get_child_transform].
   --  @param Widget a child of Fixed
   --  @param X the horizontal position of the Widget
   --  @param Y the vertical position of the Widget

   function Get_Child_Transform
      (Self   : not null access Gtk_Fixed_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gsk.Transform.Gsk_Transform;
   --  Retrieves the transformation for Widget set using
   --  Gtk.Fixed.Set_Child_Transform.
   --  @param Widget a `GtkWidget`, child of Fixed
   --  @return a `GskTransform`. Has transfer-ownership='none'.

   procedure Set_Child_Transform
      (Self      : not null access Gtk_Fixed_Record;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Transform : Gsk.Transform.Gsk_Transform);
   --  Sets the transformation for Widget.
   --  This is a convenience function that retrieves the
   --  [classGtk.FixedLayoutChild] instance associated to Widget and calls
   --  [methodGtk.FixedLayoutChild.set_transform].
   --  @param Widget a `GtkWidget`, child of Fixed
   --  @param Transform the transformation assigned to Widget to reset
   --  Widget's transform

   procedure Move
      (Self   : not null access Gtk_Fixed_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Gdouble;
       Y      : Gdouble);
   --  Sets a translation transformation to the given X and Y coordinates to
   --  the child Widget of the `GtkFixed`.
   --  @param Widget the child widget
   --  @param X the horizontal position to move the widget to
   --  @param Y the vertical position to move the widget to

   procedure Put
      (Self   : not null access Gtk_Fixed_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Gdouble;
       Y      : Gdouble);
   --  Adds a widget to a `GtkFixed` at the given position.
   --  @param Widget the widget to add
   --  @param X the horizontal position to place the widget at
   --  @param Y the vertical position to place the widget at

   procedure Remove
      (Self   : not null access Gtk_Fixed_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a child from Fixed.
   --  @param Widget the child widget to remove

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Fixed_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Fixed_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Fixed_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Fixed_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Fixed_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Fixed_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Fixed_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Fixed_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Fixed_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Fixed_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Fixed_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Fixed_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Fixed_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Fixed_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Fixed_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Fixed_Record, Gtk_Fixed);
   function "+"
     (Widget : access Gtk_Fixed_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Fixed
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Fixed_Record, Gtk_Fixed);
   function "+"
     (Widget : access Gtk_Fixed_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Fixed
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Fixed_Record, Gtk_Fixed);
   function "+"
     (Widget : access Gtk_Fixed_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Fixed
   renames Implements_Gtk_Constraint_Target.To_Object;

end Gtk.Fixed;
