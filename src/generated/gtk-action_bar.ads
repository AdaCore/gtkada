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

--  Presents contextual actions.
--
--  <picture> <source srcset="action-bar-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkActionBar"
--  src="action-bar.png"> </picture>
--  `GtkActionBar` is expected to be displayed below the content and expand
--  horizontally to fill the area.
--
--  It allows placing children at the start or the end. In addition, it
--  contains an internal centered box which is centered with respect to the
--  full width of the box, even if the children at either side take up
--  different amounts of space.
--
--  # GtkActionBar as GtkBuildable
--
--  The `GtkActionBar` implementation of the `GtkBuildable` interface supports
--  adding children at the start or end sides by specifying "start" or "end" as
--  the "type" attribute of a `<child>` element, or setting the center widget
--  by specifying "center" value.
--
--  # CSS nodes
--
--  ``` actionbar ╰── revealer ╰── box ├── box.start │ ╰── [start children]
--  ├── [center widget] ╰── box.end ╰── [end children] ```
--
--  A `GtkActionBar`'s CSS node is called `actionbar`. It contains a
--  `revealer` subnode, which contains a `box` subnode, which contains two
--  `box` subnodes at the start and end of the action bar, with `start` and
--  `end` style classes respectively, as well as a center node that represents
--  the center child.
--
--  Each of the boxes contains children packed for that side.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Action_Bar is

   type Gtk_Action_Bar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Action_Bar is access all Gtk_Action_Bar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Action_Bar);
   procedure Initialize (Self : not null access Gtk_Action_Bar_Record'Class);
   --  Creates a new action bar widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Action_Bar_New return Gtk_Action_Bar;
   --  Creates a new action bar widget.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_action_bar_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Center_Widget
      (Self : not null access Gtk_Action_Bar_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the center bar widget of the bar.
   --  @return the center widget. Has transfer-ownership='none'.

   procedure Set_Center_Widget
      (Self          : not null access Gtk_Action_Bar_Record;
       Center_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the center widget for the action bar.
   --  @param Center_Widget a widget to use for the center

   function Get_Revealed
      (Self : not null access Gtk_Action_Bar_Record) return Boolean;
   --  Gets whether the contents of the action bar are revealed.
   --  @return the current value of the [propertyGtk.ActionBar:revealed]
   --  property

   procedure Set_Revealed
      (Self     : not null access Gtk_Action_Bar_Record;
       Revealed : Boolean);
   --  Reveals or conceals the content of the action bar.
   --  Note: this does not show or hide the action bar in the
   --  [propertyGtk.Widget:visible] sense, so revealing has no effect if the
   --  action bar is hidden.
   --  @param Revealed the new value for the property

   procedure Pack_End
      (Self  : not null access Gtk_Action_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a child to the action bar, packed with reference to the end of the
   --  action bar.
   --  @param Child the widget to be added

   procedure Pack_Start
      (Self  : not null access Gtk_Action_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a child to the action, packed with reference to the start of the
   --  action bar.
   --  @param Child the widget to be added

   procedure Remove
      (Self  : not null access Gtk_Action_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a child from the action bar.
   --  @param Child the widget to be removed

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Action_Bar_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Action_Bar_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Action_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Action_Bar_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Action_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Action_Bar_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Action_Bar_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Action_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Action_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Action_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Action_Bar_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Action_Bar_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Action_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Action_Bar_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Action_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Revealed_Property : constant Glib.Properties.Property_Boolean;
   --  Controls whether the action bar shows its contents.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Action_Bar_Record, Gtk_Action_Bar);
   function "+"
     (Widget : access Gtk_Action_Bar_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Action_Bar
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Action_Bar_Record, Gtk_Action_Bar);
   function "+"
     (Widget : access Gtk_Action_Bar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Action_Bar
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Action_Bar_Record, Gtk_Action_Bar);
   function "+"
     (Widget : access Gtk_Action_Bar_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Action_Bar
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Revealed_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("revealed");
end Gtk.Action_Bar;
