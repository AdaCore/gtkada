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

--  Uses a sidebar to switch between `GtkStack` pages.
--
--  <picture> <source srcset="sidebar-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkStackSidebar" src="sidebar.png"> </picture>
--  In order to use a `GtkStackSidebar`, you simply use a `GtkStack` to
--  organize your UI flow, and add the sidebar to your sidebar area. You can
--  use [methodGtk.StackSidebar.set_stack] to connect the `GtkStackSidebar` to
--  the `GtkStack`.
--
--  # CSS nodes
--
--  `GtkStackSidebar` has a single CSS node with name stacksidebar and style
--  class .sidebar.
--
--  When circumstances require it, `GtkStackSidebar` adds the .needs-attention
--  style class to the widgets representing the stack pages.
--
--  <group>Layout containers</group>
--  <gtkada_demo>create_stack_sidebar.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Stack;             use Gtk.Stack;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Stack_Sidebar is

   type Gtk_Stack_Sidebar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Stack_Sidebar is access all Gtk_Stack_Sidebar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Stack_Sidebar);
   procedure Initialize
      (Self : not null access Gtk_Stack_Sidebar_Record'Class);
   --  Creates a new `GtkStackSidebar`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Stack_Sidebar_New return Gtk_Stack_Sidebar;
   --  Creates a new `GtkStackSidebar`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_stack_sidebar_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Stack
      (Self : not null access Gtk_Stack_Sidebar_Record)
       return Gtk.Stack.Gtk_Stack;
   --  Retrieves the stack.
   --  @return the associated `GtkStack` or null if none has been set
   --  explicitly. Has transfer-ownership='none'.

   procedure Set_Stack
      (Self  : not null access Gtk_Stack_Sidebar_Record;
       Stack : not null access Gtk.Stack.Gtk_Stack_Record'Class);
   --  Set the `GtkStack` associated with this `GtkStackSidebar`.
   --  The sidebar widget will automatically update according to the order and
   --  items within the given `GtkStack`.
   --  @param Stack a `GtkStack`

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Stack_Sidebar_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Stack_Sidebar_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Stack_Sidebar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Stack_Sidebar_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Stack_Sidebar_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Stack_Sidebar_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Stack_Sidebar_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Stack_Sidebar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Stack_Sidebar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Stack_Sidebar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Stack_Sidebar_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Stack_Sidebar_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Stack_Sidebar_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Stack_Sidebar_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Stack_Sidebar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Stack_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Stack.Gtk_Stack
   --  The stack.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Stack_Sidebar_Record, Gtk_Stack_Sidebar);
   function "+"
     (Widget : access Gtk_Stack_Sidebar_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Stack_Sidebar
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Stack_Sidebar_Record, Gtk_Stack_Sidebar);
   function "+"
     (Widget : access Gtk_Stack_Sidebar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Stack_Sidebar
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Stack_Sidebar_Record, Gtk_Stack_Sidebar);
   function "+"
     (Widget : access Gtk_Stack_Sidebar_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Stack_Sidebar
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Stack_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("stack");
end Gtk.Stack_Sidebar;
