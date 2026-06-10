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

--  Presents a horizontal bar of items that pop up menus when clicked.
--
--  <picture> <source srcset="menubar-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkPopoverMenuBar" src="menubar.png">
--  </picture>
--  The only way to create instances of `GtkPopoverMenuBar` is from a
--  `GMenuModel`.
--
--  # CSS nodes
--
--  ``` menubar ├── item[.active] ┊ ╰── popover ╰── item ╰── popover ```
--
--  `GtkPopoverMenuBar` has a single CSS node with name menubar, below which
--  each item has its CSS node, and below that the corresponding popover.
--
--  The item whose popover is currently open gets the .active style class.
--
--  # Accessibility
--
--  `GtkPopoverMenuBar` uses the [enumGtk.AccessibleRole.menu_bar] role, the
--  menu items use the [enumGtk.AccessibleRole.menu_item] role and the menus
--  use the [enumGtk.AccessibleRole.menu] role.
--
--  <group>Menus and Toolbars</group>
--  <gtkada_demo>create_menu.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Menu_Model;       use Glib.Menu_Model;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Popover_Menu_Bar is

   type Gtk_Popover_Menu_Bar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Popover_Menu_Bar is access all Gtk_Popover_Menu_Bar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_From_Model
      (Self  : out Gtk_Popover_Menu_Bar;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   procedure Initialize_From_Model
      (Self  : not null access Gtk_Popover_Menu_Bar_Record'Class;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Creates a `GtkPopoverMenuBar` from a `GMenuModel`.
   --  Initialize_From_Model does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Model a `GMenuModel`

   function Gtk_Popover_Menu_Bar_New_From_Model
      (Model : access Glib.Menu_Model.Gmenu_Model_Record'Class)
       return Gtk_Popover_Menu_Bar;
   --  Creates a `GtkPopoverMenuBar` from a `GMenuModel`.
   --  @param Model a `GMenuModel`

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_popover_menu_bar_get_type");

   -------------
   -- Methods --
   -------------

   function Add_Child
      (Self  : not null access Gtk_Popover_Menu_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Id    : UTF8_String) return Boolean;
   --  Adds a custom widget to a generated menubar.
   --  For this to work, the menu model of Bar must have an item with a
   --  `custom` attribute that matches Id.
   --  @param Child the `GtkWidget` to add
   --  @param Id the ID to insert Child at
   --  @return True if Id was found and the widget added

   function Get_Menu_Model
      (Self : not null access Gtk_Popover_Menu_Bar_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Returns the model from which the contents of Bar are taken.
   --  @return a `GMenuModel`

   procedure Set_Menu_Model
      (Self  : not null access Gtk_Popover_Menu_Bar_Record;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets a menu model from which Bar should take its contents.
   --  @param Model a `GMenuModel`

   function Remove_Child
      (Self  : not null access Gtk_Popover_Menu_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Removes a widget that has previously been added with
   --  Gtk.Popover_Menu_Bar.Add_Child.
   --  @param Child the `GtkWidget` to remove
   --  @return True if the widget was removed

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Popover_Menu_Bar_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Popover_Menu_Bar_Record)
       return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Popover_Menu_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Popover_Menu_Bar_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Popover_Menu_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Popover_Menu_Bar_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Popover_Menu_Bar_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Popover_Menu_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Popover_Menu_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Popover_Menu_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Popover_Menu_Bar_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Popover_Menu_Bar_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Popover_Menu_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Popover_Menu_Bar_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Popover_Menu_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Menu_Model_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model
   --  The `GMenuModel` from which the menu bar is created.
   --
   --  The model should only contain submenus as toplevel elements.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Popover_Menu_Bar_Record, Gtk_Popover_Menu_Bar);
   function "+"
     (Widget : access Gtk_Popover_Menu_Bar_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Popover_Menu_Bar
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Popover_Menu_Bar_Record, Gtk_Popover_Menu_Bar);
   function "+"
     (Widget : access Gtk_Popover_Menu_Bar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Popover_Menu_Bar
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Popover_Menu_Bar_Record, Gtk_Popover_Menu_Bar);
   function "+"
     (Widget : access Gtk_Popover_Menu_Bar_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Popover_Menu_Bar
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Menu_Model_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("menu-model");
end Gtk.Popover_Menu_Bar;
