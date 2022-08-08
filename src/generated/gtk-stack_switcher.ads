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

--  <description>
--  The GtkStackSwitcher widget acts as a controller for a
--  Gtk.Stack.Gtk_Stack; it shows a row of buttons to switch between the
--  various pages of the associated stack widget.
--
--  All the content for the buttons comes from the child properties of the
--  Gtk.Stack.Gtk_Stack; the button visibility in a
--  Gtk.Stack_Switcher.Gtk_Stack_Switcher widget is controlled by the
--  visibility of the child in the Gtk.Stack.Gtk_Stack.
--
--  It is possible to associate multiple Gtk.Stack_Switcher.Gtk_Stack_Switcher
--  widgets with the same Gtk.Stack.Gtk_Stack widget.
--
--  The GtkStackSwitcher widget was added in 3.10.
--
--  # CSS nodes
--
--  GtkStackSwitcher has a single CSS node named stackswitcher and style class
--  .stack-switcher.
--
--  When circumstances require it, GtkStackSwitcher adds the .needs-attention
--  style class to the widgets representing the stack pages.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Box;         use Gtk.Box;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Stack;       use Gtk.Stack;

package Gtk.Stack_Switcher is

   type Gtk_Stack_Switcher_Record is new Gtk_Box_Record with null record;
   type Gtk_Stack_Switcher is access all Gtk_Stack_Switcher_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Stack_Switcher);
   procedure Initialize
      (Self : not null access Gtk_Stack_Switcher_Record'Class);
   --  Create a new Gtk.Stack_Switcher.Gtk_Stack_Switcher.
   --  Since: gtk+ 3.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Stack_Switcher_New return Gtk_Stack_Switcher;
   --  Create a new Gtk.Stack_Switcher.Gtk_Stack_Switcher.
   --  Since: gtk+ 3.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_stack_switcher_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Stack
      (Self : not null access Gtk_Stack_Switcher_Record)
       return Gtk.Stack.Gtk_Stack;
   --  Retrieves the stack. See Gtk.Stack_Switcher.Set_Stack.
   --  Since: gtk+ 3.10

   procedure Set_Stack
      (Self  : not null access Gtk_Stack_Switcher_Record;
       Stack : access Gtk.Stack.Gtk_Stack_Record'Class);
   --  Sets the stack to control.
   --  Since: gtk+ 3.10
   --  "stack": a Gtk.Stack.Gtk_Stack

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Stack_Switcher_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Stack_Switcher_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Icon_Size_Property : constant Glib.Properties.Property_Int;
   --  Use the "icon-size" property to change the size of the image displayed
   --  when a Gtk.Stack_Switcher.Gtk_Stack_Switcher is displaying icons.

   Stack_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Stack.Gtk_Stack

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Stack_Switcher_Record, Gtk_Stack_Switcher);
   function "+"
     (Widget : access Gtk_Stack_Switcher_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Stack_Switcher
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Stack_Switcher_Record, Gtk_Stack_Switcher);
   function "+"
     (Widget : access Gtk_Stack_Switcher_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Stack_Switcher
   renames Implements_Gtk_Orientable.To_Object;

private
   Stack_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("stack");
   Icon_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("icon-size");
end Gtk.Stack_Switcher;
