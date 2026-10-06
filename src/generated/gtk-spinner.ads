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

--  Displays an icon-size spinning animation.
--
--  It is often used as an alternative to a [classGtk.ProgressBar] for
--  displaying indefinite activity, instead of actual progress.
--
--  <picture> <source srcset="spinner-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkSpinner" src="spinner.png"> </picture>
--  To start the animation, use [methodGtk.Spinner.start], to stop it use
--  [methodGtk.Spinner.stop].
--
--  # CSS nodes
--
--  `GtkSpinner` has a single CSS node with the name spinner. When the
--  animation is active, the :checked pseudoclass is added to this node.
--
--  # Accessibility
--
--  `GtkSpinner` uses the [enumGtk.AccessibleRole.progress_bar] role.
--
--  <group>Ornaments</group>
--  <gtkada_demo>create_spinners.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Spinner is

   type Gtk_Spinner_Record is new Gtk_Widget_Record with null record;
   type Gtk_Spinner is access all Gtk_Spinner_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Spinner);
   procedure Initialize (Self : not null access Gtk_Spinner_Record'Class);
   --  Returns a new spinner widget. Not yet started.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Spinner_New return Gtk_Spinner;
   --  Returns a new spinner widget. Not yet started.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_spinner_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Spinning
      (Self : not null access Gtk_Spinner_Record) return Boolean;
   --  Returns whether the spinner is spinning.
   --  @return True if the spinner is active

   procedure Set_Spinning
      (Self     : not null access Gtk_Spinner_Record;
       Spinning : Boolean);
   --  Sets the activity of the spinner.
   --  @param Spinning whether the spinner should be spinning

   procedure Start (Self : not null access Gtk_Spinner_Record);
   --  Starts the animation of the spinner.

   procedure Stop (Self : not null access Gtk_Spinner_Record);
   --  Stops the animation of the spinner.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Spinner_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Spinner_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Spinner_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Spinner_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Spinner_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Spinner_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Spinner_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Spinner_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Spinner_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Spinner_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Spinner_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Spinner_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Spinner_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Spinner_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Spinner_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Spinning_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the spinner is spinning

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Spinner_Record, Gtk_Spinner);
   function "+"
     (Widget : access Gtk_Spinner_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Spinner
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Spinner_Record, Gtk_Spinner);
   function "+"
     (Widget : access Gtk_Spinner_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Spinner
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Spinner_Record, Gtk_Spinner);
   function "+"
     (Widget : access Gtk_Spinner_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Spinner
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Spinning_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("spinning");
end Gtk.Spinner;
