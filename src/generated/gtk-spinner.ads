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
--  A GtkSpinner widget displays an icon-size spinning animation. It is often
--  used as an alternative to a Gtk.Progress_Bar.Gtk_Progress_Bar for
--  displaying indefinite activity, instead of actual progress.
--
--  To start the animation, use Gtk.Spinner.Start, to stop it use
--  Gtk.Spinner.Stop.
--
--  # CSS nodes
--
--  GtkSpinner has a single CSS node with the name spinner. When the animation
--  is active, the :checked pseudoclass is added to this node.
--
--  </description>
--  <group>Ornaments</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Spinner is

   type Gtk_Spinner_Record is new Gtk_Widget_Record with null record;
   type Gtk_Spinner is access all Gtk_Spinner_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Spinner : out Gtk_Spinner);
   procedure Initialize (Spinner : not null access Gtk_Spinner_Record'Class);
   --  Returns a new spinner widget. Not yet started.
   --  Since: gtk+ 2.20
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Spinner_New return Gtk_Spinner;
   --  Returns a new spinner widget. Not yet started.
   --  Since: gtk+ 2.20

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_spinner_get_type");

   -------------
   -- Methods --
   -------------

   procedure Start (Spinner : not null access Gtk_Spinner_Record);
   --  Starts the animation of the spinner.
   --  Since: gtk+ 2.20

   procedure Stop (Spinner : not null access Gtk_Spinner_Record);
   --  Stops the animation of the spinner.
   --  Since: gtk+ 2.20

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Property : constant Glib.Properties.Property_Boolean;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

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

private
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Spinner;
