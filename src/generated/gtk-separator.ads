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
--  GtkSeparator is a horizontal or vertical separator widget, depending on
--  the value of the Gtk.Orientable.Gtk_Orientable:orientation property, used
--  to group the widgets within a window. It displays a line with a shadow to
--  make it appear sunken into the interface.
--
--  # CSS nodes
--
--  GtkSeparator has a single CSS node with name separator. The node gets one
--  of the .horizontal or .vertical style classes.
--
--  </description>
--  <screenshot>gtk-separator</screenshot>
--  <group>Ornaments</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Types;     use Glib.Types;
with Gtk.Buildable;  use Gtk.Buildable;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Orientable; use Gtk.Orientable;
with Gtk.Widget;     use Gtk.Widget;

package Gtk.Separator is

   type Gtk_Separator_Record is new Gtk_Widget_Record with null record;
   type Gtk_Separator is access all Gtk_Separator_Record'Class;

   subtype Gtk_Hseparator_Record is Gtk_Separator_Record;
   subtype Gtk_Hseparator is Gtk_Separator;

   subtype Gtk_Vseparator_Record is Gtk_Separator_Record;
   subtype Gtk_Vseparator is Gtk_Separator;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Separator   : out Gtk_Separator;
       Orientation : Gtk.Enums.Gtk_Orientation);
   procedure Initialize
      (Separator   : not null access Gtk_Separator_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation);
   --  Creates a new Gtk.Separator.Gtk_Separator with the given orientation.
   --  Since: gtk+ 3.0
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "orientation": the separator's orientation.

   function Gtk_Separator_New
      (Orientation : Gtk.Enums.Gtk_Orientation) return Gtk_Separator;
   --  Creates a new Gtk.Separator.Gtk_Separator with the given orientation.
   --  Since: gtk+ 3.0
   --  "orientation": the separator's orientation.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_separator_get_type");

   procedure Gtk_New_Hseparator (Separator : out Gtk_Hseparator);
   procedure Initialize_Hseparator
      (Separator : not null access Gtk_Hseparator_Record'Class);
   --  Creates a new Gtk.Separator.Gtk_Hseparator.
   --  Initialize_Hseparator does nothing if the object was already created
   --  with another call to Initialize* or G_New.

   function Gtk_Hseparator_New return Gtk_Hseparator;
   --  Creates a new Gtk.Separator.Gtk_Hseparator.

   function Hseparator_Get_Type return Glib.GType;
   pragma Import (C, Hseparator_Get_Type, "gtk_hseparator_get_type");

   procedure Gtk_New_Vseparator (Separator : out Gtk_Vseparator);
   procedure Initialize_Vseparator
      (Separator : not null access Gtk_Vseparator_Record'Class);
   --  Creates a new Gtk.Separator.Gtk_Vseparator.
   --  Initialize_Vseparator does nothing if the object was already created
   --  with another call to Initialize* or G_New.

   function Gtk_Vseparator_New return Gtk_Vseparator;
   --  Creates a new Gtk.Separator.Gtk_Vseparator.

   function Vseparator_Get_Type return Glib.GType;
   pragma Import (C, Vseparator_Get_Type, "gtk_vseparator_get_type");

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Separator_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Separator_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Separator_Record, Gtk_Separator);
   function "+"
     (Widget : access Gtk_Separator_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Separator
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Separator_Record, Gtk_Separator);
   function "+"
     (Widget : access Gtk_Separator_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Separator
   renames Implements_Gtk_Orientable.To_Object;

end Gtk.Separator;
