------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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
--  The Gtk.Scrollbar.Gtk_Scrollbar widget is a horizontal or vertical
--  scrollbar, depending on the value of the
--  Gtk.Orientable.Gtk_Orientable:orientation property.
--
--  The position of the thumb in a scrollbar is controlled by the scroll
--  adjustments. See Gtk.Adjustment.Gtk_Adjustment for the fields in an
--  adjustment - for Gtk.Scrollbar.Gtk_Scrollbar, the
--  Gtk.Adjustment.Gtk_Adjustment:value field represents the position of the
--  scrollbar, which must be between the Gtk.Adjustment.Gtk_Adjustment:lower
--  field and Gtk.Adjustment.Gtk_Adjustment:upper -
--  Gtk.Adjustment.Gtk_Adjustment:page-size. The
--  Gtk.Adjustment.Gtk_Adjustment:page-size field represents the size of the
--  visible scrollable area. The Gtk.Adjustment.Gtk_Adjustment:step-increment
--  and Gtk.Adjustment.Gtk_Adjustment:page-increment fields are properties when
--  the user asks to step down (using the small stepper arrows) or page down
--  (using for example the 'PageDown' key).
--
--  </description>
pragma Ada_2005;

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Types;     use Glib.Types;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Buildable;  use Gtk.Buildable;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.GRange;     use Gtk.GRange;
with Gtk.Orientable; use Gtk.Orientable;

package Gtk.Scrollbar is

   type Gtk_Scrollbar_Record is new Gtk_Range_Record with null record;
   type Gtk_Scrollbar is access all Gtk_Scrollbar_Record'Class;

   subtype Gtk_Hscrollbar_Record is Gtk_Scrollbar_Record;
   subtype Gtk_Hscrollbar is Gtk_Scrollbar;

   subtype Gtk_Vscrollbar_Record is Gtk_Scrollbar_Record;
   subtype Gtk_Vscrollbar is Gtk_Scrollbar;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Scrollbar   : out Gtk_Scrollbar;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   procedure Initialize
      (Scrollbar   : not null access Gtk_Scrollbar_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Creates a new scrollbar with the given orientation.
   --  Since: gtk+ 3.0
   --  "orientation": the scrollbar's orientation.
   --  "adjustment": the Gtk.Adjustment.Gtk_Adjustment to use, or null to
   --  create a new adjustment.

   function Gtk_Scrollbar_New
      (Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
       return Gtk_Scrollbar;
   --  Creates a new scrollbar with the given orientation.
   --  Since: gtk+ 3.0
   --  "orientation": the scrollbar's orientation.
   --  "adjustment": the Gtk.Adjustment.Gtk_Adjustment to use, or null to
   --  create a new adjustment.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_scrollbar_get_type");

   procedure Gtk_New_Hscrollbar
      (Scrollbar  : out Gtk_Hscrollbar;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   procedure Initialize_Hscrollbar
      (Scrollbar  : not null access Gtk_Hscrollbar_Record'Class;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Creates a new horizontal scrollbar.
   --  "adjustment": the Gtk.Adjustment.Gtk_Adjustment to use, or null to
   --  create a new adjustment

   function Gtk_Hscrollbar_New
      (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
       return Gtk_Hscrollbar;
   --  Creates a new horizontal scrollbar.
   --  "adjustment": the Gtk.Adjustment.Gtk_Adjustment to use, or null to
   --  create a new adjustment

   function Hscrollbar_Get_Type return Glib.GType;
   pragma Import (C, Hscrollbar_Get_Type, "gtk_hscrollbar_get_type");

   procedure Gtk_New_Vscrollbar
      (Scrollbar  : out Gtk_Vscrollbar;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   procedure Initialize_Vscrollbar
      (Scrollbar  : not null access Gtk_Vscrollbar_Record'Class;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Creates a new vertical scrollbar.
   --  "adjustment": the Gtk.Adjustment.Gtk_Adjustment to use, or null to
   --  create a new adjustment

   function Gtk_Vscrollbar_New
      (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
       return Gtk_Vscrollbar;
   --  Creates a new vertical scrollbar.
   --  "adjustment": the Gtk.Adjustment.Gtk_Adjustment to use, or null to
   --  create a new adjustment

   function Vscrollbar_Get_Type return Glib.GType;
   pragma Import (C, Vscrollbar_Get_Type, "gtk_vscrollbar_get_type");

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Scrollbar_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Scrollbar_Record;
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
     (Gtk.Buildable.Gtk_Buildable, Gtk_Scrollbar_Record, Gtk_Scrollbar);
   function "+"
     (Widget : access Gtk_Scrollbar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Scrollbar
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Scrollbar_Record, Gtk_Scrollbar);
   function "+"
     (Widget : access Gtk_Scrollbar_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Scrollbar
   renames Implements_Gtk_Orientable.To_Object;

end Gtk.Scrollbar;
