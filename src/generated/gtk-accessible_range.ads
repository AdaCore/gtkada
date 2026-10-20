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

--  An interface for accessible objects containing a numeric value.
--
--  `GtkAccessibleRange` describes ranged controls for Assistive Technologies.
--
--  Ranged controls have a single value within an allowed range that can
--  optionally be changed by the user.
--
--  This interface is expected to be implemented by controls using the
--  following roles:
--
--  - `GTK_ACCESSIBLE_ROLE_METER` - `GTK_ACCESSIBLE_ROLE_PROGRESS_BAR` -
--  `GTK_ACCESSIBLE_ROLE_SCROLLBAR` - `GTK_ACCESSIBLE_ROLE_SLIDER` -
--  `GTK_ACCESSIBLE_ROLE_SPIN_BUTTON`
--
--  If that is not the case, a warning will be issued at run time.
--
--  In addition to this interface, its implementers are expected to provide
--  the correct values for the following properties:
--
--  - `GTK_ACCESSIBLE_PROPERTY_VALUE_MAX` -
--  `GTK_ACCESSIBLE_PROPERTY_VALUE_MIN` - `GTK_ACCESSIBLE_PROPERTY_VALUE_NOW` -
--  `GTK_ACCESSIBLE_PROPERTY_VALUE_TEXT`

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Glib.Types;  use Glib.Types;

package Gtk.Accessible_Range is

   type Gtk_Accessible_Range is new Glib.Types.GType_Interface;
   Null_Gtk_Accessible_Range : constant Gtk_Accessible_Range;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_accessible_range_get_type");

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Accessible_Range"

   function "+" (W : Gtk_Accessible_Range) return Gtk_Accessible_Range;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Set_Current_Value is access function
     (Self  : Gtk_Accessible_Range;
      Value : Gdouble) return Glib.Gboolean;
   pragma Convention (C, Virtual_Set_Current_Value);
   --  Sets the current value of the accessible range.
   --  This operation should behave similarly as if the user performed the
   --  action.
   --  Since: gtk+ 4.10
   --  @param Value the value to set
   --  @return true if the operation was performed, false otherwise

   subtype Accessible_Range_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Set_Current_Value
     (Self    : Accessible_Range_Interface_Descr;
      Handler : Virtual_Set_Current_Value);
   pragma Import (C, Set_Set_Current_Value, "gtkada_Accessible_Range_set_set_current_value");
   --  See Glib.Object.Add_Interface

private

   Null_Gtk_Accessible_Range : constant Gtk_Accessible_Range :=
      Gtk_Accessible_Range (Glib.Types.Null_Interface);
end Gtk.Accessible_Range;
