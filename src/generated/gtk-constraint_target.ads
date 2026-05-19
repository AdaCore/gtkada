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

--  Makes it possible to use an object as source or target in a
--  [classGtk.Constraint].
--
--  Besides `GtkWidget`, it is also implemented by `GtkConstraintGuide`.

pragma Warnings (Off, "*is already use-visible*");
with Glib;       use Glib;
with Glib.Types; use Glib.Types;

package Gtk.Constraint_Target is

   type Gtk_Constraint_Target is new Glib.Types.GType_Interface;
   Null_Gtk_Constraint_Target : constant Gtk_Constraint_Target;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_constraint_target_get_type");

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Constraint_Target"

   function "+" (W : Gtk_Constraint_Target) return Gtk_Constraint_Target;
   pragma Inline ("+");

private

Null_Gtk_Constraint_Target : constant Gtk_Constraint_Target :=
   Gtk_Constraint_Target (Glib.Types.Null_Interface);
end Gtk.Constraint_Target;
