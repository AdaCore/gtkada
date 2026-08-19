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

--  A `GtkShortcutTrigger` that never triggers.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                 use Glib;
with Gtk.Shortcut_Trigger; use Gtk.Shortcut_Trigger;

package Gtk.Never_Trigger is

   type Gtk_Never_Trigger_Record is new Gtk_Shortcut_Trigger_Record with null record;
   type Gtk_Never_Trigger is access all Gtk_Never_Trigger_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_never_trigger_get_type");

   ---------------
   -- Functions --
   ---------------

   function Get return Gtk_Never_Trigger;
   --  Gets the never trigger.
   --  This is a singleton for a trigger that never triggers. Use this trigger
   --  instead of null because it implements all virtual functions.
   --  Return has transfer-ownership='none'

end Gtk.Never_Trigger;
