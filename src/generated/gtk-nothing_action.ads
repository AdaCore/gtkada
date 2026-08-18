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

--  Does nothing.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                use Glib;
with Gtk.Shortcut_Action; use Gtk.Shortcut_Action;

package Gtk.Nothing_Action is

   type Gtk_Nothing_Action_Record is new Gtk_Shortcut_Action_Record with null record;
   type Gtk_Nothing_Action is access all Gtk_Nothing_Action_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_nothing_action_get_type");

   ---------------
   -- Functions --
   ---------------

   function Get return Gtk.Shortcut_Action.Gtk_Shortcut_Action;
   --  Gets the nothing action.
   --  This is an action that does nothing and where activating it always
   --  fails.
   --  @return The nothing action
   --  Return has transfer-ownership='none'

end Gtk.Nothing_Action;
