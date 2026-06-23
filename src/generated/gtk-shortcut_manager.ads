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

--  An interface that is used to implement shortcut scopes.
--
--  This is important for [ifaceGtk.Native] widgets that have their own
--  surface, since the event controllers that are used to implement managed and
--  global scopes are limited to the same native.
--
--  Examples for widgets implementing `GtkShortcutManager` are
--  [classGtk.Window] and [classGtk.Popover].
--
--  Every widget that implements `GtkShortcutManager` will be used as a
--  `GTK_SHORTCUT_SCOPE_MANAGED`.

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Glib.Types;  use Glib.Types;

package Gtk.Shortcut_Manager is

   type Gtk_Shortcut_Manager is new Glib.Types.GType_Interface;
   Null_Gtk_Shortcut_Manager : constant Gtk_Shortcut_Manager;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_shortcut_manager_get_type");

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Shortcut_Manager"

   function "+" (W : Gtk_Shortcut_Manager) return Gtk_Shortcut_Manager;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Add_Controller is access procedure (Self : Gtk_Shortcut_Manager; Controller : System.Address);
   pragma Convention (C, Virtual_Add_Controller);
   --  Add a `GtkShortcutController` to be managed.

   type Virtual_Remove_Controller is access procedure (Self : Gtk_Shortcut_Manager; Controller : System.Address);
   pragma Convention (C, Virtual_Remove_Controller);
   --  Remove a `GtkShortcutController` that had previously been added

   subtype Shortcut_Manager_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Add_Controller
     (Self    : Shortcut_Manager_Interface_Descr;
      Handler : Virtual_Add_Controller);
   pragma Import (C, Set_Add_Controller, "gtkada_Shortcut_Manager_set_add_controller");

   procedure Set_Remove_Controller
     (Self    : Shortcut_Manager_Interface_Descr;
      Handler : Virtual_Remove_Controller);
   pragma Import (C, Set_Remove_Controller, "gtkada_Shortcut_Manager_set_remove_controller");
   --  See Glib.Object.Add_Interface

private

   Null_Gtk_Shortcut_Manager : constant Gtk_Shortcut_Manager :=
      Gtk_Shortcut_Manager (Glib.Types.Null_Interface);
end Gtk.Shortcut_Manager;
