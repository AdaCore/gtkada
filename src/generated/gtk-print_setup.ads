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

--  An auxiliary object for printing that allows decoupling the setup from the
--  printing.
--
--  A print setup is obtained by calling [methodGtk.PrintDialog.setup], and
--  can later be passed to print functions such as
--  [methodGtk.PrintDialog.print].
--
--  Print setups can be reused for multiple print calls.
--
--  Applications may wish to store the page_setup and print_settings from the
--  print setup and copy them to the PrintDialog if they want to keep using
--  them.

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Gtk.Page_Setup;     use Gtk.Page_Setup;
with Gtk.Print_Settings; use Gtk.Print_Settings;

package Gtk.Print_Setup is

   type Gtk_Print_Setup is new Glib.C_Boxed with null record;
   Null_Gtk_Print_Setup : constant Gtk_Print_Setup;

   function From_Object (Object : System.Address) return Gtk_Print_Setup;
   function From_Object_Free (B : access Gtk_Print_Setup'Class) return Gtk_Print_Setup;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_print_setup_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Page_Setup
      (Self : Gtk_Print_Setup) return Gtk.Page_Setup.Gtk_Page_Setup;
   --  Returns the page setup of Setup.
   --  It may be different from the `GtkPrintDialog`'s page setup if the user
   --  changed it during the setup process.
   --  Since: gtk+ 4.14
   --  @return the page setup, or `NULL`
   --  Return has transfer-ownership='none'

   function Get_Print_Settings
      (Self : Gtk_Print_Setup) return Gtk.Print_Settings.Gtk_Print_Settings;
   --  Returns the print settings of Setup.
   --  They may be different from the `GtkPrintDialog`'s settings if the user
   --  changed them during the setup process.
   --  Since: gtk+ 4.14
   --  @return the print settings, or `NULL`
   --  Return has transfer-ownership='none'

   function Ref (Self : Gtk_Print_Setup) return Gtk_Print_Setup;
   --  Increase the reference count of Setup.
   --  Since: gtk+ 4.14
   --  @return the print setup

   procedure Unref (Self : Gtk_Print_Setup);
   --  Decrease the reference count of Setup.
   --  If the reference count reaches zero, the object is freed.
   --  Since: gtk+ 4.14

private
   Null_Gtk_Print_Setup : constant Gtk_Print_Setup :=
      (Glib.C_Boxed with null record);

end Gtk.Print_Setup;
