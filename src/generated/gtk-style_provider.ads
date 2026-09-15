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

--  An interface for style information used by [classGtk.StyleContext].
--
--  See [methodGtk.StyleContext.add_provider] and
--  [funcGtk.StyleContext.add_provider_for_display] for adding
--  `GtkStyleProviders`.
--
--  GTK uses the `GtkStyleProvider` implementation for CSS in
--  [classGtk.CssProvider].

pragma Warnings (Off, "*is already use-visible*");
with Glib;       use Glib;
with Glib.Types; use Glib.Types;

package Gtk.Style_Provider is

   type Gtk_Style_Provider is new Glib.Types.GType_Interface;
   Null_Gtk_Style_Provider : constant Gtk_Style_Provider;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_style_provider_get_type");

   ----------------------
   -- GtkAda additions --
   ----------------------

   subtype Priority is Glib.Guint;

   Priority_Fallback : constant Priority := 1;
   --  Used when no theme is defined.

   Priority_Theme : constant Priority := 200;
   --  Used for style information provided by the theme.

   Priority_Settings : constant Priority := 400;
   --  Used for style information provided via Gtk.Settings.

   Priority_Application : constant Priority := 600;
   --  Used for application-specific style information.

   Priority_User : constant Priority := 800;
   --  Used for style information from ~/.config/gtk-4.0/gtk.css.

   -------------
   -- Signals --
   -------------

   Signal_Gtk_Private_Changed : constant Glib.Signal_Name := "gtk-private-changed";
   --  Callback for this signal:
   --    procedure Handler (Self : Gtk_Style_Provider)

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Style_Provider"

   function "+" (W : Gtk_Style_Provider) return Gtk_Style_Provider;
   pragma Inline ("+");

private

   Null_Gtk_Style_Provider : constant Gtk_Style_Provider :=
      Gtk_Style_Provider (Glib.Types.Null_Interface);
end Gtk.Style_Provider;
