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
--  GtkStyleProvider is an interface used to provide style information to a
--  Gtk.Style_Context.Gtk_Style_Context. See Gtk.Style_Context.Add_Provider and
--  Gtk.Style_Context.Add_Provider_For_Screen.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Types;  use Glib.Types;
with Glib.Values; use Glib.Values;
with Gtk.Enums;   use Gtk.Enums;
with Gtk.Widget;  use Gtk.Widget;

package Gtk.Style_Provider is

   type Gtk_Style_Provider is new Glib.Types.GType_Interface;
   Null_Gtk_Style_Provider : constant Gtk_Style_Provider;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_style_provider_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Style_Property
      (Self  : Gtk_Style_Provider;
       Path  : Gtk.Widget.Gtk_Widget_Path;
       State : Gtk.Enums.Gtk_State_Flags;
       Pspec : in out Glib.Param_Spec;
       Value : out Glib.Values.GValue;
       Found : out Boolean);
   --  Looks up a widget style property as defined by Provider for the widget
   --  represented by Path.
   --  Since: gtk+ 3.0
   --  "path": Gtk.Widget.Gtk_Widget_Path to query
   --  "state": state to query the style property for
   --  "pspec": The Glib.Param_Spec to query
   --  "value": return location for the property value

   ----------------------
   -- GtkAda additions --
   ----------------------

   subtype Priority is Glib.Guint;

   Priority_Fallback : constant Priority := 1;
   --  Used when no theme is defined

   Priority_Theme : constant Priority := 200;
   --  Used for style information provided by the theme

   Priority_Settings : constant Priority := 400;
   --  Used for information provided via Gtk_Settings

   Priority_Application : constant Priority := 600;
   --  For application-specific information

   Priority_User : constant Priority := 800;
   --  Used for the style information from ~/.gtk-3.0.css

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
