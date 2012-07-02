------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

pragma Ada_05;
--  <description>
--  GtkStyleProvider is an interface used to provide style information to a
--  Gtk.Style_Context.Gtk_Style_Context. See Gtk.Style_Context.Add_Provider and
--  Gtk.Style_Context.Add_Provider_For_Screen.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Types;       use Glib.Types;
with Glib.Values;      use Glib.Values;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Icon_Factory; use Gtk.Icon_Factory;
with Gtk.Widget;       use Gtk.Widget;

package Gtk.Style_Provider is

   type Gtk_Style_Provider is new Glib.Types.GType_Interface;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_style_provider_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Icon_Factory
      (Self : Gtk_Style_Provider;
       Path : Gtk.Widget.Gtk_Widget_Path)
       return Gtk.Icon_Factory.Gtk_Icon_Factory;
   --  Returns the Gtk.Icon_Factory.Gtk_Icon_Factory defined to be in use for
   --  Path, or null if none is defined.
   --  Since: gtk+ 3.0
   --  "path": Gtk.Widget.Gtk_Widget_Path to query

   procedure Get_Style_Property
      (Self  : Gtk_Style_Provider;
       Path  : Gtk.Widget.Gtk_Widget_Path;
       State : Gtk.Enums.Gtk_State_Flags;
       Pspec : in out Glib.Param_Spec;
       Value : out Glib.Values.GValue;
       Found : out Boolean);
   --  Looks up a widget style property as defined by Provider for the widget
   --  represented by Path.
   --  "path": Gtk.Widget.Gtk_Widget_Path to query
   --  "state": state to query the style property for
   --  "pspec": The Glib.Param_Spec to query
   --  "value": return location for the property value

end Gtk.Style_Provider;
