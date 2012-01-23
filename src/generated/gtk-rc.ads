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
--  The Gtk.Rc.Gtk_Rc_Style structure is used to represent a set of
--  information about the appearance of a widget. This can later be composited
--  together with other Gtk.Rc.Gtk_Rc_Style structures to form a
--  Gtk.Style.Gtk_Style.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;

package Gtk.Rc is

   type Gtk_Rc_Style_Record is new GObject_Record with null record;
   type Gtk_Rc_Style is access all Gtk_Rc_Style_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Rc_Style);
   procedure Initialize (Self : access Gtk_Rc_Style_Record'Class);
   --  Creates a new Gtk.Rc.Gtk_Rc_Style with no fields set and a reference
   --  count of 1.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_rc_style_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Self : not null access Gtk_Rc_Style_Record) return Gtk_Rc_Style;
   pragma Obsolescent (Copy);
   --  Makes a copy of the specified Gtk.Rc.Gtk_Rc_Style. This function will
   --  correctly copy an RC style that is a member of a class derived from
   --  Gtk.Rc.Gtk_Rc_Style.
   --  Deprecated since 3.0, Use Gtk.Css_Provider.Gtk_Css_Provider instead.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

end Gtk.Rc;
