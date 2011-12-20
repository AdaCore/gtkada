------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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
--
--  This package provides a set of types and subprograms to manipulate the
--  attributes of text displayed in a pango_layout
--
--  </description>
--  <group>Pango, font handling</group>

with Glib;
with Pango.Enums;

package Pango.Attributes is

   ----------------
   -- Attributes --
   ----------------

   type Pango_Attribute is new Glib.C_Proxy;

   function Attr_Underline_New
     (Underline : Pango.Enums.Underline) return Pango_Attribute;
   --  Create a new underline attribute

   ---------------------
   -- Attributes list --
   ---------------------

   type Pango_Attr_List is new Glib.C_Proxy;

   procedure Gdk_New (Attr_List : out Pango_Attr_List);
   --  Create a new empty list of attributes

   procedure Ref (Attr_List : Pango_Attr_List);
   --  Increment the reference count of the attribute list

   procedure Unref (Attr_List : Pango_Attr_List);
   --  Decrement the reference count of the attribute list. When it reaches 0,
   --  the list is destroyed.

   procedure Insert
     (Attr_List : Pango_Attr_List;
      Attribute : Pango_Attribute);
   --  Insert a new attribute in the list

private
   pragma Import (C, Ref,   "pango_attr_list_ref");
   pragma Import (C, Unref, "pango_attr_list_unref");
   pragma Import (C, Insert, "pango_attr_list_insert");
   pragma Import (C, Attr_Underline_New, "pango_attr_underline_new");

end Pango.Attributes;
