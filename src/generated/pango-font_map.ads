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
--  The Pango.Font_Map.Pango_Font_Map represents the set of fonts available
--  for a particular rendering system. This is a virtual object with
--  implementations being specific to particular rendering systems. To create
--  an implementation of a Pango.Font_Map.Pango_Font_Map, the rendering-system
--  specific code should allocate a larger structure that contains a nested
--  Pango.Font_Map.Pango_Font_Map, fill in the <structfield>klass</structfield>
--  member of the nested Pango.Font_Map.Pango_Font_Map with a pointer to a
--  appropriate Pango_Font_Map_Class, then call pango_font_map_init on the
--  structure.
--
--  The Pango.Font_Map.Pango_Font_Map structure contains one member which the
--  implementation fills in.
--
--  </description>
--  <description>
--  An object that represents the set of fonts available for a particular
--  rendering system
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Pango.Context;     use Pango.Context;
with Pango.Font;        use Pango.Font;
with Pango.Font_Family; use Pango.Font_Family;
with Pango.Fontset;     use Pango.Fontset;
with Pango.Language;    use Pango.Language;

package Pango.Font_Map is

   type Pango_Font_Map_Record is new GObject_Record with null record;
   type Pango_Font_Map is access all Pango_Font_Map_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_font_map_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed (Self : not null access Pango_Font_Map_Record);
   --  Forces a change in the context, which will cause any
   --  Pango.Context.Pango_Context using this fontmap to change.
   --  This function is only useful when implementing a new backend for Pango,
   --  something applications won't do. Backends should call this function if
   --  they have attached extra data to the context and such data is changed.
   --  Since: gtk+ 1.34

   function Create_Context
      (Self : not null access Pango_Font_Map_Record)
       return Pango.Context.Pango_Context;
   --  Creates a Pango.Context.Pango_Context connected to Fontmap. This is
   --  equivalent to Pango.Context.Gdk_New followed by
   --  pango_context_set_font_map.
   --  If you are using Pango as part of a higher-level system, that system
   --  may have it's own way of create a Pango.Context.Pango_Context. For
   --  instance, the GTK+ toolkit has, among others,
   --  gdk_pango_context_get_for_screen, and Gtk.Widget.Get_Pango_Context. Use
   --  those instead.
   --  Since: gtk+ 1.22

   function Get_Serial
      (Self : not null access Pango_Font_Map_Record) return Guint;
   --  Returns the current serial number of Fontmap. The serial number is
   --  initialized to an small number larger than zero when a new fontmap is
   --  created and is increased whenever the fontmap is changed. It may wrap,
   --  but will never have the value 0. Since it can wrap, never compare it
   --  with "less than", always use "not equals".
   --  The fontmap can only be changed using backend-specific API, like
   --  changing fontmap resolution.
   --  This can be used to automatically detect changes to a
   --  Pango.Font_Map.Pango_Font_Map, like in Pango.Context.Pango_Context.
   --  Since: gtk+ 1.32.4

   function List_Families
      (Self : not null access Pango_Font_Map_Record)
       return Pango_Font_Family_Array;
   --  List all families for a fontmap.

   function Load_Font
      (Self    : not null access Pango_Font_Map_Record;
       Context : not null access Pango.Context.Pango_Context_Record'Class;
       Desc    : Pango.Font.Pango_Font_Description)
       return Pango.Font.Pango_Font;
   --  Load the font in the fontmap that is the closest match for Desc.
   --  "context": the Pango.Context.Pango_Context the font will be used with
   --  "desc": a Pango.Font.Pango_Font_Description describing the font to load

   function Load_Fontset
      (Self     : not null access Pango_Font_Map_Record;
       Context  : not null access Pango.Context.Pango_Context_Record'Class;
       Desc     : Pango.Font.Pango_Font_Description;
       Language : Pango.Language.Pango_Language)
       return Pango.Fontset.Pango_Fontset;
   --  Load a set of fonts in the fontmap that can be used to render a font
   --  matching Desc.
   --  "context": the Pango.Context.Pango_Context the font will be used with
   --  "desc": a Pango.Font.Pango_Font_Description describing the font to load
   --  "language": a Pango.Language.Pango_Language the fonts will be used for

end Pango.Font_Map;
