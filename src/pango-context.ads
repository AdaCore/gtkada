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

--  <group>Pango, font handling</group>

with Glib;
with Glib.Object;
with Pango.Font;

package Pango.Context is

   type Pango_Context_Record is new Glib.Object.GObject_Record with private;
   type Pango_Context is access all Pango_Context_Record'Class;
   --  A pango context is somewhat similar to a graphic context, and groups all
   --  the information on how to handle the various international scripts
   --  (font, color, direction,...)
   --  Such a context is destroyed by calling Glib.Object.Unref on it.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Pango_Context.

   procedure Set_Font_Description
     (Context     : Pango_Context;
      Description : Pango.Font.Pango_Font_Description);

   function Get_Font_Description (Context : Pango_Context)
      return Pango.Font.Pango_Font_Description;

   function Load_Font
     (Context : access Pango_Context_Record'Class;
      Descr   : Pango.Font.Pango_Font_Description)
      return Pango.Font.Pango_Font;
   --  Load a new font from its description

private
   type Pango_Context_Record is new Glib.Object.GObject_Record
     with null record;
   pragma Import (C, Get_Type, "pango_context_get_type");
end Pango.Context;

--  missing:
--  pango_context_list_families
--  pango_context_load_fontset
--  pango_context_get_metrics
--  pango_context_get_language
--  pango_context_set_language
--  pango_context_set_base_dir
--  pango_context_get_base_dir
