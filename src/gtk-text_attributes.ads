-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  This package defines the Gtk_Text_Attributes type.
--  </description>
--  <c_version>1.3.6</c_version>

with Glib; use Glib;
with Pango.Font;

package Gtk.Text_Attributes is

   type Gtk_Text_Attributes is new Glib.C_Proxy;

   procedure Gtk_New (Text_Attr : out Gtk_Text_Attributes);
   --  Create a new Gtk_Text_Attributes structure.

   procedure Ref (Values : Gtk_Text_Attributes);
   --  Increase the reference counter of the given Gtk_Text_Attributes
   --  by one (this counter is initially set to 1 when this structure
   --  is created).

   procedure Unref (Values : Gtk_Text_Attributes);
   --  Decrease the reference counter by one. When it reaches zero,
   --  the Gtk_Text_Attributes is automatically deallocated.

   function Copy (Src : Gtk_Text_Attributes) return Gtk_Text_Attributes;
   --  Create a copy of the given Gtk_Text_Attributes structure.

   procedure Copy_Values
     (Src  : Gtk_Text_Attributes;
      Dest : Gtk_Text_Attributes);
   --  Copy the values from Src into Dest so that Dest has the same values
   --  as Src. Free existing values in Dest. Dest's reference counter
   --  is preserved.

   function Get_Font (Text_Attr : Gtk_Text_Attributes)
     return Pango.Font.Pango_Font_Description_Record;
   --  Return the Pango_Font_Description associated to the given
   --  Gtk_Text_Attributes.

   procedure Set_Font
     (Text_Attr : Gtk_Text_Attributes;
      Font      : Pango.Font.Pango_Font_Description);
   --  Set the Pango_Font_Description associated to the given
   --  Gtk_Text_Attributes.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private

   pragma Import (C, Ref, "gtk_text_attributes_ref");
   pragma Import (C, Unref, "gtk_text_attributes_unref");
   pragma Import (C, Copy, "gtk_text_attributes_copy");
   pragma Import (C, Copy_Values, "gtk_text_attributes_copy_values");
   pragma Import (C, Get_Font, "ada_text_attributes_get_font");
   pragma Import (C, Set_Font, "ada_text_attributes_set_font");

end Gtk.Text_Attributes;
