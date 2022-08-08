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
--  Using Gtk.Text_Attributes.Gtk_Text_Attributes directly should rarely be
--  necessary. It's primarily useful with Gtk.Text_Iter.Get_Attributes. As with
--  most GTK+ structs, the fields in this struct should only be read, never
--  modified directly.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Color;  use Gdk.Color;
with Gdk.RGBA;   use Gdk.RGBA;
with Glib;       use Glib;
with Gtk.Enums;  use Gtk.Enums;
with Pango.Font; use Pango.Font;

package Gtk.Text_Attributes is

   type Gtk_Text_Appearance is record
      Bg_Color : Gdk.Color.Gdk_Color;
      Fg_Color : Gdk.Color.Gdk_Color;
      Rise : Glib.Gint := 0;
      Underline : Guint;
      Strikethrough : Guint;
      Draw_Bg : Guint;
      Inside_Selection : Guint;
      Is_Text : Guint;
   end record;
   pragma Convention (C, Gtk_Text_Appearance);

   function From_Object_Free (B : access Gtk_Text_Appearance) return Gtk_Text_Appearance;
   pragma Inline (From_Object_Free);


   type Gtk_Text_Attributes is record
      Refcount : Guint;
      Appearance : Gtk_Text_Appearance;
      Justification : Gtk.Enums.Gtk_Justification;
      Direction : Gtk.Enums.Gtk_Text_Direction;
      Font : Pango.Font.Pango_Font_Description;
      Font_Scale : Gdouble;
      Left_Margin : Glib.Gint := 0;
      Right_Margin : Glib.Gint := 0;
      Indent : Glib.Gint := 0;
      Pixels_Above_Lines : Glib.Gint := 0;
      Pixels_Below_Lines : Glib.Gint := 0;
      Pixels_Inside_Wrap : Glib.Gint := 0;
      Tabs : System.Address;
      Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode;
      Language : System.Address;
      Pg_Bg_Color : Gdk.Color.Gdk_Color;
      Invisible : Guint;
      Bg_Full_Height : Guint;
      Editable : Guint;
      No_Fallback : Guint;
      Pg_Bg_Rgba : Gdk.RGBA.Gdk_RGBA;
      Letter_Spacing : Glib.Gint := 0;
   end record;
   pragma Convention (C, Gtk_Text_Attributes);

   function From_Object_Free (B : access Gtk_Text_Attributes) return Gtk_Text_Attributes;
   pragma Inline (From_Object_Free);
   --  Using Gtk.Text_Attributes.Gtk_Text_Attributes directly should rarely be
   --  necessary. It's primarily useful with Gtk.Text_Iter.Get_Attributes. As
   --  with most GTK+ structs, the fields in this struct should only be read,
   --  never modified directly.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Text_Attr : out Gtk_Text_Attributes);
   --  Creates a Gtk.Text_Attributes.Gtk_Text_Attributes, which describes a
   --  set of properties on some text.

   function Gtk_Text_Attributes_New return Gtk_Text_Attributes;
   --  Creates a Gtk.Text_Attributes.Gtk_Text_Attributes, which describes a
   --  set of properties on some text.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_text_attributes_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Text_Attr : Gtk_Text_Attributes) return Gtk_Text_Attributes;
   pragma Import (C, Copy, "gtk_text_attributes_copy");
   --  Copies Src and returns a new Gtk.Text_Attributes.Gtk_Text_Attributes.

   procedure Copy_Values
      (Text_Attr : Gtk_Text_Attributes;
       Dest      : in out Gtk_Text_Attributes);
   pragma Import (C, Copy_Values, "gtk_text_attributes_copy_values");
   --  Copies the values from Src to Dest so that Dest has the same values as
   --  Src. Frees existing values in Dest.
   --  "dest": another Gtk.Text_Attributes.Gtk_Text_Attributes

   function Ref (Text_Attr : Gtk_Text_Attributes) return Gtk_Text_Attributes;
   pragma Import (C, Ref, "gtk_text_attributes_ref");
   --  Increments the reference count on Values.

   procedure Unref (Text_Attr : Gtk_Text_Attributes);
   pragma Import (C, Unref, "gtk_text_attributes_unref");
   --  Decrements the reference count on Values, freeing the structure if the
   --  reference count reaches 0.

end Gtk.Text_Attributes;
