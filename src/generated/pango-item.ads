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

--  The Pango.Item.Pango_Item structure stores information about a segment of
--  text.

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Pango.Attributes; use Pango.Attributes;

package Pango.Item is

   type Pango_Analysis is record
      Shape_Engine : System.Address;
      Lang_Engine : System.Address;
      Font : System.Address;
      Level : Guint8;
      Gravity : Guint8;
      Flags : Guint8;
      Script : Guint8;
      Language : System.Address;
      Extra_Attrs : System.Address := System.Null_Address;
   end record;
   pragma Convention (C, Pango_Analysis);
   --  The Pango.Item.Pango_Analysis structure stores information about the
   --  properties of a segment of text.

   type Pango_Item is record
      Offset : Glib.Gint := 0;
      Length : Glib.Gint := 0;
      Num_Chars : Glib.Gint := 0;
      Analysis : Pango_Analysis;
   end record;
   pragma Convention (C, Pango_Item);
   --  The Pango.Item.Pango_Item structure stores information about a segment
   --  of text.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_item_get_type");

   -------------
   -- Methods --
   -------------

   procedure Apply_Attrs
      (Self : not null access Pango_Item;
       Iter : Pango.Attributes.Pango_Attr_Iterator);
   pragma Import (C, Apply_Attrs, "pango_item_apply_attrs");
   --  Add attributes to a PangoItem. The idea is that you have attributes
   --  that don't affect itemization, such as font features, so you filter them
   --  out using Pango.Attributes.Filter, itemize your text, then reapply the
   --  attributes to the resulting items using this function.
   --  The Iter should be positioned before the range of the item, and will be
   --  advanced past it. This function is meant to be called in a loop over the
   --  items resulting from itemization, while passing the iter to each call.
   --  Since: gtk+ 1.44
   --  @param Iter a Pango.Attr_Iterator.Pango_Attr_Iterator

   function Copy (Self : access Pango_Item) return access Pango_Item;
   pragma Import (C, Copy, "pango_item_copy");
   --  Copy an existing Pango.Item.Pango_Item structure.
   --  @return the newly allocated Pango.Item.Pango_Item, which should be
   --  freed with Pango.Item.Free, or null if Item was null.

   procedure Free (Self : access Pango_Item);
   pragma Import (C, Free, "pango_item_free");
   --  Free a Pango.Item.Pango_Item and all associated memory.

   function Split
      (Self         : not null access Pango_Item;
       Split_Index  : Glib.Gint;
       Split_Offset : Glib.Gint) return access Pango_Item;
   pragma Import (C, Split, "pango_item_split");
   --  Modifies Orig to cover only the text after Split_Index, and returns a
   --  new item that covers the text before Split_Index that used to be in
   --  Orig. You can think of Split_Index as the length of the returned item.
   --  Split_Index may not be 0, and it may not be greater than or equal to the
   --  length of Orig (that is, there must be at least one byte assigned to
   --  each item, you can't create a zero-length item). Split_Offset is the
   --  length of the first item in chars, and must be provided because the text
   --  used to generate the item isn't available, so Pango.Item.Split can't
   --  count the char length of the split items itself.
   --  @param Split_Index byte index of position to split item, relative to
   --  the start of the item
   --  @param Split_Offset number of chars between start of Orig and
   --  Split_Index
   --  @return new item representing text before Split_Index, which should be
   --  freed with Pango.Item.Free.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Pango_Analysis) return Pango_Analysis;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function From_Object_Free
     (B : not null access Pango_Item) return Pango_Item;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   ------------------
   -- Constructors --
   ------------------

   function Pango_Item_New return access Pango_Item;
   --  Creates a new Pango.Item.Pango_Item structure initialized to default
   --  values.

end Pango.Item;
