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
--  The Pango.Attributes.Pango_Attribute structure represents the common
--  portions of all attributes. Particular types of attributes include this
--  structure as their initial portion. The common portion of the attribute
--  holds the range to which the value in the type-specific part of the
--  attribute applies and should be initialized using pango_attribute_init. By
--  default an attribute will have an all-inclusive range of [0,G_MAXUINT].
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.GSlist; use Glib.GSlist;
with Pango.Enums; use Pango.Enums;

package Pango.Attributes is

   type Pango_Attribute is new Glib.C_Proxy;
   function From_Object_Free (B : access Pango_Attribute) return Pango_Attribute;
   pragma Inline (From_Object_Free);
   --  The Pango.Attributes.Pango_Attribute structure represents the common
   --  portions of all attributes. Particular types of attributes include this
   --  structure as their initial portion. The common portion of the attribute
   --  holds the range to which the value in the type-specific part of the
   --  attribute applies and should be initialized using pango_attribute_init.
   --  By default an attribute will have an all-inclusive range of
   --  [0,G_MAXUINT].

   function Convert (R : Pango.Attributes.Pango_Attribute) return System.Address;
   function Convert (R : System.Address) return Pango.Attributes.Pango_Attribute;
   package Pango_Attribute_SList is new Generic_SList (Pango.Attributes.Pango_Attribute);

   type Pango_Attr_List is new Glib.C_Boxed with null record;
   Null_Pango_Attr_List : constant Pango_Attr_List;

   function From_Object (Object : System.Address) return Pango_Attr_List;
   function From_Object_Free (B : access Pango_Attr_List'Class) return Pango_Attr_List;
   pragma Inline (From_Object_Free, From_Object);

   ---------------
   -- Callbacks --
   ---------------

   type Pango_Attr_Filter_Func is access function (Attribute : Pango_Attribute) return Boolean;
   --  Type of a function filtering a list of attributes.
   --  "attribute": a Pango attribute

   ------------------
   -- Constructors --
   ------------------

   function Get_Type_Attribute return Glib.GType;
   pragma Import (C, Get_Type_Attribute, "pango_attribute_get_type");

   procedure Gdk_New (Self : out Pango_Attr_List);
   --  Create a new empty attribute list with a reference count of one.

   function Pango_Attr_List_New return Pango_Attr_List;
   --  Create a new empty attribute list with a reference count of one.

   function Get_Type_Attr_List return Glib.GType;
   pragma Import (C, Get_Type_Attr_List, "pango_attr_list_get_type");

   -------------
   -- Methods --
   -------------

   function Copy (Self : Pango_Attribute) return Pango_Attribute;
   pragma Import (C, Copy, "pango_attribute_copy");
   --  Make a copy of an attribute.

   function Copy (Self : Pango_Attr_List) return Pango_Attr_List;
   --  Copy List and return an identical new list.

   procedure Destroy (Self : Pango_Attribute);
   pragma Import (C, Destroy, "pango_attribute_destroy");
   --  Destroy a Pango.Attributes.Pango_Attribute and free all associated
   --  memory.

   function Equal
      (Self  : Pango_Attribute;
       Attr2 : Pango_Attribute) return Boolean;
   --  Compare two attributes for equality. This compares only the actual
   --  value of the two attributes and not the ranges that the attributes apply
   --  to.
   --  "attr2": another Pango.Attributes.Pango_Attribute

   procedure Change (Self : Pango_Attr_List; Attr : Pango_Attribute);
   --  Insert the given attribute into the Pango.Attributes.Pango_Attr_List.
   --  It will replace any attributes of the same type on that segment and be
   --  merged with any adjoining attributes that are identical.
   --  This function is slower than Pango.Attributes.Insert for creating an
   --  attribute list in order (potentially much slower for large lists).
   --  However, Pango.Attributes.Insert is not suitable for continually
   --  changing a set of attributes since it never removes or combines existing
   --  attributes.
   --  "attr": the attribute to insert. Ownership of this value is assumed by
   --  the list.

   function Filter
      (Self : Pango_Attr_List;
       Func : Pango_Attr_Filter_Func) return Pango_Attr_List;
   --  Given a Pango.Attributes.Pango_Attr_List and callback function, removes
   --  any elements of List for which Func returns True and inserts them into a
   --  new list.
   --  Since: gtk+ 1.2
   --  "func": callback function; returns True if an attribute should be
   --  filtered out.

   procedure Insert (Self : Pango_Attr_List; Attr : Pango_Attribute);
   --  Insert the given attribute into the Pango.Attributes.Pango_Attr_List.
   --  It will be inserted after all other attributes with a matching
   --  Start_Index.
   --  "attr": the attribute to insert. Ownership of this value is assumed by
   --  the list.

   procedure Insert_Before (Self : Pango_Attr_List; Attr : Pango_Attribute);
   --  Insert the given attribute into the Pango.Attributes.Pango_Attr_List.
   --  It will be inserted before all other attributes with a matching
   --  Start_Index.
   --  "attr": the attribute to insert. Ownership of this value is assumed by
   --  the list.

   function Ref (Self : Pango_Attr_List) return Pango_Attr_List;
   --  Increase the reference count of the given attribute list by one.
   --  Since: gtk+ 1.10

   procedure Splice
      (Self  : Pango_Attr_List;
       Other : Pango_Attr_List;
       Pos   : Glib.Gint;
       Len   : Glib.Gint);
   --  This function opens up a hole in List, fills it in with attributes from
   --  the left, and then merges Other on top of the hole.
   --  This operation is equivalent to stretching every attribute that applies
   --  at position Pos in List by an amount Len, and then calling
   --  Pango.Attributes.Change with a copy of each attribute in Other in
   --  sequence (offset in position by Pos).
   --  This operation proves useful for, for instance, inserting a pre-edit
   --  string in the middle of an edit buffer.
   --  "other": another Pango.Attributes.Pango_Attr_List
   --  "pos": the position in List at which to insert Other
   --  "len": the length of the spliced segment. (Note that this must be
   --  specified since the attributes in Other may only be present at some
   --  subsection of this range)

   procedure Unref (Self : Pango_Attr_List);
   --  Decrease the reference count of the given attribute list by one. If the
   --  result is zero, free the attribute list and the attributes it contains.

   procedure Update
      (Self   : Pango_Attr_List;
       Pos    : Glib.Gint;
       Remove : Glib.Gint;
       Add    : Glib.Gint);
   --  Update indices of attributes in List for a change in the text they
   --  refer to.
   --  The change that this function applies is removing Remove bytes at
   --  position Pos and inserting Add bytes instead.
   --  Attributes that fall entirely in the (Pos, Pos + Remove) range are
   --  removed.
   --  Attributes that start or end inside the (Pos, Pos + Remove) range are
   --  shortened to reflect the removal.
   --  Attributes start and end positions are updated if they are behind Pos +
   --  Remove.
   --  Since: gtk+ 1.44
   --  "pos": the position of the change
   --  "remove": the number of removed bytes
   --  "add": the number of added bytes

   ---------------
   -- Functions --
   ---------------

   function Attr_Underline_New
      (Underline : Pango.Enums.Underline) return Pango_Attribute;
   pragma Import (C, Attr_Underline_New, "pango_attr_underline_new");
   --  Create a new underline-style attribute.
   --  "underline": the underline style.

   function Attr_Background_New
      (Red   : Guint16;
       Green : Guint16;
       Blue  : Guint16) return Pango_Attribute;
   pragma Import (C, Attr_Background_New, "pango_attr_background_new");
   --  Create a new background color attribute.
   --  "red": the red value (ranging from 0 to 65535)
   --  "green": the green value
   --  "blue": the blue value

   function Attr_Foreground_New
      (Red   : Guint16;
       Green : Guint16;
       Blue  : Guint16) return Pango_Attribute;
   pragma Import (C, Attr_Foreground_New, "pango_attr_foreground_new");
   --  Create a new foreground color attribute.
   --  "red": the red value (ranging from 0 to 65535)
   --  "green": the green value
   --  "blue": the blue value

   function Attr_Family_New (Family : UTF8_String) return Pango_Attribute;
   --  Create a new font family attribute.
   --  "family": the family or comma separated list of families

   function Attr_Strikethrough_New
      (Strikethrough : Boolean) return Pango_Attribute;
   --  Create a new strike-through attribute.
   --  "strikethrough": True if the text should be struck-through.

   function Attr_Variant_New
      (Variant : Pango.Enums.Variant) return Pango_Attribute;
   pragma Import (C, Attr_Variant_New, "pango_attr_variant_new");
   --  Create a new font variant attribute (normal or small caps)
   --  "variant": the variant

   function Attr_Weight_New
      (Weight : Pango.Enums.Weight) return Pango_Attribute;
   pragma Import (C, Attr_Weight_New, "pango_attr_weight_new");
   --  Create a new font weight attribute.
   --  "weight": the weight

   function Attr_Stretch_New
      (Stretch : Pango.Enums.Stretch) return Pango_Attribute;
   pragma Import (C, Attr_Stretch_New, "pango_attr_stretch_new");
   --  Create a new font stretch attribute
   --  "stretch": the stretch

   function Attr_Scale_New (Scale_Factor : Gdouble) return Pango_Attribute;
   pragma Import (C, Attr_Scale_New, "pango_attr_scale_new");
   --  Create a new font size scale attribute. The base font for the affected
   --  text will have its size multiplied by Scale_Factor.
   --  "scale_factor": factor to scale the font

   function Attr_Rise_New (Rise : Glib.Gint) return Pango_Attribute;
   pragma Import (C, Attr_Rise_New, "pango_attr_rise_new");
   --  Create a new baseline displacement attribute.
   --  "rise": the amount that the text should be displaced vertically, in
   --  Pango units. Positive values displace the text upwards.

   function Attr_Gravity_New
      (Gravity : Pango.Enums.Gravity) return Pango_Attribute;
   pragma Import (C, Attr_Gravity_New, "pango_attr_gravity_new");
   --  Create a new gravity attribute.
   --  Since: gtk+ 1.16
   --  "gravity": the gravity value; should not be
   --  Pango.Enums.Pango_Gravity_Auto.

private

   Null_Pango_Attr_List : constant Pango_Attr_List := (Glib.C_Boxed with null record);

end Pango.Attributes;
