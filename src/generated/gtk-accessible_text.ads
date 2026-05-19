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

--  An interface for accessible objects containing formatted text.
--
--  The `GtkAccessibleText` interfaces is meant to be implemented by
--  accessible objects that have text formatted with attributes, or non-trivial
--  text contents.
--
--  You should use the [enumGtk.AccessibleProperty.LABEL] or the
--  [enumGtk.AccessibleProperty.DESCRIPTION] properties for accessible objects
--  containing simple, unformatted text.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Types;              use Glib.Types;
with Gtk.Enums;               use Gtk.Enums;
with Gtkada.Types;            use Gtkada.Types;

package Gtk.Accessible_Text is

   type Gtk_Accessible_Text is new Glib.Types.GType_Interface;
   Null_Gtk_Accessible_Text : constant Gtk_Accessible_Text;

   type Gtk_Accessible_Text_Granularity is (
      Accessible_Text_Granularity_Character,
      Accessible_Text_Granularity_Word,
      Accessible_Text_Granularity_Sentence,
      Accessible_Text_Granularity_Line,
      Accessible_Text_Granularity_Paragraph);
   pragma Convention (C, Gtk_Accessible_Text_Granularity);
   --  The granularity for queries about the text contents of a
   --  [ifaceGtk.AccessibleText] implementation.

   type Gtk_Accessible_Text_Range is record
      Start : Gsize;
      Length : Gsize;
   end record;
   pragma Convention (C, Gtk_Accessible_Text_Range);

   function From_Object_Free (B : access Gtk_Accessible_Text_Range) return Gtk_Accessible_Text_Range;
   pragma Inline (From_Object_Free);
   --  A range inside the text of an accessible object.

   type Gtk_Accessible_Text_Range_Array is
      array (Natural range <>) of Gtk_Accessible_Text_Range;
   --  The granularity for queries about the text contents of a
   --  [ifaceGtk.AccessibleText] implementation.
   --  A range inside the text of an accessible object.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Accessible_Text_Granularity_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Accessible_Text_Granularity);
   type Property_Gtk_Accessible_Text_Granularity is new Gtk_Accessible_Text_Granularity_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_accessible_text_get_type");

   -------------
   -- Methods --
   -------------

   procedure Update_Caret_Position (Self : Gtk_Accessible_Text);
   pragma Import (C, Update_Caret_Position, "gtk_accessible_text_update_caret_position");
   --  Updates the position of the caret.
   --  Implementations of the `GtkAccessibleText` interface should call this
   --  function every time the caret has moved, in order to notify assistive
   --  technologies.
   --  Since: gtk+ 4.14

   procedure Update_Contents
      (Self    : Gtk_Accessible_Text;
       Change  : Gtk.Enums.Gtk_Accessible_Text_Content_Change;
       Start   : Guint;
       The_End : Guint);
   pragma Import (C, Update_Contents, "gtk_accessible_text_update_contents");
   --  Notifies assistive technologies of a change in contents.
   --  Implementations of the `GtkAccessibleText` interface should call this
   --  function every time their contents change as the result of an operation,
   --  like an insertion or a removal.
   --  Note: If the change is a deletion, this function must be called
   --  *before* removing the contents, if it is an insertion, it must be called
   --  *after* inserting the new contents.
   --  Since: gtk+ 4.14
   --  @param Change the type of change in the contents
   --  @param Start the starting offset of the change, in characters
   --  @param The_End the end offset of the change, in characters

   procedure Update_Selection_Bound (Self : Gtk_Accessible_Text);
   pragma Import (C, Update_Selection_Bound, "gtk_accessible_text_update_selection_bound");
   --  Updates the boundary of the selection.
   --  Implementations of the `GtkAccessibleText` interface should call this
   --  function every time the selection has moved, in order to notify
   --  assistive technologies.
   --  Since: gtk+ 4.14

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Accessible_Text"

   function "+" (W : Gtk_Accessible_Text) return Gtk_Accessible_Text;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Get_Attributes is access function
     (Self             : Gtk_Accessible_Text;
      Offset           : Guint;
      N_Ranges         : access Gsize;
      Ranges           : access Gtk_Accessible_Text_Range_Array;
      Attribute_Names  : access Gtkada.Types.chars_ptr_array;
      Attribute_Values : access Gtkada.Types.chars_ptr_array)
   return Glib.Gboolean;
   pragma Convention (C, Virtual_Get_Attributes);
   --  Retrieves the text attributes inside the accessible object.
   --  Each attribute is composed by:
   --  - a range - a name - a value
   --  It is left to the implementation to determine the serialization format
   --  of the value to a string.
   --  GTK provides support for various text attribute names and values, but
   --  implementations of this interface are free to add their own attributes.
   --  If this function returns true, `n_ranges` will be set to a value
   --  greater than or equal to one, Ranges will be set to a newly allocated
   --  array of [structGtk.AccessibleTextRange].
   --  Since: gtk+ 4.14
   --  @param Offset the offset, in characters
   --  @param N_Ranges the number of attributes
   --  @param Ranges the ranges of the attributes inside the accessible object
   --  @param Attribute_Names the names of the attributes inside the
   --  accessible object
   --  @param Attribute_Values the values of the attributes inside the
   --  accessible object
   --  @return true if the accessible object has at least an attribute, and
   --  false otherwise

   type Virtual_Get_Caret_Position is access function (Self : Gtk_Accessible_Text) return Guint;
   pragma Convention (C, Virtual_Get_Caret_Position);
   --  Retrieves the position of the caret inside the accessible object.
   --  Since: gtk+ 4.14
   --  @return the position of the caret, in characters

   type Virtual_Get_Contents is access function
     (Self    : Gtk_Accessible_Text;
      Start   : Guint;
      The_End : Guint) return System.Address;
   pragma Convention (C, Virtual_Get_Contents);
   --  Retrieve the current contents of the accessible object within the given
   --  range.
   --  If End is `G_MAXUINT`, the end of the range is the full content of the
   --  accessible object.
   --  Since: gtk+ 4.14
   --  @param Start the beginning of the range, in characters
   --  @param The_End the end of the range, in characters
   --  @return the requested slice of the contents of the accessible object,
   --  as UTF-8. Note that the slice does not have to be NUL-terminated

   type Virtual_Get_Contents_At is access function
     (Self        : Gtk_Accessible_Text;
      Offset      : Guint;
      Granularity : Gtk_Accessible_Text_Granularity;
      Start       : access Guint;
      The_End     : access Guint) return System.Address;
   pragma Convention (C, Virtual_Get_Contents_At);
   --  Retrieve the current contents of the accessible object starting from
   --  the given offset, and using the given granularity.
   --  The Start and End values contain the boundaries of the text.
   --  Since: gtk+ 4.14
   --  @param Offset the offset, in characters
   --  @param Granularity the granularity of the query
   --  @param Start the start of the range, in characters
   --  @param The_End the end of the range, in characters
   --  @return the requested slice of the contents of the accessible object,
   --  as UTF-8. Note that the slice does not have to be NUL-terminated

   type Virtual_Get_Default_Attributes is access procedure
     (Self             : Gtk_Accessible_Text;
      Attribute_Names  : out Gtkada.Types.chars_ptr_array;
      Attribute_Values : out Gtkada.Types.chars_ptr_array);
   pragma Convention (C, Virtual_Get_Default_Attributes);
   --  Retrieves the default text attributes inside the accessible object.
   --  Each attribute is composed by:
   --  - a name - a value
   --  It is left to the implementation to determine the serialization format
   --  of the value to a string.
   --  GTK provides support for various text attribute names and values, but
   --  implementations of this interface are free to add their own attributes.
   --  Since: gtk+ 4.14
   --  @param Attribute_Names the names of the default attributes inside the
   --  accessible object
   --  @param Attribute_Values the values of the default attributes inside the
   --  accessible object

   type Virtual_Get_Extents is access function
     (Self    : Gtk_Accessible_Text;
      Start   : Guint;
      The_End : Guint;
      Extents : in out graphene_rect_t) return Glib.Gboolean;
   pragma Convention (C, Virtual_Get_Extents);
   --  Obtains the extents of a range of text, in widget coordinates.
   --  Since: gtk+ 4.16
   --  @param Start the start offset, in characters
   --  @param The_End the end offset, in characters, Extents (out
   --  caller-allocates): return location for the extents
   --  @return true if the extents were filled in, false otherwise

   type Virtual_Get_Offset is access function
     (Self   : Gtk_Accessible_Text;
      Point  : in out graphene_point_t;
      Offset : access Guint) return Glib.Gboolean;
   pragma Convention (C, Virtual_Get_Offset);
   --  Gets the text offset at a given point.
   --  Since: gtk+ 4.16
   --  @param Point a point in widget coordinates of Self
   --  @param Offset return location for the text offset at Point
   --  @return true if the offset was set, false otherwise

   type Virtual_Get_Selection is access function
     (Self     : Gtk_Accessible_Text;
      N_Ranges : access Gsize;
      Ranges   : access Gtk_Accessible_Text_Range_Array)
   return Glib.Gboolean;
   pragma Convention (C, Virtual_Get_Selection);
   --  Retrieves the selection ranges in the accessible object.
   --  If this function returns true, `n_ranges` will be set to a value
   --  greater than or equal to one, and Ranges will be set to a newly
   --  allocated array of [structGtk.AccessibleTextRange].
   --  Since: gtk+ 4.14
   --  @param N_Ranges the number of selection ranges
   --  @param Ranges the selection ranges
   --  @return true if there is at least a selection inside the accessible
   --  object, and false otherwise

   type Virtual_Set_Caret_Position is access function
     (Self   : Gtk_Accessible_Text;
      Offset : Guint) return Glib.Gboolean;
   pragma Convention (C, Virtual_Set_Caret_Position);
   --  Sets the caret position.
   --  Since: gtk+ 4.22
   --  @param Offset the text offset in characters
   --  @return true if the caret position was updated

   type Virtual_Set_Selection is access function
     (Self   : Gtk_Accessible_Text;
      I      : Gsize;
      GRange : Gtk_Accessible_Text_Range) return Glib.Gboolean;
   pragma Convention (C, Virtual_Set_Selection);
   --  Sets the caret position.
   --  Since: gtk+ 4.22
   --  @param I the selection to set
   --  @param GRange the range to set the selection to
   --  @return true if the selection was updated

   subtype Accessible_Text_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Get_Attributes
     (Self    : Accessible_Text_Interface_Descr;
      Handler : Virtual_Get_Attributes);
   pragma Import (C, Set_Get_Attributes, "gtkada_Accessible_Text_set_get_attributes");

   procedure Set_Get_Caret_Position
     (Self    : Accessible_Text_Interface_Descr;
      Handler : Virtual_Get_Caret_Position);
   pragma Import (C, Set_Get_Caret_Position, "gtkada_Accessible_Text_set_get_caret_position");

   procedure Set_Get_Contents
     (Self    : Accessible_Text_Interface_Descr;
      Handler : Virtual_Get_Contents);
   pragma Import (C, Set_Get_Contents, "gtkada_Accessible_Text_set_get_contents");

   procedure Set_Get_Contents_At
     (Self    : Accessible_Text_Interface_Descr;
      Handler : Virtual_Get_Contents_At);
   pragma Import (C, Set_Get_Contents_At, "gtkada_Accessible_Text_set_get_contents_at");

   procedure Set_Get_Default_Attributes
     (Self    : Accessible_Text_Interface_Descr;
      Handler : Virtual_Get_Default_Attributes);
   pragma Import (C, Set_Get_Default_Attributes, "gtkada_Accessible_Text_set_get_default_attributes");

   procedure Set_Get_Extents
     (Self    : Accessible_Text_Interface_Descr;
      Handler : Virtual_Get_Extents);
   pragma Import (C, Set_Get_Extents, "gtkada_Accessible_Text_set_get_extents");

   procedure Set_Get_Offset
     (Self    : Accessible_Text_Interface_Descr;
      Handler : Virtual_Get_Offset);
   pragma Import (C, Set_Get_Offset, "gtkada_Accessible_Text_set_get_offset");

   procedure Set_Get_Selection
     (Self    : Accessible_Text_Interface_Descr;
      Handler : Virtual_Get_Selection);
   pragma Import (C, Set_Get_Selection, "gtkada_Accessible_Text_set_get_selection");

   procedure Set_Set_Caret_Position
     (Self    : Accessible_Text_Interface_Descr;
      Handler : Virtual_Set_Caret_Position);
   pragma Import (C, Set_Set_Caret_Position, "gtkada_Accessible_Text_set_set_caret_position");

   procedure Set_Set_Selection
     (Self    : Accessible_Text_Interface_Descr;
      Handler : Virtual_Set_Selection);
   pragma Import (C, Set_Set_Selection, "gtkada_Accessible_Text_set_set_selection");
   --  See Glib.Object.Add_Interface

private

Null_Gtk_Accessible_Text : constant Gtk_Accessible_Text :=
   Gtk_Accessible_Text (Glib.Types.Null_Interface);
end Gtk.Accessible_Text;
