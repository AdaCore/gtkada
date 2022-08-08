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
--  You may wish to begin by reading the [text widget conceptual
--  overview][TextWidget] which gives an overview of all the objects and data
--  types related to the text widget and how they work together.
--
--  Tags should be in the Gtk.Text_Tag_Table.Gtk_Text_Tag_Table for a given
--  Gtk.Text_Buffer.Gtk_Text_Buffer before using them with that buffer.
--
--  gtk_text_buffer_create_tag is the best way to create tags. See "gtk3-demo"
--  for numerous examples.
--
--  For each property of Gtk.Text_Tag.Gtk_Text_Tag, there is a "set" property,
--  e.g. "font-set" corresponds to "font". These "set" properties reflect
--  whether a property has been set or not. They are maintained by GTK+ and you
--  should not set them independently.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Color;       use Gdk.Color;
with Gdk.RGBA;        use Gdk.RGBA;
with Glib;            use Glib;
with Glib.GSlist;     use Glib.GSlist;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Gtk.Enums;       use Gtk.Enums;
with Pango.Enums;     use Pango.Enums;
with Pango.Font;      use Pango.Font;

package Gtk.Text_Tag is

   type Gtk_Text_Tag_Record is new GObject_Record with null record;
   type Gtk_Text_Tag is access all Gtk_Text_Tag_Record'Class;

   function Convert (R : Gtk.Text_Tag.Gtk_Text_Tag) return System.Address;
   function Convert (R : System.Address) return Gtk.Text_Tag.Gtk_Text_Tag;
   package Text_Tag_List is new Generic_SList (Gtk.Text_Tag.Gtk_Text_Tag);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Tag : out Gtk_Text_Tag; Name : UTF8_String := "");
   procedure Initialize
      (Tag  : not null access Gtk_Text_Tag_Record'Class;
       Name : UTF8_String := "");
   --  Creates a Gtk.Text_Tag.Gtk_Text_Tag. Configure the tag using object
   --  arguments, i.e. using g_object_set.
   --  Newly created tags must be added to the tags table for the buffer you
   --  intend to use them in, as in: "Gtk.Text_Tag_Table.Add (Get_Tag_Table
   --  (Buffer), Tag)". See also Gtk.Text_Buffer.Create_Tag which is a more
   --  convenient way of creating a tag.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "name": tag name, or null

   function Gtk_Text_Tag_New (Name : UTF8_String := "") return Gtk_Text_Tag;
   --  Creates a Gtk.Text_Tag.Gtk_Text_Tag. Configure the tag using object
   --  arguments, i.e. using g_object_set.
   --  Newly created tags must be added to the tags table for the buffer you
   --  intend to use them in, as in: "Gtk.Text_Tag_Table.Add (Get_Tag_Table
   --  (Buffer), Tag)". See also Gtk.Text_Buffer.Create_Tag which is a more
   --  convenient way of creating a tag.
   --  "name": tag name, or null

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_text_tag_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed
      (Tag          : not null access Gtk_Text_Tag_Record;
       Size_Changed : Boolean);
   --  Emits the Gtk.Text_Tag_Table.Gtk_Text_Tag_Table::tag-changed signal on
   --  the Gtk.Text_Tag_Table.Gtk_Text_Tag_Table where the tag is included.
   --  The signal is already emitted when setting a Gtk.Text_Tag.Gtk_Text_Tag
   --  property. This function is useful for a Gtk.Text_Tag.Gtk_Text_Tag
   --  subclass.
   --  Since: gtk+ 3.20
   --  "size_changed": whether the change affects the
   --  Gtk.Text_View.Gtk_Text_View layout.

   function Get_Priority
      (Tag : not null access Gtk_Text_Tag_Record) return Glib.Gint;
   --  Get the tag priority.

   procedure Set_Priority
      (Tag      : not null access Gtk_Text_Tag_Record;
       Priority : Glib.Gint);
   --  Sets the priority of a Gtk.Text_Tag.Gtk_Text_Tag. Valid priorities
   --  start at 0 and go to one less than Gtk.Text_Tag_Table.Get_Size. Each tag
   --  in a table has a unique priority; setting the priority of one tag shifts
   --  the priorities of all the other tags in the table to maintain a unique
   --  priority for each tag. Higher priority tags "win" if two tags both set
   --  the same text attribute. When adding a tag to a tag table, it will be
   --  assigned the highest priority in the table by default; so normally the
   --  precedence of a set of tags is the order in which they were added to the
   --  table, or created with gtk_text_buffer_create_tag, which adds the tag to
   --  the buffer's table automatically.
   --  "priority": the new priority

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accumulative_Margin_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the margins accumulate or override each other.
   --
   --  When set to True the margins of this tag are added to the margins of
   --  any other non-accumulative margins present. When set to False the
   --  margins override one another (the default).

   Background_Property : constant Glib.Properties.Property_String;
   --  Flags: write

   Background_Full_Height_Property : constant Glib.Properties.Property_Boolean;

   Background_Full_Height_Set_Property : constant Glib.Properties.Property_Boolean;

   Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color;
   --  Type: Gdk.Color.Gdk_Color
   --  Background color as a Gdk.Color.Gdk_Color.

   Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  Background color as a Gdk.RGBA.Gdk_RGBA.

   Background_Set_Property : constant Glib.Properties.Property_Boolean;

   Direction_Property : constant Gtk.Enums.Property_Gtk_Text_Direction;

   Editable_Property : constant Glib.Properties.Property_Boolean;

   Editable_Set_Property : constant Glib.Properties.Property_Boolean;

   Fallback_Property : constant Glib.Properties.Property_Boolean;
   --  Whether font fallback is enabled.
   --
   --  When set to True, other fonts will be substituted where the current
   --  font is missing glyphs.

   Fallback_Set_Property : constant Glib.Properties.Property_Boolean;

   Family_Property : constant Glib.Properties.Property_String;

   Family_Set_Property : constant Glib.Properties.Property_Boolean;

   Font_Property : constant Glib.Properties.Property_String;
   --  Font description as string, e.g. \"Sans Italic 12\".
   --
   --  Note that the initial value of this property depends on the internals
   --  of Pango.Font.Pango_Font_Description.

   Font_Desc_Property : constant Pango.Font.Property_Font_Description;
   --  Type: Pango.Font.Pango_Font_Description

   Font_Features_Property : constant Glib.Properties.Property_String;
   --  OpenType font features, as a string.

   Font_Features_Set_Property : constant Glib.Properties.Property_Boolean;

   Foreground_Property : constant Glib.Properties.Property_String;
   --  Flags: write

   Foreground_Gdk_Property : constant Gdk.Color.Property_Gdk_Color;
   --  Type: Gdk.Color.Gdk_Color
   --  Foreground color as a Gdk.Color.Gdk_Color.

   Foreground_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  Foreground color as a Gdk.RGBA.Gdk_RGBA.

   Foreground_Set_Property : constant Glib.Properties.Property_Boolean;

   Indent_Property : constant Glib.Properties.Property_Int;

   Indent_Set_Property : constant Glib.Properties.Property_Boolean;

   Invisible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this text is hidden.
   --
   --  Note that there may still be problems with the support for invisible
   --  text, in particular when navigating programmatically inside a buffer
   --  containing invisible segments.

   Invisible_Set_Property : constant Glib.Properties.Property_Boolean;

   Justification_Property : constant Gtk.Enums.Property_Gtk_Justification;

   Justification_Set_Property : constant Glib.Properties.Property_Boolean;

   Language_Property : constant Glib.Properties.Property_String;
   --  The language this text is in, as an ISO code. Pango can use this as a
   --  hint when rendering the text. If not set, an appropriate default will be
   --  used.
   --
   --  Note that the initial value of this property depends on the current
   --  locale, see also Gtk.Main.Get_Default_Language.

   Language_Set_Property : constant Glib.Properties.Property_Boolean;

   Left_Margin_Property : constant Glib.Properties.Property_Int;

   Left_Margin_Set_Property : constant Glib.Properties.Property_Boolean;

   Letter_Spacing_Property : constant Glib.Properties.Property_Int;
   --  Extra spacing between graphemes, in Pango units.

   Letter_Spacing_Set_Property : constant Glib.Properties.Property_Boolean;

   Name_Property : constant Glib.Properties.Property_String;

   Paragraph_Background_Property : constant Glib.Properties.Property_String;
   --  Flags: write
   --  The paragraph background color as a string.

   Paragraph_Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color;
   --  Type: Gdk.Color.Gdk_Color
   --  The paragraph background color as a Gdk.Color.Gdk_Color.

   Paragraph_Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  The paragraph background color as a Gdk.RGBA.Gdk_RGBA.

   Paragraph_Background_Set_Property : constant Glib.Properties.Property_Boolean;

   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int;

   Pixels_Above_Lines_Set_Property : constant Glib.Properties.Property_Boolean;

   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int;

   Pixels_Below_Lines_Set_Property : constant Glib.Properties.Property_Boolean;

   Pixels_Inside_Wrap_Property : constant Glib.Properties.Property_Int;

   Pixels_Inside_Wrap_Set_Property : constant Glib.Properties.Property_Boolean;

   Right_Margin_Property : constant Glib.Properties.Property_Int;

   Right_Margin_Set_Property : constant Glib.Properties.Property_Boolean;

   Rise_Property : constant Glib.Properties.Property_Int;

   Rise_Set_Property : constant Glib.Properties.Property_Boolean;

   Scale_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble

   Scale_Set_Property : constant Glib.Properties.Property_Boolean;

   Size_Property : constant Glib.Properties.Property_Int;

   Size_Points_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble

   Size_Set_Property : constant Glib.Properties.Property_Boolean;

   Stretch_Property : constant Pango.Enums.Property_Stretch;
   --  Type: Pango.Enums.Stretch

   Stretch_Set_Property : constant Glib.Properties.Property_Boolean;

   Strikethrough_Property : constant Glib.Properties.Property_Boolean;

   Strikethrough_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  This property modifies the color of strikeouts. If not set, strikeouts
   --  will use the forground color.

   Strikethrough_Rgba_Set_Property : constant Glib.Properties.Property_Boolean;
   --  If the Gtk.Text_Tag.Gtk_Text_Tag:strikethrough-rgba property has been
   --  set.

   Strikethrough_Set_Property : constant Glib.Properties.Property_Boolean;

   Style_Property : constant Pango.Enums.Property_Style;
   --  Type: Pango.Enums.Style

   Style_Set_Property : constant Glib.Properties.Property_Boolean;

   Tabs_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Pango.Tab_Array

   Tabs_Set_Property : constant Glib.Properties.Property_Boolean;

   Underline_Property : constant Pango.Enums.Property_Underline;
   --  Type: Pango.Enums.Underline

   Underline_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  This property modifies the color of underlines. If not set, underlines
   --  will use the forground color.
   --
   --  If Gtk.Text_Tag.Gtk_Text_Tag:underline is set to
   --  Pango.Enums.Pango_Underline_Error, an alternate color may be applied
   --  instead of the foreground. Setting this property will always override
   --  those defaults.

   Underline_Rgba_Set_Property : constant Glib.Properties.Property_Boolean;
   --  If the Gtk.Text_Tag.Gtk_Text_Tag:underline-rgba property has been set.

   Underline_Set_Property : constant Glib.Properties.Property_Boolean;

   Variant_Property : constant Pango.Enums.Property_Variant;
   --  Type: Pango.Enums.Variant

   Variant_Set_Property : constant Glib.Properties.Property_Boolean;

   Weight_Property : constant Pango.Enums.Property_Weight;
   --  Type: Pango.Enums.Weight

   Weight_Set_Property : constant Glib.Properties.Property_Boolean;

   Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Wrap_Mode;

   Wrap_Mode_Set_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   Signal_Event : constant Glib.Signal_Name := "event";
   --  The ::event signal is emitted when an event occurs on a region of the
   --  buffer marked with this tag.
   --    function Handler
   --       (Self   : access Gtk_Text_Tag_Record'Class;
   --        Object : not null access Glib.Object.GObject_Record'Class;
   --        Event  : Gdk.Event.Gdk_Event;
   --        Iter   : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean
   -- 
   --  Callback parameters:
   --    --  "object": the object the event was fired from (typically a
   --    --  Gtk.Text_View.Gtk_Text_View)
   --    --  "event": the event which triggered the signal
   --    --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter pointing at the location the
   --    --  event occurred
   --    --  Returns True to stop other handlers from being invoked for the
   -- event. False to propagate the event further.

private
   Wrap_Mode_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap-mode-set");
   Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Wrap_Mode :=
     Gtk.Enums.Build ("wrap-mode");
   Weight_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("weight-set");
   Weight_Property : constant Pango.Enums.Property_Weight :=
     Pango.Enums.Build ("weight");
   Variant_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("variant-set");
   Variant_Property : constant Pango.Enums.Property_Variant :=
     Pango.Enums.Build ("variant");
   Underline_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("underline-set");
   Underline_Rgba_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("underline-rgba-set");
   Underline_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("underline-rgba");
   Underline_Property : constant Pango.Enums.Property_Underline :=
     Pango.Enums.Build ("underline");
   Tabs_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("tabs-set");
   Tabs_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("tabs");
   Style_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("style-set");
   Style_Property : constant Pango.Enums.Property_Style :=
     Pango.Enums.Build ("style");
   Strikethrough_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strikethrough-set");
   Strikethrough_Rgba_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strikethrough-rgba-set");
   Strikethrough_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("strikethrough-rgba");
   Strikethrough_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strikethrough");
   Stretch_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("stretch-set");
   Stretch_Property : constant Pango.Enums.Property_Stretch :=
     Pango.Enums.Build ("stretch");
   Size_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("size-set");
   Size_Points_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("size-points");
   Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("size");
   Scale_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("scale-set");
   Scale_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("scale");
   Rise_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("rise-set");
   Rise_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("rise");
   Right_Margin_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("right-margin-set");
   Right_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("right-margin");
   Pixels_Inside_Wrap_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("pixels-inside-wrap-set");
   Pixels_Inside_Wrap_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-inside-wrap");
   Pixels_Below_Lines_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("pixels-below-lines-set");
   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-below-lines");
   Pixels_Above_Lines_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("pixels-above-lines-set");
   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-above-lines");
   Paragraph_Background_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("paragraph-background-set");
   Paragraph_Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("paragraph-background-rgba");
   Paragraph_Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("paragraph-background-gdk");
   Paragraph_Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("paragraph-background");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Letter_Spacing_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("letter-spacing-set");
   Letter_Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("letter-spacing");
   Left_Margin_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("left-margin-set");
   Left_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("left-margin");
   Language_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("language-set");
   Language_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("language");
   Justification_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("justification-set");
   Justification_Property : constant Gtk.Enums.Property_Gtk_Justification :=
     Gtk.Enums.Build ("justification");
   Invisible_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible-set");
   Invisible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible");
   Indent_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("indent-set");
   Indent_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("indent");
   Foreground_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("foreground-set");
   Foreground_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("foreground-rgba");
   Foreground_Gdk_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("foreground-gdk");
   Foreground_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("foreground");
   Font_Features_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("font-features-set");
   Font_Features_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font-features");
   Font_Desc_Property : constant Pango.Font.Property_Font_Description :=
     Pango.Font.Build ("font-desc");
   Font_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font");
   Family_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("family-set");
   Family_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("family");
   Fallback_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("fallback-set");
   Fallback_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("fallback");
   Editable_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable-set");
   Editable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
   Direction_Property : constant Gtk.Enums.Property_Gtk_Text_Direction :=
     Gtk.Enums.Build ("direction");
   Background_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background-set");
   Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("background-rgba");
   Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("background-gdk");
   Background_Full_Height_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background-full-height-set");
   Background_Full_Height_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background-full-height");
   Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("background");
   Accumulative_Margin_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("accumulative-margin");
end Gtk.Text_Tag;
