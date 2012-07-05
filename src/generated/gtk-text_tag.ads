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

--  <description>
--  You may wish to begin by reading the <link linkend="TextWidget">text
--  widget conceptual overview</link> which gives an overview of all the
--  objects and data types related to the text widget and how they work
--  together.
--
--  Tags should be in the Gtk.Text_Tag_Table.Gtk_Text_Tag_Table for a given
--  Gtk.Text_Buffer.Gtk_Text_Buffer before using them with that buffer.
--
--  gtk_text_buffer_create_tag is the best way to create tags. See
--  <application>gtk3-demo</application> for numerous examples.
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
   --  "name": tag name, or null

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_text_tag_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Priority
      (Tag : not null access Gtk_Text_Tag_Record) return Gint;
   procedure Set_Priority
      (Tag      : not null access Gtk_Text_Tag_Record;
       Priority : Gint);
   --  Sets the priority of a Gtk.Text_Tag.Gtk_Text_Tag. Valid priorities are
   --  start at 0 and go to one less than gtk_text_tag_table_get_size. Each tag
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
   --
   --  Name: Accumulative_Margin_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether the margins accumulate or override each other.
   --
   --  When set to True the margins of this tag are added to the margins of
   --  any other non-accumulative margins present. When set to False the
   --  margins override one another (the default).
   --
   --  Name: Background_Property
   --  Type: UTF8_String
   --  Flags: write
   --
   --  Name: Background_Full_Height_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Background_Full_Height_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Background_Gdk_Property
   --  Type: Gdk.Color.Gdk_Color
   --  Flags: read-write
   --
   --  Name: Background_Rgba_Property
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  Flags: read-write
   --  Background color as a Gdk.RGBA.Gdk_RGBA.
   --
   --  Name: Background_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Direction_Property
   --  Type: Gtk.Enums.Gtk_Text_Direction
   --  Flags: read-write
   --
   --  Name: Editable_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Editable_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Family_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --
   --  Name: Family_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Font_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  Font description as string, e.g. \"Sans Italic 12\".
   --
   --  Note that the initial value of this property depends on the internals
   --  of Pango.Font_Description.Pango_Font_Description.
   --
   --  Name: Font_Desc_Property
   --  Type: Pango.Font.Pango_Font_Description
   --  Flags: read-write
   --
   --  Name: Foreground_Property
   --  Type: UTF8_String
   --  Flags: write
   --
   --  Name: Foreground_Gdk_Property
   --  Type: Gdk.Color.Gdk_Color
   --  Flags: read-write
   --
   --  Name: Foreground_Rgba_Property
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  Flags: read-write
   --  Foreground color as a Gdk.RGBA.Gdk_RGBA.
   --
   --  Name: Foreground_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Indent_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Indent_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Invisible_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether this text is hidden.
   --
   --  Note that there may still be problems with the support for invisible
   --  text, in particular when navigating programmatically inside a buffer
   --  containing invisible segments.
   --
   --  Name: Invisible_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Justification_Property
   --  Type: Gtk.Enums.Gtk_Justification
   --  Flags: read-write
   --
   --  Name: Justification_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Language_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The language this text is in, as an ISO code. Pango can use this as a
   --  hint when rendering the text. If not set, an appropriate default will be
   --  used.
   --
   --  Note that the initial value of this property depends on the current
   --  locale, see also Gtk.Main.Get_Default_Language.
   --
   --  Name: Language_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Left_Margin_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Left_Margin_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Name_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --
   --  Name: Paragraph_Background_Property
   --  Type: UTF8_String
   --  Flags: write
   --  The paragraph background color as a string.
   --
   --  Name: Paragraph_Background_Gdk_Property
   --  Type: Gdk.Color.Gdk_Color
   --  Flags: read-write
   --  The paragraph background color as a as a Gdk.Color.Gdk_Color.
   --
   --  Name: Paragraph_Background_Rgba_Property
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  Flags: read-write
   --  The paragraph background color as a as a Gdk.RGBA.Gdk_RGBA.
   --
   --  Name: Paragraph_Background_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Pixels_Above_Lines_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Pixels_Above_Lines_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Pixels_Below_Lines_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Pixels_Below_Lines_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Pixels_Inside_Wrap_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Pixels_Inside_Wrap_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Right_Margin_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Right_Margin_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Rise_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Rise_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Scale_Property
   --  Type: Gdouble
   --  Flags: read-write
   --
   --  Name: Scale_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Size_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Size_Points_Property
   --  Type: Gdouble
   --  Flags: read-write
   --
   --  Name: Size_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Stretch_Property
   --  Type: Pango.Enums.Stretch
   --  Flags: read-write
   --
   --  Name: Stretch_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Strikethrough_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Strikethrough_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Style_Property
   --  Type: Pango.Enums.Style
   --  Flags: read-write
   --
   --  Name: Style_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Tabs_Property
   --  Type: Pango.Tab_Array
   --  Flags: read-write
   --
   --  Name: Tabs_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Underline_Property
   --  Type: Pango.Enums.Underline
   --  Flags: read-write
   --
   --  Name: Underline_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Variant_Property
   --  Type: Pango.Enums.Variant
   --  Flags: read-write
   --
   --  Name: Variant_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Weight_Property
   --  Type: Pango.Enums.Weight
   --  Flags: read-write
   --
   --  Name: Weight_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Wrap_Mode_Property
   --  Type: Gtk.Enums.Gtk_Wrap_Mode
   --  Flags: read-write
   --
   --  Name: Wrap_Mode_Set_Property
   --  Type: Boolean
   --  Flags: read-write

   Accumulative_Margin_Property : constant Glib.Properties.Property_Boolean;
   Background_Property : constant Glib.Properties.Property_String;
   Background_Full_Height_Property : constant Glib.Properties.Property_Boolean;
   Background_Full_Height_Set_Property : constant Glib.Properties.Property_Boolean;
   Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color;
   Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   Background_Set_Property : constant Glib.Properties.Property_Boolean;
   Direction_Property : constant Gtk.Enums.Property_Gtk_Text_Direction;
   Editable_Property : constant Glib.Properties.Property_Boolean;
   Editable_Set_Property : constant Glib.Properties.Property_Boolean;
   Family_Property : constant Glib.Properties.Property_String;
   Family_Set_Property : constant Glib.Properties.Property_Boolean;
   Font_Property : constant Glib.Properties.Property_String;
   Font_Desc_Property : constant Pango.Font.Property_Font_Description;
   Foreground_Property : constant Glib.Properties.Property_String;
   Foreground_Gdk_Property : constant Gdk.Color.Property_Gdk_Color;
   Foreground_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   Foreground_Set_Property : constant Glib.Properties.Property_Boolean;
   Indent_Property : constant Glib.Properties.Property_Int;
   Indent_Set_Property : constant Glib.Properties.Property_Boolean;
   Invisible_Property : constant Glib.Properties.Property_Boolean;
   Invisible_Set_Property : constant Glib.Properties.Property_Boolean;
   Justification_Property : constant Gtk.Enums.Property_Gtk_Justification;
   Justification_Set_Property : constant Glib.Properties.Property_Boolean;
   Language_Property : constant Glib.Properties.Property_String;
   Language_Set_Property : constant Glib.Properties.Property_Boolean;
   Left_Margin_Property : constant Glib.Properties.Property_Int;
   Left_Margin_Set_Property : constant Glib.Properties.Property_Boolean;
   Name_Property : constant Glib.Properties.Property_String;
   Paragraph_Background_Property : constant Glib.Properties.Property_String;
   Paragraph_Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color;
   Paragraph_Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
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
   Scale_Set_Property : constant Glib.Properties.Property_Boolean;
   Size_Property : constant Glib.Properties.Property_Int;
   Size_Points_Property : constant Glib.Properties.Property_Double;
   Size_Set_Property : constant Glib.Properties.Property_Boolean;
   Stretch_Property : constant Pango.Enums.Property_Stretch;
   Stretch_Set_Property : constant Glib.Properties.Property_Boolean;
   Strikethrough_Property : constant Glib.Properties.Property_Boolean;
   Strikethrough_Set_Property : constant Glib.Properties.Property_Boolean;
   Style_Property : constant Pango.Enums.Property_Style;
   Style_Set_Property : constant Glib.Properties.Property_Boolean;
   Tabs_Property : constant Glib.Properties.Property_Boxed;
   Tabs_Set_Property : constant Glib.Properties.Property_Boolean;
   Underline_Property : constant Pango.Enums.Property_Underline;
   Underline_Set_Property : constant Glib.Properties.Property_Boolean;
   Variant_Property : constant Pango.Enums.Property_Variant;
   Variant_Set_Property : constant Glib.Properties.Property_Boolean;
   Weight_Property : constant Pango.Enums.Property_Weight;
   Weight_Set_Property : constant Glib.Properties.Property_Boolean;
   Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Wrap_Mode;
   Wrap_Mode_Set_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "event"
   --     function Handler
   --       (Self   : access Gtk_Text_Tag_Record'Class;
   --        Object : not null access Glib.Object.GObject_Record'Class;
   --        Event  : Gdk.Event;
   --        Iter   : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --    --  "object": the object the event was fired from (typically a
   --    --  Gtk.Text_View.Gtk_Text_View)
   --    --  "event": the event which triggered the signal
   --    --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter pointing at the location the
   --    --  event occured
   --  The ::event signal is emitted when an event occurs on a region of the
   --  buffer marked with this tag.
   --
   --  event. False to propagate the event further.
   --
   --  Returns True to stop other handlers from being invoked for the

   Signal_Event : constant Glib.Signal_Name := "event";

private
   Accumulative_Margin_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("accumulative-margin");
   Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("background");
   Background_Full_Height_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background-full-height");
   Background_Full_Height_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background-full-height-set");
   Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("background-gdk");
   Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("background-rgba");
   Background_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background-set");
   Direction_Property : constant Gtk.Enums.Property_Gtk_Text_Direction :=
     Gtk.Enums.Build ("direction");
   Editable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
   Editable_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable-set");
   Family_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("family");
   Family_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("family-set");
   Font_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font");
   Font_Desc_Property : constant Pango.Font.Property_Font_Description :=
     Pango.Font.Build ("font-desc");
   Foreground_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("foreground");
   Foreground_Gdk_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("foreground-gdk");
   Foreground_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("foreground-rgba");
   Foreground_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("foreground-set");
   Indent_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("indent");
   Indent_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("indent-set");
   Invisible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible");
   Invisible_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible-set");
   Justification_Property : constant Gtk.Enums.Property_Gtk_Justification :=
     Gtk.Enums.Build ("justification");
   Justification_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("justification-set");
   Language_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("language");
   Language_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("language-set");
   Left_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("left-margin");
   Left_Margin_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("left-margin-set");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Paragraph_Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("paragraph-background");
   Paragraph_Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("paragraph-background-gdk");
   Paragraph_Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("paragraph-background-rgba");
   Paragraph_Background_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("paragraph-background-set");
   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-above-lines");
   Pixels_Above_Lines_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("pixels-above-lines-set");
   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-below-lines");
   Pixels_Below_Lines_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("pixels-below-lines-set");
   Pixels_Inside_Wrap_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-inside-wrap");
   Pixels_Inside_Wrap_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("pixels-inside-wrap-set");
   Right_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("right-margin");
   Right_Margin_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("right-margin-set");
   Rise_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("rise");
   Rise_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("rise-set");
   Scale_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("scale");
   Scale_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("scale-set");
   Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("size");
   Size_Points_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("size-points");
   Size_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("size-set");
   Stretch_Property : constant Pango.Enums.Property_Stretch :=
     Pango.Enums.Build ("stretch");
   Stretch_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("stretch-set");
   Strikethrough_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strikethrough");
   Strikethrough_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strikethrough-set");
   Style_Property : constant Pango.Enums.Property_Style :=
     Pango.Enums.Build ("style");
   Style_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("style-set");
   Tabs_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("tabs");
   Tabs_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("tabs-set");
   Underline_Property : constant Pango.Enums.Property_Underline :=
     Pango.Enums.Build ("underline");
   Underline_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("underline-set");
   Variant_Property : constant Pango.Enums.Property_Variant :=
     Pango.Enums.Build ("variant");
   Variant_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("variant-set");
   Weight_Property : constant Pango.Enums.Property_Weight :=
     Pango.Enums.Build ("weight");
   Weight_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("weight-set");
   Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Wrap_Mode :=
     Gtk.Enums.Build ("wrap-mode");
   Wrap_Mode_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap-mode-set");
end Gtk.Text_Tag;
