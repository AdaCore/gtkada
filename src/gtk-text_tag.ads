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
--  The Gtk_Text_Tag data type.
--  </description>
--  <c_version>1.3.6</c_version>

with Gtk.Enums;
with Pango.Enums;
with Glib.Properties;
with Pango.Font;
with Gdk.Color;
pragma Elaborate_All (Gdk.Color);

package Gtk.Text_Tag is

   type Gtk_Text_Tag_Record is new GObject_Record with private;
   type Gtk_Text_Tag is access all Gtk_Text_Tag_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Text_Tag; Name : String := "");
   --  Create a new Gtk_Text_Tag.

   procedure Initialize
     (Widget : access Gtk_Text_Tag_Record'Class;
      Name   : String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with this widget.

   function Get_Priority (Tag : access Gtk_Text_Tag_Record) return Gint;
   --  Return the tag priority.

   procedure Set_Priority (Tag : access Gtk_Text_Tag_Record; Priority : Gint);
   --  Set the priority of a Gtk_Text_Tag.
   --  Valid priorities start at 0 and go to one less than Table_Size.
   --  Each tag in a table has a unique priority; setting the priority of one
   --  tag shifts the priorities of all the other tags in the table to maintain
   --  a unique priority for each tag. Higher priority tags "win" if two tags
   --  both set the same text attribute. When adding a tag to a tag table, it
   --  will be assigned the highest priority in the table by default; so
   --  normally the precedence of a set of tags is the order in which they were
   --  added to the table, or created with Gtk.Text_Buffer.Create_Tag, which
   --  adds the tag to the buffer's table automatically.

   --  function Event
   --    (Tag          : access Gtk_Text_Tag_Record;
   --     Event_Object : access Glib.Object.GObject_Record'Class;
   --     Event        : Gdk.Event.Gdk_Event;
   --     Iter         : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  ??? Can not be bound here. Circular dependency problem with
   --  ??? Gtk_Text_Iter.
   --  Emit the "event" signal on Tag.
   --  Event_Object: object that received the event, such as a widget.
   --  Event: the event.
   --  Iter: location where the event was received.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Name_Property
   --    Type:  String
   --    Flags: read-write (construct only)
   --    Descr: Name used to refer to the text tag
   --    See also: Gtk_New
   --
   --  - Name:  Background_Property
   --    Type:  String
   --    Flags: writable
   --    Descr: Background color as a string
   --    See also:  <none>
   --
   --  - Name:  Background_Gdk_Property
   --    Type:  Gdk_Color
   --    Flags: read-write
   --    Descr: Background color
   --    See also:  <none>
   --
   --  - Name:  Background_Full_Height_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether the background color fills the entire line height or
   --           only the height of the tagged characters
   --    See also:  <none>
   --
   --  - Name:  Background_Stipple_Property
   --    Type:  Gdk_Pixmap'Class
   --    Flags: read-write
   --    Descr: Bitmap to use as a mask when drawing the text background
   --    See also:  <none>
   --
   --  - Name:  Foreground_Property
   --    Type:  String
   --    Flags: writable
   --    Descr: Foreground color as a string
   --    See also:  <none>
   --
   --  - Name:  Foreground_Gdk_Property
   --    Type:  Gdk_Color
   --    Flags: read-write
   --    Descr: Foreground color
   --    See also:  <none>
   --
   --  - Name:  Foreground_Stipple_Property
   --    Type:  Gdk_Pixmap'Class
   --    Flags: read-write
   --    Descr: Bitmap to use as a mask when drawing the text foreground
   --    See also:  <none>
   --
   --  - Name:  Direction_Property
   --    Type:  Gtk_Text_Direction
   --    Flags: read-write
   --    Descr: Text direction, e.g. right-to-left or left-to-right
   --    See also:  <none>
   --
   --  - Name:  Editable_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether the text can be modified by the user
   --    See also:  <none>
   --
   --  - Name:  Font_Property
   --    Type:  String
   --    Flags: read-write
   --    Descr: Font description as a string
   --    See also:  <none>
   --
   --  - Name:  Font_Desc_Property
   --    Type:  Pango_Font_Description
   --    Flags: read-write
   --    Descr: Font description
   --    See also:  <none>
   --
   --  - Name:  Family_Property
   --    Type:  String
   --    Flags: read-write
   --    Descr: Name of the font family, e.g. Sans, Helvetica, Times, Monospace
   --    See also:  <none>
   --
   --  - Name:  Style_Property
   --    Type:  Pango_Enums.Style
   --    Flags: read-write
   --    Descr: Font style
   --    See also:  <none>
   --
   --  - Name:  Variant_Property
   --    Type:  Pango_Type_Variant
   --    Flags: read-write
   --    Descr: Font variant
   --    See also:  <none>
   --
   --  - Name:  Weight_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: Font weight
   --    See also:  <none>
   --
   --  - Name:  Strech_Property
   --    Type:  Pango_Type_Strech
   --    Flags: read-write
   --    Descr: Font strech
   --    See also:  <none>
   --
   --  - Name:  Size_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: Font size
   --    See also:  <none>
   --
   --  - Name:  Size_Points_Property
   --    Type:  Gdouble
   --    Flags: read-write
   --    Descr: Font size in points
   --    See also:  <none>
   --
   --  - Name:  Justification_Property
   --    Type:  Gtk_Type_Justification
   --    Flags: read-write
   --    Descr: Left, right, or center justification
   --    See also:  <none>
   --
   --  - Name:  Language_Property
   --    Type:  String
   --    Flags: read-write
   --    Descr: Language engine code to use for rendering the text
   --    See also:  <none>
   --
   --  - Name:  Left_Margin_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: Width of the left margin in pixels
   --    See also:  <none>
   --
   --  - Name:  Right_Margin_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: Width of the right margin in pixels
   --    See also:  <none>
   --
   --  - Name:  Indent_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: Amount to indent the paragraph, in pixels
   --    See also:  <none>
   --
   --  - Name:  Rise_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: Offset of text above the baseline (below the baseline if
   --           rise is negative)
   --    See also:  <none>
   --
   --  - Name:  Pixels_Above_Lines_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: Pixels of blank space above paragraphs
   --    See also:  <none>
   --
   --  - Name:  Pixels_Below_Lines_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: Pixels of blank space below paragraphs
   --    See also:  <none>
   --
   --  - Name:  Inside_Wrap_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: Pixels of blank space be tween wrapped lines in a paragraph
   --    See also:  <none>
   --
   --  - Name:  Strike_Through_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether to strike through the text
   --    See also:  <none>
   --
   --  - Name:  Underline_Property
   --    Type:  Pango_Type_Underline
   --    Flags: read-write
   --    Descr: Style of underline for this text
   --    See also:  <none>
   --
   --  - Name:  Wrap_Mode_Property
   --    Type:  Gtk_Wrap_Mode
   --    Flags: read-write
   --    Descr: Whether to wrap lines never, at word boundaries, or at
   --           character boundaries
   --    See also:  <none>
   --
   --  - Name:  Tabs_Property
   --    Type:  Pango_Tab_Array
   --    Flags: read-write
   --    Descr: Custom tabs for this text
   --    See also:  <none>
   --
   --  - Name:  Invisible_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this text is hidden
   --    See also:  <none>
   --
   --
   --  The following properties indicate whether a tag modifies some aspect of
   --  text or not. You do not need to modify them explicitely when modifying
   --  one of the above properties, since they will be automatically set to
   --  True when you modify the above.
   --  However, the ones below should be set back to False if you wish to
   --  cancel the effect of a previous modification of a tag.
   --
   --  They all default to False, unless you have modified one of the
   --  properties above.
   --
   --
   --  - Name:  Background_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the background color
   --    See also:  <none>
   --
   --  - Name:  Background_Full_Height_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects background height
   --    See also:  <none>
   --
   --  - Name:  Background_Stipple_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the background stipple
   --    See also:  <none>
   --
   --  - Name:  Foreground_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the foreground color
   --    See also:  <none>
   --
   --  - Name:  Foreground_Stipple_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the foreground stipple
   --    See also:  <none>
   --
   --  - Name:  Editable_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects text editability
   --    See also:  <none>
   --
   --  - Name:  Family_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the font family
   --    See also:  <none>
   --
   --  - Name:  Style_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the font style
   --    See also:  <none>
   --
   --  - Name:  Variant_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the font variant
   --    See also:  <none>
   --
   --  - Name:  Weight_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the font weight
   --    See also:  <none>
   --
   --  - Name:  Stretch_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the font stretch
   --    See also:  <none>
   --
   --  - Name:  Size_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the font size
   --    See also:  <none>
   --
   --  - Name:  Justification_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects paragraph justification
   --    See also:  <none>
   --
   --  - Name:  Language_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the language the text is rendered as
   --    See also:  <none>
   --
   --  - Name:  Left_Margin_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the left margin
   --    See also:  <none>
   --
   --  - Name:  Indent_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects indentation
   --    See also:  <none>
   --
   --  - Name:  Rise_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the rise
   --    See also:  <none>
   --
   --  - Name:  Pixels_Above_Lines_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the number of pixels above lines
   --    See also:  <none>
   --
   --  - Name:  Pixels_Below_Lines_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the number of pixels below lines
   --    See also:  <none>
   --
   --  - Name:  Inside_Wrap_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the number of pixels between wrapped
   --           lines
   --    See also:  <none>
   --
   --  - Name:  Strike_Through_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects strikethrough
   --    See also:  <none>
   --
   --  - Name:  Right_Margin_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects the right margin
   --    See also:  <none>
   --
   --  - Name:  Underline_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects underlining
   --    See also:  <none>
   --
   --  - Name:  Wrap_Mode_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects line wrap mode
   --    See also:  <none>
   --
   --  - Name:  Tabs_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects tabs
   --    See also:  <none>
   --
   --  - Name:  Invisible_Set_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether this tag affects text visibility
   --    See also:  <none>
   --
   --  </properties>

   Background_Full_Height_Property : constant Glib.Properties.Property_Boolean;
   Direction_Property         : constant Gtk.Enums.Property_Gtk_Text_Direction;
   Name_Property               : constant Glib.Properties.Property_String;
   Background_Property         : constant Glib.Properties.Property_String_WO;
   Background_Gdk_Property     : constant Gdk.Color.Property_Gdk_Color;
   Background_Stipple_Property : constant Glib.Properties.Property_C_Proxy;
   Foreground_Property         : constant Glib.Properties.Property_String_WO;
   Foreground_Gdk_Property     : constant Gdk.Color.Property_Gdk_Color;
   Foreground_Stipple_Property : constant Glib.Properties.Property_C_Proxy;
   Editable_Property           : constant Glib.Properties.Property_Boolean;
   Font_Property               : constant Glib.Properties.Property_String;
   Font_Desc_Property          : constant Pango.Font.Property_Font_Description;
   Famility_Property           : constant Glib.Properties.Property_String;
   Style_Property              : constant Pango.Enums.Property_Style;
   Variant_Property            : constant Pango.Enums.Property_Variant;
   Weight_Property             : constant Glib.Properties.Property_Int;
   Stretch_Property            : constant Pango.Enums.Property_Stretch;
   Size_Property               : constant Glib.Properties.Property_Int;
   Size_Points_Property        : constant Glib.Properties.Property_Double;
   Justification_Property      : constant Gtk.Enums.Property_Gtk_Justification;
   Language_Property           : constant Glib.Properties.Property_String;
   Left_Margin_Property        : constant Glib.Properties.Property_Int;
   Right_Margin_Property       : constant Glib.Properties.Property_Int;
   Indent_Property             : constant Glib.Properties.Property_Int;
   Rise_Property               : constant Glib.Properties.Property_Int;
   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int;
   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int;
   Inside_Wrap_Property        : constant Glib.Properties.Property_Int;
   Strike_Through_Property     : constant Glib.Properties.Property_Boolean;
   Underline_Property          : constant Pango.Enums.Property_Underline;
   Wrap_Mode_Property          : constant Gtk.Enums.Property_Gtk_Wrap_Mode;
   --  ???  Tabs_Property            : constant Pango.Types.Property_Tab_Array;
   Invisible_Property          : constant Glib.Properties.Property_Boolean;

   Background_Full_Height_Set_Property : constant
     Glib.Properties.Property_Boolean;
   Background_Set_Property         : constant Glib.Properties.Property_Boolean;
   Background_Stipple_Set_Property : constant Glib.Properties.Property_Boolean;
   Foreground_Set_Property         : constant Glib.Properties.Property_Boolean;
   Foreground_Stipple_Set_Property : constant Glib.Properties.Property_Boolean;
   Editable_Set_Property           : constant Glib.Properties.Property_Boolean;
   Family_Set_Property             : constant Glib.Properties.Property_Boolean;
   Style_Set_Property              : constant Glib.Properties.Property_Boolean;
   Variant_Set_Property            : constant Glib.Properties.Property_Boolean;
   Weight_Set_Property             : constant Glib.Properties.Property_Boolean;
   Stretch_Set_Property            : constant Glib.Properties.Property_Boolean;
   Size_Set_Property               : constant Glib.Properties.Property_Boolean;
   Justification_Set_Property      : constant Glib.Properties.Property_Boolean;
   Language_Set_Property           : constant Glib.Properties.Property_Boolean;
   Left_Margin_Set_Property        : constant Glib.Properties.Property_Boolean;
   Indent_Set_Property             : constant Glib.Properties.Property_Boolean;
   Rise_Set_Property               : constant Glib.Properties.Property_Boolean;
   Pixels_Above_Lines_Set_Property : constant Glib.Properties.Property_Boolean;
   Pixels_Below_Lines_Set_Property : constant Glib.Properties.Property_Boolean;
   Inside_Wrap_Set_Property        : constant Glib.Properties.Property_Boolean;
   Strike_Through_Set_Property     : constant Glib.Properties.Property_Boolean;
   Right_Margin_Set_Property       : constant Glib.Properties.Property_Boolean;
   Underline_Set_Property          : constant Glib.Properties.Property_Boolean;
   Wrap_Mode_Set_Property          : constant Glib.Properties.Property_Boolean;
   Tabs_Set_Property               : constant Glib.Properties.Property_Boolean;
   Invisible_Set_Property          : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "event"
   --    function Handler
   --      (Widget       : access Gtk_Text_Tag_Record'Class;
   --       Event_Object : out G_Object;
   --       Event        : Gdk.Event.Gdk_Event;
   --       Iter         : access Gtk.Text_Iter.Gtk_Text_Iter_Record'Class)
   --       return Gint;
   --
   --  </signals>

private
   type Gtk_Text_Tag_Record is new GObject_Record with null record;

   Background_Full_Height_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("background_full_height");
   Direction_Property         : constant Gtk.Enums.Property_Gtk_Text_Direction
     := Gtk.Enums.Build ("direction");
   Name_Property               : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Background_Property         : constant Glib.Properties.Property_String_WO :=
     Glib.Properties.Build ("background");
   Background_Gdk_Property     : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("background_gdk");
   Background_Stipple_Property : constant Glib.Properties.Property_C_Proxy :=
     Glib.Properties.Build ("background_stipple");
   Foreground_Property         : constant Glib.Properties.Property_String_WO :=
     Glib.Properties.Build ("foreground");
   Foreground_Gdk_Property     : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("foreground_gdk");
   Foreground_Stipple_Property : constant Glib.Properties.Property_C_Proxy :=
     Glib.Properties.Build ("foreground_stipple");
   Editable_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
   Font_Property               : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font");
   Font_Desc_Property       : constant Pango.Font.Property_Font_Description :=
     Pango.Font.Build ("font_desc");
   Famility_Property           : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("family");
   Style_Property              : constant Pango.Enums.Property_Style :=
     Pango.Enums.Build ("style");
   Variant_Property            : constant Pango.Enums.Property_Variant :=
     Pango.Enums.Build ("variant");
   Weight_Property             : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("weight");
   Stretch_Property            : constant Pango.Enums.Property_Stretch :=
     Pango.Enums.Build ("stretch");
   Size_Property               : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("size");
   Size_Points_Property        : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("size_points");
   Justification_Property    : constant Gtk.Enums.Property_Gtk_Justification :=
     Gtk.Enums.Build ("justification");
   Language_Property           : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("language");
   Left_Margin_Property        : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("left_margin");
   Right_Margin_Property       : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("right_margin");
   Indent_Property             : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("indent");
   Rise_Property               : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("rise");
   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels_above_lines");
   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels_below_lines");
   Inside_Wrap_Property        : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("inside_wrap");
   Strike_Through_Property     : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strikethrough");
   Underline_Property          : constant Pango.Enums.Property_Underline :=
     Pango.Enums.Build ("underline");
   Wrap_Mode_Property          : constant Gtk.Enums.Property_Gtk_Wrap_Mode :=
     Gtk.Enums.Build ("wrap_mode");
   --  Tabs_Property               : constant Pango.Types.Property_Tab_Array :=
   --     Pango.Types.Build ("tabs");
   Invisible_Property          : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible");

   Background_Full_Height_Set_Property : constant
     Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background_full_height_set");
   Background_Set_Property       : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background_set");
   Background_Stipple_Set_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("background_stipple_set");
   Foreground_Set_Property       : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("foreground_set");
   Foreground_Stipple_Set_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("foreground_stipple_set");
   Editable_Set_Property         : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable_set");
   Family_Set_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("family_set");
   Style_Set_Property            : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("style_set");
   Variant_Set_Property          : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("variant_set");
   Weight_Set_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("weight_set");
   Stretch_Set_Property          : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("stretch_set");
   Size_Set_Property             : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("size_set");
   Justification_Set_Property    : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("justification_set");
   Language_Set_Property         : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("language_set");
   Left_Margin_Set_Property      : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("left_margin_set");
   Indent_Set_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("indent_set");
   Rise_Set_Property             : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("rise_set");
   Pixels_Above_Lines_Set_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("pixels_above_lines_set");
   Pixels_Below_Lines_Set_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("pixels_below_lines_set");
   Inside_Wrap_Set_Property      : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inside_wrap_set");
   Strike_Through_Set_Property   : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strike_through_set");
   Right_Margin_Set_Property     : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("right_margin_set");
   Underline_Set_Property        : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("underline_set");
   Wrap_Mode_Set_Property        : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap_mode_set");
   Tabs_Set_Property             : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("tabs_set");
   Invisible_Set_Property        : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible_set");

   pragma Import (C, Get_Type, "gtk_text_tag_get_type");
end Gtk.Text_Tag;
