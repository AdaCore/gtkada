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
--  A Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text renders a given text in
--  its cell, using the font, color and style information provided by its
--  properties. The text will be ellipsized if it is too long and the
--  Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text:ellipsize property allows it.
--
--  If the Gtk.Cell_Renderer.Gtk_Cell_Renderer:mode is
--  Gtk.Cell_Renderer.Cell_Renderer_Mode_Editable, the
--  Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text allows to edit its text using
--  an entry.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Color;         use Gdk.Color;
with Gdk.RGBA;          use Gdk.RGBA;
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Gtk.Cell_Renderer; use Gtk.Cell_Renderer;
with Pango.Enums;       use Pango.Enums;
with Pango.Font;        use Pango.Font;
with Pango.Layout;      use Pango.Layout;

package Gtk.Cell_Renderer_Text is

   type Gtk_Cell_Renderer_Text_Record is new Gtk_Cell_Renderer_Record with null record;
   type Gtk_Cell_Renderer_Text is access all Gtk_Cell_Renderer_Text_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Text);
   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Text_Record'Class);
   --  Creates a new Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text. Adjust how
   --  text is drawn using object properties. Object properties can be set
   --  globally (with g_object_set). Also, with
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column, you can bind a property to a
   --  value in a Gtk.Tree_Model.Gtk_Tree_Model. For example, you can bind the
   --  "text" property on the cell renderer to a string value in the model,
   --  thus rendering a different string in each row of the
   --  Gtk.Tree_View.Gtk_Tree_View
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Cell_Renderer_Text_New return Gtk_Cell_Renderer_Text;
   --  Creates a new Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text. Adjust how
   --  text is drawn using object properties. Object properties can be set
   --  globally (with g_object_set). Also, with
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column, you can bind a property to a
   --  value in a Gtk.Tree_Model.Gtk_Tree_Model. For example, you can bind the
   --  "text" property on the cell renderer to a string value in the model,
   --  thus rendering a different string in each row of the
   --  Gtk.Tree_View.Gtk_Tree_View

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_renderer_text_get_type");

   -------------
   -- Methods --
   -------------

   procedure Set_Fixed_Height_From_Font
      (Self           : not null access Gtk_Cell_Renderer_Text_Record;
       Number_Of_Rows : Glib.Gint);
   --  Sets the height of a renderer to explicitly be determined by the "font"
   --  and "y_pad" property set on it. Further changes in these properties do
   --  not affect the height, so they must be accompanied by a subsequent call
   --  to this function. Using this function is unflexible, and should really
   --  only be used if calculating the size of a cell is too slow (ie, a
   --  massive number of cells displayed). If Number_Of_Rows is -1, then the
   --  fixed height is unset, and the height is determined by the properties
   --  again.
   --  "number_of_rows": Number of rows of text each cell renderer is
   --  allocated, or -1

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Align_Set_Property : constant Glib.Properties.Property_Boolean;

   Alignment_Property : constant Pango.Enums.Property_Alignment;
   --  Type: Pango.Enums.Alignment
   --  Specifies how to align the lines of text with respect to each other.
   --
   --  Note that this property describes how to align the lines of text in
   --  case there are several of them. The "xalign" property of
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer, on the other hand, sets the
   --  horizontal alignment of the whole text.

   Attributes_Property : constant Glib.Properties.Property_Object;
   --  Type: Pango.Attributes.Pango_Attr_List

   Background_Property : constant Glib.Properties.Property_String;
   --  Flags: write

   Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color;
   --  Type: Gdk.Color.Gdk_Color
   --  Background color as a Gdk.Color.Gdk_Color

   Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  Background color as a Gdk.RGBA.Gdk_RGBA

   Background_Set_Property : constant Glib.Properties.Property_Boolean;

   Cancel_On_Focus_Out_Property : constant Glib.Properties.Property_Boolean;
   --  Specifies whether or not to discard the edition when the
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer loses focus.
   --
   --  If this is set to TRUE (the default), then edition will be canceled if
   --  the focus leaves the tree.
   --
   --  Since 3.22

   Editable_Property : constant Glib.Properties.Property_Boolean;

   Editable_Set_Property : constant Glib.Properties.Property_Boolean;

   Ellipsize_Property : constant Pango.Layout.Property_Pango_Ellipsize_Mode;
   --  Type: Pango.Layout.Pango_Ellipsize_Mode
   --  Specifies the preferred place to ellipsize the string, if the cell
   --  renderer does not have enough room to display the entire string. Setting
   --  it to Pango.Layout.Ellipsize_None turns off ellipsizing. See the
   --  wrap-width property for another way of making the text fit into a given
   --  width.

   Ellipsize_Set_Property : constant Glib.Properties.Property_Boolean;

   Family_Property : constant Glib.Properties.Property_String;

   Family_Set_Property : constant Glib.Properties.Property_Boolean;

   Font_Property : constant Glib.Properties.Property_String;

   Font_Desc_Property : constant Pango.Font.Property_Font_Description;
   --  Type: Pango.Font.Pango_Font_Description

   Foreground_Property : constant Glib.Properties.Property_String;
   --  Flags: write

   Foreground_Gdk_Property : constant Gdk.Color.Property_Gdk_Color;
   --  Type: Gdk.Color.Gdk_Color
   --  Foreground color as a Gdk.Color.Gdk_Color

   Foreground_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  Foreground color as a Gdk.RGBA.Gdk_RGBA

   Foreground_Set_Property : constant Glib.Properties.Property_Boolean;

   Language_Property : constant Glib.Properties.Property_String;

   Language_Set_Property : constant Glib.Properties.Property_Boolean;

   Markup_Property : constant Glib.Properties.Property_String;
   --  Flags: write

   Max_Width_Chars_Property : constant Glib.Properties.Property_Int;
   --  The desired maximum width of the cell, in characters. If this property
   --  is set to -1, the width will be calculated automatically.
   --
   --  For cell renderers that ellipsize or wrap text; this property controls
   --  the maximum reported width of the cell. The cell should not receive any
   --  greater allocation unless it is set to expand in its
   --  Gtk.Cell_Layout.Gtk_Cell_Layout and all of the cell's siblings have
   --  received their natural width.

   Placeholder_Text_Property : constant Glib.Properties.Property_String;
   --  The text that will be displayed in the
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer if
   --  Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text:editable is True and the
   --  cell is empty.
   --
   --  Since 3.6

   Rise_Property : constant Glib.Properties.Property_Int;

   Rise_Set_Property : constant Glib.Properties.Property_Boolean;

   Scale_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble

   Scale_Set_Property : constant Glib.Properties.Property_Boolean;

   Single_Paragraph_Mode_Property : constant Glib.Properties.Property_Boolean;

   Size_Property : constant Glib.Properties.Property_Int;

   Size_Points_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble

   Size_Set_Property : constant Glib.Properties.Property_Boolean;

   Stretch_Property : constant Pango.Enums.Property_Stretch;
   --  Type: Pango.Enums.Stretch

   Stretch_Set_Property : constant Glib.Properties.Property_Boolean;

   Strikethrough_Property : constant Glib.Properties.Property_Boolean;

   Strikethrough_Set_Property : constant Glib.Properties.Property_Boolean;

   Style_Property : constant Pango.Enums.Property_Style;
   --  Type: Pango.Enums.Style

   Style_Set_Property : constant Glib.Properties.Property_Boolean;

   Text_Property : constant Glib.Properties.Property_String;

   Underline_Property : constant Pango.Enums.Property_Underline;
   --  Type: Pango.Enums.Underline

   Underline_Set_Property : constant Glib.Properties.Property_Boolean;

   Variant_Property : constant Pango.Enums.Property_Variant;
   --  Type: Pango.Enums.Variant

   Variant_Set_Property : constant Glib.Properties.Property_Boolean;

   Weight_Property : constant Pango.Enums.Property_Weight;
   --  Type: Pango.Enums.Weight

   Weight_Set_Property : constant Glib.Properties.Property_Boolean;

   Width_Chars_Property : constant Glib.Properties.Property_Int;
   --  The desired width of the cell, in characters. If this property is set
   --  to -1, the width will be calculated automatically, otherwise the cell
   --  will request either 3 characters or the property value, whichever is
   --  greater.

   Wrap_Mode_Property : constant Pango.Enums.Property_Wrap_Mode;
   --  Type: Pango.Enums.Wrap_Mode
   --  Specifies how to break the string into multiple lines, if the cell
   --  renderer does not have enough room to display the entire string. This
   --  property has no effect unless the wrap-width property is set.

   Wrap_Width_Property : constant Glib.Properties.Property_Int;
   --  Specifies the minimum width at which the text is wrapped. The wrap-mode
   --  property can be used to influence at what character positions the line
   --  breaks can be placed. Setting wrap-width to -1 turns wrapping off.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Cell_Renderer_Text_UTF8_String_UTF8_String_Void is not null access procedure
     (Self     : access Gtk_Cell_Renderer_Text_Record'Class;
      Path     : UTF8_String;
      New_Text : UTF8_String);

   type Cb_GObject_UTF8_String_UTF8_String_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Path     : UTF8_String;
      New_Text : UTF8_String);

   Signal_Edited : constant Glib.Signal_Name := "edited";
   procedure On_Edited
      (Self  : not null access Gtk_Cell_Renderer_Text_Record;
       Call  : Cb_Gtk_Cell_Renderer_Text_UTF8_String_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Edited
      (Self  : not null access Gtk_Cell_Renderer_Text_Record;
       Call  : Cb_GObject_UTF8_String_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted after Renderer has been edited.
   --
   --  It is the responsibility of the application to update the model and
   --  store New_Text at the position indicated by Path.
   -- 
   --  Callback parameters:
   --    --  "path": the path identifying the edited cell
   --    --  "new_text": the new text

private
   Wrap_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("wrap-width");
   Wrap_Mode_Property : constant Pango.Enums.Property_Wrap_Mode :=
     Pango.Enums.Build ("wrap-mode");
   Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-chars");
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
   Underline_Property : constant Pango.Enums.Property_Underline :=
     Pango.Enums.Build ("underline");
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Style_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("style-set");
   Style_Property : constant Pango.Enums.Property_Style :=
     Pango.Enums.Build ("style");
   Strikethrough_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strikethrough-set");
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
   Single_Paragraph_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("single-paragraph-mode");
   Scale_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("scale-set");
   Scale_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("scale");
   Rise_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("rise-set");
   Rise_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("rise");
   Placeholder_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("placeholder-text");
   Max_Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-width-chars");
   Markup_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("markup");
   Language_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("language-set");
   Language_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("language");
   Foreground_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("foreground-set");
   Foreground_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("foreground-rgba");
   Foreground_Gdk_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("foreground-gdk");
   Foreground_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("foreground");
   Font_Desc_Property : constant Pango.Font.Property_Font_Description :=
     Pango.Font.Build ("font-desc");
   Font_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font");
   Family_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("family-set");
   Family_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("family");
   Ellipsize_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("ellipsize-set");
   Ellipsize_Property : constant Pango.Layout.Property_Pango_Ellipsize_Mode :=
     Pango.Layout.Build ("ellipsize");
   Editable_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable-set");
   Editable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
   Cancel_On_Focus_Out_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("cancel-on-focus-out");
   Background_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background-set");
   Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("background-rgba");
   Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("background-gdk");
   Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("background");
   Attributes_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("attributes");
   Alignment_Property : constant Pango.Enums.Property_Alignment :=
     Pango.Enums.Build ("alignment");
   Align_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("align-set");
end Gtk.Cell_Renderer_Text;
