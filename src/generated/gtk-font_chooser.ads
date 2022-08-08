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
--  Gtk.Font_Chooser.Gtk_Font_Chooser is an interface that can be implemented
--  by widgets displaying the list of fonts. In GTK+, the main objects that
--  implement this interface are
--  Gtk.Font_Chooser_Widget.Gtk_Font_Chooser_Widget,
--  Gtk.Font_Chooser_Dialog.Gtk_Font_Chooser_Dialog and
--  Gtk.Font_Button.Gtk_Font_Button. The GtkFontChooser interface has been
--  introducted in GTK+ 3.2.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Pango.Font;              use Pango.Font;
with Pango.Font_Face;         use Pango.Font_Face;
with Pango.Font_Family;       use Pango.Font_Family;
with Pango.Font_Map;          use Pango.Font_Map;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;            use Gtkada.Types;
pragma Warnings(On);

package Gtk.Font_Chooser is

   type Gtk_Font_Chooser is new Glib.Types.GType_Interface;
   Null_Gtk_Font_Chooser : constant Gtk_Font_Chooser;

   type Gtk_Font_Chooser_Level is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Font_Chooser_Level);
   --  This enumeration specifies the granularity of font selection that is
   --  desired in a font chooser.
   --
   --  This enumeration may be extended in the future; applications should
   --  ignore unknown values.

   Font_Chooser_Level_Family : constant Gtk_Font_Chooser_Level := 0;
   Font_Chooser_Level_Style : constant Gtk_Font_Chooser_Level := 1;
   Font_Chooser_Level_Size : constant Gtk_Font_Chooser_Level := 2;
   Font_Chooser_Level_Variations : constant Gtk_Font_Chooser_Level := 4;
   Font_Chooser_Level_Features : constant Gtk_Font_Chooser_Level := 8;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Font_Filter_Func is access function
     (Family : not null access Pango.Font_Family.Pango_Font_Family_Record'Class;
      Face   : not null access Pango.Font_Face.Pango_Font_Face_Record'Class)
   return Boolean;
   --  The type of function that is used for deciding what fonts get shown in
   --  a Gtk.Font_Chooser.Gtk_Font_Chooser. See
   --  Gtk.Font_Chooser.Set_Filter_Func.
   --  "family": a Pango.Font_Family.Pango_Font_Family
   --  "face": a Pango.Font_Face.Pango_Font_Face belonging to Family

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Font_Chooser_Level_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Font_Chooser_Level);
   type Property_Gtk_Font_Chooser_Level is new Gtk_Font_Chooser_Level_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_font_chooser_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Font (Self : Gtk_Font_Chooser) return UTF8_String;
   --  Gets the currently-selected font name.
   --  Note that this can be a different string than what you set with
   --  Gtk.Font_Chooser.Set_Font, as the font chooser widget may normalize font
   --  names and thus return a string with a different structure. For example,
   --  "Helvetica Italic Bold 12" could be normalized to "Helvetica Bold Italic
   --  12".
   --  Use Pango.Font.Equal if you want to compare two font descriptions.
   --  Since: gtk+ 3.2

   procedure Set_Font (Self : Gtk_Font_Chooser; Fontname : UTF8_String);
   --  Sets the currently-selected font.
   --  Since: gtk+ 3.2
   --  "fontname": a font name like "Helvetica 12" or "Times Bold 18"

   function Get_Font_Desc
      (Self : Gtk_Font_Chooser) return Pango.Font.Pango_Font_Description;
   pragma Import (C, Get_Font_Desc, "gtk_font_chooser_get_font_desc");
   --  Gets the currently-selected font.
   --  Note that this can be a different string than what you set with
   --  Gtk.Font_Chooser.Set_Font, as the font chooser widget may normalize font
   --  names and thus return a string with a different structure. For example,
   --  "Helvetica Italic Bold 12" could be normalized to "Helvetica Bold Italic
   --  12".
   --  Use Pango.Font.Equal if you want to compare two font descriptions.
   --  Since: gtk+ 3.2

   procedure Set_Font_Desc
      (Self      : Gtk_Font_Chooser;
       Font_Desc : Pango.Font.Pango_Font_Description);
   pragma Import (C, Set_Font_Desc, "gtk_font_chooser_set_font_desc");
   --  Sets the currently-selected font from Font_Desc.
   --  Since: gtk+ 3.2
   --  "font_desc": a Pango.Font.Pango_Font_Description

   function Get_Font_Face
      (Self : Gtk_Font_Chooser) return Pango.Font_Face.Pango_Font_Face;
   --  Gets the Pango.Font_Face.Pango_Font_Face representing the selected font
   --  group details (i.e. family, slant, weight, width, etc).
   --  If the selected font is not installed, returns null.
   --  Since: gtk+ 3.2

   function Get_Font_Family
      (Self : Gtk_Font_Chooser) return Pango.Font_Family.Pango_Font_Family;
   --  Gets the Pango.Font_Family.Pango_Font_Family representing the selected
   --  font family. Font families are a collection of font faces.
   --  If the selected font is not installed, returns null.
   --  Since: gtk+ 3.2

   function Get_Font_Features (Self : Gtk_Font_Chooser) return UTF8_String;
   --  Gets the currently-selected font features.
   --  Since: gtk+ 3.24

   function Get_Font_Map
      (Self : Gtk_Font_Chooser) return Pango.Font_Map.Pango_Font_Map;
   --  Gets the custom font map of this font chooser widget, or null if it
   --  does not have one.
   --  Since: gtk+ 3.18

   procedure Set_Font_Map
      (Self    : Gtk_Font_Chooser;
       Fontmap : access Pango.Font_Map.Pango_Font_Map_Record'Class);
   --  Sets a custom font map to use for this font chooser widget. A custom
   --  font map can be used to present application-specific fonts instead of or
   --  in addition to the normal system fonts.
   --  |[<!-- language="C" --> FcConfig *config; PangoFontMap *fontmap;
   --  config = FcInitLoadConfigAndFonts (); FcConfigAppFontAddFile (config,
   --  my_app_font_file);
   --  fontmap = pango_cairo_font_map_new_for_font_type (CAIRO_FONT_TYPE_FT);
   --  pango_fc_font_map_set_config (PANGO_FC_FONT_MAP (fontmap), config);
   --  gtk_font_chooser_set_font_map (font_chooser, fontmap); ]|
   --  Note that other GTK+ widgets will only be able to use the
   --  application-specific font if it is present in the font map they use:
   --  |[ context = gtk_widget_get_pango_context (label);
   --  pango_context_set_font_map (context, fontmap); ]|
   --  Since: gtk+ 3.18
   --  "fontmap": a Pango.Font_Map.Pango_Font_Map

   function Get_Font_Size (Self : Gtk_Font_Chooser) return Glib.Gint;
   pragma Import (C, Get_Font_Size, "gtk_font_chooser_get_font_size");
   --  The selected font size.
   --  Since: gtk+ 3.2

   function Get_Language (Self : Gtk_Font_Chooser) return UTF8_String;
   --  Gets the language that is used for font features.
   --  Since: gtk+ 3.24

   procedure Set_Language (Self : Gtk_Font_Chooser; Language : UTF8_String);
   --  Sets the language to use for font features.
   --  Since: gtk+ 3.24
   --  "language": a language

   function Get_Level
      (Self : Gtk_Font_Chooser) return Gtk_Font_Chooser_Level;
   pragma Import (C, Get_Level, "gtk_font_chooser_get_level");
   --  Returns the current level of granularity for selecting fonts.
   --  Since: gtk+ 3.24

   procedure Set_Level
      (Self  : Gtk_Font_Chooser;
       Level : Gtk_Font_Chooser_Level);
   pragma Import (C, Set_Level, "gtk_font_chooser_set_level");
   --  Sets the desired level of granularity for selecting fonts.
   --  Since: gtk+ 3.24
   --  "level": the desired level of granularity

   function Get_Preview_Text (Self : Gtk_Font_Chooser) return UTF8_String;
   --  Gets the text displayed in the preview area.
   --  Since: gtk+ 3.2

   procedure Set_Preview_Text (Self : Gtk_Font_Chooser; Text : UTF8_String);
   --  Sets the text displayed in the preview area. The Text is used to show
   --  how the selected font looks.
   --  Since: gtk+ 3.2
   --  "text": the text to display in the preview area

   function Get_Show_Preview_Entry (Self : Gtk_Font_Chooser) return Boolean;
   --  Returns whether the preview entry is shown or not.
   --  Since: gtk+ 3.2

   procedure Set_Show_Preview_Entry
      (Self               : Gtk_Font_Chooser;
       Show_Preview_Entry : Boolean);
   --  Shows or hides the editable preview entry.
   --  Since: gtk+ 3.2
   --  "show_preview_entry": whether to show the editable preview entry or not

   procedure Set_Filter_Func
      (Self   : Gtk_Font_Chooser;
       Filter : Gtk_Font_Filter_Func);
   --  Adds a filter function that decides which fonts to display in the font
   --  chooser.
   --  Since: gtk+ 3.2
   --  "filter": a Gtk_Font_Filter_Func, or null

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Filter_Func_User_Data is

      type Gtk_Font_Filter_Func is access function
        (Family : not null access Pango.Font_Family.Pango_Font_Family_Record'Class;
         Face   : not null access Pango.Font_Face.Pango_Font_Face_Record'Class;
         Data   : User_Data_Type) return Boolean;
      --  The type of function that is used for deciding what fonts get shown in
      --  a Gtk.Font_Chooser.Gtk_Font_Chooser. See
      --  Gtk.Font_Chooser.Set_Filter_Func.
      --  "family": a Pango.Font_Family.Pango_Font_Family
      --  "face": a Pango.Font_Face.Pango_Font_Face belonging to Family
      --  "data": user data passed to Gtk.Font_Chooser.Set_Filter_Func

      procedure Set_Filter_Func
         (Self      : Gtk.Font_Chooser.Gtk_Font_Chooser;
          Filter    : Gtk_Font_Filter_Func;
          User_Data : User_Data_Type);
      --  Adds a filter function that decides which fonts to display in the
      --  font chooser.
      --  Since: gtk+ 3.2
      --  "filter": a Gtk_Font_Filter_Func, or null
      --  "user_data": data to pass to Filter

   end Set_Filter_Func_User_Data;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Font_Property : constant Glib.Properties.Property_String;
   --  The font description as a string, e.g. "Sans Italic 12".

   Font_Desc_Property : constant Pango.Font.Property_Font_Description;
   --  Type: Pango.Font.Pango_Font_Description
   --  The font description as a Pango.Font.Pango_Font_Description.

   Font_Features_Property : constant Glib.Properties.Property_String;
   --  The selected font features, in a format that is compatible with CSS and
   --  with Pango attributes.

   Language_Property : constant Glib.Properties.Property_String;
   --  The language for which the
   --  Gtk.Font_Chooser.Gtk_Font_Chooser:font-features were selected, in a
   --  format that is compatible with CSS and with Pango attributes.

   Level_Property : constant Gtk.Font_Chooser.Property_Gtk_Font_Chooser_Level;
   --  Type: Gtk_Font_Chooser_Level
   --  The level of granularity to offer for selecting fonts.

   Preview_Text_Property : constant Glib.Properties.Property_String;
   --  The string with which to preview the font.

   Show_Preview_Entry_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to show an entry to change the preview text.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Font_Chooser_UTF8_String_Void is not null access procedure
     (Self     : Gtk_Font_Chooser;
      Fontname : UTF8_String);

   type Cb_GObject_UTF8_String_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Fontname : UTF8_String);

   Signal_Font_Activated : constant Glib.Signal_Name := "font-activated";
   procedure On_Font_Activated
      (Self  : Gtk_Font_Chooser;
       Call  : Cb_Gtk_Font_Chooser_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Font_Activated
      (Self  : Gtk_Font_Chooser;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a font is activated. This usually happens when the user
   --  double clicks an item, or an item is selected and the user presses one
   --  of the keys Space, Shift+Space, Return or Enter.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Font_Chooser"

   function "+" (W : Gtk_Font_Chooser) return Gtk_Font_Chooser;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Font_Activated is access procedure
     (Self     : Gtk_Font_Chooser;
      Fontname : Gtkada.Types.Chars_Ptr);
   pragma Convention (C, Virtual_Font_Activated);

   type Virtual_Get_Font_Face is access function (Self : Gtk_Font_Chooser) return System.Address;
   pragma Convention (C, Virtual_Get_Font_Face);
   --  Gets the Pango.Font_Face.Pango_Font_Face representing the selected font
   --  group details (i.e. family, slant, weight, width, etc).
   --  If the selected font is not installed, returns null.
   --  Since: gtk+ 3.2

   type Virtual_Get_Font_Family is access function (Self : Gtk_Font_Chooser) return System.Address;
   pragma Convention (C, Virtual_Get_Font_Family);
   --  Gets the Pango.Font_Family.Pango_Font_Family representing the selected
   --  font family. Font families are a collection of font faces.
   --  If the selected font is not installed, returns null.
   --  Since: gtk+ 3.2

   type Virtual_Get_Font_Map is access function (Self : Gtk_Font_Chooser) return System.Address;
   pragma Convention (C, Virtual_Get_Font_Map);
   --  Gets the custom font map of this font chooser widget, or null if it
   --  does not have one.
   --  Since: gtk+ 3.18

   type Virtual_Get_Font_Size is access function (Self : Gtk_Font_Chooser) return Glib.Gint;
   pragma Convention (C, Virtual_Get_Font_Size);
   --  The selected font size.
   --  Since: gtk+ 3.2

   type Virtual_Set_Filter_Func is access procedure
     (Self      : Gtk_Font_Chooser;
      Filter    : System.Address;
      User_Data : System.Address;
      Destroy   : Glib.G_Destroy_Notify_Address);
   pragma Convention (C, Virtual_Set_Filter_Func);
   --  Adds a filter function that decides which fonts to display in the font
   --  chooser.
   --  Since: gtk+ 3.2
   --  "filter": a Gtk_Font_Filter_Func, or null
   --  "user_data": data to pass to Filter
   --  "destroy": function to call to free Data when it is no longer needed

   type Virtual_Set_Font_Map is access procedure (Self : Gtk_Font_Chooser; Fontmap : System.Address);
   pragma Convention (C, Virtual_Set_Font_Map);
   --  Sets a custom font map to use for this font chooser widget. A custom
   --  font map can be used to present application-specific fonts instead of or
   --  in addition to the normal system fonts.
   --  |[<!-- language="C" --> FcConfig *config; PangoFontMap *fontmap;
   --  config = FcInitLoadConfigAndFonts (); FcConfigAppFontAddFile (config,
   --  my_app_font_file);
   --  fontmap = pango_cairo_font_map_new_for_font_type (CAIRO_FONT_TYPE_FT);
   --  pango_fc_font_map_set_config (PANGO_FC_FONT_MAP (fontmap), config);
   --  gtk_font_chooser_set_font_map (font_chooser, fontmap); ]|
   --  Note that other GTK+ widgets will only be able to use the
   --  application-specific font if it is present in the font map they use:
   --  |[ context = gtk_widget_get_pango_context (label);
   --  pango_context_set_font_map (context, fontmap); ]|
   --  Since: gtk+ 3.18
   --  "fontmap": a Pango.Font_Map.Pango_Font_Map

   subtype Font_Chooser_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Font_Activated
     (Self    : Font_Chooser_Interface_Descr;
      Handler : Virtual_Font_Activated);
   pragma Import (C, Set_Font_Activated, "gtkada_Font_Chooser_set_font_activated");

   procedure Set_Get_Font_Face
     (Self    : Font_Chooser_Interface_Descr;
      Handler : Virtual_Get_Font_Face);
   pragma Import (C, Set_Get_Font_Face, "gtkada_Font_Chooser_set_get_font_face");

   procedure Set_Get_Font_Family
     (Self    : Font_Chooser_Interface_Descr;
      Handler : Virtual_Get_Font_Family);
   pragma Import (C, Set_Get_Font_Family, "gtkada_Font_Chooser_set_get_font_family");

   procedure Set_Get_Font_Map
     (Self    : Font_Chooser_Interface_Descr;
      Handler : Virtual_Get_Font_Map);
   pragma Import (C, Set_Get_Font_Map, "gtkada_Font_Chooser_set_get_font_map");

   procedure Set_Get_Font_Size
     (Self    : Font_Chooser_Interface_Descr;
      Handler : Virtual_Get_Font_Size);
   pragma Import (C, Set_Get_Font_Size, "gtkada_Font_Chooser_set_get_font_size");

   procedure Set_Set_Filter_Func
     (Self    : Font_Chooser_Interface_Descr;
      Handler : Virtual_Set_Filter_Func);
   pragma Import (C, Set_Set_Filter_Func, "gtkada_Font_Chooser_set_set_filter_func");

   procedure Set_Set_Font_Map
     (Self    : Font_Chooser_Interface_Descr;
      Handler : Virtual_Set_Font_Map);
   pragma Import (C, Set_Set_Font_Map, "gtkada_Font_Chooser_set_set_font_map");
   --  See Glib.Object.Add_Interface

private
   Show_Preview_Entry_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-preview-entry");
   Preview_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("preview-text");
   Level_Property : constant Gtk.Font_Chooser.Property_Gtk_Font_Chooser_Level :=
     Gtk.Font_Chooser.Build ("level");
   Language_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("language");
   Font_Features_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font-features");
   Font_Desc_Property : constant Pango.Font.Property_Font_Description :=
     Pango.Font.Build ("font-desc");
   Font_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font");

Null_Gtk_Font_Chooser : constant Gtk_Font_Chooser :=
   Gtk_Font_Chooser (Glib.Types.Null_Interface);
end Gtk.Font_Chooser;
