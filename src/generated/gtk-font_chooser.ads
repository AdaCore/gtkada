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

pragma Ada_05;
--  <description>
--  The Gtk.Font_Chooser_Widget.Gtk_Font_Chooser_Widget widget lists the
--  available fonts, styles and sizes, allowing the user to select a font. It
--  is used in the Gtk.Font_Chooser_Dialog.Gtk_Font_Chooser_Dialog widget to
--  provide a dialog box for selecting fonts.
--
--  To set the font which is initially selected, use Gtk.Font_Button.Set_Font
--  or Gtk.Font_Button.Set_Font_Desc.
--
--  To get the selected font use Gtk.Font_Button.Get_Font or
--  Gtk.Font_Button.Get_Font_Desc.
--
--  To change the text which is shown in the preview area, use
--  Gtk.Font_Button.Set_Preview_Text.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Pango.Font;      use Pango.Font;

package Gtk.Font_Chooser is

   type Gtk_Font_Chooser is new Glib.Types.GType_Interface;

   type Gtk_Font_Filter_Func is access function
     (Family : not null access Pango.Font.Pango_Font_Family_Record'Class;
      Face   : not null access Pango.Font.Pango_Font_Face_Record'Class)
   return Boolean;
   --  The type of function that is used for deciding what fonts get shown in
   --  a Gtk.Font_Chooser.Gtk_Font_Chooser. See
   --  Gtk.Font_Chooser.Set_Filter_Func.
   --  "family": a Pango.Font_Family.Pango_Font_Family
   --  "face": a Pango.Font_Face.Pango_Font_Face belonging to Family

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_font_chooser_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Font (Self : Gtk_Font_Chooser) return UTF8_String;
   procedure Set_Font (Self : Gtk_Font_Chooser; Fontname : UTF8_String);
   --  Sets the currently-selected font.
   --  Since: gtk+ 3.2
   --  "fontname": a font name like "Helvetica 12" or "Times Bold 18"

   function Get_Font_Desc
      (Self : Gtk_Font_Chooser) return Pango.Font.Pango_Font_Description;
   pragma Import (C, Get_Font_Desc, "gtk_font_chooser_get_font_desc");
   procedure Set_Font_Desc
      (Self      : Gtk_Font_Chooser;
       Font_Desc : Pango.Font.Pango_Font_Description);
   pragma Import (C, Set_Font_Desc, "gtk_font_chooser_set_font_desc");
   --  Sets the currently-selected font from Font_Desc.
   --  Since: gtk+ 3.2
   --  "font_desc": a Pango.Font_Description.Pango_Font_Description

   function Get_Font_Face
      (Self : Gtk_Font_Chooser) return Pango.Font.Pango_Font_Face;
   --  Gets the Pango.Font_Face.Pango_Font_Face representing the selected font
   --  group details (i.e. family, slant, weight, width, etc).
   --  If the selected font is not installed, returns null.
   --  selected font group details, or null. The returned object is owned by
   --  Fontchooser and must not be modified or freed.
   --  Since: gtk+ 3.2

   function Get_Font_Family
      (Self : Gtk_Font_Chooser) return Pango.Font.Pango_Font_Family;
   --  Gets the Pango.Font_Family.Pango_Font_Family representing the selected
   --  font family. Font families are a collection of font faces.
   --  If the selected font is not installed, returns null.
   --  selected font family, or null. The returned object is owned by
   --  Fontchooser and must not be modified or freed.
   --  Since: gtk+ 3.2

   function Get_Font_Size (Self : Gtk_Font_Chooser) return Gint;
   pragma Import (C, Get_Font_Size, "gtk_font_chooser_get_font_size");
   --  The selected font size.
   --  or -1 if no font size is selected.
   --  Since: gtk+ 3.2

   function Get_Preview_Text (Self : Gtk_Font_Chooser) return UTF8_String;
   procedure Set_Preview_Text (Self : Gtk_Font_Chooser; Text : UTF8_String);
   --  Sets the text displayed in the preview area. The Text is used to show
   --  how the selected font looks.
   --  Since: gtk+ 3.2
   --  "text": the text to display in the preview area

   function Get_Show_Preview_Entry (Self : Gtk_Font_Chooser) return Boolean;
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
        (Family : not null access Pango.Font.Pango_Font_Family_Record'Class;
         Face   : not null access Pango.Font.Pango_Font_Face_Record'Class;
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
   --
   --  Name: Font_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The font description as a string, e.g. "Sans Italic 12".
   --
   --  Name: Font_Desc_Property
   --  Type: Pango.Font.Pango_Font_Description
   --  Flags: read-write
   --  The font description as a
   --  Pango.Font_Description.Pango_Font_Description.
   --
   --  Name: Preview_Text_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The string with which to preview the font.
   --
   --  Name: Show_Preview_Entry_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether to show an entry to change the preview text.

   Font_Property : constant Glib.Properties.Property_String;
   Font_Desc_Property : constant Pango.Font.Property_Font_Description;
   Preview_Text_Property : constant Glib.Properties.Property_String;
   Show_Preview_Entry_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "font-activated"
   --     procedure Handler
   --       (Self   : access Gtk_Font_Chooser;
   --        Object : UTF8_String);

   Signal_Font_Activated : constant Glib.Signal_Name := "font-activated";

private
   Font_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font");
   Font_Desc_Property : constant Pango.Font.Property_Font_Description :=
     Pango.Font.Build ("font-desc");
   Preview_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("preview-text");
   Show_Preview_Entry_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-preview-entry");
end Gtk.Font_Chooser;
