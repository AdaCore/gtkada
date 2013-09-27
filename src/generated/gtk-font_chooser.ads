------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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
--  Gtk.Font_Button.Gtk_Font_Button.
--
--  </description>
pragma Ada_2005;


pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Pango.Font;        use Pango.Font;
with Pango.Font_Face;   use Pango.Font_Face;
with Pango.Font_Family; use Pango.Font_Family;

package Gtk.Font_Chooser is

   type Gtk_Font_Chooser is new Glib.Types.GType_Interface;
   Null_Gtk_Font_Chooser : constant Gtk_Font_Chooser;

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

   function Get_Font_Size (Self : Gtk_Font_Chooser) return Gint;
   pragma Import (C, Get_Font_Size, "gtk_font_chooser_get_font_size");
   --  The selected font size.
   --  Since: gtk+ 3.2

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

private
   Show_Preview_Entry_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-preview-entry");
   Preview_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("preview-text");
   Font_Desc_Property : constant Pango.Font.Property_Font_Description :=
     Pango.Font.Build ("font-desc");
   Font_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font");

Null_Gtk_Font_Chooser : constant Gtk_Font_Chooser :=
   Gtk_Font_Chooser (Glib.Types.Null_Interface);
end Gtk.Font_Chooser;
