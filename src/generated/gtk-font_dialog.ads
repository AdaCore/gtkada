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

--  Asynchronous API to present a font chooser dialog.
--
--  `GtkFontDialog` collects the arguments that are needed to present the
--  dialog to the user, such as a title for the dialog and whether it should be
--  modal.
--
--  The dialog is shown with the [methodGtk.FontDialog.choose_font] function
--  or its variants.
--
--  See [classGtk.FontDialogButton] for a convenient control that uses
--  `GtkFontDialog` and presents the results.
--
--  <group>Dialogs</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Cancellable;  use Glib.Cancellable;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Gtk.Window;        use Gtk.Window;
with Pango.Font;        use Pango.Font;
with Pango.Font_Face;   use Pango.Font_Face;
with Pango.Font_Family; use Pango.Font_Family;
with Pango.Font_Map;    use Pango.Font_Map;
with Pango.Language;    use Pango.Language;

package Gtk.Font_Dialog is

   type Gtk_Font_Dialog_Record is new GObject_Record with null record;
   type Gtk_Font_Dialog is access all Gtk_Font_Dialog_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gasync_Ready_Callback is access procedure
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   --  Type definition for a function that will be called back when an
   --  asynchronous operation within GIO has been completed.
   --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
   --  invoked in a later iteration of the [thread-default main
   --  context][g-main-context-push-thread-default] where the Gtask.Gtask was
   --  created. All other users of Gasync_Ready_Callback must likewise call it
   --  asynchronously in a later iteration of the main context.
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Font_Dialog);
   procedure Initialize
      (Self : not null access Gtk_Font_Dialog_Record'Class);
   --  Creates a new `GtkFontDialog` object.
   --  Since: gtk+ 4.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Font_Dialog_New return Gtk_Font_Dialog;
   --  Creates a new `GtkFontDialog` object.
   --  Since: gtk+ 4.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_font_dialog_get_type");

   -------------
   -- Methods --
   -------------

   procedure Choose_Face
      (Self          : not null access Gtk_Font_Dialog_Record;
       Parent        : access Gtk.Window.Gtk_Window_Record'Class;
       Initial_Value : access Pango.Font_Face.Pango_Font_Face_Record'Class;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback      : Gasync_Ready_Callback);
   --  Presents a font chooser dialog to the user.
   --  The font chooser dialog will be set up for selecting a font face.
   --  A font face represents a font family and style, but no specific font
   --  size.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Initial_Value the initial value
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete

   function Choose_Face_Finish
      (Self   : not null access Gtk_Font_Dialog_Record;
       Result : Glib.G_Async_Result) return Pango.Font_Face.Pango_Font_Face;
   --  Finishes the [methodGtk.FontDialog.choose_face] call.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @return the selected [classPango.FontFace]

   procedure Choose_Family
      (Self          : not null access Gtk_Font_Dialog_Record;
       Parent        : access Gtk.Window.Gtk_Window_Record'Class;
       Initial_Value : access Pango.Font_Family.Pango_Font_Family_Record'Class;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback      : Gasync_Ready_Callback);
   --  Presents a font chooser dialog to the user.
   --  The font chooser dialog will be set up for selecting a font family.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Initial_Value the initial value
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete

   function Choose_Family_Finish
      (Self   : not null access Gtk_Font_Dialog_Record;
       Result : Glib.G_Async_Result)
       return Pango.Font_Family.Pango_Font_Family;
   --  Finishes the [methodGtk.FontDialog.choose_family] call.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @return the selected [classPango.FontFamily]

   procedure Choose_Font
      (Self          : not null access Gtk_Font_Dialog_Record;
       Parent        : access Gtk.Window.Gtk_Window_Record'Class;
       Initial_Value : Pango.Font.Pango_Font_Description;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback      : Gasync_Ready_Callback);
   --  Presents a font chooser dialog to the user.
   --  The font chooser dialog will be set up for selecting a font.
   --  If you want to let the user select font features as well, use
   --  [methodGtk.FontDialog.choose_font_and_features] instead.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Initial_Value the font to select initially
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete

   function Choose_Font_Finish
      (Self   : not null access Gtk_Font_Dialog_Record;
       Result : Glib.G_Async_Result)
       return Pango.Font.Pango_Font_Description;
   --  Finishes the [methodGtk.FontDialog.choose_font] call.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @return a [structPango.FontDescription] describing the selected font

   function Get_Font_Map
      (Self : not null access Gtk_Font_Dialog_Record)
       return Pango.Font_Map.Pango_Font_Map;
   --  Returns the fontmap from which fonts are selected, or `NULL` for the
   --  default fontmap.
   --  Since: gtk+ 4.10
   --  @return the fontmap

   procedure Set_Font_Map
      (Self    : not null access Gtk_Font_Dialog_Record;
       Fontmap : access Pango.Font_Map.Pango_Font_Map_Record'Class);
   --  Sets the fontmap from which fonts are selected.
   --  If Fontmap is `NULL`, the default fontmap is used.
   --  Since: gtk+ 4.10
   --  @param Fontmap the fontmap

   function Get_Language
      (Self : not null access Gtk_Font_Dialog_Record)
       return Pango.Language.Pango_Language;
   --  Returns the language for which font features are applied.
   --  Since: gtk+ 4.10
   --  @return the language for font features

   procedure Set_Language
      (Self     : not null access Gtk_Font_Dialog_Record;
       Language : Pango.Language.Pango_Language);
   --  Sets the language for which font features are applied.
   --  Since: gtk+ 4.10
   --  @param Language the language for font features

   function Get_Modal
      (Self : not null access Gtk_Font_Dialog_Record) return Boolean;
   --  Returns whether the font chooser dialog blocks interaction with the
   --  parent window while it is presented.
   --  Since: gtk+ 4.10
   --  @return true if the font chooser dialog is modal

   procedure Set_Modal
      (Self  : not null access Gtk_Font_Dialog_Record;
       Modal : Boolean);
   --  Sets whether the font chooser dialog blocks interaction with the parent
   --  window while it is presented.
   --  Since: gtk+ 4.10
   --  @param Modal the new value

   function Get_Title
      (Self : not null access Gtk_Font_Dialog_Record) return UTF8_String;
   --  Returns the title that will be shown on the font chooser dialog.
   --  Since: gtk+ 4.10
   --  @return the title

   procedure Set_Title
      (Self  : not null access Gtk_Font_Dialog_Record;
       Title : UTF8_String);
   --  Sets the title that will be shown on the font chooser dialog.
   --  Since: gtk+ 4.10
   --  @param Title the new title

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Filter_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Filter.Gtk_Filter
   --  A filter to restrict what fonts are shown in the font chooser dialog.

   Font_Map_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Pango.Font_Map
   --  A custom font map to select fonts from.
   --
   --  A custom font map can be used to present application-specific fonts
   --  instead of or in addition to the normal system fonts.

   Language_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Pango.Language
   --  The language for which the font features are selected.

   Modal_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the font chooser dialog is modal.

   Title_Property : constant Glib.Properties.Property_String;
   --  A title that may be shown on the font chooser dialog that is presented
   --  by [methodGtk.FontDialog.choose_font].

private
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Modal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("modal");
   Language_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("language");
   Font_Map_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("font-map");
   Filter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("filter");
end Gtk.Font_Dialog;
