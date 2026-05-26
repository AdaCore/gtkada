------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Opens a font chooser dialog to select a font.
--
--  <picture> <source srcset="font-button-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example
--  GtkFontDialogButton" src="font-button.png"> </picture>
--  It is suitable widget for selecting a font in a preference dialog.
--
--  # CSS nodes
--
--  ``` fontbutton ╰── button.font ╰── [content] ```
--
--  `GtkFontDialogButton` has a single CSS node with name fontbutton which
--  contains a button node with the .font style class.
--
--  <group>Buttons and Toggles</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Font_Dialog;         use Gtk.Font_Dialog;
with Gtk.Widget;              use Gtk.Widget;
with Pango.Font;              use Pango.Font;
with Pango.Language;          use Pango.Language;

package Gtk.Font_Dialog_Button is

   type Gtk_Font_Dialog_Button_Record is new Gtk_Widget_Record with null record;
   type Gtk_Font_Dialog_Button is access all Gtk_Font_Dialog_Button_Record'Class;

   type Gtk_Font_Level is (
      Font_Level_Family,
      Font_Level_Face,
      Font_Level_Font,
      Font_Level_Features);
   pragma Convention (C, Gtk_Font_Level);
   --  The level of granularity for the font selection.
   --
   --  Depending on this value, the `PangoFontDescription` that is returned by
   --  [methodGtk.FontDialogButton.get_font_desc] will have more or less fields
   --  set.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Font_Level_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Font_Level);
   type Property_Gtk_Font_Level is new Gtk_Font_Level_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Font_Dialog_Button;
       Dialog : access Gtk.Font_Dialog.Gtk_Font_Dialog_Record'Class);
   procedure Initialize
      (Self   : not null access Gtk_Font_Dialog_Button_Record'Class;
       Dialog : access Gtk.Font_Dialog.Gtk_Font_Dialog_Record'Class);
   --  Creates a new `GtkFontDialogButton` with the given `GtkFontDialog`.
   --  You can pass `NULL` to this function and set a `GtkFontDialog` later.
   --  The button will be insensitive until that happens.
   --  Since: gtk+ 4.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Dialog the `GtkFontDialog` to use

   function Gtk_Font_Dialog_Button_New
      (Dialog : access Gtk.Font_Dialog.Gtk_Font_Dialog_Record'Class)
       return Gtk_Font_Dialog_Button;
   --  Creates a new `GtkFontDialogButton` with the given `GtkFontDialog`.
   --  You can pass `NULL` to this function and set a `GtkFontDialog` later.
   --  The button will be insensitive until that happens.
   --  Since: gtk+ 4.10
   --  @param Dialog the `GtkFontDialog` to use

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_font_dialog_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Dialog
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Gtk.Font_Dialog.Gtk_Font_Dialog;
   --  Returns the `GtkFontDialog` of Self.
   --  Since: gtk+ 4.10
   --  @return the `GtkFontDialog`

   procedure Set_Dialog
      (Self   : not null access Gtk_Font_Dialog_Button_Record;
       Dialog : not null access Gtk.Font_Dialog.Gtk_Font_Dialog_Record'Class);
   --  Sets a `GtkFontDialog` object to use for creating the font chooser
   --  dialog that is presented when the user clicks the button.
   --  Since: gtk+ 4.10
   --  @param Dialog the new `GtkFontDialog`

   function Get_Font_Desc
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Pango.Font.Pango_Font_Description;
   --  Returns the font of the button.
   --  This function is what should be used to obtain the font that was chosen
   --  by the user. To get informed about changes, listen to
   --  "notify::font-desc".
   --  Since: gtk+ 4.10
   --  @return the font

   procedure Set_Font_Desc
      (Self      : not null access Gtk_Font_Dialog_Button_Record;
       Font_Desc : Pango.Font.Pango_Font_Description);
   --  Sets the font of the button.
   --  Since: gtk+ 4.10
   --  @param Font_Desc the new font

   function Get_Font_Features
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return UTF8_String;
   --  Returns the font features of the button.
   --  This function is what should be used to obtain the font features that
   --  were chosen by the user. To get informed about changes, listen to
   --  "notify::font-features".
   --  Note that the button will only let users choose font features if
   --  [propertyGtk.FontDialogButton:level] is set to
   --  `GTK_FONT_LEVEL_FEATURES`.
   --  Since: gtk+ 4.10
   --  @return the font features

   procedure Set_Font_Features
      (Self          : not null access Gtk_Font_Dialog_Button_Record;
       Font_Features : UTF8_String := "");
   --  Sets the font features of the button.
   --  Since: gtk+ 4.10
   --  @param Font_Features the font features

   function Get_Language
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Pango.Language.Pango_Language;
   --  Returns the language that is used for font features.
   --  Since: gtk+ 4.10
   --  @return the language

   procedure Set_Language
      (Self     : not null access Gtk_Font_Dialog_Button_Record;
       Language : Pango.Language.Pango_Language);
   --  Sets the language to use for font features.
   --  Since: gtk+ 4.10
   --  @param Language the new language

   function Get_Level
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Gtk_Font_Level;
   --  Returns the level of detail at which this dialog lets the user select
   --  fonts.
   --  Since: gtk+ 4.10
   --  @return the level of detail

   procedure Set_Level
      (Self  : not null access Gtk_Font_Dialog_Button_Record;
       Level : Gtk_Font_Level);
   --  Sets the level of detail at which this dialog lets the user select
   --  fonts.
   --  Since: gtk+ 4.10
   --  @param Level the level of detail

   function Get_Use_Font
      (Self : not null access Gtk_Font_Dialog_Button_Record) return Boolean;
   --  Returns whether the selected font is used in the label.
   --  Since: gtk+ 4.10
   --  @return whether the selected font is used in the label

   procedure Set_Use_Font
      (Self     : not null access Gtk_Font_Dialog_Button_Record;
       Use_Font : Boolean);
   --  If Use_Font is `TRUE`, the font name will be written using the selected
   --  font.
   --  Since: gtk+ 4.10
   --  @param Use_Font If `TRUE`, font name will be written using the chosen
   --  font

   function Get_Use_Size
      (Self : not null access Gtk_Font_Dialog_Button_Record) return Boolean;
   --  Returns whether the selected font size is used in the label.
   --  Since: gtk+ 4.10
   --  @return whether the selected font size is used in the label

   procedure Set_Use_Size
      (Self     : not null access Gtk_Font_Dialog_Button_Record;
       Use_Size : Boolean);
   --  If Use_Size is `TRUE`, the font name will be written using the selected
   --  font size.
   --  Since: gtk+ 4.10
   --  @param Use_Size If `TRUE`, font name will be written using the chosen
   --  font size

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Font_Dialog_Button_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Font_Dialog_Button_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Font_Dialog_Button_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Font_Dialog_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Font_Dialog_Button_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Font_Dialog_Button_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Font_Dialog_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Font_Dialog_Button_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Font_Dialog_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Dialog_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Font_Dialog.Gtk_Font_Dialog
   --  The `GtkFontDialog` that contains parameters for the font chooser
   --  dialog.

   Font_Desc_Property : constant Pango.Font.Property_Font_Description;
   --  Type: Pango.Font.Pango_Font_Description
   --  The selected font.
   --
   --  This property can be set to give the button its initial font, and it
   --  will be updated to reflect the users choice in the font chooser dialog.
   --
   --  Listen to `notify::font-desc` to get informed about changes to the
   --  buttons font.

   Font_Features_Property : constant Glib.Properties.Property_String;
   --  The selected font features.
   --
   --  This property will be updated to reflect the users choice in the font
   --  chooser dialog.
   --
   --  Listen to `notify::font-features` to get informed about changes to the
   --  buttons font features.

   Language_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Pango.Language
   --  The selected language for font features.
   --
   --  This property will be updated to reflect the users choice in the font
   --  chooser dialog.
   --
   --  Listen to `notify::language` to get informed about changes to the
   --  buttons language.

   Level_Property : constant Gtk.Font_Dialog_Button.Property_Gtk_Font_Level;
   --  Type: Gtk_Font_Level
   --  The level of detail for the font chooser dialog.

   Use_Font_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the buttons label will be drawn in the selected font.

   Use_Size_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the buttons label will use the selected font size.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Font_Dialog_Button_Void is not null access procedure
     (Self : access Gtk_Font_Dialog_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Font_Dialog_Button_Record;
       Call  : Cb_Gtk_Font_Dialog_Button_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Font_Dialog_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the font dialog button is activated.
   --
   --  The `::activate` signal on `GtkFontDialogButton` is an action signal
   --  and emitting it causes the button to pop up its dialog.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Font_Dialog_Button_Record, Gtk_Font_Dialog_Button);
   function "+"
     (Widget : access Gtk_Font_Dialog_Button_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Font_Dialog_Button
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Font_Dialog_Button_Record, Gtk_Font_Dialog_Button);
   function "+"
     (Widget : access Gtk_Font_Dialog_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Font_Dialog_Button
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Font_Dialog_Button_Record, Gtk_Font_Dialog_Button);
   function "+"
     (Widget : access Gtk_Font_Dialog_Button_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Font_Dialog_Button
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Use_Size_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-size");
   Use_Font_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-font");
   Level_Property : constant Gtk.Font_Dialog_Button.Property_Gtk_Font_Level :=
     Gtk.Font_Dialog_Button.Build ("level");
   Language_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("language");
   Font_Features_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font-features");
   Font_Desc_Property : constant Pango.Font.Property_Font_Description :=
     Pango.Font.Build ("font-desc");
   Dialog_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("dialog");
end Gtk.Font_Dialog_Button;
