-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  This widget provides a nice way for the user of your application to
--  select fonts.
--  It first searches on your system for the list of fonts available, and
--  displays a set of boxes to select them based on their name, their
--  weight, their size, etc.
--  This widget is provided in two forms, one widget that can be embedded
--  in any container, a Gtk_Font_Selection, whereas the other one comes
--  directly in its own separate window (to be popped up as a dialog).
--
--  Some filters can be applied to the widget, when you want the user to
--  select only a font only among a specific subset (like bitmap or
--  true-type fonts for instance).
--  There are two kinds of filters: a base filter, set in your application
--  and that the user can not change; a user filter that can be modified
--  interactively by the user.
--
--  </description>
--  <c_version>2.8.17</c_version>

with Glib.Properties;
with Gdk.Font;
with Gtk.Box;
with Gtk.Button;
with Gtk.Dialog;

package Gtk.Font_Selection is

   type Gtk_Font_Selection_Dialog_Record is new
     Gtk.Dialog.Gtk_Dialog_Record with private;
   type Gtk_Font_Selection_Dialog is access all
     Gtk_Font_Selection_Dialog_Record'Class;

   type Gtk_Font_Selection_Record is new
     Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Font_Selection is access all Gtk_Font_Selection_Record'Class;

   ------------------------------
   -- Font_Selection functions --
   ------------------------------

   procedure Gtk_New (Widget : out Gtk_Font_Selection);
   procedure Initialize (Widget : access Gtk_Font_Selection_Record'Class);
   --  Creates or initialises a new font selection widget.
   --  It can be added to any existing container.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Font_Selection.

   function Set_Font_Name
     (Fontsel  : access Gtk_Font_Selection_Record;
      Fontname : String) return Boolean;
   function Get_Font_Name
     (Fontsel : access Gtk_Font_Selection_Record) return String;
   --  Set the name and attributes of the selected font in Fontsel.
   --  Fontname should have the format described in Pango.Font.
   --  Fontself must have been displayed on the screen already

   procedure Set_Preview_Text
     (Fontsel : access Gtk_Font_Selection_Record; Text : UTF8_String);
   function Get_Preview_Text
     (Fontsel : access Gtk_Font_Selection_Record) return UTF8_String;
   --  Set or Get the string used to preview the selected font in the dialog.

   -------------------------------------
   -- Font_Selection_Dialog functions --
   -------------------------------------

   procedure Gtk_New
     (Widget : out Gtk_Font_Selection_Dialog; Title : UTF8_String);
   procedure Initialize
     (Widget : access Gtk_Font_Selection_Dialog_Record'Class;
      Title  : UTF8_String);
   --  Creates or initialises a new dialog to select a font.
   --  The font selection widget has its own window, whose title is chosen
   --  by Title.

   function Dialog_Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Font_Selection_Dialog.

   function Set_Font_Name
     (Fsd      : access Gtk_Font_Selection_Dialog_Record;
      Fontname : String) return Boolean;
   function Get_Font_Name
     (Fsd : access Gtk_Font_Selection_Dialog_Record) return String;
   --  Return the name of the font selected by the user.
   --  It returns an empty string if not font is selected.
   --  The string has the same format as excepted in the Gdk.Font package.
   --  This is also the standard format on X11 systems.

   procedure Set_Preview_Text
     (Fsd : access Gtk_Font_Selection_Dialog_Record; Text : UTF8_String);
   function Get_Preview_Text
     (Fsd : access Gtk_Font_Selection_Dialog_Record) return UTF8_String;
   --  Return the string used to preview the selected font in the dialog.

   function Get_Cancel_Button
     (Fsd : access Gtk_Font_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button;
   --  Return the Id of the cancel button of the dialog.
   --  You can use this to set up a callback on that button.
   --  The callback should close the dialog, and ignore any value that has been
   --  set in it.

   function Get_OK_Button
     (Fsd : access Gtk_Font_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button;
   --  Return the Id of the Ok button.
   --  The callback set on this button should close the dialog if the selected
   --  font is valid, and do whatever if should with it.

   function Get_Apply_Button
     (Fsd : access Gtk_Font_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button;
   --  Return the Id of the Apply button.
   --  The callback on this button should temporarily apply the font, but
   --  should be able to cancel its effect if the Cancel button is selected.

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   function Get_Font
     (Fontsel : access Gtk_Font_Selection_Record) return Gdk.Font.Gdk_Font;
   pragma Obsolescent;  --  Get_Font
   --  Allocate and return the font selected by the user.
   --  This newly created font can be used as is by all the drawing functions
   --  in the Gdk.Drawable package.
   --  If not font has been selected, Gdk.Font.Null_Font is returned.

   function Get_Font
     (Fsd : access Gtk_Font_Selection_Dialog_Record) return Gdk.Font.Gdk_Font;
   pragma Obsolescent;  --  Dialog_Get_Font
   --  Allocate and return the font selected by the user.
   --  This newly created font can be used as is by all the drawing functions
   --  in the Gdk.Drawable package.
   --  If not font has been selected, Gdk.Font.Null_Font is returned.

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Font_Property
   --  Type:  Boxed
   --  Descr: The GdkFont that is currently selected
   --
   --  Name:  Font_Name_Property
   --  Type:  String
   --  Descr: The X string that represents this font
   --
   --  Name:  Preview_Text_Property
   --  Type:  String
   --  Descr: The text to display in order to demonstrate the selected font
   --
   --  </properties>

   --  Font_Property         : constant Glib.Properties.Property_Boxed;
   Font_Name_Property    : constant Glib.Properties.Property_String;
   Preview_Text_Property : constant Glib.Properties.Property_String;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Font_Selection_Dialog_Record is new
     Gtk.Dialog.Gtk_Dialog_Record with null record;

   type Gtk_Font_Selection_Record is new
     Gtk.Box.Gtk_Box_Record with null record;

   --     Font_Property : constant Glib.Properties.Property_Boxed :=
   --       Glib.Properties.Build ("font");
   Font_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font-name");
   Preview_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("preview-text");

   pragma Import (C, Get_Type, "gtk_font_selection_get_type");
   pragma Import (C, Dialog_Get_Type, "gtk_font_selection_dialog_get_type");

end Gtk.Font_Selection;
