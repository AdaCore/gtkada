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
--  This widget provides a dialog for selecting a font. See also
--  Gtk.Font_Selection.
--
--  </description>
--  <screenshot>gtk-fontsel</screenshot>
--  <group>Selectors</group>
--  <testgtk>create_font_selection.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Dialog;    use Gtk.Dialog;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Font_Selection_Dialog is

   type Gtk_Font_Selection_Dialog_Record is new Gtk_Dialog_Record with null record;
   type Gtk_Font_Selection_Dialog is access all Gtk_Font_Selection_Dialog_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Dialog : out Gtk_Font_Selection_Dialog;
       Title  : UTF8_String);
   procedure Initialize
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record'Class;
       Title  : UTF8_String);
   --  Creates a new Gtk.Font_Selection_Dialog.Gtk_Font_Selection_Dialog.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "title": the title of the dialog window

   function Gtk_Font_Selection_Dialog_New
      (Title : UTF8_String) return Gtk_Font_Selection_Dialog;
   --  Creates a new Gtk.Font_Selection_Dialog.Gtk_Font_Selection_Dialog.
   --  "title": the title of the dialog window

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_font_selection_dialog_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Cancel_Button
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Cancel_Button);
   --  Gets the "Cancel" button.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.2, 1

   function Get_Font_Name
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record)
       return UTF8_String;
   pragma Obsolescent (Get_Font_Name);
   --  Gets the currently-selected font name.
   --  Note that this can be a different string than what you set with
   --  Gtk.Font_Selection_Dialog.Set_Font_Name, as the font selection widget
   --  may normalize font names and thus return a string with a different
   --  structure. For example, "Helvetica Italic Bold 12" could be normalized
   --  to "Helvetica Bold Italic 12". Use Pango.Font.Equal if you want to
   --  compare two font descriptions.
   --  Deprecated since 3.2, 1

   function Set_Font_Name
      (Dialog   : not null access Gtk_Font_Selection_Dialog_Record;
       Fontname : UTF8_String) return Boolean;
   pragma Obsolescent (Set_Font_Name);
   --  Sets the currently selected font.
   --  Deprecated since 3.2, 1
   --  "fontname": a font name like "Helvetica 12" or "Times Bold 18"

   function Get_Font_Selection
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Font_Selection);
   --  Retrieves the Gtk.Font_Selection.Gtk_Font_Selection widget embedded in
   --  the dialog.
   --  Since: gtk+ 2.22
   --  Deprecated since 3.2, 1

   function Get_Ok_Button
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Ok_Button);
   --  Gets the "OK" button.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.2, 1

   function Get_Preview_Text
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record)
       return UTF8_String;
   pragma Obsolescent (Get_Preview_Text);
   --  Gets the text displayed in the preview area.
   --  Deprecated since 3.2, 1

   procedure Set_Preview_Text
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record;
       Text   : UTF8_String);
   pragma Obsolescent (Set_Preview_Text);
   --  Sets the text displayed in the preview area.
   --  Deprecated since 3.2, 1
   --  "text": the text to display in the preview area

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Font_Selection_Dialog_Record, Gtk_Font_Selection_Dialog);
   function "+"
     (Widget : access Gtk_Font_Selection_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Font_Selection_Dialog
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Font_Selection_Dialog;
