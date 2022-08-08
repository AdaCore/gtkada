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
--  This widget provides a nice way for the user of your application to select
--  fonts. It first searches on your system for the list of fonts available,
--  and displays a set of boxes to select them based on their name, their
--  weight, their size, etc. This widget is provided in two forms, one widget
--  that can be embedded in any container, a Gtk_Font_Selection, whereas the
--  other one comes directly in its own separate window (to be popped up as a
--  dialog).
--
--  Some filters can be applied to the widget, when you want the user to
--  select only a font only among a specific subset (like bitmap or true-type
--  fonts for instance). There are two kinds of filters: a base filter, set in
--  your application and that the user can not change; a user filter that can
--  be modified interactively by the user.
--
--  </description>
--  <screenshot>gtk-fontsel</screenshot>
--  <group>Selectors</group>
--  <testgtk>create_font_selection.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Box;         use Gtk.Box;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Font_Selection is

   type Gtk_Font_Selection_Record is new Gtk_Box_Record with null record;
   type Gtk_Font_Selection is access all Gtk_Font_Selection_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Fontsel : out Gtk_Font_Selection);
   procedure Initialize
      (Fontsel : not null access Gtk_Font_Selection_Record'Class);
   --  Creates a new Gtk.Font_Selection.Gtk_Font_Selection.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Font_Selection_New return Gtk_Font_Selection;
   --  Creates a new Gtk.Font_Selection.Gtk_Font_Selection.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_font_selection_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Face_List
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Face_List);
   --  This returns the Gtk.Tree_View.Gtk_Tree_View which lists all styles
   --  available for the selected font. For example, "Regular", "Bold", etc.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.2, 1

   function Get_Family_List
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Family_List);
   --  This returns the Gtk.Tree_View.Gtk_Tree_View that lists font families,
   --  for example, "Sans", "Serif", etc.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.2, 1

   function Get_Font_Name
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return UTF8_String;
   pragma Obsolescent (Get_Font_Name);
   --  Gets the currently-selected font name.
   --  Note that this can be a different string than what you set with
   --  Gtk.Font_Selection.Set_Font_Name, as the font selection widget may
   --  normalize font names and thus return a string with a different
   --  structure. For example, "Helvetica Italic Bold 12" could be normalized
   --  to "Helvetica Bold Italic 12". Use Pango.Font.Equal if you want to
   --  compare two font descriptions.
   --  Deprecated since 3.2, 1

   function Set_Font_Name
      (Fontsel  : not null access Gtk_Font_Selection_Record;
       Fontname : UTF8_String) return Boolean;
   pragma Obsolescent (Set_Font_Name);
   --  Sets the currently-selected font.
   --  Note that the Fontsel needs to know the screen in which it will appear
   --  for this to work; this can be guaranteed by simply making sure that the
   --  Fontsel is inserted in a toplevel window before you call this function.
   --  Deprecated since 3.2, 1
   --  "fontname": a font name like "Helvetica 12" or "Times Bold 18"

   function Get_Preview_Entry
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Preview_Entry);
   --  This returns the Gtk.GEntry.Gtk_Entry used to display the font as a
   --  preview.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.2, 1

   function Get_Preview_Text
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return UTF8_String;
   pragma Obsolescent (Get_Preview_Text);
   --  Gets the text displayed in the preview area.
   --  Deprecated since 3.2, 1

   procedure Set_Preview_Text
      (Fontsel : not null access Gtk_Font_Selection_Record;
       Text    : UTF8_String);
   pragma Obsolescent (Set_Preview_Text);
   --  Sets the text displayed in the preview area. The Text is used to show
   --  how the selected font looks.
   --  Deprecated since 3.2, 1
   --  "text": the text to display in the preview area

   function Get_Size
      (Fontsel : not null access Gtk_Font_Selection_Record) return Glib.Gint;
   pragma Obsolescent (Get_Size);
   --  The selected font size.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.2, 1

   function Get_Size_Entry
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Size_Entry);
   --  This returns the Gtk.GEntry.Gtk_Entry used to allow the user to edit
   --  the font number manually instead of selecting it from the list of font
   --  sizes.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.2, 1

   function Get_Size_List
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Size_List);
   --  This returns the Gtk.Tree_View.Gtk_Tree_View used to list font sizes.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.2, 1

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Font_Selection_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Font_Selection_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Font_Name_Property : constant Glib.Properties.Property_String;

   Preview_Text_Property : constant Glib.Properties.Property_String;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Font_Selection_Record, Gtk_Font_Selection);
   function "+"
     (Widget : access Gtk_Font_Selection_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Font_Selection
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Font_Selection_Record, Gtk_Font_Selection);
   function "+"
     (Widget : access Gtk_Font_Selection_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Font_Selection
   renames Implements_Gtk_Orientable.To_Object;

private
   Preview_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("preview-text");
   Font_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font-name");
end Gtk.Font_Selection;
