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
--  The Gtk.Font_Chooser_Widget.Gtk_Font_Chooser_Widget widget lists the
--  available fonts, styles and sizes, allowing the user to select a font. It
--  is used in the Gtk.Font_Chooser_Dialog.Gtk_Font_Chooser_Dialog widget to
--  provide a dialog box for selecting fonts.
--
--  To set the font which is initially selected, use Gtk.Font_Chooser.Set_Font
--  or Gtk.Font_Chooser.Set_Font_Desc.
--
--  To get the selected font use Gtk.Font_Chooser.Get_Font or
--  Gtk.Font_Chooser.Get_Font_Desc.
--
--  To change the text which is shown in the preview area, use
--  Gtk.Font_Chooser.Set_Preview_Text.
--
--  # CSS nodes
--
--  GtkFontChooserWidget has a single CSS node with name fontchooser.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Box;           use Gtk.Box;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Font_Chooser;  use Gtk.Font_Chooser;
with Gtk.Orientable;    use Gtk.Orientable;
with Pango.Font;        use Pango.Font;
with Pango.Font_Face;   use Pango.Font_Face;
with Pango.Font_Family; use Pango.Font_Family;
with Pango.Font_Map;    use Pango.Font_Map;

package Gtk.Font_Chooser_Widget is

   type Gtk_Font_Chooser_Widget_Record is new Gtk_Box_Record with null record;
   type Gtk_Font_Chooser_Widget is access all Gtk_Font_Chooser_Widget_Record'Class;

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

   procedure Gtk_New (Self : out Gtk_Font_Chooser_Widget);
   procedure Initialize
      (Self : not null access Gtk_Font_Chooser_Widget_Record'Class);
   --  Creates a new Gtk.Font_Chooser_Widget.Gtk_Font_Chooser_Widget.
   --  Since: gtk+ 3.2
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Font_Chooser_Widget_New return Gtk_Font_Chooser_Widget;
   --  Creates a new Gtk.Font_Chooser_Widget.Gtk_Font_Chooser_Widget.
   --  Since: gtk+ 3.2

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_font_chooser_widget_get_type");

   -------------
   -- Methods --
   -------------

   procedure Set_Filter_Func
      (Self   : not null access Gtk_Font_Chooser_Widget_Record;
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
         (Self      : not null access Gtk.Font_Chooser_Widget.Gtk_Font_Chooser_Widget_Record'Class;
          Filter    : Gtk_Font_Filter_Func;
          User_Data : User_Data_Type);
      --  Adds a filter function that decides which fonts to display in the
      --  font chooser.
      --  Since: gtk+ 3.2
      --  "filter": a Gtk_Font_Filter_Func, or null
      --  "user_data": data to pass to Filter

   end Set_Filter_Func_User_Data;

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Font
      (Self : not null access Gtk_Font_Chooser_Widget_Record)
       return UTF8_String;

   procedure Set_Font
      (Self     : not null access Gtk_Font_Chooser_Widget_Record;
       Fontname : UTF8_String);

   function Get_Font_Desc
      (Self : not null access Gtk_Font_Chooser_Widget_Record)
       return Pango.Font.Pango_Font_Description;

   procedure Set_Font_Desc
      (Self      : not null access Gtk_Font_Chooser_Widget_Record;
       Font_Desc : Pango.Font.Pango_Font_Description);

   function Get_Font_Face
      (Self : not null access Gtk_Font_Chooser_Widget_Record)
       return Pango.Font_Face.Pango_Font_Face;

   function Get_Font_Family
      (Self : not null access Gtk_Font_Chooser_Widget_Record)
       return Pango.Font_Family.Pango_Font_Family;

   function Get_Font_Features
      (Self : not null access Gtk_Font_Chooser_Widget_Record)
       return UTF8_String;

   function Get_Font_Map
      (Self : not null access Gtk_Font_Chooser_Widget_Record)
       return Pango.Font_Map.Pango_Font_Map;

   procedure Set_Font_Map
      (Self    : not null access Gtk_Font_Chooser_Widget_Record;
       Fontmap : access Pango.Font_Map.Pango_Font_Map_Record'Class);

   function Get_Font_Size
      (Self : not null access Gtk_Font_Chooser_Widget_Record)
       return Glib.Gint;

   function Get_Language
      (Self : not null access Gtk_Font_Chooser_Widget_Record)
       return UTF8_String;

   procedure Set_Language
      (Self     : not null access Gtk_Font_Chooser_Widget_Record;
       Language : UTF8_String);

   function Get_Level
      (Self : not null access Gtk_Font_Chooser_Widget_Record)
       return Gtk.Font_Chooser.Gtk_Font_Chooser_Level;

   procedure Set_Level
      (Self  : not null access Gtk_Font_Chooser_Widget_Record;
       Level : Gtk.Font_Chooser.Gtk_Font_Chooser_Level);

   function Get_Preview_Text
      (Self : not null access Gtk_Font_Chooser_Widget_Record)
       return UTF8_String;

   procedure Set_Preview_Text
      (Self : not null access Gtk_Font_Chooser_Widget_Record;
       Text : UTF8_String);

   function Get_Show_Preview_Entry
      (Self : not null access Gtk_Font_Chooser_Widget_Record) return Boolean;

   procedure Set_Show_Preview_Entry
      (Self               : not null access Gtk_Font_Chooser_Widget_Record;
       Show_Preview_Entry : Boolean);

   function Get_Orientation
      (Self : not null access Gtk_Font_Chooser_Widget_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Font_Chooser_Widget_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Tweak_Action_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Action.Gtk_Action
   --  A toggle action that can be used to switch to the tweak page of the
   --  font chooser widget, which lets the user tweak the OpenType features and
   --  variation axes of the selected font.
   --
   --  The action will be enabled or disabled depending on whether the
   --  selected font has any features or axes.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "FontChooser"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Font_Chooser_Widget_Record, Gtk_Font_Chooser_Widget);
   function "+"
     (Widget : access Gtk_Font_Chooser_Widget_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Font_Chooser_Widget
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Font_Chooser is new Glib.Types.Implements
     (Gtk.Font_Chooser.Gtk_Font_Chooser, Gtk_Font_Chooser_Widget_Record, Gtk_Font_Chooser_Widget);
   function "+"
     (Widget : access Gtk_Font_Chooser_Widget_Record'Class)
   return Gtk.Font_Chooser.Gtk_Font_Chooser
   renames Implements_Gtk_Font_Chooser.To_Interface;
   function "-"
     (Interf : Gtk.Font_Chooser.Gtk_Font_Chooser)
   return Gtk_Font_Chooser_Widget
   renames Implements_Gtk_Font_Chooser.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Font_Chooser_Widget_Record, Gtk_Font_Chooser_Widget);
   function "+"
     (Widget : access Gtk_Font_Chooser_Widget_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Font_Chooser_Widget
   renames Implements_Gtk_Orientable.To_Object;

private
   Tweak_Action_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("tweak-action");
end Gtk.Font_Chooser_Widget;
