-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

--  <c_version>1.3.11</c_version>

with Glib.Object;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget;
with Gtkada.Types; use Gtkada.Types;

package Gtk.Rc is

   type Gtk_Rc_Style_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Rc_Style is access all Gtk_Rc_Style_Record'Class;

   procedure Gtk_New (Rc_Style : out Gtk_Rc_Style);

   procedure Initialize (Rc_Style : access Gtk_Rc_Style_Record'Class);

   function Get_Type return Glib.GType;
   --  Return the internal value associated with Gtk_Rc_Style.

   function Copy (Orig : access Gtk_Rc_Style_Record) return Gtk_Rc_Style;
   --  Make a copy of the specified Gtk_Rc.Style.
   --  This function will correctly copy an rc style that is a member of a
   --  class derived from Gtk_Rc_Style.

   procedure Add_Default_File (Filename : String);
   --  Add a file to the list of files to be parsed at the end of Gtk.Main.Init

   procedure Set_Default_Files (Filenames : Chars_Ptr_Array);
   --  Set the list of files that GtkAda will read at the end of Gtk.Main.Init

   function Get_Default_Files return Chars_Ptr_Array;
   --  Retrieve the current list of RC files that will be parsed
   --  at the end of Gtk.Main.Init

   function Get_Style
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Gtk_Style;
   --  Find all matching RC styles for a given widget, composites them
   --  together, and then create a Gtk_Style representing the composite
   --  appearance. (GtkAda actually keeps a cache of previously created styles,
   --  so a new style may not be created)
   --  Return the resulting style. No refcount is added to the returned style,
   --  so if you want to save this style around, you should add a reference
   --  yourself.

   --  ??? function Get_Style_By_Paths
   --    (Settings    : access Gtk_Settings_Record'Class;
   --     Widget_Path : String := "";
   --     Class_Path  : String := "";
   --     The_Type    : Glib.GType := Glib.GType_None) return Gtk_Style;
   --  Create up a Gtk_Style from styles defined in a RC file by providing
   --  the raw components used in matching. This function may be useful
   --  when creating pseudo-widgets that should be themed like widgets but
   --  don't actually have corresponding GtkAda widgets. An example of this
   --  would be items inside a GNOME canvas widget.
   --
   --  Widget_Path: the widget path to use when looking up the style
   --  Class_Path: the class path to use when looking up the style
   --  The_Type: a type that will be used along with parent types of this type
   --            when matching against class styles.
   --  Return a style created by matching with the supplied paths, or null if
   --  nothing matching was specified and the default style should be used. The
   --  returned value is owned by GtkAda as part of an internal cache, so you
   --  must call Ref on the returned value if you want to keep a reference to
   --  it.

   --  ??? function Reparse_All_For_Settings
   --    (Settings   : access Gtk_Settings_Record'Class;
   --     Force_Load : Boolean) return Boolean;
   --  If the modification time on any previously read file
   --  for the given Gtk_Settings has changed, discard all style information
   --  and then reread all previously read RC files.
   --  Return True if the files were reread.

   --  ??? function Find_Pixmap_In_Path
   --    (Settings    : access Gtk_Settings_Record'Class;
   --     Pixmap_File : String) return String;
   --  Look up a file in pixmap path for the specified Gtk_Settings.
   --  If the file is not found, it outputs a warning message and return null.
   --  Pixmap_File: name of the pixmap file to locate.

   procedure Parse (Filename : String);

   procedure Parse_String (Rc_String : String);

   function Reparse_All return Boolean;
   --  If the modification time on any previously read file for the
   --  default Gtk_Settings has changed, discard all style information
   --  and then reread all previously read RC files.
   --  Return True if the files were reread.

   function Find_Module_In_Path (Module_File : String) return String;

   function Get_Theme_Dir return String;

   function Get_Module_Dir return String;

   function Get_Im_Module_Path return String;

   function Get_Im_Module_File return String;

   ------------------------------
   -- Widget related functions --
   ------------------------------

   procedure Modify_Style
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Style  : access Gtk_Rc_Style_Record'Class);
   --  Modify the default style of a widget.

   function Get_Modifier_Style
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Gtk_Rc_Style;
   --  Return the current modifier style for the widget.
   --  (As set by Modify_Style.) If no style has previously set, a new
   --  Gtk_Rc_Style will be created with all values unset, and set as the
   --  modifier style for the widget. If you make changes to this rc
   --  style, you must call Modify_Style, passing in the
   --  returned rc style, to make sure that your changes take effect.
   --
   --  Return value: the modifier style for the widget. This rc style is
   --    owned by the widget. If you want to keep a pointer to value this
   --    around, you must add a refcount using Ref.

private
   type Gtk_Rc_Style_Record is new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_rc_style_get_type");

end Gtk.Rc;
