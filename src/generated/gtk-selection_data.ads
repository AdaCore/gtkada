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


pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings; use GNAT.Strings;
with Gdk.Display;  use Gdk.Display;
with Gdk.Pixbuf;   use Gdk.Pixbuf;
with Gdk.Types;    use Gdk.Types;
with Glib;         use Glib;

package Gtk.Selection_Data is

   type Gtk_Selection_Data is new Glib.C_Boxed with null record;
   Null_Gtk_Selection_Data : constant Gtk_Selection_Data;

   function From_Object (Object : System.Address) return Gtk_Selection_Data;
   function From_Object_Free (B : access Gtk_Selection_Data'Class) return Gtk_Selection_Data;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_selection_data_get_type");

   -------------
   -- Methods --
   -------------

   function Copy (Selection : Gtk_Selection_Data) return Gtk_Selection_Data;
   --  Makes a copy of a Gtk.Selection_Data.Gtk_Selection_Data-struct and its
   --  data.

   procedure Free (Selection : Gtk_Selection_Data);
   --  Frees a Gtk.Selection_Data.Gtk_Selection_Data-struct returned from
   --  Gtk.Selection_Data.Copy.

   function Get_Data (Selection : Gtk_Selection_Data) return System.Address;
   --  Retrieves the raw data of the selection.
   --  Since: gtk+ 2.14

   function Get_Data_Type
      (Selection : Gtk_Selection_Data) return Gdk.Types.Gdk_Atom;
   --  Retrieves the data type of the selection.
   --  Since: gtk+ 2.14

   function Get_Display
      (Selection : Gtk_Selection_Data) return Gdk.Display.Gdk_Display;
   --  Retrieves the display of the selection.
   --  Since: gtk+ 2.14

   function Get_Format (Selection : Gtk_Selection_Data) return Glib.Gint;
   --  Retrieves the format of the selection.
   --  Since: gtk+ 2.14

   function Get_Length (Selection : Gtk_Selection_Data) return Glib.Gint;
   --  Retrieves the length of the raw data of the selection.
   --  Since: gtk+ 2.14

   function Get_Pixbuf
      (Selection : Gtk_Selection_Data) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Gets the contents of the selection data as a Gdk.Pixbuf.Gdk_Pixbuf.
   --  Since: gtk+ 2.6

   function Set_Pixbuf
      (Selection : Gtk_Selection_Data;
       Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
       return Boolean;
   --  Sets the contents of the selection from a Gdk.Pixbuf.Gdk_Pixbuf The
   --  pixbuf is converted to the form determined by Selection_Data->target.
   --  Since: gtk+ 2.6
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf

   function Get_Selection
      (Selection : Gtk_Selection_Data) return Gdk.Types.Gdk_Atom;
   --  Retrieves the selection Gdk.Types.Gdk_Atom of the selection data.
   --  Since: gtk+ 2.16

   function Get_Target
      (Selection : Gtk_Selection_Data) return Gdk.Types.Gdk_Atom;
   --  Retrieves the target of the selection.
   --  Since: gtk+ 2.14

   function Get_Text (Selection : Gtk_Selection_Data) return UTF8_String;
   --  Gets the contents of the selection data as a UTF-8 string.

   function Set_Text
      (Selection : Gtk_Selection_Data;
       Str       : UTF8_String;
       Len       : Glib.Gint) return Boolean;
   --  Sets the contents of the selection from a UTF-8 encoded string. The
   --  string is converted to the form determined by Selection_Data->target.
   --  "str": a UTF-8 string
   --  "len": the length of Str, or -1 if Str is nul-terminated.

   function Get_Uris
      (Selection : Gtk_Selection_Data) return GNAT.Strings.String_List;
   --  Gets the contents of the selection data as array of URIs.
   --  Since: gtk+ 2.6

   function Set_Uris
      (Selection : Gtk_Selection_Data;
       Uris      : GNAT.Strings.String_List) return Boolean;
   --  Sets the contents of the selection from a list of URIs. The string is
   --  converted to the form determined by Selection_Data->target.
   --  Since: gtk+ 2.6
   --  "uris": a null-terminated array of strings holding URIs

   function Targets_Include_Image
      (Selection : Gtk_Selection_Data;
       Writable  : Boolean) return Boolean;
   --  Given a Gtk.Selection_Data.Gtk_Selection_Data object holding a list of
   --  targets, determines if any of the targets in Targets can be used to
   --  provide a Gdk.Pixbuf.Gdk_Pixbuf.
   --  Since: gtk+ 2.6
   --  "writable": whether to accept only targets for which GTK+ knows how to
   --  convert a pixbuf into the format

   function Targets_Include_Text
      (Selection : Gtk_Selection_Data) return Boolean;
   --  Given a Gtk.Selection_Data.Gtk_Selection_Data object holding a list of
   --  targets, determines if any of the targets in Targets can be used to
   --  provide text.

   function Targets_Include_Uri
      (Selection : Gtk_Selection_Data) return Boolean;
   --  Given a Gtk.Selection_Data.Gtk_Selection_Data object holding a list of
   --  targets, determines if any of the targets in Targets can be used to
   --  provide a list or URIs.
   --  Since: gtk+ 2.10

   ----------------------
   -- GtkAda additions --
   ----------------------

   ---------------
   -- Selection --
   ---------------

   function Make_Atom (Num : Gulong) return Gdk.Types.Gdk_Atom;
   pragma Import (C, Make_Atom, "ada_make_atom");
   --  Auxiliary subprogram

   subtype Gdk_Selection is Gdk.Types.Gdk_Atom;
   --  These are predefined atom values for several common selections.
   --  You are of course free to create new ones, but most of the time you
   --  should simply use Selection_Primary unless you foresee the need for
   --  multiple simultaneous selections.
   --  To access the clipboard on windows machines, you might need to create
   --  a new selection with Gdk.Property.Atom_Intern ("CLIPBOARD");

   Selection_Primary   : constant Gdk_Selection := Make_Atom (1);
   Selection_Secondary : constant Gdk_Selection := Make_Atom (2);

   procedure Selection_Data_Set
     (Selection : Gtk_Selection_Data;
      The_Type  : Gdk.Types.Gdk_Atom;
      Format    : Gint;
      Data      : System.Address;
      Length    : Gint);
   --  General form of Selection_Data_Set.
   --  Any data can be transmitted. Length is the number of bytes in Data.

   procedure Selection_Data_Set
     (Selection : Gtk_Selection_Data;
      The_Type  : Gdk.Types.Gdk_Atom;
      Format    : Gint;
      Data      : String);
   --  Set the data for a selection (special case for strings)
   --  This function is generally called when a drag-and-drop operation
   --  ask the source widget for the data to be transmitted. In that case,
   --  a Selection_Data was already transmitted and is given as a handler
   --  parameter for the signal "drag_data_get". The_Type can simply be
   --  extracted from the Selection_Data.

   function Get_Targets
     (Selection : Gtk_Selection_Data) return Gdk.Types.Gdk_Atom_Array;
   --  Gets the contents of Selection_Data as an array of targets.
   --  This can be used to interpret the results of getting
   --  the standard TARGETS target that is always supplied for
   --  any selection.
   --  This is different from Get_Target, which indicate the current format
   --  that the selection contains. Get_Targets only applies when Get_Target
   --  is "TARGETS".

   function Get_Data_As_String (Selection : Gtk_Selection_Data) return String;
   --  Return the data as a string.
   --  This is only a convenience function, since it simply creates a string
   --  from the return of Get_Data.

private

   Null_Gtk_Selection_Data : constant Gtk_Selection_Data := (Glib.C_Boxed with null record);

end Gtk.Selection_Data;
