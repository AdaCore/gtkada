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
--  The Gtk_Color_Selection_Dialog provides a standard dialog which allows the
--  user to select a color much like the Gtk_File_Selection provides a standard
--  dialog for file selection.
--
--  </description>
--  <group>Drawing</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                use Glib;
with Glib.Properties;     use Glib.Properties;
with Glib.Types;          use Glib.Types;
with Gtk.Buildable;       use Gtk.Buildable;
with Gtk.Color_Selection; use Gtk.Color_Selection;
with Gtk.Dialog;          use Gtk.Dialog;

package Gtk.Color_Selection_Dialog is

   type Gtk_Color_Selection_Dialog_Record is new Gtk_Dialog_Record with null record;
   type Gtk_Color_Selection_Dialog is access all Gtk_Color_Selection_Dialog_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Color_Selection_Dialog : out Gtk_Color_Selection_Dialog;
       Title                  : UTF8_String);
   procedure Initialize
      (Color_Selection_Dialog : not null access Gtk_Color_Selection_Dialog_Record'Class;
       Title                  : UTF8_String);
   --  Creates a new Gtk.Color_Selection_Dialog.Gtk_Color_Selection_Dialog.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "title": a string containing the title text for the dialog.

   function Gtk_Color_Selection_Dialog_New
      (Title : UTF8_String) return Gtk_Color_Selection_Dialog;
   --  Creates a new Gtk.Color_Selection_Dialog.Gtk_Color_Selection_Dialog.
   --  "title": a string containing the title text for the dialog.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_color_selection_dialog_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Color_Selection
      (Color_Selection_Dialog : not null access Gtk_Color_Selection_Dialog_Record)
       return Gtk.Color_Selection.Gtk_Color_Selection;
   --  Retrieves the Gtk.Color_Selection.Gtk_Color_Selection widget embedded
   --  in the dialog.
   --  Since: gtk+ 2.14

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Cancel_Button_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Color_Selection_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Help_Button_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Ok_Button_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Color_Selection_Dialog_Record, Gtk_Color_Selection_Dialog);
   function "+"
     (Widget : access Gtk_Color_Selection_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Color_Selection_Dialog
   renames Implements_Gtk_Buildable.To_Object;

private
   Ok_Button_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("ok-button");
   Help_Button_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("help-button");
   Color_Selection_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("color-selection");
   Cancel_Button_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("cancel-button");
end Gtk.Color_Selection_Dialog;
