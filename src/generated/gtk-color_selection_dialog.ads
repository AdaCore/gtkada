-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
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
with Gtk.Widget;          use Gtk.Widget;

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
      (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record'Class;
       Title                  : UTF8_String);

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_color_selection_dialog_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Color_Selection
      (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
       return Gtk.Color_Selection.Gtk_Color_Selection;
   --  Retrieves the Gtk.Color_Selection.Gtk_Color_Selection widget embedded
   --  in the dialog.
   --  Since: gtk+ 2.14

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Color_Selection_Dialog_Record, Gtk_Color_Selection_Dialog);
   function "+"
     (Widget : access Gtk_Color_Selection_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Color_Selection_Dialog
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Cancel_Button_Property
   --  Type: Gtk.Widget.Gtk_Widget
   --  Flags: read-write
   --
   --  Name: Color_Selection_Property
   --  Type: Gtk.Widget.Gtk_Widget
   --  Flags: read-write
   --
   --  Name: Help_Button_Property
   --  Type: Gtk.Widget.Gtk_Widget
   --  Flags: read-write
   --
   --  Name: Ok_Button_Property
   --  Type: Gtk.Widget.Gtk_Widget
   --  Flags: read-write

   Cancel_Button_Property : constant Glib.Properties.Property_Object;
   Color_Selection_Property : constant Glib.Properties.Property_Object;
   Help_Button_Property : constant Glib.Properties.Property_Object;
   Ok_Button_Property : constant Glib.Properties.Property_Object;

private
   Cancel_Button_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("cancel-button");
   Color_Selection_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("color-selection");
   Help_Button_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("help-button");
   Ok_Button_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("ok-button");
end Gtk.Color_Selection_Dialog;
