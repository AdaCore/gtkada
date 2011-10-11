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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Color_Selection_Dialog is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Color_Selection_Dialog_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Color_Selection_Dialog : out Gtk_Color_Selection_Dialog;
       Title                  : UTF8_String)
   is
   begin
      Color_Selection_Dialog := new Gtk_Color_Selection_Dialog_Record;
      Gtk.Color_Selection_Dialog.Initialize (Color_Selection_Dialog, Title);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record'Class;
       Title                  : UTF8_String)
   is
      function Internal
         (Title : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_color_selection_dialog_new");
      Tmp_Title  : Interfaces.C.Strings.chars_ptr := New_String (Title);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Title);
      Free (Tmp_Title);
      Set_Object (Color_Selection_Dialog, Tmp_Return);
   end Initialize;

   -------------------------
   -- Get_Color_Selection --
   -------------------------

   function Get_Color_Selection
      (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
       return Gtk.Color_Selection.Gtk_Color_Selection
   is
      function Internal
         (Color_Selection_Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_color_selection_dialog_get_color_selection");
      Stub_Gtk_Color_Selection : Gtk.Color_Selection.Gtk_Color_Selection_Record;
   begin
      return Gtk.Color_Selection.Gtk_Color_Selection (Get_User_Data (Internal (Get_Object (Color_Selection_Dialog)), Stub_Gtk_Color_Selection));
   end Get_Color_Selection;

end Gtk.Color_Selection_Dialog;
