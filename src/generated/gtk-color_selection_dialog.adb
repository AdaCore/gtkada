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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Color_Selection_Dialog is

   package Type_Conversion_Gtk_Color_Selection_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Color_Selection_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Color_Selection_Dialog);

   ------------------------------------
   -- Gtk_Color_Selection_Dialog_New --
   ------------------------------------

   function Gtk_Color_Selection_Dialog_New
      (Title : UTF8_String) return Gtk_Color_Selection_Dialog
   is
      Color_Selection_Dialog : constant Gtk_Color_Selection_Dialog := new Gtk_Color_Selection_Dialog_Record;
   begin
      Gtk.Color_Selection_Dialog.Initialize (Color_Selection_Dialog, Title);
      return Color_Selection_Dialog;
   end Gtk_Color_Selection_Dialog_New;

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
      (Color_Selection_Dialog : not null access Gtk_Color_Selection_Dialog_Record'Class;
       Title                  : UTF8_String)
   is
      function Internal
         (Title : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_color_selection_dialog_new");
      Tmp_Title  : Gtkada.Types.Chars_Ptr := New_String (Title);
      Tmp_Return : System.Address;
   begin
      if not Color_Selection_Dialog.Is_Created then
         Tmp_Return := Internal (Tmp_Title);
         Free (Tmp_Title);
         Set_Object (Color_Selection_Dialog, Tmp_Return);
      end if;
   end Initialize;

   -------------------------
   -- Get_Color_Selection --
   -------------------------

   function Get_Color_Selection
      (Color_Selection_Dialog : not null access Gtk_Color_Selection_Dialog_Record)
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
