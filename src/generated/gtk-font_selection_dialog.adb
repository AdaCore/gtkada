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
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Font_Selection_Dialog is

   package Type_Conversion_Gtk_Font_Selection_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Font_Selection_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Font_Selection_Dialog);

   -----------------------------------
   -- Gtk_Font_Selection_Dialog_New --
   -----------------------------------

   function Gtk_Font_Selection_Dialog_New
      (Title : UTF8_String) return Gtk_Font_Selection_Dialog
   is
      Dialog : constant Gtk_Font_Selection_Dialog := new Gtk_Font_Selection_Dialog_Record;
   begin
      Gtk.Font_Selection_Dialog.Initialize (Dialog, Title);
      return Dialog;
   end Gtk_Font_Selection_Dialog_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Dialog : out Gtk_Font_Selection_Dialog;
       Title  : UTF8_String)
   is
   begin
      Dialog := new Gtk_Font_Selection_Dialog_Record;
      Gtk.Font_Selection_Dialog.Initialize (Dialog, Title);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record'Class;
       Title  : UTF8_String)
   is
      function Internal
         (Title : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_new");
      Tmp_Title  : Gtkada.Types.Chars_Ptr := New_String (Title);
      Tmp_Return : System.Address;
   begin
      if not Dialog.Is_Created then
         Tmp_Return := Internal (Tmp_Title);
         Free (Tmp_Title);
         Set_Object (Dialog, Tmp_Return);
      end if;
   end Initialize;

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_cancel_button");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Dialog)), Stub_Gtk_Widget));
   end Get_Cancel_Button;

   -------------------
   -- Get_Font_Name --
   -------------------

   function Get_Font_Name
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record)
       return UTF8_String
   is
      function Internal
         (Dialog : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_font_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Dialog)));
   end Get_Font_Name;

   ------------------------
   -- Get_Font_Selection --
   ------------------------

   function Get_Font_Selection
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_font_selection");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Dialog)), Stub_Gtk_Widget));
   end Get_Font_Selection;

   -------------------
   -- Get_Ok_Button --
   -------------------

   function Get_Ok_Button
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_ok_button");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Dialog)), Stub_Gtk_Widget));
   end Get_Ok_Button;

   ----------------------
   -- Get_Preview_Text --
   ----------------------

   function Get_Preview_Text
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record)
       return UTF8_String
   is
      function Internal
         (Dialog : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_preview_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Dialog)));
   end Get_Preview_Text;

   -------------------
   -- Set_Font_Name --
   -------------------

   function Set_Font_Name
      (Dialog   : not null access Gtk_Font_Selection_Dialog_Record;
       Fontname : UTF8_String) return Boolean
   is
      function Internal
         (Dialog   : System.Address;
          Fontname : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_selection_dialog_set_font_name");
      Tmp_Fontname : Gtkada.Types.Chars_Ptr := New_String (Fontname);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Dialog), Tmp_Fontname);
      Free (Tmp_Fontname);
      return Tmp_Return /= 0;
   end Set_Font_Name;

   ----------------------
   -- Set_Preview_Text --
   ----------------------

   procedure Set_Preview_Text
      (Dialog : not null access Gtk_Font_Selection_Dialog_Record;
       Text   : UTF8_String)
   is
      procedure Internal
         (Dialog : System.Address;
          Text   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_font_selection_dialog_set_preview_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Dialog), Tmp_Text);
      Free (Tmp_Text);
   end Set_Preview_Text;

end Gtk.Font_Selection_Dialog;
