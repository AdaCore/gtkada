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

package body Gtk.Font_Selection is

   package Type_Conversion_Gtk_Font_Selection is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Font_Selection_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Font_Selection);

   ----------------------------
   -- Gtk_Font_Selection_New --
   ----------------------------

   function Gtk_Font_Selection_New return Gtk_Font_Selection is
      Fontsel : constant Gtk_Font_Selection := new Gtk_Font_Selection_Record;
   begin
      Gtk.Font_Selection.Initialize (Fontsel);
      return Fontsel;
   end Gtk_Font_Selection_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Fontsel : out Gtk_Font_Selection) is
   begin
      Fontsel := new Gtk_Font_Selection_Record;
      Gtk.Font_Selection.Initialize (Fontsel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Fontsel : not null access Gtk_Font_Selection_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_new");
   begin
      if not Fontsel.Is_Created then
         Set_Object (Fontsel, Internal);
      end if;
   end Initialize;

   -------------------
   -- Get_Face_List --
   -------------------

   function Get_Face_List
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Fontsel : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_get_face_list");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Fontsel)), Stub_Gtk_Widget));
   end Get_Face_List;

   ---------------------
   -- Get_Family_List --
   ---------------------

   function Get_Family_List
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Fontsel : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_get_family_list");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Fontsel)), Stub_Gtk_Widget));
   end Get_Family_List;

   -------------------
   -- Get_Font_Name --
   -------------------

   function Get_Font_Name
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return UTF8_String
   is
      function Internal
         (Fontsel : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_selection_get_font_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Fontsel)));
   end Get_Font_Name;

   -----------------------
   -- Get_Preview_Entry --
   -----------------------

   function Get_Preview_Entry
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Fontsel : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_get_preview_entry");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Fontsel)), Stub_Gtk_Widget));
   end Get_Preview_Entry;

   ----------------------
   -- Get_Preview_Text --
   ----------------------

   function Get_Preview_Text
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return UTF8_String
   is
      function Internal
         (Fontsel : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_selection_get_preview_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Fontsel)));
   end Get_Preview_Text;

   --------------
   -- Get_Size --
   --------------

   function Get_Size
      (Fontsel : not null access Gtk_Font_Selection_Record) return Glib.Gint
   is
      function Internal (Fontsel : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_font_selection_get_size");
   begin
      return Internal (Get_Object (Fontsel));
   end Get_Size;

   --------------------
   -- Get_Size_Entry --
   --------------------

   function Get_Size_Entry
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Fontsel : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_get_size_entry");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Fontsel)), Stub_Gtk_Widget));
   end Get_Size_Entry;

   -------------------
   -- Get_Size_List --
   -------------------

   function Get_Size_List
      (Fontsel : not null access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Fontsel : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_get_size_list");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Fontsel)), Stub_Gtk_Widget));
   end Get_Size_List;

   -------------------
   -- Set_Font_Name --
   -------------------

   function Set_Font_Name
      (Fontsel  : not null access Gtk_Font_Selection_Record;
       Fontname : UTF8_String) return Boolean
   is
      function Internal
         (Fontsel  : System.Address;
          Fontname : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_selection_set_font_name");
      Tmp_Fontname : Gtkada.Types.Chars_Ptr := New_String (Fontname);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Fontsel), Tmp_Fontname);
      Free (Tmp_Fontname);
      return Tmp_Return /= 0;
   end Set_Font_Name;

   ----------------------
   -- Set_Preview_Text --
   ----------------------

   procedure Set_Preview_Text
      (Fontsel : not null access Gtk_Font_Selection_Record;
       Text    : UTF8_String)
   is
      procedure Internal
         (Fontsel : System.Address;
          Text    : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_font_selection_set_preview_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Fontsel), Tmp_Text);
      Free (Tmp_Text);
   end Set_Preview_Text;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Font_Selection_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Font_Selection_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

end Gtk.Font_Selection;
