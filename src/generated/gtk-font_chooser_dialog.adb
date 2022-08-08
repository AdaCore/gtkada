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
with Ada.Unchecked_Conversion;
with Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Font_Chooser_Dialog is

   procedure C_Gtk_Font_Chooser_Set_Filter_Func
      (Self      : System.Address;
       Filter    : System.Address;
       User_Data : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Font_Chooser_Set_Filter_Func, "gtk_font_chooser_set_filter_func");
   --  Adds a filter function that decides which fonts to display in the font
   --  chooser.
   --  Since: gtk+ 3.2
   --  "filter": a Gtk_Font_Filter_Func, or null
   --  "user_data": data to pass to Filter
   --  "destroy": function to call to free Data when it is no longer needed

   function To_Gtk_Font_Filter_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Font_Filter_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Font_Filter_Func, System.Address);

   function Internal_Gtk_Font_Filter_Func
      (Family : System.Address;
       Face   : System.Address;
       Data   : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Font_Filter_Func);
   --  "family": a Pango.Font_Family.Pango_Font_Family
   --  "face": a Pango.Font_Face.Pango_Font_Face belonging to Family
   --  "data": user data passed to Gtk.Font_Chooser.Set_Filter_Func

   -----------------------------------
   -- Internal_Gtk_Font_Filter_Func --
   -----------------------------------

   function Internal_Gtk_Font_Filter_Func
      (Family : System.Address;
       Face   : System.Address;
       Data   : System.Address) return Glib.Gboolean
   is
      Func                   : constant Gtk_Font_Filter_Func := To_Gtk_Font_Filter_Func (Data);
      Stub_Pango_Font_Family : Pango.Font_Family.Pango_Font_Family_Record;
      Stub_Pango_Font_Face   : Pango.Font_Face.Pango_Font_Face_Record;
   begin
      return Boolean'Pos (Func (Pango.Font_Family.Pango_Font_Family (Get_User_Data (Family, Stub_Pango_Font_Family)), Pango.Font_Face.Pango_Font_Face (Get_User_Data (Face, Stub_Pango_Font_Face))));
   end Internal_Gtk_Font_Filter_Func;

   package Type_Conversion_Gtk_Font_Chooser_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Font_Chooser_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Font_Chooser_Dialog);

   ---------------------------------
   -- Gtk_Font_Chooser_Dialog_New --
   ---------------------------------

   function Gtk_Font_Chooser_Dialog_New
      (Title  : UTF8_String := "";
       Parent : access Gtk.Window.Gtk_Window_Record'Class)
       return Gtk_Font_Chooser_Dialog
   is
      Self : constant Gtk_Font_Chooser_Dialog := new Gtk_Font_Chooser_Dialog_Record;
   begin
      Gtk.Font_Chooser_Dialog.Initialize (Self, Title, Parent);
      return Self;
   end Gtk_Font_Chooser_Dialog_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gtk_Font_Chooser_Dialog;
       Title  : UTF8_String := "";
       Parent : access Gtk.Window.Gtk_Window_Record'Class)
   is
   begin
      Self := new Gtk_Font_Chooser_Dialog_Record;
      Gtk.Font_Chooser_Dialog.Initialize (Self, Title, Parent);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Font_Chooser_Dialog_Record'Class;
       Title  : UTF8_String := "";
       Parent : access Gtk.Window.Gtk_Window_Record'Class)
   is
      function Internal
         (Title  : Gtkada.Types.Chars_Ptr;
          Parent : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_chooser_dialog_new");
      Tmp_Title  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         if Title = "" then
            Tmp_Title := Gtkada.Types.Null_Ptr;
         else
            Tmp_Title := New_String (Title);
         end if;
         Tmp_Return := Internal (Tmp_Title, Get_Object_Or_Null (GObject (Parent)));
         Free (Tmp_Title);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize;

   ---------------------
   -- Set_Filter_Func --
   ---------------------

   procedure Set_Filter_Func
      (Self   : not null access Gtk_Font_Chooser_Dialog_Record;
       Filter : Gtk_Font_Filter_Func)
   is
   begin
      if Filter = null then
         C_Gtk_Font_Chooser_Set_Filter_Func (Get_Object (Self), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Font_Chooser_Set_Filter_Func (Get_Object (Self), Internal_Gtk_Font_Filter_Func'Address, To_Address (Filter), System.Null_Address);
      end if;
   end Set_Filter_Func;

   package body Set_Filter_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Font_Filter_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Font_Filter_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Font_Filter_Func, System.Address);

      function Internal_Cb
         (Family : System.Address;
          Face   : System.Address;
          Data   : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  The type of function that is used for deciding what fonts get shown
      --  in a Gtk.Font_Chooser.Gtk_Font_Chooser. See
      --  Gtk.Font_Chooser.Set_Filter_Func.
      --  "family": a Pango.Font_Family.Pango_Font_Family
      --  "face": a Pango.Font_Face.Pango_Font_Face belonging to Family
      --  "data": user data passed to Gtk.Font_Chooser.Set_Filter_Func

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Family : System.Address;
          Face   : System.Address;
          Data   : System.Address) return Glib.Gboolean
      is
         D                      : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Pango_Font_Family : Pango.Font_Family.Pango_Font_Family_Record;
         Stub_Pango_Font_Face   : Pango.Font_Face.Pango_Font_Face_Record;
      begin
         return Boolean'Pos (To_Gtk_Font_Filter_Func (D.Func) (Pango.Font_Family.Pango_Font_Family (Get_User_Data (Family, Stub_Pango_Font_Family)), Pango.Font_Face.Pango_Font_Face (Get_User_Data (Face, Stub_Pango_Font_Face)), D.Data.all));
      end Internal_Cb;

      ---------------------
      -- Set_Filter_Func --
      ---------------------

      procedure Set_Filter_Func
         (Self      : not null access Gtk.Font_Chooser_Dialog.Gtk_Font_Chooser_Dialog_Record'Class;
          Filter    : Gtk_Font_Filter_Func;
          User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Filter = null then
            C_Gtk_Font_Chooser_Set_Filter_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Filter), User_Data);
            C_Gtk_Font_Chooser_Set_Filter_Func (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Filter_Func;

   end Set_Filter_Func_User_Data;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
      (Self : not null access Gtk_Font_Chooser_Dialog_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_chooser_get_font");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Font;

   -------------------
   -- Get_Font_Desc --
   -------------------

   function Get_Font_Desc
      (Self : not null access Gtk_Font_Chooser_Dialog_Record)
       return Pango.Font.Pango_Font_Description
   is
      function Internal
         (Self : System.Address) return Pango.Font.Pango_Font_Description;
      pragma Import (C, Internal, "gtk_font_chooser_get_font_desc");
   begin
      return Internal (Get_Object (Self));
   end Get_Font_Desc;

   -------------------
   -- Get_Font_Face --
   -------------------

   function Get_Font_Face
      (Self : not null access Gtk_Font_Chooser_Dialog_Record)
       return Pango.Font_Face.Pango_Font_Face
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_chooser_get_font_face");
      Stub_Pango_Font_Face : Pango.Font_Face.Pango_Font_Face_Record;
   begin
      return Pango.Font_Face.Pango_Font_Face (Get_User_Data (Internal (Get_Object (Self)), Stub_Pango_Font_Face));
   end Get_Font_Face;

   ---------------------
   -- Get_Font_Family --
   ---------------------

   function Get_Font_Family
      (Self : not null access Gtk_Font_Chooser_Dialog_Record)
       return Pango.Font_Family.Pango_Font_Family
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_chooser_get_font_family");
      Stub_Pango_Font_Family : Pango.Font_Family.Pango_Font_Family_Record;
   begin
      return Pango.Font_Family.Pango_Font_Family (Get_User_Data (Internal (Get_Object (Self)), Stub_Pango_Font_Family));
   end Get_Font_Family;

   -----------------------
   -- Get_Font_Features --
   -----------------------

   function Get_Font_Features
      (Self : not null access Gtk_Font_Chooser_Dialog_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_chooser_get_font_features");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Font_Features;

   ------------------
   -- Get_Font_Map --
   ------------------

   function Get_Font_Map
      (Self : not null access Gtk_Font_Chooser_Dialog_Record)
       return Pango.Font_Map.Pango_Font_Map
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_chooser_get_font_map");
      Stub_Pango_Font_Map : Pango.Font_Map.Pango_Font_Map_Record;
   begin
      return Pango.Font_Map.Pango_Font_Map (Get_User_Data (Internal (Get_Object (Self)), Stub_Pango_Font_Map));
   end Get_Font_Map;

   -------------------
   -- Get_Font_Size --
   -------------------

   function Get_Font_Size
      (Self : not null access Gtk_Font_Chooser_Dialog_Record)
       return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_font_chooser_get_font_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Font_Size;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
      (Self : not null access Gtk_Font_Chooser_Dialog_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_chooser_get_language");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Language;

   ---------------
   -- Get_Level --
   ---------------

   function Get_Level
      (Self : not null access Gtk_Font_Chooser_Dialog_Record)
       return Gtk.Font_Chooser.Gtk_Font_Chooser_Level
   is
      function Internal
         (Self : System.Address)
          return Gtk.Font_Chooser.Gtk_Font_Chooser_Level;
      pragma Import (C, Internal, "gtk_font_chooser_get_level");
   begin
      return Internal (Get_Object (Self));
   end Get_Level;

   ----------------------
   -- Get_Preview_Text --
   ----------------------

   function Get_Preview_Text
      (Self : not null access Gtk_Font_Chooser_Dialog_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_chooser_get_preview_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Preview_Text;

   ----------------------------
   -- Get_Show_Preview_Entry --
   ----------------------------

   function Get_Show_Preview_Entry
      (Self : not null access Gtk_Font_Chooser_Dialog_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_chooser_get_show_preview_entry");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Show_Preview_Entry;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
      (Self     : not null access Gtk_Font_Chooser_Dialog_Record;
       Fontname : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Fontname : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_font_chooser_set_font");
      Tmp_Fontname : Gtkada.Types.Chars_Ptr := New_String (Fontname);
   begin
      Internal (Get_Object (Self), Tmp_Fontname);
      Free (Tmp_Fontname);
   end Set_Font;

   -------------------
   -- Set_Font_Desc --
   -------------------

   procedure Set_Font_Desc
      (Self      : not null access Gtk_Font_Chooser_Dialog_Record;
       Font_Desc : Pango.Font.Pango_Font_Description)
   is
      procedure Internal
         (Self      : System.Address;
          Font_Desc : Pango.Font.Pango_Font_Description);
      pragma Import (C, Internal, "gtk_font_chooser_set_font_desc");
   begin
      Internal (Get_Object (Self), Font_Desc);
   end Set_Font_Desc;

   ------------------
   -- Set_Font_Map --
   ------------------

   procedure Set_Font_Map
      (Self    : not null access Gtk_Font_Chooser_Dialog_Record;
       Fontmap : access Pango.Font_Map.Pango_Font_Map_Record'Class)
   is
      procedure Internal (Self : System.Address; Fontmap : System.Address);
      pragma Import (C, Internal, "gtk_font_chooser_set_font_map");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Fontmap)));
   end Set_Font_Map;

   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language
      (Self     : not null access Gtk_Font_Chooser_Dialog_Record;
       Language : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Language : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_font_chooser_set_language");
      Tmp_Language : Gtkada.Types.Chars_Ptr := New_String (Language);
   begin
      Internal (Get_Object (Self), Tmp_Language);
      Free (Tmp_Language);
   end Set_Language;

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level
      (Self  : not null access Gtk_Font_Chooser_Dialog_Record;
       Level : Gtk.Font_Chooser.Gtk_Font_Chooser_Level)
   is
      procedure Internal
         (Self  : System.Address;
          Level : Gtk.Font_Chooser.Gtk_Font_Chooser_Level);
      pragma Import (C, Internal, "gtk_font_chooser_set_level");
   begin
      Internal (Get_Object (Self), Level);
   end Set_Level;

   ----------------------
   -- Set_Preview_Text --
   ----------------------

   procedure Set_Preview_Text
      (Self : not null access Gtk_Font_Chooser_Dialog_Record;
       Text : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_font_chooser_set_preview_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Tmp_Text);
      Free (Tmp_Text);
   end Set_Preview_Text;

   ----------------------------
   -- Set_Show_Preview_Entry --
   ----------------------------

   procedure Set_Show_Preview_Entry
      (Self               : not null access Gtk_Font_Chooser_Dialog_Record;
       Show_Preview_Entry : Boolean)
   is
      procedure Internal
         (Self               : System.Address;
          Show_Preview_Entry : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_font_chooser_set_show_preview_entry");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Show_Preview_Entry));
   end Set_Show_Preview_Entry;

end Gtk.Font_Chooser_Dialog;
