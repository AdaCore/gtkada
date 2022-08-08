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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Font_Button is

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

   package Type_Conversion_Gtk_Font_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Font_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Font_Button);

   -------------------------
   -- Gtk_Font_Button_New --
   -------------------------

   function Gtk_Font_Button_New return Gtk_Font_Button is
      Font_Button : constant Gtk_Font_Button := new Gtk_Font_Button_Record;
   begin
      Gtk.Font_Button.Initialize (Font_Button);
      return Font_Button;
   end Gtk_Font_Button_New;

   -----------------------------------
   -- Gtk_Font_Button_New_With_Font --
   -----------------------------------

   function Gtk_Font_Button_New_With_Font
      (Fontname : UTF8_String) return Gtk_Font_Button
   is
      Font_Button : constant Gtk_Font_Button := new Gtk_Font_Button_Record;
   begin
      Gtk.Font_Button.Initialize_With_Font (Font_Button, Fontname);
      return Font_Button;
   end Gtk_Font_Button_New_With_Font;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Font_Button : out Gtk_Font_Button) is
   begin
      Font_Button := new Gtk_Font_Button_Record;
      Gtk.Font_Button.Initialize (Font_Button);
   end Gtk_New;

   -----------------------
   -- Gtk_New_With_Font --
   -----------------------

   procedure Gtk_New_With_Font
      (Font_Button : out Gtk_Font_Button;
       Fontname    : UTF8_String)
   is
   begin
      Font_Button := new Gtk_Font_Button_Record;
      Gtk.Font_Button.Initialize_With_Font (Font_Button, Fontname);
   end Gtk_New_With_Font;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Font_Button : not null access Gtk_Font_Button_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_font_button_new");
   begin
      if not Font_Button.Is_Created then
         Set_Object (Font_Button, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_With_Font --
   --------------------------

   procedure Initialize_With_Font
      (Font_Button : not null access Gtk_Font_Button_Record'Class;
       Fontname    : UTF8_String)
   is
      function Internal
         (Fontname : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_font_button_new_with_font");
      Tmp_Fontname : Gtkada.Types.Chars_Ptr := New_String (Fontname);
      Tmp_Return   : System.Address;
   begin
      if not Font_Button.Is_Created then
         Tmp_Return := Internal (Tmp_Fontname);
         Free (Tmp_Fontname);
         Set_Object (Font_Button, Tmp_Return);
      end if;
   end Initialize_With_Font;

   -------------------
   -- Get_Font_Name --
   -------------------

   function Get_Font_Name
      (Font_Button : not null access Gtk_Font_Button_Record)
       return UTF8_String
   is
      function Internal
         (Font_Button : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_button_get_font_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Font_Button)));
   end Get_Font_Name;

   -------------------
   -- Get_Show_Size --
   -------------------

   function Get_Show_Size
      (Font_Button : not null access Gtk_Font_Button_Record) return Boolean
   is
      function Internal (Font_Button : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_button_get_show_size");
   begin
      return Internal (Get_Object (Font_Button)) /= 0;
   end Get_Show_Size;

   --------------------
   -- Get_Show_Style --
   --------------------

   function Get_Show_Style
      (Font_Button : not null access Gtk_Font_Button_Record) return Boolean
   is
      function Internal (Font_Button : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_button_get_show_style");
   begin
      return Internal (Get_Object (Font_Button)) /= 0;
   end Get_Show_Style;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Font_Button : not null access Gtk_Font_Button_Record)
       return UTF8_String
   is
      function Internal
         (Font_Button : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_button_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Font_Button)));
   end Get_Title;

   ------------------
   -- Get_Use_Font --
   ------------------

   function Get_Use_Font
      (Font_Button : not null access Gtk_Font_Button_Record) return Boolean
   is
      function Internal (Font_Button : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_button_get_use_font");
   begin
      return Internal (Get_Object (Font_Button)) /= 0;
   end Get_Use_Font;

   ------------------
   -- Get_Use_Size --
   ------------------

   function Get_Use_Size
      (Font_Button : not null access Gtk_Font_Button_Record) return Boolean
   is
      function Internal (Font_Button : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_button_get_use_size");
   begin
      return Internal (Get_Object (Font_Button)) /= 0;
   end Get_Use_Size;

   ---------------------
   -- Set_Filter_Func --
   ---------------------

   procedure Set_Filter_Func
      (Self   : not null access Gtk_Font_Button_Record;
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
         (Self      : not null access Gtk.Font_Button.Gtk_Font_Button_Record'Class;
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

   -------------------
   -- Set_Font_Name --
   -------------------

   function Set_Font_Name
      (Font_Button : not null access Gtk_Font_Button_Record;
       Fontname    : UTF8_String) return Boolean
   is
      function Internal
         (Font_Button : System.Address;
          Fontname    : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_button_set_font_name");
      Tmp_Fontname : Gtkada.Types.Chars_Ptr := New_String (Fontname);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Font_Button), Tmp_Fontname);
      Free (Tmp_Fontname);
      return Tmp_Return /= 0;
   end Set_Font_Name;

   -------------------
   -- Set_Show_Size --
   -------------------

   procedure Set_Show_Size
      (Font_Button : not null access Gtk_Font_Button_Record;
       Show_Size   : Boolean)
   is
      procedure Internal
         (Font_Button : System.Address;
          Show_Size   : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_font_button_set_show_size");
   begin
      Internal (Get_Object (Font_Button), Boolean'Pos (Show_Size));
   end Set_Show_Size;

   --------------------
   -- Set_Show_Style --
   --------------------

   procedure Set_Show_Style
      (Font_Button : not null access Gtk_Font_Button_Record;
       Show_Style  : Boolean)
   is
      procedure Internal
         (Font_Button : System.Address;
          Show_Style  : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_font_button_set_show_style");
   begin
      Internal (Get_Object (Font_Button), Boolean'Pos (Show_Style));
   end Set_Show_Style;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Font_Button : not null access Gtk_Font_Button_Record;
       Title       : UTF8_String)
   is
      procedure Internal
         (Font_Button : System.Address;
          Title       : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_font_button_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Get_Object (Font_Button), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   ------------------
   -- Set_Use_Font --
   ------------------

   procedure Set_Use_Font
      (Font_Button : not null access Gtk_Font_Button_Record;
       Use_Font    : Boolean)
   is
      procedure Internal
         (Font_Button : System.Address;
          Use_Font    : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_font_button_set_use_font");
   begin
      Internal (Get_Object (Font_Button), Boolean'Pos (Use_Font));
   end Set_Use_Font;

   ------------------
   -- Set_Use_Size --
   ------------------

   procedure Set_Use_Size
      (Font_Button : not null access Gtk_Font_Button_Record;
       Use_Size    : Boolean)
   is
      procedure Internal
         (Font_Button : System.Address;
          Use_Size    : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_font_button_set_use_size");
   begin
      Internal (Get_Object (Font_Button), Boolean'Pos (Use_Size));
   end Set_Use_Size;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Font_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_do_set_related_action");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Do_Set_Related_Action;

   ---------------------
   -- Get_Action_Name --
   ---------------------

   function Get_Action_Name
      (Self : not null access Gtk_Font_Button_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_actionable_get_action_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Action_Name;

   -----------------------------
   -- Get_Action_Target_Value --
   -----------------------------

   function Get_Action_Target_Value
      (Self : not null access Gtk_Font_Button_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_actionable_get_action_target_value");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Action_Target_Value;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
      (Self : not null access Gtk_Font_Button_Record) return UTF8_String
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
      (Self : not null access Gtk_Font_Button_Record)
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
      (Self : not null access Gtk_Font_Button_Record)
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
      (Self : not null access Gtk_Font_Button_Record)
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
      (Self : not null access Gtk_Font_Button_Record) return UTF8_String
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
      (Self : not null access Gtk_Font_Button_Record)
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
      (Self : not null access Gtk_Font_Button_Record) return Glib.Gint
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
      (Self : not null access Gtk_Font_Button_Record) return UTF8_String
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
      (Self : not null access Gtk_Font_Button_Record)
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
      (Self : not null access Gtk_Font_Button_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_chooser_get_preview_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Preview_Text;

   ------------------------
   -- Get_Related_Action --
   ------------------------

   function Get_Related_Action
      (Self : not null access Gtk_Font_Button_Record)
       return Gtk.Action.Gtk_Action
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_activatable_get_related_action");
      Stub_Gtk_Action : Gtk.Action.Gtk_Action_Record;
   begin
      return Gtk.Action.Gtk_Action (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Action));
   end Get_Related_Action;

   ----------------------------
   -- Get_Show_Preview_Entry --
   ----------------------------

   function Get_Show_Preview_Entry
      (Self : not null access Gtk_Font_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_chooser_get_show_preview_entry");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Show_Preview_Entry;

   -------------------------------
   -- Get_Use_Action_Appearance --
   -------------------------------

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Font_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_activatable_get_use_action_appearance");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Action_Appearance;

   ---------------------
   -- Set_Action_Name --
   ---------------------

   procedure Set_Action_Name
      (Self        : not null access Gtk_Font_Button_Record;
       Action_Name : UTF8_String := "")
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_actionable_set_action_name");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Action_Name = "" then
         Tmp_Action_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Action_Name := New_String (Action_Name);
      end if;
      Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Set_Action_Name;

   -----------------------------
   -- Set_Action_Target_Value --
   -----------------------------

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Font_Button_Record;
       Target_Value : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self         : System.Address;
          Target_Value : System.Address);
      pragma Import (C, Internal, "gtk_actionable_set_action_target_value");
   begin
      Internal (Get_Object (Self), Get_Object (Target_Value));
   end Set_Action_Target_Value;

   ------------------------------
   -- Set_Detailed_Action_Name --
   ------------------------------

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Font_Button_Record;
       Detailed_Action_Name : UTF8_String)
   is
      procedure Internal
         (Self                 : System.Address;
          Detailed_Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_actionable_set_detailed_action_name");
      Tmp_Detailed_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Detailed_Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Detailed_Action_Name);
      Free (Tmp_Detailed_Action_Name);
   end Set_Detailed_Action_Name;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
      (Self     : not null access Gtk_Font_Button_Record;
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
      (Self      : not null access Gtk_Font_Button_Record;
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
      (Self    : not null access Gtk_Font_Button_Record;
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
      (Self     : not null access Gtk_Font_Button_Record;
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
      (Self  : not null access Gtk_Font_Button_Record;
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
      (Self : not null access Gtk_Font_Button_Record;
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

   ------------------------
   -- Set_Related_Action --
   ------------------------

   procedure Set_Related_Action
      (Self   : not null access Gtk_Font_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_set_related_action");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Set_Related_Action;

   ----------------------------
   -- Set_Show_Preview_Entry --
   ----------------------------

   procedure Set_Show_Preview_Entry
      (Self               : not null access Gtk_Font_Button_Record;
       Show_Preview_Entry : Boolean)
   is
      procedure Internal
         (Self               : System.Address;
          Show_Preview_Entry : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_font_chooser_set_show_preview_entry");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Show_Preview_Entry));
   end Set_Show_Preview_Entry;

   -------------------------------
   -- Set_Use_Action_Appearance --
   -------------------------------

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Font_Button_Record;
       Use_Appearance : Boolean)
   is
      procedure Internal
         (Self           : System.Address;
          Use_Appearance : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_activatable_set_use_action_appearance");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Appearance));
   end Set_Use_Action_Appearance;

   ----------------------------
   -- Sync_Action_Properties --
   ----------------------------

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Font_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Font_Button_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Font_Button_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Font_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Font_Button_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Font_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Font_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Font_Button_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Font_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Font_Button_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Font_Button_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Font_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   --------------------------------
   -- Marsh_Gtk_Font_Button_Void --
   --------------------------------

   procedure Marsh_Gtk_Font_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Font_Button_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Font_Button := Gtk_Font_Button (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Font_Button_Void;

   -----------------
   -- On_Font_Set --
   -----------------

   procedure On_Font_Set
      (Self  : not null access Gtk_Font_Button_Record;
       Call  : Cb_Gtk_Font_Button_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "font-set" & ASCII.NUL, Call, After);
   end On_Font_Set;

   -----------------
   -- On_Font_Set --
   -----------------

   procedure On_Font_Set
      (Self  : not null access Gtk_Font_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "font-set" & ASCII.NUL, Call, After, Slot);
   end On_Font_Set;

end Gtk.Font_Button;
