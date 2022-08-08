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
with Glib.Values;              use Glib.Values;
with Gtk.Arguments;            use Gtk.Arguments;
with Gtkada.Bindings;          use Gtkada.Bindings;

package body Gtk.Font_Chooser is

   procedure C_Gtk_Font_Chooser_Set_Filter_Func
      (Self      : Gtk_Font_Chooser;
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

   --------------
   -- Get_Font --
   --------------

   function Get_Font (Self : Gtk_Font_Chooser) return UTF8_String is
      function Internal
         (Self : Gtk_Font_Chooser) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_chooser_get_font");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Font;

   -------------------
   -- Get_Font_Face --
   -------------------

   function Get_Font_Face
      (Self : Gtk_Font_Chooser) return Pango.Font_Face.Pango_Font_Face
   is
      function Internal (Self : Gtk_Font_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_font_chooser_get_font_face");
      Stub_Pango_Font_Face : Pango.Font_Face.Pango_Font_Face_Record;
   begin
      return Pango.Font_Face.Pango_Font_Face (Get_User_Data (Internal (Self), Stub_Pango_Font_Face));
   end Get_Font_Face;

   ---------------------
   -- Get_Font_Family --
   ---------------------

   function Get_Font_Family
      (Self : Gtk_Font_Chooser) return Pango.Font_Family.Pango_Font_Family
   is
      function Internal (Self : Gtk_Font_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_font_chooser_get_font_family");
      Stub_Pango_Font_Family : Pango.Font_Family.Pango_Font_Family_Record;
   begin
      return Pango.Font_Family.Pango_Font_Family (Get_User_Data (Internal (Self), Stub_Pango_Font_Family));
   end Get_Font_Family;

   -----------------------
   -- Get_Font_Features --
   -----------------------

   function Get_Font_Features (Self : Gtk_Font_Chooser) return UTF8_String is
      function Internal
         (Self : Gtk_Font_Chooser) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_chooser_get_font_features");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Font_Features;

   ------------------
   -- Get_Font_Map --
   ------------------

   function Get_Font_Map
      (Self : Gtk_Font_Chooser) return Pango.Font_Map.Pango_Font_Map
   is
      function Internal (Self : Gtk_Font_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_font_chooser_get_font_map");
      Stub_Pango_Font_Map : Pango.Font_Map.Pango_Font_Map_Record;
   begin
      return Pango.Font_Map.Pango_Font_Map (Get_User_Data (Internal (Self), Stub_Pango_Font_Map));
   end Get_Font_Map;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language (Self : Gtk_Font_Chooser) return UTF8_String is
      function Internal
         (Self : Gtk_Font_Chooser) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_chooser_get_language");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Language;

   ----------------------
   -- Get_Preview_Text --
   ----------------------

   function Get_Preview_Text (Self : Gtk_Font_Chooser) return UTF8_String is
      function Internal
         (Self : Gtk_Font_Chooser) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_chooser_get_preview_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Preview_Text;

   ----------------------------
   -- Get_Show_Preview_Entry --
   ----------------------------

   function Get_Show_Preview_Entry (Self : Gtk_Font_Chooser) return Boolean is
      function Internal (Self : Gtk_Font_Chooser) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_chooser_get_show_preview_entry");
   begin
      return Internal (Self) /= 0;
   end Get_Show_Preview_Entry;

   ---------------------
   -- Set_Filter_Func --
   ---------------------

   procedure Set_Filter_Func
      (Self   : Gtk_Font_Chooser;
       Filter : Gtk_Font_Filter_Func)
   is
   begin
      if Filter = null then
         C_Gtk_Font_Chooser_Set_Filter_Func (Self, System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Font_Chooser_Set_Filter_Func (Self, Internal_Gtk_Font_Filter_Func'Address, To_Address (Filter), System.Null_Address);
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
         (Self      : Gtk.Font_Chooser.Gtk_Font_Chooser;
          Filter    : Gtk_Font_Filter_Func;
          User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Filter = null then
            C_Gtk_Font_Chooser_Set_Filter_Func (Self, System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Filter), User_Data);
            C_Gtk_Font_Chooser_Set_Filter_Func (Self, Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Filter_Func;

   end Set_Filter_Func_User_Data;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font (Self : Gtk_Font_Chooser; Fontname : UTF8_String) is
      procedure Internal
         (Self     : Gtk_Font_Chooser;
          Fontname : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_font_chooser_set_font");
      Tmp_Fontname : Gtkada.Types.Chars_Ptr := New_String (Fontname);
   begin
      Internal (Self, Tmp_Fontname);
      Free (Tmp_Fontname);
   end Set_Font;

   ------------------
   -- Set_Font_Map --
   ------------------

   procedure Set_Font_Map
      (Self    : Gtk_Font_Chooser;
       Fontmap : access Pango.Font_Map.Pango_Font_Map_Record'Class)
   is
      procedure Internal (Self : Gtk_Font_Chooser; Fontmap : System.Address);
      pragma Import (C, Internal, "gtk_font_chooser_set_font_map");
   begin
      Internal (Self, Get_Object_Or_Null (GObject (Fontmap)));
   end Set_Font_Map;

   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language (Self : Gtk_Font_Chooser; Language : UTF8_String) is
      procedure Internal
         (Self     : Gtk_Font_Chooser;
          Language : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_font_chooser_set_language");
      Tmp_Language : Gtkada.Types.Chars_Ptr := New_String (Language);
   begin
      Internal (Self, Tmp_Language);
      Free (Tmp_Language);
   end Set_Language;

   ----------------------
   -- Set_Preview_Text --
   ----------------------

   procedure Set_Preview_Text (Self : Gtk_Font_Chooser; Text : UTF8_String) is
      procedure Internal
         (Self : Gtk_Font_Chooser;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_font_chooser_set_preview_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Self, Tmp_Text);
      Free (Tmp_Text);
   end Set_Preview_Text;

   ----------------------------
   -- Set_Show_Preview_Entry --
   ----------------------------

   procedure Set_Show_Preview_Entry
      (Self               : Gtk_Font_Chooser;
       Show_Preview_Entry : Boolean)
   is
      procedure Internal
         (Self               : Gtk_Font_Chooser;
          Show_Preview_Entry : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_font_chooser_set_show_preview_entry");
   begin
      Internal (Self, Boolean'Pos (Show_Preview_Entry));
   end Set_Show_Preview_Entry;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Font_Chooser_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Font_Chooser_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Void);

   procedure Connect
      (Object  : Gtk_Font_Chooser;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Font_Chooser_UTF8_String_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Gtk_Font_Chooser;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Void);

   procedure Marsh_Gtk_Font_Chooser_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Font_Chooser_UTF8_String_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gtk_Font_Chooser;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Font_Chooser_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Font_Chooser_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gtk_Font_Chooser;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------
   -- Marsh_GObject_UTF8_String_Void --
   ------------------------------------

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Void;

   ---------------------------------------------
   -- Marsh_Gtk_Font_Chooser_UTF8_String_Void --
   ---------------------------------------------

   procedure Marsh_Gtk_Font_Chooser_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Font_Chooser_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Font_Chooser := Gtk_Font_Chooser (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Font_Chooser_UTF8_String_Void;

   -----------------------
   -- On_Font_Activated --
   -----------------------

   procedure On_Font_Activated
      (Self  : Gtk_Font_Chooser;
       Call  : Cb_Gtk_Font_Chooser_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "font-activated" & ASCII.NUL, Call, After);
   end On_Font_Activated;

   -----------------------
   -- On_Font_Activated --
   -----------------------

   procedure On_Font_Activated
      (Self  : Gtk_Font_Chooser;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "font-activated" & ASCII.NUL, Call, After, Slot);
   end On_Font_Activated;

   function "+" (W : Gtk_Font_Chooser) return Gtk_Font_Chooser is
   begin
      return W;
   end "+";

end Gtk.Font_Chooser;
