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

package body Gtk.About_Dialog is

   package Type_Conversion_Gtk_About_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_About_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_About_Dialog);

   --------------------------
   -- Gtk_About_Dialog_New --
   --------------------------

   function Gtk_About_Dialog_New return Gtk_About_Dialog is
      About : constant Gtk_About_Dialog := new Gtk_About_Dialog_Record;
   begin
      Gtk.About_Dialog.Initialize (About);
      return About;
   end Gtk_About_Dialog_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (About : out Gtk_About_Dialog) is
   begin
      About := new Gtk_About_Dialog_Record;
      Gtk.About_Dialog.Initialize (About);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (About : not null access Gtk_About_Dialog_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_about_dialog_new");
   begin
      if not About.Is_Created then
         Set_Object (About, Internal);
      end if;
   end Initialize;

   ------------------------
   -- Add_Credit_Section --
   ------------------------

   procedure Add_Credit_Section
      (About        : not null access Gtk_About_Dialog_Record;
       Section_Name : UTF8_String;
       People       : GNAT.Strings.String_List)
   is
      procedure Internal
         (About        : System.Address;
          Section_Name : Gtkada.Types.Chars_Ptr;
          People       : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_about_dialog_add_credit_section");
      Tmp_Section_Name : Gtkada.Types.Chars_Ptr := New_String (Section_Name);
      Tmp_People       : Gtkada.Types.chars_ptr_array := From_String_List (People);
   begin
      Internal (Get_Object (About), Tmp_Section_Name, Tmp_People);
      Gtkada.Types.Free (Tmp_People);
      Free (Tmp_Section_Name);
   end Add_Credit_Section;

   -----------------
   -- Get_Artists --
   -----------------

   function Get_Artists
      (About : not null access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (About : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_about_dialog_get_artists");
   begin
      return To_String_List (Internal (Get_Object (About)).all);
   end Get_Artists;

   -----------------
   -- Get_Authors --
   -----------------

   function Get_Authors
      (About : not null access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (About : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_about_dialog_get_authors");
   begin
      return To_String_List (Internal (Get_Object (About)).all);
   end Get_Authors;

   ------------------
   -- Get_Comments --
   ------------------

   function Get_Comments
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_comments");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (About)));
   end Get_Comments;

   -------------------
   -- Get_Copyright --
   -------------------

   function Get_Copyright
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_copyright");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (About)));
   end Get_Copyright;

   ---------------------
   -- Get_Documenters --
   ---------------------

   function Get_Documenters
      (About : not null access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (About : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_about_dialog_get_documenters");
   begin
      return To_String_List (Internal (Get_Object (About)).all);
   end Get_Documenters;

   -----------------
   -- Get_License --
   -----------------

   function Get_License
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_license");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (About)));
   end Get_License;

   ----------------------
   -- Get_License_Type --
   ----------------------

   function Get_License_Type
      (About : not null access Gtk_About_Dialog_Record) return Gtk_License
   is
      function Internal (About : System.Address) return Gtk_License;
      pragma Import (C, Internal, "gtk_about_dialog_get_license_type");
   begin
      return Internal (Get_Object (About));
   end Get_License_Type;

   --------------
   -- Get_Logo --
   --------------

   function Get_Logo
      (About : not null access Gtk_About_Dialog_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (About : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_about_dialog_get_logo");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (About)), Stub_Gdk_Pixbuf));
   end Get_Logo;

   ------------------------
   -- Get_Logo_Icon_Name --
   ------------------------

   function Get_Logo_Icon_Name
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_logo_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (About)));
   end Get_Logo_Icon_Name;

   ----------------------
   -- Get_Program_Name --
   ----------------------

   function Get_Program_Name
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_program_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (About)));
   end Get_Program_Name;

   ----------------------------
   -- Get_Translator_Credits --
   ----------------------------

   function Get_Translator_Credits
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_translator_credits");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (About)));
   end Get_Translator_Credits;

   -----------------
   -- Get_Version --
   -----------------

   function Get_Version
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_version");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (About)));
   end Get_Version;

   -----------------
   -- Get_Website --
   -----------------

   function Get_Website
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_website");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (About)));
   end Get_Website;

   -----------------------
   -- Get_Website_Label --
   -----------------------

   function Get_Website_Label
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_website_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (About)));
   end Get_Website_Label;

   ----------------------
   -- Get_Wrap_License --
   ----------------------

   function Get_Wrap_License
      (About : not null access Gtk_About_Dialog_Record) return Boolean
   is
      function Internal (About : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_about_dialog_get_wrap_license");
   begin
      return Internal (Get_Object (About)) /= 0;
   end Get_Wrap_License;

   -----------------
   -- Set_Artists --
   -----------------

   procedure Set_Artists
      (About   : not null access Gtk_About_Dialog_Record;
       Artists : GNAT.Strings.String_List)
   is
      procedure Internal
         (About   : System.Address;
          Artists : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_about_dialog_set_artists");
      Tmp_Artists : Gtkada.Types.chars_ptr_array := From_String_List (Artists);
   begin
      Internal (Get_Object (About), Tmp_Artists);
      Gtkada.Types.Free (Tmp_Artists);
   end Set_Artists;

   -----------------
   -- Set_Authors --
   -----------------

   procedure Set_Authors
      (About   : not null access Gtk_About_Dialog_Record;
       Authors : GNAT.Strings.String_List)
   is
      procedure Internal
         (About   : System.Address;
          Authors : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_about_dialog_set_authors");
      Tmp_Authors : Gtkada.Types.chars_ptr_array := From_String_List (Authors);
   begin
      Internal (Get_Object (About), Tmp_Authors);
      Gtkada.Types.Free (Tmp_Authors);
   end Set_Authors;

   ------------------
   -- Set_Comments --
   ------------------

   procedure Set_Comments
      (About    : not null access Gtk_About_Dialog_Record;
       Comments : UTF8_String := "")
   is
      procedure Internal
         (About    : System.Address;
          Comments : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_comments");
      Tmp_Comments : Gtkada.Types.Chars_Ptr;
   begin
      if Comments = "" then
         Tmp_Comments := Gtkada.Types.Null_Ptr;
      else
         Tmp_Comments := New_String (Comments);
      end if;
      Internal (Get_Object (About), Tmp_Comments);
      Free (Tmp_Comments);
   end Set_Comments;

   -------------------
   -- Set_Copyright --
   -------------------

   procedure Set_Copyright
      (About     : not null access Gtk_About_Dialog_Record;
       Copyright : UTF8_String := "")
   is
      procedure Internal
         (About     : System.Address;
          Copyright : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_copyright");
      Tmp_Copyright : Gtkada.Types.Chars_Ptr;
   begin
      if Copyright = "" then
         Tmp_Copyright := Gtkada.Types.Null_Ptr;
      else
         Tmp_Copyright := New_String (Copyright);
      end if;
      Internal (Get_Object (About), Tmp_Copyright);
      Free (Tmp_Copyright);
   end Set_Copyright;

   ---------------------
   -- Set_Documenters --
   ---------------------

   procedure Set_Documenters
      (About       : not null access Gtk_About_Dialog_Record;
       Documenters : GNAT.Strings.String_List)
   is
      procedure Internal
         (About       : System.Address;
          Documenters : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_about_dialog_set_documenters");
      Tmp_Documenters : Gtkada.Types.chars_ptr_array := From_String_List (Documenters);
   begin
      Internal (Get_Object (About), Tmp_Documenters);
      Gtkada.Types.Free (Tmp_Documenters);
   end Set_Documenters;

   -----------------
   -- Set_License --
   -----------------

   procedure Set_License
      (About   : not null access Gtk_About_Dialog_Record;
       License : UTF8_String := "")
   is
      procedure Internal
         (About   : System.Address;
          License : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_license");
      Tmp_License : Gtkada.Types.Chars_Ptr;
   begin
      if License = "" then
         Tmp_License := Gtkada.Types.Null_Ptr;
      else
         Tmp_License := New_String (License);
      end if;
      Internal (Get_Object (About), Tmp_License);
      Free (Tmp_License);
   end Set_License;

   ----------------------
   -- Set_License_Type --
   ----------------------

   procedure Set_License_Type
      (About        : not null access Gtk_About_Dialog_Record;
       License_Type : Gtk_License)
   is
      procedure Internal
         (About        : System.Address;
          License_Type : Gtk_License);
      pragma Import (C, Internal, "gtk_about_dialog_set_license_type");
   begin
      Internal (Get_Object (About), License_Type);
   end Set_License_Type;

   --------------
   -- Set_Logo --
   --------------

   procedure Set_Logo
      (About : not null access Gtk_About_Dialog_Record;
       Logo  : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal (About : System.Address; Logo : System.Address);
      pragma Import (C, Internal, "gtk_about_dialog_set_logo");
   begin
      Internal (Get_Object (About), Get_Object_Or_Null (GObject (Logo)));
   end Set_Logo;

   ------------------------
   -- Set_Logo_Icon_Name --
   ------------------------

   procedure Set_Logo_Icon_Name
      (About     : not null access Gtk_About_Dialog_Record;
       Icon_Name : UTF8_String := "")
   is
      procedure Internal
         (About     : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_logo_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Icon_Name = "" then
         Tmp_Icon_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Icon_Name := New_String (Icon_Name);
      end if;
      Internal (Get_Object (About), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_Logo_Icon_Name;

   ----------------------
   -- Set_Program_Name --
   ----------------------

   procedure Set_Program_Name
      (About : not null access Gtk_About_Dialog_Record;
       Name  : UTF8_String)
   is
      procedure Internal
         (About : System.Address;
          Name  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_program_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (About), Tmp_Name);
      Free (Tmp_Name);
   end Set_Program_Name;

   ----------------------------
   -- Set_Translator_Credits --
   ----------------------------

   procedure Set_Translator_Credits
      (About              : not null access Gtk_About_Dialog_Record;
       Translator_Credits : UTF8_String := "")
   is
      procedure Internal
         (About              : System.Address;
          Translator_Credits : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_translator_credits");
      Tmp_Translator_Credits : Gtkada.Types.Chars_Ptr;
   begin
      if Translator_Credits = "" then
         Tmp_Translator_Credits := Gtkada.Types.Null_Ptr;
      else
         Tmp_Translator_Credits := New_String (Translator_Credits);
      end if;
      Internal (Get_Object (About), Tmp_Translator_Credits);
      Free (Tmp_Translator_Credits);
   end Set_Translator_Credits;

   -----------------
   -- Set_Version --
   -----------------

   procedure Set_Version
      (About   : not null access Gtk_About_Dialog_Record;
       Version : UTF8_String := "")
   is
      procedure Internal
         (About   : System.Address;
          Version : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_version");
      Tmp_Version : Gtkada.Types.Chars_Ptr;
   begin
      if Version = "" then
         Tmp_Version := Gtkada.Types.Null_Ptr;
      else
         Tmp_Version := New_String (Version);
      end if;
      Internal (Get_Object (About), Tmp_Version);
      Free (Tmp_Version);
   end Set_Version;

   -----------------
   -- Set_Website --
   -----------------

   procedure Set_Website
      (About   : not null access Gtk_About_Dialog_Record;
       Website : UTF8_String := "")
   is
      procedure Internal
         (About   : System.Address;
          Website : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_website");
      Tmp_Website : Gtkada.Types.Chars_Ptr;
   begin
      if Website = "" then
         Tmp_Website := Gtkada.Types.Null_Ptr;
      else
         Tmp_Website := New_String (Website);
      end if;
      Internal (Get_Object (About), Tmp_Website);
      Free (Tmp_Website);
   end Set_Website;

   -----------------------
   -- Set_Website_Label --
   -----------------------

   procedure Set_Website_Label
      (About         : not null access Gtk_About_Dialog_Record;
       Website_Label : UTF8_String)
   is
      procedure Internal
         (About         : System.Address;
          Website_Label : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_website_label");
      Tmp_Website_Label : Gtkada.Types.Chars_Ptr := New_String (Website_Label);
   begin
      Internal (Get_Object (About), Tmp_Website_Label);
      Free (Tmp_Website_Label);
   end Set_Website_Label;

   ----------------------
   -- Set_Wrap_License --
   ----------------------

   procedure Set_Wrap_License
      (About        : not null access Gtk_About_Dialog_Record;
       Wrap_License : Boolean)
   is
      procedure Internal
         (About        : System.Address;
          Wrap_License : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_about_dialog_set_wrap_license");
   begin
      Internal (Get_Object (About), Boolean'Pos (Wrap_License));
   end Set_Wrap_License;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_About_Dialog_UTF8_String_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_About_Dialog_UTF8_String_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Boolean);

   procedure Connect
      (Object  : access Gtk_About_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_About_Dialog_UTF8_String_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_About_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_UTF8_String_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Boolean);

   procedure Marsh_Gtk_About_Dialog_UTF8_String_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_About_Dialog_UTF8_String_Boolean);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_About_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_About_Dialog_UTF8_String_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_About_Dialog_UTF8_String_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_About_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------------------
   -- Marsh_GObject_UTF8_String_Boolean --
   ---------------------------------------

   procedure Marsh_GObject_UTF8_String_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_UTF8_String (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Boolean;

   ------------------------------------------------
   -- Marsh_Gtk_About_Dialog_UTF8_String_Boolean --
   ------------------------------------------------

   procedure Marsh_Gtk_About_Dialog_UTF8_String_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_About_Dialog_UTF8_String_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_About_Dialog := Gtk_About_Dialog (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_UTF8_String (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_About_Dialog_UTF8_String_Boolean;

   ----------------------
   -- On_Activate_Link --
   ----------------------

   procedure On_Activate_Link
      (Self  : not null access Gtk_About_Dialog_Record;
       Call  : Cb_Gtk_About_Dialog_UTF8_String_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate-link" & ASCII.NUL, Call, After);
   end On_Activate_Link;

   ----------------------
   -- On_Activate_Link --
   ----------------------

   procedure On_Activate_Link
      (Self  : not null access Gtk_About_Dialog_Record;
       Call  : Cb_GObject_UTF8_String_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate-link" & ASCII.NUL, Call, After, Slot);
   end On_Activate_Link;

end Gtk.About_Dialog;
