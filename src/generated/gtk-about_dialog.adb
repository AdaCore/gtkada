------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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
with Gdk.Display;
with Gdk.Surface;
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
      Self : constant Gtk_About_Dialog := new Gtk_About_Dialog_Record;
   begin
      Gtk.About_Dialog.Initialize (Self);
      return Self;
   end Gtk_About_Dialog_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_About_Dialog) is
   begin
      Self := new Gtk_About_Dialog_Record;
      Gtk.About_Dialog.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_About_Dialog_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_about_dialog_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------------------
   -- Add_Credit_Section --
   ------------------------

   procedure Add_Credit_Section
      (Self         : not null access Gtk_About_Dialog_Record;
       Section_Name : UTF8_String;
       People       : GNAT.Strings.String_List)
   is
      procedure Internal
         (Self         : System.Address;
          Section_Name : Gtkada.Types.Chars_Ptr;
          People       : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_about_dialog_add_credit_section");
      Tmp_Section_Name : Gtkada.Types.Chars_Ptr := New_String (Section_Name);
      Tmp_People       : Gtkada.Types.chars_ptr_array := From_String_List (People);
   begin
      Internal (Get_Object (Self), Tmp_Section_Name, Tmp_People);
      Gtkada.Types.Free (Tmp_People);
      Free (Tmp_Section_Name);
   end Add_Credit_Section;

   -----------------
   -- Get_Artists --
   -----------------

   function Get_Artists
      (Self : not null access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_about_dialog_get_artists");
   begin
      return To_String_List (Internal (Get_Object (Self)).all);
   end Get_Artists;

   -----------------
   -- Get_Authors --
   -----------------

   function Get_Authors
      (Self : not null access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_about_dialog_get_authors");
   begin
      return To_String_List (Internal (Get_Object (Self)).all);
   end Get_Authors;

   ------------------
   -- Get_Comments --
   ------------------

   function Get_Comments
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_comments");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Comments;

   -------------------
   -- Get_Copyright --
   -------------------

   function Get_Copyright
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_copyright");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Copyright;

   ---------------------
   -- Get_Documenters --
   ---------------------

   function Get_Documenters
      (Self : not null access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_about_dialog_get_documenters");
   begin
      return To_String_List (Internal (Get_Object (Self)).all);
   end Get_Documenters;

   -----------------
   -- Get_License --
   -----------------

   function Get_License
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_license");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_License;

   ----------------------
   -- Get_License_Type --
   ----------------------

   function Get_License_Type
      (Self : not null access Gtk_About_Dialog_Record) return Gtk_License
   is
      function Internal (Self : System.Address) return Gtk_License;
      pragma Import (C, Internal, "gtk_about_dialog_get_license_type");
   begin
      return Internal (Get_Object (Self));
   end Get_License_Type;

   --------------
   -- Get_Logo --
   --------------

   function Get_Logo
      (Self : not null access Gtk_About_Dialog_Record)
       return Gdk.Paintable.Gdk_Paintable
   is
      function Internal
         (Self : System.Address) return Gdk.Paintable.Gdk_Paintable;
      pragma Import (C, Internal, "gtk_about_dialog_get_logo");
   begin
      return Internal (Get_Object (Self));
   end Get_Logo;

   ------------------------
   -- Get_Logo_Icon_Name --
   ------------------------

   function Get_Logo_Icon_Name
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_logo_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Logo_Icon_Name;

   ----------------------
   -- Get_Program_Name --
   ----------------------

   function Get_Program_Name
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_program_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Program_Name;

   ----------------------------
   -- Get_System_Information --
   ----------------------------

   function Get_System_Information
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_system_information");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_System_Information;

   ----------------------------
   -- Get_Translator_Credits --
   ----------------------------

   function Get_Translator_Credits
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_translator_credits");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Translator_Credits;

   -----------------
   -- Get_Version --
   -----------------

   function Get_Version
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_version");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Version;

   -----------------
   -- Get_Website --
   -----------------

   function Get_Website
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_website");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Website;

   -----------------------
   -- Get_Website_Label --
   -----------------------

   function Get_Website_Label
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_website_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Website_Label;

   ----------------------
   -- Get_Wrap_License --
   ----------------------

   function Get_Wrap_License
      (Self : not null access Gtk_About_Dialog_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_about_dialog_get_wrap_license");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Wrap_License;

   -----------------
   -- Set_Artists --
   -----------------

   procedure Set_Artists
      (Self    : not null access Gtk_About_Dialog_Record;
       Artists : GNAT.Strings.String_List)
   is
      procedure Internal
         (Self    : System.Address;
          Artists : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_about_dialog_set_artists");
      Tmp_Artists : Gtkada.Types.chars_ptr_array := From_String_List (Artists);
   begin
      Internal (Get_Object (Self), Tmp_Artists);
      Gtkada.Types.Free (Tmp_Artists);
   end Set_Artists;

   -----------------
   -- Set_Authors --
   -----------------

   procedure Set_Authors
      (Self    : not null access Gtk_About_Dialog_Record;
       Authors : GNAT.Strings.String_List)
   is
      procedure Internal
         (Self    : System.Address;
          Authors : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_about_dialog_set_authors");
      Tmp_Authors : Gtkada.Types.chars_ptr_array := From_String_List (Authors);
   begin
      Internal (Get_Object (Self), Tmp_Authors);
      Gtkada.Types.Free (Tmp_Authors);
   end Set_Authors;

   ------------------
   -- Set_Comments --
   ------------------

   procedure Set_Comments
      (Self     : not null access Gtk_About_Dialog_Record;
       Comments : UTF8_String := "")
   is
      procedure Internal
         (Self     : System.Address;
          Comments : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_comments");
      Tmp_Comments : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Comments :=
        (if Comments = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Comments));
      Internal (Get_Object (Self), Tmp_Comments);
      Free (Tmp_Comments);
   end Set_Comments;

   -------------------
   -- Set_Copyright --
   -------------------

   procedure Set_Copyright
      (Self      : not null access Gtk_About_Dialog_Record;
       Copyright : UTF8_String := "")
   is
      procedure Internal
         (Self      : System.Address;
          Copyright : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_copyright");
      Tmp_Copyright : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Copyright :=
        (if Copyright = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Copyright));
      Internal (Get_Object (Self), Tmp_Copyright);
      Free (Tmp_Copyright);
   end Set_Copyright;

   ---------------------
   -- Set_Documenters --
   ---------------------

   procedure Set_Documenters
      (Self        : not null access Gtk_About_Dialog_Record;
       Documenters : GNAT.Strings.String_List)
   is
      procedure Internal
         (Self        : System.Address;
          Documenters : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_about_dialog_set_documenters");
      Tmp_Documenters : Gtkada.Types.chars_ptr_array := From_String_List (Documenters);
   begin
      Internal (Get_Object (Self), Tmp_Documenters);
      Gtkada.Types.Free (Tmp_Documenters);
   end Set_Documenters;

   -----------------
   -- Set_License --
   -----------------

   procedure Set_License
      (Self    : not null access Gtk_About_Dialog_Record;
       License : UTF8_String := "")
   is
      procedure Internal
         (Self    : System.Address;
          License : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_license");
      Tmp_License : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_License :=
        (if License = ""
         then Gtkada.Types.Null_Ptr
         else New_String (License));
      Internal (Get_Object (Self), Tmp_License);
      Free (Tmp_License);
   end Set_License;

   ----------------------
   -- Set_License_Type --
   ----------------------

   procedure Set_License_Type
      (Self         : not null access Gtk_About_Dialog_Record;
       License_Type : Gtk_License)
   is
      procedure Internal (Self : System.Address; License_Type : Gtk_License);
      pragma Import (C, Internal, "gtk_about_dialog_set_license_type");
   begin
      Internal (Get_Object (Self), License_Type);
   end Set_License_Type;

   --------------
   -- Set_Logo --
   --------------

   procedure Set_Logo
      (Self : not null access Gtk_About_Dialog_Record;
       Logo : Gdk.Paintable.Gdk_Paintable)
   is
      procedure Internal
         (Self : System.Address;
          Logo : Gdk.Paintable.Gdk_Paintable);
      pragma Import (C, Internal, "gtk_about_dialog_set_logo");
   begin
      Internal (Get_Object (Self), Logo);
   end Set_Logo;

   ------------------------
   -- Set_Logo_Icon_Name --
   ------------------------

   procedure Set_Logo_Icon_Name
      (Self      : not null access Gtk_About_Dialog_Record;
       Icon_Name : UTF8_String := "")
   is
      procedure Internal
         (Self      : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_logo_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Icon_Name :=
        (if Icon_Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Icon_Name));
      Internal (Get_Object (Self), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_Logo_Icon_Name;

   ----------------------
   -- Set_Program_Name --
   ----------------------

   procedure Set_Program_Name
      (Self : not null access Gtk_About_Dialog_Record;
       Name : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_program_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Name :=
        (if Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Name));
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Set_Program_Name;

   ----------------------------
   -- Set_System_Information --
   ----------------------------

   procedure Set_System_Information
      (Self               : not null access Gtk_About_Dialog_Record;
       System_Information : UTF8_String := "")
   is
      procedure Internal
         (Self               : System.Address;
          System_Information : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_system_information");
      Tmp_System_Information : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_System_Information :=
        (if System_Information = ""
         then Gtkada.Types.Null_Ptr
         else New_String (System_Information));
      Internal (Get_Object (Self), Tmp_System_Information);
      Free (Tmp_System_Information);
   end Set_System_Information;

   ----------------------------
   -- Set_Translator_Credits --
   ----------------------------

   procedure Set_Translator_Credits
      (Self               : not null access Gtk_About_Dialog_Record;
       Translator_Credits : UTF8_String := "")
   is
      procedure Internal
         (Self               : System.Address;
          Translator_Credits : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_translator_credits");
      Tmp_Translator_Credits : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Translator_Credits :=
        (if Translator_Credits = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Translator_Credits));
      Internal (Get_Object (Self), Tmp_Translator_Credits);
      Free (Tmp_Translator_Credits);
   end Set_Translator_Credits;

   -----------------
   -- Set_Version --
   -----------------

   procedure Set_Version
      (Self    : not null access Gtk_About_Dialog_Record;
       Version : UTF8_String := "")
   is
      procedure Internal
         (Self    : System.Address;
          Version : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_version");
      Tmp_Version : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Version :=
        (if Version = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Version));
      Internal (Get_Object (Self), Tmp_Version);
      Free (Tmp_Version);
   end Set_Version;

   -----------------
   -- Set_Website --
   -----------------

   procedure Set_Website
      (Self    : not null access Gtk_About_Dialog_Record;
       Website : UTF8_String := "")
   is
      procedure Internal
         (Self    : System.Address;
          Website : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_website");
      Tmp_Website : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Website :=
        (if Website = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Website));
      Internal (Get_Object (Self), Tmp_Website);
      Free (Tmp_Website);
   end Set_Website;

   -----------------------
   -- Set_Website_Label --
   -----------------------

   procedure Set_Website_Label
      (Self          : not null access Gtk_About_Dialog_Record;
       Website_Label : UTF8_String)
   is
      procedure Internal
         (Self          : System.Address;
          Website_Label : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_website_label");
      Tmp_Website_Label : Gtkada.Types.Chars_Ptr := New_String (Website_Label);
   begin
      Internal (Get_Object (Self), Tmp_Website_Label);
      Free (Tmp_Website_Label);
   end Set_Website_Label;

   ----------------------
   -- Set_Wrap_License --
   ----------------------

   procedure Set_Wrap_License
      (Self         : not null access Gtk_About_Dialog_Record;
       Wrap_License : Boolean)
   is
      procedure Internal
         (Self         : System.Address;
          Wrap_License : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_about_dialog_set_wrap_license");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Wrap_License));
   end Set_Wrap_License;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_About_Dialog_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority)
   is
      procedure Internal
         (Self     : System.Address;
          Message  : Gtkada.Types.Chars_Ptr;
          Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);
      pragma Import (C, Internal, "gtk_accessible_announce");
      Tmp_Message : Gtkada.Types.Chars_Ptr := New_String (Message);
   begin
      Internal (Get_Object (Self), Tmp_Message, Priority);
      Free (Tmp_Message);
   end Announce;

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_id");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Accessible_Id;

   ---------------------------
   -- Get_Accessible_Parent --
   ---------------------------

   function Get_Accessible_Parent
      (Self : not null access Gtk_About_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_parent");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Parent;

   -------------------------
   -- Get_Accessible_Role --
   -------------------------

   function Get_Accessible_Role
      (Self : not null access Gtk_About_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible_Role
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible_Role;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_role");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Role;

   --------------------
   -- Get_At_Context --
   --------------------

   function Get_At_Context
      (Self : not null access Gtk_About_Dialog_Record)
       return Gtk.Atcontext.Gtk_Atcontext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accessible_get_at_context");
      Stub_Gtk_Atcontext : Gtk.Atcontext.Gtk_Atcontext_Record;
   begin
      return Gtk.Atcontext.Gtk_Atcontext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Atcontext));
   end Get_At_Context;

   ----------------
   -- Get_Bounds --
   ----------------

   function Get_Bounds
      (Self   : not null access Gtk_About_Dialog_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Acc_X      : access Glib.Gint;
          Acc_Y      : access Glib.Gint;
          Acc_Width  : access Glib.Gint;
          Acc_Height : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_bounds");
      Acc_X      : aliased Glib.Gint;
      Acc_Y      : aliased Glib.Gint;
      Acc_Width  : aliased Glib.Gint;
      Acc_Height : aliased Glib.Gint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_X'Access, Acc_Y'Access, Acc_Width'Access, Acc_Height'Access);
      X := Acc_X;
      Y := Acc_Y;
      Width := Acc_Width;
      Height := Acc_Height;
      return Tmp_Return /= 0;
   end Get_Bounds;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gtk_About_Dialog_Record)
       return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_root_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_About_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   ---------------
   -- Get_Focus --
   ---------------

   function Get_Focus
      (Self : not null access Gtk_About_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_root_get_focus");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Focus;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_About_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_next_accessible_sibling");
   begin
      return Internal (Get_Object (Self));
   end Get_Next_Accessible_Sibling;

   ------------------------
   -- Get_Platform_State --
   ------------------------

   function Get_Platform_State
      (Self  : not null access Gtk_About_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_platform_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Get_Platform_State;

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface
      (Self : not null access Gtk_About_Dialog_Record)
       return Gdk.Gdk_Surface
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_native_get_surface");
      Stub_Gdk_Surface : Gdk.Surface.Gdk_Surface_Record;
   begin
      return Gdk.Gdk_Surface (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Surface));
   end Get_Surface;

   ---------------------------
   -- Get_Surface_Transform --
   ---------------------------

   procedure Get_Surface_Transform
      (Self : not null access Gtk_About_Dialog_Record;
       X    : out Gdouble;
       Y    : out Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          X    : out Gdouble;
          Y    : out Gdouble);
      pragma Import (C, Internal, "gtk_native_get_surface_transform");
   begin
      Internal (Get_Object (Self), X, Y);
   end Get_Surface_Transform;

   -------------
   -- Realize --
   -------------

   procedure Realize (Self : not null access Gtk_About_Dialog_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_realize");
   begin
      Internal (Get_Object (Self));
   end Realize;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_About_Dialog_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtk.Accessible.Gtk_Accessible_Property);
      pragma Import (C, Internal, "gtk_accessible_reset_property");
   begin
      Internal (Get_Object (Self), Property);
   end Reset_Property;

   --------------------
   -- Reset_Relation --
   --------------------

   procedure Reset_Relation
      (Self     : not null access Gtk_About_Dialog_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation)
   is
      procedure Internal
         (Self     : System.Address;
          Relation : Gtk.Accessible.Gtk_Accessible_Relation);
      pragma Import (C, Internal, "gtk_accessible_reset_relation");
   begin
      Internal (Get_Object (Self), Relation);
   end Reset_Relation;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State
      (Self  : not null access Gtk_About_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_State);
      pragma Import (C, Internal, "gtk_accessible_reset_state");
   begin
      Internal (Get_Object (Self), State);
   end Reset_State;

   ---------------------------
   -- Set_Accessible_Parent --
   ---------------------------

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_About_Dialog_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self         : System.Address;
          Parent       : Gtk.Accessible.Gtk_Accessible;
          Next_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_set_accessible_parent");
   begin
      Internal (Get_Object (Self), Parent, Next_Sibling);
   end Set_Accessible_Parent;

   ---------------
   -- Set_Focus --
   ---------------

   procedure Set_Focus
      (Self  : not null access Gtk_About_Dialog_Record;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Focus : System.Address);
      pragma Import (C, Internal, "gtk_root_set_focus");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Focus)));
   end Set_Focus;

   ---------------
   -- Unrealize --
   ---------------

   procedure Unrealize (Self : not null access Gtk_About_Dialog_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_unrealize");
   begin
      Internal (Get_Object (Self));
   end Unrealize;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_About_Dialog_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self        : System.Address;
          New_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_update_next_accessible_sibling");
   begin
      Internal (Get_Object (Self), New_Sibling);
   end Update_Next_Accessible_Sibling;

   ---------------------------
   -- Update_Platform_State --
   ---------------------------

   procedure Update_Platform_State
      (Self  : not null access Gtk_About_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

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
   exception
      when E : others => Process_Exception (E);
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
   exception
      when E : others => Process_Exception (E);
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
