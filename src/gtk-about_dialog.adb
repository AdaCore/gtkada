-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2006-2009, AdaCore                  --
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

with Gdk.Pixbuf;            use Gdk.Pixbuf;
with Gtkada.Bindings;       use Gtkada.Bindings;
with Gtkada.Types;
with GNAT.Strings;          use GNAT.Strings;
with Interfaces.C.Strings;  use Interfaces.C, Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;

package body Gtk.About_Dialog is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_About_Dialog_Record);
   pragma Warnings (Off, Type_Conversion);

   -----------------
   -- Get_Artists --
   -----------------

   function Get_Artists
     (About : access Gtk_About_Dialog_Record) return String_List
   is
      function Internal
        (About : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_about_dialog_get_artists");
   begin
      --  Returned value owned by gtk+, and must not be freed
      return To_String_List (Internal (Get_Object (About)).all);
   end Get_Artists;

   -----------------
   -- Get_Authors --
   -----------------

   function Get_Authors
     (About : access Gtk_About_Dialog_Record) return String_List
   is
      function Internal
        (About : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_about_dialog_get_authors");
   begin
      --  Returned value owned by gtk+
      return To_String_List (Internal (Get_Object (About)).all);
   end Get_Authors;

   ------------------
   -- Get_Comments --
   ------------------

   function Get_Comments
     (About : access Gtk_About_Dialog_Record) return String
   is
      function Internal (About : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_comments");
   begin
      --  returned value owned by gtk+
      return Value (Internal (Get_Object (About)));
   end Get_Comments;

   -------------------
   -- Get_Copyright --
   -------------------

   function Get_Copyright
     (About : access Gtk_About_Dialog_Record) return String
   is
      function Internal (About : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_copyright");
   begin
      --  returned value owned by gtk+
      return Value (Internal (Get_Object (About)));
   end Get_Copyright;

   ---------------------
   -- Get_Documenters --
   ---------------------

   function Get_Documenters
     (About : access Gtk_About_Dialog_Record) return String_List
   is
      function Internal
        (About : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_about_dialog_get_documenters");
   begin
      --  Returned value owned by gtk+
      return To_String_List (Internal (Get_Object (About)).all);
   end Get_Documenters;

   -----------------
   -- Get_License --
   -----------------

   function Get_License
     (About : access Gtk_About_Dialog_Record) return String
   is
      function Internal (About : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_license");
   begin
      --  Returned value owned by gtk+
      return Value (Internal (Get_Object (About)));
   end Get_License;

   --------------
   -- Get_Logo --
   --------------

   function Get_Logo
     (About : access Gtk_About_Dialog_Record)
      return Gdk_Pixbuf
   is
      function Internal (About : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_about_dialog_get_logo");

   begin
      return Convert (Internal (Get_Object (About)));
   end Get_Logo;

   ------------------------
   -- Get_Logo_Icon_Name --
   ------------------------

   function Get_Logo_Icon_Name
     (About : access Gtk_About_Dialog_Record) return String
   is
      function Internal (About : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_logo_icon_name");
   begin
      --  Returned value owned by gtk+
      return Value (Internal (Get_Object (About)));
   end Get_Logo_Icon_Name;

   -----------------------
   -- Get_Proogram_Name --
   -----------------------

   function Get_Program_Name
     (About : access Gtk_About_Dialog_Record) return String
   is
      function Internal (About : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_program_name");
   begin
      --  Returned value owned by gtk+
      return Value (Internal (Get_Object (About)));
   end Get_Program_Name;

   ----------------------------
   -- Get_Translator_Credits --
   ----------------------------

   function Get_Translator_Credits
     (About : access Gtk_About_Dialog_Record) return String
   is
      function Internal (About : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_translator_credits");
   begin
      --  Returned value owned by gtk+
      return Value (Internal (Get_Object (About)));
   end Get_Translator_Credits;

   -----------------
   -- Get_Version --
   -----------------

   function Get_Version
     (About : access Gtk_About_Dialog_Record) return String
   is
      function Internal (About : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_version");
   begin
      --  Returned value owned by gtk+
      return Value (Internal (Get_Object (About)));
   end Get_Version;

   -----------------
   -- Get_Website --
   -----------------

   function Get_Website
     (About : access Gtk_About_Dialog_Record) return String
   is
      function Internal (About : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_website");
   begin
      --  Returned value owned by gtk+
      return Value (Internal (Get_Object (About)));
   end Get_Website;

   -----------------------
   -- Get_Website_Label --
   -----------------------

   function Get_Website_Label
     (About : access Gtk_About_Dialog_Record) return String
   is
      function Internal (About : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_website_label");
   begin
      --  Returned value owned by gtk+
      return Value (Internal (Get_Object (About)));
   end Get_Website_Label;

   ----------------------
   -- Get_Wrap_License --
   ----------------------

   function Get_Wrap_License
     (About : access Gtk_About_Dialog_Record)
      return Boolean
   is
      function Internal
        (About : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_about_dialog_get_wrap_license");
   begin
      return Boolean'Val (Internal (Get_Object (About)));
   end Get_Wrap_License;

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
     (About : access Gtk_About_Dialog_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_about_dialog_new");
   begin
      Set_Object (About, Internal);
   end Initialize;

   -----------------
   -- Set_Artists --
   -----------------

   procedure Set_Artists
     (About   : access Gtk_About_Dialog_Record;
      Artists : String_List)
   is
      procedure Internal (About : System.Address; Artists : System.Address);
      pragma Import (C, Internal, "gtk_about_dialog_set_artists");
      Val : aliased chars_ptr_array := From_String_List (Artists);
   begin
      Internal (Get_Object (About), Val (Val'First)'Address);
      Gtkada.Types.Free (Val);
   end Set_Artists;

   -----------------
   -- Set_Authors --
   -----------------

   procedure Set_Authors
     (About   : access Gtk_About_Dialog_Record;
      Authors : String_List)
   is
      procedure Internal (About : System.Address; Authors : System.Address);
      pragma Import (C, Internal, "gtk_about_dialog_set_authors");
      Val : aliased chars_ptr_array := From_String_List (Authors);
   begin
      Internal (Get_Object (About), Val (Val'First)'Address);
      Gtkada.Types.Free (Val);
   end Set_Authors;

   ------------------
   -- Set_Comments --
   ------------------

   procedure Set_Comments
     (About    : access Gtk_About_Dialog_Record;
      Comments : String)
   is
      procedure Internal
        (About    : System.Address;
         Comments : String);
      pragma Import (C, Internal, "gtk_about_dialog_set_comments");
   begin
      Internal (Get_Object (About), Comments & ASCII.NUL);
   end Set_Comments;

   -------------------
   -- Set_Copyright --
   -------------------

   procedure Set_Copyright
     (About     : access Gtk_About_Dialog_Record;
      Copyright : String)
   is
      procedure Internal
        (About     : System.Address;
         Copyright : String);
      pragma Import (C, Internal, "gtk_about_dialog_set_copyright");
   begin
      Internal (Get_Object (About), Copyright & ASCII.NUL);
   end Set_Copyright;

   ---------------------
   -- Set_Documenters --
   ---------------------

   procedure Set_Documenters
     (About : access Gtk_About_Dialog_Record;
      Documenters : String_List)
   is
      procedure Internal (About, Documenters : System.Address);
      pragma Import (C, Internal, "gtk_about_dialog_set_documenters");
      Val : aliased chars_ptr_array := From_String_List (Documenters);
   begin
      Internal (Get_Object (About), Val (Val'First)'Address);
      Gtkada.Types.Free (Val);
   end Set_Documenters;

   -----------------
   -- Set_License --
   -----------------

   procedure Set_License
     (About   : access Gtk_About_Dialog_Record;
      License : String)
   is
      procedure Internal
        (About   : System.Address;
         License : String);
      pragma Import (C, Internal, "gtk_about_dialog_set_license");
   begin
      Internal (Get_Object (About), License & ASCII.NUL);
   end Set_License;

   --------------
   -- Set_Logo --
   --------------

   procedure Set_Logo
     (About : access Gtk_About_Dialog_Record;
      Logo  : Gdk_Pixbuf)
   is
      procedure Internal
        (About : System.Address;
         Logo  : System.Address);
      pragma Import (C, Internal, "gtk_about_dialog_set_logo");
   begin
      Internal (Get_Object (About), Get_Object (Logo));
   end Set_Logo;

   ------------------------
   -- Set_Logo_Icon_Name --
   ------------------------

   procedure Set_Logo_Icon_Name
     (About     : access Gtk_About_Dialog_Record;
      Icon_Name : String := "")
   is
      procedure Internal (About : System.Address; Icon_Name : chars_ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_logo_icon_name");
      Str : chars_ptr := String_Or_Null (Icon_Name);
   begin
      Internal (Get_Object (About), Str);
      Free (Str);
   end Set_Logo_Icon_Name;

   ----------------------
   -- Set_Program_Name --
   ----------------------

   procedure Set_Program_Name
     (About : access Gtk_About_Dialog_Record;
      Name  : String)
   is
      procedure Internal
        (About : System.Address;
         Name  : String);
      pragma Import (C, Internal, "gtk_about_dialog_set_program_name");
   begin
      Internal (Get_Object (About), Name & ASCII.NUL);
   end Set_Program_Name;

   ----------------------------
   -- Set_Translator_Credits --
   ----------------------------

   procedure Set_Translator_Credits
     (About              : access Gtk_About_Dialog_Record;
      Translator_Credits : String)
   is
      procedure Internal
        (About              : System.Address;
         Translator_Credits : String);
      pragma Import (C, Internal, "gtk_about_dialog_set_translator_credits");
   begin
      Internal (Get_Object (About), Translator_Credits & ASCII.NUL);
   end Set_Translator_Credits;

   -----------------
   -- Set_Version --
   -----------------

   procedure Set_Version
     (About   : access Gtk_About_Dialog_Record;
      Version : String)
   is
      procedure Internal
        (About   : System.Address;
         Version : String);
      pragma Import (C, Internal, "gtk_about_dialog_set_version");
   begin
      Internal (Get_Object (About), Version & ASCII.NUL);
   end Set_Version;

   -----------------
   -- Set_Website --
   -----------------

   procedure Set_Website
     (About   : access Gtk_About_Dialog_Record;
      Website : String)
   is
      procedure Internal
        (About   : System.Address;
         Website : String);
      pragma Import (C, Internal, "gtk_about_dialog_set_website");
   begin
      Internal (Get_Object (About), Website & ASCII.NUL);
   end Set_Website;

   -----------------------
   -- Set_Website_Label --
   -----------------------

   procedure Set_Website_Label
     (About         : access Gtk_About_Dialog_Record;
      Website_Label : String)
   is
      procedure Internal
        (About         : System.Address;
         Website_Label : String);
      pragma Import (C, Internal, "gtk_about_dialog_set_website_label");
   begin
      Internal (Get_Object (About), Website_Label & ASCII.NUL);
   end Set_Website_Label;

   ----------------------
   -- Set_Wrap_License --
   ----------------------

   procedure Set_Wrap_License
     (About        : access Gtk_About_Dialog_Record;
      Wrap_License : Boolean)
   is
      procedure Internal
        (About        : System.Address;
         Wrap_License : Gboolean);
      pragma Import (C, Internal, "gtk_about_dialog_set_wrap_license");
   begin
      Internal (Get_Object (About), Boolean'Pos (Wrap_License));
   end Set_Wrap_License;

end Gtk.About_Dialog;
