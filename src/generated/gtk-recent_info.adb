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
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;

package body Gtk.Recent_Info is

   function From_Object_Free
     (B : access Gtk_Recent_Info'Class) return Gtk_Recent_Info
   is
      Result : constant Gtk_Recent_Info := Gtk_Recent_Info (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Recent_Info is
      S : Gtk_Recent_Info;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   ---------------------
   -- Create_App_Info --
   ---------------------

   function Create_App_Info
      (Self     : Gtk_Recent_Info;
       App_Name : UTF8_String := "") return Glib.GApp_Info
   is
      function Internal
         (Self     : System.Address;
          App_Name : Gtkada.Types.Chars_Ptr) return Glib.GApp_Info;
      pragma Import (C, Internal, "gtk_recent_info_create_app_info");
      Tmp_App_Name : Gtkada.Types.Chars_Ptr;
      Tmp_Return   : Glib.GApp_Info;
   begin
      if App_Name = "" then
         Tmp_App_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_App_Name := New_String (App_Name);
      end if;
      Tmp_Return := Internal (Get_Object (Self), Tmp_App_Name);
      Free (Tmp_App_Name);
      return Tmp_Return;
   end Create_App_Info;

   ------------
   -- Exists --
   ------------

   function Exists (Self : Gtk_Recent_Info) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_exists");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Exists;

   ---------------
   -- Get_Added --
   ---------------

   function Get_Added (Self : Gtk_Recent_Info) return time_t is
      function Internal (Self : System.Address) return time_t;
      pragma Import (C, Internal, "gtk_recent_info_get_added");
   begin
      return Internal (Get_Object (Self));
   end Get_Added;

   -------------
   -- Get_Age --
   -------------

   function Get_Age (Self : Gtk_Recent_Info) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_recent_info_get_age");
   begin
      return Internal (Get_Object (Self));
   end Get_Age;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description (Self : Gtk_Recent_Info) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_description");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Description;

   ----------------------
   -- Get_Display_Name --
   ----------------------

   function Get_Display_Name (Self : Gtk_Recent_Info) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_display_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Display_Name;

   ---------------
   -- Get_Gicon --
   ---------------

   function Get_Gicon (Self : Gtk_Recent_Info) return Glib.G_Icon.G_Icon is
      function Internal (Self : System.Address) return Glib.G_Icon.G_Icon;
      pragma Import (C, Internal, "gtk_recent_info_get_gicon");
   begin
      return Internal (Get_Object (Self));
   end Get_Gicon;

   --------------
   -- Get_Icon --
   --------------

   function Get_Icon
      (Self : Gtk_Recent_Info;
       Size : Glib.Gint) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Self : System.Address;
          Size : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_recent_info_get_icon");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Self), Size), Stub_Gdk_Pixbuf));
   end Get_Icon;

   -------------------
   -- Get_Mime_Type --
   -------------------

   function Get_Mime_Type (Self : Gtk_Recent_Info) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_mime_type");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Mime_Type;

   ------------------
   -- Get_Modified --
   ------------------

   function Get_Modified (Self : Gtk_Recent_Info) return time_t is
      function Internal (Self : System.Address) return time_t;
      pragma Import (C, Internal, "gtk_recent_info_get_modified");
   begin
      return Internal (Get_Object (Self));
   end Get_Modified;

   ----------------------
   -- Get_Private_Hint --
   ----------------------

   function Get_Private_Hint (Self : Gtk_Recent_Info) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_get_private_hint");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Private_Hint;

   --------------------
   -- Get_Short_Name --
   --------------------

   function Get_Short_Name (Self : Gtk_Recent_Info) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_short_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Short_Name;

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri (Self : Gtk_Recent_Info) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_uri");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Uri;

   ---------------------
   -- Get_Uri_Display --
   ---------------------

   function Get_Uri_Display (Self : Gtk_Recent_Info) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_uri_display");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Uri_Display;

   -----------------
   -- Get_Visited --
   -----------------

   function Get_Visited (Self : Gtk_Recent_Info) return time_t is
      function Internal (Self : System.Address) return time_t;
      pragma Import (C, Internal, "gtk_recent_info_get_visited");
   begin
      return Internal (Get_Object (Self));
   end Get_Visited;

   ---------------------
   -- Has_Application --
   ---------------------

   function Has_Application
      (Self     : Gtk_Recent_Info;
       App_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self     : System.Address;
          App_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_has_application");
      Tmp_App_Name : Gtkada.Types.Chars_Ptr := New_String (App_Name);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_App_Name);
      Free (Tmp_App_Name);
      return Tmp_Return /= 0;
   end Has_Application;

   ---------------
   -- Has_Group --
   ---------------

   function Has_Group
      (Self       : Gtk_Recent_Info;
       Group_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Group_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_has_group");
      Tmp_Group_Name : Gtkada.Types.Chars_Ptr := New_String (Group_Name);
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Group_Name);
      Free (Tmp_Group_Name);
      return Tmp_Return /= 0;
   end Has_Group;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (Self : Gtk_Recent_Info) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_is_local");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Local;

   ----------------------
   -- Last_Application --
   ----------------------

   function Last_Application (Self : Gtk_Recent_Info) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_last_application");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Last_Application;

   -----------
   -- Match --
   -----------

   function Match
      (Self   : Gtk_Recent_Info;
       Info_B : Gtk_Recent_Info) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Info_B : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_match");
   begin
      return Internal (Get_Object (Self), Get_Object (Info_B)) /= 0;
   end Match;

   ---------
   -- Ref --
   ---------

   procedure Ref (Self : Gtk_Recent_Info) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_recent_info_ref");
   begin
      Internal (Get_Object (Self));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gtk_Recent_Info) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_recent_info_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gtk.Recent_Info;
