------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with Gtkada.Bindings;      use Gtkada.Bindings;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gtk.File_Chooser is

   ----------------
   -- Add_Filter --
   ----------------

   procedure Add_Filter
      (Chooser : Gtk_File_Chooser;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class)
   is
      procedure Internal
         (Chooser : Gtk_File_Chooser;
          Filter  : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_add_filter");
   begin
      Internal (Chooser, Get_Object (Filter));
   end Add_Filter;

   -------------------------
   -- Add_Shortcut_Folder --
   -------------------------

   function Add_Shortcut_Folder
      (Chooser : Gtk_File_Chooser;
       Folder  : UTF8_String) return Boolean
   is
      function Internal
         (Chooser : Gtk_File_Chooser;
          Folder  : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_add_shortcut_folder");
      Tmp_Folder : Interfaces.C.Strings.chars_ptr := New_String (Folder);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Chooser, Tmp_Folder);
      Free (Tmp_Folder);
      return Boolean'Val (Tmp_Return);
   end Add_Shortcut_Folder;

   -----------------------------
   -- Add_Shortcut_Folder_Uri --
   -----------------------------

   function Add_Shortcut_Folder_Uri
      (Chooser : Gtk_File_Chooser;
       URI     : UTF8_String) return Boolean
   is
      function Internal
         (Chooser : Gtk_File_Chooser;
          URI     : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_add_shortcut_folder_uri");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Chooser, Tmp_URI);
      Free (Tmp_URI);
      return Boolean'Val (Tmp_Return);
   end Add_Shortcut_Folder_Uri;

   ------------------------
   -- Get_Create_Folders --
   ------------------------

   function Get_Create_Folders (Chooser : Gtk_File_Chooser) return Boolean is
      function Internal (Chooser : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_create_folders");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Create_Folders;

   ------------------------
   -- Get_Current_Folder --
   ------------------------

   function Get_Current_Folder
      (Chooser : Gtk_File_Chooser) return UTF8_String
   is
      function Internal
         (Chooser : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_current_folder");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Chooser));
   end Get_Current_Folder;

   ----------------------------
   -- Get_Current_Folder_Uri --
   ----------------------------

   function Get_Current_Folder_Uri
      (Chooser : Gtk_File_Chooser) return UTF8_String
   is
      function Internal
         (Chooser : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_current_folder_uri");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Chooser));
   end Get_Current_Folder_Uri;

   -----------------------------------
   -- Get_Do_Overwrite_Confirmation --
   -----------------------------------

   function Get_Do_Overwrite_Confirmation
      (Chooser : Gtk_File_Chooser) return Boolean
   is
      function Internal (Chooser : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_do_overwrite_confirmation");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Do_Overwrite_Confirmation;

   ----------------------
   -- Get_Extra_Widget --
   ----------------------

   function Get_Extra_Widget
      (Chooser : Gtk_File_Chooser) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_extra_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Chooser), Stub_Gtk_Widget));
   end Get_Extra_Widget;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Chooser : Gtk_File_Chooser) return UTF8_String is
      function Internal
         (Chooser : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_filename");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Chooser));
   end Get_Filename;

   -------------------
   -- Get_Filenames --
   -------------------

   function Get_Filenames
      (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_filenames");
      Tmp_Return : Gtk.Enums.String_SList.GSlist;
   begin
      Gtk.Enums.String_SList.Set_Object (Tmp_Return, Internal (Chooser));
      return Tmp_Return;
   end Get_Filenames;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter
      (Chooser : Gtk_File_Chooser) return Gtk.File_Filter.Gtk_File_Filter
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_filter");
      Stub_Gtk_File_Filter : Gtk.File_Filter.Gtk_File_Filter_Record;
   begin
      return Gtk.File_Filter.Gtk_File_Filter (Get_User_Data (Internal (Chooser), Stub_Gtk_File_Filter));
   end Get_Filter;

   --------------------
   -- Get_Local_Only --
   --------------------

   function Get_Local_Only (Chooser : Gtk_File_Chooser) return Boolean is
      function Internal (Chooser : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_local_only");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Local_Only;

   --------------------------
   -- Get_Preview_Filename --
   --------------------------

   function Get_Preview_Filename
      (Chooser : Gtk_File_Chooser) return UTF8_String
   is
      function Internal
         (Chooser : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_preview_filename");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Chooser));
   end Get_Preview_Filename;

   ---------------------
   -- Get_Preview_Uri --
   ---------------------

   function Get_Preview_Uri (Chooser : Gtk_File_Chooser) return UTF8_String is
      function Internal
         (Chooser : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_preview_uri");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Chooser));
   end Get_Preview_Uri;

   ------------------------
   -- Get_Preview_Widget --
   ------------------------

   function Get_Preview_Widget
      (Chooser : Gtk_File_Chooser) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_preview_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Chooser), Stub_Gtk_Widget));
   end Get_Preview_Widget;

   -------------------------------
   -- Get_Preview_Widget_Active --
   -------------------------------

   function Get_Preview_Widget_Active
      (Chooser : Gtk_File_Chooser) return Boolean
   is
      function Internal (Chooser : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_preview_widget_active");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Preview_Widget_Active;

   -------------------------
   -- Get_Select_Multiple --
   -------------------------

   function Get_Select_Multiple (Chooser : Gtk_File_Chooser) return Boolean is
      function Internal (Chooser : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_select_multiple");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Select_Multiple;

   ---------------------
   -- Get_Show_Hidden --
   ---------------------

   function Get_Show_Hidden (Chooser : Gtk_File_Chooser) return Boolean is
      function Internal (Chooser : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_show_hidden");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Show_Hidden;

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri (Chooser : Gtk_File_Chooser) return UTF8_String is
      function Internal
         (Chooser : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_uri");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Chooser));
   end Get_Uri;

   --------------
   -- Get_Uris --
   --------------

   function Get_Uris
      (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_uris");
      Tmp_Return : Gtk.Enums.String_SList.GSlist;
   begin
      Gtk.Enums.String_SList.Set_Object (Tmp_Return, Internal (Chooser));
      return Tmp_Return;
   end Get_Uris;

   ---------------------------
   -- Get_Use_Preview_Label --
   ---------------------------

   function Get_Use_Preview_Label
      (Chooser : Gtk_File_Chooser) return Boolean
   is
      function Internal (Chooser : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_use_preview_label");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Use_Preview_Label;

   ------------------
   -- List_Filters --
   ------------------

   function List_Filters
      (Chooser : Gtk_File_Chooser) return Glib.Object.Object_List.GSlist
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_list_filters");
      Tmp_Return : Glib.Object.Object_List.GSlist;
   begin
      Glib.Object.Object_List.Set_Object (Tmp_Return, Internal (Chooser));
      return Tmp_Return;
   end List_Filters;

   -------------------------------
   -- List_Shortcut_Folder_Uris --
   -------------------------------

   function List_Shortcut_Folder_Uris
      (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_list_shortcut_folder_uris");
      Tmp_Return : Gtk.Enums.String_SList.GSlist;
   begin
      Gtk.Enums.String_SList.Set_Object (Tmp_Return, Internal (Chooser));
      return Tmp_Return;
   end List_Shortcut_Folder_Uris;

   ---------------------------
   -- List_Shortcut_Folders --
   ---------------------------

   function List_Shortcut_Folders
      (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_list_shortcut_folders");
      Tmp_Return : Gtk.Enums.String_SList.GSlist;
   begin
      Gtk.Enums.String_SList.Set_Object (Tmp_Return, Internal (Chooser));
      return Tmp_Return;
   end List_Shortcut_Folders;

   -------------------
   -- Remove_Filter --
   -------------------

   procedure Remove_Filter
      (Chooser : Gtk_File_Chooser;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class)
   is
      procedure Internal
         (Chooser : Gtk_File_Chooser;
          Filter  : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_remove_filter");
   begin
      Internal (Chooser, Get_Object (Filter));
   end Remove_Filter;

   ----------------------------
   -- Remove_Shortcut_Folder --
   ----------------------------

   function Remove_Shortcut_Folder
      (Chooser : Gtk_File_Chooser;
       Folder  : UTF8_String) return Boolean
   is
      function Internal
         (Chooser : Gtk_File_Chooser;
          Folder  : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_remove_shortcut_folder");
      Tmp_Folder : Interfaces.C.Strings.chars_ptr := New_String (Folder);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Chooser, Tmp_Folder);
      Free (Tmp_Folder);
      return Boolean'Val (Tmp_Return);
   end Remove_Shortcut_Folder;

   --------------------------------
   -- Remove_Shortcut_Folder_Uri --
   --------------------------------

   function Remove_Shortcut_Folder_Uri
      (Chooser : Gtk_File_Chooser;
       URI     : UTF8_String) return Boolean
   is
      function Internal
         (Chooser : Gtk_File_Chooser;
          URI     : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_remove_shortcut_folder_uri");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Chooser, Tmp_URI);
      Free (Tmp_URI);
      return Boolean'Val (Tmp_Return);
   end Remove_Shortcut_Folder_Uri;

   ---------------------
   -- Select_Filename --
   ---------------------

   function Select_Filename
      (Chooser  : Gtk_File_Chooser;
       Filename : UTF8_String) return Boolean
   is
      function Internal
         (Chooser  : Gtk_File_Chooser;
          Filename : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_select_filename");
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
      Tmp_Return   : Integer;
   begin
      Tmp_Return := Internal (Chooser, Tmp_Filename);
      Free (Tmp_Filename);
      return Boolean'Val (Tmp_Return);
   end Select_Filename;

   ----------------
   -- Select_Uri --
   ----------------

   function Select_Uri
      (Chooser : Gtk_File_Chooser;
       URI     : UTF8_String) return Boolean
   is
      function Internal
         (Chooser : Gtk_File_Chooser;
          URI     : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_select_uri");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Chooser, Tmp_URI);
      Free (Tmp_URI);
      return Boolean'Val (Tmp_Return);
   end Select_Uri;

   ------------------------
   -- Set_Create_Folders --
   ------------------------

   procedure Set_Create_Folders
      (Chooser        : Gtk_File_Chooser;
       Create_Folders : Boolean)
   is
      procedure Internal
         (Chooser        : Gtk_File_Chooser;
          Create_Folders : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_create_folders");
   begin
      Internal (Chooser, Boolean'Pos (Create_Folders));
   end Set_Create_Folders;

   ------------------------
   -- Set_Current_Folder --
   ------------------------

   function Set_Current_Folder
      (Chooser  : Gtk_File_Chooser;
       Filename : UTF8_String) return Boolean
   is
      function Internal
         (Chooser  : Gtk_File_Chooser;
          Filename : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_set_current_folder");
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
      Tmp_Return   : Integer;
   begin
      Tmp_Return := Internal (Chooser, Tmp_Filename);
      Free (Tmp_Filename);
      return Boolean'Val (Tmp_Return);
   end Set_Current_Folder;

   ----------------------------
   -- Set_Current_Folder_Uri --
   ----------------------------

   function Set_Current_Folder_Uri
      (Chooser : Gtk_File_Chooser;
       URI     : UTF8_String) return Boolean
   is
      function Internal
         (Chooser : Gtk_File_Chooser;
          URI     : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_set_current_folder_uri");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Chooser, Tmp_URI);
      Free (Tmp_URI);
      return Boolean'Val (Tmp_Return);
   end Set_Current_Folder_Uri;

   ----------------------
   -- Set_Current_Name --
   ----------------------

   procedure Set_Current_Name
      (Chooser : Gtk_File_Chooser;
       Name    : UTF8_String)
   is
      procedure Internal
         (Chooser : Gtk_File_Chooser;
          Name    : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_file_chooser_set_current_name");
      Tmp_Name : Interfaces.C.Strings.chars_ptr := New_String (Name);
   begin
      Internal (Chooser, Tmp_Name);
      Free (Tmp_Name);
   end Set_Current_Name;

   -----------------------------------
   -- Set_Do_Overwrite_Confirmation --
   -----------------------------------

   procedure Set_Do_Overwrite_Confirmation
      (Chooser                   : Gtk_File_Chooser;
       Do_Overwrite_Confirmation : Boolean)
   is
      procedure Internal
         (Chooser                   : Gtk_File_Chooser;
          Do_Overwrite_Confirmation : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_do_overwrite_confirmation");
   begin
      Internal (Chooser, Boolean'Pos (Do_Overwrite_Confirmation));
   end Set_Do_Overwrite_Confirmation;

   ----------------------
   -- Set_Extra_Widget --
   ----------------------

   procedure Set_Extra_Widget
      (Chooser      : Gtk_File_Chooser;
       Extra_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Chooser      : Gtk_File_Chooser;
          Extra_Widget : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_set_extra_widget");
   begin
      Internal (Chooser, Get_Object (Extra_Widget));
   end Set_Extra_Widget;

   ------------------
   -- Set_Filename --
   ------------------

   function Set_Filename
      (Chooser  : Gtk_File_Chooser;
       Filename : UTF8_String) return Boolean
   is
      function Internal
         (Chooser  : Gtk_File_Chooser;
          Filename : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_set_filename");
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
      Tmp_Return   : Integer;
   begin
      Tmp_Return := Internal (Chooser, Tmp_Filename);
      Free (Tmp_Filename);
      return Boolean'Val (Tmp_Return);
   end Set_Filename;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
      (Chooser : Gtk_File_Chooser;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class)
   is
      procedure Internal
         (Chooser : Gtk_File_Chooser;
          Filter  : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_set_filter");
   begin
      Internal (Chooser, Get_Object (Filter));
   end Set_Filter;

   --------------------
   -- Set_Local_Only --
   --------------------

   procedure Set_Local_Only
      (Chooser    : Gtk_File_Chooser;
       Local_Only : Boolean)
   is
      procedure Internal (Chooser : Gtk_File_Chooser; Local_Only : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_local_only");
   begin
      Internal (Chooser, Boolean'Pos (Local_Only));
   end Set_Local_Only;

   ------------------------
   -- Set_Preview_Widget --
   ------------------------

   procedure Set_Preview_Widget
      (Chooser        : Gtk_File_Chooser;
       Preview_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Chooser        : Gtk_File_Chooser;
          Preview_Widget : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_set_preview_widget");
   begin
      Internal (Chooser, Get_Object (Preview_Widget));
   end Set_Preview_Widget;

   -------------------------------
   -- Set_Preview_Widget_Active --
   -------------------------------

   procedure Set_Preview_Widget_Active
      (Chooser : Gtk_File_Chooser;
       Active  : Boolean)
   is
      procedure Internal (Chooser : Gtk_File_Chooser; Active : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_preview_widget_active");
   begin
      Internal (Chooser, Boolean'Pos (Active));
   end Set_Preview_Widget_Active;

   -------------------------
   -- Set_Select_Multiple --
   -------------------------

   procedure Set_Select_Multiple
      (Chooser         : Gtk_File_Chooser;
       Select_Multiple : Boolean)
   is
      procedure Internal
         (Chooser         : Gtk_File_Chooser;
          Select_Multiple : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_select_multiple");
   begin
      Internal (Chooser, Boolean'Pos (Select_Multiple));
   end Set_Select_Multiple;

   ---------------------
   -- Set_Show_Hidden --
   ---------------------

   procedure Set_Show_Hidden
      (Chooser     : Gtk_File_Chooser;
       Show_Hidden : Boolean)
   is
      procedure Internal (Chooser : Gtk_File_Chooser; Show_Hidden : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_show_hidden");
   begin
      Internal (Chooser, Boolean'Pos (Show_Hidden));
   end Set_Show_Hidden;

   -------------
   -- Set_Uri --
   -------------

   function Set_Uri
      (Chooser : Gtk_File_Chooser;
       URI     : UTF8_String) return Boolean
   is
      function Internal
         (Chooser : Gtk_File_Chooser;
          URI     : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_set_uri");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Chooser, Tmp_URI);
      Free (Tmp_URI);
      return Boolean'Val (Tmp_Return);
   end Set_Uri;

   ---------------------------
   -- Set_Use_Preview_Label --
   ---------------------------

   procedure Set_Use_Preview_Label
      (Chooser   : Gtk_File_Chooser;
       Use_Label : Boolean)
   is
      procedure Internal (Chooser : Gtk_File_Chooser; Use_Label : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_use_preview_label");
   begin
      Internal (Chooser, Boolean'Pos (Use_Label));
   end Set_Use_Preview_Label;

   -----------------------
   -- Unselect_Filename --
   -----------------------

   procedure Unselect_Filename
      (Chooser  : Gtk_File_Chooser;
       Filename : UTF8_String)
   is
      procedure Internal
         (Chooser  : Gtk_File_Chooser;
          Filename : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_file_chooser_unselect_filename");
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
   begin
      Internal (Chooser, Tmp_Filename);
      Free (Tmp_Filename);
   end Unselect_Filename;

   ------------------
   -- Unselect_Uri --
   ------------------

   procedure Unselect_Uri (Chooser : Gtk_File_Chooser; URI : UTF8_String) is
      procedure Internal
         (Chooser : Gtk_File_Chooser;
          URI     : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_file_chooser_unselect_uri");
      Tmp_URI : Interfaces.C.Strings.chars_ptr := New_String (URI);
   begin
      Internal (Chooser, Tmp_URI);
      Free (Tmp_URI);
   end Unselect_Uri;

   --------------------------
   -- On_Confirm_Overwrite --
   --------------------------

   procedure On_Confirm_Overwrite
      (Self : Gtk_File_Chooser;
       Call : not null access function
         (Self : Gtk_File_Chooser)
          return Gtk_File_Chooser_Confirmation)
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Confirm_Overwrite;

   --------------------------
   -- On_Confirm_Overwrite --
   --------------------------

   procedure On_Confirm_Overwrite
      (Self : Gtk_File_Chooser;
       Call : not null access function
         (Self : access Glib.Object.GObject_Record'Class)
          return Gtk_File_Chooser_Confirmation;
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Confirm_Overwrite;

   -------------------------------
   -- On_Current_Folder_Changed --
   -------------------------------

   procedure On_Current_Folder_Changed
      (Self : Gtk_File_Chooser;
       Call : not null access procedure (Self : Gtk_File_Chooser))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Current_Folder_Changed;

   -------------------------------
   -- On_Current_Folder_Changed --
   -------------------------------

   procedure On_Current_Folder_Changed
      (Self : Gtk_File_Chooser;
       Call : not null access procedure
         (Self : access Glib.Object.GObject_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Current_Folder_Changed;

   -----------------------
   -- On_File_Activated --
   -----------------------

   procedure On_File_Activated
      (Self : Gtk_File_Chooser;
       Call : not null access procedure (Self : Gtk_File_Chooser))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_File_Activated;

   -----------------------
   -- On_File_Activated --
   -----------------------

   procedure On_File_Activated
      (Self : Gtk_File_Chooser;
       Call : not null access procedure
         (Self : access Glib.Object.GObject_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_File_Activated;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
      (Self : Gtk_File_Chooser;
       Call : not null access procedure (Self : Gtk_File_Chooser))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Selection_Changed;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
      (Self : Gtk_File_Chooser;
       Call : not null access procedure
         (Self : access Glib.Object.GObject_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Selection_Changed;

   -----------------------
   -- On_Update_Preview --
   -----------------------

   procedure On_Update_Preview
      (Self : Gtk_File_Chooser;
       Call : not null access procedure (Self : Gtk_File_Chooser))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Update_Preview;

   -----------------------
   -- On_Update_Preview --
   -----------------------

   procedure On_Update_Preview
      (Self : Gtk_File_Chooser;
       Call : not null access procedure
         (Self : access Glib.Object.GObject_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Update_Preview;

   function "+" (W : Gtk_File_Chooser) return Gtk_File_Chooser is
   begin
      return W;
   end "+";

end Gtk.File_Chooser;
