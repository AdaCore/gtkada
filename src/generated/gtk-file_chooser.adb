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
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gtk.File_Chooser is

   ----------------
   -- Add_Filter --
   ----------------

   procedure Add_Filter
      (Self   : Gtk_File_Chooser;
       Filter : access Gtk.File_Filter.Gtk_File_Filter_Record'Class)
   is
      procedure Internal (Self : Gtk_File_Chooser; Filter : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_add_filter");
   begin
      Internal (Self, Get_Object (Filter));
   end Add_Filter;

   -------------------------
   -- Add_Shortcut_Folder --
   -------------------------

   function Add_Shortcut_Folder
      (Self   : Gtk_File_Chooser;
       Folder : UTF8_String) return Boolean
   is
      function Internal
         (Self   : Gtk_File_Chooser;
          Folder : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_add_shortcut_folder");
      Tmp_Folder : Interfaces.C.Strings.chars_ptr := New_String (Folder);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Self, Tmp_Folder);
      Free (Tmp_Folder);
      return Boolean'Val (Tmp_Return);
   end Add_Shortcut_Folder;

   -----------------------------
   -- Add_Shortcut_Folder_Uri --
   -----------------------------

   function Add_Shortcut_Folder_Uri
      (Self : Gtk_File_Chooser;
       Uri  : UTF8_String) return Boolean
   is
      function Internal
         (Self : Gtk_File_Chooser;
          Uri  : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_add_shortcut_folder_uri");
      Tmp_Uri    : Interfaces.C.Strings.chars_ptr := New_String (Uri);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Self, Tmp_Uri);
      Free (Tmp_Uri);
      return Boolean'Val (Tmp_Return);
   end Add_Shortcut_Folder_Uri;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action
      (Self : Gtk_File_Chooser) return Gtk.Enums.Gtk_File_Chooser_Action
   is
      function Internal (Self : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_action");
   begin
      return Gtk.Enums.Gtk_File_Chooser_Action'Val (Internal (Self));
   end Get_Action;

   ------------------------
   -- Get_Create_Folders --
   ------------------------

   function Get_Create_Folders (Self : Gtk_File_Chooser) return Boolean is
      function Internal (Self : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_create_folders");
   begin
      return Boolean'Val (Internal (Self));
   end Get_Create_Folders;

   ------------------------
   -- Get_Current_Folder --
   ------------------------

   function Get_Current_Folder (Self : Gtk_File_Chooser) return UTF8_String is
      function Internal
         (Self : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_current_folder");
   begin
      return Interfaces.C.Strings.Value (Internal (Self));
   end Get_Current_Folder;

   ----------------------------
   -- Get_Current_Folder_Uri --
   ----------------------------

   function Get_Current_Folder_Uri
      (Self : Gtk_File_Chooser) return UTF8_String
   is
      function Internal
         (Self : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_current_folder_uri");
   begin
      return Interfaces.C.Strings.Value (Internal (Self));
   end Get_Current_Folder_Uri;

   -----------------------------------
   -- Get_Do_Overwrite_Confirmation --
   -----------------------------------

   function Get_Do_Overwrite_Confirmation
      (Self : Gtk_File_Chooser) return Boolean
   is
      function Internal (Self : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_do_overwrite_confirmation");
   begin
      return Boolean'Val (Internal (Self));
   end Get_Do_Overwrite_Confirmation;

   ----------------------
   -- Get_Extra_Widget --
   ----------------------

   function Get_Extra_Widget
      (Self : Gtk_File_Chooser) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_extra_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Self), Stub_Gtk_Widget));
   end Get_Extra_Widget;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Self : Gtk_File_Chooser) return UTF8_String is
      function Internal
         (Self : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_filename");
   begin
      return Interfaces.C.Strings.Value (Internal (Self));
   end Get_Filename;

   -------------------
   -- Get_Filenames --
   -------------------

   function Get_Filenames
      (Self : Gtk_File_Chooser) return Gtk.Enums.String_List.Glist
   is
      function Internal (Self : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_filenames");
      Tmp_Return : Gtk.Enums.String_List.Glist;
   begin
      Gtk.Enums.String_List.Set_Object (Tmp_Return, Internal (Self));
      return Tmp_Return;
   end Get_Filenames;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter
      (Self : Gtk_File_Chooser) return Gtk.File_Filter.Gtk_File_Filter
   is
      function Internal (Self : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_filter");
      Stub_Gtk_File_Filter : Gtk.File_Filter.Gtk_File_Filter_Record;
   begin
      return Gtk.File_Filter.Gtk_File_Filter (Get_User_Data (Internal (Self), Stub_Gtk_File_Filter));
   end Get_Filter;

   --------------------
   -- Get_Local_Only --
   --------------------

   function Get_Local_Only (Self : Gtk_File_Chooser) return Boolean is
      function Internal (Self : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_local_only");
   begin
      return Boolean'Val (Internal (Self));
   end Get_Local_Only;

   --------------------------
   -- Get_Preview_Filename --
   --------------------------

   function Get_Preview_Filename
      (Self : Gtk_File_Chooser) return UTF8_String
   is
      function Internal
         (Self : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_preview_filename");
   begin
      return Interfaces.C.Strings.Value (Internal (Self));
   end Get_Preview_Filename;

   ---------------------
   -- Get_Preview_Uri --
   ---------------------

   function Get_Preview_Uri (Self : Gtk_File_Chooser) return UTF8_String is
      function Internal
         (Self : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_preview_uri");
   begin
      return Interfaces.C.Strings.Value (Internal (Self));
   end Get_Preview_Uri;

   ------------------------
   -- Get_Preview_Widget --
   ------------------------

   function Get_Preview_Widget
      (Self : Gtk_File_Chooser) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_preview_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Self), Stub_Gtk_Widget));
   end Get_Preview_Widget;

   -------------------------------
   -- Get_Preview_Widget_Active --
   -------------------------------

   function Get_Preview_Widget_Active
      (Self : Gtk_File_Chooser) return Boolean
   is
      function Internal (Self : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_preview_widget_active");
   begin
      return Boolean'Val (Internal (Self));
   end Get_Preview_Widget_Active;

   -------------------------
   -- Get_Select_Multiple --
   -------------------------

   function Get_Select_Multiple (Self : Gtk_File_Chooser) return Boolean is
      function Internal (Self : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_select_multiple");
   begin
      return Boolean'Val (Internal (Self));
   end Get_Select_Multiple;

   ---------------------
   -- Get_Show_Hidden --
   ---------------------

   function Get_Show_Hidden (Self : Gtk_File_Chooser) return Boolean is
      function Internal (Self : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_show_hidden");
   begin
      return Boolean'Val (Internal (Self));
   end Get_Show_Hidden;

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri (Self : Gtk_File_Chooser) return UTF8_String is
      function Internal
         (Self : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_uri");
   begin
      return Interfaces.C.Strings.Value (Internal (Self));
   end Get_Uri;

   --------------
   -- Get_Uris --
   --------------

   function Get_Uris
      (Self : Gtk_File_Chooser) return Gtk.Enums.String_List.Glist
   is
      function Internal (Self : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_uris");
      Tmp_Return : Gtk.Enums.String_List.Glist;
   begin
      Gtk.Enums.String_List.Set_Object (Tmp_Return, Internal (Self));
      return Tmp_Return;
   end Get_Uris;

   ---------------------------
   -- Get_Use_Preview_Label --
   ---------------------------

   function Get_Use_Preview_Label (Self : Gtk_File_Chooser) return Boolean is
      function Internal (Self : Gtk_File_Chooser) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_get_use_preview_label");
   begin
      return Boolean'Val (Internal (Self));
   end Get_Use_Preview_Label;

   ------------------
   -- List_Filters --
   ------------------

   function List_Filters
      (Self : Gtk_File_Chooser)
       return Gtk.File_Filter.File_Filter_SList.GSlist
   is
      function Internal (Self : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_list_filters");
      Tmp_Return : Gtk.File_Filter.File_Filter_SList.GSlist;
   begin
      Gtk.File_Filter.File_Filter_SList.Set_Object (Tmp_Return, Internal (Self));
      return Tmp_Return;
   end List_Filters;

   -------------------------------
   -- List_Shortcut_Folder_Uris --
   -------------------------------

   function List_Shortcut_Folder_Uris
      (Self : Gtk_File_Chooser) return Gtk.Enums.String_List.Glist
   is
      function Internal (Self : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_list_shortcut_folder_uris");
      Tmp_Return : Gtk.Enums.String_List.Glist;
   begin
      Gtk.Enums.String_List.Set_Object (Tmp_Return, Internal (Self));
      return Tmp_Return;
   end List_Shortcut_Folder_Uris;

   ---------------------------
   -- List_Shortcut_Folders --
   ---------------------------

   function List_Shortcut_Folders
      (Self : Gtk_File_Chooser) return Gtk.Enums.String_List.Glist
   is
      function Internal (Self : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_list_shortcut_folders");
      Tmp_Return : Gtk.Enums.String_List.Glist;
   begin
      Gtk.Enums.String_List.Set_Object (Tmp_Return, Internal (Self));
      return Tmp_Return;
   end List_Shortcut_Folders;

   -------------------
   -- Remove_Filter --
   -------------------

   procedure Remove_Filter
      (Self   : Gtk_File_Chooser;
       Filter : access Gtk.File_Filter.Gtk_File_Filter_Record'Class)
   is
      procedure Internal (Self : Gtk_File_Chooser; Filter : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_remove_filter");
   begin
      Internal (Self, Get_Object (Filter));
   end Remove_Filter;

   ----------------------------
   -- Remove_Shortcut_Folder --
   ----------------------------

   function Remove_Shortcut_Folder
      (Self   : Gtk_File_Chooser;
       Folder : UTF8_String) return Boolean
   is
      function Internal
         (Self   : Gtk_File_Chooser;
          Folder : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_remove_shortcut_folder");
      Tmp_Folder : Interfaces.C.Strings.chars_ptr := New_String (Folder);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Self, Tmp_Folder);
      Free (Tmp_Folder);
      return Boolean'Val (Tmp_Return);
   end Remove_Shortcut_Folder;

   --------------------------------
   -- Remove_Shortcut_Folder_Uri --
   --------------------------------

   function Remove_Shortcut_Folder_Uri
      (Self : Gtk_File_Chooser;
       Uri  : UTF8_String) return Boolean
   is
      function Internal
         (Self : Gtk_File_Chooser;
          Uri  : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_remove_shortcut_folder_uri");
      Tmp_Uri    : Interfaces.C.Strings.chars_ptr := New_String (Uri);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Self, Tmp_Uri);
      Free (Tmp_Uri);
      return Boolean'Val (Tmp_Return);
   end Remove_Shortcut_Folder_Uri;

   ---------------------
   -- Select_Filename --
   ---------------------

   function Select_Filename
      (Self     : Gtk_File_Chooser;
       Filename : UTF8_String) return Boolean
   is
      function Internal
         (Self     : Gtk_File_Chooser;
          Filename : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_select_filename");
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
      Tmp_Return   : Integer;
   begin
      Tmp_Return := Internal (Self, Tmp_Filename);
      Free (Tmp_Filename);
      return Boolean'Val (Tmp_Return);
   end Select_Filename;

   ----------------
   -- Select_Uri --
   ----------------

   function Select_Uri
      (Self : Gtk_File_Chooser;
       Uri  : UTF8_String) return Boolean
   is
      function Internal
         (Self : Gtk_File_Chooser;
          Uri  : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_select_uri");
      Tmp_Uri    : Interfaces.C.Strings.chars_ptr := New_String (Uri);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Self, Tmp_Uri);
      Free (Tmp_Uri);
      return Boolean'Val (Tmp_Return);
   end Select_Uri;

   ----------------
   -- Set_Action --
   ----------------

   procedure Set_Action
      (Self   : Gtk_File_Chooser;
       Action : Gtk.Enums.Gtk_File_Chooser_Action)
   is
      procedure Internal (Self : Gtk_File_Chooser; Action : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_action");
   begin
      Internal (Self, Gtk.Enums.Gtk_File_Chooser_Action'Pos (Action));
   end Set_Action;

   ------------------------
   -- Set_Create_Folders --
   ------------------------

   procedure Set_Create_Folders
      (Self           : Gtk_File_Chooser;
       Create_Folders : Boolean)
   is
      procedure Internal (Self : Gtk_File_Chooser; Create_Folders : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_create_folders");
   begin
      Internal (Self, Boolean'Pos (Create_Folders));
   end Set_Create_Folders;

   ------------------------
   -- Set_Current_Folder --
   ------------------------

   function Set_Current_Folder
      (Self     : Gtk_File_Chooser;
       Filename : UTF8_String) return Boolean
   is
      function Internal
         (Self     : Gtk_File_Chooser;
          Filename : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_set_current_folder");
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
      Tmp_Return   : Integer;
   begin
      Tmp_Return := Internal (Self, Tmp_Filename);
      Free (Tmp_Filename);
      return Boolean'Val (Tmp_Return);
   end Set_Current_Folder;

   ----------------------------
   -- Set_Current_Folder_Uri --
   ----------------------------

   function Set_Current_Folder_Uri
      (Self : Gtk_File_Chooser;
       Uri  : UTF8_String) return Boolean
   is
      function Internal
         (Self : Gtk_File_Chooser;
          Uri  : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_set_current_folder_uri");
      Tmp_Uri    : Interfaces.C.Strings.chars_ptr := New_String (Uri);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Self, Tmp_Uri);
      Free (Tmp_Uri);
      return Boolean'Val (Tmp_Return);
   end Set_Current_Folder_Uri;

   ----------------------
   -- Set_Current_Name --
   ----------------------

   procedure Set_Current_Name (Self : Gtk_File_Chooser; Name : UTF8_String) is
      procedure Internal
         (Self : Gtk_File_Chooser;
          Name : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_file_chooser_set_current_name");
      Tmp_Name : Interfaces.C.Strings.chars_ptr := New_String (Name);
   begin
      Internal (Self, Tmp_Name);
      Free (Tmp_Name);
   end Set_Current_Name;

   -----------------------------------
   -- Set_Do_Overwrite_Confirmation --
   -----------------------------------

   procedure Set_Do_Overwrite_Confirmation
      (Self                      : Gtk_File_Chooser;
       Do_Overwrite_Confirmation : Boolean)
   is
      procedure Internal
         (Self                      : Gtk_File_Chooser;
          Do_Overwrite_Confirmation : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_do_overwrite_confirmation");
   begin
      Internal (Self, Boolean'Pos (Do_Overwrite_Confirmation));
   end Set_Do_Overwrite_Confirmation;

   ----------------------
   -- Set_Extra_Widget --
   ----------------------

   procedure Set_Extra_Widget
      (Self         : Gtk_File_Chooser;
       Extra_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Self         : Gtk_File_Chooser;
          Extra_Widget : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_set_extra_widget");
   begin
      Internal (Self, Get_Object (Extra_Widget));
   end Set_Extra_Widget;

   ------------------
   -- Set_Filename --
   ------------------

   function Set_Filename
      (Self     : Gtk_File_Chooser;
       Filename : UTF8_String) return Boolean
   is
      function Internal
         (Self     : Gtk_File_Chooser;
          Filename : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_set_filename");
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
      Tmp_Return   : Integer;
   begin
      Tmp_Return := Internal (Self, Tmp_Filename);
      Free (Tmp_Filename);
      return Boolean'Val (Tmp_Return);
   end Set_Filename;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
      (Self   : Gtk_File_Chooser;
       Filter : access Gtk.File_Filter.Gtk_File_Filter_Record'Class)
   is
      procedure Internal (Self : Gtk_File_Chooser; Filter : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_set_filter");
   begin
      Internal (Self, Get_Object (Filter));
   end Set_Filter;

   --------------------
   -- Set_Local_Only --
   --------------------

   procedure Set_Local_Only (Self : Gtk_File_Chooser; Local_Only : Boolean) is
      procedure Internal (Self : Gtk_File_Chooser; Local_Only : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_local_only");
   begin
      Internal (Self, Boolean'Pos (Local_Only));
   end Set_Local_Only;

   ------------------------
   -- Set_Preview_Widget --
   ------------------------

   procedure Set_Preview_Widget
      (Self           : Gtk_File_Chooser;
       Preview_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Self           : Gtk_File_Chooser;
          Preview_Widget : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_set_preview_widget");
   begin
      Internal (Self, Get_Object (Preview_Widget));
   end Set_Preview_Widget;

   -------------------------------
   -- Set_Preview_Widget_Active --
   -------------------------------

   procedure Set_Preview_Widget_Active
      (Self   : Gtk_File_Chooser;
       Active : Boolean)
   is
      procedure Internal (Self : Gtk_File_Chooser; Active : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_preview_widget_active");
   begin
      Internal (Self, Boolean'Pos (Active));
   end Set_Preview_Widget_Active;

   -------------------------
   -- Set_Select_Multiple --
   -------------------------

   procedure Set_Select_Multiple
      (Self            : Gtk_File_Chooser;
       Select_Multiple : Boolean)
   is
      procedure Internal
         (Self            : Gtk_File_Chooser;
          Select_Multiple : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_select_multiple");
   begin
      Internal (Self, Boolean'Pos (Select_Multiple));
   end Set_Select_Multiple;

   ---------------------
   -- Set_Show_Hidden --
   ---------------------

   procedure Set_Show_Hidden
      (Self        : Gtk_File_Chooser;
       Show_Hidden : Boolean)
   is
      procedure Internal (Self : Gtk_File_Chooser; Show_Hidden : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_show_hidden");
   begin
      Internal (Self, Boolean'Pos (Show_Hidden));
   end Set_Show_Hidden;

   -------------
   -- Set_Uri --
   -------------

   function Set_Uri
      (Self : Gtk_File_Chooser;
       Uri  : UTF8_String) return Boolean
   is
      function Internal
         (Self : Gtk_File_Chooser;
          Uri  : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_file_chooser_set_uri");
      Tmp_Uri    : Interfaces.C.Strings.chars_ptr := New_String (Uri);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Self, Tmp_Uri);
      Free (Tmp_Uri);
      return Boolean'Val (Tmp_Return);
   end Set_Uri;

   ---------------------------
   -- Set_Use_Preview_Label --
   ---------------------------

   procedure Set_Use_Preview_Label
      (Self      : Gtk_File_Chooser;
       Use_Label : Boolean)
   is
      procedure Internal (Self : Gtk_File_Chooser; Use_Label : Integer);
      pragma Import (C, Internal, "gtk_file_chooser_set_use_preview_label");
   begin
      Internal (Self, Boolean'Pos (Use_Label));
   end Set_Use_Preview_Label;

   -----------------------
   -- Unselect_Filename --
   -----------------------

   procedure Unselect_Filename
      (Self     : Gtk_File_Chooser;
       Filename : UTF8_String)
   is
      procedure Internal
         (Self     : Gtk_File_Chooser;
          Filename : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_file_chooser_unselect_filename");
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
   begin
      Internal (Self, Tmp_Filename);
      Free (Tmp_Filename);
   end Unselect_Filename;

   ------------------
   -- Unselect_Uri --
   ------------------

   procedure Unselect_Uri (Self : Gtk_File_Chooser; Uri : UTF8_String) is
      procedure Internal
         (Self : Gtk_File_Chooser;
          Uri  : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_file_chooser_unselect_uri");
      Tmp_Uri : Interfaces.C.Strings.chars_ptr := New_String (Uri);
   begin
      Internal (Self, Tmp_Uri);
      Free (Tmp_Uri);
   end Unselect_Uri;

end Gtk.File_Chooser;
