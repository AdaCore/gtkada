-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with System;
with Gdk; use Gdk;
with Gtk.Util; use Gtk.Util;
with Interfaces.C.Strings;

package body Gtk.File_Selection is

   ---------------------
   -- Get_Action_Area --
   ---------------------

   function Get_Action_Area (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Box.Gtk_Box
   is
      function Internal (File_Selection : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_action_area");
      Stub : Gtk.Box.Gtk_Box_Record;
   begin
      return Gtk.Box.Gtk_Box
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Action_Area;

   ---------------------
   -- Get_Button_Area --
   ---------------------

   function Get_Button_Area (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Box.Gtk_Box
   is
      function Internal (File_Selection : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_button_area");
      Stub : Gtk.Box.Gtk_Box_Record;
   begin
      return Gtk.Box.Gtk_Box
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Button_Area;

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button
     (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Button.Gtk_Button
   is
      function Internal (File_Selection : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_cancel_button");
      Stub : Gtk.Button.Gtk_Button_Record;
   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Cancel_Button;

   ------------------
   -- Get_Dir_List --
   ------------------

   function Get_Dir_List (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Widget.Gtk_Widget
   is
      function Internal (File_Selection : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_dir_list");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Dir_List;

   -------------------
   -- Get_File_List --
   -------------------

   function Get_File_List (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Widget.Gtk_Widget
   is
      function Internal (File_Selection : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_file_list");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_File_List;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (File_Selection : access Gtk_File_Selection_Record)
     return String
   is
      function Internal (File_Selection : in System.Address)
        return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_selection_get_filename");

   begin
      return Interfaces.C.Strings.Value
        (Internal (Get_Object (File_Selection)));
   end Get_Filename;

   ---------------------
   -- Get_Help_Button --
   ---------------------

   function Get_Help_Button (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Button.Gtk_Button
   is
      function Internal (File_Selection : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_help_button");
      Stub : Gtk.Button.Gtk_Button_Record;
   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Help_Button;

   --------------------------
   -- Get_History_Pulldown --
   --------------------------

   function Get_History_Pulldown
     (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Widget.Gtk_Widget
   is
      function Internal (File_Selection : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_history_pulldown");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_History_Pulldown;

   -------------------
   -- Get_Ok_Button --
   -------------------

   function Get_Ok_Button (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Button.Gtk_Button
   is
      function Internal (File_Selection : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_ok_button");
      Stub : Gtk.Button.Gtk_Button_Record;
   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Ok_Button;

   -------------------------
   -- Get_Selection_Entry --
   -------------------------

   function Get_Selection_Entry
     (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Widget.Gtk_Widget
   is
      function Internal (File_Selection : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_selection_entry");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Selection_Entry;

   ------------------------
   -- Get_Selection_Text --
   ------------------------

   function Get_Selection_Text
     (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Widget.Gtk_Widget
   is
      function Internal (File_Selection : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_selection_text");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (File_Selection)), Stub));
   end Get_Selection_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (File_Selection : out Gtk_File_Selection;
                      Title  : in String)
   is
   begin
      File_Selection := new Gtk_File_Selection_Record;
      Initialize (File_Selection, Title);
   end Gtk_New;

   -------------------------
   -- Hide_Fileop_Buttons --
   -------------------------

   procedure Hide_Fileop_Buttons
     (File_Selection : access Gtk_File_Selection_Record)
   is
      procedure Internal (File_Selection : in System.Address);
      pragma Import (C, Internal, "gtk_file_selection_hide_fileop_buttons");

   begin
      Internal (Get_Object (File_Selection));
   end Hide_Fileop_Buttons;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (File_Selection : access Gtk_File_Selection_Record;
                         Title  : in String)
   is
      function Internal (Title  : in String)
        return System.Address;
      pragma Import (C, Internal, "gtk_file_selection_new");
   begin
      Set_Object (File_Selection, Internal (Title & Ascii.NUL));
      Initialize_User_Data (File_Selection);
   end Initialize;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
      (File_Selection  : access Gtk_File_Selection_Record;
       Filename : in String)
   is
      procedure Internal
        (File_Selection  : in System.Address;
         Filename : in String);
      pragma Import (C, Internal, "gtk_file_selection_set_filename");

   begin
      Internal (Get_Object (File_Selection), Filename & Ascii.NUL);
   end Set_Filename;

   -------------------------
   -- Show_Fileop_Buttons --
   -------------------------

   procedure Show_Fileop_Buttons
     (File_Selection : access Gtk_File_Selection_Record)
   is
      procedure Internal (File_Selection : in System.Address);
      pragma Import (C, Internal, "gtk_file_selection_show_fileop_buttons");

   begin
      Internal (Get_Object (File_Selection));
   end Show_Fileop_Buttons;

   --------------
   -- Generate --
   --------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type) is
   begin
      Gen_New (N, "File_Selection", Get_Field (N, "title").all,
        File => File, Delim => '"');
      Window.Generate (N, File);
   end Generate;

   procedure Generate (File_Selection : in out Gtk_Object;
                       N              : in Node_Ptr) is
   begin
      if not N.Specific_Data.Created then
         Gtk_New
           (Gtk_File_Selection (File_Selection), Get_Field (N, "title").all);
         Set_Object (Get_Field (N, "name"), File_Selection);
         N.Specific_Data.Created := True;
      end if;

      Window.Generate (File_Selection, N);
   end Generate;

end Gtk.File_Selection;
