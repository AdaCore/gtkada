-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------

with System;
with Gdk; use Gdk;
with Interfaces.C.Strings;

package body Gtk.File_Selection is

   ---------------------
   -- Get_Action_Area --
   ---------------------

   function Get_Action_Area (Widget : in Gtk_File_Selection'Class)
                             return      Gtk.Box.Gtk_Box
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_action_area");
      Tmp : Gtk.Box.Gtk_Box;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Action_Area;

   ---------------------
   -- Get_Button_Area --
   ---------------------

   function Get_Button_Area (Widget : in Gtk_File_Selection'Class)
                             return      Gtk.Box.Gtk_Box
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_button_area");
      Tmp : Gtk.Box.Gtk_Box;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Button_Area;

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button (Widget : in Gtk_File_Selection'Class)
                               return      Gtk.Button.Gtk_Button
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_cancel_button");
      Tmp : Gtk.Button.Gtk_Button;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Cancel_Button;

   ------------------
   -- Get_Dir_List --
   ------------------

   function Get_Dir_List (Widget : in Gtk_File_Selection'Class)
                          return      Gtk.Widget.Gtk_Widget'Class
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_dir_list");
      Tmp : Gtk.Widget.Gtk_Widget;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Dir_List;

   -------------------
   -- Get_File_List --
   -------------------

   function Get_File_List (Widget : in Gtk_File_Selection'Class)
                           return      Gtk.Widget.Gtk_Widget'Class
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_file_list");
      Tmp : Gtk.Widget.Gtk_Widget;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_File_List;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Filesel : in Gtk_File_Selection'Class)
                          return       String
   is
      function Internal (Filesel : in System.Address)
                         return       Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_selection_get_filename");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Filesel)));
   end Get_Filename;

   ---------------------
   -- Get_Help_Button --
   ---------------------

   function Get_Help_Button (Widget : in Gtk_File_Selection'Class)
                             return      Gtk.Button.Gtk_Button
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_help_button");
      Tmp : Gtk.Button.Gtk_Button;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Help_Button;

   --------------------------
   -- Get_History_Pulldown --
   --------------------------

   function Get_History_Pulldown (Widget : in Gtk_File_Selection'Class)
                                  return      Gtk.Widget.Gtk_Widget'Class
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_history_pulldown");
      Tmp : Gtk.Widget.Gtk_Widget;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_History_Pulldown;

   -------------------
   -- Get_Ok_Button --
   -------------------

   function Get_Ok_Button (Widget : in Gtk_File_Selection'Class)
                           return      Gtk.Button.Gtk_Button
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_ok_button");
      Tmp : Gtk.Button.Gtk_Button;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Ok_Button;

   -------------------------
   -- Get_Selection_Entry --
   -------------------------

   function Get_Selection_Entry (Widget : in Gtk_File_Selection'Class)
                                 return      Gtk.Widget.Gtk_Widget'Class
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_selection_entry");
      Tmp : Gtk.Widget.Gtk_Widget;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Selection_Entry;

   ------------------------
   -- Get_Selection_Text --
   ------------------------

   function Get_Selection_Text (Widget : in Gtk_File_Selection'Class)
                                return      Gtk.Widget.Gtk_Widget'Class
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_file_selection_get_selection_text");
      Tmp : Gtk.Widget.Gtk_Widget;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Selection_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_File_Selection;
                      Title  : in String)
   is
      function Internal (Title  : in String)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_file_selection_new");
   begin
      Set_Object (Widget, Internal (Title & Ascii.NUL));
   end Gtk_New;

   -------------------------
   -- Hide_Fileop_Buttons --
   -------------------------

   procedure Hide_Fileop_Buttons (Filesel : in out Gtk_File_Selection'Class)
   is
      procedure Internal (Filesel : in System.Address);
      pragma Import (C, Internal, "gtk_file_selection_hide_fileop_buttons");
   begin
      Internal (Get_Object (Filesel));
   end Hide_Fileop_Buttons;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
      (Filesel  : in Gtk_File_Selection'Class;
       Filename : in String)
   is
      procedure Internal
         (Filesel  : in System.Address;
          Filename : in String);
      pragma Import (C, Internal, "gtk_file_selection_set_filename");
   begin
      Internal (Get_Object (Filesel),
                Filename & Ascii.NUL);
   end Set_Filename;

   -------------------------
   -- Show_Fileop_Buttons --
   -------------------------

   procedure Show_Fileop_Buttons (Filesel : in out Gtk_File_Selection'Class)
   is
      procedure Internal (Filesel : in System.Address);
      pragma Import (C, Internal, "gtk_file_selection_show_fileop_buttons");
   begin
      Internal (Get_Object (Filesel));
   end Show_Fileop_Buttons;

end Gtk.File_Selection;
