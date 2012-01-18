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

package body Gtk.Tool_Shell is

   ------------------------
   -- Get_Ellipsize_Mode --
   ------------------------

   function Get_Ellipsize_Mode
      (Self : Gtk_Tool_Shell) return Pango.Layout.Pango_Ellipsize_Mode
   is
      function Internal (Self : Gtk_Tool_Shell) return Integer;
      pragma Import (C, Internal, "gtk_tool_shell_get_ellipsize_mode");
   begin
      return Pango.Layout.Pango_Ellipsize_Mode'Val (Internal (Self));
   end Get_Ellipsize_Mode;

   -------------------
   -- Get_Icon_Size --
   -------------------

   function Get_Icon_Size
      (Self : Gtk_Tool_Shell) return Gtk.Enums.Gtk_Icon_Size
   is
      function Internal (Self : Gtk_Tool_Shell) return Integer;
      pragma Import (C, Internal, "gtk_tool_shell_get_icon_size");
   begin
      return Gtk.Enums.Gtk_Icon_Size'Val (Internal (Self));
   end Get_Icon_Size;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : Gtk_Tool_Shell) return Gtk.Enums.Gtk_Orientation
   is
      function Internal (Self : Gtk_Tool_Shell) return Integer;
      pragma Import (C, Internal, "gtk_tool_shell_get_orientation");
   begin
      return Gtk.Enums.Gtk_Orientation'Val (Internal (Self));
   end Get_Orientation;

   ----------------------
   -- Get_Relief_Style --
   ----------------------

   function Get_Relief_Style
      (Self : Gtk_Tool_Shell) return Gtk.Enums.Gtk_Relief_Style
   is
      function Internal (Self : Gtk_Tool_Shell) return Integer;
      pragma Import (C, Internal, "gtk_tool_shell_get_relief_style");
   begin
      return Gtk.Enums.Gtk_Relief_Style'Val (Internal (Self));
   end Get_Relief_Style;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
      (Self : Gtk_Tool_Shell) return Gtk.Enums.Gtk_Toolbar_Style
   is
      function Internal (Self : Gtk_Tool_Shell) return Integer;
      pragma Import (C, Internal, "gtk_tool_shell_get_style");
   begin
      return Gtk.Enums.Gtk_Toolbar_Style'Val (Internal (Self));
   end Get_Style;

   --------------------------
   -- Get_Text_Orientation --
   --------------------------

   function Get_Text_Orientation
      (Self : Gtk_Tool_Shell) return Gtk.Enums.Gtk_Orientation
   is
      function Internal (Self : Gtk_Tool_Shell) return Integer;
      pragma Import (C, Internal, "gtk_tool_shell_get_text_orientation");
   begin
      return Gtk.Enums.Gtk_Orientation'Val (Internal (Self));
   end Get_Text_Orientation;

   -------------------------
   -- Get_Text_Size_Group --
   -------------------------

   function Get_Text_Size_Group
      (Self : Gtk_Tool_Shell) return Gtk.Size_Group.Gtk_Size_Group
   is
      function Internal (Self : Gtk_Tool_Shell) return System.Address;
      pragma Import (C, Internal, "gtk_tool_shell_get_text_size_group");
      Stub_Gtk_Size_Group : Gtk.Size_Group.Gtk_Size_Group_Record;
   begin
      return Gtk.Size_Group.Gtk_Size_Group (Get_User_Data (Internal (Self), Stub_Gtk_Size_Group));
   end Get_Text_Size_Group;

end Gtk.Tool_Shell;
