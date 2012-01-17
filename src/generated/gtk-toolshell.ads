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

--  <description>
--  The Gtk.Toolshell.Gtk_Toolshell interface allows container widgets to
--  provide additional information when embedding Gtk.Tool_Item.Gtk_Tool_Item
--  widgets.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Types;     use Glib.Types;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Size_Group; use Gtk.Size_Group;
with Pango.Layout;   use Pango.Layout;

package Gtk.Toolshell is

   type Gtk_Toolshell is new Glib.Types.GType_Interface;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tool_shell_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Ellipsize_Mode
      (Self : Gtk_Toolshell) return Pango.Layout.Pango_Ellipsize_Mode;
   --  Retrieves the current ellipsize mode for the tool shell. Tool items
   --  must not call this function directly, but rely on
   --  gtk_tool_item_get_ellipsize_mode instead.
   --  Since: gtk+ 2.20

   function Get_Icon_Size
      (Self : Gtk_Toolshell) return Gtk.Enums.Gtk_Icon_Size;
   --  Retrieves the icon size for the tool shell. Tool items must not call
   --  this function directly, but rely on gtk_tool_item_get_icon_size instead.
   --  Since: gtk+ 2.14

   function Get_Orientation
      (Self : Gtk_Toolshell) return Gtk.Enums.Gtk_Orientation;
   --  Retrieves the current orientation for the tool shell. Tool items must
   --  not call this function directly, but rely on
   --  gtk_tool_item_get_orientation instead.
   --  Since: gtk+ 2.14

   function Get_Relief_Style
      (Self : Gtk_Toolshell) return Gtk.Enums.Gtk_Relief_Style;
   --  Returns the relief style of buttons on Shell. Tool items must not call
   --  this function directly, but rely on gtk_tool_item_get_relief_style
   --  instead.
   --  Since: gtk+ 2.14

   function Get_Style (Self : Gtk_Toolshell) return Gtk_Toolbar_Style;
   pragma Import (C, Get_Style, "gtk_tool_shell_get_style");
   --  Retrieves whether the tool shell has text, icons, or both. Tool items
   --  must not call this function directly, but rely on
   --  gtk_tool_item_get_toolbar_style instead.
   --  Since: gtk+ 2.14

   function Get_Text_Alignment (Self : Gtk_Toolshell) return Gfloat;
   pragma Import (C, Get_Text_Alignment, "gtk_tool_shell_get_text_alignment");
   --  Retrieves the current text alignment for the tool shell. Tool items
   --  must not call this function directly, but rely on
   --  gtk_tool_item_get_text_alignment instead.
   --  Since: gtk+ 2.20

   function Get_Text_Orientation
      (Self : Gtk_Toolshell) return Gtk.Enums.Gtk_Orientation;
   --  Retrieves the current text orientation for the tool shell. Tool items
   --  must not call this function directly, but rely on
   --  gtk_tool_item_get_text_orientation instead.
   --  Since: gtk+ 2.20

   function Get_Text_Size_Group
      (Self : Gtk_Toolshell) return Gtk.Size_Group.Gtk_Size_Group;
   --  Retrieves the current text size group for the tool shell. Tool items
   --  must not call this function directly, but rely on
   --  gtk_tool_item_get_text_size_group instead.
   --  Since: gtk+ 2.20

   procedure Rebuild_Menu (Self : Gtk_Toolshell);
   pragma Import (C, Rebuild_Menu, "gtk_tool_shell_rebuild_menu");
   --  Calling this function signals the tool shell that the overflow menu
   --  item for tool items have changed. If there is an overflow menu and if it
   --  is visible when this function it called, the menu will be rebuilt.
   --  Tool items must not call this function directly, but rely on
   --  gtk_tool_item_rebuild_menu instead.
   --  Since: gtk+ 2.14

end Gtk.Toolshell;
