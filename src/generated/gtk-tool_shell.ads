------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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
--  The Gtk.Tool_Shell.Gtk_Tool_Shell interface allows container widgets to
--  provide additional information when embedding Gtk.Tool_Item.Gtk_Tool_Item
--  widgets.
--
--  </description>
pragma Ada_2005;

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Types;     use Glib.Types;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Size_Group; use Gtk.Size_Group;
with Pango.Layout;   use Pango.Layout;

package Gtk.Tool_Shell is

   type Gtk_Tool_Shell is new Glib.Types.GType_Interface;
   Null_Gtk_Tool_Shell : constant Gtk_Tool_Shell;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tool_shell_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Ellipsize_Mode
      (Self : Gtk_Tool_Shell) return Pango.Layout.Pango_Ellipsize_Mode;
   pragma Import (C, Get_Ellipsize_Mode, "gtk_tool_shell_get_ellipsize_mode");
   --  Retrieves the current ellipsize mode for the tool shell. Tool items
   --  must not call this function directly, but rely on
   --  Gtk.Tool_Item.Get_Ellipsize_Mode instead.
   --  Since: gtk+ 2.20

   function Get_Icon_Size
      (Self : Gtk_Tool_Shell) return Gtk.Enums.Gtk_Icon_Size;
   pragma Import (C, Get_Icon_Size, "gtk_tool_shell_get_icon_size");
   --  Retrieves the icon size for the tool shell. Tool items must not call
   --  this function directly, but rely on Gtk.Tool_Item.Get_Icon_Size instead.
   --  Since: gtk+ 2.14

   function Get_Orientation
      (Self : Gtk_Tool_Shell) return Gtk.Enums.Gtk_Orientation;
   pragma Import (C, Get_Orientation, "gtk_tool_shell_get_orientation");
   --  Retrieves the current orientation for the tool shell. Tool items must
   --  not call this function directly, but rely on
   --  Gtk.Tool_Item.Get_Orientation instead.
   --  Since: gtk+ 2.14

   function Get_Relief_Style
      (Self : Gtk_Tool_Shell) return Gtk.Enums.Gtk_Relief_Style;
   pragma Import (C, Get_Relief_Style, "gtk_tool_shell_get_relief_style");
   --  Returns the relief style of buttons on Shell. Tool items must not call
   --  this function directly, but rely on Gtk.Tool_Item.Get_Relief_Style
   --  instead.
   --  Since: gtk+ 2.14

   function Get_Style
      (Self : Gtk_Tool_Shell) return Gtk.Enums.Gtk_Toolbar_Style;
   pragma Import (C, Get_Style, "gtk_tool_shell_get_style");
   --  Retrieves whether the tool shell has text, icons, or both. Tool items
   --  must not call this function directly, but rely on
   --  Gtk.Tool_Item.Get_Toolbar_Style instead.
   --  Since: gtk+ 2.14

   function Get_Text_Alignment (Self : Gtk_Tool_Shell) return Gfloat;
   pragma Import (C, Get_Text_Alignment, "gtk_tool_shell_get_text_alignment");
   --  Retrieves the current text alignment for the tool shell. Tool items
   --  must not call this function directly, but rely on
   --  Gtk.Tool_Item.Get_Text_Alignment instead.
   --  Since: gtk+ 2.20

   function Get_Text_Orientation
      (Self : Gtk_Tool_Shell) return Gtk.Enums.Gtk_Orientation;
   pragma Import (C, Get_Text_Orientation, "gtk_tool_shell_get_text_orientation");
   --  Retrieves the current text orientation for the tool shell. Tool items
   --  must not call this function directly, but rely on
   --  Gtk.Tool_Item.Get_Text_Orientation instead.
   --  Since: gtk+ 2.20

   function Get_Text_Size_Group
      (Self : Gtk_Tool_Shell) return Gtk.Size_Group.Gtk_Size_Group;
   --  Retrieves the current text size group for the tool shell. Tool items
   --  must not call this function directly, but rely on
   --  Gtk.Tool_Item.Get_Text_Size_Group instead.
   --  Since: gtk+ 2.20

   procedure Rebuild_Menu (Self : Gtk_Tool_Shell);
   pragma Import (C, Rebuild_Menu, "gtk_tool_shell_rebuild_menu");
   --  Calling this function signals the tool shell that the overflow menu
   --  item for tool items have changed. If there is an overflow menu and if it
   --  is visible when this function it called, the menu will be rebuilt.
   --  Tool items must not call this function directly, but rely on
   --  Gtk.Tool_Item.Rebuild_Menu instead.
   --  Since: gtk+ 2.14

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Tool_Shell"

   function "+" (W : Gtk_Tool_Shell) return Gtk_Tool_Shell;
   pragma Inline ("+");

private

Null_Gtk_Tool_Shell : constant Gtk_Tool_Shell :=
   Gtk_Tool_Shell (Glib.Types.Null_Interface);
end Gtk.Tool_Shell;
