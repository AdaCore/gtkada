------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gtkada.Types; use Gtkada.Types;
with Gtk;
with Gtk.Widget;

package Gnome.Pixmap is

   type Gnome_Pixmap_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gnome_Pixmap is access all Gnome_Pixmap_Record'Class;

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Xpm_Data : Chars_Ptr_Array;
      Width    : Integer;
      Height   : Integer);

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Xpm_Data : Chars_Ptr_Array;
      Width    : Integer;
      Height   : Integer);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Gnome_New
     (Widget  : out Gnome_Pixmap;
      Gpixmap : access Gnome_Pixmap_Record);

   procedure Initialize
     (Widget  : access Gnome_Pixmap_Record'Class;
      Gpixmap : access Gnome_Pixmap_Record);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Xpm_Data : Chars_Ptr_Array);

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Xpm_Data : Chars_Ptr_Array);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Filename : String);

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Filename : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Filename : String;
      Width    : Integer;
      Height   : Integer);

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Filename : String;
      Width    : Integer;
      Height   : Integer);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Load_File
     (Gpixmap  : access Gnome_Pixmap_Record;
      Filename : String);

   procedure Load_File_At_Size
     (Gpixmap  : access Gnome_Pixmap_Record;
      Filename : String;
      Width    : Integer;
      Height   : Integer);

   procedure Load_Xpm_D
     (Gpixmap  : access Gnome_Pixmap_Record;
      Xpm_Data : Chars_Ptr_Array);

   procedure Load_Xpm_D_At_Size
     (Gpixmap  : access Gnome_Pixmap_Record;
      Xpm_Data : Chars_Ptr_Array;
      Width    : Integer;
      Height   : Integer);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_Pixmap_Record is new
     Gtk.Widget.Gtk_Widget_Record with null record;

   pragma Import (C, Get_Type, "gnome_pixmap_get_type");
end Gnome.Pixmap;
