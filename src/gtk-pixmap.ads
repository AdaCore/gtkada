-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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

--  <c_version>2.8.17</c_version>

with Gdk.Bitmap;
with Gdk.Pixmap;
with Gtk.Misc;
with Gtk.Window;
with Gtkada.Types;

package Gtk.Pixmap is
   pragma Obsolescent ("Use Gtk.Image instead");

   type Gtk_Pixmap_Record is new Gtk.Misc.Gtk_Misc_Record with private;
   type Gtk_Pixmap is access all Gtk_Pixmap_Record'Class;

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Pixmap.

   procedure Gtk_New
     (Widget : out Gtk_Pixmap;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap := null;
      Mask   : Gdk.Bitmap.Gdk_Bitmap := null);
   procedure Initialize
     (Widget : access Gtk_Pixmap_Record'Class;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap);
   --  Creates or initializes a pixmap

   procedure Set
     (Pixmap : access Gtk_Pixmap_Record;
      Val    : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap);
   procedure Get
     (Pixmap : access Gtk_Pixmap_Record;
      Val    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask   : out Gdk.Bitmap.Gdk_Bitmap);
   --  Set or get the components of the pixmap

   function Create_Pixmap
     (Filename : String;
      Window   : access Gtk.Window.Gtk_Window_Record'Class) return Gtk_Pixmap;
   --  Create a pixmap given a window and a filename

   function Create_Pixmap
     (Data     : Gtkada.Types.Chars_Ptr_Array;
      Window   : access Gtk.Window.Gtk_Window_Record'Class) return Gtk_Pixmap;
   --  Create a pixmap given a window and a buffer.

   procedure Set_Build_Insensitive
     (Pixmap : access Gtk_Pixmap_Record;
      Build  : Boolean);
   --  Whether the pixmap should be grayed out, as is done for insensitive
   --  widgets that do not accept user interaction

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private
   type Gtk_Pixmap_Record is new Gtk.Misc.Gtk_Misc_Record with null record;

   pragma Import (C, Get_Type, "gtk_pixmap_get_type");
end Gtk.Pixmap;
