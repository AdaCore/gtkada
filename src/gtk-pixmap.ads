-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

with Gdk.Bitmap;
with Gdk.Pixmap;
with Gtk.Misc;
with Gtk.Window;
with Gtk.Object;

package Gtk.Pixmap is

   type Gtk_Pixmap_Record is new Gtk.Misc.Gtk_Misc_Record with private;
   type Gtk_Pixmap is access all Gtk_Pixmap_Record'Class;

   procedure Get
     (Pixmap : access Gtk_Pixmap_Record;
      Val    : in Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   function Get_Mask (Widget : access Gtk_Pixmap_Record)
     return Gdk.Bitmap.Gdk_Bitmap'Class;
   function Get_Pixmap (Widget : access Gtk_Pixmap_Record)
     return Gdk.Pixmap.Gdk_Pixmap'Class;
   procedure Gtk_New
     (Widget : out Gtk_Pixmap;
      Pixmap : in Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   procedure Initialize
     (Widget : access Gtk_Pixmap_Record'Class;
      Pixmap : in Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   procedure Set
     (Pixmap : access Gtk_Pixmap_Record;
      Val    : in Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);

   function Create_Pixmap
     (Filename : in String;
      Window   : access Gtk.Window.Gtk_Window_Record'Class)
      return Gtk_Pixmap;
   --  Create a pixmap given a window and a filename

   procedure Generate (N : in Node_Ptr; File : in File_Type);
   --  Gate internal function

   procedure Generate (Pixmap : in out Object.Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function

private
   type Gtk_Pixmap_Record is new Gtk.Misc.Gtk_Misc_Record with null record;

end Gtk.Pixmap;
