-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--               Copyright (C) 2000 Helix Code, Inc.                 --
--               Copyright (C) 2000-2001 ACT-Europe                  --
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

--  <description>
--  A simple Gtk_Text wrapper with a scroll bar, convenience functions,
--  and a right-click popup menu. For displaying info to the user.
--  </description>

with Gtk; use Gtk;
with Gtk.Box;
with Gdk.Font;

package Gnome.Less is

   type Gnome_Less_Record is new Gtk.Box.Gtk_Vbox_Record
     with private;
   type Gnome_Less is access all Gnome_Less_Record'Class;

   procedure Gnome_New (Less : out Gnome_Less);
   --  Create a new Gnome_Less.

   procedure Initialize (Less : access Gnome_Less_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gnome_Less.

   function Show_File (Less : Gnome_Less; Path : String) return Boolean;
   --  Clear any existing text and show the contents of the file Path.
   --  Return True on success, False and set errno on failure.

   function Show_Command (Less : Gnome_Less; Command : String) return Boolean;
   --  Clear any existing text and show the output of the execution of Command.
   --  Return True on success, False and set errno on failure.

   function Show_String (Less : Gnome_Less; S : String) return Boolean;
   --  Clear any existing text and show a string.
   --  Return True on success, False and set errno on failure.

   function Write_File (Less : Gnome_Less; Path : String) return Boolean;
   --  Write a file.
   --  Return False and set errno if either open or close fails on the file.
   --  Overwrite any existing file.

   procedure Set_Font (Less : Gnome_Less; Font : Gdk.Font.Gdk_Font);
   --  Set an arbitrary Font.

   procedure Set_Fixed_Font (Less  : Gnome_Less; Fixed : Boolean := True);
   --  Set whether to use a fixed font for any future showings.
   --  Recommended for anything that comes in columns, program code,
   --  etc. Just loads a fixed font and calls Set_Font above.

   procedure Reshow (Less : Gnome_Less);
   --  Re-insert the text with the current font settings.

private
   type Gnome_Less_Record is new Gtk.Box.Gtk_Vbox_Record
     with null record;
   pragma Import (C, Get_Type, "gnome_less_get_type");
end Gnome.Less;
